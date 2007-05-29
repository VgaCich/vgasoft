unit VSECore;

interface

uses
  Windows, Messages, MMSystem, AvL, avlUtils, dglOpenGL, OpenGLExt,
  VSEInit, VSEPakMan, VSEGameStates, VSEConsole, VSEConsoleVariables, VSELog,
  VSEManagers, {$IFNDEF VSE_NOSOUND}USound, {$ENDIF} SysInfo;

type
  TCore=class
  private
    FHandle: THandle;
    FDC: HDC;
    FRC: HGLRC;
    FResX, FResY, FRefresh, FDepth: Cardinal;
    FVSync: Integer;
    FFramesCount, FFPS, FFPSTimer, FPreviousUpdate, FUpdInt, FUpdOverloadCount, FUpdOverloadThreshold: Cardinal;
    FPerformanceFrequency: Int64;
    FStates: array of TGameState;
    FState, FSwitchTo: Cardinal;
    FCurState: TGameState;
    FPrevStateName: string;
    FFullscreen, FNeedSwitch, FMinimized: Boolean;
    FKeyState: TKeyboardState;
    {$IFNDEF VSE_NOSOUND}FSound: TSound;{$ENDIF}
    procedure SetFullscreen(Value: Boolean);
    procedure SetState(Value: Cardinal);
    function  GetKeyPressed(Index: Byte): Boolean;
    function  GetTime: Cardinal;
    procedure LoadFonts;
  protected
    procedure StartEngine;
    procedure SaveSettings;
    procedure Update;
    procedure Resume;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
    procedure KeyEvent(Button: Integer; Event: TKeyEvent);
    procedure CharEvent(C: Char);
    procedure RegisterCommands;
    function  Quit(const Args: string): Boolean;
    function  Resolution(const Args: string): Boolean;
    function  CmdFullscreen(const Args: string): Boolean;
  public
    constructor Create(WndHandle: THandle);
    destructor Destroy; override;
    procedure StopEngine;
    ///
    function  AddState(State: TGameState): Cardinal;
    function  ReplaceState(OrigState: Cardinal; NewState: TGameState): Boolean;
    procedure DeleteState(State: Cardinal);
    procedure SwitchState(NewState: Cardinal); overload;
    procedure SwitchState(const NewStateName: string); overload;
    function  StateExists(State: Cardinal): Boolean;
    function  GetState(State: Cardinal): TGameState;
    function  FindState(const Name: string): Cardinal;
    function  KeyRepeat(Key: Byte; Rate: Integer; var KeyVar: Cardinal): Boolean;
    procedure SetResolution(ResX, ResY, Refresh: Cardinal; CanReset: Boolean);
    ///
    property Handle: THandle read FHandle;
    property DC: HDC read FDC;
    property RC: HGLRC read FRC;
    property ResX: Cardinal read FResX;
    property ResY: Cardinal read FResY;
    property Refresh: Cardinal read FRefresh;
    property Fullscreen: Boolean read FFullscreen write SetFullscreen;
    property Minimized: Boolean read FMinimized;
    property KeyPressed[Index: Byte]: Boolean read GetKeyPressed;
    property Time: Cardinal read GetTime;
    property State: Cardinal read FState write SetState;
    property CurState: TGameState read FCurState;
    property PrevStateName: string read FPrevStateName;
    property FPS: Cardinal read FFPS;
    {$IFNDEF VSE_NOSOUND}property Sound: TSound read FSound;{$ENDIF}
    property UpdateInterval: Cardinal read FUpdInt write FUpdInt;
    property UpdateOverloadThreshold: Cardinal read FUpdOverloadThreshold write FUpdOverloadThreshold;
  end;

function VSEStart: Boolean;
function GetCursorPos(var Cursor: TPoint): Boolean;

var
  Core: TCore;
  VSEStopState: Integer=0;

implementation

const
  WndClassName: PChar = 'VSENGINE';
  WM_XBUTTONDOWN=$20B;
  WM_XBUTTONUP=$20C;

var
  WndClass: TWndClass;
  Handle: THandle;
  Msg: TMsg;
  Fin, Initializing, Quitting: Boolean;
  Mutex: Integer=0;

function GetCursorPos(var Cursor: TPoint): Boolean;
var
  Rect: TRect;
begin
  Result:=Windows.GetCursorPos(Cursor);
  GetWindowRect(Core.Handle, Rect);
  if not Core.Fullscreen then
  begin
    Cursor.X:=Cursor.X-Rect.Left-GetSystemMetrics(SM_CXDLGFRAME);
    Cursor.Y:=Cursor.Y-Rect.Top-GetSystemMetrics(SM_CYCAPTION)-GetSystemMetrics(SM_CYDLGFRAME);
  end;
end;

procedure UpdateFPS(uID, uMsg, dwUser, dw1, dw2: Cardinal); stdcall;
begin
  Core.FFPS:=Core.FFramesCount;
  Core.FFramesCount:=0;
end;

constructor TCore.Create(WndHandle: THandle);
begin
  Initializing:=true;
  inherited Create;
  FMinimized:=true;
  FNeedSwitch:=false;
  FState:=$FFFFFFFF;
  FStates:=nil;
  FCurState:=nil;
  FHandle:=WndHandle;
  FFullscreen:=false;
  FFPS:=0;
  FFramesCount:=0;
  QueryPerformanceFrequency(FPerformanceFrequency);
  FPreviousUpdate:=0;
  FUpdOverloadCount:=0;
  FUpdOverloadThreshold:=100;
  Initializing:=false;
end;

destructor TCore.Destroy;
var
  GSI: Integer;
  StateName: string;
begin
  SaveSettings;
  for GSI:=0 to High(FStates) do
  try
    StateName:=FStates[GSI].Name;
    FAN(FStates[GSI]);
  except
    LogException('in state '+StateName+'.Free');
  end;
  {$IFNDEF VSE_NOSOUND}FAN(FSound);{$ENDIF}
  Managers.CleanupManagers;
  gleFreeFonts;
  wglMakeCurrent(FDC, 0);
  wglDeleteContext(FRC);
  if FDC>0 then ReleaseDC(FHandle, FDC);
  timeKillEvent(FFPSTimer);
  inherited Destroy;
end;

//Public

procedure TCore.StartEngine;
begin
  FResX:=VSEInit.ResX;
  FResY:=VSEInit.ResY;
  FRefresh:=VSEInit.Refresh;
  FDepth:=VSEInit.Depth;
  Fullscreen:=VSEInit.Fullscreen;
  FVSync:=VSEInit.VSync;
  {$IFNDEF VSE_NOSOUND}FSound:=TSound.Create(VSEInit.SoundDevice);{$ENDIF}
  if (FVSync<>0) and (FVSync<>1) then FVSync:=1;
  if FResX<640 then FResX:=640;
  if FResY<480 then FResY:=480;
  if FRefresh<60 then Frefresh:=60;
  FDC:=GetDC(FHandle);
  FRC:=gleSetPix(FDC, FDepth);
  if FRC=0 then raise Exception.Create('Unable to set rendering context');
  Managers.InitManagers;
  LogSysInfo;
  glShadeModel(GL_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glHint(GL_POLYGON_SMOOTH, GL_NICEST);
  glHint(GL_SHADE_MODEL, GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_NORMALIZE);
  LoadFonts;
  FFPSTimer:=timeSetEvent(1000, 0, @UpdateFPS, 0, TIME_PERIODIC);
  RegisterCommands;
end;

procedure TCore.StopEngine;
begin
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TCore.SaveSettings;
begin
  VSEInit.ResX:=FResX;
  VSEInit.ResY:=FResY;
  VSEInit.Refresh:=FRefresh;
  VSEInit.Depth:=FDepth;
  VSEInit.Fullscreen:=FFullscreen;
  VSEInit.VSync:=FVSync;
  {$IFNDEF VSE_NOSOUND}VSEInit.SoundDevice:=Sound.DeviceName;{$ENDIF}
end;

procedure TCore.Update;
var
  T, i, UpdTime: Cardinal;
begin
  GetKeyboardState(FKeyState);
  if FMinimized then Exit;
  if GetForegroundWindow<>FHandle then
  begin
    Log(llInfo, 'Minimized');
    FMinimized:=True;
    {$IFNDEF VSE_NOSOUND}Sound.Pause;{$ENDIF}
    if FFullscreen then gleGoBack;
    SendMessage(FHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  end;
  if FNeedSwitch
    then State:=FSwitchTo;
  if FPreviousUpdate=0 then FPreviousUpdate:=Time;
  T:=Time-FPreviousUpdate;
  FPreviousUpdate:=FPreviousUpdate+T-(T mod FUpdInt);
  if FCurState<>nil then
  begin
    UpdTime:=Time;
    for i:=1 to T div FUpdInt do
    try
      FCurState.Update;
    except
      LogException('in state '+FCurState.Name+'.Update');
    end;
    if Time-UpdTime>T
      then Inc(FUpdOverloadCount, T div FUpdInt)
      else FUpdOverloadCount:=0;
    if (FUpdOverloadThreshold>0) and (FUpdOverloadCount>FUpdOverloadThreshold) then
    try
      FCurState.SysNotify(snUpdateOverload);
    except
      LogException('in state '+FCurState.Name+'.SysNotify(snUpdateOverload)');
    end;
    try
      FCurState.Draw;
    except
      LogException('in state '+FCurState.Name+'.Draw');
    end;
    if WGL_EXT_swap_control and (FVSync<>wglGetSwapIntervalEXT)
      then wglSwapIntervalEXT(FVSync);
    SwapBuffers(FDC);
    Inc(FFramesCount);
  end;
end;

procedure TCore.Resume;
begin
  Log(llInfo, 'Maximized');
  if FFullscreen then gleGoFullscreen(ResX, ResY, Refresh, FDepth);
  FMinimized:=false;
  if FCurState<>nil then
  try
    FCurState.Resume;
  except
    LogException('in state '+FCurState.Name+'.Resume');
  end;
  {$IFNDEF VSE_NOSOUND}Sound.Start;{$ENDIF}
end;

procedure TCore.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
var
  Rect: TRect;
begin
  GetWindowRect(Handle, Rect);
  if (Event=meWheel) and not Fullscreen then
  begin
    X:=X-Rect.Left-GetSystemMetrics(SM_CXDLGFRAME);
    Y:=Y-Rect.Top-GetSystemMetrics(SM_CYCAPTION)-GetSystemMetrics(SM_CYDLGFRAME);
  end;
  if Event=meDown
    then SetCapture(FHandle)
    else if Event=meUp
      then ReleaseCapture;
  if FCurState<>nil then
  try
    FCurState.MouseEvent(Button, Event, X, Y);
  except
    LogException('in state '+FCurState.Name+Format('.MouseEvent(%d, %d, %d, %d)', [Button, Integer(Event), X, Y]));
  end;
end;

procedure TCore.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if (Button=27) and (Event=keUp) then
  begin
    StopEngine;
    Exit;
  end;
  if FCurState<>nil then
  try
    FCurState.KeyEvent(Button, Event);
  except
    LogException('in state '+FCurState.Name+Format('.KeyEvent(%d, %d)', [Button, Integer(Event)]));
  end;
end;

procedure TCore.CharEvent(C: Char);
begin
  if FCurState<>nil then
  try
    FCurState.CharEvent(C);
  except
    LogException('in state '+FCurState.Name+'.CharEvent("'+C+'")');
  end;
end;

function TCore.AddState(State: TGameState): Cardinal;
begin
  Result:=Length(FStates);
  SetLength(FStates, Result+1);
  FStates[Result]:=State;
end;

function TCore.ReplaceState(OrigState: Cardinal; NewState: TGameState): Boolean;
begin
  Result:=true;
  if OrigState<Length(FStates)
    then FStates[OrigState]:=NewState
    else Result:=false;
end;

procedure TCore.DeleteState(State: Cardinal);
begin
  if State<Length(FStates) then
  begin
    if State<High(FStates)
      then Move(FStates[State+1], FStates[State], (Length(FStates)-State-1)*SizeOf(TGameState));
    SetLength(FStates, Length(FStates)-1);
  end;
end;

procedure TCore.SwitchState(NewState: Cardinal);
begin
  FSwitchTo:=NewState;
  FNeedSwitch:=true;
end;

procedure TCore.SwitchState(const NewStateName: string);
begin
  SwitchState(FindState(NewStateName));
end;

function TCore.StateExists(State: Cardinal): Boolean;
begin
  Result:=(State<Length(FStates)) and Assigned(FStates[State]);
end;

function TCore.GetState(State: Cardinal): TGameState;
begin
  if State<Length(FStates)
    then Result:=FStates[State]
    else Result:=nil;
end;

function TCore.FindState(const Name: string): Cardinal;
var
  i: Cardinal;
begin
  Result:=$FFFFFFFF;
  for i:=0 to High(FStates) do
    if Assigned(FStates[i]) and (FStates[i].Name=Name) then
    begin
      Result:=i;
      Exit;
    end;
end;

function TCore.KeyRepeat(Key: Byte; Rate: Integer; var KeyVar: Cardinal): Boolean;
var
  T: Cardinal;
begin
  Result:=false;
  if KeyPressed[Key] then
  begin
    T:=Time;
    if (T>KeyVar+Rate) or (KeyVar=0) then
    begin
      Result:=true;
      KeyVar:=T;
    end;
  end
    else KeyVar:=0;
end;

procedure TCore.SetResolution(ResX, ResY, Refresh: Cardinal; CanReset: Boolean);
var
  OldResX, OldResY, OldRefresh: Cardinal;
begin
  OldResX:=FResX;
  OldResY:=FResY;
  OldRefresh:=FRefresh;
  FResX:=ResX;
  FResY:=ResY;
  FRefresh:=Refresh;
  LogF(llInfo, 'Set resolution %dx%d@%d', [ResX, ResY, Refresh]);
  SendMessage(FHandle, WM_SIZE, 0, ResY shl 16 + ResX);
  if FFullscreen then
  begin
    if not gleGoFullscreen(ResX, ResY, Refresh, FDepth) then
    begin
      Log(llError, 'Unable to enter fullscreen! Choose lower resolution or refresh rate');
      if CanReset
        then SetResolution(OldResX, OldResY, OldRefresh, false)
        else begin
          MessageBox(FHandle, 'Unable to enter fullscreen! Choose lower resolution or refresh rate', PChar(CaptionVer), MB_ICONERROR);
          VSEStopState:=1;
          Exit;
        end;
    end;
  end
  else SetWindowPos(FHandle, 0, (Screen.Width-FResX) div 2, (Screen.Height-FResY) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE); 
end;

//Private

procedure TCore.SetFullscreen(Value: Boolean);
begin
  if FFullscreen=Value then Exit;
  FFullscreen:=Value;
  if Value
    then FFullscreen:=gleGoFullscreen(FResX, FResY, FRefresh, FDepth)
    else gleGoBack;
  if FFullscreen<>Value then Log(llError, 'Unable to enter fullscreen! Choose lower resolution or refresh rate');
  if FFullscreen then
  begin
    SetWindowLong(FHandle, GWL_EXSTYLE, WS_EX_APPWINDOW);
    SetWindowLong(FHandle, GWL_STYLE, Integer(WS_POPUP) or WS_CLIPCHILDREN or WS_CLIPSIBLINGS);
  end
  else begin
    SetWindowLong(FHandle, GWL_EXSTYLE, WS_EX_APPWINDOW or WS_EX_WINDOWEDGE);
    SetWindowLong(FHandle, GWL_STYLE,
      WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_CLIPCHILDREN or WS_CLIPSIBLINGS);
    SetWindowPos(FHandle, 0, (Screen.Width-FResX) div 2, (Screen.Height-FResY) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
  end;
  SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_SHOWWINDOW);
end;

procedure TCore.SetState(Value: Cardinal);
begin
  if StateExists(FState) and StateExists(Value)
    then LogFNC(llInfo, 'Switch state from %s to %s', [FStates[FState].Name, FStates[Value].Name]);
  FNeedSwitch:=false;
  if (FState=Value) or (Value>High(FStates)) then Exit;
  if FCurState<>nil then
  begin
    try
      FCurState.Deactivate;
    except
      LogException('in state '+FCurState.Name+'.Deactivate');
    end;
    FPrevStateName:=FCurState.Name;
  end;
  FState:=Value;
  FCurState:=FStates[Value];
  try
    FUpdInt:=FCurState.Activate;
  except
    LogException('in state '+FCurState.Name+'.Activate');
    if FUpdInt<=1 then FUpdInt:=50;
  end;
end;

function TCore.GetKeyPressed(Index: Byte): Boolean;
begin
  Result:=FKeyState[Index]>127;
end;

function TCore.GetTime: Cardinal;
var
  T: Int64;
begin
  QueryPerformanceCounter(T);
  Result:=Trunc(1000*T/FPerformanceFrequency);
end;

procedure TCore.LoadFonts;

  procedure LoadFont(const Name, ID: string);
  var
    Data: TStream;
  begin
    Data:=PakMan.OpenFile(Name, ofNoCreate);
    try
      if not gleLoadFont(ID, Data) then LogF(llError, 'Could''n load font %s(%s)!', [ID, Name]);
    finally
      FAN(Data);
    end;
  end;

var
  FntList: TStringList;
  FntListFile: TStream;
  i, P: Integer;
begin
  if FontsList='' then Exit;
  Log(llInfo, 'Loading fonts');
  FntList:=TStringList.Create;
  try
    FntListFile:=PakMan.OpenFile(FontsList, 0);
    FntList.LoadFromStream(FntListFile);
    FAN(FntListFile);
    for i:=0 to FntList.Count-1 do
    begin
      P:=Pos('=', FntList[i]);
      LoadFont(Trim(Copy(FntList[i], P+1, MaxInt)), Trim(Copy(FntList[i], 1, P-1)));
    end;
  finally
    FAN(FntList);
  end;
end;

// Protected - console commands handlers

const
  QuitHelp='quit'#13+
           '  Quits the program';
  ResolutionHelp='resolution <width> <height> [refreshrate]'#13+
                 '  Sets resolution width x height @ refreshrate'#13+
                 'resolution'#13+
                 '  Shows current resolution and refresh rate';
  FullscreenHelp='fullscreen <value>'#13+
                 '  Sets fullscreen to value (0: disabled; 1: enabled)'#13+
                 'fullscreen'#13+
                 '  Shows current fullscreen state';
  VSyncHelp='Vertical synchronization state (0: disabled; 1: enabled)';
  FPSHelp='Current FPS';

procedure TCore.RegisterCommands;
begin
  Console.RegisterCommand('quit', QuitHelp, Quit);
  Console.RegisterCommand('resolution', ResolutionHelp, Resolution);
  Console.RegisterCommand('fullscreen', FullscreenHelp, CmdFullscreen);
  TConsoleVariableInteger.Create('vsync', VSyncHelp, @FVSync, 0, 1, 1, false);
  TConsoleVariableInteger.Create('fps', FPSHelp, @FFPS, 0, 0, 0, true);
end;

function TCore.Quit(const Args: string): Boolean;
begin
  Result:=true;
  StopEngine;
end;

function TCore.Resolution(const Args: string): Boolean;
var
  ResX, ResY, Refresh: Cardinal;
  S: string;
begin
  Result:=false;
  if Args='' then
  begin
    Console.AddToConsole(Format('Resolution: %dx%d@%d', [FResX, FResY, FRefresh]));
    Result:=true;
    Exit;
  end;
  S:=Args;
  ResX:=StrToCar(Tok(' ', S));
  ResY:=StrToCar(Tok(' ', S));
  Refresh:=StrToCar(Tok(' ', S));
  if (ResX<640) or (ResY<480) then
  begin
    Console.AddToConsole('^1Error: Too small resolution');
    Exit;
  end;
  if Refresh=0 then Refresh:=FRefresh;
  SetResolution(ResX, ResY, Refresh, true);
  Result:=true;
end;

function TCore.CmdFullscreen(const Args: string): Boolean;
begin
  Result:=false;
  if Args='' then
  begin
    Console.AddToConsole('Fullscreen='+IntToStr(Byte(FFullscreen)));
    Result:=true;
    Exit;
  end;
  if StrToInt(Args)=0
    then Fullscreen:=false
    else Fullscreen:=true;
end;

function WndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  WndWidth, WndHeight: Word;
begin
  Result:=0;
  case (Msg) of
    WM_ACTIVATE:
      begin
        if Initializing or Quitting or (Core<>nil) then Exit;
        Log(llInfo, 'Creating engine');
        {$IFDEF VSEDEBUG}Log(llInfo, 'Debug mode');{$ENDIF}
        try
          Core:=TCore.Create(Handle);
          Core.StartEngine;
        except
          LogException('while initializing engine');
          VSEStopState:=0;
          SendMessage(Handle, WM_CLOSE, 0, 0);
          Exit;
        end;
        Log(llInfo, 'Engine created');
        Core.SetResolution(Core.ResX, Core.ResY, Core.Refresh, false);
        Log(llInfo, 'Init states');
        if Assigned(InitStates) then
          try
            InitStates;
          except
            LogException('in InitStates');
            MessageBox(Handle, PChar('Exception "'+ExceptObject.ClassName+'" at '+IntToHex(Cardinal(ExceptAddr), 8)+' with message "'+Exception(ExceptObject).Message+'" in InitStates'), 'Error', MB_ICONERROR);
            VSEStopState:=1;
            Core.StopEngine;
            Exit;
          end
          else begin
            Log(llError, 'InitStates() not initialized');
            MessageBox(0, 'InitStates() not initialized', 'Error', MB_ICONERROR);
            VSEStopState:=1;
            Core.StopEngine;
            Exit;
          end;
        if DoAutoexec then Console.ExecCommand('/exec autoexec');
        Core.FMinimized:=false;
      end;
    WM_KEYUP: Core.KeyEvent(wParam, keUp);
    WM_KEYDOWN: Core.KeyEvent(wParam, keDown);
    WM_CHAR: Core.CharEvent(Chr(wParam));
    WM_MOUSEMOVE: Core.MouseEvent(0, meMove, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_LBUTTONDOWN: Core.MouseEvent(1, meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_LBUTTONUP: Core.MouseEvent(1, meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_RBUTTONDOWN: Core.MouseEvent(2, meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_RBUTTONUP: Core.MouseEvent(2, meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_MBUTTONDOWN: Core.MouseEvent(3, meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_MBUTTONUP: Core.MouseEvent(3, meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_XBUTTONDOWN: Core.MouseEvent(3+HiWord(wParam), meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_XBUTTONUP: Core.MouseEvent(3+HiWord(wParam), meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_MOUSEWHEEL: Core.MouseEvent(SmallInt(HiWord(wParam)) div 120, meWheel, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_DESTROY:
      begin
        Log(llInfo, 'Destroying engine');
        if Assigned(Core) and Core.Fullscreen then gleGoBack;
        FAN(Core);
        Quitting:=true;
        Log(llInfo, 'Engine destroyed');
        LogRaw('');
        Result:=0;
        PostQuitMessage(VSEStopState);
      end;
    WM_QUERYENDSESSION:
      begin
        Core.StopEngine;
        Result:=1;
      end;
    WM_SIZE:
      begin
        if (ExtensionsRead and ImplementationRead) and Assigned(Core) then begin
          if Core.Fullscreen then
          begin
            WndWidth:=Core.ResX;
            WndHeight:=Core.ResY;
          end
          else begin
            WndWidth:=Core.ResX+GetSystemMetrics(SM_CXDLGFRAME)*2;
            WndHeight:=Core.ResY+GetSystemMetrics(SM_CYCAPTION)+GetSystemMetrics(SM_CYDLGFRAME)*2;
          end;
          gleResizeWnd(Core.ResX, Core.ResY);
          if Core.Fullscreen
            then SetWindowPos(Handle, HWND_TOPMOST, 0, 0, WndWidth, WndHeight, 0)
            else SetWindowPos(Handle, HWND_TOP, 0, 0, WndWidth, WndHeight, SWP_NOMOVE or SWP_FRAMECHANGED);
        end;
        Result:=0;
      end;
    else
      Result:=DefWindowProc(hWnd, Msg, wParam, lParam);
  end;
end;

function IsRunning(const ID: string): Boolean;
begin
  Result:=false;
  if Mutex<>0 then Exit;
  Mutex:=CreateMutex(nil, true, PChar(ID));
  if GetLastError=ERROR_ALREADY_EXISTS then Result:=true;
end;

function VSEStart: Boolean;
begin
  Result:=false;
  if CaptionVer='' then CaptionVer:=Caption+' '+Version;
  if IsRunning(Caption) then Exit;
  Log(llInfo, CaptionVer+' started');
  Log(llInfo, VSECaptVer);
  Initializing:=false;
  Fin:=false;
  Quitting:=false;
  ZeroMemory(@WndClass, SizeOf(WndClass));
  with WndClass do
  begin
    style:=CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
    lpfnWndProc:=@WndProc;
    hInstance:=SysInit.hInstance;
    hCursor:=LoadCursor(0, IDC_ARROW);
    hIcon:=LoadIcon(hInstance, 'MAINICON');
    lpszClassName:=WndClassName;
    hbrBackground:=0;
  end;    
  if Windows.RegisterClass(WndClass)=0 then
  begin
    Log(llError, 'Failed to register the window class');
    MessageBox(0, 'Failed to register the window class!', 'Error', MB_ICONERROR);
    Exit;
  end;
  Handle:=CreateWindowEx(WS_EX_APPWINDOW or WS_EX_WINDOWEDGE, WndClassName, PChar(Caption),
    WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,
    0, 0, 800, 600, 0, 0, hInstance, nil);
  if Handle=0 then
  begin
    Log(llError, 'Unable to create window');
    MessageBox(0, 'Unable to create window!', 'Error', MB_ICONERROR);
    Exit;
  end;
  SendMessage(Handle, WM_SETICON, 1, LoadIcon(hInstance, 'MAINICON'));
  ShowWindow(Handle, SW_SHOW);
  while not Fin do
  begin
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      if (Msg.message=WM_QUIT)
        then Fin:=true
      else begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end
    else
      if Core<>nil then
        if not Core.Minimized
          then Core.Update
          else if GetForegroundWindow=Handle
            then Core.Resume;
  end;
  Result:=Msg.wParam=0;
  if not UnregisterClass(WndClassName, hInstance) then
  begin
    Log(llError, 'Failed to unregister the window class');
    MessageBox(0, 'Failed to unregister the window class!', 'Error', MB_ICONERROR);
    Exit;
  end;
end;

initialization

finalization
  ReleaseMutex(Mutex);

end.
