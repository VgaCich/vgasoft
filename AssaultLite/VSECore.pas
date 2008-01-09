unit VSECore;

interface

uses
  Windows, Messages, MMSystem, AvL, avlUtils, OpenGL, OpenGLExt, oglExtensions,
  VSEInit, VSEGameStates{$IFDEF VSE_LOG}, VSELog, VSESysInfo{$ENDIF};

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
    FFullscreen, FNeedSwitch, FMinimized, FMouseCapture: Boolean;
    FKeyState: TKeyboardState;
    procedure SetFullscreen(Value: Boolean);
    function  GetVSync: Boolean;
    procedure SetVSync(Value: Boolean);
    procedure SetState(Value: Cardinal);
    function  GetKeyPressed(Index: Byte): Boolean;
    procedure SetMouseCapture(Value: Boolean);
    function  GetTime: Cardinal;
  protected
    procedure StartEngine;
    procedure SaveSettings;
    procedure Update;
    procedure Resume;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
    procedure KeyEvent(Button: Integer; Event: TKeyEvent);
    procedure CharEvent(C: Char);
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
    procedure MakeScreenshot(const Name: string);
    ///
    property Handle: THandle read FHandle;
    property DC: HDC read FDC;
    property RC: HGLRC read FRC;
    property ResX: Cardinal read FResX;
    property ResY: Cardinal read FResY;
    property Refresh: Cardinal read FRefresh;
    property Depth: Cardinal read FDepth write FDepth;
    property Fullscreen: Boolean read FFullscreen write SetFullscreen;
    property VSync: Boolean read GetVSync write SetVSync;
    property Minimized: Boolean read FMinimized;
    property KeyPressed[Index: Byte]: Boolean read GetKeyPressed;
    property MouseCapture: Boolean read FMouseCapture write SetMouseCapture;
    property Time: Cardinal read GetTime;
    property State: Cardinal read FState write SetState;
    property CurState: TGameState read FCurState;
    property PrevStateName: string read FPrevStateName;
    property FPS: Cardinal read FFPS;
    property UpdateInterval: Cardinal read FUpdInt write FUpdInt;
    property UpdateOverloadThreshold: Cardinal read FUpdOverloadThreshold write FUpdOverloadThreshold;
  end;

function VSEStart: Boolean;
function GetCursorPos(var Cursor: TPoint): Boolean;

var
  Core: TCore;
  VSEStopState: Integer=0;

implementation

uses
  USound, UTexMan;

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
    {$IFDEF VSE_LOG}LogException('in state '+StateName+'.Free');
    {$ELSE}StopEngine; raise;{$ENDIF}
  end;
  FAN(TexMan);
  FAN(Sound);
  wglMakeCurrent(FDC, 0);
  wglDeleteContext(FRC);
  if FDC>0 then ReleaseDC(FHandle, FDC);
  timeKillEvent(FFPSTimer);
  inherited Destroy;
end;

//Protected - interacting with WndProc

procedure TCore.StartEngine;
begin
  FResX:=VSEInit.ResX;
  FResY:=VSEInit.ResY;
  FRefresh:=VSEInit.Refresh;
  FDepth:=VSEInit.Depth;
  Fullscreen:=VSEInit.Fullscreen;
  FVSync:=VSEInit.VSync;
  if (FVSync<>0) and (FVSync<>1) then FVSync:=1;
  if FResX<640 then FResX:=640;
  if FResY<480 then FResY:=480;
  if FRefresh<60 then FRefresh:=60;
  FDC:=GetDC(FHandle);
  FRC:=gleSetPix(FDC, FDepth);
  if FRC=0 then raise Exception.Create('Unable to set rendering context');
  {$IFDEF VSE_LOG}LogSysInfo;{$ENDIF}
  Sound:=TSound.Create;
  TexMan:=TTexMan.Create;
  glShadeModel(GL_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glHint(GL_POLYGON_SMOOTH, GL_NICEST);
  glHint(GL_SHADE_MODEL, GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_NORMALIZE);
  FFPSTimer:=timeSetEvent(1000, 0, @UpdateFPS, 0, TIME_PERIODIC);
end;

procedure TCore.SaveSettings;
begin
  VSEInit.ResX:=FResX;
  VSEInit.ResY:=FResY;
  VSEInit.Refresh:=FRefresh;
  VSEInit.Depth:=FDepth;
  VSEInit.Fullscreen:=FFullscreen;
  VSEInit.VSync:=FVSync;
end;

procedure TCore.Update;
var
  T, i, UpdTime: Cardinal;
  Cur: TPoint;
begin
  GetKeyboardState(FKeyState);
  if FMinimized then Exit;
  if GetForegroundWindow<>FHandle then
  begin
    {$IFDEF VSE_LOG}Log(llInfo, 'Minimized');{$ENDIF}
    FMinimized:=True;
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
    if FMouseCapture then
    begin
      Windows.GetCursorPos(Cur);
      Cur.X:=Cur.X-FResX div 2;
      Cur.Y:=Cur.Y-FResY div 2;
      SetCursorPos(FResX div 2, FResY div 2);
      try
        FCurState.MouseEvent(0, meMove, Cur.X, Cur.Y);
      except
        {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+Format('.MouseEvent(0, %d, %d, %d)', [Integer(meMove), Cur.X, Cur.Y]));
        {$ELSE}StopEngine; raise;{$ENDIF}
      end;
    end;
    for i:=1 to T div FUpdInt do
    try
      FCurState.Update;
    except
      {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.Update');
      {$ELSE}StopEngine; raise;{$ENDIF}
    end;
    if Time-UpdTime>T
      then Inc(FUpdOverloadCount, T div FUpdInt)
      else FUpdOverloadCount:=0;
    if (FUpdOverloadThreshold>0) and (FUpdOverloadCount>FUpdOverloadThreshold) then
    try
      FCurState.SysNotify(snUpdateOverload);
    except
      {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.SysNotify(snUpdateOverload)');
      {$ELSE}StopEngine; raise;{$ENDIF}
    end;
    try
      FCurState.Draw;
    except
      {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.Draw');
      {$ELSE}StopEngine; raise;{$ENDIF}
    end;
    if WGL_EXT_swap_control and (FVSync<>wglGetSwapIntervalEXT)
      then wglSwapIntervalEXT(FVSync);
    SwapBuffers(FDC);
    Inc(FFramesCount);
  end;
end;

procedure TCore.Resume;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Maximized');{$ENDIF}
  if FFullscreen then gleGoFullscreen(ResX, ResY, Refresh, FDepth);
  FMinimized:=false;
  if FCurState<>nil then
  try
    FCurState.Resume;
  except
    {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.Resume');
    {$ELSE}StopEngine; raise;{$ENDIF}
  end;
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
  if FMouseCapture and (Event=meMove) then Exit;
  if FCurState<>nil then
  try
    FCurState.MouseEvent(Button, Event, X, Y);
  except
    {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+Format('.MouseEvent(%d, %d, %d, %d)', [Button, Integer(Event), X, Y]));
    {$ELSE}StopEngine; raise;{$ENDIF}
  end;
end;

procedure TCore.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  {$IFDEF VSE_ESC_EXIT}
  if (Button=VK_ESCAPE) and (Event=keUp) then
  begin
    StopEngine;
    Exit;
  end;
  {$ENDIF}
  if (Button=VK_SNAPSHOT) and (Event=keUp) then
  begin
    MakeScreenshot('Screen');
    Exit;
  end;
  if FCurState<>nil then
  try
    FCurState.KeyEvent(Button, Event);
  except
    {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+Format('.KeyEvent(%d, %d)', [Button, Integer(Event)]));
    {$ELSE}StopEngine; raise;{$ENDIF}
  end;
end;

procedure TCore.CharEvent(C: Char);
begin
  if FCurState<>nil then
  try
    FCurState.CharEvent(C);
  except
    {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.CharEvent("'+C+'")');
    {$ELSE}StopEngine; raise;{$ENDIF}
  end;
end;

//Public

procedure TCore.StopEngine;
begin
  PostMessage(Handle, WM_CLOSE, 0, 0);
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
  {$IFDEF VSE_LOG}LogF(llInfo, 'Set resolution %dx%d@%d', [ResX, ResY, Refresh]);{$ENDIF}
  SendMessage(FHandle, WM_SIZE, 0, ResY shl 16 + ResX);
  if FFullscreen then
  begin
    if not gleGoFullscreen(ResX, ResY, Refresh, FDepth) then
    begin
      {$IFDEF VSE_LOG}Log(llError, 'Unable to enter fullscreen! Choose lower resolution or refresh rate');{$ENDIF}
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

procedure TCore.MakeScreenshot(const Name: string);
var
  F: TFileStream;
  Pix: Pointer;
  BMPFH: TBitmapFileHeader;
  BMPIH: TBitmapInfoHeader;
begin
  GetMem(Pix, FResX*FResY*3);
  try
    glReadPixels(0, 0, FResX, FResY, GL_BGR, GL_UNSIGNED_BYTE, Pix);
    F:=TFileStream.Create(ExePath+Name+'.bmp', fmCreate);
    with BMPFH, BMPIH do
    begin
      bfType:=$4D42;
      bfSize:=FResX*FResY*3+SizeOf(BMPFH)+SizeOf(BMPIH);
      bfReserved1:=0;
      bfReserved2:=0;
      bfOffBits:=SizeOf(BMPFH)+SizeOf(BMPIH);
      biSize:=SizeOf(BMPIH);
      biWidth:=FResX;
      biHeight:=FResY;
      biPlanes:=1;
      biBitCount:=24;
      biCompression:=0;
      biSizeImage:=FResX*FResY*3;
      biXPelsPerMeter:=0;
      biYPelsPerMeter:=0;
      biClrUsed:=0;
      biClrImportant:=0;
    end;
    F.Write(BMPFH, SizeOf(BMPFH));
    F.Write(BMPIH, SizeOf(BMPIH));
    F.Write(Pix^, FResX*FResY*3);
  finally
    FreeMem(Pix);
    FAN(F);
  end;
end;

//Private

procedure TCore.SetFullscreen(Value: Boolean);
begin
  if FFullscreen=Value then Exit;
  FFullscreen:=Value;
  if Value
    then FFullscreen:=gleGoFullscreen(FResX, FResY, FRefresh, FDepth)
    else gleGoBack;
  {$IFDEF VSE_LOG}if FFullscreen<>Value then Log(llError, 'Unable to enter fullscreen! Choose lower resolution or refresh rate');{$ENDIF}
  if FFullscreen then
  begin
    SetWindowLong(FHandle, GWL_EXSTYLE, WS_EX_APPWINDOW);
    SetWindowLong(FHandle, GWL_STYLE, Integer(WS_POPUP) or WS_CLIPCHILDREN or WS_CLIPSIBLINGS);
  end
  else begin
    SetWindowLong(FHandle, GWL_EXSTYLE, WS_EX_APPWINDOW or WS_EX_WINDOWEDGE);
    SetWindowLong(FHandle, GWL_STYLE, WS_OVERLAPPED or WS_CAPTION or WS_CLIPCHILDREN or WS_CLIPSIBLINGS);
    SetWindowPos(FHandle, 0, (Screen.Width-FResX) div 2, (Screen.Height-FResY) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
  end;
  SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_SHOWWINDOW);
end;

function TCore.GetVSync: Boolean;
begin
  Result:=FVSync<>0;
end;

procedure TCore.SetVSync(Value: Boolean);
const
  VSync: array[Boolean] of Integer=(0, 1);
begin
  FVSync:=VSync[Value];
end;

procedure TCore.SetState(Value: Cardinal);
begin
  {$IFDEF VSE_LOG}if StateExists(FState) and StateExists(Value)
    then LogF(llInfo, 'Switch state from %s to %s', [FStates[FState].Name, FStates[Value].Name]);{$ENDIF}
  FNeedSwitch:=false;
  if (FState=Value) or (Value>High(FStates)) then Exit;
  if FCurState<>nil then
  begin
    try
      FCurState.Deactivate;
    except
      {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.Deactivate');
      {$ELSE}StopEngine; raise;{$ENDIF}
    end;
    FPrevStateName:=FCurState.Name;
  end;
  FState:=Value;
  FCurState:=FStates[Value];
  try
    FUpdInt:=FCurState.Activate;
  except
    if FUpdInt<=1 then FUpdInt:=50;
    {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.Activate');
    {$ELSE}StopEngine; raise;{$ENDIF}
  end;
end;

function TCore.GetKeyPressed(Index: Byte): Boolean;
begin
  Result:=FKeyState[Index]>127;
end;

procedure TCore.SetMouseCapture(Value: Boolean);
begin
  if Value=FMouseCapture then Exit;
  FMouseCapture:=Value;
  if Value then
  begin
    SetCursorPos(FResX div 2, FResY div 2);
    SetCapture(FHandle);
  end
    else ReleaseCapture;
end;

function TCore.GetTime: Cardinal;
var
  T: Int64;
begin
  QueryPerformanceCounter(T);
  Result:=Trunc(1000*T/FPerformanceFrequency);
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
        {$IFDEF VSE_LOG}Log(llInfo, 'Creating engine');
        {$IFDEF VSEDEBUG}Log(llInfo, 'Debug mode');{$ENDIF}{$ENDIF}
        try
          Core:=TCore.Create(Handle);
          Core.StartEngine;
        except
          {$IFDEF VSE_LOG}LogException('while initializing engine');{$ENDIF}
          VSEStopState:=0;
          SendMessage(Handle, WM_CLOSE, 0, 0);
          {$IFNDEF VSE_LOG}raise;{$ENDIF}
        end;
        {$IFDEF VSE_LOG}Log(llInfo, 'Engine created');{$ENDIF}
        Core.SetResolution(Core.ResX, Core.ResY, Core.Refresh, false);
        {$IFDEF VSE_LOG}Log(llInfo, 'Init states');{$ENDIF}
        if Assigned(InitStates) then
          try
            InitStates;
          except
            {$IFDEF VSE_LOG}LogException('in InitStates');{$ENDIF}
            MessageBox(Handle, PChar('Exception "'+ExceptObject.ClassName+'" at '+IntToHex(Cardinal(ExceptAddr), 8)+' with message "'+Exception(ExceptObject).Message+'" in InitStates'), 'Error', MB_ICONERROR);
            VSEStopState:=1;
            Core.StopEngine;
            {$IFNDEF VSE_LOG}raise;{$ENDIF}
          end
          else begin
            {$IFDEF VSE_LOG}Log(llError, 'InitStates() not initialized');{$ENDIF}
            MessageBox(0, 'InitStates() not initialized', 'Error', MB_ICONERROR);
            VSEStopState:=1;
            Core.StopEngine;
            Exit;
          end;
        //if DoAutoexec then Console.ExecCommand('/exec autoexec');
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
        {$IFDEF VSE_LOG}Log(llInfo, 'Destroying engine');{$ENDIF}
        if Assigned(Core) and Core.Fullscreen then gleGoBack;
        FAN(Core);
        Quitting:=true;
        {$IFDEF VSE_LOG}Log(llInfo, 'Engine destroyed');
        LogRaw('');{$ENDIF}
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
        if {(ExtensionsRead and ImplementationRead) and} Assigned(Core) then begin
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
  {$IFDEF VSE_LOG}Log(llInfo, CaptionVer+' started');
  Log(llInfo, VSECaptVer);{$ENDIF}
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
    {$IFDEF VSE_LOG}Log(llError, 'Failed to register the window class');{$ENDIF}
    MessageBox(0, 'Failed to register the window class!', 'Error', MB_ICONERROR);
    Exit;
  end;
  Handle:=CreateWindowEx(WS_EX_APPWINDOW or WS_EX_WINDOWEDGE, WndClassName, PChar(Caption),
    WS_OVERLAPPED or WS_CAPTION or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,
    0, 0, 800, 600, 0, 0, hInstance, nil);
  if Handle=0 then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Unable to create window');{$ENDIF}
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
            then Core.Resume
            else Sleep(50);
  end;
  Result:=Msg.wParam=0;
  UnregisterClass(WndClassName, hInstance);
end;

initialization

finalization
  ReleaseMutex(Mutex);

end.
