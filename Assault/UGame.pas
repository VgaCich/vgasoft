unit UGame;

interface

uses
  Windows, Messages, MMSystem, AvL, avlUtils, dglOpenGL, OpenGLExt, Textures, PakMan,
  GameStates, UConsole, UConsoleVariables, ULog, SysInfo, USound, ifps3, ifpscomp;

type
  TGame=class
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
    FFullscreen, FNeedSwitch, FMinimized: Boolean;
    FConfig: TIniFile;
    FKeyState: TKeyboardState;
    FPakMan: TPakMan;
    FSound: TSound;
    FIFPSExec: TIFPSExec;
    FIFPSComp: TIFPSPascalCompiler;
    procedure SetFullscreen(Value: Boolean);
    procedure SetState(Value: Cardinal);
    function  GetKeyPressed(Index: Byte): Boolean;
    function  GetTime: Cardinal;
    procedure LoadFonts;
  protected
    procedure RegisterCommands;
    function  Quit(const Args: string): Boolean;
    function  Resolution(const Args: string): Boolean;
    function  CmdFullscreen(const Args: string): Boolean;
  public
    constructor Create(WndHandle: THandle; var Initializing: Boolean);
    destructor Destroy; override;
    ///
    procedure StartEngine;
    procedure StopEngine;
    procedure SaveSettings;
    procedure Update;
    procedure Resume;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
    procedure KeyEvent(Button: Integer; Event: TKeyEvent);
    procedure CharEvent(C: Char);
    function  AddState(State: TGameState): Cardinal;
    function  ReplaceState(OrigState: Cardinal; NewState: TGameState): Boolean;
    procedure DeleteState(State: Cardinal);
    procedure SwitchState(NewState: Cardinal);
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
    property FPS: Cardinal read FFPS;
    property PakMan: TPakMan read FPakMan;
    property Sound: TSound read FSound;
    property UpdateInterval: Cardinal read FUpdInt write FUpdInt;
    property UpdateOverloadThreshold: Cardinal read FUpdOverloadThreshold write FUpdOverloadThreshold;
  end;

function GetCursorPos(var lpPoint: TPoint): Boolean;

var
  Game: TGame;
  Caption, Version, CaptionVer: string;

const
  BaseDir='Data';

implementation

function GetCursorPos(var lpPoint: TPoint): Boolean;
var
  Rect: TRect;
begin
  Result:=Windows.GetCursorPos(lpPoint);
  GetWindowRect(Game.Handle, Rect);
  if not Game.Fullscreen then
  begin
    lpPoint.X:=lpPoint.X-Rect.Left-GetSystemMetrics(SM_CXDLGFRAME);
    lpPoint.Y:=lpPoint.Y-Rect.Top-GetSystemMetrics(SM_CYCAPTION)-GetSystemMetrics(SM_CYDLGFRAME);
  end;
end;

procedure UpdateFPS(uID, uMsg, dwUser, dw1, dw2: Cardinal); stdcall;
begin
  Game.FFPS:=Game.FFramesCount;
  Game.FFramesCount:=0;
end;

constructor TGame.Create(WndHandle: THandle; var Initializing: Boolean);
begin
  Initializing:=true;
  inherited Create;
  FMinimized:=false;
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

destructor TGame.Destroy;
var
  GSI: Cardinal;
begin
  SaveSettings;
  for GSI:=0 to High(FStates)-1 do
    FAN(FStates[GSI]);
  FAN(FIFPSExec);
  FAN(FIFPSComp);
  FAN(FSound);
  FAN(FPakMan);
  gleFreeFonts;
  wglMakeCurrent(FDC, 0);
  wglDeleteContext(FRC);
  if FDC>0 then ReleaseDC(FHandle, FDC);
  ShowCursor(true);
  FAN(FConfig);
  timeKillEvent(FFPSTimer);
  inherited Destroy;
end;

//Public

procedure TGame.StartEngine;
begin
  FConfig:=TIniFile.Create(ExePath+'Assault.ini');
  FResX:=FConfig.ReadInteger('Settings', 'ResX', 800);
  FResY:=FConfig.ReadInteger('Settings', 'ResY', 600);
  FRefresh:=FConfig.ReadInteger('Settings', 'Refresh', 60);
  FDepth:=FConfig.ReadInteger('Settings', 'Depth', 32);
  Fullscreen:=FConfig.ReadBool('Settings', 'Fullscreen', false);
  FVSync:=FConfig.ReadInteger('Settings', 'VSync', 1);
  if (FVSync<>0) and (FVSync<>1) then FVSync:=1;
  if FResX<640 then FResX:=640;
  if FResY<480 then FResY:=480;
  if FRefresh<60 then Frefresh:=60;
  FDC:=GetDC(FHandle);
  FRC:=gleSetPix(FDC, FDepth);
  LogSysInfo;
  glShadeModel(GL_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glHint(GL_POLYGON_SMOOTH, GL_NICEST);
  glHint(GL_SHADE_MODEL, GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_NORMALIZE);
  FPakMan:=TPakMan.Create(ExePath+BaseDir);
  Console.PakMan:=FPakMan;
  FSound:=TSound.Create(FConfig.ReadString('Settings', 'SoundDevice', 'Default'));
  FIFPSComp:=TIFPSPascalCompiler.Create;
  FIFPSExec:=TIFPSExec.Create;
  LoadFonts;
  FFPSTimer:=timeSetEvent(1000, 0, @UpdateFPS, 0, TIME_PERIODIC);
  RegisterCommands;
end;

procedure TGame.StopEngine;
begin
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TGame.SaveSettings;
begin
  FConfig.WriteInteger('Settings', 'ResX', FResX);
  FConfig.WriteInteger('Settings', 'ResY', FResY);
  FConfig.WriteInteger('Settings', 'Refresh', FRefresh);
  FConfig.WriteInteger('Settings', 'Depth', FDepth);
  FConfig.WriteBool('Settings', 'Fullscreen', FFullscreen);
  FConfig.WriteInteger('Settings', 'VSync', FVSync);
  FConfig.WriteString('Settings', 'SoundDevice', Sound.DeviceName);
end;

procedure TGame.Update;
var
  T, i, UpdTime: Cardinal;
begin
  GetKeyboardState(FKeyState);
  if FMinimized then Exit;
  if GetForegroundWindow<>FHandle then
  begin
    Log('Minimized');
    FMinimized:=True;
    Sound.Pause;
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
      FCurState.Update;
    if Time-UpdTime>T
      then Inc(FUpdOverloadCount, T div FUpdInt)
      else FUpdOverloadCount:=0;
    if (FUpdOverloadThreshold>0) and (FUpdOverloadCount>FUpdOverloadThreshold)
      then FCurState.SysNotify(snUpdateOverload);
    FCurState.Draw;
    if WGL_EXT_swap_control and (FVSync<>wglGetSwapIntervalEXT)
      then wglSwapIntervalEXT(FVSync);
    SwapBuffers(FDC);
    Inc(FFramesCount);
  end;
end;

procedure TGame.Resume;
begin
  Log('Maximized');
  if FFullscreen then gleGoFullscreen(ResX, ResY, Refresh, FDepth);
  FMinimized:=false;
  if FCurState<>nil then FCurState.Resume;
  Sound.Start;
end;

procedure TGame.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
var
  Rect: TRect;
begin
  GetWindowRect(Game.Handle, Rect);
  if (Event=meWheel) and not Game.Fullscreen then
  begin
    X:=X-Rect.Left-GetSystemMetrics(SM_CXDLGFRAME);
    Y:=Y-Rect.Top-GetSystemMetrics(SM_CYCAPTION)-GetSystemMetrics(SM_CYDLGFRAME);
  end;
  if Event=meDown
    then SetCapture(FHandle)
    else if Event=meUp
      then ReleaseCapture;
  if FCurState<>nil then FCurState.MouseEvent(Button, Event, X, Y);
end;

procedure TGame.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if (Button=27) and (Event=keUp) then
  begin
    StopEngine;
    Exit;
  end;
  if FCurState<>nil then FCurState.KeyEvent(Button, Event);
end;

procedure TGame.CharEvent(C: Char);
begin
  if FCurState<>nil then FCurState.CharEvent(C);
end;

function TGame.AddState(State: TGameState): Cardinal;
begin
  Result:=Length(FStates);
  SetLength(FStates, Result+1);
  FStates[Result]:=State;
end;

function TGame.ReplaceState(OrigState: Cardinal; NewState: TGameState): Boolean;
begin
  Result:=true;
  if OrigState<Length(FStates)
    then FStates[OrigState]:=NewState
    else Result:=false;
end;

procedure TGame.DeleteState(State: Cardinal);
begin
  if State<Length(FStates) then
  begin
    if State<High(FStates)
      then Move(FStates[State+1], FStates[State], (Length(FStates)-State-1)*SizeOf(TGameState));
    SetLength(FStates, Length(FStates)-1);
  end;
end;

procedure TGame.SwitchState(NewState: Cardinal);
begin
  FSwitchTo:=NewState;
  FNeedSwitch:=true;
end;

function TGame.StateExists(State: Cardinal): Boolean;
begin
  Result:=(State<Length(FStates)) and Assigned(FStates[State]);
end;

function TGame.GetState(State: Cardinal): TGameState;
begin
  if State<Length(FStates)
    then Result:=FStates[State]
    else Result:=nil;
end;

function TGame.FindState(const Name: string): Cardinal;
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

function TGame.KeyRepeat(Key: Byte; Rate: Integer; var KeyVar: Cardinal): Boolean;
var
  T: Cardinal;
begin
  Result:=false;
  if Game.KeyPressed[Key] then
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

procedure TGame.SetResolution(ResX, ResY, Refresh: Cardinal; CanReset: Boolean);
var
  OldResX, OldResY, OldRefresh: Cardinal;
begin
  OldResX:=FResX;
  OldResY:=FResY;
  OldRefresh:=FRefresh;
  FResX:=ResX;
  FResY:=ResY;
  FRefresh:=Refresh;
  LogF('Set resolution %dx%d@%d', [ResX, ResY, Refresh]);
  SendMessage(FHandle, WM_SIZE, 0, ResY shl 16 + ResX);
  if FFullscreen then
  begin
    if not gleGoFullscreen(ResX, ResY, Refresh, FDepth) then
    begin
      Log('Unable to enter fullscreen! Choose lower resolution or refresh rate');
      if CanReset
        then SetResolution(OldResX, OldResY, OldRefresh, false)
        else begin
          MessageBox(FHandle, 'Unable to enter fullscreen! Choose lower resolution or refresh rate', PChar(CaptionVer), MB_ICONERROR);
          Halt(1);
        end;
    end;
  end
  else SetWindowPos(FHandle, 0, (Screen.Width-FResX) div 2, (Screen.Height-FResY) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE); 
end;

//Private

procedure TGame.SetFullscreen(Value: Boolean);
begin
  if FFullscreen=Value then Exit;
  FFullscreen:=Value;
  if Value
    then gleGoFullscreen(FResX, FResY, FRefresh, FDepth)
    else gleGoBack;
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

procedure TGame.SetState(Value: Cardinal);
begin
  if StateExists(FState) and StateExists(Value)
    then LogFNC('Switch state from %s to %s', [FStates[FState].Name, FStates[Value].Name]);
  FNeedSwitch:=false;
  if (FState=Value) or (Value>High(FStates)) then Exit;
  if FCurState<>nil then FCurState.Deactivate;
  FState:=Value;
  FCurState:=FStates[Value];
  FUpdInt:=FCurState.Activate;
end;

function TGame.GetKeyPressed(Index: Byte): Boolean;
begin
  Result:=FKeyState[Index]>127;
end;

function TGame.GetTime: Cardinal;
var
  T: Int64;
begin
  QueryPerformanceCounter(T);
  Result:=Trunc(1000*T/FPerformanceFrequency);
end;

procedure TGame.LoadFonts;

  procedure LoadFont(const Name, ID: string);
  var
    Data: TStream;
  begin
    Data:=PakMan.OpenFile(Name, ofNoCreate);
    try
      if not gleLoadFont(ID, Data) then LogF('Could''n load font %s(%s)!', [ID, Name]);
    finally
      PakMan.CloseFile(Data);
    end;
  end;

var
  FntList: TStringList;
  FntListFile: TStream;
  i, P: Integer;
begin
  Log('Loading fonts');
  FntList:=TStringList.Create;
  try
    FntListFile:=PakMan.OpenFile('Fonts.ini', 0);
    FntList.LoadFromStream(FntListFile);
    PakMan.CloseFile(FntListFile);
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
           '  Quits the game';
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

procedure TGame.RegisterCommands;
begin
  Console.RegisterCommand('quit', QuitHelp, Quit);
  Console.RegisterCommand('resolution', ResolutionHelp, Resolution);
  Console.RegisterCommand('fullscreen', FullscreenHelp, CmdFullscreen);
  RegisterCVI('vsync', VSyncHelp, @FVSync, 0, 1, 1, false);
  RegisterCVI('fps', FPSHelp, @FFPS, 0, 0, 0, true);
end;

function TGame.Quit(const Args: string): Boolean;
begin
  Result:=true;
  StopEngine;
end;

function TGame.Resolution(const Args: string): Boolean;
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
    Console.AddToConsole('Too small resolution');
    Exit;
  end;
  if Refresh=0 then Refresh:=FRefresh;
  SetResolution(ResX, ResY, Refresh, true);
  Result:=true;
end;

function TGame.CmdFullscreen(const Args: string): Boolean;
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

end.
