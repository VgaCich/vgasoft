unit VSECore;

interface

uses
  Windows, Messages, MMSystem, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions,
  VSEInit, VSEGameStates{$IFDEF VSE_LOG}, VSELog, VSESysInfo{$ENDIF};

const
  InvalidState = $FFFFFFFF; //Non-existing state index
  //Engine stop codes
  StopNormal=0; //Engine stopped normally
  //Critical error
  StopInitError=1; //Cannot initialize engine
  StopInternalError=2; //Internal engine error
  StopUserException=3; //Engine stopped due to unhandled exception in user code
  StopDisplayModeError=4; //Engine stopped due to error when setting display mode
  StopUserError=5; //Engine stopped by user code due to error
  StopCodeNames: array[StopNormal..StopUserError] of string =
    ('Normal', 'Init Error', 'Internal Error', 'User Exception', 'Display Mode Error', 'User Error');

type
  TCore=class
  private
    FHandle: THandle;
    FDC: HDC;
    FRC: HGLRC;
    FResolutionX: Cardinal;
    FResolutionY: Cardinal;
    FRefreshRate: Cardinal;
    FColorDepth: Cardinal;
    FFramesCount, FFPS, FFPSTimer, FPreviousUpdate, FUpdInt, FUpdOverloadCount, FUpdOverloadThreshold: Cardinal;
    FPerformanceFrequency: Int64;
    FStates: array of TGameState;
    FState, FSwitchTo: Cardinal;
    FCurState: TGameState;
    FPrevStateName: string;
    FFullscreen, FNeedSwitch, FMinimized, FPaused, FMouseCapture: Boolean;
    FKeyState: TKeyboardState;
    procedure SetFullscreen(Value: Boolean);
    function  GetVSync: Boolean;
    procedure SetVSync(Value: Boolean);
    procedure SetState(Value: Cardinal);
    function  GetKeyPressed(Index: Byte): Boolean;
    procedure SetMouseCapture(Value: Boolean);
    function  GetMouseCursor: TPoint;
    function  GetTime: Cardinal;
  protected
    procedure StartEngine;
    procedure SaveSettings;
    procedure Update;
    procedure Resume;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
    procedure KeyEvent(Button: Integer; Event: TKeyEvent);
    procedure CharEvent(C: Char);
    function  SendNotify(Notify: TSysNotify): Boolean;
  public
    constructor Create(WndHandle: THandle); //internally used
    destructor Destroy; override; //internally used
    procedure StopEngine(StopState: Integer = StopNormal); //Stop engine with stop code StopState and quit
    {State manager}
    function  AddState(State: TGameState): Cardinal; //Add state object, returns state index
    function  ReplaceState(OrigState: Cardinal; NewState: TGameState): Boolean; //Replace state at index OrigState with state object NewState; returns true if success
    procedure DeleteState(State: Cardinal); //Delete state, may change indices of other states
    procedure SwitchState(NewState: Cardinal); overload; //Switch to state by state index
    procedure SwitchState(const NewStateName: string); overload; //Switch to state by state name
    function  StateExists(State: Cardinal): Boolean; //Returns true if exists state with supplied index
    function  GetState(State: Cardinal): TGameState; //Returns state object by index
    function  FindState(const Name: string): Cardinal; //Returns state index by state name or InvalidState if state not found
    {Misc.}
    function  KeyRepeat(Key: Byte; Rate: Integer; var KeyVar: Cardinal): Boolean; //Returns true if Key pressed, but no more often then Rate; KeyVar - counter for rate limiting
    procedure SetResolution(ResolutionX, ResolutionY, RefreshRate: Cardinal; Fullscreen: Boolean; CanReset: Boolean = true);  //Set resolution ResX*ResY@Refresh; CanReset: return to previous resolution if fail
    procedure MakeScreenshot(Name: string; Numerate: Boolean = true); //Makes screenshot in exe folder; Name: screentshot file name; Numerate: append counter to name
    procedure ResetUpdateTimer; //Reset update timer and clear pending updates
    ///
    property Handle: THandle read FHandle; //Engine window handle
    property DC: HDC read FDC; //Engine window GDI device context
    property RC: HGLRC read FRC; //Engine window OpenGL rendering context
    property ResolutionX: Cardinal read FResolutionX; //Horizontal resolution of viewport
    property ResolutionY: Cardinal read FResolutionY; //Vertical resolution of viewport
    property RefreshRate: Cardinal read FRefreshRate; //Screen refresh rate, fullscreen only
    property ColorDepth: Cardinal read FColorDepth write FColorDepth; //Color depth, applied after engine restart
    property Fullscreen: Boolean read FFullscreen write SetFullscreen; //Fullscreen mode
    property VSync: Boolean read GetVSync write SetVSync; //Vertical synchronization
    property Minimized: Boolean read FMinimized; //Engine window minimized
    property Paused: Boolean read FPaused; //Engine paused
    property KeyPressed[Index: Byte]: Boolean read GetKeyPressed; //True if Key pressed
    property MouseCapture: Boolean read FMouseCapture write SetMouseCapture; //Mouse capture mode
    property MouseCursor: TPoint read GetMouseCursor; //Mouse cursor coordinates relative to engine window
    property Time: Cardinal read GetTime; //Current time in ms
    property State: Cardinal read FState write SetState; //Current state index
    property CurState: TGameState read FCurState; //Current state object
    property PrevStateName: string read FPrevStateName; //Name of previous state
    property FPS: Cardinal read FFPS; //Current FPS
    property UpdateInterval: Cardinal read FUpdInt write FUpdInt; //Current state updates interval
    property UpdateOverloadThreshold: Cardinal read FUpdOverloadThreshold write FUpdOverloadThreshold; //Update Overload Detection threshold, overloaded update cycles before triggering
  end;

function VSEStart: Integer; //Start engine, returns engine stop code
procedure LogException(Comment: string); //Writes current exception info to log, followed by Comment. Call only in except block

var
  Core: TCore; //Global variable for accessing to Engine Core

implementation

uses
  VSESound, VSETexMan, VSEBindMan;

const
  MinResolutionX = 640;
  MinResolutionY = 480;
  DefaultOverloadThreshold = 8;
  WndClassName: PChar = 'VSENGINE';
  WM_XBUTTONDOWN=$20B;
  WM_XBUTTONUP=$20C;

var
  Mutex: Integer=0;
  VSEStopState: Integer=StopNormal;

procedure LogErrorAndShowMessage(Msg: string);
begin
  {$IFDEF VSE_LOG}Log(llError, Msg);{$ENDIF}
  MessageBox(0, PChar(Msg), PChar(InitSettings.Caption), MB_ICONERROR);
end;

procedure LogException(Comment: string);
begin
  Comment:=Format('Exception "%s" at $%s with message "%s" %s', [string(ExceptObject.ClassName), IntToHex(Cardinal(ExceptAddr), 8), Exception(ExceptObject).Message, Comment]);
  {$IFDEF VSE_LOG}Log(llError, Comment);{$ENDIF}
  {$IFNDEF VSE_DEBUG}MessageBox(Handle, PChar(Comment), PChar(InitSettings.Caption), MB_ICONERROR){$ENDIF}
end;

procedure UpdateFPS(uID, uMsg, dwUser, dw1, dw2: Cardinal); stdcall;
begin
  Core.FFPS:=Core.FFramesCount;
  Core.FFramesCount:=0;
end;

constructor TCore.Create(WndHandle: THandle);
begin
  inherited Create;
  FPaused:=true;
  FMinimized:=false;
  FNeedSwitch:=false;
  FState:=InvalidState;
  FStates:=nil;
  FCurState:=nil;
  FHandle:=WndHandle;
  FFullscreen:=false;
  FFPS:=0;
  FFramesCount:=0;
  QueryPerformanceFrequency(FPerformanceFrequency);
  FPreviousUpdate:=0;
  FUpdOverloadCount:=0;
  FUpdOverloadThreshold:=DefaultOverloadThreshold;
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
    {$IFDEF VSE_LOG}LogException('in state '+StateName+'.Free');{$ENDIF}
    {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
  end;
  FAN(TexMan);
  FAN(Sound);
  FAN(BindMan);
  if FFullscreen then gleGoBack;
  wglMakeCurrent(FDC, 0);
  wglDeleteContext(FRC);
  if FDC>0 then ReleaseDC(FHandle, FDC);
  timeKillEvent(FFPSTimer);
  inherited Destroy;
end;

//Protected - interacting with WndProc

procedure TCore.StartEngine;
begin
  FResolutionX:=InitSettings.ResolutionX;
  FResolutionY:=InitSettings.ResolutionY;
  FRefreshRate:=InitSettings.RefreshRate;
  FColorDepth:=InitSettings.ColorDepth;
  FFullscreen:=InitSettings.Fullscreen;
  if FResolutionX<MinResolutionX then FResolutionX:=MinResolutionX;
  if FResolutionY<MinResolutionY then FResolutionY:=MinResolutionY;
  if FRefreshRate=0 then FRefreshRate:=gleGetCurrentResolution.RefreshRate;
  FDC:=GetDC(FHandle);
  FRC:=gleSetPix(FDC, FColorDepth);
  if FRC=0 then raise Exception.Create('Unable to set rendering context');
  Sound:=TSound.Create;
  TexMan:=TTexMan.Create;
  BindMan:=TBindMan.Create;
  {$IFDEF VSE_LOG}LogSysInfo;{$ENDIF}
  SetResolution(FResolutionX, FResolutionY, FRefreshRate, FFullscreen, false);
  VSync:=InitSettings.VSync;
  {$IFDEF VSE_LOG}Log(llInfo, 'States initialization');{$ENDIF}
  if not Assigned(InitSettings.InitStates) then raise Exception.Create('InitStates() are NULL');
  try
    InitSettings.InitStates;
    {$IFDEF VSE_LOG}Log(llInfo, 'States initialized');{$ENDIF}
  except
    {$IFDEF VSE_LOG}LogException('in InitStates');{$ENDIF}
    Core.StopEngine(StopUserException);
  end;
  glShadeModel(GL_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glHint(GL_POLYGON_SMOOTH, GL_NICEST);
  glHint(GL_SHADE_MODEL, GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_NORMALIZE);
  FFPSTimer:=timeSetEvent(1000, 0, @UpdateFPS, 0, TIME_PERIODIC);
  FPaused:=false;
end;

procedure TCore.SaveSettings;
begin
  Settings.Int[SSectionSettings, SNameResolutionX]:=FResolutionX;
  Settings.Int[SSectionSettings, SNameResolutionY]:=FResolutionY;
  Settings.Int[SSectionSettings, SNameRefreshRate]:=FRefreshRate;
  Settings.Int[SSectionSettings, SNameColorDepth]:=FColorDepth;
  Settings.Bool[SSectionSettings, SNameFullscreen]:=Fullscreen;
  Settings.Bool[SSectionSettings, SNameVSync]:=VSync;
end;

procedure TCore.Update;
var
  T, i, UpdTime: Cardinal;
  Cursor: TPoint;
begin
  try
    GetKeyboardState(FKeyState);
    BindMan.Update;
    if FPaused then Exit;
    if GetForegroundWindow<>FHandle then
    begin
      {$IFDEF VSE_LOG}Log(llInfo, 'Window minimized');{$ENDIF}
      FMinimized:=True;
      if FFullscreen then
      begin
        gleGoBack;
        SendMessage(FHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
      end;
      FPaused:=not SendNotify(snMinimize);
      if FPaused then
      begin
        if FNeedSwitch
          then State:=FSwitchTo;
        {TODO: Pause subsystems like sound}
        if not FFullscreen
          then SendMessage(FHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
        Exit;
      end;
    end;
    if FNeedSwitch
      then State:=FSwitchTo;
    Sound.Update;
    if FPreviousUpdate=0 then FPreviousUpdate:=Time;
    T:=Time-FPreviousUpdate;
    FPreviousUpdate:=FPreviousUpdate+T-(T mod FUpdInt);
    if FCurState<>nil then
    begin
      if FMouseCapture and not FMinimized then
      begin
        GetCursorPos(Cursor);
        Cursor.X:=Cursor.X-FResolutionX div 2;
        Cursor.Y:=Cursor.Y-FResolutionY div 2;
        SetCursorPos(FResolutionX div 2, FResolutionY div 2);
        try
          FCurState.MouseEvent(0, meMove, Cursor.X, Cursor.Y);
        except
          {$IFDEF VSE_LOG}LogException(Format('in state %s.MouseEvent(0, %s, %d, %d)', [FCurState.Name, MouseEventNames[meMove], Cursor.X, Cursor.Y]));{$ENDIF}
          {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
        end;
      end;
      for i:=1 to T div FUpdInt do
      begin
        UpdTime:=Time;
        try
          FCurState.Update;
        except
          {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.Update');{$ENDIF}
          {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
        end;
        if Time-UpdTime>FUpdInt then
        begin
          Inc(FUpdOverloadCount);
          if FUpdOverloadCount>FUpdOverloadThreshold then
            if not SendNotify(snUpdateOverload) then
            begin
              {$IFDEF VSE_LOG}Log(llWarning, 'Update overload in state "'+FCurState.Name+'"');{$ENDIF}
              ResetUpdateTimer;
            end;
        end
          else if FUpdOverloadCount>0 then Dec(FUpdOverloadCount);
      end;
      try
        FCurState.Draw;
      except
        {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.Draw');{$ENDIF}
        {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
      end;
      SwapBuffers(FDC);
      Inc(FFramesCount);
    end;
  except
    {$IFDEF VSE_LOG}LogException('in TCore.Update');{$ENDIF}
    StopEngine(StopInternalError);
  end;
end;

procedure TCore.Resume;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Window maximized');{$ENDIF}
  try
    if FFullscreen then gleGoFullscreen(ResolutionX, ResolutionY, RefreshRate, FColorDepth);
    FMinimized:=false;
    if FPaused then ResetUpdateTimer;
    FPaused:=false;
    SendNotify(snMaximize);
  except
    {$IFDEF VSE_LOG}LogException('in TCore.Resume');{$ENDIF}
    StopEngine(StopInternalError);
  end;
end;

procedure TCore.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
var
  P: TPoint;
begin
  try
    if (Event=meWheel) and not Fullscreen then
    begin
      P:=Point(X, Y);
      ScreenToClient(Handle, P);
      X:=P.X;
      Y:=P.Y;
    end;
    if Event=meDown
      then SetCapture(FHandle)
      else if Event=meUp
        then ReleaseCapture;
    if (FMouseCapture and (Event=meMove)) or FPaused then Exit;
    if Event in [meDown, meUp, meWheel] then BindMan.MouseEvent(Button, Event);
    if FCurState<>nil then
    try
      FCurState.MouseEvent(Button, Event, X, Y);
    except
      {$IFDEF VSE_LOG}LogException(Format('in state %s.MouseEvent(%d, %s, %d, %d)', [FCurState.Name, Button, MouseEventNames[Event], X, Y]));{$ENDIF}
      {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
    end;
  except
    {$IFDEF VSE_LOG}LogException(Format('in TCore.MouseEvent(%d, %s, %d, %d)', [Button, MouseEventNames[Event], X, Y]));{$ENDIF}
    StopEngine(StopInternalError);
  end;
end;

procedure TCore.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  try
    if FPaused then Exit;
    {$IFDEF VSE_ESC_EXIT}
    if (Button=VK_ESCAPE) and (Event=keUp) then
    begin
      StopEngine;
      Exit;
    end;
    {$ENDIF}
    {$IFDEF VSE_USE_SNAPSHOT_KEY}
    if (Button=VK_SNAPSHOT) and (Event=keUp) then
    begin
      MakeScreenshot('Screen');
      Exit;
    end;
    {$ENDIF}
    BindMan.KeyEvent(Button, Event);
    if FCurState<>nil then
    try
      FCurState.KeyEvent(Button, Event);
    except
      {$IFDEF VSE_LOG}LogException(Format('in state %s.KeyEvent(%d, %s)', [FCurState.Name, Button, KeyEventNames[Event]]));{$ENDIF}
      {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
    end;
  except
    {$IFDEF VSE_LOG}LogException(Format('in TCore.KeyEvent(%d, %s)', [Button, KeyEventNames[Event]]));{$ENDIF}
    StopEngine(StopInternalError);
  end;
end;

procedure TCore.CharEvent(C: Char);
begin
  try
    if FPaused then Exit;
    if FCurState<>nil then
    try
      FCurState.CharEvent(C);
    except
      {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.CharEvent(#'+IntToStr(Ord(C))+')');{$ENDIF}
      {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
    end;
  except
    {$IFDEF VSE_LOG}LogException('in TCore.CharEvent(#'+IntToStr(Ord(C))+')');{$ENDIF}
    StopEngine(StopInternalError);
  end;
end;

function TCore.SendNotify(Notify: TSysNotify): Boolean;
begin
  Result:=false;
  if FCurState<>nil then
  try
    Result:=FCurState.SysNotify(Notify);
  except
    {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.SysNotify('+SysNotifyNames[Notify]+')');{$ENDIF}
    {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
  end;
end;

//Public

procedure TCore.StopEngine(StopState: Integer);
begin
  {$IFDEF VSE_LOG}LogF(llInfo, 'Stopping engine with code %d (%s)', [StopState, StopCodeNames[StopState]]);{$ENDIF}
  VSEStopState:=StopState;
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

function TCore.AddState(State: TGameState): Cardinal;
begin
  Result:=Length(FStates);
  SetLength(FStates, Result+1);
  FStates[Result]:=State;
  {$IFDEF VSE_LOG}LogF(llInfo, 'Added state #%d %s', [Result, State.Name]);{$ENDIF}
end;

function TCore.ReplaceState(OrigState: Cardinal; NewState: TGameState): Boolean;
begin
  Result:=true;
  {$IFDEF VSE_LOG}LogF(llInfo, 'Replacing state #%d with %s', [OrigState, NewState.Name]);{$ENDIF}
  if OrigState<Length(FStates)
    then FStates[OrigState]:=NewState
    else Result:=false;
end;

procedure TCore.DeleteState(State: Cardinal);
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Deleting state #'+IntToStr(State));{$ENDIF}
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
  Result:=InvalidState;
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

procedure TCore.SetResolution(ResolutionX, ResolutionY, RefreshRate: Cardinal; Fullscreen: Boolean; CanReset: Boolean = true);
var
  OldResX, OldResY, OldRefresh: Cardinal;
begin
  OldResX:=FResolutionX;
  OldResY:=FResolutionY;
  OldRefresh:=FRefreshRate;
  FResolutionX:=ResolutionX;
  FResolutionY:=ResolutionY;
  FRefreshRate:=RefreshRate;
  {$IFDEF VSE_LOG}LogF(llInfo, 'Set resolution %dx%d@%d', [ResolutionX, ResolutionY, RefreshRate]);{$ENDIF}
  Self.Fullscreen:=Fullscreen;
  SendMessage(FHandle, WM_SIZE, 0, ResolutionY shl 16 + ResolutionX);
  if FFullscreen then
  begin
    if not gleGoFullscreen(ResolutionX, ResolutionY, RefreshRate, FColorDepth) then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'Unable to set resolution %dx%d@%d', [ResolutionX, ResolutionY, RefreshRate]);{$ENDIF}
      if CanReset
        then SetResolution(OldResX, OldResY, OldRefresh, false)
        else begin
          MessageBox(FHandle, 'Unable to set resolution! Choose lower resolution or refresh rate', PChar(InitSettings.Caption), MB_ICONERROR);
          StopEngine(StopDisplayModeError);
        end;
    end;
  end
  else SetWindowPos(FHandle, 0, (Screen.Width-FResolutionX) div 2, (Screen.Height-FResolutionY) div 2, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
  TexMan.RebuildFonts;
end;

procedure TCore.MakeScreenshot(Name: string; Numerate: Boolean = true);
var
  F: TFileStream;
  Pix: Pointer;
  BMPFH: TBitmapFileHeader;
  BMPIH: TBitmapInfoHeader;
  i: Integer;
begin
  if Numerate then
  begin
    for i:=0 to 99 do
      if (i=99) or not FileExists(ExePath+Name+IntToStrLZ(i, 2)+'.bmp') then
      begin
        Name:=ExePath+Name+IntToStrLZ(i, 2)+'.bmp';
        Break;
      end;
  end
    else Name:=ExePath+Name+'.bmp';
  GetMem(Pix, FResolutionX*FResolutionY*3);
  try
    glReadPixels(0, 0, FResolutionX, FResolutionY, GL_BGR, GL_UNSIGNED_BYTE, Pix);
    F:=TFileStream.Create(Name, fmCreate);
    with BMPFH, BMPIH do
    begin
      bfType:=$4D42;
      bfSize:=FResolutionX*FResolutionY*3+SizeOf(BMPFH)+SizeOf(BMPIH);
      bfReserved1:=0;
      bfReserved2:=0;
      bfOffBits:=SizeOf(BMPFH)+SizeOf(BMPIH);
      biSize:=SizeOf(BMPIH);
      biWidth:=FResolutionX;
      biHeight:=FResolutionY;
      biPlanes:=1;
      biBitCount:=24;
      biCompression:=0;
      biSizeImage:=FResolutionX*FResolutionY*3;
      biXPelsPerMeter:=0;
      biYPelsPerMeter:=0;
      biClrUsed:=0;
      biClrImportant:=0;
    end;
    F.Write(BMPFH, SizeOf(BMPFH));
    F.Write(BMPIH, SizeOf(BMPIH));
    F.Write(Pix^, FResolutionX*FResolutionY*3);
    {$IFDEF VSE_LOG}Log(llInfo, 'Screenshot saved to "'+Name+'"');{$ENDIF}
  finally
    FreeMem(Pix);
    FAN(F);
  end;
end;

procedure TCore.ResetUpdateTimer;
begin
  FPreviousUpdate:=Time;
end;

//Private

procedure TCore.SetFullscreen(Value: Boolean);
begin
  if FFullscreen=Value then Exit;
  FFullscreen:=Value;
  if Value
    then FFullscreen:=gleGoFullscreen(FResolutionX, FResolutionY, FRefreshRate, FColorDepth)
    else gleGoBack;
  {$IFDEF VSE_LOG}if FFullscreen<>Value then Log(llError, 'Unable to enter fullscreen! Choose lower resolution or refresh rate');{$ENDIF}
  if FFullscreen then
  begin
    SetWindowLong(FHandle, GWL_EXSTYLE, WS_EX_APPWINDOW);
    SetWindowLong(FHandle, GWL_STYLE, Integer(WS_POPUP) or WS_CLIPCHILDREN or WS_CLIPSIBLINGS);
    SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_SHOWWINDOW);
  end
  else begin
    SetWindowLong(FHandle, GWL_EXSTYLE, WS_EX_APPWINDOW or WS_EX_WINDOWEDGE);
    SetWindowLong(FHandle, GWL_STYLE, WS_OVERLAPPED or WS_CAPTION or WS_CLIPCHILDREN or WS_CLIPSIBLINGS);
    SetWindowPos(FHandle, HWND_NOTOPMOST, (Screen.Width-FResolutionX) div 2, (Screen.Height-FResolutionY) div 2, 0, 0, SWP_NOSIZE or SWP_FRAMECHANGED or SWP_SHOWWINDOW);
  end;
end;

function TCore.GetVSync: Boolean;
begin
  Result:=false;
  if WGL_EXT_swap_control then Result:=wglGetSwapIntervalEXT<>0;
end;

procedure TCore.SetVSync(Value: Boolean);
const
  VSync: array[Boolean] of Integer=(0, 1);
begin
  if WGL_EXT_swap_control then wglSwapIntervalEXT(VSync[Value]);
end;

procedure TCore.SetState(Value: Cardinal);
begin
  {$IFDEF VSE_LOG}if StateExists(FState) and StateExists(Value)
    then LogF(llInfo, 'Switch state from %s to %s', [FStates[FState].Name, FStates[Value].Name])
    else if StateExists(Value)
      then Log(llInfo, 'Switch state to '+FStates[Value].Name);{$ENDIF}
  FNeedSwitch:=false;
  if (FState=Value) or (Value>High(FStates)) then Exit;
  if FCurState<>nil then
  begin
    try
      FCurState.Deactivate;
    except
      {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.Deactivate');{$ENDIF}
      {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
    end;
    FPrevStateName:=FCurState.Name;
  end;
  BindMan.ResetEvents;
  FState:=Value;
  FCurState:=FStates[Value];
  try
    FUpdInt:=FCurState.Activate;
  except
    if FUpdInt<=1 then FUpdInt:=50;
    {$IFDEF VSE_LOG}LogException('in state '+FCurState.Name+'.Activate');{$ENDIF}
    {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
  end;
end;

function TCore.GetKeyPressed(Index: Byte): Boolean;
begin
  Result:=(FKeyState[Index]>127) and not FMinimized;
end;

procedure TCore.SetMouseCapture(Value: Boolean);
begin
  if Value=FMouseCapture then Exit;
  FMouseCapture:=Value;
  if Value then
  begin
    SetCursorPos(FResolutionX div 2, FResolutionY div 2);
    SetCapture(FHandle);
    ShowCursor(false);
  end
  else begin
    ReleaseCapture;
    ShowCursor(true);
  end;
end;

function TCore.GetMouseCursor: TPoint;
begin
  GetCursorPos(Result);
  ScreenToClient(FHandle, Result);
end;

function TCore.GetTime: Cardinal;
var
  T: Int64;
begin
  QueryPerformanceCounter(T);
  Result:=Trunc(1000*(T/FPerformanceFrequency));
end;

function WndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  WndWidth, WndHeight: Word;
begin
  Result:=0;
  case (Msg) of
    WM_ACTIVATE:
      begin
        if not Assigned(Core) then
        begin
          {$IFDEF VSE_LOG}Log(llInfo, 'Creating engine');
          {$IFDEF VSE_DEBUG}Log(llInfo, 'Debug mode');{$ENDIF}{$ENDIF}
          try
            Core:=TCore.Create(hWnd);
            Core.StartEngine;
            {$IFDEF VSE_LOG}Log(llInfo, 'Engine created');{$ENDIF}
          except
            {$IFDEF VSE_LOG}LogException('while initializing engine');{$ENDIF}
            VSEStopState:=StopInitError;
            SendMessage(hWnd, WM_CLOSE, 0, 0);
          end;
        end;
      end;
    WM_KEYUP: Core.KeyEvent(wParam, keUp);
    WM_KEYDOWN: Core.KeyEvent(wParam, keDown);
    WM_CHAR: Core.CharEvent(Chr(wParam));
    WM_MOUSEMOVE: Core.MouseEvent(0, meMove, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_LBUTTONDOWN: Core.MouseEvent(mbLeft, meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_LBUTTONUP: Core.MouseEvent(mbLeft, meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_RBUTTONDOWN: Core.MouseEvent(mbRight, meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_RBUTTONUP: Core.MouseEvent(mbRight, meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_MBUTTONDOWN: Core.MouseEvent(mbMiddle, meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_MBUTTONUP: Core.MouseEvent(mbMiddle, meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_XBUTTONDOWN: Core.MouseEvent(3+HiWord(wParam), meDown, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_XBUTTONUP: Core.MouseEvent(3+HiWord(wParam), meUp, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_MOUSEWHEEL: Core.MouseEvent(SmallInt(HiWord(wParam)) div 120, meWheel, SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
    WM_DESTROY:
      begin
        {$IFDEF VSE_LOG}Log(llInfo, 'Destroying engine');{$ENDIF}
        try
          FAN(Core);
          {$IFDEF VSE_LOG}Log(llInfo, 'Engine destroyed');{$ENDIF}
        except
          {$IFDEF VSE_LOG}LogException('while destroying engine');{$ENDIF}
          VSEStopState:=StopInternalError;
        end;
        PostQuitMessage(VSEStopState);
        Result:=0;
      end;
    WM_QUERYENDSESSION:
      begin
        {$IFDEF VSE_LOG}Log(llInfo, 'Received WM_QUERYENDSESSION');{$ENDIF}
        Core.StopEngine;
        Result:=1;
      end;
    WM_SIZE:
      try
        if Assigned(Core) then begin
          if Core.Fullscreen then
          begin
            WndWidth:=Core.ResolutionX;
            WndHeight:=Core.ResolutionY;
          end
          else begin
            WndWidth:=Core.ResolutionX+GetSystemMetrics(SM_CXDLGFRAME)*2;
            WndHeight:=Core.ResolutionY+GetSystemMetrics(SM_CYCAPTION)+GetSystemMetrics(SM_CYDLGFRAME)*2;
          end;
          gleResizeWnd(Core.ResolutionX, Core.ResolutionY);
          if Core.Fullscreen
            then SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, WndWidth, WndHeight, 0)
            else SetWindowPos(hWnd, HWND_TOP, 0, 0, WndWidth, WndHeight, SWP_NOMOVE or SWP_FRAMECHANGED);
        end;
        Result:=0;
      except
        {$IFDEF VSE_LOG}LogException('while resizing window');{$ENDIF}
        {$IFNDEF VSE_DEBUG}StopEngine(StopInternalError);{$ENDIF}
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

function VSEStart: Integer;
var
  WndClass: TWndClass;
  Handle: THandle;
  Msg: TMsg;
  Fin: Boolean;
begin
  Result:=StopInitError;
  if IsRunning(InitSettings.Caption) then Exit;
  {$IFDEF VSE_LOG}Log(llInfo, InitSettings.Caption+' '+InitSettings.Version+' started');
  Log(llInfo, VSECaptVer);{$ENDIF}
  Fin:=false;
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
    LogErrorAndShowMessage('Failed to register the window class');
    Exit;
  end;
  Handle:=CreateWindowEx(WS_EX_APPWINDOW or WS_EX_WINDOWEDGE, WndClassName, PChar(InitSettings.Caption),
    WS_OVERLAPPED or WS_CAPTION or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,
    0, 0, 800, 600, 0, 0, hInstance, nil);
  if Handle=0 then
  begin
    LogErrorAndShowMessage('Unable to create window');
    Exit;
  end;
  SendMessage(Handle, WM_SETICON, 1, LoadIcon(hInstance, 'MAINICON'));
  ShowWindow(Handle, SW_SHOW);
  try
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
        begin
          if Core.Minimized and (GetForegroundWindow=Handle)
            then Core.Resume;
          if Core.Paused
            then Sleep(50)
            else Core.Update;
        end;
    end;
    Result:=Msg.wParam;
  except
    {$IFDEF VSE_LOG}LogException('in main loop');{$ENDIF}
    Result:=StopInternalError;
  end;
  if Result<>StopNormal then
  begin
    LogF(llInfo, 'Engine stopped with error code %d (%s)', [Result, StopCodeNames[Result]]);
  end;
  UnregisterClass(WndClassName, hInstance);
end;

initialization

finalization
  ReleaseMutex(Mutex);

end.
