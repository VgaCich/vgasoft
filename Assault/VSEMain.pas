unit VSEMain;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, UGame, GameStates,
  OneInstance, ULog, UConsole, States;

procedure VSEStart(const ACaption, AVersion: string);

implementation

const
  VSECaptVer='VgaSoft Engine 0.1';
  WndClassName: PChar = 'VSENGINE';
  WM_XBUTTONDOWN=$20B;
  WM_XBUTTONUP=$20C;

var
  WndClass: TWndClass;
  Handle: THandle;
  Msg: TMsg;
  Fin, Initializing: Boolean;

function WndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  WndWidth, WndHeight: Word;
begin
  Result:=0;
  case (Msg) of
    WM_ACTIVATE:
      begin
        if Initializing or (Game<>nil) then Exit;
        Log(llInfo, 'Creating engine');
        {$IFDEF VSEDEBUG}Log(llInfo, 'Debug mode');{$ENDIF}
        Game:=TGame.Create(Handle, Initializing);
        Game.StartEngine;
        Log(llInfo, 'Engine created');
        Game.SetResolution(Game.ResX, Game.ResY, Game.Refresh, false);
        InitStates;
        Console.ExecCommand('/exec autoexec');
      end;
    WM_KEYUP: Game.KeyEvent(wParam, keUp);
    WM_KEYDOWN: Game.KeyEvent(wParam, keDown);
    WM_CHAR: Game.CharEvent(Chr(wParam));
    WM_MOUSEMOVE: Game.MouseEvent(0, meMove, LoWord(lParam), HiWord(lParam));
    WM_LBUTTONDOWN: Game.MouseEvent(1, meDown, LoWord(lParam), HiWord(lParam));
    WM_LBUTTONUP: Game.MouseEvent(1, meUp, LoWord(lParam), HiWord(lParam));
    WM_RBUTTONDOWN: Game.MouseEvent(2, meDown, LoWord(lParam), HiWord(lParam));
    WM_RBUTTONUP: Game.MouseEvent(2, meUp, LoWord(lParam), HiWord(lParam));
    WM_MBUTTONDOWN: Game.MouseEvent(3, meDown, LoWord(lParam), HiWord(lParam));
    WM_MBUTTONUP: Game.MouseEvent(3, meUp, LoWord(lParam), HiWord(lParam));
    WM_XBUTTONDOWN: Game.MouseEvent(3+HiWord(wParam), meDown, LoWord(lParam), HiWord(lParam));
    WM_XBUTTONUP: Game.MouseEvent(3+HiWord(wParam), meUp, LoWord(lParam), HiWord(lParam));
    WM_MOUSEWHEEL: Game.MouseEvent(SmallInt(HiWord(wParam)) div 120, meWheel, LoWord(lParam), HiWord(lParam));
    WM_CLOSE:
      begin
        if Game.Fullscreen then gleGoBack;
        FAN(Game);
        LogNC(llInfo, 'Engine destroyed');
        Result:=0;
        PostQuitMessage(0);
      end;
    WM_QUERYENDSESSION:
      begin
        Game.StopEngine;
        Result:=1;
      end;
    WM_SIZE:
      begin
        if (ExtensionsRead and ImplementationRead) and Assigned(Game) then begin
          if Game.Fullscreen then
          begin
            WndWidth:=Game.ResX;
            WndHeight:=Game.ResY;
          end
          else begin
            WndWidth:=Game.ResX+GetSystemMetrics(SM_CXDLGFRAME)*2;
            WndHeight:=Game.ResY+GetSystemMetrics(SM_CYCAPTION)+GetSystemMetrics(SM_CYDLGFRAME)*2;
          end;
          gleResizeWnd(Game.ResX, Game.ResY);
          if Game.Fullscreen
            then SetWindowPos(Handle, HWND_TOPMOST, 0, 0, WndWidth, WndHeight, 0)
            else SetWindowPos(Handle, HWND_TOP, 0, 0, WndWidth, WndHeight, SWP_NOMOVE or SWP_FRAMECHANGED);
        end;
        Result:=0;
      end;
    else
      Result:=DefWindowProc(hWnd, Msg, wParam, lParam);
  end;
end;

procedure VSEStart(const ACaption, AVersion: string);
begin
  Caption:=ACaption;
  Version:=AVersion;
  CaptionVer:=Caption+' '+Version;
  if IsRunning(Caption) then Exit;
  Log(llInfo, CaptionVer+' started');
  Log(llInfo, VSECaptVer);
  Initializing:=false;
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
    LogNC(llError, 'Failed to register the window class');
    MessageBox(0, 'Failed to register the window class!', 'Error', MB_ICONERROR);
    Halt(1);
  end;
  Handle:=CreateWindowEx(WS_EX_APPWINDOW or WS_EX_WINDOWEDGE, WndClassName, PChar(Caption),
    WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_CLIPCHILDREN or WS_CLIPSIBLINGS,
    0, 0, 800, 600, 0, 0, hInstance, nil);
  if Handle=0 then
  begin
    LogNC(llError, 'Unable to create window');
    MessageBox(0, 'Unable to create window!', 'Error', MB_ICONERROR);
    Halt(1);
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
      if Game<>nil then
        if not Game.Minimized
          then Game.Update
          else if GetForegroundWindow=Handle
            then Game.Resume;
  end;
  if not UnregisterClass(WndClassName, hInstance)
    then LogNC(llError, 'Failed to unregister window class');
end;

end.