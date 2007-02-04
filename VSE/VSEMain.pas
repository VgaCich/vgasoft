unit VSEMain;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, UCore, GameStates,
  OneInstance, ULog, UConsole, VSEConfig;

procedure VSEStart;

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
        Core:=TCore.Create(Handle, Initializing);
        Core.StartEngine;
        Log(llInfo, 'Engine created');
        Core.SetResolution(Core.ResX, Core.ResY, Core.Refresh, false);
        Log(llInfo, 'Init states');
        if Assigned(InitStates) then
          try
            InitStates;
          except
            LogException('in InitStates');
            MessageBox(Handle, PChar('Exception "'+ExceptObject.ClassName+'" at '+IntToHex(Cardinal(ExceptAddr), 8)+' with message "'+Exception(ExceptObject).Message+'" in InitStates'), 'Error', MB_ICONERROR);
            Core.StopEngine;
            Exit;
          end
          else begin
            Log(llError, 'InitStates() not initialized');
            MessageBox(0, 'InitStates() not initialized', 'Error', MB_ICONERROR);
            Core.StopEngine;
            Exit;
          end;
        if DoAutoexec then Console.ExecCommand('/exec autoexec');
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
        if Core.Fullscreen then gleGoBack;
        FAN(Core);
        Quitting:=true;
        LogNC(llInfo, 'Engine destroyed');
        Result:=0;
        PostQuitMessage(0);
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

procedure VSEStart;
begin
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
      if Core<>nil then
        if not Core.Minimized
          then Core.Update
          else if GetForegroundWindow=Handle
            then Core.Resume;
  end;
  if not UnregisterClass(WndClassName, hInstance) then
  begin
    LogNC(llError, 'Failed to unregister the window class');
    MessageBox(0, 'Failed to unregister the window class!', 'Error', MB_ICONERROR);
    Halt(1);
  end;
end;

end.