program toonelaunch;

uses
  Windows, Messages, Avl, avlUtils, Constant, TaskBar;

{$R *.RES}
{$R TrayRes.RES}

const
  WM_TASKBAR = WM_APP + 1;
  ICON_ID = 0;
  Hint: array[0..63] of Char = 'toonel.net';
  sClassName = 'VStoonelaunch';
  AboutText = 'VgaSoft toonel.net Launcher 1.0'#13 +
              'Copyright '#169' VgaSoft, 2004-2007';
  AboutCaption = 'About';
  AboutIcon = 'MAINICON';

var
  hWnd, ToonelWnd: THandle;
  WndClass: TWndClass;
  Msg: TMsg;
  TaskBarCreated: Integer;

procedure ShowAboutDialog;
var
  Version: TOSVersionInfo;
  MsgBoxParamsW: TMsgBoxParamsW;
  MsgBoxParamsA: TMsgBoxParamsA;
begin
  Version.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(Version);
  if Version.dwPlatformId = VER_PLATFORM_WIN32_NT then begin
    FillChar(MsgBoxParamsW, SizeOf(MsgBoxParamsW), #0);
    with MsgBoxParamsW do begin
      cbSize := SizeOf(MsgBoxParamsW);
      hwndOwner := hWnd;
      hInstance := SysInit.hInstance;
      lpszText  := AboutText;
      lpszCaption := AboutCaption;
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectW(MsgBoxParamsW);
  end else begin
    FillChar(MsgBoxParamsA, SizeOf(MsgBoxParamsA), #0);
    with MsgBoxParamsA do begin
      cbSize := SizeOf(MsgBoxParamsA);
      hwndOwner := hWnd;
      hInstance := SysInit.hInstance;
      lpszText  := AboutText;
      lpszCaption := AboutCaption;
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectA(MsgBoxParamsA);
  end;
end;

procedure PopupMenu(hWnd: THandle);
var
  Menu: hMenu;
  Popup: hMenu;
  P: TPoint;
begin
  Menu := LoadMenu(hInstance, 'MAINMENU');
  Popup := GetSubMenu(Menu, 0);
  GetCursorPos(P);
  SetForegroundWindow(hWnd);
  TrackPopupMenu(Popup, TPM_CENTERALIGN or TPM_LEFTBUTTON,
    P.X, P.Y, 0, hWnd, NIL);
  PostMessage(hWnd, WM_NULL, 0, 0);  
  DestroyMenu(Menu);
end;

procedure ShowToonelWnd;
begin
  if ToonelWnd=0 then Exit;
  if not ShowWindow(ToonelWnd, SW_HIDE) then
  begin
    ShowWindow(ToonelWnd, SW_SHOWNORMAL);
    SetForegroundWindow(ToonelWnd);
  end;
end;

function WindowProc(hWnd: THandle; uMsg, wParam, lParam: Integer): Integer;
  stdcall; export;
begin
  Result := 0;
  if uMsg = TaskBarCreated
    then TaskBarAddIcon(hWnd, ICON_ID, LoadIcon(hInstance, 'MAINICON'), WM_TASKBAR, Hint);
  case uMsg of
    WM_COMMAND:
      case wParam of
        ID_CLOSE: begin
                  if ToonelWnd<>0 then PostMessage(ToonelWnd, WM_CLOSE, 0, 0);
                  PostMessage(hWnd, WM_DESTROY, 0, 0);
                  end;
        ID_ABOUT: ShowAboutDialog;
        ID_TOONEL: ShowToonelWnd;
      end;
    WM_TASKBAR:
      case wParam of
        ICON_ID:
          case lParam of
            WM_RBUTTONDOWN: PopupMenu(hWnd);
            WM_LBUTTONDOWN: ShowToonelWnd;
          end;
      end;
    WM_DESTROY:
      begin
        TaskBarDeleteIcon(hWnd, ICON_ID);
        PostQuitMessage(0);
        Exit;
      end;
  end;
  Result := DefWindowProc(hWnd, uMsg, wParam, lParam);
end;

begin
  ToonelWnd:=0;
  WinExec('javaw -jar toonel.jar', SW_HIDE);
  repeat
    ToonelWnd:=FindWindow(nil, 'toonel.net');
    Sleep(100);
  until ToonelWnd<>0;
  FillChar(WndClass, SizeOf(WndClass), 0);
  with WndClass do begin
    hInstance      := SysInit.hInstance;
    lpszClassName  := sClassName;
    lpfnWndProc    := @WindowProc;
  end;
  Windows.RegisterClass(WndClass);
  hWnd := CreateWindow(sClassName, '', 0, 0, 0, 0, 0, 0, 0, hInstance, NIL);
  if hWnd = 0 then begin
    MessageBox(0, 'Initialization failed', NIL, ID_OK);
    Exit;
  end;
  TaskBarCreated := RegisterWindowMessage('TaskbarCreated');
  TaskBarAddIcon(hWnd, ICON_ID, LoadIcon(hInstance, 'MAINICON'), WM_TASKBAR, Hint);;
  ShowWindow(hWnd, SW_HIDE);
  while GetMessage(Msg, 0, 0, 0) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end.

