program VSKbdInd;

uses
  Windows, Messages, TaskBar, avlOneInstance, MenuIDs;
  
{$R *.res}
{$R Tray.res}

const
  WM_TASKBAR = WM_APP+1;
  TrayIconID = 0;
  CRLF = #13#10;
  ClassName = 'VSKeyboardIndicatorsWnd';
  AboutText = 'VgaSoft Keyboard Indicators 1.1'+CRLF+CRLF+
              'Copyright '#169' VgaSoft, 2012-2013'+CRLF+
              'vgasoft@gmail.com';
  AboutIcon = 'MAINICON';
  RegRunKey = 'Software\Microsoft\Windows\CurrentVersion\Run';
  RegRunValueName = 'VSKbdInd';

resourcestring
  AboutCaption = 'About';
  TrayIconHint = 'Num Lock: %s'+CRLF+
                 'Caps Lock: %s'+CRLF+
                 'Scroll Lock: %s';
  LEDOn = 'On';
  LEDOff = 'Off';
  NextWPHotKey = '0'; //Hotkey for Win7's "Next desktop background", =IntToStr((MOD_ALT|MOD_CONTROL|MOD_SHIFT shl 8) or VK_xxx)

var
  hWnd: THandle;
  WndClass: TWndClass;
  Msg: TMsg;
  TaskBarCreated, OldKeyState: Integer;
  TrayIconCreated: Boolean = false;
  HextWPHotKeyID: Word;

function Format(const Format: string; const Args: array of const): string;
var
  Buffer: array[0..2047] of Char;
  ElsArray, El: PDWORD;
  I: Integer;
  P: PDWORD;
begin
  ElsArray := nil;
  if High(Args) >= 0 then GetMem(ElsArray, (High(Args)+1) * SizeOf(Pointer));
  El := ElsArray;
  for I := 0 to High(Args) do
  begin
    P := @Args[I];
    P := Pointer(P^);
    El^ := DWORD(P);
    Inc( El );
  end;
  wvsprintf(@Buffer[0], PChar(Format), PChar(ElsArray));
  Result := Buffer;
  if ElsArray <> nil then FreeMem(ElsArray);
end;

function StrToInt(const S: string): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
end;

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
      lpszCaption := PWideChar(WideString(AboutCaption));
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
      lpszCaption := PAnsiChar(AboutCaption);
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectA(MsgBoxParamsA);
  end;
end;

function GetAutostartState: Boolean;
var
  Key: HKey;
  dwSize, dwType: DWORD;
begin
  Result:=false;
  if RegOpenKeyEx(HKEY_CURRENT_USER, RegRunKey, 0, KEY_READ, Key) <> ERROR_SUCCESS then Exit;
  Result:=RegQueryValueEx(Key, PChar(RegRunValueName), nil, @dwType, nil, @dwSize)=ERROR_SUCCESS;
  RegCloseKey(Key);
end;

procedure SetAutostartState(State: Boolean);
var
  Key: HKey;
  ExeName: string;
begin
  if RegOpenKeyEx(HKEY_CURRENT_USER, RegRunKey, 0, KEY_READ or KEY_SET_VALUE, Key) <> ERROR_SUCCESS then Exit;
  SetLength(ExeName, MAX_PATH);
  SetLength(ExeName, GetModuleFileName(hInstance, PAnsiChar(ExeName), MAX_PATH));
  if State
    then RegSetValueEx(Key, RegRunValueName, 0, REG_SZ, PChar(ExeName), Length(ExeName)+1)
    else RegDeleteValue(Key, RegRunValueName);
  RegCloseKey(Key);
end;

procedure PopupMenu(hWnd: THandle);
var
  Menu: hMenu;
  TrayMenu: hMenu;
  MenuItemInfo: TMenuItemInfo;
  P: TPoint;
begin
  Menu:=LoadMenu(hInstance, 'TRAYMENU');
  TrayMenu:=GetSubMenu(Menu, 0);
  with MenuItemInfo do
  begin
    cbSize:=SizeOf(MenuItemInfo);
    fMask:=MIIM_STATE;
    if GetAutostartState
      then fState:=MF_CHECKED
      else fState:=MF_UNCHECKED;
  end;
  SetMenuItemInfo(TrayMenu, ID_AUTOSTART, false, MenuItemInfo);
  GetCursorPos(P);
  SetForegroundWindow(hWnd);
  TrackPopupMenu(TrayMenu, TPM_CENTERALIGN or TPM_LEFTBUTTON, P.X, P.Y, 0, hWnd, nil);
  PostMessage(hWnd, WM_NULL, 0, 0);
  DestroyMenu(Menu);
end;

procedure CreateTrayIcon;
begin
  TrayIconCreated:=TaskBarAddIcon(hWnd, TrayIconID, LoadIcon(hInstance, 'TR000'), WM_TASKBAR, '');
  OldKeyState:=-1;
end;

procedure UpdateTrayIcon;

  function KeyStateToStr(Key: Integer): string;
  begin
    if Odd(GetKeyState(Key))
      then Result:=LEDOn
      else Result:=LEDOff;
  end;

  function KeyStateToChar(Key: Integer): Char;
  begin
    if Odd(GetKeyState(Key))
      then Result:='1'
      else Result:='0';
  end;

var
  KeyState: Integer;
  Hint, IconName: string;
begin
  if not TrayIconCreated then CreateTrayIcon;
  KeyState:=((GetKeyState(VK_NUMLOCK) and 1) shl 2) or ((GetKeyState(VK_CAPITAL) and 1) shl 1) or (GetKeyState(VK_SCROLL) and 1);
  if KeyState<>OldKeyState then
  begin
    Hint:=Format(TrayIconHint, [KeyStateToStr(VK_NUMLOCK), KeyStateToStr(VK_CAPITAL), KeyStateToStr(VK_SCROLL)]);
    IconName:='TR'+KeyStateToChar(VK_NUMLOCK)+KeyStateToChar(VK_CAPITAL)+KeyStateToChar(VK_SCROLL);
    TaskBarModifyIcon(hWnd, TrayIconID, NIF_ICON or NIF_TIP, LoadIcon(hInstance, PAnsiChar(IconName)), PAnsiChar(Hint));
    OldKeyState:=KeyState;
  end;
end;

function WindowProc(hWnd: THandle; uMsg, wParam, lParam: Integer): Integer;
  stdcall; export;
begin
  Result:=0;
  if uMsg=TaskBarCreated then CreateTrayIcon;
  case uMsg of
    WM_COMMAND:
      case wParam of
        ID_CLOSE: PostMessage(hWnd, WM_DESTROY, 0, 0);
        ID_ABOUT: ShowAboutDialog;
        ID_AUTOSTART: SetAutostartState(not GetAutostartState);
      end;
    WM_HOTKEY:
      if wParam=HextWPHotKeyID then
      begin
        hWnd:=FindWindow('SystemTray_Main', nil);
        SendMessage(hWnd, $04E7, 0, 0);
      end;
    WM_TASKBAR:
      case wParam of
        TrayIconID:
          case lParam of
            WM_RBUTTONDOWN: PopupMenu(hWnd);
          end;
      end;
    WM_TIMER: UpdateTrayIcon;
    WM_DESTROY:
      begin
        KillTimer(hWnd, 1);
        TaskBarDeleteIcon(hWnd, TrayIconID);
        PostQuitMessage(0);
        Exit;
      end;
  end;
  Result:=DefWindowProc(hWnd, uMsg, wParam, lParam);
end;

begin
  TaskBarCreated:=RegisterWindowMessage('TaskbarCreated');
  if IsRunning(ClassName) then
  begin
    hWnd:=FindWindow(Classname, nil);
    if hWnd<>0 then PostMessage(hWnd, TaskBarCreated, 0, 0);
    Exit;
  end;
  FillChar(WndClass, SizeOf(WndClass), 0);
  with WndClass do
  begin
    hInstance:=SysInit.hInstance;
    lpszClassName:=ClassName;
    lpfnWndProc:=@WindowProc;
  end;
  Windows.RegisterClass(WndClass);
  hWnd:=CreateWindow(ClassName, '', 0, 0, 0, 0, 0, 0, 0, hInstance, nil);
  if hWnd=0 then begin
    MessageBox(0, 'CreateWindow failed', nil, ID_OK);
    Exit;
  end;
  CreateTrayIcon;
  ShowWindow(hWnd, SW_HIDE);
  if NextWPHotKey <> '0' then
  begin
    HextWPHotKeyID:=GlobalAddAtom('vs.kbdind.hotkey.nextwallpaper');
    if HextWPHotKeyID<>0
      then RegisterHotKey(hWnd, HextWPHotKeyID, Hi(StrToInt(NextWPHotKey)), Lo(StrToInt(NextWPHotKey)));
  end;
  SetTimer(hWnd, 1, 500, nil);
  while GetMessage(Msg, 0, 0, 0) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
  if HextWPHotKeyID <> 0 then
  begin
    UnregisterHotKey(hWnd, HextWPHotKeyID);
    GlobalDeleteAtom(HextWPHotKeyID);
  end;
end.
