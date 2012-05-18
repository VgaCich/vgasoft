unit TaskBar;

interface

uses Windows, ShellAPI;

const
  // ��������� �������� �� ShellAPI, ����� �� ������ ��� � Uses
  // �����, ��� �� ���������� ���� ������
  NIF_TIP  = ShellAPI.NIF_TIP;
  NIF_ICON = ShellAPI.NIF_ICON;

function TaskBarAddIcon(
  hWindow: THandle;            // ����, ��������� ������
  ID: Cardinal;                // ������������� ������
  ICON: hIcon;                 // ������
  CallbackMessage: Cardinal;   // ���������, ������� ����� ���������� ����
  Tip: PChar                   // ToolTip
): Boolean;

function TaskBarModifyIcon(
  hWindow: THandle;
  ID: Cardinal;
  Flags: Cardinal;
  ICON: hIcon;
  Tip: PChar): Boolean;

function TaskBarDeleteIcon(
 hWindow: THandle;
 ID: Integer): Boolean;

implementation

function TaskBarAddIcon(
  hWindow: THandle;
  ID: Cardinal;
  ICON: hIcon;
  CallbackMessage: Cardinal;
  Tip: PChar): Boolean;
var
  NID: TNotifyIconData;
begin
  FillChar(NID, SizeOf(TNotifyIconData), 0);
  with NID do begin
    cbSize := SizeOf(TNotifyIconData);
    Wnd   := hWindow;
    uID    := ID;
    uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    uCallbackMessage := CallbackMessage;
    hIcon  := Icon;
    lstrcpyn(szTip, Tip, SizeOf(szTip));
  end;
  Result := Shell_NotifyIcon(NIM_ADD, @NID);
end;

function TaskBarModifyIcon(
  hWindow: THandle;
  ID: Cardinal;
  Flags: Cardinal;
  ICON: hIcon;
  Tip: PChar): Boolean;
var
  NID: TNotifyIconData;
begin
  FillChar(NID, SizeOf(TNotifyIconData), 0);
  with NID do begin
    cbSize := SizeOf(TNotifyIconData);
    Wnd    := hWindow;
    uID    := ID;
    uFlags := Flags;
    hIcon  := Icon;
    lstrcpyn(szTip, Tip, SizeOf(szTip));
  end;
  Result := Shell_NotifyIcon(NIM_MODIFY, @NID);
end;

function TaskBarDeleteIcon(
  hWindow: THandle;
  ID: Integer): Boolean;
var
  NID: TNotifyIconData;
begin
  FillChar(NID, SizeOf(TNotifyIconData), 0);
  with NID do begin
    cbSize := SizeOf(TNotifyIconData);
    Wnd    := hWindow;
    uID    := ID;
  end;
  Result := Shell_NotifyIcon(NIM_DELETE, @NID);
end;

end.
