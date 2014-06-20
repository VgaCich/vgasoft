unit TrayIcon;

interface

uses Windows, Messages, ShellAPI;

const
  WM_TASKBAR = WM_APP+1;

type
  TTrayIcon = class
  private
    FIconCreated: Boolean;
    FData: TNotifyIconData;
    FPrevIcon: TTrayIcon;
    procedure CreateIcon;
    function GetIcon: hIcon;
    function GetID: Cardinal;
    function GetTip: string;
    procedure SetIcon(const Value: hIcon);
    procedure SetTip(const Value: string);
  public
    constructor Create(ParentWindow: THandle);
    destructor Destroy; override;
    procedure Update;
    property Icon: hIcon read GetIcon write SetIcon;
    property ID: Cardinal read GetID;
    property Tip: string read GetTip write SetTip;
  end;

procedure RecreateTrayIcons;

implementation

var
  NextIconID: Cardinal = 0;
  RootIcon: TTrayIcon = nil;

procedure RecreateTrayIcons;
var
  Icon: TTrayIcon;
begin
  Icon:=RootIcon;
  while Assigned(Icon) do
  begin
    Icon.CreateIcon;
    Icon:=Icon.FPrevIcon;
  end;
end;

constructor TTrayIcon.Create(ParentWindow: THandle);
begin
  FIconCreated:=false;
  FillChar(FData, SizeOf(FData), 0);
  with FData do
  begin
    cbSize:=SizeOf(FData);
    Wnd:=ParentWindow;
    uID:=NextIconId;
    uCallbackMessage:=WM_TASKBAR;
  end;
  Inc(NextIconID);
  FPrevIcon:=RootIcon;
  RootIcon:=Self;
end;

procedure TTrayIcon.CreateIcon;
begin
  FData.uFlags:=FData.uFlags or NIF_MESSAGE or NIF_ICON or NIF_TIP;
  if Shell_NotifyIcon(NIM_ADD, @FData) then
  begin
    FData.uFlags:=0;
    FIconCreated:=true;
  end;
end;

destructor TTrayIcon.Destroy;
begin
  if FIconCreated then Shell_NotifyIcon(NIM_DELETE, @FData);
  inherited;
end;

function TTrayIcon.GetIcon: hIcon;
begin
  Result:=FData.hIcon;
end;

function TTrayIcon.GetID: Cardinal;
begin
  Result:=FData.uID;
end;

function TTrayIcon.GetTip: string;
begin
  Result:=string(FData.szTip);
end;

procedure TTrayIcon.SetIcon(const Value: hIcon);
begin
  with FData do
  begin
    hIcon:=Value;
    uFlags:=uFlags or NIF_ICON;
  end;
end;

procedure TTrayIcon.SetTip(const Value: string);
begin
  with FData do
  begin
    lstrcpyn(szTip, PChar(Value), Length(Value)+1);
    uFlags:=uFlags or NIF_TIP;
  end;
end;

procedure TTrayIcon.Update;
begin
  if FIconCreated then
  begin
    Shell_NotifyIcon(NIM_MODIFY, @FData);
    FData.uFlags:=0;
  end
    else CreateIcon;  
end;

end.
