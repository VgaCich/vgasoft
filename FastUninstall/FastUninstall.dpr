program FastUninstall;

uses
  Windows, Messages, AvL, avlUtils;

{$R *.res}
{$R FastUninstallRes.res}

const
  TVN_FIRST=0-400;
  TVN_SELCHANGED=TVN_FIRST-2;
  TB_UNINSTALL=0;
  TB_DELETE=1;
  TB_REFRESH=2;
  TB_ABOUT=3;
  TB_EXIT=4;
  CCapt='VS Fast Uninstall 1.2';
  CKeyPrefix='Key: ';

type
  TStr=class
  public
    FS: string;
    constructor Create(const S: string);
    destructor Destroy; override;
  end;
  TMainForm=class(TForm)
  private
    LIncSearch: TLabel;
    EIncSearch: TEdit;
    LBProgramsList: TListBox;
    MInfo: TMemo;
    TB: TToolBar;
    TBImages: TImageList;
    function  GetRegValue(Key: HKEY; const Name: string): string;
  protected
    procedure FormCreate(Sender: TObject);
    function  FormClose(Sender: TObject): Boolean;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UninstallClick(Sender: TObject);
    procedure DeleteClick;
    procedure RefreshClick;
    procedure AboutClick;
    procedure IncSearch(Sender: TObject);
    procedure ShowInfo(Sender: TObject);
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  end;

constructor TStr.Create(const S: string);
begin
  inherited Create;
  FS:=S;
end;

destructor TStr.Destroy;
begin
  Finalize(FS);
  inherited Destroy;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RefreshClick;
end;

function TMainForm.FormClose(Sender: TObject): Boolean;
var
  i: Integer;
begin
  FAN(TBImages);
  for i:=0 to LBProgramsList.ItemCount-1 do
    if Assigned(LBProgramsList.Objects[i]) then
    begin
      LBProgramsList.Objects[i].Free;
      LBProgramsList.Objects[i]:=nil;
    end;
  Result:=true;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: UninstallClick(Self);
    VK_ESCAPE: Close;
    VK_F1: AboutClick;
    VK_F5: RefreshClick;
    VK_F8: DeleteClick;
  end;
end;

procedure TMainForm.UninstallClick(Sender: TObject);
var
  L: TStringList;
begin
  if (LBProgramsList.ItemIndex<0) or (LBProgramsList.ItemIndex>=LBProgramsList.ItemCount) then Exit;
  L:=TStringList.Create;
  L.Text:=(LBProgramsList.Objects[LBProgramsList.ItemIndex] as TStr).FS;
  WinExec(PChar(L.Values['UninstallString']), SW_SHOWNORMAL);
end;

procedure TMainForm.DeleteClick;
var
  Key: HKey;
  S: string;
begin
  if (LBProgramsList.ItemIndex<0) or (LBProgramsList.ItemIndex>=LBProgramsList.ItemCount) then Exit;
  if MessageDlg('Are you sure you want to delete uninstallation key?', CCapt, MB_ICONQUESTION or MB_OKCANCEL)<>ID_OK then Exit;
  Key:=RegKeyOpenWrite(HKEY_LOCAL_MACHINE, 'software\microsoft\windows\currentversion\uninstall');
  if Key=0 then
  begin
    MessageDlg('Unable to open key HKLM\Software\Microsoft\Windows\CurrentVersion\Uninstall', CCapt, MB_ICONERROR);
    Exit;
  end;
  S:=(LBProgramsList.Objects[LBProgramsList.ItemIndex] as TStr).FS;
  RegKeyDelete(Key, Copy(S, Length(CKeyPrefix)+1, Pos(#13#10, S)-Length(CKeyPrefix)-1));
  RegKeyClose(Key);
end;

procedure TMainForm.RefreshClick;
var
  Key, SubKey: HKey;
  i, j, Index, OI: Integer;
  S: string;
  Keys, Values: TStringList;
begin
  Keys:=TStringList.Create;
  Values:=TStringList.Create;
  Key:=RegKeyOpenRead(HKEY_LOCAL_MACHINE, 'software\microsoft\windows\currentversion\uninstall');
  if Key=0 then
  begin
    MessageDlg('Unable to open key HKLM\Software\Microsoft\Windows\CurrentVersion\Uninstall', CCapt, MB_ICONERROR);
    Exit;
  end;
  RegKeyGetKeyNames(Key, Keys);
  OI:=LBProgramsList.ItemIndex;
  LBProgramsList.BeginUpdate;
  for i:=0 to LBProgramsList.ItemCount-1 do
    if Assigned(LBProgramsList.Objects[i]) then
    begin
      LBProgramsList.Objects[i].Free;
      LBProgramsList.Objects[i]:=nil;
    end;
  LBProgramsList.Clear;
  for i:=0 to Keys.Count-1 do
  begin
    SubKey:=RegKeyOpenRead(Key, Keys[i]);
    if SubKey=0 then Continue;
    if RegKeyValExists(SubKey, 'DisplayName') then
    begin
      Index:=LBProgramsList.ItemAdd(RegKeyGetStr(SubKey, 'DisplayName'));
      S:=CKeyPrefix+Keys[i]+#13#10;
      RegKeyGetValueNames(SubKey, Values);
      for j:=0 to Values.Count-1 do
        S:=S+Values[j]+'='+GetRegValue(SubKey, Values[j])+#13#10;
      LBProgramsList.Objects[Index]:=TStr.Create(S);
    end;
    RegKeyClose(SubKey);
  end;
  LBProgramsList.EndUpdate;
  if OI>=LBProgramsList.ItemCount then OI:=LBProgramsList.ItemCount-1;
  if OI<0 then OI:=0;
  if OI<LBProgramsList.ItemCount then LBProgramsList.ItemIndex:=OI;
  ShowInfo(LBProgramsList);
  RegKeyClose(Key);
  FAN(Keys);
  FAN(Values);
end;

procedure TMainForm.AboutClick;
const
  AboutText=CCapt+#13#10 +
            'Copyright '#169'VgaSoft, 2004-2010'#13#10#13#10+
            'Keys:'#13#10+
            '  F1: About'#13#10+
            '  F5: Refresh'#13#10+
            '  F8: Delete'#13#10+
            '  Enter: Uninstall'#13#10+
            '  Esc: Exit';
  AboutCaption='About';
  AboutIcon='MAINICON';
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
      hwndOwner := Handle;
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
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText  := AboutText;
      lpszCaption := AboutCaption;
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectA(MsgBoxParamsA);
  end;
end;

procedure TMainForm.IncSearch(Sender: TObject);
var
  S: string;
  i, Len: Integer;
begin
  if EIncSearch.Text='' then Exit;
  S:=EIncSearch.Text;
  Len:=Length(S);
  for i:=0 to LBProgramsList.ItemCount-1 do
    if Length(LBProgramsList.Items[i])>=Len then
      if AnsiCompareText(Copy(LBProgramsList.Items[i], 1, Len), S)=0 then
      begin
        LBProgramsList.ItemIndex:=i;
        ShowInfo(Self);
        Exit;
      end;
end;

procedure TMainForm.ShowInfo(Sender: TObject);
begin
  if (LBProgramsList.ItemIndex<0) or (LBProgramsList.ItemIndex>=LBProgramsList.ItemCount) then Exit;
  MInfo.Text:=(LBProgramsList.Objects[LBProgramsList.ItemIndex] as TStr).FS;
end;

procedure TMainForm.WMCommand(var Msg: TWMCommand);
begin
  if Msg.Ctl=TB.Handle then
    case Msg.ItemID of
      TB_UNINSTALL: UninstallClick(Self);
      TB_DELETE: DeleteClick;
      TB_REFRESH: RefreshClick;
      TB_ABOUT: AboutClick;
      TB_EXIT: Close;
    end;
end;

procedure TMainForm.WMSize(var Msg: TWMSize);
var
  TBH: Integer;
begin
  if Assigned(TB) then TBH:=TB.Height;
  if Assigned(EIncSearch)
    then EIncSearch.Width:=Msg.Width-60;
  if Assigned(LBProgramsList)
    then LBProgramsList.SetSize(Msg.Width-10, Msg.Height-TBH-140);
  if Assigned(MInfo)
    then MInfo.SetBounds(5, Msg.Height-105, Msg.Width-10, 100);
end;

function TMainForm.GetRegValue(Key: HKEY; const Name: string): string;
var
  BinBuf: array[0..15] of Byte;
  i, Size: Integer;
begin
  Result:='';
  case RegKeyGetValueTyp(Key, Name) of
    REG_SZ: Result:=RegKeyGetStr(Key, Name);
    REG_EXPAND_SZ: Result:=RegKeyGetStrEx(Key, Name);
    REG_BINARY:
      begin
        Size:=RegKeyGetBin(Key, Name, BinBuf, 16);
        if Size>16 then
        begin
          Size:=16;
          Result:='...';
        end;
        for i:=Size-1 downto 0 do
          Result:=IntToHex(BinBuf[i], 2)+' '+Result;
      end;
    REG_DWORD: Result:=IntToStr(RegKeyGetInt(Key, Name));
  end;
  for i:=Length(Result) downto 1 do
    if Result[i]=#0 then Delete(Result, i, 1);
end;

var
  MainForm: TMainForm;
  TBH: Integer;

begin
  MainForm:=TMainForm.Create(nil, CCapt);
  with MainForm do
  begin
    SetSize(640, 480);
    Position:=poScreenCenter;
    OnCreate:=FormCreate;
    OnClose:=FormClose;
    OnKeyUp:=FormKeyUp;
    TBImages:=TImageList.Create;
    TBImages.AddMasked(LoadImage(hInstance, 'TB', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
    TB:=TToolBar.Create(MainForm, True);
    TB.OnKeyUp:=FormKeyUp;
    TB.SetPosition(0, 0);
    TB.Images:=TBImages;
    TB.ButtonAdd('Uninstall', TB_UNINSTALL);
    TB.ButtonAdd('Delete', TB_DELETE);
    TB.ButtonAdd('Refresh', TB_REFRESH);
    TB.ButtonAdd('About', TB_ABOUT);
    TB.ButtonAdd('Exit', TB_EXIT);
    TBH:=TB.Height;
    LIncSearch:=TLabel.Create(MainForm, 'Search:');
    LIncSearch.SetBounds(5, 10+TBH, 50, 15);
    LIncSearch.OnKeyUp:=FormKeyUp;
    EIncSearch:=TEdit.Create(MainForm, '');
    EIncSearch.SetPosition(55, 5+TBH);
    EIncSearch.OnChange:=IncSearch;
    EIncSearch.OnKeyUp:=FormKeyUp;
    LBProgramsList:=TListBox.Create(MainForm, lbSorted);
    LBProgramsList.SetPosition(5, 30+TBH);
    LBProgramsList.OnChange:=ShowInfo;
    LBProgramsList.OnDblClick:=UninstallClick;
    LBProgramsList.OnKeyUp:=FormKeyUp;
    MInfo:=TMemo.Create(MainForm, '');
    MInfo.ReadOnly:=true;
    MInfo.Font.Name:='Courier New';
    MInfo.OnKeyUp:=FormKeyUp;
    MInfo.Style:=MInfo.Style or WS_VSCROLL;
  end;
  MainForm.Run;
end.
