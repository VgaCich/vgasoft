program JSONView;

uses
  Windows, Messages, CommCtrl, ShellAPI, AvL, avlUtils, avlTreeViewEx, avlJSON;

{$R *.res}

type
  TMainForm = class(TForm)
    Menu: TMenu;
    Tree: TTreeViewEx;
  private
    procedure FormResize(Sender: TObject);
    procedure OpenFile(const FileName: string);
    procedure ExportFile(const FileName: string);
    procedure ShowAbout;
    function AddNode(Parent: Integer; Data: PJsonValue; const Name: string = ''): Integer;
    procedure ExportNode(Node: Integer; Data: TStream; const Prefix: string);
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  public
    constructor Create;
    destructor Destroy; override;
  end;

const
  IDOpen = 1001;
  IDExport = IDOpen + 1;
  IDAbout = IDExport + 1;
  IDExit = IDAbout + 1;
  MenuMain: array[0..4] of PChar = ('1001',
    '&Open',
    '&Export',
    '&About',
    'E&xit');
  AboutIcon = 'MAINICON';
  AboutCaption = 'About ';
  AboutText = 'VgaSoft JSONView 1.0'+CRLF+CRLF+
              'Copyright '#169' VgaSoft, 2019'+CRLF+
              'vgasoft@gmail.com';

{ TMainForm }

constructor TMainForm.Create;
begin
  inherited Create(nil, 'JSON Viewer');
  SetSize(600, 400);
  Position := poScreenCenter;
  Menu := TMenu.Create(Self, true, MenuMain);
  SetMenu(Handle, Menu.Handle);
  Tree := TTreeViewEx.Create(Self);
  Tree.SetPosition(0, 0);
  DragAcceptFiles(Handle, true);
  OnResize := FormResize;
  FormResize(Self);
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    OpenFile(ExpandFileName(ParamStr(1)));
end;

destructor TMainForm.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  Tree.SetSize(ClientWidth, ClientHeight);
end;

procedure TMainForm.OpenFile(const FileName: string);
var
  F: TFileStream;
  S: string;
  J: PJsonValue;
begin
  Tree.DeleteItem(Integer(TVI_ROOT));
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(S, F.Size);
    F.Read(S[1], Length(S));
  finally
    F.Free;
  end;
  J := JsonParse(S);
  try
    Tree.ExpandItem(AddNode(Integer(TVI_ROOT), J), emExpand);
  finally
    JsonFree(J);
  end;
end;

procedure TMainForm.ExportFile(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
  try
    ExportNode(Tree.Selected, F, '');
  finally
    F. Free;
  end;
end;

procedure TMainForm.ShowAbout;
var
  Version: TOSVersionInfo;
  MsgBoxParamsW: TMsgBoxParamsW;
  MsgBoxParamsA: TMsgBoxParamsA;
begin
  Version.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(Version);
  if Version.dwPlatformId = VER_PLATFORM_WIN32_NT then
  begin
    FillChar(MsgBoxParamsW, SizeOf(MsgBoxParamsW), #0);
    with MsgBoxParamsW do
    begin
      cbSize := SizeOf(MsgBoxParamsW);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText  := PWideChar(WideString(AboutText));
      lpszCaption := PWideChar(WideString(AboutCaption+Caption));
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectW(MsgBoxParamsW);
  end
  else begin
    FillChar(MsgBoxParamsA, SizeOf(MsgBoxParamsA), #0);
    with MsgBoxParamsA do
    begin
      cbSize := SizeOf(MsgBoxParamsA);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText  := PAnsiChar(AboutText);
      lpszCaption := PAnsiChar(AboutCaption+Caption);
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectA(MsgBoxParamsA);
  end;
end;

function TMainForm.AddNode(Parent: Integer; Data: PJsonValue; const Name: string): Integer;

  function GetCaption: string;
  const
    BoolToStr: array[Boolean] of string = ('false', 'true');
  begin
    case Data.VType of
      jtNone: Result := '[none]';
      jtObject: Result := '[obj (' + IntToStr(Data.Obj.Length) + ')]';
      jtArray: Result := '[arr (' + IntToStr(Data.Arr.Length) + ')]';
      jtInteger: Result := '[int] ' + Int64ToStr(Data.Int);
      jtDouble: Result := '[flt] ' + FloatToStr(Data.Dbl);
      jtString: Result := '[str] ' + UTF8Decode(Data.Str.Value);
      jtBoolean: Result := '[bool] ' + BoolToStr[Data.Bool <> 0];
      jtNull: Result := '[null]';
    end;
  end;

var
  i: Integer;
begin
  if not Assigned(Data) then Exit;
  Result := Tree.ItemInsert(Parent, Integer(TVI_LAST), Name + GetCaption, nil);
  if Data.VType = jtArray then
    for i := 0 to Data.Arr.Length - 1 do
      AddNode(Result, Data.Arr.Values[i], IntToStr(i) + ': ');
  if Data.VType = jtObject then
    for i := 0 to Data.Obj.Length - 1 do
      AddNode(Result, Data.Obj.Values[i].Value, Data.Obj.Values[i].Name + ': ');
end;

procedure TMainForm.ExportNode(Node: Integer; Data: TStream; const Prefix: string);

  procedure Write(const S: string);
  begin
    if S = '' then Exit;
    Data.Write(S[1], Length(S));
    Data.Write(CRLF[1], Length(CRLF));
  end;

begin
  Write(Prefix + Tree.GetItemText(Node));
  Node := Tree.Perform(TVM_GETNEXTITEM, TVGN_CHILD, Node);
  while Node <> 0 do
  begin
    ExportNode(Node, Data, Prefix + '  ');
    Node := Tree.Perform(TVM_GETNEXTITEM, TVGN_NEXT, Node);
  end;
end;

procedure TMainForm.WMCommand(var Msg: TWMCommand);
var
  FileName: string;
begin
  if (Msg.Ctl = 0) and (Msg.NotifyCode in [0, 1]) then
    case Msg.ItemID of
      IDOpen: if OpenSaveDialog(Handle, true, '', '', 'JSON Files|*.json;*.js|All files|*.*', '', 0, OFN_FILEMUSTEXIST, FileName) then
        OpenFile(FileName);
      IDExport: if OpenSaveDialog(Handle, false, '', 'txt', 'Text Files|*.txt|All files|*.*', '', 0, OFN_OVERWRITEPROMPT, FileName) then
        ExportFile(FileName);
      IDAbout: ShowAbout;
      IDExit: Close;
    end;
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  FileName: array[0..MAX_PATH] of Char;
begin
  if DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0) > 0 then
  begin
    DragQueryFile(Msg.Drop, 0, FileName, MAX_PATH + 1);
    if FileExists(string(FileName)) then
      OpenFile(string(FileName));
  end;
  DragFinish(Msg.Drop);
end;

var
  MainForm: TMainForm;

begin
  InitCommonControls;
  MainForm := TMainForm.Create;
  MainForm.Run;
  MainForm.Free;
end.
