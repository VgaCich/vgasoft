program FLViewer;

{$R *.res}
{$R FLViewerRes.res}

uses Windows, Messages, CommCtrl, AvL, avlUtils, ShellAPI, avlMasks, avlSplitter;

type
  TExtR=record
    Hash: Integer;
    IsLong: Boolean;
    Ext: string;
  end;
  TFolderData=class
  public
    Name: string;
    Files: TStringList;
    FilesSize, FolderSize: Int64;
    constructor Create;
    destructor Destroy; override;
  end;
  TMainForm=class(TForm)
  private
    TVFolders: TTreeView;
    LVFiles: TListView;
    TB: TToolBar;
    SB: TStatusBar;
    PStatus: TPanel;
    Splitter: TSplitter;
    PBStatus: TProgressBar;
    LStatus: TLabel;
    MenuView: TMenu;
    TBImages, Icons, SmallIcons, Folders: TImageList;
    Data, Files: TStringList;
    Garbage: TList;
    FileName, Mask: string;
    Initialized, OpenDelay, Unicode, FindDelay, FindFiles, FindDirs: Boolean;
    RefreshTimer: TTimer;
    CurFolder, ExtsCount, ExtsCapacity: Integer;
    SizeMin, SizeMax: Int64;
    SizeMode: Cardinal;
    Exts: array of TExtR;
    procedure AdjustColumns(Sender: TObject);
    procedure CopyClick;
    function NodeName(Node: Integer): string;
    procedure ViewClick;
  protected
    constructor Create;
    destructor Destroy; override;
    procedure SplitterMove(Sender: TObject);
    procedure OpenClick;
    procedure ExportClick;
    procedure FindClick;
    procedure RefreshTick(Sender: TObject);
    procedure ShowStatusPanel(const Caption: string);
    procedure HideStatusPanel;
    procedure Find;
    procedure Open;
    function  Load: Boolean;
    function  GetExtHash(const Ext: string): Integer;
    function  AddExt(FileName: string): Integer;
    function  FindExt(FileName: string): Integer;
    function  C(const S: string): string;
    function  T(const S: string): string;
    function  GetName(const S: string): string;
    function  GetSize(const S: string): Int64;
    function  GetNextFolder: string;
    procedure GetFolderFiles(Files: TStringList);
    function  InsertTreeItem(Parent: Integer; Text: string; Data: TObject): Integer;
    function  FillNode(Node: Integer; S: string): Int64;
    procedure FillTree;
    procedure FillList;
    procedure ClearTree;
    procedure ShowAbout;
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  end;
  TFindForm=class(TForm)
    LMask, LSizeMin, LSizeMax, LSize: TLabel;
    EMask, ESizeMin, ESizeMax: TEdit;
    CFiles, CDirs: TCheckBox;
    CBSize: TComboBox;
    BOK, BCancel: TButton;
    constructor Create(Parent: TWinControl; Caption: string);
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SizeChange(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
  end;

const
  IDA: Cardinal=$4C465356;
  IDU: Cardinal=$55465356;
  CRLFU=#$0D#$00#$0A#$00;
  CCapt='VgaSoft FileList Viewer 3.0';
  AboutText=CCapt+#13#10 +
            'Copyright '#169'VgaSoft, 2004-2017';

  TB_OPEN=0;
  TB_EXPORT=1;
  TB_COPY=2;
  TB_FIND=3;
  TB_REFRESH=4;
  TB_VIEW=5;
  TB_ABOUT=6;
  TB_EXIT=7;

  ViewReport=1001;
  ViewList=1002;
  ViewSmallIcons=1003;
  ViewIcons=1004;
  MenuViewTemplate: array[0..4] of PChar=('1001',
    'Report',
    'List',
    'Small icons',
    'Icons');

  SIZES=4;
  SIZE_NONE=0;
  SIZE_MIN=1;
  SIZE_MAX=2;
  SIZE_BETWEEN=3;

  SizeNames: array[0..SIZES-1] of string=(
    'Ignore', 'Minimum', 'Maximum', 'Between');

  SB_PARTS=3;

  SBParts: array[0..SB_PARTS-1] of Integer=(125, 250, -1);

function ImageList_GetIcon(ImageList: HIMAGELIST; Index: Integer; Flags: DWORD): HICON; stdcall; external 'comctl32.dll';

function nrv2e_decompress(src: Pointer; src_len: Cardinal; dst: Pointer; var dst_len: Cardinal): Integer;
var
  BB, ILen: Cardinal;

  function GetByte: Byte;
  begin
    Result := PByteArray(Src)[ILen];
    Inc(ILen);
  end;

  function GetBit: Cardinal;
  begin
    if (BB and $7f) = 0 then
      BB := 2 * GetByte + 1
    else
      BB := 2 * BB;
    Result := (BB shr 8) and 1;
  end;

var
  OLen, OEnd, LastMOff, MOff, MLen: Cardinal;

  function Check(Condition: Boolean; ErrorCode: Integer): Boolean;
  begin
    Result := Condition;
    if Result then
    begin
      nrv2e_decompress := ErrorCode;
      dst_len := OLen;
    end;
  end;

begin
  BB := 0;
  ILen := 0;
  OLen := 0;
  OEnd := dst_len;
  LastMOff := 1;
  while true do
  begin
    while GetBit > 0 do
    begin
      if Check(ILen >= src_len, -201) then Exit;
      if Check(OLen >= OEnd, -202) then Exit;
      PByteArray(dst)[OLen] := GetByte;
      Inc(OLen);
    end;
    MOff := 1;
    while true do
    begin
      MOff := 2 * MOff + GetBit;
      if Check(ILen >= src_len, -201) then Exit;
      if Check(MOff > $1000002, -203) then Exit;
      if GetBit > 0 then break;
      MOff := 2 * (MOff - 1) + GetBit;
    end;
    if MOff = 2 then
    begin
      MOff := LastMOff;
      MLen := GetBit;
    end
    else begin
      if Check(ILen >= src_len, -201) then Exit;
      MOff := 256 * (MOff - 3) + GetByte;
      if MOff = $FFFFFFFF then break;
      MLen := (not MOff) and 1;
      MOff := (MOff shr 1) + 1;
      LastMOff := MOff;
    end;
    if MLen > 0 then
      MLen := GetBit + 1
    else if GetBit > 0 then
      MLen := GetBit + 3
    else begin
      Inc(MLen);
      repeat
        MLen := 2 * MLen + GetBit;
        if Check(ILen >= src_len, -201) then Exit;
        if Check(MLen >= OEnd, -202) then Exit;
      until GetBit > 0;
      Inc(MLen, 3);
    end;
    if MOff > $500 then
      Inc(MLen);
    if Check(OLen + MLen > OEnd, -202) then Exit;
    if Check(MOff > OLen, -203) then Exit;
    PByteArray(dst)[OLen] := PByteArray(dst)[OLen - MOff];
    Inc(OLen);
    repeat
      PByteArray(dst)[OLen] := PByteArray(dst)[OLen - MOff];
      Inc(OLen);
      Dec(MLen);
    until MLen = 0;
  end;
  dst_len := OLen;
  if ILen = src_len then
    Result := 0
  else if ILen < src_len then
    Result := -205
  else
    Result := -201;
end;

function WS2S(const S: WideString): string;
begin
  SetLength(Result, Length(S) * 2);
  Move(S[1], Result[1], Length(Result));
end;

function S2WS(const S: string): WideString;
begin
  SetLength(Result, Length(S) div 2);
  Move(S[1], Result[1], Length(Result) * 2);
end;

var
  MainForm: TMainForm;
  FindForm: TFindForm;

{ TFolderData }

constructor TFolderData.Create;
begin
  inherited Create;
  Files:=TStringList.Create;
end;

destructor TFolderData.Destroy;
begin
  FAN(Files);
  inherited Destroy;
end;

{ TMainForm }

constructor TMainForm.Create;
var
  i: Integer;
begin
  inherited Create(nil, CCapt);
  ExStyle:=ExStyle or WS_EX_ACCEPTFILES;
  Initialized:=false;
  OpenDelay:=false;
  FindDelay:=false;
  CanvasInit;
  SetSize(640, 480);
  Position:=poScreenCenter;
  DragAcceptFiles(Handle, True);
  Icons:=TImageList.Create;
  Icons.Height:=32;
  Icons.Width:=32;
  SmallIcons:=TImageList.Create;
  SmallIcons.Height:=16;
  SmallIcons.Width:=16;
  ExtsCapacity:=512;
  ExtsCount:=0;
  SetLength(Exts, ExtsCapacity);
  Data:=TStringList.Create;
  Garbage:=TList.Create;
  TBImages:=TImageList.Create;
  TBImages.AddMasked(LoadImage(hInstance, 'TB', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
  TB:=TToolBar.Create(Self, false);
  TB.SetPosition(0, 0);
  TB.Images:=TBImages;
  TB.ButtonAdd('Open', TB_OPEN);
  TB.ButtonAdd('Export', TB_EXPORT);
  TB.ButtonAdd('Copy', TB_COPY);
  TB.ButtonAdd('Find', TB_FIND);
  TB.ButtonAdd('Refresh', TB_REFRESH);
  TB.ButtonAdd('View', TB_VIEW);
  TB.ButtonAdd('About', TB_ABOUT);
  TB.ButtonAdd('Exit', TB_EXIT);
  MenuView:=TMenu.Create(Self, false, MenuViewTemplate);
  SB:=TStatusBar.Create(Self, '');
  SB.SetParts(SB_PARTS, SBParts);
  Folders:=TImageList.Create;
  Folders.AddMasked(LoadImage(hInstance, 'FOLDER', IMAGE_BITMAP, 0, 0, 0), clWhite);
  Splitter := TSplitter.Create(Self, true);
  Splitter.SetBounds(200, TB.Height, Splitter.Width, ClientHeight-TB.Height-SB.Height);
  Splitter.OnMove:=SplitterMove;
  TVFolders:=TTreeView.Create(Self);
  TVFolders.Images:=Folders;
  TVFolders.StateImages:=Folders;
  TVFolders.SetBounds(0, TB.Height, Splitter.Left, Splitter.Height);
  LVFiles:=TListView.Create(Self);
  LVFiles.Style:=LVFiles.Style or LVS_SORTASCENDING or LVS_SHOWSELALWAYS or LVS_NOSORTHEADER;
  LVFiles.ViewStyle := LVS_REPORT;
  LVFiles.OptionsEx:=LVFiles.OptionsEx or LVS_EX_FULLROWSELECT or LVS_EX_GRIDLINES or LVS_EX_INFOTIP;
  LVFiles.ColumnAdd('Name', 100);
  LVFiles.ColumnAdd('Size', 100);
  LVFiles.OnResize:=AdjustColumns;
  LVFiles.SetBounds(Splitter.Right, TB.Height,
    ClientWidth-Splitter.Right, Splitter.Height);
  Splitter.BringToFront;
  PStatus:=TPanel.Create(Self, '');
  PStatus.Bevel:=bvRaised;
  PStatus.SetSize(200, 45);
  PStatus.Visible:=false;
  LStatus:=TLabel.Create(PStatus, '');
  LStatus.Transparent:=true;
  LStatus.SetBounds(5, 5, 190, 15);
  PBStatus:=TProgressBar.Create(PStatus);
  PBStatus.SetBounds(5, 25, 190, 15);
  PBStatus.Position:=0;
  RefreshTimer:=TTimer.CreateEx(100, false);
  RefreshTimer.OnTimer:=RefreshTick;
  Initialized:=true;
  for i:=1 to ParamCount do
    if FileExists(ParamStr(i)) then
    begin
      FileName:=ParamStr(i);
      OpenDelay:=true;
      RefreshTimer.Enabled:=true;
      Break;
    end;
end;

destructor TMainForm.Destroy;
begin
  ClearTree;
  FAN(Folders);
  FAN(Icons);
  FAN(SmallIcons);
  Finalize(Exts);
  FAN(Data);
  FAN(Garbage);
  inherited;
end;

procedure TMainForm.WMCommand(var Msg: TWMCommand);
begin
  if Assigned(TB) and (Msg.Ctl=TB.Handle) then
    case Msg.ItemID of
      TB_OPEN: OpenClick;
      TB_EXPORT: ExportClick;
      TB_COPY: CopyClick;
      TB_FIND: FindClick;
      TB_REFRESH: FillList;
      TB_VIEW: ViewClick;
      TB_ABOUT: ShowAbout;
      TB_EXIT: Close;
    end;
  if (Msg.Ctl=0) and (Msg.NotifyCode in [0, 1]) then
    case Msg.ItemID of
      ViewReport: LVFiles.ViewStyle:=LVS_REPORT; 
      ViewList: LVFiles.ViewStyle:=LVS_LIST;
      ViewSmallIcons: LVFiles.ViewStyle:=LVS_SMALLICON;
      ViewIcons: LVFiles.ViewStyle:=LVS_ICON;
    end;
end;

procedure TMainForm.WMSize(var Msg: TWMSize);
begin
  if not Initialized then Exit;
  TB.Width:=ClientWidth;
  Splitter.Height:=ClientHeight-TB.Height-SB.Height;
  if ClientWidth > 0 then
    Splitter.MaxPos:=ClientWidth-Splitter.Width;
  TVFolders.Height:=Splitter.Height;
  LVFiles.SetSize(ClientWidth-Splitter.Right, Splitter.Height);
  RefreshTimer.Enabled:=false;
  RefreshTimer.Enabled:=true;
end;

procedure TMainForm.WMNotify(var Msg: TWMNotify);
type
  PNMTreeView=^TNMTreeView;
  TNMTreeView=packed record
    hdr: TNMHDR;
    action: UINT;
    itemOld: TTVItem;
    itemNew: TTVItem;
    ptDrag: TPoint;
  end;
var
  i: Integer;
  S: string;
  FD: TFolderData;
begin
  if not Assigned(TVFolders) or not Assigned(LVFIles) then Exit;
  if PNMHdr(Msg.NMHdr).hwndFrom=TVFolders.Handle then
    if PNMHdr(Msg.NMHdr).code=TVN_SELCHANGED then
    begin
      FD:=TFolderData(PNMTreeView(Msg.NMHdr).itemNew.lParam);
      Files:=FD.Files;
      SB.SetPartText(0, 0, 'Files: '+IntToStr(FD.Files.Count)+' ('+SizeToStr(FD.FilesSize)+')');
      SB.SetPartText(1, 0, 'Folder size: '+SizeToStr(FD.FolderSize));
      FillList;
    end;
  if PNMHdr(Msg.NMHdr).hwndFrom=LVFiles.Handle then
    if PNMHdr(Msg.NMHdr).code=LVN_ITEMCHANGED then
    begin
      S:=LVFiles.SelectedCaption;
      for i:=0 to Files.Count-1 do
      begin
        if S=GetName(T(Files[i])) then
        begin
          SB.SetPartText(2, 0, S+': '+SizeToStr(GetSize(T(Files[i]))));
          Break;
        end;
      end;
    end;
end;

procedure TMainForm.WMDropFiles(var Message: TWMDropFiles);
var
  aFile: array[0..255] of Char;
begin
  inherited;
  begin
    DragQueryFile(Message.drop, 0, aFile, 256);
    FileName:=string(aFile);
    Open;
  end;
  DragFinish(Message.Drop);
end;

procedure TMainForm.OpenClick;
begin
  if OpenSaveDialog(Handle, true, '', 'vfl', 'VgaSoft FileList files|*.vfl|All files|*.*',
    ExtractFilePath(FileName), 0, OFN_FILEMUSTEXIST, FileName) then Open;
end;

procedure TMainForm.ExportClick;

  function Indent(Level: Integer): string;
  begin
    Result:='';
    while Level>1 do
    begin
      Result:=Result+'  ';
      Dec(Level);
    end;
  end;

  procedure UpdateProgress(Progress: Single);
  begin
    if PBStatus.Position<>Round(100*Progress) then
      PBStatus.Position:=Round(100*Progress);
    ProcessMessages;
  end;

var
  F: TFileStream;

  function GetFileName(const S: string): string;
  var
    N: WideString;
  begin
    if Unicode then
    begin
      N:=S2WS(S);
      while N[Length(N)] <> '|' do Delete(N, Length(N), 1);
      Delete(N, Length(N), 1);
      Result:=WS2S(N);
    end
      else Result:=GetName(S);
  end;

  procedure WriteFolder(Name: string; Level: Integer);
  begin
    Name:=C(Indent(Level)+'[')+Name+C(']'#13#10);
    F.Write(Name[1], Length(Name));
  end;

  procedure WriteFile(Name: string; Level: Integer);
  begin
    Name:=C(Indent(Level)+'+-')+GetFileName(Name)+C(': '+SizeToStr(GetSize(T(Name)))+#13#10);
    F.Write(Name[1], Length(Name));
  end;

  procedure ExportData;
  var
    i, Level: Integer;
    S: string;
  begin
    Level:=0;
    for i:=0 to Data.Count-1 do
    begin
      S:=T(Data[i]);
      if S[1]='<' then
      begin
        if S[2]<>'|' then
        begin
          if Unicode then
            WriteFolder(Copy(Data[i], 3, Length(Data[i])-4), Level)
          else
            WriteFolder(Copy(Data[i], 2, Length(Data[i])-2), Level);
          Inc(Level);
        end
          else Dec(Level);
      end
        else WriteFile(Data[i], Level);
      UpdateProgress(i/Data.Count);
    end;
  end;

var
  NodesCount, NodesProcessed: Integer;

  procedure CountNodes(Node: Integer);
  begin
    Inc(NodesCount);
    Node:=TVFolders.Perform(TVM_GETNEXTITEM, TVGN_CHILD, Node);
    while Node<>0 do
    begin
      CountNodes(Node);
      Node:=TVFolders.Perform(TVM_GETNEXTITEM, TVGN_NEXT, Node);
    end;
  end;

  procedure ExportNode(Node, Level: Integer);
  var
    i: Integer;
    Item: TTVItem;
    FD: TFolderData;
  begin
    Item.mask:=TVIF_PARAM;
    Item.hItem:=HTreeItem(Node);
    TVFolders.Perform(TVM_GETITEM, 0, Integer(@Item));
    FD:=TFolderData(Item.lParam);
    WriteFolder(FD.Name, Level);
    for i:=0 to FD.Files.Count-1 do
      WriteFile(FD.Files[i], Level+1);
    Inc(NodesProcessed);
    UpdateProgress(NodesProcessed/NodesCount);
    Node:=TVFolders.Perform(TVM_GETNEXTITEM, TVGN_CHILD, Node);
    while Node<>0 do
    begin
      ExportNode(Node, Level+1);
      Node:=TVFolders.Perform(TVM_GETNEXTITEM, TVGN_NEXT, Node);
    end;
  end;

var
  FN: string;
  Node: Integer;
const
  BOM: Word = $FEFF;
begin
  if Data.Count<=0 then
  begin
    MessageDlg('Nothing to export', Caption, MB_ICONERROR);
    Exit;
  end;
  FN:=ChangeFileExt(FileName, '.txt');
  if not OpenSaveDialog(Handle, false, '', 'txt', 'Text files|*.txt|All files|*.*',
    ExtractFilePath(FN), 0, OFN_OVERWRITEPROMPT or OFN_PATHMUSTEXIST, FN) then Exit;
  ShowStatusPanel('Exporting...');
  PBStatus.Position:=0;
  F:=TFileStream.Create(FN, fmCreate);
  try
    if Unicode then F.Write(BOM, SizeOf(BOM));
    Node:=TVFolders.Perform(TVM_GETNEXTITEM, TVGN_CARET, 0);
    if Node<>0 then
    begin
      NodesCount:=0;
      NodesProcessed:=0;
      CountNodes(Node);
      ExportNode(Node, 0);
    end
      else ExportData;
  finally
    FAN(F);
    HideStatusPanel;
  end;
end;

procedure TMainForm.FindClick;
begin
  if not Assigned(FindForm)
    then FindForm:=TFindForm.Create(Self, 'Find');
  FindForm.EMask.SetFocus;
  FindForm.ShowModal;
end;

procedure TMainForm.RefreshTick(Sender: TObject);
begin
  if OpenDelay then
  begin
    OpenDelay:=false;
    Open;
    Exit;
  end;
  if FindDelay then
  begin
    FindDelay:=false;
    Find;
    Exit;
  end;
  RefreshTimer.Enabled:=false;
  FillList;
end;

procedure TMainForm.ShowStatusPanel(const Caption: string);
begin
  PStatus.Visible:=true;
  PStatus.BringToFront;
  PStatus.SetPosition((ClientWidth - PStatus.Width) div 2, (ClientHeight - PStatus.Height) div 2);
  LStatus.Caption:=Caption;
  PBStatus.Position:=0;
  LVFiles.Enabled:=false;
  Splitter.Enabled:=false;
  TB.Enabled:=false;
  TVFolders.Enabled:=false;
  ProcessMessages;
end;

procedure TMainForm.HideStatusPanel;
begin
  LVFiles.Enabled:=true;
  Splitter.Enabled:=true;
  TB.Enabled:=true;
  TVFolders.Enabled:=true;
  PStatus.Visible:=false;
end;

procedure TMainForm.Find;
var
  S: string;
  i, Pr, OldPr: Integer;
  Sz: Int64;
  List: TStringList;
  CMask: TMask;

  function FillFindNode(Node: Integer; const S: string): Int64;
  var
    FD: TFolderData;
    j: Integer;
  begin
    FD:=TFolderData.Create;
    Garbage.Add(FD);
    FD.Name:=C(S);
    Inc(i);
    Node:=InsertTreeItem(Node, S, FD);
    while true do
    begin
      if i=List.Count then Exit;
      case List[i][1] of
        '>', '|': FD.FolderSize:=FD.FolderSize+FillFindNode(Node, Copy(List[i], 2, MaxInt));
        '<': Break;
        '+': FD.Files.Add(C(Copy(List[i], 2, MaxInt)));
      end;
      Inc(i);
    end;
    for j:=0 to FD.Files.Count-1 do
      FD.FilesSize:=FD.FilesSize+GetSize(T(FD.Files[i]));
    FD.FolderSize:=FD.FolderSize+FD.FilesSize;
    TVFolders.Perform(TV_FIRST+19, 0, Node);//Sort childrens
    Result:=FD.FolderSize;
  end;

begin
  OldPr:=-1;
  CMask:=TMask.Create(Mask);
  try
    ShowStatusPanel('Find...');
    List:=TStringList.Create;
    for i:=0 to Data.Count-1 do
    begin
      Pr:=Round(100*(i/Data.Count));
      if Pr>OldPr then PBStatus.Position:=Pr;
      OldPr:=Pr;
      ProcessMessages;
      S:=T(Data[i]);
      if S[1]='<' then
      begin
        if S[2]='|'
          then if (List.Count>0) and (List[List.Count-1][1]='>')
            then List.Delete(List.Count-1)
            else List.Add('<')
          else begin
            S:=Copy(S, 2, Length(S)-2);
            if FindDirs and CMask.Matches(S)
              then List.Add('|'+S)
              else List.Add('>'+S);
          end;
        Continue;
      end;
      if FindFiles and CMask.Matches(GetName(S)) then
      begin
        Sz:=GetSize(S);
        case SizeMode of
          SIZE_NONE:;
          SIZE_MIN: if Sz<SizeMin then Continue;
          SIZE_MAX: if Sz>SizeMax then Continue;
          SIZE_BETWEEN: if (Sz<SizeMin) or (Sz>SizeMax) then Continue;
        end;
        List.Add('+'+S);
      end;
    end;
    i:=-1;
    FillFindNode(Integer(TVI_ROOT), 'Find: '+Mask);
  finally
    HideStatusPanel;
    FAN(CMask);
    FAN(List);
  end;
end;

procedure TMainForm.Open;
begin
  try
    ShowStatusPanel('Loading...');
    if not Load then Exit;
    Caption:=CCapt+' ['+ExtractFileName(FileName)+']';
    LStatus.Caption:='Building tree...';
    ProcessMessages;
    FillTree;
    ProcessMessages;
    FillList;
  finally
    HideStatusPanel;
  end;
end;

function TMainForm.Load: Boolean;
var
  IBuffer: Pointer;
  IBufSize, TextSize, OBFTemp, Res, Start: Cardinal;
  Text: string;
  IFile: TFileStream;
begin
  IBuffer:=nil;
  Result:=false;
  try
    IFile:=TFileStream.Create(FileName, fmOpenRead);
    IFile.Read(IBufSize, SizeOf(IBufSize));
    if (IBufSize<>IDA) and (IBufSize<>IDU) then
    begin
      MessageBox(Handle, 'Error: invalid file', 'Error', MB_ICONERROR);
      Exit;
    end;
    Unicode:=IBufSize=IDU; 
    IFile.Read(TextSize, SizeOf(TextSize));
    IBufSize:=IFile.Size-SizeOf(IBufSize)*2;
    if TextSize<>0 then
    begin
      GetMem(IBuffer, IBufSize);
      SetLength(Text, TextSize);
      IFile.Read(IBuffer^, IBufSize);
      Res:=nrv2e_decompress(IBuffer, IBufSize, Pointer(Text), TextSize);
      if Res<>0 then
      begin
        MessageBox(Handle, PChar(Format('NRV2E decompressing error: %d', [Res])), 'Error', MB_ICONERROR);
        Exit;
      end;
      if TextSize<>Length(Text) then
        if MessageBox(Handle, 'Data corrupted. Continue?', 'Error', MB_ICONERROR or MB_YESNO)=ID_YES then
          SetLength(Text, TextSize)
        else Exit;
    end
    else begin
      SetLength(Text, IBufSize);
      IFile.Read(Text[1], Length(Text));
    end;
    if Unicode then
    begin
      Data.Clear;
      Start:=1;
      Res:=Pos(CRLFU, Text);
      while Res>0 do
      begin
        Data.Add(Copy(Text, Start, Res-Start));
        Start:=Res+4;
        Res:=PosEx(CRLFU, Text, Start);
      end;
      if Start<Length(Text) then
        Data.Add(Copy(Text, Start, MaxInt));
    end
      else Data.Text:=Text;
    RemoveVoidStrings(Data);
    PBStatus.Position:=10;
    Result:=true;
  finally
    FAN(IFile);
    FreeMemAndNil(IBuffer, IBufSize);
  end;
end;

function TMainForm.GetExtHash(const Ext: string): Integer;
var
  i, Count: Integer;
begin
  if Length(Ext)>4
    then Count:=4
    else Count:=Length(Ext);
  Result:=0;
  for i:=0 to Count-1 do
    Result:=Result or (Integer(Ext[i+1]) shl (i*8));
end;

function TMainForm.AddExt(FileName: string): Integer;
var
  Ext: TExtR;
begin
  Ext.Ext:=LowerCase(ExtractFileExt(FileName));
  Delete(Ext.Ext, 1, 1);
  Ext.IsLong:=Length(Ext.Ext)>4;
  Ext.Hash:=GetExtHash(Ext.Ext);
  if ExtsCount=ExtsCapacity then
  begin
    ExtsCapacity:=2*ExtsCapacity;
    SetLength(Exts, ExtsCapacity);
  end;
  Exts[ExtsCount]:=Ext;
  Result:=ExtsCount;
  Inc(ExtsCount);
end;

procedure TMainForm.AdjustColumns(Sender: TObject);
var
  SizeWidth: Integer;
begin
  if LVFiles.ViewStyle<>LVS_REPORT then Exit;
  SizeWidth:=LVFiles.Perform(LVM_GETCOLUMNWIDTH, 1, 0);
  LVFiles.Perform(LVM_SETCOLUMNWIDTH, 0, LVFiles.ClientWidth-SizeWidth);
  LVFiles.Perform(LVM_SETCOLUMNWIDTH, 1, SizeWidth);
end;

function TMainForm.FindExt(FileName: string): Integer;
var
  i, Hash: Integer;
  IsLong: Boolean;
begin
  Result:=-1;
  FileName:=LowerCase(ExtractFileExt(FileName));
  Delete(FileName, 1, 1);
  Hash:=GetExtHash(FileName);
  IsLong:=Length(FileName)>4;
  for i:=0 to ExtsCount-1 do
  begin
    if (IsLong and not Exts[i].IsLong) or (not IsLong and Exts[i].IsLong) or
       (Hash<>Exts[i].Hash) or (IsLong and (FileName<>Exts[i].Ext)) then Continue;
    Result:=i;
    Break;
  end;
end;

function TMainForm.C(const S: string): string;
begin
  if Unicode then
    Result:=WS2S(WideString(S))
  else
    Result:=S;
end;

function TMainForm.T(const S: string): string;
begin
  if Unicode then
    Result:=AnsiString(S2WS(S))
  else
    Result:=S;
end;

function TMainForm.GetName(const S: string): string;
begin
  Result:=Copy(S, 1, LastDelimiter('|', S)-1);
end;

function TMainForm.GetSize(const S: string): Int64;
begin
  Result:=StrToInt64Def(Copy(S, LastDelimiter('|', S)+1, MaxInt), 0);
end;

function TMainForm.GetNextFolder: string;
var
  S: string;
begin
  Result:='';
  if CurFolder=Data.Count-1 then raise Exception.Create('Malformed file');
  Inc(CurFolder);
  S:=Data[CurFolder];
  while T(S)[1]<>'<' do
  begin
    if CurFolder<>Data.Count-1
      then Inc(CurFolder)
      else Exit;
    S:=Data[CurFolder];
  end;
  if Unicode then
  begin
    Result:=Copy(S, 3, Length(S)-4);
    if S2WS(Result)='|' then Result:='|';
  end
  else
    Result:=Copy(S, 2, Length(S)-2);
end;

procedure TMainForm.GetFolderFiles(Files: TStringList);

  function NextFolder: Integer;
  begin
    Result:=CurFolder;
    if CurFolder=Data.Count-1 then Exit;
    Inc(Result);
    while (Result<Data.Count) and (T(Data[Result])[1]<>'<') do Inc(Result);
  end;

var
  i: Integer;
begin
  Files.Clear;
  for i:=CurFolder+1 to NextFolder-1 do
    Files.Add(Data[i]);
end;

function TMainForm.InsertTreeItem(Parent: Integer; Text: String; Data: TObject): Integer;
type
  TTVInsertStruct=packed Record
    hParent: THandle;
    hAfter: THandle;
    item: TTVItem;
  end;
var
  TVIns: TTVInsertStruct;
begin
  TVIns.hParent:=Parent;
  TVIns.hAfter:=0;
  TVIns.item.mask:=TVIF_TEXT or TVIF_SELECTEDIMAGE or TVIF_PARAM;
  TVIns.item.iSelectedImage:=1;
  TVIns.item.pszText:=PChar(Text);
  TVIns.item.lParam:=Integer(Data);
  Result:=TVFolders.Perform(TVM_INSERTITEM, 0, Integer(@TVIns));
end;

function TMainForm.FillNode(Node: Integer; S: string): Int64;
var
  FD: TFolderData;
  i: Integer;
begin
  FD:=TFolderData.Create;
  Garbage.Add(FD);
  FD.Name:=S;
  GetFolderFiles(FD.Files);
  for i:=0 to FD.Files.Count-1 do
    FD.FilesSize:=FD.FilesSize+GetSize(T(FD.Files[i]));
  FD.FolderSize:=FD.FilesSize;
  if Node=Integer(TVI_ROOT) then Files:=FD.Files;
  Node:=InsertTreeItem(Node, T(S), FD);
  while true do
  begin
    S:=GetNextFolder;
    if S='|' then Break;
    FD.FolderSize:=FD.FolderSize+FillNode(Node, S);
  end;
  TVFolders.Perform(TV_FIRST+19, 0, Node);//Sort childrens
  PBStatus.Position:=10+Round(90*(CurFolder/Data.Count));
  Result:=FD.FolderSize;
  ProcessMessages;
end;

procedure TMainForm.FillTree;
begin
  TVFolders.BeginUpdate;
  ClearTree;
  CurFolder:=-1;
  try
    FillNode(Integer(TVI_ROOT), GetNextFolder);
  except
    MessageBox(Handle, PChar(Exception(ExceptObject).Message), 'Error', MB_ICONERROR);
  end;
  TVFolders.EndUpdate;
  ProcessMessages;
end;

procedure TMainForm.FillList;
var
  i, Index, IcoIndex: integer;
  FN: string;
  PSFI: SHFILEINFO;
begin
  if not Assigned(Files) then Exit;
  LVFiles.BeginUpdate;
  LVFiles.Clear;
  for i:= 0 to Files.Count-1 do
  begin
    FN:=GetName(T(Files[i]));
    Index:=LVFiles.ItemAdd(FN);
    LVFiles.Items[Index, 1]:=SizeToStr(GetSize(T(Files[i])));
    IcoIndex:=FindExt(FN);
    if IcoIndex>-1
      then LVFiles.ItemImageIndex[Index]:=IcoIndex
    else begin
      SHGetFileInfo(PChar(string(ExtractFileExt(FN))), FILE_ATTRIBUTE_NORMAL, PSFI, SizeOf(PSFI),
                    SHGFI_USEFILEATTRIBUTES or SHGFI_ICON or SHGFI_LARGEICON);
      Icons.AddIcon(PSFI.hIcon);
      SHGetFileInfo(PChar(string(ExtractFileExt(FN))), FILE_ATTRIBUTE_NORMAL, PSFI, SizeOf(PSFI),
                    SHGFI_USEFILEATTRIBUTES or SHGFI_ICON or SHGFI_SMALLICON);
      SmallIcons.AddIcon(PSFI.hIcon);
      LVFiles.ItemImageIndex[Index]:=AddExt(FN);
    end;
  end;
  if (LVFiles.LargeImages=nil) and (ExtsCount>0) then
  begin
    LVFiles.LargeImages:=Icons;
    LVFiles.SmallImages:=SmallIcons;
  end;
  LVFiles.EndUpdate;
  //AdjustColumns(Self);
end;

procedure TMainForm.ClearTree;
var
  i: Integer;
begin
  TVFolders.Perform(TV_FIRST+1, 0, Integer(TVI_ROOT));
  for i:=0 to Garbage.Count-1 do
    if Assigned(Garbage[i]) then TObject(Garbage[i]).Free;
  Garbage.Clear;
end;

procedure TMainForm.CopyClick;
var
  Path: string;
  Node: Integer;
begin
  Path:='';
  if GetFocus<>TVFolders.Handle then
  begin
    if LVFiles.SelectedIndex<0 then Exit;
    Path:=LVFiles.SelectedCaption;
  end
    else if TVFolders.Perform(TVM_GETNEXTITEM, TVGN_CARET, 0)=0 then Exit;
  Node:=TVFolders.Perform(TVM_GETNEXTITEM, TVGN_CARET, 0);
  while Node<>0 do
  begin
    Path:=NodeName(Node)+'\'+Path;
    Node:=TVFolders.Perform(TVM_GETNEXTITEM, TVGN_PARENT, Node);
  end;
  if Path[Length(Path)]='\' then Delete(Path, Length(Path), 1);
  SetClipboardText(Path);
end;

function TMainForm.NodeName(Node: Integer): string;
var
  Item: TTVItem;
begin
  SetLength(Result, MAX_PATH);
  Item.mask:=TVIF_TEXT;
  Item.pszText:=@Result[1];
  Item.cchTextMax:=MAX_PATH;
  Item.hItem:=HTreeItem(Node);
  TVFolders.Perform(TVM_GETITEM, 0, Integer(@Item));
  SetLength(Result, FirstDelimiter(#0, Result)-1);
end;

procedure TMainForm.ShowAbout;
const
  AboutCaption='About';
  AboutIcon='MAINICON';
var
  Version: TOSVersionInfo;
  MsgBoxParamsW: TMsgBoxParamsW;
  MsgBoxParamsA: TMsgBoxParamsA;
begin
  Version.dwOSVersionInfoSize:=SizeOf(TOSVersionInfo);
  GetVersionEx(Version);
  if Version.dwPlatformId=VER_PLATFORM_WIN32_NT then
  begin
    FillChar(MsgBoxParamsW, SizeOf(MsgBoxParamsW), #0);
    with MsgBoxParamsW do begin
      cbSize:=SizeOf(MsgBoxParamsW);
      hwndOwner:=Handle;
      hInstance:=SysInit.hInstance;
      lpszText:=AboutText;
      lpszCaption:=AboutCaption;
      lpszIcon:=AboutIcon;
      dwStyle:=MB_USERICON;
    end;
    MessageBoxIndirectW(MsgBoxParamsW);
  end
  else begin
    FillChar(MsgBoxParamsA, SizeOf(MsgBoxParamsA), #0);
    with MsgBoxParamsA do begin
      cbSize:=SizeOf(MsgBoxParamsA);
      hwndOwner:=Handle;
      hInstance:=SysInit.hInstance;
      lpszText:=AboutText;
      lpszCaption:=AboutCaption;
      lpszIcon:=AboutIcon;
      dwStyle:=MB_USERICON;
    end;
    MessageBoxIndirectA(MsgBoxParamsA);
  end;
end;

procedure TMainForm.SplitterMove(Sender: TObject);
begin
  TVFolders.Width:=Splitter.Left;
  LVFiles.SetBounds(Splitter.Right, TB.Height, ClientWidth-Splitter.Right, LVFiles.Height);
end;

procedure TMainForm.ViewClick;
const
  MenuCheck: array[Boolean] of UINT = (MF_UNCHECKED, MF_CHECKED);
var
  Cursor: TPoint;
begin
  GetCursorPos(Cursor);
  CheckMenuItem(MenuView.Handle, ViewReport, MenuCheck[LVFiles.ViewStyle=LVS_REPORT] or MF_BYCOMMAND);
  CheckMenuItem(MenuView.Handle, ViewList, MenuCheck[LVFiles.ViewStyle=LVS_LIST] or MF_BYCOMMAND);
  CheckMenuItem(MenuView.Handle, ViewSmallIcons, MenuCheck[LVFiles.ViewStyle=LVS_SMALLICON] or MF_BYCOMMAND);
  CheckMenuItem(MenuView.Handle, ViewIcons, MenuCheck[LVFiles.ViewStyle=LVS_ICON] or MF_BYCOMMAND);
  MenuView.Popup(Cursor.X, Cursor.Y);
end;

{ TFindForm }

constructor TFindForm.Create(Parent: TWinControl; Caption: string);
var
  i: Integer;
begin
  inherited Create(Parent, Caption);
  BorderStyle:=bsDialog;
  BorderIcons:=[biMinimize];
  SetSize(300+Width-Clientwidth, 115+Height-ClientHeight);
  Position:=poScreenCenter;
  OnKeyUp:=KeyUp;
  LMask:=TLabel.Create(Self, 'Mask:');
  LMask.SetBounds(5, 10, 50, 15);
  LSizeMin:=TLabel.Create(Self, 'Min size:');
  LSizeMin.SetBounds(5, 60, 50, 15);
  LSizeMin.Visible:=false;
  LSizeMax:=TLabel.Create(Self, 'Max size:');
  LSizeMax.SetBounds(165, 60, 50, 15);
  LSizeMax.Visible:=false;
  LSize:=TLabel.Create(Self, 'Size:');
  LSize.SetBounds(5, 90, 50, 15);
  EMask:=TEdit.Create(Self, '');
  EMask.SetBounds(55, 5, 240, 24);
  EMask.OnKeyUp:=KeyUp;
  ESizeMin:=TEdit.Create(Self, '');
  ESizeMin.SetBounds(55, 55, 80, 24);
  ESizeMin.OnKeyUp:=KeyUp;
  ESizeMin.Visible:=false;
  ESizeMax:=TEdit.Create(Self, '');
  ESizeMax.SetBounds(215, 55, 80, 24);
  ESizeMax.OnKeyUp:=KeyUp;
  ESizeMax.Visible:=false;
  CFiles:=TCheckBox.Create(Self, 'Files');
  CFiles.Checked:=true;
  CFiles.SetBounds(5, 35, 140, 15);
  CDirs:=TCheckBox.Create(Self, 'Directories');
  CDirs.Checked:=true;
  CDirs.SetBounds(150, 35, 140, 15);
  CBSize:=TComboBox.Create(Self, csDropDownList);
  CBSize.SetBounds(55, 85, 80, 24);
  for i:=0 to SIZES-1 do
    CBSize.ItemAdd(SizeNames[i]);
  CBSize.ItemIndex:=0;
  CBSize.OnChange:=SizeChange;
  BOK:=TButton.Create(Self, 'OK');
  BOK.SetPosition(140, 85);
  BOK.Default:=true;
  BOK.OnClick:=OKClick;
  BCancel:=TButton.Create(Self, 'Cancel');
  BCancel.SetPosition(220, 85);
  BCancel.OnClick:=CancelClick;
end;

procedure TFindForm.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then OKClick(BOK);
  if Key=VK_ESCAPE then CancelClick(BCancel);
end;

procedure TFindForm.SizeChange(Sender: TObject);

  procedure Sizes(Min, Max: Boolean);
  begin
    LSizeMin.Visible:=Min;
    ESizeMin.Visible:=Min;
    LSizeMax.Visible:=Max;
    ESizeMax.Visible:=Max;
  end;

begin
  case CBSize.ItemIndex of
    SIZE_NONE: Sizes(false, false);
    SIZE_MIN: Sizes(true, false);
    SIZE_MAX: Sizes(false, true);
    SIZE_BETWEEN: Sizes(true, true);
  end;
end; 

procedure TFindForm.OKClick(Sender: TObject);
begin
  with MainForm do
  begin
    Mask:=EMask.Text;
    SizeMin:=StrToInt64Def(ESizeMin.Text, 0);
    SizeMax:=StrToInt64Def(ESizeMax.Text, 0);
    SizeMode:=CBSize.ItemIndex;
    FindFiles:=CFiles.Checked;
    FindDirs:=CDirs.Checked;
    FindDelay:=true;
    RefreshTimer.Enabled:=true;
  end;
  Close;
end;

procedure TFindForm.CancelClick(Sender: TObject);
begin
  Close;
end;

begin
  MainForm:=TMainForm.Create;
  MainForm.Run;
  MainForm.Free;
end.

