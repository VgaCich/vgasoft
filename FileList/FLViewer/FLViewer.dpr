program FLViewer;

{$R *.res}
{$R FLViewerRes.res}

uses Windows, Messages, AvL, avlUtils, UCLAPI, ShellAPI, avlMasks;

const
  TVN_FIRST=0-400;
  TVN_SELCHANGED=TVN_FIRST-2;

type
  TExtR=record
    Hash: Integer;
    IsLong: Boolean;
    Ext: string;
  end;
  TFolderData=class
  public
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
    PStatus, PSplitter: TPanel;
    PBStatus: TProgressBar;
    LStatus: TLabel;
    TBImages, Icons, Folders: TImageList;
    Data, Files: TStringList;
    Garbage: TList;
    FileName, Mask: string;
    SplitMoving, Initialized, OpenDelay, FindDelay, FindFiles, FindDirs: Boolean;
    RefreshTimer: TTimer;
    CurFolder, SplitColor, ExtsCount, ExtsCapacity: Integer;
    SizeMin, SizeMax, SizeMode: Cardinal;
    Exts: array of TExtR;
  protected
    procedure FormCreate(Sender: TObject);
    function  FormClose(Sender: TObject): Boolean;
    procedure OpenClick;
    procedure SaveClick;
    procedure FindClick;
    procedure SplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SplitterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RefreshTick(Sender: TObject);
    procedure ShowStatusPanel(const Caption: string);
    procedure HideStatusPanel;
    procedure Find;
    procedure Open;
    function  Load: Boolean;
    function  GetExtHash(const Ext: string): Integer;
    function  AddExt(FileName: string): Integer;
    function  FindExt(FileName: string): Integer;
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
  ID: Cardinal=1279677270;
  CCapt='VgaSoft FileList Viewer 2.5';
  AboutText=CCapt+#13#10 +
            'Copyright '#169'VgaSoft, 2004-2007';

  TB_OPEN=0;
  TB_SAVE=1;
  TB_FIND=2;
  TB_REFRESH=3;
  TB_ABOUT=4;
  TB_EXIT=5;

  SPLITTERWIDTH=8;

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

var
  MainForm: TMainForm;
  FindForm: TFindForm;

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

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Folders:=TImageList.Create;
  Folders.AddMasked(LoadImage(hInstance, 'Folder', IMAGE_BITMAP, 0, 0, 0), clWhite);
  Icons:=TImageList.Create;
  Icons.Height:=48;
  Icons.Width:=48;
  ExtsCapacity:=512;
  ExtsCount:=0;
  SetLength(Exts, ExtsCapacity);
  Data:=TStringList.Create;
  Garbage:=TList.Create;
  for i:=1 to ParamCount do
    if FileExists(ParamStr(i)) then
    begin
      FileName:=ParamStr(i);
      OpenDelay:=true;
      RefreshTimer.Enabled:=true;
      Break;
    end;
end;

function TMainForm.FormClose(Sender: TObject): Boolean;
begin
  ClearTree;
  FAN(Folders);
  FAN(Icons);
  Finalize(Exts);
  FAN(Data);
  FAN(Garbage);
  Result:=true;
end;

procedure TMainForm.WMCommand(var Msg: TWMCommand);
begin
  if Msg.Ctl=TB.Handle then
    case Msg.ItemID of
      TB_OPEN: OpenClick;
      TB_SAVE: SaveClick;
      TB_FIND: FindClick;
      TB_REFRESH: FillList;
      TB_ABOUT: ShowAbout;
      TB_EXIT: Close;
    end;
end;

procedure TMainForm.WMSize(var Msg: TWMSize);
begin
  if not Initialized then Exit;
  TB.Width:=ClientWidth;
  TVFolders.Height:=ClientHeight-TB.Height-SB.Height;
  LVFiles.SetSize(ClientWidth-TVFolders.Width-SPLITTERWIDTH, TVFolders.Height);
  PSplitter.SetSize(SPLITTERWIDTH, TVFolders.Height);
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
  S, S1: string;
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
        S1:=Copy(Files[i], 1, LastDelimiter('|', Files[i])-1);
        if S1=S then
        begin
          SB.SetPartText(2, 0, S1+': '+
            SizeToStr(StrToInt(Copy(Files[i], LastDelimiter('|', Files[i])+1, MaxInt))));
          Break;
        end;
      end;
    end;
end;

procedure TMainForm.OpenClick;
begin
  if OpenSaveDialog(Handle, true, '', 'vfl', 'VgaSoft FileList files|*.vfl|All files|*.*',
    ExtractFilePath(FileName), 0, OFN_FILEMUSTEXIST, FileName) then Open;
end;

procedure TMainForm.SaveClick;

  function Indent(Level: Integer): string;
  begin
    Result:='';
    while Level>1 do
    begin
      Result:=Result+'  ';
      Dec(Level);
    end;
  end;

var
  FN: string;
  i, Level, Pr, OldPr: Integer;
  F: TextFile;
begin
  if Data.Count<=0 then
  begin
    MessageDlg('Nothing to save', Caption, MB_ICONERROR);
    Exit;
  end;
  FN:=ChangeFileExt(FileName, '.txt');
  if not OpenSaveDialog(Handle, false, '', 'txt', 'Text files|*.txt|All files|*.*',
    ExtractFilePath(FN), 0, OFN_OVERWRITEPROMPT or OFN_PATHMUSTEXIST, FN) then Exit;
  ShowStatusPanel('Saving...');  
  Level:=0;
  Pr:=0;
  OldPr:=0;
  PBStatus.Position:=Pr;
  AssignFile(F, FN);
  try
    ReWrite(F);
    for i:=0 to Data.Count-1 do
    begin
      if Data[i][1]='<'
        then begin
          if Data[i][2]='|' then Dec(Level)
          else begin
            WriteLn(F, Indent(Level)+'['+Copy(Data[i], 2, Length(Data[i])-2)+']');
            Inc(Level);
          end;
        end
        else WriteLn(F, Indent(Level)+'+-'+Copy(Data[i], 1, FirstDelimiter('|', Data[i])-1)+
                     ': '+SizeToStr(StrToCar(Copy(Data[i], FirstDelimiter('|', Data[i])+1, MaxInt))));
      Pr:=Round(100*(i/Data.Count));
      if Pr>OldPr then PBStatus.Position:=Pr;
      OldPr:=Pr;
    end;
  finally
    CloseFile(F);
    HideStatusPanel;
    //PostMessage(PStatus.Handle, WM_CLOSE, 0, 0);
  end;
end;

procedure TMainForm.FindClick;
begin
  if not Assigned(FindForm)
    then FindForm:=TFindForm.Create(Self, 'Find');
  FindForm.EMask.SetFocus;
  FindForm.ShowModal;
end;

procedure TMainForm.SplitterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if SplitMoving then
  begin
    GetCursorPos(P);
    P.X:=P.X-Left-Width+ClientWidth;
    if (P.X<10) then P.X:=10;
    if (P.X>ClientWidth-10-SPLITTERWIDTH) then P.X:=ClientWidth-10-SPLITTERWIDTH;
    PSplitter.Left:=P.X;
  end;
end;

procedure TMainForm.SplitterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
  begin
    SplitMoving:=true;
    PSplitter.Color:=clBlack;
    SetCapture(PSplitter.Handle);
  end;
end;

procedure TMainForm.SplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and SplitMoving then
  begin
    ReleaseCapture;
    SplitMoving:=false;
    PSplitter.Color:=SplitColor;
    TVFolders.Width:=PSplitter.Left-Left-(Width-ClientWidth) div 2;
    LVFiles.SetBounds(TVFolders.Width+SPLITTERWIDTH, TB.Height,
      ClientWidth-TVFolders.Width-SPLITTERWIDTH, LVFiles.Height);
  end;
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
  if not Assigned(PStatus) then PStatus:=TPanel.Create(Self, '');
  PStatus.BringToFront;
  PStatus.Bevel:=bvRaised;
  PStatus.SetBounds(ClientWidth div 2 - 100, ClientHeight div 2 - 22, 200, 45);
  if not Assigned(LStatus)
    then LStatus:=TLabel.Create(PStatus, Caption)
    else LStatus.Caption:=Caption;
  LStatus.Transparent:=true;
  LStatus.SetBounds(5, 5, 190, 15);
  if not Assigned(PBStatus) then PBStatus:=TProgressBar.Create(PStatus);
  PBStatus.SetBounds(5, 25, 190, 15);
  PBStatus.Position:=0;
  LVFiles.Enabled:=false;
  PSplitter.Enabled:=false;
  TB.Enabled:=false;
  TVFolders.Enabled:=false;
  ProcessMessages;
end;

procedure TMainForm.HideStatusPanel;
begin
  LVFiles.Enabled:=true;
  PSplitter.Enabled:=true;
  TB.Enabled:=true;
  TVFolders.Enabled:=true;
  PostMessage(PStatus.Handle, WM_CLOSE, 0, 0);
end;

procedure TMainForm.Find;
var
  S: string;
  i, Pr, OldPr: Integer;
  Sz: Cardinal;
  List: TStringList;
  CMask: TMask;

  function FillFindNode(Node: Integer; const S: string): Int64;
  var
    FD: TFolderData;
    j: Integer;
  begin
    FD:=TFolderData.Create;
    Garbage.Add(FD);
    Inc(i);
    Node:=InsertTreeItem(Node, S, FD);
    while true do
    begin
      if i=List.Count then Exit;
      case List[i][1] of
        '>', '|': FD.FolderSize:=FD.FolderSize+FillFindNode(Node, Copy(List[i], 2, MaxInt));
        '<': Break;
        '+': FD.Files.Add(Copy(List[i], 2, MaxInt));
      end;
      Inc(i);
    end;
    for j:=0 to FD.Files.Count-1 do
      FD.FilesSize:=FD.FilesSize+StrToInt(Copy(FD.Files[i], FirstDelimiter('|', FD.Files[i])+1, MaxInt));
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
      S:=Data[i];
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
      if FindFiles and CMask.Matches(Copy(S, 1, LastDelimiter('|', S)-1)) then
      begin
        Sz:=StrToInt(Copy(S, LastDelimiter('|', S)+1, MaxInt));
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
  IBuffer, OBuffer: Pointer;
  IBufSize, OBufSize, OBFTemp, Res: Cardinal;
  IFile: TFileStream;
begin
  IBuffer:=nil;
  OBuffer:=nil;
  Result:=false;
  try
    IFile:=TFileStream.Create(FileName, fmOpenRead);
    IFile.Read(IBufSize, SizeOf(IBufSize));
    if IBufSize<>ID then
    begin
      MessageBox(Handle, 'Error: invalid file', 'Error', MB_ICONERROR);
      Exit;
    end;
    IFile.Read(OBufSize, SizeOf(OBufSize));
    IBufSize:=IFile.Size-SizeOf(IBufSize)*2;
    if OBufSize<>0 then
    begin
      OBFTemp:=OBufSize;
      GetMem(IBuffer, IBufSize);
      GetMem(OBuffer, OBufSize);
      IFile.Read(IBuffer^, IBufSize);
      Res:=ucl_nrv2e_decompress_asm_safe_8(IBuffer, IBufSize, OBuffer, OBFTemp, nil);
      if Res<>UCL_E_OK then
      begin
        MessageBox(Handle, PChar(Format('NRV2E decompressing error: %d', [Res])), 'Error', MB_ICONERROR);
        Exit;
      end;
      if OBFTemp<>OBufSize then
        if MessageBox(Handle, 'Data corrupted. Continue?', 'Error', MB_ICONERROR or MB_YESNO)=ID_NO then Exit;
      Data.Text:=string(OBuffer);
    end
    else begin
      GetMem(IBuffer, IBufSize);
      IFile.Read(IBuffer^, IBufSize);
      Data.Text:=string(IBuffer);
    end;
    RemoveVoidStrings(Data);
    PBStatus.Position:=10;
    Result:=true;
  finally
    FAN(IFile);
    FreeMemAndNil(IBuffer, IBufSize);
    FreeMemAndNil(OBuffer, OBufSize);
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

function TMainForm.GetNextFolder: string;
var
  S: string;
begin
  Result:='';
  if CurFolder=Data.Count-1 then Exit;
  Inc(CurFolder);
  S:=Data[CurFolder];
  while S[1]<>'<' do
  begin
    if CurFolder<>Data.Count-1
      then Inc(CurFolder)
      else Exit;
    S:=Data[CurFolder];
  end;
  Result:=Copy(S, 2, Length(S)-2);
end;

procedure TMainForm.GetFolderFiles(Files: TStringList);

  function NextFolder: Integer;
  begin
    Result:=CurFolder;
    if CurFolder=Data.Count-1 then Exit;
    Inc(Result);
    while Data[Result][1]<>'<' do Inc(Result);
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
  GetFolderFiles(FD.Files);
  for i:=0 to FD.Files.Count-1 do
    FD.FilesSize:=FD.FilesSize+StrToInt(Copy(FD.Files[i], FirstDelimiter('|', FD.Files[i])+1, MaxInt));
  FD.FolderSize:=FD.FilesSize;
  if Node=Integer(TVI_ROOT) then Files:=FD.Files;
  Node:=InsertTreeItem(Node, S, FD);
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
  FillNode(Integer(TVI_ROOT), GetNextFolder);
  TVFolders.EndUpdate;
  ProcessMessages;
end;

procedure TMainForm.FillList;
var
  i, Index, IcoIndex: integer;
  FN: string;
  PSFI: SHFILEINFO;
  ImgList: HIMAGELIST;
begin
  if not Assigned(Files) then Exit;
  LVFiles.BeginUpdate;
  LVFiles.Clear;
  for i:= 0 to Files.Count-1 do
  begin
    FN:=Copy(Files[i], 1, LastDelimiter('|', Files[i])-1);
    Index:=LVFiles.ItemAdd(FN);
    IcoIndex:=FindExt(FN);
    if IcoIndex>-1
      then LVFiles.ItemImageIndex[Index]:=IcoIndex
    else begin
      ImgList:=SHGetFileInfo(PChar(string(ExtractFileExt(FN))), FILE_ATTRIBUTE_NORMAL, PSFI, SizeOf(PSFI),
                             SHGFI_SYSICONINDEX or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES or
                             SHGFI_ICON or SHGFI_LARGEICON);
      Icons.AddIcon(ImageList_GetIcon(ImgList, PSFI.iIcon, 0));
      LVFiles.ItemImageIndex[Index]:=AddExt(FN);
    end;
  end;
  if (LVFiles.LargeImages=nil) and (ExtsCount>0) then LVFiles.LargeImages:=Icons;
  LVFiles.EndUpdate;
end;

procedure TMainForm.ClearTree;
var
  i: Integer;
begin
  for i:=0 to Garbage.Count-1 do
    if Assigned(Garbage[i]) then TObject(Garbage[i]).Free;
  Garbage.Clear;
  TVFolders.Perform(TV_FIRST+1, 0, Integer(TVI_ROOT));//Clear tree view
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
    SizeMin:=StrToInt(ESizeMin.Text);
    SizeMax:=StrToInt(ESizeMax.Text);
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
  MainForm:=TMainForm.Create(nil, CCapt);
  with MainForm do
  begin
    Initialized:=false;
    OpenDelay:=false;
    FindDelay:=false;
    CanvasInit;
    SetSize(640, 480);
    Position:=poScreenCenter;
    OnCreate:=FormCreate;
    OnClose:=FormClose;
    TBImages:=TImageList.Create;
    TBImages.AddMasked(LoadImage(hInstance, 'TB', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
    TB:=TToolBar.Create(MainForm, True);
    TB.SetPosition(0, 0);
    TB.Images:=TBImages;
    TB.ButtonAdd('Open', TB_OPEN);
    TB.ButtonAdd('Save', TB_SAVE);
    TB.ButtonAdd('Find', TB_FIND);
    TB.ButtonAdd('Refresh', TB_REFRESH);
    TB.ButtonAdd('About', TB_ABOUT);
    TB.ButtonAdd('Exit', TB_EXIT);
    SB:=TStatusBar.Create(MainForm, '');
    SB.SetParts(SB_PARTS, SBParts);
    Folders:=TImageList.Create;
    Folders.AddMasked(LoadImage(hInstance, 'FOLDER', IMAGE_BITMAP, 0, 0, 0), clFuchsia);
    TVFolders:=TTreeView.Create(MainForm);
    TVFolders.Images:=Folders;
    TVFolders.StateImages:=Folders;
    TVFolders.SetBounds(0, TB.Height, 200, ClientHeight-TB.Height-SB.Height);
    PSplitter:=TPanel.Create(MainForm, '');
    PSplitter.Bevel:=bvLowered;
    PSplitter.SetBounds(TVFolders.Width, TB.Height, SPLITTERWIDTH, TVFolders.Height);
    PSplitter.OnMouseMove:=SplitterMouseMove;
    PSplitter.OnMouseDown:=SplitterMouseDown;
    PSplitter.OnMouseUp:=SplitterMouseUp;
    PSplitter.Cursor:=LoadCursor(0, IDC_SIZEWE);
    SplitColor:=PSplitter.Color;
    SplitMoving:=false;
    LVFiles:=TListView.Create(MainForm);
    LVFiles.Style:=LVFiles.Style or LVS_SORTASCENDING;
    LVFiles.SetBounds(TVFolders.Width+SPLITTERWIDTH, TB.Height,
      ClientWidth-TVFolders.Width-SPLITTERWIDTH, TVFolders.Height);
    PSplitter.BringToFront;
    RefreshTimer:=TTimer.CreateEx(100, false);
    RefreshTimer.OnTimer:=RefreshTick;
    Initialized:=true;
  end;
  MainForm.Run;
end.

