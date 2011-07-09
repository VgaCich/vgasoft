program FileList;

{$R *.res}
{$R FileListMan.res}

uses Windows, AvL, avlUtils, UCLAPI;

type
  TMainForm=class(TForm)
    LSrc, LDest: TLabel;
    ESrc, EDest: TEdit;
    CAll, CCompr: TCheckBox;
    BSrc, BDest, BSearch: TButton;
    procedure SrcClick(Sender: TObject);
    procedure DestClick(Sender: TObject);
    procedure SearchClick(Sender: Tobject);
    procedure CancelClick(Sender: TObject);
    procedure AllClick(Sender: TObject);
    procedure SrcChange(Sender: TObject);
    procedure GetAllFiles(Mask: string; Files: TStringList);
  end;

const
  ID: Cardinal=1279677270;
  Capt='VgaSoft FileList 2.9';

var
  MainForm: TMainForm;
  Progress: Integer;
  FCancel: Boolean;
  
procedure UCLProgressCallback(TextSize, CodeSize: Cardinal; State: Integer; User: Pointer);
var
  S: string;
begin
  if Progress<>Round(100*(TextSize/Integer(User))) then
  begin
    Progress:=Round(100*(TextSize/Integer(User)));
    S:=MainForm.Caption;
    MainForm.Caption:=Copy(S, 1, LastDelimiter(':', S)+1)+IntToStr(Progress)+'%';
  end;
  MainForm.ProcessMessages;
end;

procedure TMainForm.SrcClick(Sender: TObject);
var
  Path: string;
begin
  Path:=ESrc.Text;
  if OpenDirDialog(Handle, '', true, Path)
    then ESrc.Text:=Path;
end;

procedure TMainForm.DestClick(Sender: TObject);
var
  Path: string;
begin
  Path:=EDest.Text;
  if OpenSaveDialog(Handle, false, '', 'vfl', 'VgaSoft FileList files|*.vfl|All files|*.*',
    ExtractFilePath(Path), 0, OFN_OVERWRITEPROMPT, Path)
    then EDest.Text:=Path;
end;

procedure TMainForm.SearchClick(Sender: TObject);
var
  Files: TStringList;
  Buffer: Pointer;
  OFile: TFileStream;
  Size, BufSize, Index: Cardinal;
  Res: Integer;
  Drives, Dest: string;
  UCLCB: TUCLProgressCallback;
Label
  NextDisk;

  function GetDrives: string;
  var
    i: Integer;
    Buf: array[0..255] of Char;
  begin
    Result:='';
    FillMemory(@Buf, 256, 0);
    GetLogicalDriveStrings(SizeOf(Buf), Buf);
    for i:=0 to Length(Buf) do
      if GetDriveType(PChar(Buf[i]+':\'))=DRIVE_FIXED then
        Result:=Result+Buf[i];
  end;

  procedure EnableAll(Enable: Boolean);
  begin
    if not CAll.Checked then
    begin
      ESrc.Enabled:=Enable;
      BSrc.Enabled:=Enable;
    end;
    EDest.Enabled:=Enable;
    CAll.Enabled:=Enable;
    CCompr.Enabled:=Enable;
    BDest.Enabled:=Enable;
  end;

begin
  EnableAll(false);
  BSearch.Caption:='Cancel';
  BSearch.OnClick:=CancelClick;
  try
    Index:=0;
    Drives:=GetDrives;
    Dest:=EDest.Text;
    NextDisk:
    if CAll.Checked then
    begin
      Inc(Index);
      ESrc.Text:=Drives[Index]+':\';
      if Pos('%s', Dest)=0
        then EDest.Text:=Copy(Dest, 1, LastDelimiter('.', Dest)-1)+'%s'+ExtractFileExt(Dest)
        else EDest.Text:=Dest;
      EDest.Text:=Format(EDest.Text, [UpperCase(Drives[Index])]);
    end;
    if not DirectoryExists(ESrc.Text) then
    begin
      MessageBox(Handle, 'Search directory not exists', 'Error', MB_ICONERROR);
      Exit;
    end;
    if EDest.Text='' then
    begin
      MessageBox(Handle, 'Filename not specified', 'Error', MB_ICONERROR);
      Exit;
    end;
    if ESrc.Text[Length(ESrc.Text)]<>'\' then ESrc.Text:=ESrc.Text+'\';
    try
      ProcessMessages;
      Buffer:=nil;
      Files:=TStringList.Create;
      Files.Clear;
      Caption:=Capt+': Searching in <'+ESrc.Text+'>...';
      GetAllFiles(ESrc.Text+'*.*', Files);
      if FCancel then Exit;
      BSearch.Enabled:=false;
      Caption:=Capt+': Saving to <'+ExtractFileName(EDest.Text)+'>: 0%';
      Progress:=0;
      ProcessMessages;
      OFile:=TFileStream.Create(EDest.Text, fmCreate);
      OFile.WriteBuffer(ID, SizeOf(ID));
      if CCompr.Checked then
      begin
        Size:=Length(Files.Text);
        OFile.WriteBuffer(Size, SizeOf(Size));
        BufSize:=UCLOutputBlockSize(Size);
        GetMem(Buffer, BufSize);
        UCLCB.Callback:=UCLProgressCallback;
        UCLCB.User:=Pointer(Size);
        Res:=ucl_nrv2e_99_compress(Pointer(Files.Text), Size, Buffer, BufSize, @UCLCB, 10, nil, nil);
        if Res<>UCL_E_OK then
        begin
          MessageBox(Handle, PChar(Format('Compression error %d', [Res])), 'Error', MB_ICONERROR);
          Exit;
        end;
        OFile.Write(Buffer^, BufSize);
      end
      else begin
        Size:=0;
        OFile.WriteBuffer(Size, SizeOf(Size));
        Files.SaveToStream(OFile);
      end;
      ProcessMessages;
    finally
      FreeMemAndNil(Buffer, UCLOutputBlockSize(Size));
      FAN(OFile);
      FAN(Files);
      BSearch.Enabled:=true;
    end;
    if CAll.Checked then
      if Index<Length(Drives)
        then goto NextDisk;
    Caption:=Capt;
    ShowMessage('Job done');
  finally
    EDest.Text:=Dest;
    EnableAll(true);
    BSearch.Caption:='Search';
    BSearch.OnClick:=SearchClick;
    FCancel:=false;
  end;
end;

procedure TMainForm.CancelClick(Sender: TObject);
begin
  FCancel:=true;
end;

procedure TMainForm.AllClick(Sender: TObject);
begin
  ESrc.Enabled:=not CAll.Checked;
  LSrc.Enabled:=not CAll.Checked;
  BSrc.Enabled:=not CAll.Checked;
end;

procedure TMainForm.SrcChange(Sender: TObject);
begin
  EDest.Text:=ExcludeTrailingBackslash(ESrc.Text)+'.vfl';
end;

procedure TMainForm.GetAllFiles(Mask: string; Files: TStringList);
var
  i: Integer;
  Search: TSearchRec;
  Directory: string;
  Directories: TStringList;

    function GetLastDirectory(S: string): string;
    var
      i: integer;
    begin
      Delete(S, Length(S), 1);
      i:=Length(S);
      while S[i]<>'\' do Dec(i);
      Result:=Copy(S, i+1, MaxInt);
    end;

begin
  Directory := ExtractFilePath(Mask);
  Files.Add('<'+GetLastDirectory(Directory)+'>');
  if FindFirst(Mask, faAnyFile and not faDirectory, Search) = 0 then
  begin
    repeat
      Files.Add(Search.Name+'|'+Int64ToStr((Int64(Search.FindData.nFileSizeHigh) shl 32) or Search.FindData.nFileSizeLow));
    until FindNext(Search) <> 0;
  end;
  FindClose(Search);
  ProcessMessages;
  if FCancel then Exit;
  Directories:=TStringList.Create;
  try
    if FindFirst(Directory + '*.*', faDirectory+faHidden, Search) = 0 then
    begin
      repeat
        if ((Search.Attr and faDirectory) = faDirectory) and (Search.Name<>'.') and (Search.Name<>'..') then
          Directories.Add(Directory + Search.Name + '\' + ExtractFileName(Mask));
      until FindNext(Search) <> 0;
      FindClose(Search);
    end;
    for i:=0 to Directories.Count-1 do
    begin
      GetAllFiles(Directories[i], Files);
      if FCancel then Exit;
    end;
  finally
    FAN(Directories);
    ProcessMessages;
  end;
  Files.Add('<|>');
end;

begin
  MainForm:=TMainForm.Create(nil, Capt);
  with MainForm do
  begin
    SetSize(400, Height-ClientHeight+95);
    Position:=poScreenCenter;
    BorderStyle:=bsDialog;
    LSrc:=TLabel.Create(MainForm, 'Search in:');
    LSrc.Alignment:=SS_RIGHT;
    LSrc.SetBounds(5, 10, 50, 15);
    LDest:=TLabel.Create(MainForm, 'Save to:');
    LDest.Alignment:=SS_RIGHT;
    LDest.SetBounds(5, 40, 50, 15);
    ESrc:=TEdit.Create(MainForm, 'C:\');
    ESrc.SetBounds(60, 5, ClientWidth-145, 24);
    ESrc.OnChange:=SrcChange;
    EDest:=TEdit.Create(MainForm, 'C:\List.vfl');
    EDest.SetBounds(60, 35, ClientWidth-145, 24);
    if ParamCount>0 then ESrc.Text:=ParamStr(1);
    if ParamCount>1 then EDest.Text:=ParamStr(2);
    CAll:=TCheckBox.Create(MainForm, 'Search all drives');
    CAll.OnClick:=AllClick;
    CAll.SetBounds(60, 70, 120, 15);
    CCompr:=TCheckBox.Create(MainForm, 'Use compression');
    CCompr.SetBounds(185, 70, 120, 15);
    CCompr.Checked:=true;
    BSrc:=TButton.Create(MainForm, 'Browse...');
    BSrc.OnClick:=SrcClick;
    BSrc.SetBounds(ClientWidth-80, 5, 75, 25);
    BDest:=TButton.Create(MainForm, 'Browse...');
    BDest.OnClick:=DestClick;
    BDest.SetBounds(ClientWidth-80, 35, 75, 25);
    BSearch:=TButton.Create(MainForm, 'Search');
    BSearch.OnClick:=SearchClick;
    BSearch.SetBounds(ClientWidth-80, 65, 75, 25);
    FCancel:=false;
    Run;
  end;
end.
