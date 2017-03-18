program FileList;

{$R *.res}
{$R FileListMan.res}

uses Windows, AvL, avlUtils, UCLAPI;

type
  TMainForm=class(TForm)
    LSrc, LDest: TLabel;
    ESrc, EDest: TEdit;
    CAll, CCompr, CUnicode: TCheckBox;
    BSrc, BDest, BSearch: TButton;
    procedure SrcClick(Sender: TObject);
    procedure DestClick(Sender: TObject);
    procedure SearchClick(Sender: Tobject);
    procedure CancelClick(Sender: TObject);
    procedure AllClick(Sender: TObject);
    procedure SrcChange(Sender: TObject);
    procedure GetAllFiles(Mask: string; Files: TStringList);
    procedure GetAllFilesU(Mask: WideString; Files: TStringList);
  end;

const
  IDA: Cardinal=$4C465356;
  IDU: Cardinal=$55465356;
  Capt='VgaSoft FileList 3.0';

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
  Size, BufSize, Index, ID: Cardinal;
  Res: Integer;
  Drives, Dest, Text: string;
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
    CUnicode.Enabled:=Enable;
    BDest.Enabled:=Enable;
  end;

  function GetText: string;
  var
    i, L: Integer;
    P: PChar;
    S: string;
  const
    CRLF=#$0D#$00#$0A#$00;
  begin
    if CUnicode.Checked then
    begin
      L:=0;
      for i:=0 to Files.Count-1 do Inc(L, Length(Files[i])+4);
      SetString(Result, nil, L);
      P:=PChar(Result);
      for i:=0 to Files.Count-1 do
      begin
        S:=Files[i]+CRLF;
        Move(S[1], P^, Length(S));
        Inc(P, Length(S));
      end;
    end
      else Result:=Files.Text;
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
      if CUnicode.Checked then
      begin
        GetAllFilesU(WideString(ESrc.Text+'*.*'), Files);
        ID:=IDU;
      end
      else begin
        GetAllFiles(ESrc.Text+'*.*', Files);
        ID:=IDA;
      end;
      if FCancel then Exit;
      BSearch.Enabled:=false;
      Caption:=Capt+': Saving to <'+ExtractFileName(EDest.Text)+'>: 0%';
      Progress:=0;
      ProcessMessages;
      OFile:=TFileStream.Create(EDest.Text, fmCreate);
      OFile.WriteBuffer(ID, SizeOf(ID));
      Text:=GetText;
      if CCompr.Checked then
      begin
        Size:=Length(Text);
        OFile.WriteBuffer(Size, SizeOf(Size));
        BufSize:=UCLOutputBlockSize(Size);
        GetMem(Buffer, BufSize);
        UCLCB.Callback:=UCLProgressCallback;
        UCLCB.User:=Pointer(Size);
        Res:=ucl_nrv2e_99_compress(Pointer(Text), Size, Buffer, BufSize, @UCLCB, 10, nil, nil);
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
        OFile.Write(Text[1], Length(Text));
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
    while (i>0) and (S[i]<>'\') do Dec(i);
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

procedure TMainForm.GetAllFilesU(Mask: WideString; Files: TStringList);
var
  i: Integer;
  F: THandle;
  FD: TWin32FindDataW;
  Directory: WideString;
  Directories: TStringList;
const
  Delims = [WideChar('\'), WideChar(':')];

  function GetLastDirectory(S: WideString): WideString;
  var
    i: integer;
  begin
    Delete(S, Length(S), 1);
    i:=Length(S);
    while (i>0) and (S[i]<>'\') do Dec(i);
    Result:=Copy(S, i+1, MaxInt);
  end;

  function ExtractFilePath(const FileName: WideString): WideString;
  var
    I: Integer;
  begin
    I := Length(FileName);
    while (I > 1) and not (FileName[I] in Delims) do Dec(I);
    if FileName[I] in Delims
      then Result := Copy(FileName, 1, I)
      else Result:='';
  end;

  function ExtractFileName(const FileName: WideString): WideString;
  var
    I: Integer;
  begin
    I := Length(FileName);
    while (I >= 1) and not (FileName[I] in Delims) do Dec(I);
    Result := Copy(FileName, I + 1, MAX_PATH);
  end;

  function WS2S(const S: WideString): string;
  begin
    SetLength(Result, Length(S) * 2);
    Move(S[1], Result[1], Length(Result));
  end;

begin
  Directory := ExtractFilePath(Mask);
  Files.Add(WS2S('<'+GetLastDirectory(Directory)+'>'));
  Directories:=TStringList.Create;
  try
    F:=FindFirstFileW(PWideChar(Mask), FD);
    if F=INVALID_HANDLE_VALUE then
    begin
      Files.Add(WS2S('*** ACCESS DENIED ***|0'));
      Exit;
    end;
    repeat
      if FD.dwFileAttributes and (faDirectory or faVolumeID) = 0 then
        Files.Add(WS2S(WideString(FD.cFileName)+'|'+Int64ToStr((Int64(FD.nFileSizeHigh) shl 32) or FD.nFileSizeLow)))
      else if (FD.dwFileAttributes and faDirectory <> 0) and (WideString(FD.cFileName) <> '.') and (WideString(FD.cFileName) <> '..') then
        Directories.Add(WS2S(Directory + WideString(FD.cFileName) + '\' + ExtractFileName(Mask)));
    until not FindNextFileW(F, FD);
    Windows.FindClose(F);
    ProcessMessages;
    if FCancel then Exit;
    for i:=0 to Directories.Count-1 do
    begin
      GetAllFilesU(WideString(Pointer(Directories[i])), Files);
      if FCancel then Exit;
    end;
  finally
    Files.Add(WS2S('<|>'));
    FAN(Directories);
    ProcessMessages;
  end;
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
    CAll.SetBounds(60, 70, 100, 15);
    CCompr:=TCheckBox.Create(MainForm, 'Compress');
    CCompr.SetBounds(165, 70, 70, 15);
    CCompr.Checked:=true;
    CUnicode:=TCheckBox.Create(MainForm, 'Unicode');
    CUnicode.SetBounds(240, 70, 70, 15);
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
