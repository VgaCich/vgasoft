program FileList;

{$R *.res}
{$R FileListMan.res}

uses Windows, AvL, avlUtils, UCLAPI;

type
  TMainForm=class(TForm)
    LSrc, LDest: TLabel;
    ESrc, EDest: TEdit;
    CAll: TCheckBox;
    BSrc, BDest, BSearch: TButton;
    procedure SrcClick(Sender: TObject);
    procedure DestClick(Sender: TObject);
    procedure SearchClick(Sender: Tobject);
    procedure AllClick(Sender: TObject);
  end;

const
  ID: Cardinal=1279677270;
  Capt='VgaSoft FileList 2.2';

var
  MainForm: TMainForm;

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
Label
  NextDisk;

  procedure GetAllFiles(Mask: string);
  var
    Search: TSearchRec;
    Directory: string;

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
        Files.Add(Search.Name+'|'+IntToStr(Search.Size));
      until FindNext(Search) <> 0;
    end;
    if FindFirst(Directory + '*.*', faDirectory+faHidden, Search) = 0 then
    begin
      repeat
        if ((Search.Attr and faDirectory) = faDirectory) and (Search.Name<>'.') and (Search.Name<>'..') then
          GetAllFiles(Directory + Search.Name + '\' + ExtractFileName(Mask));
      until FindNext(Search) <> 0;
      FindClose(Search);
    end;
    Files.Add('<|>');
  end;

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

begin
  Index:=0;
  Drives:=GetDrives;
  Dest:=EDest.Text;
  NextDisk:
  if CAll.Checked then
  begin
    Inc(Index);
    ESrc.Text:=Drives[Index]+':\';
    EDest.Text:=Copy(Dest, 1, LastDelimiter('.', Dest)-1)+UpCase(Drives[Index])+ExtractFileExt(Dest);
  end;
  if not DirectoryExists(ESrc.Text) then Exit;
  if EDest.Text='' then Exit;
  if ESrc.Text[Length(ESrc.Text)]<>'\' then ESrc.Text:=ESrc.Text+'\';
  try
    ProcessMessages;
    Buffer:=nil;
    Files:=TStringList.Create;
    Files.Clear;
    Caption:=Capt+': Поиск...';
    GetAllFiles(ESrc.Text+'*.*');
    ProcessMessages;
    Size:=Length(Files.Text);
    OFile:=TFileStream.Create(EDest.Text, fmCreate);
    OFile.WriteBuffer(ID, SizeOf(ID));
    OFile.WriteBuffer(Size, SizeOf(Size));
    Caption:=Capt+': Сохранение...';
    BufSize:=UCLOutputBlockSize(Size);
    GetMem(Buffer, BufSize);
    Res:=ucl_nrv2e_99_compress(Pointer(Files.Text), Size, Buffer, BufSize, nil, 10, nil, nil);
    if Res<>UCL_E_OK then
    begin
      MessageBox(Handle, PChar(Format('Compression error %d', [Res])), 'Error', MB_ICONERROR);
      Exit;
    end;
    OFile.Write(Buffer^, BufSize);
    ProcessMessages;
  finally
    FreeMemAndNil(Buffer, UCLOutputBlockSize(Size));
    FAN(OFile);
    FAN(Files);
  end;
  if CAll.Checked then
    if Index<Length(Drives)
      then goto NextDisk;
  EDest.Text:=Dest;
  Caption:=Capt;
  ShowMessage('Выполнено');
end;

procedure TMainForm.AllClick(Sender: TObject);
begin
  ESrc.Visible:=not CAll.Checked;
  LSrc.Visible:=ESrc.Visible;
  BSrc.Visible:=ESrc.Visible;
end;

begin
  MainForm:=TMainForm.Create(nil, Capt);
  with MainForm do
  begin
    SetSize(400, Height-ClientHeight+95);
    Position:=poScreenCenter;
    BorderStyle:=bsDialog;
    LSrc:=TLabel.Create(MainForm, 'Где искать:');
    LSrc.Alignment:=SS_RIGHT;
    LSrc.SetBounds(5, 10, 80, 15);
    LDest:=TLabel.Create(MainForm, 'Сохранить в:');
    LDest.Alignment:=SS_RIGHT;
    LDest.SetBounds(5, 40, 80, 15);
    ESrc:=TEdit.Create(MainForm, 'C:\');
    ESrc.SetBounds(90, 5, ClientWidth-175, 24);
    EDest:=TEdit.Create(MainForm, 'C:\List.vfl');
    EDest.SetBounds(90, 35, ClientWidth-175, 24);
    if ParamCount>0 then ESrc.Text:=ParamStr(1);
    if ParamCount>1 then EDest.Text:=ParamStr(2);
    CAll:=TCheckBox.Create(MainForm, 'Все диски');
    CAll.OnClick:=AllClick;
    CAll.SetBounds(90, 70, 100, 15);
    BSrc:=TButton.Create(MainForm, 'Обзор...');
    BSrc.OnClick:=SrcClick;
    BSrc.SetBounds(ClientWidth-80, 5, 75, 25);
    BDest:=TButton.Create(MainForm, 'Обзор...');
    BDest.OnClick:=DestClick;
    BDest.SetBounds(ClientWidth-80, 35, 75, 25);
    BSearch:=TButton.Create(MainForm, 'Искать');
    BSearch.OnClick:=SearchClick;
    BSearch.SetBounds(ClientWidth-80, 65, 75, 25);
    Run;
  end;
end.
