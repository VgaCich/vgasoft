unit VSEPakMan;

interface

uses
  Windows, AvL, avlMath, avlUtils, UCLAPI, avlLZMADec, avlAdler32, avlMasks,
  VSEManagers;

type
  TFileSource=(fsFile, fsPakNRV, fsPakLZMA, fsPakStore);

  PFileInfo=^TFileInfo;
  TFileInfo=record
    Source: TFileSource;
    Name,
    PakFile: string;
    Offset, Hash: Cardinal;
  end;

  TFileInfoRes=record
    Source: TFileSource;
    PakFile: string;
    Offset: Cardinal;
  end;

  TMountPoint=record
    Exist: Boolean;
    MountPoint, Source: string;
  end;

  TDirInfo=class;

  TDirInfo=class(TObject)
  public
    Name, Source: string;
    Hash: Cardinal;
    Files: array of TFileInfo;
    Dirs: array of TDirInfo;
    FilesRes: array of TFileInfoRes;
    constructor Create(const Dir: string);
    destructor Destroy; override;
    function  AddDir(const Name: string): Integer;
    function  AddFile(const Name: string): Integer;
    procedure ReadDir(Dir: string);
    function  FindDir(Name: string): Integer;
    function  FindFile(Name: string): Integer;
  end;

  TPakMan=class(TManager)
  private
    FIndex: TDirInfo;
    FMountPoints: array of TMountPoint;
    FBaseDir: string;
    FOpenedFiles: TList;
  protected
    procedure Init; override;
    procedure Cleanup; override;
    function  FindFile(Name: string): PFileInfo;
    procedure Mount(MountPoint: TMountPoint);
    procedure LoadPak(const PakName: string);
    procedure CloseFile(F: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    function  AddMountPoint(MountPoint, Source: string): Boolean;
    function  DeleteMountPoint(MountPoint: string): Boolean;
    function  OpenFile(const Name: string; Flags: Cardinal): TStream;
    function  CreateFile(Name: string; Flags: Cardinal): TStream;
    procedure DeleteFile(Name: string);
    function  FileExists(const Name: string): Boolean;
    procedure FindFiles(const Mask: string; Recursive: Boolean; List: TStringList);
  end;

  TPakFileStream=class(TFileStream)
  private
    FPakMan: TPakMan;
  public
    constructor Create(FileInfo: TFileInfo; Flags: Cardinal; PakMan: TPakMan);
    destructor Destroy; override;
  end;

  TPakNRVStream=class(TStream)
  private
    FPakMan: TPakMan;
    FBuffer: Pointer;
    FSize, FPosition, FIndexLen: Integer;
    FInStream: TFileStream;
    FIndex: packed array of Cardinal;
    procedure UnpackBuffer;
  protected
    function GetSize: Integer; override;
    function GetPosition: Integer; override;
  public
    constructor Create(FileInfo: TFileInfo; Flags: Cardinal; PakMan: TPakMan);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TPakLZMAStream=class(TStream)
  private
    FPakMan: TPakMan;
    FInStream: TFileStream;
    FDecompressor: TLZMADecompressor;
    FSize, FPosition: Integer;
  protected
    function GetSize: Integer; override;
    function GetPosition: Integer; override;
  public
    constructor Create(FileInfo: TFileInfo; Flags: Cardinal; PakMan: TPakMan);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TPakStoreStream=class(TStream)
  private
    FPakMan: TPakMan;
    FSize, FOffset: Integer;
    FInStream: TFileStream;
  protected
    function GetSize: Integer; override;
    function GetPosition: Integer; override;
  public
    constructor Create(FileInfo: TFileInfo; Flags: Cardinal; PakMan: TPakMan);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

function ExtractFileExt(const FileName: string): string;
function ExtractFileName(const FileName: string): string;
function ExtractFilePath(const FileName: string): string;

const
  //TPakMan.Open/CreateFile:Flags - accepts FileOpen modes if File.Source=fsFile
  ofNoCreate=$00010000;
  ofNoCheck=$00020000;

var
  PakMan: TPakMan;

implementation

uses VSELog, VSEInit;

{$I PakTypes.inc}

const
  PakExt='.vpk';
  PakMask='*'+PakExt;
  Sep='/';
  CBufSize=1048576;

{ExtractFile* functions}

function ExtractFileExt(const FileName: string): string;
var
  i: Integer;
begin
  i:=Length(FileName);
  while (i>1) and not (FileName[i] in [Sep, '.']) do Dec(i);
  if (i>1) and (FileName[i]='.')
    then Result:=Copy(FileName, i+1, MaxInt)
    else Result:='';
end;

function ExtractFileName(const FileName: string): string;
var
  i: Integer;
begin
  i:=Length(FileName);
  while (i>=1) and (FileName[i]<>Sep) do Dec(i);
  Result:=Copy(FileName, i+1, MaxInt);
end;

function ExtractFilePath(const FileName: string): string;
var
  i: Integer;
begin
  i:=Length(FileName);
  while (i>1) and (FileName[i]<>Sep) do Dec(i);
  if FileName[i]=Sep
    then Result:=Copy(FileName, 1, i)
    else Result:='';
end;

{TDirInfo}

constructor TDirInfo.Create(const Dir: string);
begin
  inherited Create;
  Name:=LowerCase(Dir);
  Hash:=NextAdler32(1, @Name[1], Length(Name));
end;

destructor TDirInfo.Destroy;
var
  i: Integer;
begin
  for i:=0 to High(Dirs) do
    if Assigned(Dirs[i]) then Dirs[i].Free;
  Finalize(FilesRes);
  Finalize(Files);
  Finalize(Dirs);
  inherited Destroy;
end;

function TDirInfo.AddDir(const Name: string): Integer;
begin
  Result:=FindDir(Name);
  if Result<0 then
  begin
    SetLength(Dirs, Length(Dirs)+1);
    Dirs[High(Dirs)]:=TDirInfo.Create(Name);
    if Source<>'' then Dirs[High(Dirs)].Source:=Source+Name+'\';
    Result:=High(Dirs);
  end;
end;

function TDirInfo.AddFile(const Name: string): Integer;
begin
  Result:=FindFile(Name);
  if Result<0 then
  begin
    SetLength(Files, Length(Files)+1);
    Result:=High(Files);
    Files[Result].Name:=LowerCase(Name);
    Files[Result].Hash:=NextAdler32(1, @Files[Result].Name[1], Length(Files[Result].Name));
    Files[Result].Source:=fsFile;
    Files[Result].Offset:=0;
  end;
end;

procedure TDirInfo.ReadDir(Dir: string);
var
  SR: TSearchRec;
  i: Integer;
  SubDir: TDirInfo;
begin
  if Source<>'' then Dir:=Source;
  for i:=0 to High(Dirs) do
    Dirs[i].ReadDir(Dir+Dirs[i].Name+'\');
  if FindFirst(Dir+'*', faAnyFile, SR)=0 then
    repeat
      if (SR.Name='.') or (SR.Name='..') or ((SR.Attr and faDirectory=0) and (LowerCase(AvL.ExtractFileExt(SR.Name))=PakExt)) then Continue;
      if (SR.Attr and faDirectory)<>0 then
      begin
        if FindDir(SR.Name)<0 then
        begin
          SubDir:=Dirs[AddDir(SR.Name)];
          SubDir.ReadDir(Dir+SR.Name+'\');
        end;
      end
      else
        with Files[AddFile(SR.Name)] do
        begin
          if Source<>fsFile then
          begin
            SetLength(FilesRes, Length(FilesRes)+1);
            FilesRes[High(FilesRes)].Source:=Source;
            FilesRes[High(FilesRes)].PakFile:=PakFile;
            FilesRes[High(FilesRes)].Offset:=Offset;
            Offset:=High(FilesRes)+1;
          end;
          Source:=fsFile;
          PakFile:=Dir;
        end;
    until FindNext(SR)<>0;
  FindClose(SR);
  for i:=0 to High(Files) do
    if (Files[i].Source=fsFile) and not FileExists(Files[i].PakFile+Files[i].Name) then
      with Files[i] do
        if Offset>0 then
        begin
          Source:=FilesRes[Offset-1].Source;
          PakFile:=FilesRes[Offset-1].PakFile;
          Offset:=FilesRes[Offset-1].Offset;
        end
        else begin
          Name:='';
          PakFile:='';
        end;
end;

function TDirInfo.FindDir(Name: string): Integer;
var
  Hash: Cardinal;
begin
  Result:=-1;
  Name:=LowerCase(Name);
  Hash:=NextAdler32(1, @Name[1], Length(Name));
  for Result:=0 to High(Dirs) do
    if (Dirs[Result].Hash=Hash) and (Dirs[Result].Name=Name) then Exit;
  Result:=-1;
end;

function TDirInfo.FindFile(Name: string): Integer;
var
  Hash: Cardinal;
begin
  Result:=-1;
  if Name='' then Exit;
  Name:=LowerCase(Name);
  Hash:=NextAdler32(1, @Name[1], Length(Name));
  for Result:=0 to High(Files) do
    if (Files[Result].Hash=Hash) and (Files[Result].Name=Name) then Exit;
  Result:=-1;
end;

{TPakMan}

constructor TPakMan.Create;
begin
  LogNC(llInfo, 'PakMan: Create');
  inherited Create;
  FOpenedFiles:=TList.Create;
  SetLength(FMountPoints, 8);
end;

destructor TPakMan.Destroy;
var
  i: Integer;
begin
  LogNC(llInfo, 'PakMan: Destroy');
  if Assigned(FIndex) then FAN(FIndex);
  Finalize(FMountPoints);
  if FOpenedFiles.Count>0 then
  begin
    LogF(llWarning, 'PakMan: %d files not closed', [FOpenedFiles.Count]);
    for i:=0 to FOpenedFiles.Count-1 do
      if Assigned(FOpenedFiles[i]) then TObject(FOpenedFiles[i]).Free;
    FOpenedFiles.Clear;
  end;
  if PakMan=Self then PakMan:=nil;
  FAN(FOpenedFiles);
  inherited Destroy;
end;

procedure TPakMan.Init;
var
  SR: TSearchRec;
  i: Integer;
  Paks: TStringList;
begin
  if Assigned(FIndex) then Cleanup;
  Log(llInfo, 'PakMan: Initialize');
  FBaseDir:=AddTrailingBackslash(BaseDir);
  FIndex:=TDirInfo.Create('');
  for i:=0 to  High(FMountPoints) do
    if FMountPoints[i].Exist then Mount(FMountPoints[i]);
  Paks:=TStringList.Create;
  try
    if FindFirst(FBaseDir+PakMask, 0, SR)=0 then
      repeat Paks.Add(SR.Name);
      until FindNext(SR)<>0;
    FindClose(SR);
    Paks.Sort;
    for i:=0 to Paks.Count-1 do LoadPak(FBaseDir+Paks[i]);
  finally
    FAN(Paks);
  end;
  FIndex.ReadDir(FBaseDir);
end;

procedure TPakMan.Cleanup;
var
  i: Integer;
begin
  Log(llInfo, 'PakMan: Cleanup');
  if Assigned(FIndex) then FAN(FIndex);
  if FOpenedFiles.Count>0 then
  begin
    LogF(llWarning, 'PakMan: %d files not closed', [FOpenedFiles.Count]);
    for i:=0 to FOpenedFiles.Count-1 do
      if Assigned(FOpenedFiles[i]) then TObject(FOpenedFiles[i]).Free;
    FOpenedFiles.Clear;
  end;
end;

function TPakMan.AddMountPoint(MountPoint, Source: string): Boolean;
var
  i, Index: Integer;
begin
  Result:=false;
  try
    if Pos(Sep, MountPoint)>0
      then raise Exception.CreateFmt('PakMan: AddMountPoint(%s, %s) failed: invalid mount point', [MountPoint, Source]);
    Index:=-1;
    MountPoint:=LowerCase(MountPoint);
    for i:=0 to High(FMountPoints) do
    begin
      if (Index=-1) and not FMountPoints[i].Exist then Index:=i;
      if FMountPoints[i].Exist and (FMountPoints[i].MountPoint=MountPoint)
        then raise Exception.CreateFmt('PakMan: AddMountPoint(%s, %s) failed: mount point already exists', [MountPoint, Source]);
    end;
    Source:=AddTrailingBackslash(ExpandFileName(Source));
    if not DirectoryExists(Source) then
      if not ForceDirectories(Source)
        then raise Exception.CreateFmt('PakMan: AddMountPoint(%s, %s) failed: cannot create source dir', [MountPoint, Source]);
    if Index=-1 then
    begin
      Index:=Length(FMountPoints);
      SetLength(FMountPoints, 2*Index);
    end;
    FMountPoints[Index].Exist:=true;
    FMountPoints[Index].MountPoint:=MountPoint;
    FMountPoints[Index].Source:=Source;
    Result:=true;
  except
    on E:Exception do Log(llError, E.Message);
  end;
end;

function TPakMan.DeleteMountPoint(MountPoint: string): Boolean;
var
  i: Integer;
begin
  Result:=false;
  MountPoint:=LowerCase(MountPoint);
  for i:=0 to High(FMountPoints) do
    if FMountPoints[i].Exist and (FMountPoints[i].MountPoint=MountPoint) then
      with FMountPoints[i] do
      begin
        Exist:=false;
        MountPoint:='';
        Source:='';
        Result:=true;
        Exit;
      end;
  LogF(llError, 'PakMan: DeleteMountPoint(%s) failed: mount point not exists', [MountPoint]);
end;

function TPakMan.OpenFile(const Name: string; Flags: Cardinal): TStream;
var
  FI: PFileInfo;
begin
  Result:=nil;
  if FIndex=nil then Exit;
  FI:=FindFile(Name);
  if FI<>nil then
  begin
    case FI.Source of
      fsFile: Result:=TPakFileStream.Create(FI^, Flags, Self);
      fsPakNRV: Result:=TPakNRVStream.Create(FI^, Flags, Self);
      fsPakLZMA: Result:=TPakLZMAStream.Create(FI^, Flags, Self);
      fsPakStore: Result:=TPakStoreStream.Create(FI^, Flags, Self);
    end;
    FOpenedFiles.Add(Result);
  end
    else if Flags and ofNoCreate=0
      then Result:=CreateFile(Name, Flags)
      else begin
        Log(llError, 'PakMan: OpenFile('+Name+') failed: file not exists');
        Exit;
      end;
end;

function TPakMan.CreateFile(Name: string; Flags: Cardinal): TStream;
var
  CurTok, NextTok, DestDir: string;
  CurDir: TDirInfo;
  i: Integer;
  FI: TFileInfo;
begin
  Result:=nil;
  if FIndex=nil then Exit;
  NextTok:=Tok(Sep, Name);
  CurDir:=FIndex;
  DestDir:=FBaseDir;
  while true do
  begin
    CurTok:=NextTok;
    NextTok:=Tok(Sep, Name);
    if NextTok<>'' then
    begin
      i:=CurDir.AddDir(CurTok);
      if (CurDir.Source<>'') and (CurDir.Dirs[i].Source='')
        then CurDir.Dirs[i].Source:=CurDir.Source+CurTok+'\';
      CurDir:=CurDir.Dirs[i];
      DestDir:=DestDir+CurTok+'\';
    end
      else Break;
  end;
  if CurDir.Source<>'' then DestDir:=CurDir.Source;
  if not ForceDirectories(DestDir) then
  begin
    Log(llError, 'PakMan: CreateFile('+Name+') failed: cannot create destination dir');
    Exit;
  end;
  if AvL.FileExists(DestDir+CurTok) then AvL.DeleteFile(DestDir+CurTok);
  FI.PakFile:=DestDir;
  FI.Name:=CurTok;
  Result:=TPakFileStream.Create(FI, fmCreate, Self);
  FOpenedFiles.Add(Result);
  FIndex.ReadDir(FBaseDir);
end;

procedure TPakMan.CloseFile(F: TStream);
begin
  if not Assigned(F) then Exit;
  FOpenedFiles.Remove(F);
end;

procedure TPakMan.DeleteFile(Name: string);
var
  FI: PFileInfo;
begin
  if FIndex=nil then Exit;
  FI:=FindFile(Name);
  if FI=nil then
  begin
    Log(llError, 'PakMan: DeleteFile('+Name+') failed: file not exists');
    Exit;
  end;
  if FI.Source<>fsFile then
  begin
    Log(llError, 'PakMan: DeleteFile('+Name+') failed: file read-only');
    Exit;
  end;
  AvL.DeleteFile(FI.PakFile+FI.Name);
  FIndex.ReadDir(FBaseDir);
end;

function TPakMan.FileExists(const Name: string): Boolean;
begin
  Result:=FindFile(Name)<>nil;
end;

procedure TPakMan.FindFiles(const Mask: string; Recursive: Boolean; List: TStringList);
var
  Dir: TDirInfo;
  Path, S: string;
  i: Integer;
  M: TMask;

  procedure Find(Dir: TDirInfo; const CurPath: string);
  var
    i: Integer;
  begin
    for i:=0 to High(Dir.Files) do
      if (Dir.Files[i].Name<>'') and M.Matches(Dir.Files[i].Name)
        then List.Add(CurPath+Dir.Files[i].Name);
    if Recursive then
      for i:=0 to High(Dir.Dirs) do
        if Dir.Dirs[i].Name<>''
          then Find(Dir.Dirs[i], CurPath+Dir.Dirs[i].Name+Sep);
  end;

begin
  List.Clear;
  if FIndex=nil then Exit;
  Path:=ExtractFilePath(Mask);
  M:=TMask.Create(ExtractFileName(Mask));
  try
    Dir:=FIndex;
    S:=Tok(Sep, Path);
    while S<>'' do
    begin
      i:=Dir.FindDir(S);
      if i<0
        then Exit
        else Dir:=Dir.Dirs[i];
      S:=Tok(Sep, Path);
    end;
    Find(Dir, ExtractFilePath(Mask));
  finally
    FAN(M);
  end;
end;

function TPakMan.FindFile(Name: string): PFileInfo;
var
  CurTok, NextTok: string;
  CurDir: TDirInfo;
  i: Integer;
begin
  Result:=nil;
  if FIndex=nil then Exit;
  NextTok:=Tok(Sep, Name);
  CurDir:=FIndex;
  repeat
    CurTok:=NextTok;
    NextTok:=Tok(Sep, Name);
    if NextTok='' then
    begin
      i:=CurDir.FindFile(CurTok);
      if i>=0 then Result:=@CurDir.Files[i];
      Break;
    end
    else begin
      i:=CurDir.FindDir(CurTok);
      if i>=0
        then CurDir:=CurDir.Dirs[i]
        else Break;
    end;
  until false;
end;

procedure TPakMan.Mount(MountPoint: TMountPoint);
var
  Dir: TDirInfo;
begin
  if FIndex=nil then Exit;
  Dir:=FIndex.Dirs[FIndex.AddDir(MountPoint.MountPoint)];
  Dir.Source:=MountPoint.Source;
end;

procedure TPakMan.LoadPak(const PakName: string);
type
  TFTRec=record
    FTEntry: TPakFTEntry;
    Name: string;
  end;
var
  Pak: TFileStream;
  FTData: TMemoryStream;
  FTPacked: Pointer;
  Hdr: TPakHdr;
  FileTable: array of TFTRec;
  i, CurEntry, OffsetInc: Cardinal;
  Res: Integer;

  procedure ProcessEntry(CurDir: TDirInfo);
  var
    i, Index: Integer;
  begin
    with FileTable[CurEntry] do
      case FTEntry.EntryType of
        FTE_DIR: begin
          Index:=CurDir.AddDir(Name);
          for i:=0 to FTEntry.EntriesCount-1 do
          begin
            Inc(CurEntry);
            ProcessEntry(CurDir.Dirs[Index]);
          end
        end;
        FTE_NRV, FTE_LZMA, FTE_STORE:
          with CurDir.Files[CurDir.AddFile(Name)] do
          begin
            case FTEntry.EntryType of
              FTE_NRV: Source:=fsPakNRV;
              FTE_LZMA: Source:=fsPakLZMA;
              FTE_STORE: Source:=fsPakStore;
            end;
            PakFile:=PakName;
            Offset:=OffsetInc+FTEntry.Offset;
          end;
        FTE_DISCARD: begin
          Index:=CurDir.FindFile(Name);
          if Index>-1 then
          begin
            CurDir.Files[Index].Name:='';
            CurDir.Files[Index].PakFile:='';
          end;
        end;
      end;
  end;

begin
  Pak:=TFileStream.Create(PakName, fmOpenRead);
  try
    try
      Pak.Read(Hdr, SizeOf(Hdr));
      if Hdr.ID<>PakID
        then raise Exception.Create('PakMan: LoadPak('+AvL.ExtractFileName(PakName)+') failed: file is not VgaSoft Package');
      Pak.Seek(8, soFromBeginning);
      if Hdr.Adler32<>StreamAdler32(Pak, SizeOf(Hdr)-8+Hdr.FTPackedSize)
        then raise Exception.Create('PakMan: LoadPak('+AvL.ExtractFileName(PakName)+') failed: package corrupted');
      Pak.Seek(SizeOf(Hdr), soFromBeginning);
      FTPacked:=nil;
      GetMem(FTPacked, Hdr.FTPackedSize);
      try
        FTData:=TMemoryStream.Create;
        FTData.Size:=Hdr.FTSize;
        Pak.Read(FTPacked^, Hdr.FTPackedSize);
        OffsetInc:=Pak.Position;
        Res:=ucl_nrv2e_decompress_asm_safe_8(FTPacked, Hdr.FTPackedSize, FTData.Memory, Hdr.FTSize, nil);
        if Res<>UCL_E_OK
          then raise Exception.Create('PakMan: LoadPak('+AvL.ExtractFileName(PakName)+') failed: FileTable decompression error ('+IntToStr(Res)+')');
        SetLength(FileTable, Hdr.EntriesCount);
        for i:=0 to Hdr.EntriesCount-1 do
        begin
          FTData.Read(FileTable[i].FTEntry, SizeOf(TPakFTEntry));
          SetLength(FileTable[i].Name, FileTable[i].FTEntry.NameSize);
          FTData.Read(FileTable[i].Name[1], FileTable[i].FTEntry.NameSize);
        end;
      finally
        FAN(FTData);
        if FTPacked<>nil then FreeMem(FTPacked);
      end;
      CurEntry:=0;
      while CurEntry<Hdr.EntriesCount do
      begin
        ProcessEntry(FIndex);
        Inc(CurEntry);
      end;
    except
      on E: Exception do Log(llError, E.Message);
    end;
  finally
    Finalize(FileTable);
    FAN(Pak);
  end;
end;

{TPakFileStream}

constructor TPakFileStream.Create(FileInfo: TFileInfo; Flags: Cardinal; PakMan: TPakMan);
begin
  inherited Create(FileInfo.PakFile+FileInfo.Name, Flags and $FFFF);
  FPakMan:=PakMan;
end;

destructor TPakFileStream.Destroy;
begin
  FPakMan.CloseFile(Self);
  inherited Destroy;
end;

{TPakNRVStream}

constructor TPakNRVStream.Create(FileInfo: TFileInfo; Flags: Cardinal; PakMan: TPakMan);
var
  i: Integer;
  Adler32, CheckSize: Cardinal;
begin
  inherited Create;
  FPakMan:=PakMan;
  GetMem(FBuffer, UCLOutputBlockSize(CBufSize));
  FInStream:=TFileStream.Create(FileInfo.PakFile, fmOpenRead or fmShareDenyWrite);
  FInStream.Seek(FileInfo.Offset, soFromBeginning);
  FInStream.Read(Adler32, 4);
  FInStream.Read(CheckSize, 4);
  if Flags and ofNoCheck=0 then
    if Adler32<>StreamAdler32(FInStream, CheckSize)
      then raise Exception.Create('Source data is corrupted')
      else FInStream.Seek(FileInfo.Offset+8, soFromBeginning);
  FInStream.Read(FSize, 4);
  FIndexLen:=Ceil(FSize/CBufSize);
  SetLength(FIndex, FIndexLen+1);
  FInStream.Read(FIndex[1], FIndexLen*4);
  FIndex[0]:=FInStream.Position;
  for i:=1 to FIndexLen do
    FIndex[i]:=FIndex[i-1]+FIndex[i];
  FPosition:=0;
  UnpackBuffer;
end;

destructor TPakNRVStream.Destroy;
begin
  FPakMan.CloseFile(Self);
  FAN(FInStream);
  FreeMemAndNil(FBuffer, UCLOutputBlockSize(CBufSize));
  Finalize(FIndex);
  inherited Destroy;
end;

procedure TPakNRVStream.UnpackBuffer;
var
  i: Integer;
  BS: Cardinal;
  InBuffer: Pointer;
begin
  i:=FPosition div CBufSize;
  if FPosition=FSize then Exit;
  if (i>=FIndexLen) or (i<0) then raise Exception.Create('Index position out of bounds');
  FInStream.Seek(FIndex[i], soFromBeginning);
  i:=FIndex[i+1]-FIndex[i];
  InBuffer:=IncPtr(FBuffer, UCLOutputBlockSize(CBufSize)-i);
  if FInStream.Read(InBuffer^, i)<>i then raise Exception.Create('Pak file reading error');
  BS:=CBufSize;
  i:=ucl_nrv2e_decompress_asm_safe_8(InBuffer, i, FBuffer, BS, nil);
  if i<>UCL_E_OK then raise Exception.CreateFmt('Buffer decompression error: %d', [i]);
end;

function TPakNRVStream.GetSize: Integer;
begin
  Result:=FSize;
end;

function TPakNRVStream.GetPosition: Integer;
begin
  Result:=FPosition;
end;

function TPakNRVStream.Read(var Buffer; Count: Longint): Longint;
var
  BufPos, OBufPos, BufSize, IncSize, OldPos: integer;
begin
  OBufPos:=0;
  OldPos:=FPosition;
  BufPos:=FPosition mod CBufSize;
  if FPosition div CBufSize<FIndexLen-1
    then BufSize:=CBufSize
    else BufSize:=FSize-CBufSize*(FIndexLen-1);
  if FPosition+Count>FSize then Count:=FSize-FPosition;
  while (BufSize-BufPos)<Count do
  begin
    IncSize:=BufSize-BufPos;
    CopyMemory(IncPtr(@Buffer, OBufPos), IncPtr(FBuffer, BufPos), IncSize);
    Inc(FPosition, IncSize);
    try
      UnpackBuffer;
      BufPos:=0;
    except
      raise;
      Exit;
    end;
    Inc(OBufPos, IncSize);
    Dec(Count, IncSize);
  end;
  CopyMemory(IncPtr(@Buffer, OBufPos), IncPtr(FBuffer, BufPos), Count);
  Inc(FPosition, Count);
  Result:=FPosition-OldPos;
  UnpackBuffer; 
end;

function TPakNRVStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Stream is read only');
end;

function TPakNRVStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  OldPos: Integer;
begin
  OldPos:=FPosition;
  case Origin of
    soFromBeginning: FPosition:=Offset;
    soFromCurrent: FPosition:=FPosition+Offset;
    soFromEnd: FPosition:=FSize+Offset;
  end;
  if (FPosition<0) or (FPosition>FSize) then
  begin
    FPosition:=OldPos;
    Result:=FPosition;
    raise Exception.Create('Seek offset out of bounds');
  end;
  Result:=FPosition;
  if OldPos div CBufSize<>FPosition div CBufSize then UnpackBuffer;
end;

{TPakLZMAStream}

constructor TPakLZMAStream.Create(FileInfo: TFileInfo; Flags: Cardinal; PakMan: TPakMan);
var
  Adler32, CheckSize: Cardinal;
begin
  inherited Create;
  FPakMan:=PakMan;
  FInStream:=TFileStream.Create(FileInfo.PakFile, fmOpenRead or fmShareDenyWrite);
  FInStream.Seek(FileInfo.Offset, soFromBeginning);
  FInStream.Read(Adler32, 4);
  FInStream.Read(CheckSize, 4);
  if Flags and ofNoCheck=0 then
    if Adler32<>StreamAdler32(FInStream, CheckSize)
      then raise Exception.Create('Source data is corrupted')
      else FInStream.Seek(FileInfo.Offset+8, soFromBeginning);
  FInStream.Read(FSize, 4);
  FDecompressor:=TLZMADecompressor.Create(FInStream.Read);
  FPosition:=0;
end;

destructor TPakLZMAStream.Destroy;
begin
  FPakMan.CloseFile(Self);
  FAN(FDecompressor);
  FAN(FInStream);
  inherited Destroy;
end;

function TPakLZMAStream.GetSize: Integer;
begin
  Result:=FSize;
end;

function TPakLZMAStream.GetPosition: Integer;
begin
  Result:=FPosition;
end;

function TPakLZMAStream.Read(var Buffer; Count: Longint): Longint;
begin
  if FPosition+Count>FSize then Count:=FSize-FPosition;
  FDecompressor.DecompressInto(Buffer, Count);
  Inc(FPosition, Count);
  Result:=Count;
end;

function TPakLZMAStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Stream is read only');
end;

function TPakLZMAStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  OldPos: Integer;
  Buf: Pointer;
begin
  OldPos:=FPosition;
  case Origin of
    soFromBeginning: FPosition:=Offset;
    soFromCurrent: FPosition:=FPosition+Offset;
    soFromEnd: FPosition:=FSize+Offset;
  end;
  if (FPosition<OldPos) or (FPosition>FSize) then
  begin
    Result:=FPosition;
    FPosition:=OldPos;
    raise Exception.Create('Seek offset out of bounds');
  end;
  Buf:=nil;
  GetMem(Buf, FPosition-OldPos);
  try
    FDecompressor.DecompressInto(Buf^, FPosition-OldPos);
  finally
    if Buf<>nil then FreeMem(Buf);
  end;
  Result:=FPosition;
end;

{TPakStoreStream}

constructor TPakStoreStream.Create(FileInfo: TFileInfo; Flags: Cardinal; PakMan: TPakMan);
var
  Adler32: Cardinal;
begin
  inherited Create;
  FPakMan:=PakMan;
  FInStream:=TFileStream.Create(FileInfo.PakFile, fmOpenRead or fmShareDenyWrite);
  FInStream.Seek(FileInfo.Offset, soFromBeginning);
  FInStream.Read(Adler32, 4);
  FInStream.Read(FSize, 4);
  FOffset:=FInStream.Position;
  if Flags and ofNoCheck=0 then
    if Adler32<>StreamAdler32(FInStream, FSize)
      then raise Exception.Create('Source data is corrupted')
      else FInStream.Seek(FOffset, soFromBeginning);
end;

destructor TPakStoreStream.Destroy;
begin
  FPakMan.CloseFile(Self);
  FAN(FInStream);
  inherited Destroy;
end;

function TPakStoreStream.GetSize: Integer;
begin
  Result:=FSize;
end;

function TPakStoreStream.GetPosition: Integer;
begin
  Result:=FInStream.Position-FOffset;
  if Result<0 then
  begin
    FInStream.Seek(FOffset, soFromBeginning);
    Result:=0;
  end;
  if Result>FSize then
  begin
    FInStream.Seek(FOffset+FSize, soFromBeginning);
    Result:=FSize;
  end;
end;

function TPakStoreStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count>FSize-Position then Count:=FSize-Position;
  Result:=FInStream.Read(Buffer, Count);
end;

function TPakStoreStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Stream is read only');
end;

function TPakStoreStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FInStream.Seek(FOffset+Offset, soFromBeginning);
    soFromCurrent: FInStream.Seek(Offset, soFromCurrent);
    soFromEnd: FInStream.Seek(FSize+FOffset+Offset, soFromBeginning);
  end;
  Result:=Position;
end;

initialization
  PakMan:=TPakMan.Create;

end.
