program VSPak;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysSfIni, Windows, AvL, avlMath, avlUtils, UCLAPI, avlLZMA, avlAdler32, avlMasks;

{$I ../../PakTypes.inc}

const
  CBufSize=1048576;

function FirstDelimiter(const Delimiters, S: String): Integer;
var
  n: LongWord;
begin
  Result:=Length(S)+1;
  for n:=Result to Length(S) do
   if S[n]=Delimiters then
    begin
     Result:=n;
     Exit;
    end;
end;

procedure About;
begin
  WriteLn('VSE VSPak package management program 1.3.0');
  WriteLn('(c)VgaSoft, 2004-2006');
  WriteLn;
end;

procedure Usage;
begin
  WriteLn('Usage:');
  WriteLn('VSPak file.vpp');
  WriteLn('  Create package from project file.vpp');
  WriteLn('VSPak /cmd [options] file.vpk');
  WriteLn('  Open package file.vpk');
  WriteLn('  Commands:');
  WriteLn('    l: list package contents');
  WriteLn('    x: extract package');
  WriteLn('  Options (used only by extract):');
  WriteLn('    /d<DIR> - extract to dir (default: current directory)');
  WriteLn('    /m<MASK> - extract files by mask (default: *)');
  WriteLn('VSPak /c [options] dir');
  WriteLn('  Create project from dir');
  WriteLn('  Options:');
  WriteLn('    /p<COMPRESSION> - set compression to:');
  WriteLn('      NRV/Level, Level: 1...10');
  WriteLn('      LZMA/Level, Level: Fast, Normal, Max, Ultra');
  WriteLn('      Store');
  WriteLn('      Discard');
  WriteLn('    Default: NRV/10');
  WriteLn('    /a - ask compression from user');
  WriteLn('Common options:');
  WriteLn('  /w - wait for a key after done');
end;

function PrjDirName(const PrjStr: string; Line: Integer): string;
const
  ErrStr='%d: directory entry incorrect';
var
  Len: Integer;
begin
  Len:=Length(PrjStr);
  if Len<3 then raise Exception.CreateFmt(ErrStr, [Line]);
  if (PrjStr[1]<>'<') or (PrjStr[Len]<>'>') then raise Exception.CreateFmt(ErrStr, [Line]);
  Result:=Copy(PrjStr, 2, Len-2);
  if (Pos('\', Result)>0) or (Length(Result)>255) then raise Exception.CreateFmt(ErrStr, [Line]);
end;

function PrjFileName(const PrjStr: string; Line: Integer): string;
var
  P: Integer;
begin
  P:=Pos('|', PrjStr);
  if P>0
    then Result:=Copy(PrjStr, 1, P-1)
    else Result:=PrjStr;
  if (Pos('\', Result)>0) or (Length(Result)>255) then raise Exception.CreateFmt('%d: file entry incorrect', [Line]);
end;

function PrjCompression(const PrjStr: string; Line: Integer): string;
var
  P1, P2: Integer;
begin
  P1:=Pos('|', PrjStr);
  P2:=Pos('/', PrjStr);
  if P1=0 then raise Exception.CreateFmt('%d: file entry incorrect: compression not defined', [Line]);
  if P2>0
    then P2:=P2-P1-1
    else P2:=MaxInt;
  Result:=LowerCase(Copy(PrjStr, P1+1, P2));
end;

function PrjComprLevel(const PrjStr: string; Line: Integer): string;
var
  P: Integer;
begin
  P:=Pos('/', PrjStr);
  if P=0
    then Result:=''
    else Result:=LowerCase(Copy(PrjStr, P+1, MaxInt));
end;

function LoadPak(const PakName: string; List: TStringList): Boolean;
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

  procedure ProcessEntry;
  var
    i: Integer;
  begin
    with FileTable[CurEntry] do
      case FTEntry.EntryType of
        FTE_DIR: begin
          List.Add('<'+Name+'>');
          for i:=0 to FTEntry.EntriesCount-1 do
          begin
            Inc(CurEntry);
            ProcessEntry;
          end;
          List.Add('<|>');
        end;
        FTE_NRV: List.Add(Name+'|NRV/'+IntToStr(OffsetInc+FTEntry.Offset));
        FTE_LZMA: List.Add(Name+'|LZMA/'+IntToStr(OffsetInc+FTEntry.Offset));
        FTE_STORE: List.Add(Name+'|Store/'+IntToStr(OffsetInc+FTEntry.Offset));
        FTE_DISCARD: List.Add(Name+'|Discard/'+IntToStr(OffsetInc+FTEntry.Offset));
      end;
  end;

begin
  Result:=false;
  if not FileExists(PakName) then
  begin
    WriteLn('LoadPak('+AvL.ExtractFileName(PakName)+') failed: file not found');
    Exit;
  end;
  Pak:=TFileStream.Create(PakName, fmOpenRead);
  try
    Pak.Read(Hdr, SizeOf(Hdr));
    if Hdr.ID<>PakID then
    begin
      WriteLn('LoadPak('+AvL.ExtractFileName(PakName)+') failed: file is not VgaSoft Package');
      Exit;
    end;
    Pak.Seek(8, soFromBeginning);
    if Hdr.Adler32<>StreamAdler32(Pak, SizeOf(Hdr)-8+Hdr.FTPackedSize) then
    begin
      WriteLn('LoadPak('+AvL.ExtractFileName(PakName)+') failed: package corrupted');
      Exit;
    end;
    Pak.Seek(SizeOf(Hdr), soFromBeginning);
    FTPacked:=nil;
    GetMem(FTPacked, Hdr.FTPackedSize);
    try
      FTData:=TMemoryStream.Create;
      FTData.Size:=Hdr.FTSize;
      Pak.Read(FTPacked^, Hdr.FTPackedSize);
      OffsetInc:=Pak.Position;
      Res:=ucl_nrv2e_decompress_asm_safe_8(FTPacked, Hdr.FTPackedSize, FTData.Memory, Hdr.FTSize, nil);
      if Res<>UCL_E_OK then
      begin
        WriteLn('LoadPak('+AvL.ExtractFileName(PakName)+') failed: FileTable decompression error ('+IntToStr(Res)+')');
        Exit;
      end;
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
      ProcessEntry;
      Inc(CurEntry);
    end;
    Result:=true;
  finally
    Finalize(FileTable);
    FAN(Pak);
  end;
end;

procedure ListContents(const PakName: string);
var
  List: TStringList;
  i: Integer;
begin
  WriteLn('List: ', PakName);
  List:=TStringList.Create;
  try
    if not LoadPak(PakName, List) then Exit;
    RemoveVoidStrings(List);
    for i:=0 to List.Count-1 do
      if List[i][1]='<'
        then WriteLn(List[i])
        else WriteLn(Copy(List[i], 1, LastDelimiter('/', List[i])-1));
  finally
    FAN(List);
  end;
end;

function CheckAdler32(Pak: TStream): Boolean;
var
  Adler32, CheckSize: Cardinal;
begin
  Pak.Read(Adler32, 4);
  Pak.Read(CheckSize, 4);
  Result:=Adler32=StreamAdler32(Pak, CheckSize);
  if not Result
    then WriteLn('Error: file is corrupted');
  Pak.Seek(-CheckSize, soFromCurrent);
end;

procedure ExtractFileNRV(Pak, OFile: TStream);
var
  Size, BlocksCount, IBufSize, OBufSize: Cardinal;
  Blocks: array of Cardinal;
  IBuf, OBuf: Pointer;
  i, Res: Integer;
begin
  IBuf:=nil;
  OBuf:=nil;
  if not CheckAdler32(Pak) then Exit;
  Pak.Read(Size, 4);
  BlocksCount:=Ceil(Size/CBufSize);
  SetLength(Blocks, BlocksCount);
  Pak.Read(Blocks[0], BlocksCount*4);
  try
    GetMem(IBuf, UCLOutputBlockSize(CBufSize));
    GetMem(OBuf, CBufSize);
    for i:=0 to BlocksCount-1 do
    begin
      IBufSize:=Blocks[i];
      if Pak.Read(IBuf^, IBufSize)<>IBufSize then
      begin
        WriteLn('Error: package reading error');
        Exit;
      end;
      OBufSize:=CBufSize;
      Res:=ucl_nrv2e_decompress_asm_safe_8(IBuf, IBufSize, OBuf, OBufSize, nil);
      if Res<>UCL_E_OK then
      begin
        WriteLn('Error: Buffer decompression error: ', Res);
        Exit;
      end;
      OFile.Write(OBuf^, OBufSize);
    end;
  finally
    FreeMemAndNil(IBuf, UCLOutputBlockSize(CBufSize));
    FreeMemAndNil(OBuf, CBufSize);
  end;
end;

procedure ExtractFileLZMA(Pak, OFile: TStream);
var
  Left, BufSize: Cardinal;
  Buf: Pointer;
  Decompr: TLZMADecompressor;
begin
  if not CheckAdler32(Pak) then Exit;
  Pak.Read(Left, 4);
  Buf:=nil;
  Decompr:=TLZMADecompressor.Create(Pak.Read);
  BufSize:=CBufSize;
  try
    GetMem(Buf, CBufSize);
    while Left>0 do
    begin
      if Left<BufSize then BufSize:=Left;
      try
        Decompr.DecompressInto(Buf^, BufSize);
      except
        WriteLn('Error: '+Exception(ExceptObject).Message);
        Exit;
      end;
      OFile.Write(Buf^, BufSize);
      Dec(Left, BufSize);
    end;
  finally
    FAN(Decompr);
    FreeMemAndNil(Buf, CBufSize);
  end;
end;

procedure ExtractFileStore(Pak, OFile: TStream);
var
  Size: Integer;
begin
  if not CheckAdler32(Pak) then Exit;
  Pak.Seek(-4, soFromCurrent);
  Pak.Read(Size, 4);
  OFile.CopyFrom(Pak, Size);
end;

procedure Extract(Mask, Dir, PakName: string);
var
  List: TStringList;
  i: Integer;
  CurDir, FromDir, S: string;
  Pak: TFileStream;
  M: TMask;

  procedure ExtractFile(const Name, Compression: string; Offset: Cardinal);
  var
    OFile: TFileStream;
  begin
    if LowerCase(Compression)='discard' then
    begin
      WriteLn('Discard: ', CurDir, Name);
      Exit;
    end;
    ForceDirectories(CurDir);
    WriteLn('Extracting file ', CurDir, Name);
    Pak.Seek(Offset, soFromBeginning);
    OFile:=TFileStream.Create(CurDir+Name, fmCreate);
    try
      if LowerCase(Compression)='nrv' then
      begin
        ExtractFileNRV(Pak, OFile);
        Exit;
      end;
      if LowerCase(Compression)='lzma' then
      begin
        ExtractFileLZMA(Pak, OFile);
        Exit;
      end;
      if LowerCase(Compression)='store' then
      begin
        ExtractFileStore(Pak, OFile);
        Exit;
      end;
    finally
      FAN(OFile);
    end;
  end;

begin
  WriteLn('Extracting package ', PakName);
  WriteLn('To dir: ', Dir);
  WriteLn('Mask: ', Mask);
  CurDir:=AddTrailingBackslash(Dir);
  List:=TStringList.Create;
  if not LoadPak(PakName, List) then Exit;
  FromDir:=CurDir;
  if Pos('/', Mask)>0 then
  begin
    FromDir:=Copy(Mask, 1, LastDelimiter('/', Mask));
    for i:=1 to Length(FromDir) do
      if FromDir[i]='/' then FromDir[i]:='\';
    if FromDir[1]='/' then FromDir:=Copy(FromDir, 2, MaxInt);
    FromDir:=AddTrailingBackslash(CurDir+FromDir);
    Mask:=Copy(Mask, LastDelimiter('/', Mask)+1, MaxInt);
  end;
  FromDir:=LowerCase(FromDir);
  M:=TMask.Create(Mask);
  Pak:=TFileStream.Create(PakName, fmOpenRead);
  try
    for i:=0 to List.Count-1 do
    begin
      S:=List[i];
      if (S[1]='<') and (S[Length(S)]='>') then
      begin
        S:=Copy(S, 2, Length(S)-2);
        if S='|'
          then CurDir:=Copy(CurDir, 1, LastDelimiter('\', ExcludeTrailingBackslash(CurDir)))
          else CurDir:=CurDir+S+'\';
        Continue;
      end;
      try
        if LowerCase(Copy(CurDir, 1, Length(FromDir)))<>FromDir then Continue;
        if M.Matches(PrjFileName(S, 0))
          then ExtractFile(PrjFileName(S, 0), PrjCompression(S, 0), StrToCar(PrjComprLevel(S, 0)));
      except
        Continue;
      end;
    end;
    WriteLn('Package successfully extracted');
  finally
    FAN(Pak);
    FAN(M);
    FAN(List);
  end;
end;

procedure GetAllFiles(const Mask, Compr: string; List: TStringList);
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
  List.Add('<'+GetLastDirectory(Directory)+'>');
  if FindFirst(Mask, faAnyFile and not faDirectory, Search) = 0 then
  begin
    repeat
      List.Add(Search.Name+'|'+Compr);
    until FindNext(Search) <> 0;
  end;
  if FindFirst(Directory + '*.*', faDirectory+faHidden, Search) = 0 then
  begin
    repeat
      if ((Search.Attr and faDirectory) = faDirectory) and (Search.Name<>'.') and (Search.Name<>'..') then
        GetAllFiles(Directory + Search.Name + '\' + ExtractFileName(Mask), Compr, List);
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
  List.Add('<|>');
end;

procedure CreatePrj(Dir, Compression: string);
var
  Prj: TStringList;
begin
  Dir:=ExpandFileName(Dir);
  if Trim(Compression)='' then Compression:='NRV/10';
  WriteLn('Create project from ', Dir);
  WriteLn('Compression: ', Compression);
  if not DirectoryExists(Dir) then
  begin
    WriteLn('Error: base directory not exists');
    Exit;
  end;
  Prj:=TStringList.Create;
  try
    Prj.Add(Dir);
    GetAllFiles(AddTrailingBackslash(Dir)+'*.*', Compression, Prj);
    Prj.Delete(1);
    Prj.Delete(Prj.Count-1);
    Prj.SaveToFile(ExcludeTrailingBackslash(Dir)+'.vpp');
    WriteLn('Project successfully created');
  finally
    FAN(Prj);
  end;
end;

procedure RemComments(Prj: TStringList);
var
  i, P: Integer;
begin
  for i:=0 to Prj.Count-1 do
  begin
    P:=Pos('//', Prj[i]);
    if P>0
      then Prj[i]:=Trim(Copy(Prj[i], 1, P-1))
      else Prj[i]:=Trim(Prj[i]);
  end;
end;

procedure CheckProject(Prj: TStringList);
var
  CurDir, DirName, FileName, Compression, ComprLevel: string;
  i, DirLevel, L: Integer;
begin
  DirLevel:=0;
  i:=0;
  while (Prj[i]='') and (i<Prj.Count) do Inc(i);
  CurDir:=AddTrailingBackslash(ExpandFileName(Prj[i]));
  if not DirectoryExists(CurDir) then raise Exception.CreateFmt('%d: base directory not found', [i+1]);
  while i<Prj.Count-1 do
  begin
    Inc(i);
    if Prj[i]='' then Continue;
    if Prj[i][1]='<' then
    begin
      DirName:=PrjDirName(Prj[i], i+1);
      if DirName='|' then
      begin
        Dec(DirLevel);
        CurDir:=ExtractFilePath(ExcludeTrailingBackslash(CurDir));
      end
      else begin
        Inc(DirLevel);
        CurDir:=AddTrailingBackslash(CurDir+DirName);
        if not DirectoryExists(CurDir) then raise Exception.CreateFmt('%d: directory %s not found', [i+1, CurDir]);
      end;
    end
    else begin
      FileName:=PrjFileName(Prj[i], i+1);
      Compression:=PrjCompression(Prj[i], i+1);
      ComprLevel:=PrjComprLevel(Prj[i], i+1);
      if not FileExists(CurDir+FileName) and (Compression<>'discard')
        then raise Exception.CreateFmt('%d: file %s not found', [i+1, CurDir+FileName]);
      if (Compression<>'nrv') and (Compression<>'lzma') and (Compression<>'store') and (Compression<>'discard')
        then raise Exception.CreateFmt('%d: invalid compression (%s)', [i+1, Compression]);
      if ((Compression='nrv') and ((StrToInt(ComprLevel)<1) or (StrToInt(ComprLevel)>10))) or
         ((Compression='lzma') and not LZMAGetLevel(ComprLevel, L))
         then raise Exception.CreateFmt('%d: invalid compression level (%s/%s)', [i+1, Compression, ComprLevel]);
    end;
  end;
  if DirLevel<>0 then raise Exception.CreateFmt('directories entries closing disbalance (%d)', [DirLevel]);
end;

var
  DirStack: array of record
    Offset, EntriesCount: Cardinal;
  end; 

procedure PushDir(Offset: Cardinal);
begin
  SetLength(DirStack, Length(DirStack)+1);
  DirStack[High(DirStack)].Offset:=Offset;
  DirStack[High(DirStack)].EntriesCount:=0;
end;

procedure PopDir(FT: TMemoryStream);
begin
  FT.Seek(DirStack[High(DirStack)].Offset, soFromBeginning);
  FT.Write(DirStack[High(DirStack)].EntriesCount, 4);
  FT.Seek(0, soFromEnd);
  SetLength(DirStack, Length(DirStack)-1);
end;

procedure IncDirEntries;
begin
  if High(DirStack)=-1 then Exit;
  Inc(DirStack[High(DirStack)].EntriesCount);
end;

procedure PackFileNRV(TempFile: TFileStream; const FileName, Compr: string);
type
  THdr=packed record
    Adler32, Adler32CheckSize: Cardinal;
    Size: Integer;
  end;
var
  i, Level, StartPos, Res: Integer;
  Hdr: THdr;
  Index: array of Cardinal;
  InFile: TFileStream;
  InBuf, OutBuf: Pointer;
  InBufSize, OutBufSize, IndexLen: Cardinal;
begin
  InBuf:=nil;
  OutBuf:=nil;
  InFile:=nil;
  Level:=StrToInt(Compr);
  StartPos:=TempFile.Position;
  InFile:=TFileStream.Create(FileName, fmOpenRead);
  try
    Hdr.Size:=InFile.Size;
    IndexLen:=Ceil(Hdr.Size/CBufSize);
    SetLength(Index, IndexLen);
    TempFile.Write(Hdr, SizeOf(Hdr));
    TempFile.Write(Index[0], IndexLen*4);
    GetMem(InBuf, CBufSize);
    GetMem(OutBuf, UCLOutputBlockSize(CBufSize));
    for i:=0 to IndexLen-1 do
    begin
      InBufSize:=InFile.Read(InBuf^, CBufSize);
      OutBufSize:=UCLOutputBlockSize(InBufSize);
      Res:=ucl_nrv2e_99_compress(InBuf, InBufSize, OutBuf, OutBufSize, nil, Level, nil, nil);
      if Res<>UCL_E_OK then raise Exception.CreateFmt('NRV2E: cannot compress block (%d)', [Res]);
      Index[i]:=OutBufSize;
      TempFile.Write(OutBuf^, OutBufSize);
      FlushFileBuffers(TempFile.Handle);
    end;
    Hdr.Adler32CheckSize:=TempFile.Position-StartPos-8;
    TempFile.Seek(StartPos+SizeOf(Hdr), soFromBeginning);
    TempFile.Write(Index[0], IndexLen*4);
    TempFile.Seek(StartPos+8, soFromBeginning);
    Hdr.Adler32:=StreamAdler32(TempFile, Hdr.Adler32CheckSize);
    TempFile.Seek(StartPos, soFromBeginning);
    TempFile.Write(Hdr, SizeOf(Hdr));
    TempFile.Seek(0, soFromEnd);
  finally
    FAN(InFile);
    if InBuf<>nil then FreeMem(InBuf, CBufSize);
    if OutBuf<>nil then FreeMem(OutBuf, UCLOutputBlockSize(CBufSize));
  end;
end;

procedure PackFileLZMA(TempFile: TFileStream; const FileName, Compr: string);
type
  THdr=packed record
    Adler32, Adler32CheckSize: Cardinal;
    Size: Integer;
  end;
var
  Level, StartPos: Integer;
  Compressor: TLZMACompressor;
  InFile: TFileStream;
  Hdr: THdr;
  Buf: Pointer;
  BufSize, Left: Cardinal;
begin
  Buf:=nil;
  Compressor:=nil;
  InFile:=nil;
  LZMAGetLevel(Compr, Level);
  StartPos:=TempFile.Position;
  InFile:=TFileStream.Create(FileName, fmOpenRead);
  try
    Hdr.Size:=InFile.Size;
    TempFile.Write(Hdr, SizeOf(Hdr));
    Compressor:=TLZMACompressor.Create(TempFile.WriteBuffer, nil, Level);
    GetMem(Buf, CBufSize);
    BufSize:=CBufSize;
    Left:=Hdr.Size;
    while Left>0 do
    begin
      if BufSize>Left then BufSize:=Left;
      InFile.Read(Buf^, BufSize);
      Compressor.Compress(Buf^, BufSize);
      FlushFileBuffers(TempFile.Handle);
      Dec(Left, BufSize);
    end;
    Compressor.Finish;
    FAN(Compressor);
    Hdr.Adler32CheckSize:=TempFile.Position-StartPos-8;
    TempFile.Seek(StartPos+8, soFromBeginning);
    Hdr.Adler32:=StreamAdler32(TempFile, Hdr.Adler32CheckSize);
    TempFile.Seek(StartPos, soFromBeginning);
    TempFile.Write(Hdr, SizeOf(Hdr));
    TempFile.Seek(0, soFromEnd);
  finally
    if Buf<>nil then FreeMem(Buf, CBufSize);
    FAN(Compressor);
    FAN(InFile);
  end;
end;

procedure PackFileStore(TempFile: TFileStream; const FileName, Compr: string);
type
  THdr=packed record
    Adler32: Cardinal;
    Size: Integer;
  end;
var
  Hdr: THdr;
  InFile: TFileStream;
begin
  InFile:=TFileStream.Create(FileName, fmOpenRead);
  try
    Hdr.Size:=InFile.Size;
    Hdr.Adler32:=StreamAdler32(InFile, Hdr.Size);
    InFile.Seek(0, soFromBeginning);
    TempFile.Write(Hdr, SizeOf(Hdr));
    TempFile.CopyFrom(InFile, Hdr.Size);
  finally
    FAN(InFile);
  end;
end;

procedure PackFile(TempFile: TFileStream; var Entry: TPakFTEntry; var Name: string; const CurDir, PrjStr: string);
var
  S: string;
begin
  Name:=PrjFileName(PrjStr, 0);
  Entry.NameSize:=Length(Name);
  Entry.Offset:=TempFile.Position;
  S:=PrjCompression(PrjStr, 0);
  if S='nrv'
    then Entry.EntryType:=FTE_NRV;
  if S='lzma'
    then Entry.EntryType:=FTE_LZMA;
  if S='store'
    then Entry.EntryType:=FTE_STORE;
  if S='discard'
    then Entry.EntryType:=FTE_DISCARD;
  S:=PrjComprLevel(PrjStr, 0);
  WriteLn('Compressing file ', CurDir, Name);
  case Entry.EntryType of
    FTE_NRV: PackFileNRV(TempFile, CurDir+Name, S);
    FTE_LZMA: PackFileLZMA(TempFile, CurDir+Name, S);
    FTE_STORE: PackFileStore(TempFile, CurDir+Name, S);
    FTE_DISCARD: Entry.Offset:=0;
  end;
end;

procedure BuildProject(Prj: TStringList; const PakName: string);
var
  OutFile, TempFile: TFileStream;
  TempFileName, CurDir, Name: string;
  FT: TMemoryStream;
  Hdr: TPakHdr;
  Entry: TPakFTEntry;
  i: Integer;
  FTBuf: Pointer;
  FTBufSize: Cardinal;
  LZMALib: HModule;
begin
  TempFileName:='';
  FTBuf:=nil;
  OutFile:=nil;
  TempFile:=nil;
  FT:=nil;
  WriteLn('Building package...');
  try
    LZMALib:=LoadLibrary('lzma.dll');
    if LZMALib=0 then raise Exception.Create('Cannot load LZMA library');
    if not LZMAInitCompressFunctions(LZMALib) then raise Exception.Create('Cannot initialize LZMA library');
    FT:=TMemoryStream.Create;
    TempFileName:=UniTempFile;
    TempFile:=TFileStream.Create(TempFileName, fmCreate);
    CurDir:=AddTrailingBackslash(Prj[0]);
    Hdr.ID:=PakID;
    Hdr.EntriesCount:=0;
    for i:=1 to Prj.Count-1 do
    begin
      if Prj[i][1]='<' then
      begin
        Name:=PrjDirName(Prj[i], 0);
        if Name='|' then
        begin
          CurDir:=ExtractFilePath(ExcludeTrailingBackslash(CurDir));
          PopDir(FT);
        end
        else begin
          Inc(Hdr.EntriesCount);
          IncDirEntries;
          CurDir:=AddTrailingBackslash(CurDir+Name);
          PushDir(FT.Position+2);
          Entry.NameSize:=Length(Name);
          Entry.EntryType:=FTE_DIR;
          FT.Write(Entry, SizeOf(Entry));
          FT.Write(Name[1], Entry.NameSize);
        end;
      end
      else begin
        Inc(Hdr.EntriesCount);
        IncDirEntries;
        PackFile(TempFile, Entry, Name, CurDir, Prj[i]);
        FT.Write(Entry, SizeOf(Entry));
        FT.Write(Name[1], Entry.NameSize);
      end;
    end;
    Hdr.FTSize:=FT.Size;
    FTBufSize:=UCLOutputBlockSize(FT.Size);
    GetMem(FTBuf, FTBufSize);
    i:=ucl_nrv2e_99_compress(FT.Memory, FT.Size, FTBuf, FTBufSize, nil, 10, nil, nil);
    if i<>UCL_E_OK then raise Exception.CreateFMT('cannot compress FileTable (NRV error %d)', [i]);
    Hdr.FTPackedSize:=FTBufSize;
    Hdr.Adler32:=NextAdler32(1, @Hdr.EntriesCount, 12);
    Hdr.Adler32:=NextAdler32(Hdr.Adler32, FTBuf, FTBufSize);
    if FileExists(PakName) then DeleteFile(PakName);
    WriteLn('Writing output file...');
    OutFile:=TFileStream.Create(PakName, fmCreate);
    OutFile.Write(HDr, SizeOf(Hdr));
    OutFile.Write(FTBuf^, FTBufSize);
    OutFile.CopyFrom(TempFile, 0);
  finally
    if LZMALib<>0 then FreeLibrary(LZMALib);
    if FTBuf<>nil then FreeMem(FTBuf, FTBufSize);
    FAN(OutFile);
    FAN(TempFile);
    if FileExists(TempFileName) then DeleteFile(TempFileName);
    FAN(FT);
  end;
end;

procedure CreatePak(PrjName: string);
var
  Prj: TStringList;
begin
  Prj:=nil;
  PrjName:=ExpandFileName(PrjName);
  WriteLn('Create package: ', PrjName);
  try
    if not FileExists(PrjName) then raise Exception.Create('Project file not found');
    Prj:=TStringList.Create;
    Prj.LoadFromFile(PrjName);
    RemComments(Prj);
    CheckProject(Prj);
    RemoveVoidStrings(Prj);
    BuildProject(Prj, ChangeFileExt(PrjName, '.vpk'));
    FAN(Prj);
    WriteLn('Package successfully created');
  except
    WriteLn('Error: '+Exception(ExceptObject).Message);
    FAN(Prj);
    Exit;
  end;
end;

type
  TMode=(mBuildPak, mCreatePrj, mListPak, mExtractPak);

var
  i: Integer;
  Mode: TMode;
  S, Compr, Dir, Mask, Name: string;
  W: Boolean;

begin
  Mode:=mBuildPak;
  W:=false;
  Compr:='NRV/10';
  Mask:='*';
  Dir:=GetCurrentDir;
  About;
  if ParamCount<1 then
  begin
    Usage;
    Exit;
  end;
  for i:=1 to ParamCount do
  begin
    S:=ParamStr(i);
    if S[1]='/' then
    begin
      if Length(S)>1 then
        case UpCase(S[2]) of
          'A': begin
                 Write('Enter compression mode:');
                 ReadLn(Compr);
               end;
          'C': Mode:=mCreatePrj;
          'D': Dir:=Copy(S, 3, MaxInt);
          'L': Mode:=mListPak;
          'M': Mask:=Copy(S, 3, MaxInt);
          'P': Compr:=Copy(S, 3, MaxInt);
          'W': W:=true;
          'X': Mode:=mExtractPak;
        end;
    end
      else if FileExists(S) or DirectoryExists(S)
        then Name:=S;
  end;
  case Mode of
    mBuildPak: CreatePak(Name);
    mCreatePrj: CreatePrj(Name, Compr);
    mListPak: ListContents(Name);
    mExtractPak: Extract(Mask, Dir, Name);
  end;
  if W then
  begin
    Write('Press enter');
    ReadLn;
  end;
end.
