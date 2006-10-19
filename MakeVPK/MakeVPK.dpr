library MakeVPK;

uses
  SysSfIni, Windows, AvL, avlUtils, UCLAPI, avlAdler32;

{$I PakTypes.inc}

type
  TFTE=record
    Name, NameLo: string;
    PakEntry: TPakFTEntry;
    Next, Sub: Word;
  end;

const
  Sep='/';

var
  VPKName: string;
  TempFile: TFileStream;
  FT: array of TFTE;

function VPKClose: Boolean; stdcall; forward;

function VPKNew(Name: PChar): Boolean; stdcall;
begin
  Result:=false;
  VPKClose;
  VPKName:=ExpandFileName(Name);
  if VPKName='' then Exit;
  if FileExists(VPKName) then Exit;
  if FileExists(VPKName+'.tmp') then DeleteFile(VPKName+'.tmp');
  TempFile:=TFileStream.Create(VPKName+'.tmp', fmCreate);
  SetLength(FT, MaxWord);
  ZeroMemory(@FT[0], SizeOf(TFTE)*MaxWord);
  Result:=true;
end;

function NewEntry: Word;
begin
  for Result:=1 to MaxWord do
    if FT[Result].Name='' then Exit;
  raise Exception.Create('File table overload');
end;

function FindEntry(Parent: Word; Name: string; Dir: Boolean): Word;
begin
  Name:=LowerCase(Name);
  Result:=FT[Parent].Sub;
  while Result>0 do
    if (FT[Result].NameLo=Name) and not(Dir xor (FT[Result].PakEntry.EntryType=FTE_DIR))
      then Exit
      else Result:=FT[Result].Next;
end;

function AddEntry(Parent: Word): Word;
begin
  Result:=FT[Parent].Sub;
  if Result=0 then
  begin
    Result:=NewEntry;
    FT[Parent].Sub:=Result;
  end
  else begin
    while FT[Result].Next>0 do Result:=FT[Result].Next;
    FT[Result].Next:=NewEntry;
    Result:=FT[Result].Next;
  end;
  Inc(FT[Parent].PakEntry.EntriesCount);
end;

function AddDir(Parent: Word; const Name: string): Word;
begin
  Result:=FindEntry(Parent, Name, true);
  if Result=0 then
  begin
    Result:=AddEntry(Parent);
    FT[Result].Name:=Name;
    FT[Result].NameLo:=LowerCase(Name);
    FT[Result].PakEntry.NameSize:=Length(Name);
    FT[Result].PakEntry.EntryType:=FTE_DIR;
  end;
end;

function AddFile(Parent: Word; const Name: string; FileType: Byte): Word;
begin
  Result:=FindEntry(Parent, Name, false);
  if Result=0 then
  begin
    Result:=AddEntry(Parent);
    FT[Result].Name:=Name;
    FT[Result].NameLo:=LowerCase(Name);
    FT[Result].PakEntry.NameSize:=Length(Name);
    FT[Result].PakEntry.EntryType:=FileType;
  end
    else raise Exception.Create('File '+Name+' already exists');
end;

function VPKAddFile(Name: PChar; FileType: Byte; var FileData; FileSize: Cardinal): Boolean; stdcall;
var
  FileName, CurName: string;
  CurDir, Entry: Word;
begin
  Result:=false;
  if (TempFile=nil) or (Length(FT)=0) then Exit;
  FileName:=Name;
  if (FileName='') or ((FileName[1]='/') and (Length(FileName)<2)) then Exit;
  if FileName[1]='/' then Delete(FileName, 1, 1);
  if not Assigned(@FileData) then Exit;
  if FileSize=0 then Exit;
  if (FileType<>FTE_NRV) and (FileType<>FTE_LZMA) and (FileType<>FTE_STORE) then Exit;
  CurName:=Tok(Sep, FileName);
  CurDir:=0;
  while FileName<>'' do
  begin
    CurDir:=AddDir(CurDir, CurName);
    CurName:=Tok(Sep, FileName);
  end;
  Entry:=AddFile(CurDir, CurName, FileType);
  FT[Entry].PakEntry.Offset:=TempFile.Position;
  TempFile.Write(FileData, FileSize);
  Result:=true;
end;

function VPKClose: Boolean; stdcall;
var
  OutFile: TFileStream;
  TempFT: TMemoryStream;
  Buf: Pointer;
  Hdr: TPakHdr;
  CurEntry: Word;

  procedure WriteEntry(Entry: Word);
  var
    SubEntry: Word;
  begin
    TempFT.Write(FT[Entry].PakEntry, SizeOf(TPakFTEntry));
    TempFT.Write(FT[Entry].Name[1], FT[Entry].PakEntry.NameSize);
    Inc(Hdr.EntriesCount);
    SubEntry:=FT[Entry].Sub;
    while SubEntry<>0 do
    begin
      WriteEntry(SubEntry);
      SubEntry:=FT[SubEntry].Next;
    end;
  end;

begin
  Result:=false;
  OutFile:=nil;
  TempFT:=nil;
  Buf:=nil;
  if (TempFile=nil) or (Length(FT)=0) then Exit;
  try
    OutFile:=TFileStream.Create(VPKName, fmCreate);
    Hdr.ID:=PakID;
    Hdr.EntriesCount:=0;
    TempFT:=TMemoryStream.Create;
    CurEntry:=FT[0].Sub;
    repeat
      WriteEntry(CurEntry);
      CurEntry:=FT[CurEntry].Next;
    until CurEntry=0;
    Hdr.FTSize:=TempFT.Size;
    Hdr.FTPackedSize:=UCLOutputBlockSize(Hdr.FTSize);
    GetMem(Buf, Hdr.FTPackedSize);
    if ucl_nrv2e_99_compress(TempFT.Memory, TempFT.Size, Buf, Hdr.FTPackedSize, nil, 10, nil, nil)<>UCL_E_OK
      then raise Exception.Create('FileTable compressing failed');
    OutFile.Write(Hdr, SizeOf(Hdr));
    OutFile.Write(Buf^, Hdr.FTPackedSize);
    OutFile.Seek(8, soFromBeginning);
    Hdr.Adler32:=StreamAdler32(OutFile, OutFile.Size-8);
    OutFile.Seek(0, soFromBeginning);
    OutFile.Write(Hdr, SizeOf(Hdr));
    OutFile.Seek(0, soFromEnd);
    OutFile.CopyFrom(TempFile, 0);
    Result:=true;
  finally
    Finalize(FT);
    FAN(TempFile);
    DeleteFile(VPKName+'.tmp');
    FAN(OutFile);
    FAN(TempFT);
    FreeMemAndNil(Buf, UCLOutputBlockSize(Hdr.FTSize));
  end;
end;

exports
  VPKNew,
  VPKAddFile,
  VPKClose;

begin
  FT:=nil;
  TempFile:=nil;
end.