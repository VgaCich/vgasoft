//----------------------------------------------------------------------------//
//                                                                            //
// UPakMan.pas 1.4.1, 12.05.2007; 13:40                                       //
//                                                                            //
// VSE Package Manager 1.4.0                                                  //
//                                                                            //
// Copyright (C) 2004-2007 VgaSoft                                            //
//                                                                            //
//This program is free software; you can redistribute it and/or               //
//modify it under the terms of the GNU General Public License                 //
//as published by the Free Software Foundation; either version 2              //
//of the License, or any later version.                                       //
//                                                                            //
//This program is distributed in the hope that it will be useful,             //
//but WITHOUT ANY WARRANTY; without even the implied warranty of              //
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               //
//GNU General Public License (http://www.gnu.org/copyleft/gpl.html)           //
//for more details.                                                           //
//----------------------------------------------------------------------------//

unit UPakMan;

interface

uses
  Windows, AvL, avlUtils, avlMath, UCLAPI, avlLZMADec, avlAdler32, avlMasks;

type
  TLogCB=procedure(S: PChar); stdcall;
  TPakFileSource=(fsFile, fsPakNRV, fsPakLZMA, fsPakStore);

procedure PakInit(BaseDir: PChar; LogCallback: TLogCB); stdcall;
procedure PakFree; stdcall;
function  PakAddMountPoint(MntPoint, Src: PChar): Boolean; stdcall;
function  PakDeleteMountPoint(MntPoint: PChar): Boolean; stdcall;
function  PakOpenFile(Name: PChar; Flags: Cardinal): Cardinal; stdcall;
function  PakCreateFile(Name: PChar; Flags: Cardinal): Cardinal; stdcall;
procedure PakCloseFile(F: Cardinal); stdcall;
procedure PakDeleteFile(Name: PChar); stdcall;
function  PakFileExists(Name: PChar): Boolean; stdcall;
function  PakFindFiles(Mask: PChar; Recursive: Boolean): PChar; stdcall;

function  PakFileType(F: Cardinal): TPakFileSource; stdcall;
function  PakFileRead(F: Cardinal; var Buffer; Count: Longint): Longint; stdcall;
function  PakFileWrite(F: Cardinal; const Buffer; Count: Longint): Longint; stdcall;
function  PakFileSeek(F: Cardinal; Offset: Longint; Origin: Word): Longint; stdcall;
function  PakFileSize(F: Cardinal): Longint; stdcall;
function  PakFilePosition(F: Cardinal): Longint; stdcall;

const
  InvalidPakFile: Cardinal=$FFFFFFFF;
  //Open/CreateFile:Flags - accepts FileOpen modes if PakFileSource=fsFile
  ofNoCreate=$00010000;
  ofNoCheck=$00020000;

var LogCB: TLogCB;

implementation

{$I PakTypes.inc}

const
  PakExt='.vpk';
  PakMask='*'+PakExt;
  Sep='/';
  CBufSize=1048576;

type
  PFileInfo=^TFileInfo;
  TFileInfo=record
    Source: TPakFileSource;
    Name,
    PakFile: string;
    Offset, Hash: Cardinal;
  end;

  TFileInfoRes=record
    Source: TPakFileSource;
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

  TPakFile=record
    Data: Pointer;
    Source: TPakFileSource;
    Exists: Boolean;
  end;

  PFileData=TFileStream;

  PNRVData=^TNRVData;
  TNRVData=record
    Buffer: Pointer;
    Size, Position, IndexLen: Integer;
    InStream: TFileStream;
    Index: packed array of Cardinal;
  end;

  PLZMAData=^TLZMAData;
  TLZMAData=record
    InStream: TFileStream;
    Decompressor: TLZMADecompressor;
    Size, Position: Integer;
  end;

  PStoreData=^TStoreData;
  TStoreData=record
    InStream: TFileStream;
    Size, FOffset: Integer;
  end;

var
  Index: TDirInfo;
  BaseDir, FindResultBuffer: string;
  OpenedFiles: array of TPakFile;
  MountPoints: array of TMountPoint;

procedure Log(const S: string);
begin
  if Assigned(LogCB) then LogCB(PChar(S));
end;

{ExtractFile* functions}

function PakExtractFileExt(const FileName: string): string;
var
  i: Integer;
begin
  i:=Length(FileName);
  while (i>1) and not (FileName[i] in [Sep, '.']) do Dec(i);
  if (i>1) and (FileName[i]='.')
    then Result:=Copy(FileName, i+1, MaxInt)
    else Result:='';
end;

function PakExtractFileName(const FileName: string): string;
var
  i: Integer;
begin
  i:=Length(FileName);
  while (i>=1) and (FileName[i]<>Sep) do Dec(i);
  Result:=Copy(FileName, i+1, MaxInt);
end;

function PakExtractFilePath(const FileName: string): string;
var
  i: Integer;
begin
  i:=Length(FileName);
  while (i>1) and (FileName[i]<>Sep) do Dec(i);
  if FileName[i]=Sep
    then Result:=Copy(FileName, 1, i)
    else Result:='';
end;

function OFCreate: Cardinal;
var
  i: Cardinal;
begin
  if Length(OpenedFiles)=0 then Exit;
  Result:=InvalidPakFile;
  for i:=0 to Length(OpenedFiles)-1 do
    if not OpenedFiles[i].Exists then
    begin
      Result:=i;
      Break;
    end;
  if Result=InvalidPakFile then
  begin
    Result:=Length(OpenedFiles);
    SetLength(OpenedFiles, 2*Length(OpenedFiles));
  end;
  OpenedFiles[Result].Exists:=true;
end;

function OFDelete(Index: Cardinal): Boolean;
begin
  Result:=false;
  if Index<Length(OpenedFiles) then
  begin
    Result:=OpenedFiles[Index].Exists;
    OpenedFiles[Index].Exists:=false;
  end;
end;

{File access internal functions}

function InitFile(FI: TFileInfo; Flags: Cardinal): PFileData;
begin
  Result:=TFileStream.Create(FI.PakFile+FI.Name, Flags);
end;

procedure NRVUnpackBuffer(Data: PNRVData);
var
  i: Integer;
  BS: Cardinal;
  InBuffer: Pointer;
begin
  with Data^ do
  begin
    i:=Position div CBufSize;
    if Position=Size then Exit;
    if (i>=IndexLen) or (i<0) then raise Exception.Create('Index position out of bounds');
    InStream.Seek(Index[i], soFromBeginning);
    i:=Index[i+1]-Index[i];
    InBuffer:=IncPtr(Buffer, UCLOutputBlockSize(CBufSize)-i);
    if InStream.Read(InBuffer^, i)<>i then raise Exception.Create('Pak file reading error');
    BS:=CBufSize;
    i:=ucl_nrv2e_decompress_asm_safe_8(InBuffer, i, Buffer, BS, nil);
    if i<>UCL_E_OK then raise Exception.CreateFmt('Buffer decompression error: %d', [i]);
  end;
end;

function InitNRV(FI: TFileInfo; Flags: Cardinal): PNRVData;
var
  i: Integer;
  Adler32, CheckSize: Cardinal;
begin
  GetMem(Result, SizeOf(TNRVData));
  ZeroMemory(Result, SizeOf(TNRVData));
  with Result^ do
  begin
    GetMem(Buffer, UCLOutputBlockSize(CBufSize));
    InStream:=TFileStream.Create(FI.PakFile, fmOpenRead or fmShareDenyWrite);
    InStream.Seek(FI.Offset, soFromBeginning);
    InStream.Read(Adler32, 4);
    InStream.Read(CheckSize, 4);
    if Flags and ofNoCheck=0 then
      if Adler32<>StreamAdler32(InStream, CheckSize)
        then raise Exception.Create('Source data is corrupted')
        else InStream.Seek(FI.Offset+8, soFromBeginning);
    InStream.Read(Size, 4);
    IndexLen:=Ceil(Size/CBufSize);
    SetLength(Result^.Index, IndexLen+1);
    InStream.Read(Index[1], IndexLen*4);
    Index[0]:=InStream.Position;
    for i:=1 to IndexLen do
      Index[i]:=Index[i-1]+Index[i];
    Position:=0;
    NRVUnpackBuffer(Result);
  end;
end;

function InitLZMA(FI: TFileInfo; Flags: Cardinal): PLZMAData;
var
  Adler32, CheckSize: Cardinal;
begin
  GetMem(Result, SizeOf(TLZMAData));
  with Result^ do
  begin
    InStream:=TFileStream.Create(FI.PakFile, fmOpenRead or fmShareDenyWrite);
    InStream.Seek(FI.Offset, soFromBeginning);
    InStream.Read(Adler32, 4);
    InStream.Read(CheckSize, 4);
    if Flags and ofNoCheck=0 then
      if Adler32<>StreamAdler32(InStream, CheckSize)
        then raise Exception.Create('Source data is corrupted')
        else InStream.Seek(FI.Offset+8, soFromBeginning);
    InStream.Read(Size, 4);
    Decompressor:=TLZMADecompressor.Create(InStream.Read);
    Position:=0;
  end;
end;

function InitStore(FI: TFileInfo; Flags: Cardinal): PStoreData;
var
  Adler32: Cardinal;
begin
  GetMem(Result, SizeOf(TStoreData));
  with Result^ do
  begin
    InStream:=TFileStream.Create(FI.PakFile, fmOpenRead or fmShareDenyWrite);
    InStream.Seek(FI.Offset, soFromBeginning);
    InStream.Read(Adler32, 4);
    InStream.Read(Size, 4);
    FOffset:=InStream.Position;
    if Flags and ofNoCheck=0 then
      if Adler32<>StreamAdler32(InStream, Size)
        then raise Exception.Create('Source data is corrupted')
        else InStream.Seek(FOffset, soFromBeginning);
  end;
end;

procedure FreeFile(Data: PFileData);
begin
  FAN(Data);
end;

procedure FreeNRV(Data: PNRVData);
begin
  with Data^ do
  begin
    FAN(InStream);
    FreeMem(Buffer, UCLOutputBlockSize(CBufSize));
    Finalize(Index);
  end;
  FreeMem(Data);
end;

procedure FreeLZMA(Data: PLZMAData);
begin
  with Data^ do
  begin
    FAN(Decompressor);
    FAN(InStream);
  end;
  FreeMem(Data);
end;

procedure FreeStore(Data: PStoreData);
begin
  FAN(Data^.InStream);
  FreeMem(Data)
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
      if (SR.Name='.') or (SR.Name='..') or ((SR.Attr and faDirectory=0) and (LowerCase(ExtractFileExt(SR.Name))=PakExt)) then Continue;
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

{PakMan}

procedure LoadPak(const PakName: string);
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
    Pak.Read(Hdr, SizeOf(Hdr));
    if Hdr.ID<>PakID then
    begin
      Log('PakMan: LoadPak('+ExtractFileName(PakName)+') failed: file is not VgaSoft Package');
      Exit;
    end;
    Pak.Seek(8, soFromBeginning);
    if Hdr.Adler32<>StreamAdler32(Pak, SizeOf(Hdr)-8+Hdr.FTPackedSize) then
    begin
      Log('PakMan: LoadPak('+ExtractFileName(PakName)+') failed: package corrupted');
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
        Log('PakMan: LoadPak('+ExtractFileName(PakName)+') failed: FileTable decompression error ('+IntToStr(Res)+')');
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
      ProcessEntry(Index);
      Inc(CurEntry);
    end;
  finally
    Finalize(FileTable);
    FAN(Pak);
  end;
end;

procedure Mount(MountPoint: TMountPoint);
var
  Dir: TDirInfo;
begin
  if Index=nil then Exit;
  Dir:=Index.Dirs[Index.AddDir(MountPoint.MountPoint)];
  Dir.Source:=MountPoint.Source;
end;

function FindFile(Name: string): PFileInfo;
var
  CurTok, NextTok: string;
  CurDir: TDirInfo;
  i: Integer;
begin
  NextTok:=Tok(Sep, Name);
  Result:=nil;
  CurDir:=Index;
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

procedure PakInit(BaseDir: PChar; LogCallback: TLogCB);
var
  SR: TSearchRec;
  i: Integer;
  Paks: TStringList;
begin
  LogCB:=LogCallback;
  PakFree;
  UPakMan.BaseDir:=AddTrailingBackslash(BaseDir);
  Index:=TDirInfo.Create('');
  SetLength(OpenedFiles, 8);
  for i:=0 to  High(MountPoints) do
    if MountPoints[i].Exist then Mount(MountPoints[i]);
  Paks:=TStringList.Create;
  try
    if FindFirst(UPakMan.BaseDir+PakMask, 0, SR)=0 then
      repeat Paks.Add(SR.Name);
      until FindNext(SR)<>0;
    FindClose(SR);
    Paks.Sort;
    for i:=0 to Paks.Count-1 do LoadPak(UPakMan.BaseDir+Paks[i]);
  finally
    FAN(Paks);
  end;
  Index.ReadDir(UPakMan.BaseDir);
end;

procedure PakFree;
var
  i: Integer;
begin
  FAN(Index);
  for i:=0 to Length(OpenedFiles)-1 do
    if OpenedFiles[i].Exists then PakCloseFile(i);
  Finalize(OpenedFiles);
  BaseDir:='';
  FindResultBuffer:='';
end;

function PakAddMountPoint(MntPoint, Src: PChar): Boolean;
var
  i, Idx: Integer;
  MountPoint, Source: string;
begin
  Result:=false;
  MountPoint:=MntPoint;
  Source:=Src;
  if Pos(Sep, MountPoint)>0 then
  begin
    Log('PakMan: AddMountPoint('+MountPoint+', '+Source+') failed: invalid mount point');
    Exit;
  end;
  Idx:=-1;
  MountPoint:=LowerCase(MountPoint);
  for i:=0 to High(MountPoints) do
  begin
    if (Idx=-1) and not MountPoints[i].Exist then Idx:=i;
    if MountPoints[i].Exist and (MountPoints[i].MountPoint=MountPoint) then
    begin
      Log('PakMan: AddMountPoint('+MountPoint+', '+Source+') failed: mount point already exists');
      Exit;
    end;
  end;
  Source:=AddTrailingBackslash(ExpandFileName(Source));
  if not DirectoryExists(Source) then
    if not ForceDirectories(Source) then
    begin
      Log('PakMan: AddMountPoint('+MountPoint+', '+Source+') failed: cannot create source dir');
      Exit;
    end;
  if Idx=-1 then
  begin
    Idx:=Length(MountPoints);
    SetLength(MountPoints, 2*Idx);
  end;
  MountPoints[Idx].Exist:=true;
  MountPoints[Idx].MountPoint:=MountPoint;
  MountPoints[Idx].Source:=Source;
  Result:=true;
end;

function PakDeleteMountPoint(MntPoint: PChar): Boolean;
var
  i: Integer;
  MountPoint: string;
begin
  Result:=false;
  MountPoint:=MntPoint;
  MountPoint:=LowerCase(MountPoint);
  for i:=0 to High(MountPoints) do
    if MountPoints[i].Exist and (MountPoints[i].MountPoint=MountPoint) then
      with MountPoints[i] do
      begin
        Exist:=false;
        MountPoint:='';
        Source:='';
        Result:=true;
        Exit;
      end;
  Log('PakMan: DeleteMountPoint('+MountPoint+') failed: mount point not exist');
end;

function PakOpenFile(Name: PChar; Flags: Cardinal): Cardinal;
var
  FI: PFileInfo;
begin
  Result:=InvalidPakFile;
  if Index=nil then Exit;
  FI:=FindFile(Name);
  if FI<>nil then
  begin
    Result:=OFCreate;
    OpenedFiles[Result].Source:=FI.Source;
    case FI.Source of
      fsFile: OpenedFiles[Result].Data:=InitFile(FI^, Flags);
      fsPakNRV: OpenedFiles[Result].Data:=InitNRV(FI^, Flags);
      fsPakLZMA: OpenedFiles[Result].Data:=InitLZMA(FI^, Flags);
      fsPakStore: OpenedFiles[Result].Data:=InitStore(FI^, Flags);
    end;
  end
    else if Flags and ofNoCreate=0
      then Result:=PakCreateFile(Name, Flags)
      else begin
        Log('PakMan: OpenFile('+Name+') failed: file not exists');
        Exit;
      end;
end;

function PakCreateFile(Name: PChar; Flags: Cardinal): Cardinal;
var
  CurTok, NextTok, DestDir, NameS: string;
  CurDir: TDirInfo;
  i: Integer;
  FI: TFileInfo;
begin
  Result:=InvalidPakFile;
  if Index=nil then Exit;
  NameS:=Name;
  NextTok:=Tok(Sep, NameS);
  CurDir:=Index;
  DestDir:=BaseDir;
  while true do
  begin
    CurTok:=NextTok;
    NextTok:=Tok(Sep, NameS);
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
    Log('PakMan: CreateFile('+Name+') failed: cannot create destination dir');
    Exit;
  end;
  if FileExists(DestDir+CurTok) then DeleteFile(DestDir+CurTok);
  FI.Source:=fsFile;
  FI.PakFile:=DestDir;
  FI.Name:=CurTok;
  Result:=OFCreate;
  OpenedFiles[Result].Source:=fsFile;
  OpenedFiles[Result].Data:=InitFile(FI, fmCreate);
  Index.ReadDir(BaseDir);
end;

procedure PakCloseFile(F: Cardinal);
begin
  if OFDelete(F) then
    case OpenedFiles[F].Source of
      fsFile: FreeFile(OpenedFiles[F].Data);
      fsPakNRV: FreeNRV(OpenedFiles[F].Data);
      fsPakLZMA: FreeLZMA(OpenedFiles[F].Data);
      fsPakStore: FreeStore(OpenedFiles[F].Data);
    end;
end;

procedure PakDeleteFile(Name: PChar);
var
  FI: PFileInfo;
  NameS: string;
begin
  if Index=nil then Exit;
  NameS:=Name;
  FI:=FindFile(NameS);
  if FI=nil then
  begin
    Log('PakMan: DeleteFile('+Name+') failed: file not exists');
    Exit;
  end;
  if FI.Source<>fsFile then
  begin
    Log('PakMan: DeleteFile('+Name+') failed: file read-only');
    Exit;
  end;
  DeleteFile(FI.PakFile+FI.Name);
  Index.ReadDir(BaseDir);
end;

function PakFileExists(Name: PChar): Boolean;
begin
  if Index=nil then Exit;
  Result:=FindFile(Name)<>nil;
end;

function PakFindFiles(Mask: PChar; Recursive: Boolean): PChar;
var
  Dir: TDirInfo;
  Path, S: string;
  i: Integer;
  M: TMask;
  List: TStringList;

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
  if Index=nil then Exit;
  List:=TStringList.Create;
  Path:=PakExtractFilePath(Mask);
  M:=TMask.Create(PakExtractFileName(Mask));
  try
    Dir:=Index;
    S:=Tok(Sep, Path);
    while S<>'' do
    begin
      i:=Dir.FindDir(S);
      if i<0
        then Exit
        else Dir:=Dir.Dirs[i];
      S:=Tok(Sep, Path);
    end;
    Find(Dir, PakExtractFilePath(Mask));
    FindResultBuffer:=List.Text;
    Result:=PChar(FindResultBuffer);
  finally
    FAN(M);
    FAN(List);
  end;
end;

{File access functions}

procedure CheckFile(F: Cardinal);
begin
  if (F>=Length(OpenedFiles)) or not OpenedFiles[F].Exists
    then raise Exception.Create('Invalid file index'); 
end;

function NRVRead(Data: PNRVData; var Buf; Count: Longint): Longint;
var
  BufPos, OBufPos, BufSize, IncSize, OldPos: integer;
begin
  with Data^ do
  begin
    OBufPos:=0;
    OldPos:=Position;
    BufPos:=Position mod CBufSize;
    if Position div CBufSize<IndexLen-1
      then BufSize:=CBufSize
      else BufSize:=Size-CBufSize*(IndexLen-1);
    if Position+Count>Size then Count:=Size-Position;
    while (BufSize-BufPos)<Count do
    begin
      IncSize:=BufSize-BufPos;
      CopyMemory(IncPtr(@Buf, OBufPos), IncPtr(Buffer, BufPos), IncSize);
      Inc(Position, IncSize);
      try
        NRVUnpackBuffer(Data);
        BufPos:=0;
      except
        raise;
        Exit;
      end;
      Inc(OBufPos, IncSize);
      Dec(Count, IncSize);
    end;
    CopyMemory(IncPtr(@Buf, OBufPos), IncPtr(Buffer, BufPos), Count);
    Inc(Position, Count);
    Result:=Position-OldPos;
    NRVUnpackBuffer(Data);
  end;
end;

function LZMARead(Data: PLZMAData; var Buffer; Count: Longint): Longint;
begin
  with Data^ do
  begin
    if Position+Count>Size then Count:=Size-Position;
    Decompressor.DecompressInto(Buffer, Count);
    Inc(Position, Count);
    Result:=Count;
  end;
end;

function StorePosition(Data: PStoreData): Longint;
begin
  with Data^ do
  begin
    Result:=InStream.Position-FOffset;
    if Result<0 then
    begin
      InStream.Seek(FOffset, soFromBeginning);
      Result:=0;
    end;
    if Result>Size then
    begin
      InStream.Seek(FOffset+Size, soFromBeginning);
      Result:=Size;
    end;
  end;
end;

function StoreRead(Data: PStoreData; var Buffer; Count: Longint): Longint;
begin
  with Data^ do
  begin
    if Count>Size-StorePosition(Data) then Count:=Size-StorePosition(Data);
    Result:=InStream.Read(Buffer, Count);
  end;
end;

function NRVSeek(Data: PNRVData; Offset: Longint; Origin: Word): Longint;
var
  OldPos: Integer;
begin
  with Data^ do
  begin
    OldPos:=Position;
    case Origin of
      soFromBeginning: Position:=Offset;
      soFromCurrent: Position:=Position+Offset;
      soFromEnd: Position:=Size+Offset;
    end;
    if (Position<0) or (Position>Size) then
    begin
      Position:=OldPos;
      Result:=Position;
      raise Exception.Create('Seek offset out of bounds');
    end;
    Result:=Position;
    if OldPos div CBufSize<>Position div CBufSize then NRVUnpackBuffer(Data);
  end;
end;

function LZMASeek(Data: PLZMAData; Offset: Longint; Origin: Word): Longint;
var
  OldPos: Integer;
  Buf: Pointer;
begin
  with Data^ do
  begin
    OldPos:=Position;
    case Origin of
      soFromBeginning: Position:=Offset;
      soFromCurrent: Position:=Position+Offset;
      soFromEnd: Position:=Size+Offset;
    end;
    if (Position<OldPos) or (Position>Size) then
    begin
      Result:=Position;
      Position:=OldPos;
      raise Exception.Create('Seek offset out of bounds');
    end;
    Buf:=nil;
    GetMem(Buf, Position-OldPos);
    try
      Decompressor.DecompressInto(Buf^, Position-OldPos);
    finally
      if Buf<>nil then FreeMem(Buf);
    end;
    Result:=Position;
  end;
end;

function StoreSeek(Data: PStoreData; Offset: Longint; Origin: Word): Longint;
begin
  with Data^ do
  begin
    case Origin of
      soFromBeginning: InStream.Seek(FOffset+Offset, soFromBeginning);
      soFromCurrent: InStream.Seek(Offset, soFromCurrent);
      soFromEnd: InStream.Seek(Size+FOffset+Offset, soFromBeginning);
    end;
    Result:=StorePosition(Data);
  end;
end;

function  PakFileType(F: Cardinal): TPakFileSource;
begin
  CheckFile(F);
  Result:=OpenedFiles[F].Source;
end;

function  PakFileRead(F: Cardinal; var Buffer; Count: Longint): Longint;
begin
  CheckFile(F);
  case OpenedFiles[F].Source of
    fsFile: Result:=PFileData(OpenedFiles[F].Data).Read(Buffer, Count);
    fsPakNRV: Result:=NRVRead(OpenedFiles[F].Data, Buffer, Count);
    fsPakLZMA: Result:=LZMARead(OpenedFiles[F].Data, Buffer, Count);
    fsPakStore: Result:=StoreRead(OpenedFiles[F].Data, Buffer, Count);
  end;
end;

function  PakFileWrite(F: Cardinal; const Buffer; Count: Longint): Longint;
begin
  CheckFile(F);
  if OpenedFiles[F].Source=fsFile
    then Result:=PFileData(OpenedFiles[F].Data).Write(Buffer, Count)
    else raise Exception.Create('File is read only');
end;

function  PakFileSeek(F: Cardinal; Offset: Longint; Origin: Word): Longint;
begin
  CheckFile(F);
  case OpenedFiles[F].Source of
    fsFile: Result:=PFileData(OpenedFiles[F].Data).Seek(Offset, Origin);
    fsPakNRV: Result:=NRVSeek(OpenedFiles[F].Data, Offset, Origin);
    fsPakLZMA: Result:=LZMASeek(OpenedFiles[F].Data, Offset, Origin);
    fsPakStore: Result:=StoreSeek(OpenedFiles[F].Data, Offset, Origin);
  end;
end;

function  PakFileSize(F: Cardinal): Longint;
begin
  CheckFile(F);
  case OpenedFiles[F].Source of
    fsFile: Result:=PFileData(OpenedFiles[F].Data).Size;
    fsPakNRV: Result:=PNRVData(OpenedFiles[F].Data).Size;
    fsPakLZMA: Result:=PLZMAData(OpenedFiles[F].Data).Size;
    fsPakStore: Result:=PStoreData(OpenedFiles[F].Data).Size;
  end;
end;

function  PakFilePosition(F: Cardinal): Longint;
begin
  CheckFile(F);
  case OpenedFiles[F].Source of
    fsFile: Result:=PFileData(OpenedFiles[F].Data).Position;
    fsPakNRV: Result:=PNRVData(OpenedFiles[F].Data).Position;
    fsPakLZMA: Result:=PLZMAData(OpenedFiles[F].Data).Position;
    fsPakStore: Result:=StorePosition(OpenedFiles[F].Data);
  end;
end;

initialization

  SetLength(MountPoints, 8);

finalization

  PakFree;
  Finalize(MountPoints);

end.
