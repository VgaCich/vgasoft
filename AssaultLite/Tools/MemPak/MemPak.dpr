program MemPak;

{$APPTYPE CONSOLE}

uses
  Windows, AvL, avlUtils;

{$I VSEMemPakTypes.inc}

var
  FilesList: TStringList;
  Pak: TMemoryStream;
  i: Integer;
  S: string;

procedure FillList(const Mask: string);
var
  SR: TSearchRec;
  Dir: string;
begin
  Dir:=ExtractFilePath(Mask);
  if FindFirst(Mask, 0, SR)=0 then
    repeat
      FilesList.Add(ExpandFileName(Dir+SR.Name));
    until FindNext(SR)<>0;
  FindClose(SR);
end;

procedure Add(FileName: string);
var
  IFile: TFileStream;
  Hdr: TFileHeader;
begin
  IFile:=TFileStream.Create(FileName, fmOpenRead);
  try
    FileName:=ExtractFileName(FileName);
    WriteLn('Adding: ', FileName, ': ', IFile.Size);
    Hdr.NameLen:=Length(FileName);
    Hdr.FileSize:=IFile.Size;
    Pak.Write(Hdr, SizeOf(Hdr));
    Pak.Write(UpperCase(FileName)[1], Hdr.NameLen);
    Pak.CopyFrom(IFile, Hdr.FileSize);
  finally
    FAN(IFile);
  end;
end;

begin
  WriteLn('MemPak 1.0');
  WriteLn('(c)VgaSoft, 2004-2007');
  WriteLn;
  if ParamCount=0 then
  begin
    WriteLn('Usage:');
    WriteLn('  MemPak <in file> [in file] ...');
    Exit;
  end;
  FilesList:=TStringList.Create;
  try
    for i:=1 to ParamCount do
      FillList(ParamStr(i));
    Pak:=TMemoryStream.Create;
    try
      for i:=0 to FilesList.Count-1 do
        Add(FilesList[i]);
      FilesList.Clear;
      FilesList.Add('MemPakData: array[0..'+IntToStr(Pak.Size-1)+'] of Byte = ');
      S:='(';
      for i:=0 to Pak.Size-1 do
      begin
        S:=S+'$'+IntToHex(PByteArray(Pak.Memory)[i], 2)+', ';
        if i mod 16 = 15 then
        begin
          FilesList.Add(S);
          S:=' ';
        end;
      end;
      if S<>' ' then FilesList.Add(S);
      FilesList[FilesList.Count-1]:=Copy(FilesList[FilesList.Count-1], 1, Length(FilesList[FilesList.Count-1])-2)+');';
      FilesList.SaveToFile(ExePath+'MemPak.inc');
    finally
      FAN(Pak);
    end;
  finally
    FAN(FilesList);
  end;
end.
