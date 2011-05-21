program FormDataComp;

{$APPTYPE CONSOLE}

uses Windows, AvL, avlUtils;

type
  TDataTableItem=packed record
    Offset, Size: Cardinal;
  end;

procedure CompileForm(FormData: string);
var
  InFile, OutFile: TFileStream;
  Temp: TMemoryStream;
  FilesList: TStringList;
  i: Integer;
  Size: Cardinal;
  DTI: TDataTableItem;
begin
  if not FileExists(FormData+'Form.txt') then
  begin
    WriteLn(FormData+'Form script not found');
    Exit;
  end;
  if not FileExists(FormData+'Files.txt') then
  begin
    WriteLn('Files list not found');
    Exit;
  end;
  try
    OutFile:=TFileStream.Create(ExcludeTrailingBackslash(FormData)+'.fdt', fmOpenWrite or fmCreate);
    InFile:=TFileStream.Create(FormData+'Form.txt', fmOpenRead);
    OutFile.CopyFrom(InFile, 0);
    FAN(InFile);
    FilesList:=TStringList.Create;
    FilesList.LoadFromFile(FormData+'Files.txt');
    RemoveVoidStrings(FilesList);
    Temp:=TMemoryStream.Create;
    Temp.Clear;
    Size:=FilesList.Count;
    Temp.Write(Size, SizeOf(Size));
    Temp.SetSize(Temp.Size+Size*SizeOf(TDataTableItem)+SizeOf(Size));
    Temp.Seek(0, soFromEnd);
    Size:=Temp.Size;
    FilesList.SaveToStream(Temp);
    Temp.Position:=Size-SizeOf(Size);
    Size:=Temp.Size-Size;
    Temp.Write(Size, SizeOf(Size));
    Temp.Seek(0, soFromEnd);
    for i:=0 to FilesList.Count-1 do
    begin
      if not FileExists(FormData+FilesList[i]) then
      begin
        WriteLn('File '+FilesList[i]+' not found');
        Exit;
      end;
      InFile:=TFileStream.Create(FormData+FilesList[i], fmOpenRead);
      DTI.Offset:=Temp.Size;
      DTI.Size:=InFile.Size;
      Temp.Position:=SizeOf(Size)+i*SizeOf(TDataTableItem);
      Temp.Write(DTI, SizeOf(DTI));
      Temp.Seek(0, soFromEnd);
      Temp.CopyFrom(InFile, 0);
    end;
    OutFile.CopyFrom(Temp, 0);
    Size:=Temp.Size;
    OutFile.Write(Size, SizeOf(Size));
    WriteLn('Done');
  finally
    FAN(OutFile);
    FAN(InFile);
    FAN(Temp);
    FAN(FilesList);
  end;
end;

var
  i: Integer;

begin
  if ParamCount<1 then
  begin
    WriteLn('Usage: FormDataComp form_data_folder');
    Exit;
  end;
  for i:=1 to ParamCount do
    if DirectoryExists(ParamStr(i)) then
    begin
      CompileForm(AddTrailingBackslash(ParamStr(i)));
      Exit;
    end;
  WriteLn('Form data folder not found');
end.
