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
  Sz: Cardinal;
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
    Sz:=InFile.Size;
    OutFile.Write(Sz, 4);
    OutFile.CopyFrom(InFile, 0);
    FAN(InFile);
    FilesList:=TStringList.Create;
    FilesList.LoadFromFile(FormData+'Files.txt');
    RemoveVoidStrings(FilesList);
    Temp:=TMemoryStream.Create;
    Temp.Clear;
    Sz:=FilesList.Count;
    OutFile.Write(Sz, 4);
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
      OutFile.Write(DTI, SizeOf(DTI));
      Temp.CopyFrom(InFile, 0);
    end;
    OutFile.CopyFrom(Temp, 0);
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
