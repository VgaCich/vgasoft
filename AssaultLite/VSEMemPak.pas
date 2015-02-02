unit VSEMemPak;

interface

uses
  AvL, avlUtils;

function GetFile(FileName: string): TCustomMemoryStream; //Get file as stream
function GetFileText(const FileName: string): TStringList; //Get text file as TStringList

implementation

{$I VSEMemPakTypes.inc}
type
  TMemStream=class(TCustomMemoryStream)
  public
    constructor Create(Mem: Pointer; Size: Integer);
  end;

const
{$I MemPak.inc}

constructor TMemStream.Create(Mem: Pointer; Size: Integer);
begin
  inherited Create;
  SetPointer(Mem, Size);
end;

function GetFile(FileName: string): TCustomMemoryStream;
var
  Hdr: TFileHeader;
  Name: string;
  Pak: TMemStream;
begin
  Result:=nil;
  if FileExists(ExePath+'Data\'+FileName) then
  begin
    Result:=TMemoryStream.Create;
    TMemoryStream(Result).LoadFromFile(ExePath+'Data\'+FileName);
  end
  else begin
    FileName:=UpperCase(FileName);
    Pak:=TMemStream.Create(@MemPakData, SizeOf(MemPakData));
    try
      while Pak.Position<Pak.Size do
      begin
        Pak.Read(Hdr, SizeOf(Hdr));
        SetLength(Name, Hdr.NameLen);
        Pak.Read(Name[1], Hdr.NameLen);
        if Name=FileName then
        begin
          Result:=TMemStream.Create(IncPtr(Pak.Memory, Pak.Position), Hdr.FileSize);
          Exit;
        end
          else Pak.Seek(Hdr.FileSize, soFromCurrent);
      end;
    finally
      FAN(Pak);
    end;
  end;
end;

function GetFileText(const FileName: string): TStringList;
var
  F: TStream;
begin
  Result:=nil;
  F:=GetFile(FileName);
  if Assigned(F) then
  try
    Result:=TStringList.Create;
    Result.LoadFromStream(F);
  finally
    FAN(F);
  end;
end;

end.
