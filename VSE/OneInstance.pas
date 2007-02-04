unit OneInstance;

interface

uses
  Windows, Avl;

function IsRunning(const ID: string): Boolean;

implementation

var
  Mutex: Integer=0;

function IsRunning(const ID: string): Boolean;
begin
  Result:=false;
  if Mutex<>0 then Exit;
  Mutex:=CreateMutex(nil, true, PChar(ID));
  if GetLastError=ERROR_ALREADY_EXISTS then Result:=true;
end;

initialization

finalization
  ReleaseMutex(Mutex);

end.
