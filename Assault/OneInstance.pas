unit OneInstance;

interface

uses
  Windows, Avl;

function IsRunning(const ID: string): Boolean;

implementation

var
  Mutex: Integer;

function IsRunning(const ID: string): Boolean;
begin
  Result:=false;
  Mutex:=CreateMutex(nil, true, PChar(ID));
  if GetLastError=ERROR_ALREADY_EXISTS then Result:=true;
end;

initialization

finalization
  ReleaseMutex(Mutex);

end.
