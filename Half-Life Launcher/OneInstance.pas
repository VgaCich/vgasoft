unit OneInstance;

interface

uses
  Windows;

function IsRunning: Boolean;

implementation

const
  ID: PChar = 'VS HL Launcher';

var
  Mutex: Integer;

function IsRunning: Boolean;
begin
  Result:=false;
  Mutex:=CreateMutex(nil, true, ID);
  if GetLastError=ERROR_ALREADY_EXISTS then Result:=true;
end;

initialization

finalization
  ReleaseMutex(Mutex);

end.
