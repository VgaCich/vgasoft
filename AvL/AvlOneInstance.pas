unit AvlOneInstance;

interface

uses
  Windows, Avl;

function IsRunning: Boolean;

const
  UniqueID = 'Avl Program: ';

implementation

function IsRunning: Boolean;
var
  hMutex : Integer;
begin
  Result := False;
  hMutex := CreateMutex(nil, True , PChar(UniqueID + ExtractFileName(ParamStr(0))));
  if GetLastError = ERROR_ALREADY_EXISTS then Result := True;
end;

end.

