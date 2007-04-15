unit ULogFile;

interface

uses avlUtils;

var
  LogFile: TextFile;
  CanLog: Boolean;

implementation

initialization

AssignFile(LogFile, ChangeFileExt(FullExeName, '.log'));
ReWrite(LogFile);
CanLog:=true;

finalization

CanLog:=false;
Flush(LogFile);
CloseFile(LogFile);

end.