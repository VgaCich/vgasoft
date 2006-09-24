unit ULog;

interface

uses AvL, avlUtils, UConsole;

procedure Log(const S: string);
procedure LogF(const Fmt: string; const Args: array of const);
procedure LogRaw(const S: string);
procedure LogAssert(Condition: Boolean; const Msg: string);
procedure LogNC(const S: string);
procedure LogFNC(const Fmt: string; const Args: array of const);
procedure LogRawNC(const S: string);

implementation

var
  LogFile: TextFile;

procedure Log(const S: string);
begin
  WriteLn(LogFile, '['+DateTimeToStr(Now)+'] '+S);
  Flush(LogFile);
  if Console<>nil then Console.AddToConsole(S);
end;

procedure LogF(const Fmt: string; const Args: array of const);
begin
  Log(Format(Fmt, Args));
end;

procedure LogRaw(const S: string);
begin
  WriteLn(LogFile, S);
  Flush(LogFile);
  if Console<>nil then Console.AddToConsole(S);
end;

procedure LogAssert(Condition: Boolean; const Msg: string);
begin
  if not Condition then Log(Msg);
end;

procedure LogNC(const S: string);
begin
  WriteLn(LogFile, '['+DateTimeToStr(Now)+'] '+S);
  Flush(LogFile);
end;

procedure LogFNC(const Fmt: string; const Args: array of const);
begin
  LogNC(Format(Fmt, Args));
end;

procedure LogRawNC(const S: string);
begin
  WriteLn(LogFile, S);
  Flush(LogFile);
end;

initialization

AssignFile(LogFile, ChangeFileExt(FullExeName, '.log'));
ReWrite(LogFile);

finalization

Flush(LogFile);
CloseFile(LogFile);

end.
