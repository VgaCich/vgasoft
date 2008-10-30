unit VSELog;

interface

uses Windows, AvL, avlUtils;

type
  TLogLevel=(llInfo, llWarning, llError); //Level of log message: informational, warning, error

procedure Log(Level: TLogLevel; const S: string); //Add message to log
procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const); //Add message to log, Format version
procedure LogRaw(const S: string); //Add string to log without timestamp
procedure LogAssert(Condition: Boolean; const Msg: string); //Add message if Condition=false
procedure LogException(const Comment: string); //Write exception information to log and append Comment

implementation

var
  LogFile: TFileStream;
  CanLog: Boolean;

procedure LogAssert(Condition: Boolean; const Msg: string);
begin
  if not Condition then Log(llError, 'Assertion failed: '+Msg);
end;

procedure WrLn(const S: string);
const
  CRLF: Word=$0A0D;
begin
  LogFile.Write(S[1], Length(S));
  LogFile.Write(CRLF, 2);
  {$IFDEF VSEDEBUG}FlushFileBuffers(LogFile.Handle);{$ENDIF}
end;

procedure Log(Level: TLogLevel; const S: string);
begin
  if not CanLog then Exit;
  case Level of
    llInfo: WrLn('['+DateTimeToStr(Now)+'] '+S);
    llWarning: WrLn('['+DateTimeToStr(Now)+'] Warning: '+S);
    llError: begin
               WrLn('['+DateTimeToStr(Now)+'] Error: '+S);
               FlushFileBuffers(LogFile.Handle);
             end;
  end;
end;

procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const);
begin
  Log(Level, Format(Fmt, Args));
end;

procedure LogRaw(const S: string);
begin
  if not CanLog then Exit;
  WrLn(S);
end;

procedure LogException(const Comment: string);
begin
  Log(llError, 'Exception "'+ExceptObject.ClassName+'" at '+IntToHex(Cardinal(ExceptAddr), 8)+' with message "'+Exception(ExceptObject).Message+'" '+Comment);
end;

initialization

  DeleteFile(ChangeFileExt(FullExeName, '.log'));
  LogFile:=TFileStream.Create(ChangeFileExt(FullExeName, '.log'), fmCreate);
  CanLog:=true;

finalization

  CanLog:=false;
  FlushFileBuffers(LogFile.Handle);
  FAN(LogFile);

end.
