unit ULog;

interface

uses AvL, avlUtils, ULogFile, UConsole;

type
  TLogLevel=(llInfo, llWarning, llError);

procedure Log(Level: TLogLevel; const S: string);
procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const);
procedure LogRaw(const S: string);
procedure LogAssert(Condition: Boolean; const Msg: string);
procedure LogNC(Level: TLogLevel; const S: string);
procedure LogFNC(Level: TLogLevel; const Fmt: string; const Args: array of const);
procedure LogRawNC(const S: string);
procedure LogException(const Comment: string);

implementation

procedure Log(Level: TLogLevel; const S: string);
begin
  if not CanLog then Exit;
  case Level of
    llInfo: begin
              WriteLn(LogFile, '['+DateTimeToStr(Now)+'] '+S);
              if Console<>nil then Console.AddToConsole(S);
            end;
    llWarning: begin
                 WriteLn(LogFile, '['+DateTimeToStr(Now)+'] Warning: '+S);
                 if Console<>nil then Console.AddToConsole('^3Warning: '+S);
               end;
    llError: begin
               WriteLn(LogFile, '['+DateTimeToStr(Now)+'] Error: '+S);
               if Console<>nil then Console.AddToConsole('^1Error: '+S);
               Flush(LogFile);
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
  WriteLn(LogFile, S);
  {$IFDEF VSEDEBUG}Flush(LogFile);{$ENDIF};
  if Console<>nil then Console.AddToConsole(S);
end;

procedure LogAssert(Condition: Boolean; const Msg: string);
begin
  if not Condition then Log(llError, 'Assertion failed: '+Msg);
end;

procedure LogNC(Level: TLogLevel; const S: string);
begin
  if not CanLog then Exit;
  case Level of
    llInfo: WriteLn(LogFile, '['+DateTimeToStr(Now)+'] '+S);
    llWarning: WriteLn(LogFile, '['+DateTimeToStr(Now)+'] Warning: '+S);
    llError: begin
               WriteLn(LogFile, '['+DateTimeToStr(Now)+'] Error: '+S);
               Flush(LogFile);
             end;
  end;
end;

procedure LogFNC(Level: TLogLevel; const Fmt: string; const Args: array of const);
begin
  LogNC(Level, Format(Fmt, Args));
end;

procedure LogRawNC(const S: string);
begin
  if not CanLog then Exit;
  WriteLn(LogFile, S);
  {$IFDEF VSEDEBUG}Flush(LogFile);{$ENDIF}
end;

procedure LogException(const Comment: string);
begin
  Log(llError, 'Exception "'+ExceptObject.ClassName+'" at '+IntToHex(Cardinal(ExceptAddr), 8)+' with message "'+Exception(ExceptObject).Message+'" '+Comment);
end;

end.
