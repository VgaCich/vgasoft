unit VSELog;

interface

uses Windows, AvL, avlSyncObjs, avlUtils;

type
  TLogLevel=(llInfo, llWarning, llError); //Level of log message: informational, warning, error

procedure Log(Level: TLogLevel; const S: string); //Add message to log
procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const); //Add message to log, Format version
procedure LogRaw(Level: TLogLevel; const S: string);
procedure LogAssert(Condition: Boolean; const Msg: string); //Add message if Condition=false
procedure LogException(const Comment: string); //Write exception information to log and append Comment

var
  LogLevel: TLogLevel=llInfo; //Minimal level of message, that will pass to log

implementation

type
  TLoggerThread=class(TThread)
  protected
    procedure Execute; override;
  end;

var
  LogBuffer: string = '';
  LogBufferLock: TCriticalSection;
  LogEvent: TEvent;
  Logger: TLoggerThread;
  LogInitialized: Boolean;

procedure TLoggerThread.Execute;
var
  LogFile: TFileStream;
  Buffer: string;
begin
  DeleteFile(ChangeFileExt(FullExeName, '.log'));
  LogFile:=TFileStream.Create(ChangeFileExt(FullExeName, '.log'), fmCreate);
  try
    while not Terminated do
      if LogEvent.WaitFor(INFINITE)=wrSignaled then
      begin
        LogBufferLock.Acquire;
        Buffer:=LogBuffer;
        LogBuffer:='';
        LogBufferLock.Release;
        LogFile.Write(Buffer[1], Length(Buffer));
        FlushFileBuffers(LogFile.Handle);
      end
      else begin
        Buffer:='LogEvent error: '+SysErrorMessage(LogEvent.LastError);
        LogFile.Write(Buffer[1], Length(Buffer));
        Break;
      end;
  finally
    FAN(LogFile);
  end;
end;

procedure LogAssert(Condition: Boolean; const Msg: string);
begin
  if not Condition then Log(llError, 'Assertion failed: '+Msg);
end;

procedure Log(Level: TLogLevel; const S: string);
begin
  case Level of
    llInfo: LogRaw(Level, '['+DateTimeToStr(Now)+'] '+S);
    llWarning: LogRaw(Level, '['+DateTimeToStr(Now)+'] Warning: '+S);
    llError: LogRaw(Level, '['+DateTimeToStr(Now)+'] Error: '+S);
  end;
end;

procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const);
begin
  Log(Level, Format(Fmt, Args));
end;

procedure LogRaw(Level: TLogLevel; const S: string);
begin
  if (Level<LogLevel) or not LogInitialized then Exit;
  LogBufferLock.Acquire;
  LogBuffer:=LogBuffer+S+#13#10;
  LogBufferLock.Release;
  LogEvent.SetEvent;
end;

procedure LogException(const Comment: string);
begin
  Log(llError, 'Exception "'+ExceptObject.ClassName+'" at '+IntToHex(Cardinal(ExceptAddr), 8)+' with message "'+Exception(ExceptObject).Message+'" '+Comment);
end;

initialization

  LogEvent:=TEvent.Create(nil, false, false, LogBuffer);
  LogBufferLock:=TCriticalSection.Create;
  Logger:=TLoggerThread.Create(false);
  LogInitialized:=true;

finalization

  LogInitialized:=false;
  Logger.Terminate;
  LogEvent.SetEvent;
  Logger.WaitFor;
  FAN(Logger);
  FAN(LogEvent);
  FAN(LogBufferLock);
  LogBuffer:='';

end.
