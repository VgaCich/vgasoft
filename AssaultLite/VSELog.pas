unit VSELog;

interface

uses Windows, AvL, avlSyncObjs, avlUtils;

type
  TLogLevel=(llInfo, llWarning, llError, llAlways); //Level of log message: informational, warning, error

procedure Log(Level: TLogLevel; const S: string); //Add message to log
procedure LogF(Level: TLogLevel; const Fmt: string; const Args: array of const); //Add message to log, Format version
procedure LogRaw(Level: TLogLevel; const S: string);
procedure LogAssert(Condition: Boolean; const Msg: string); //Add message if Condition=false

var
  LogLevel: TLogLevel=llInfo; //Minimal level of message, that will be passed to log

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
  LogStart, LastEvent: Cardinal;
  LogErrors: Integer = 0;
  LogWarnings: Integer = 0;

procedure TLoggerThread.Execute;
var
  LogFile: TFileStream;
  Buffer, LogFileName: string;

  procedure WriteBuffer;
  begin
    LogBufferLock.Acquire;
    Buffer:=LogBuffer;
    LogBuffer:='';
    LogBufferLock.Release;
    LogFile.Write(Buffer[1], Length(Buffer));
    FlushFileBuffers(LogFile.Handle);
  end;

begin
  LogFileName:=ChangeFileExt(FullExeName, '.log');
  if not FileExists(LogFileName)
    then CloseHandle(FileCreate(LogFileName));
  LogFile:=TFileStream.Create(LogFileName, fmOpenWrite or fmShareDenyWrite);
  LogFile.Position:=0;
  SetEndOfFile(LogFile.Handle);
  try
    while not Terminated do
      if LogEvent.WaitFor(INFINITE)=wrSignaled
        then WriteBuffer
      else begin
        Buffer:='LogEvent error: '+SysErrorMessage(LogEvent.LastError);
        LogFile.Write(Buffer[1], Length(Buffer));
        FlushFileBuffers(LogFile.Handle);
        Break;
      end;
    WriteBuffer;
  finally
    FAN(LogFile);
  end;
end;

procedure LogAssert(Condition: Boolean; const Msg: string);
begin
  if not Condition then Log(llError, 'Assertion failed: '+Msg);
end;

procedure Log(Level: TLogLevel; const S: string);
var
  TimeStamp: string;
  Time, Delta: Cardinal;
begin
  LogBufferLock.Acquire;
  Time:=GetTickCount;
  Delta:=Time-LastEvent;
  LastEvent:=Time;
  Time:=Time-LogStart;
  LogBufferLock.Release;
  TimeStamp:=Format('[%02d:%02d:%02d.%03d (+%d ms)] ', [Time div 3600000, Time mod 3600000 div 60000, Time mod 60000 div 1000, Time mod 1000, Delta]);
  case Level of
    llInfo, llAlways: LogRaw(Level, TimeStamp+S);
    llWarning: begin
      LogRaw(Level, TimeStamp+'Warning: '+S);
      InterlockedIncrement(LogWarnings);
    end;
    llError: begin
      LogRaw(Level, TimeStamp+'Error: '+S);
      InterlockedIncrement(LogErrors);
    end;
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

initialization

  LogEvent:=TEvent.Create(nil, false, false, LogBuffer);
  LogBufferLock:=TCriticalSection.Create;
  Logger:=TLoggerThread.Create(false);
  LogStart:=GetTickCount;
  LastEvent:=LogStart;
  LogInitialized:=true;
  LogRaw(llAlways, 'Log started at '+DateTimeToStr(Now));
  LogRaw(llAlways, '');

finalization

  LogRaw(llAlways, '');
  LogRaw(llAlways, 'Log closed at '+DateTimeToStr(Now));
  LogRaw(llAlways, 'Errors: '+IntToStr(LogErrors)+', Warnings: '+IntToStr(LogWarnings));
  LogInitialized:=false;
  Logger.Terminate;
  LogEvent.SetEvent;
  Logger.WaitFor;
  FAN(Logger);
  FAN(LogEvent);
  FAN(LogBufferLock);
  LogBuffer:='';

end.
