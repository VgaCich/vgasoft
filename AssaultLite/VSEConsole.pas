unit VSEConsole;

{$IFNDEF VSE_CONSOLE}{$ERROR Please don't include VSEConsole unit without VSE_CONSOLE defined}{$ENDIF}

interface

uses
  Windows, AvL, avlSyncObjs, avlMath, avlUtils, OpenGL, VSEInit, VSEOpenGLExt,
  VSEGameStates{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TOnConsoleCommand = function(Sender: TObject; ArgsCount: Integer; Args: array of const): Boolean of object;
  TOnConsoleExecute = function(Sender: TObject; const Command: string): Boolean of object;
  TConsole = class
  private
    FActive: Boolean;
    FFont: Integer;
    FLog, FLogCache, FCmdHistory: TStringList;
    {$IFDEF VSE_LOG}
    FLogBuffer: TStringList;
    FLogBufferLock: TCriticalSection;
    FLogBufferEvent: TEvent;
    {$ENDIF}
    FLogPosition, FCursor, FCmdHistoryIndex: Integer;
    FCommandLine: string;
    FOnExecute: TOnConsoleExecute;
    FScreenWidth, FLineLength, FCharWidth: Integer;
    procedure AddToCache(const Line: string);
    function LogEndPosition: Integer;
    procedure SetActive(Value: Boolean);
    function GetCommand(const Cmd: string): TOnConsoleCommand;
    procedure SetCommand(const Cmd: string; Value: TOnConsoleCommand);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure Update;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent);
    procedure CharEvent(C: Char);
    procedure WriteLn(const Line: string);
    function Execute(Command: string): Boolean;
    function GetCommands(const Prefix: string): TStringList;
    property Active: Boolean read FActive write SetActive;
    property OnCommand[const Cmd: string]: TOnConsoleCommand read GetCommand write SetCommand; default;
    property OnExecute: TOnConsoleExecute read FOnExecute write FOnExecute;
  end;
  
var
  Console: TConsole;

implementation

uses
  VSECore, VSETexMan;

const
  DisplayLines = 19;
  VK_TILDE = 192;
  
{$IFDEF VSE_LOG}
procedure LogUpdateHandler(Level: TLogLevel; const S: string);
begin
  if Assigned(Console) then
  begin
    Console.FLogBufferLock.Acquire;
    case Level of
      llInfo, llAlways: Console.FLogBuffer.Add(S);
      llWarning: Console.FLogBuffer.Add('Warning: ' + S + #10);
      llError: Console.FLogBuffer.Add('Error: ' + S + #13);
    end;
    Console.FLogBufferLock.Release;
    Console.FLogBufferEvent.SetEvent;
  end;
end;
{$ENDIF}

function LastChar(const S: string): Char;
begin
  Result := #0;
  if S <> '' then Result := S[Length(S)];
end;

{ TConsole }

constructor TConsole.Create;
begin
  inherited;
  FFont := -1;
  FLog := TStringList.Create;
  FLogCache := TStringList.Create;
  FCmdHistory := TStringList.Create;
  {$IFDEF VSE_LOG}
  Log(llInfo, 'Console: Create');
  FLogBuffer := TStringList.Create;
  FLogBufferLock := TCriticalSection.Create;
  FLogBufferEvent := TEvent.Create(nil, false, false, '');
  WriteLn('Console: Create');
  {$ENDIF}
end;

destructor TConsole.Destroy;
begin
  {$IFDEF VSE_LOG}
  Log(llInfo, 'Console: Destroy');
  Update;
  FLog.SaveToFile(ExePath + 'Console.log');
  FAN(FLogBufferLock);
  FAN(FLogBufferEvent);
  FAN(FLogBuffer);
  {$ENDIF}
  FAN(FCmdHistory);
  FAN(FLogCache);
  FAN(FLog);
  inherited;
end;

procedure TConsole.Draw;
var
  i, CommandLineStart: Integer;
  CaretPos, CaretHeight: Single;
begin
  if FFont = -1 then FFont := TexMan.FontCreate('Courier New', 10, true);
  if not FActive then Exit;
  if FScreenWidth <> (600 * Core.ResolutionX) div Core.ResolutionY then
  begin
    FScreenWidth := (600 * Core.ResolutionX) div Core.ResolutionY;
    FCharWidth := TexMan.TextWidth(FFont, '_');
    FLineLength := FScreenWidth div FCharWidth;
    FLogCache.Clear;
    for i := 0 to FLog.Count - 1 do
      AddToCache(FLog[i]);
    FLogPosition := LogEndPosition;
  end;
  glPushMatrix;
  glPushAttrib(GL_ENABLE_BIT or GL_LINE_BIT or GL_LIGHTING_BIT or GL_CURRENT_BIT or GL_COLOR_BUFFER_BIT);
  gleOrthoMatrix2(0, 0, FScreenWidth, 600);
  glEnable(GL_LINE_SMOOTH);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glLineWidth(Core.ResolutionY / 600);
  glBegin(GL_QUADS);
    glColor(0.3, 0.3, 0.3, 0.8);
    glVertex(0, 0);
    glVertex(FScreenWidth, 0);
    glVertex(FScreenWidth, 302);
    glVertex(0, 302);
  glEnd;
  glBegin(GL_LINES);
    glColor(0.2, 0.2, 0.2, 0.5);
    for i := 0 to 100 do
    begin
      glVertex(0, 3 * i);
      glVertex(FSCreenWidth, 3 * i);
    end;
    glColor(0.0, 1.0, 0.0);
    glVertex(0, 302);
    glVertex(FScreenWidth, 302);
    glColor(0.5, 0.8, 0.5, 0.8);
    glVertex(0, 285);
    glVertex(FScreenWidth, 285);
    glColor(0.5, 0.5, 0.5, 0.5);
    glVertex(FScreenWidth - 15, 0);
    glVertex(FScreenWidth - 15, 285);
  glEnd;
  glBegin(GL_QUADS);
    CaretHeight := Max(285 * DisplayLines / FLogCache.Count, 5);
    CaretPos := Min(285 * FLogPosition / FLogCache.Count, 285 - CaretHeight);
    glVertex(FScreenWidth - 15, CaretPos);
    glVertex(FScreenWidth, CaretPos);
    glVertex(FScreenWidth, CaretPos + CaretHeight);
    glVertex(FScreenWidth - 15, CaretPos + CaretHeight);
  glEnd;
  for i := 0 to Min(DisplayLines - 1, FLogCache.Count - FLogPosition - 1) do
  begin
    if LastChar(FLogCache[FLogPosition + i]) = #13 then glColor(1.0, 0.0, 0.0)
    else if LastChar(FLogCache[FLogPosition + i]) = #10 then glColor(1.0, 1.0, 0.0)
    else glColor(0.0, 1.0, 0.0);
    TexMan.TextOut(FFont, 0, 15 * i, FLogCache[FLogPosition + i]);
  end;
  glColor(0.0, 1.0, 0.0);
  CommandLineStart := Max(FCursor - FLineLength + 1, 0);
  TexMan.TextOut(FFont, 0, 285, '>' + Copy(FCommandLine, CommandLineStart + 1, FLineLength - 1));
  if (Core.Time div 500) mod 2 = 0 then TexMan.TextOut(FFont, FCharWidth * (FCursor - CommandLineStart), 285, '_');
  glBegin(GL_QUADS);
    glColor(0.3, 0.3, 0.3, 0.8);
    glVertex(FScreenWidth - 8 * FCharWidth - 15, 0);
    glVertex(FScreenWidth - 16, 0);
    glVertex(FScreenWidth - 16, 15);
    glVertex(FScreenWidth - 8 * FCharWidth - 15, 15);
  glEnd;
  glColor(0.0, 1.0, 1.0);
  TexMan.TextOut(FFont, FScreenWidth - 8 * FCharWidth - 15, 0, 'FPS: ' + IntToStr(Core.FPS));
  glPopAttrib;
  glPopMatrix;
end;

procedure TConsole.Update;
var
  i: Integer;
begin
  {$IFDEF VSE_LOG}
  if FLogBufferEvent.WaitFor(0) = wrSignaled then
  begin
    FLogBufferLock.Acquire;
    try
      for i := 0 to FLogBuffer.Count-1 do
        WriteLn(FLogBuffer[i]);
      FLogBuffer.Clear;
    finally
      FLogBufferLock.Release;
    end;
  end;
  {$ENDIF}
end;

procedure TConsole.KeyEvent(Key: Integer; Event: TKeyEvent);
var
  List: TStringList;
  i: Integer;
  S: string;

  procedure SetCommandLine(const Cmd: string);
  begin
    FCommandLine := Cmd;
    FCursor := Length(Cmd) + 1;
  end;

begin
  if (Event = keDown) then
  begin
    case Key of
      VK_PRIOR: FLogPosition := Max(FLogPosition - DisplayLines + 1, 0);
      VK_NEXT: FLogPosition := Min(FLogPosition + DisplayLines - 1, LogEndPosition);
      VK_LEFT: FCursor := Max(FCursor - 1, 1);
      VK_RIGHT: FCursor := Min(FCursor + 1, Length(FCommandLine) + 1);
      VK_UP: if Core.KeyPressed[VK_CONTROL]
        then FLogPosition := Max(FLogPosition - 1, 0)
        else begin
          FCmdHistoryIndex := Max(FCmdHistoryIndex - 1, 0);
          SetCommandLine(FCmdHistory[FCmdHistoryIndex]);
        end;
      VK_DOWN: if Core.KeyPressed[VK_CONTROL]
        then FLogPosition := Min(FLogPosition + 1, LogEndPosition)
        else begin
          FCmdHistoryIndex := Min(FCmdHistoryIndex + 1, FCmdHistory.Count - 1);
          SetCommandLine(FCmdHistory[FCmdHistoryIndex]);
        end;
      VK_DELETE: Delete(FCommandLine, FCursor, 1);
      VK_BACK: if FCursor > 1 then
        begin
          Delete(FCommandLine, FCursor - 1, 1);
          FCursor := Max(FCursor - 1, 1);
        end;
    end;
  end
  else begin
    case Key of
      VK_TAB: if FCommandLine <> '' then
        begin
          List := GetCommands(FCommandLine);
          try
            if List.Count = 1
              then SetCommandLine(List[0])
            else if List.Count > 1 then
              for i := 0 to List.Count - 1 do
                WriteLn('    ' + List[i]);
          finally
            FAN(List);
          end;
        end;
      VK_HOME: if Core.KeyPressed[VK_CONTROL]
        then FLogPosition := 0
        else FCursor := 1;
      VK_END: if Core.KeyPressed[VK_CONTROL]
        then FLogPosition := LogEndPosition
        else FCursor := Length(FCommandLine) + 1;
      VK_INSERT:
        begin
          S := GetClipboardText;
          Insert(S, FCommandLine, FCursor);
          Inc(FCursor, Length(S)); 
        end;
      VK_ESCAPE: if FLogPosition <> LogEndPosition
        then FLogPosition := LogEndPosition
        else SetCommandLine('');
      VK_RETURN:
        begin
          WriteLn('>' + FCommandLine);
          FCmdHistory.Add(FCommandLine);
          if FCmdHistory.Count > 32 then FCmdHistory.Delete(0);
          FCmdHistoryIndex := FCmdHistory.Count;
          Execute(FCommandLine);
          SetCommandLine('');
        end;
      VK_TILDE: if not Core.KeyPressed[VK_SHIFT] then Active := not Active;
    end;
  end;
end;

procedure TConsole.CharEvent(C: Char);
begin
  if C in [#31..#95, #97..#126, #126..#255] then
  begin
    Insert(C, FCommandLine, FCursor);
    Inc(FCursor);
  end;
end;

procedure TConsole.WriteLn(const Line: string);
var
  AtEnd: Boolean;
begin
  FLog.Add(Line);
  AtEnd := FLogPosition = LogEndPosition;
  AddToCache(Line);
  if AtEnd then FLogPosition := LogEndPosition;
end;

function TConsole.Execute(Command: string): Boolean;
begin
  Result := false;
  if Assigned(FOnExecute) then
    try
      if not FOnExecute(Self, Command) then Exit;
    except
      {$IFDEF VSE_LOG}LogException('in Console.OnExecute handler');{$ENDIF}
      {$IFNDEF VSE_DEBUG}StopEngine(StopUserException);{$ENDIF}
    end;
end;

function TConsole.GetCommands(const Prefix: string): TStringList;
begin

end;

procedure TConsole.AddToCache(const Line: string);
var
  Pos: Integer;
  Postfix: string;
begin
  if FLineLength = 0 then Exit;
  Pos := 1;
  Postfix := ' ';
  if LastChar(Line) in [#10, #13] then Postfix := LastChar(Line);
  while Pos <= Length(Line) do
  begin
    FLogCache.Add(Copy(Line, Pos, FLineLength)+Postfix);
    Inc(Pos, FLineLength);
  end;
end;

function TConsole.LogEndPosition: Integer;
begin
  Result := Max(FLogCache.Count - DisplayLines, 0);
end;

function TConsole.GetCommand(const Cmd: string): TOnConsoleCommand;
begin

end;

procedure TConsole.SetActive(Value: Boolean);
begin
  FActive := Value;
  if FActive then
  begin
    if FCursor = 0 then WriteLn('Print "help" for help'#10);
    FLogPosition := LogEndPosition;
    FCommandLine:='';
    FCursor:=1;
    FCmdHistoryIndex := FCmdHistory.Count;
  end;
end;

procedure TConsole.SetCommand(const Cmd: string; Value: TOnConsoleCommand);
begin

end;

{$IFDEF VSE_LOG}
initialization
  LogOnUpdate := LogUpdateHandler;
{$ENDIF}

end.