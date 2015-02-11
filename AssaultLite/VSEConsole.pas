unit VSEConsole;

{$IFNDEF VSE_CONSOLE}{$ERROR Please don't include VSEConsole unit without VSE_CONSOLE defined}{$ENDIF}

interface

uses
  Windows, AvL, avlSyncObjs, avlMath, avlUtils, OpenGL, VSEInit, VSEOpenGLExt,
  VSEGameStates{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TOnConsoleCommand = function(Sender: TObject; Args: array of const): Boolean of object; // Console command handler
  TOnConsoleExecute = function(Sender: TObject; const CommandLine: string): Boolean of object; // Console commands hook
  TConsoleCommandArgument = class // internally used
  private
    FName: string;
    FOptional: Boolean;
    FValue: TVarRec;
    function GetNextOption(var Options: string): string;
    function GetArgument(var CommandLine: string): string;
    function GetDescription: string;
    function GetType: string; virtual; abstract;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; virtual; abstract;
    property Value: TVarRec read FValue;
    property Name: string read FName;
    property Description: string read GetDescription;
  end;
  TConsoleCommand = class // internally used
  private
    FName: string;
    FArguments: array of TConsoleCommandArgument;
    FHandler: TOnConsoleCommand;
    FPrev, FNext: TConsoleCommand;
    function CreateArgumentParser(ArgumentDesc: string): TConsoleCommandArgument;
    function GetDescription: string;
  public
    constructor Create(Prev: TConsoleCommand; CmdDesc: string; Handler: TOnConsoleCommand);
    destructor Destroy; override;
    function Execute(CommandLine: string): Boolean;
    function FindCommand(Name: string): TConsoleCommand;
    property Handler: TOnConsoleCommand read FHandler;
    property Name: string read FName;
    property Description: string read GetDescription;
    property Prev: TConsoleCommand read FPrev;
    property Next: TConsoleCommand read FNext;
  end;
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
    FCommands: TConsoleCommand;
    procedure AddToCache(const Line: string);
    function LogEndPosition: Integer;
    {$IFDEF VSE_LOG}procedure UpdateLog;{$ENDIF}
    procedure SetActive(Value: Boolean);
    function GetCommand(const CmdDesc: string): TOnConsoleCommand;
    procedure SetCommand(const CmdDesc: string; Value: TOnConsoleCommand);
    function HelpHandler(Sender: TObject; Args: array of const): Boolean;
    function CmdListHandler(Sender: TObject; Args: array of const): Boolean;
    function ExecHandler(Sender: TObject; Args: array of const): Boolean;
    function EchoHandler(Sender: TObject; Args: array of const): Boolean;
    class function GetCommandName(CommandLine: string): string;
  public
    constructor Create; // internally used
    destructor Destroy; override; // internally used
    procedure Draw; // internally used
    procedure Update; // internally used
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); // internally used
    procedure CharEvent(C: Char); // internally used
    procedure WriteLn(const Line: string = ''); // Write line to console
    function Execute(const CommandLine: string): Boolean; // Execute command; returns true if successful
    function GetCommands(Prefix: string): TStringList; // Get list of commands, starts with Prefix
    property Active: Boolean read FActive write SetActive; // Console is open
    property OnCommand[const CmdDesc: string]: TOnConsoleCommand read GetCommand write SetCommand; default; // Command handlers; assign nil to delete command; see FmtDocs for commands description language
    property OnExecute: TOnConsoleExecute read FOnExecute write FOnExecute; // Console commands hook
  end;

var
  Console: TConsole; // Console interface

implementation

uses
  VSECore, VSETexMan, VSEMemPak;

const
  DisplayLines = 19;
  VK_TILDE = 192;
  vtInvalid = 255;

type
  TCCAInteger = class(TConsoleCommandArgument)
  private
    FMin, FMax: Integer;
    function GetType: string; override;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; override;
  end;
  TCCAFloat = class(TConsoleCommandArgument)
  private
    FMin, FMax: Single;
    function GetType: string; override;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; override;
  end;
  TCCAString = class(TConsoleCommandArgument)
  private
    FFullLine: Boolean;
    FValueBuffer: string;
    function GetType: string; override;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; override;
  end;
  TCCAEnum = class(TConsoleCommandArgument)
  private
    FValues: array of string;
    function GetType: string; override;
  public
    constructor Create(const Name: string; Options: string);
    function Parse(var CommandLine: string): Boolean; override;
  end;

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
  FCommands := TConsoleCommand.Create(nil, 'help', HelpHandler);
  OnCommand['cmdlist ?prefix=s'] := CmdListHandler;
  OnCommand['exec file=s'] := ExecHandler;
  OnCommand['echo msg=s*'] := EchoHandler;
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
  while Assigned(FCommands.Next) do FCommands.Next.Free;
  FAN(FCommands);
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
begin
  {$IFDEF VSE_LOG}UpdateLog;{$ENDIF}
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
  if not FActive then
  begin
    if (Event = keUp) and (Key = VK_TILDE) then Active := true;
    Exit;
  end;
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
              then SetCommandLine(List[0] + ' ')
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
          if FCommandLine <> '' then
          begin
            FCmdHistory.Add(FCommandLine);
            if FCmdHistory.Count > 32 then FCmdHistory.Delete(0);
            FCmdHistoryIndex := FCmdHistory.Count;
            Execute(FCommandLine);
            SetCommandLine('');
          end;
        end;
      VK_TILDE: if not Core.KeyPressed[VK_SHIFT] then Active := false;
    end;
  end;
end;

procedure TConsole.CharEvent(C: Char);
begin
  if FActive and (C in [#31..#95, #97..#126, #128..#255]) then
  begin
    Insert(C, FCommandLine, FCursor);
    Inc(FCursor);
  end;
end;

procedure TConsole.WriteLn(const Line: string = '');
var
  AtEnd: Boolean;
begin
  {$IFDEF VSE_LOG}UpdateLog;{$ENDIF}
  FLog.Add(Line);
  AtEnd := FLogPosition = LogEndPosition;
  AddToCache(Line);
  if AtEnd then FLogPosition := LogEndPosition;
end;

function TConsole.Execute(const CommandLine: string): Boolean;
var
  Command: TConsoleCommand;
begin
  Result := false;
  if Trim(CommandLine) = '' then Exit;
  if Assigned(FOnExecute) then
    try
      if not FOnExecute(Self, CommandLine) then Exit;
    except
      {$IFDEF VSE_LOG}LogException('in Console.OnExecute handler');{$ENDIF}
      {$IFNDEF VSE_DEBUG}Core.StopEngine(StopUserException);{$ENDIF}
    end;
  Command := FCommands.FindCommand(GetCommandName(CommandLine));
  if not Assigned(Command) then
  begin
    WriteLn('Command "' + GetCommandName(CommandLine) + '" not found');
    Exit;
  end;
  Result := Command.Execute(CommandLine);
end;

function TConsole.GetCommands(Prefix: string): TStringList;
var
  CurCmd: TConsoleCommand;
begin
  Result := TStringList.Create;
  CurCmd := FCommands;
  Prefix := LowerCase(Prefix);
  while Assigned(CurCmd) do
  begin
    if Prefix = Copy(CurCmd.Name, 1, Length(Prefix))
      then Result.Add(CurCmd.Name);
    CurCmd := CurCmd.Next;
  end;
  Result.Sort;
end;

procedure TConsole.AddToCache(const Line: string);
var
  Pos: Integer;
  Postfix: string;
begin
  if FLineLength = 0 then Exit;
  if Line <> '' then
  begin
    Pos := 1;
    Postfix := ' ';
    if LastChar(Line) in [#10, #13] then Postfix := LastChar(Line);
    while Pos <= Length(Line) do
    begin
      FLogCache.Add(Copy(Line, Pos, FLineLength)+Postfix);
      Inc(Pos, FLineLength);
    end;
  end
    else  FLogCache.Add('');
end;

function TConsole.LogEndPosition: Integer;
begin
  Result := Max(FLogCache.Count - DisplayLines, 0);
end;

{$IFDEF VSE_LOG}
procedure TConsole.UpdateLog;
var
  i: Integer;
begin
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
end;
{$ENDIF}

function TConsole.GetCommand(const CmdDesc: string): TOnConsoleCommand;
var
  Command: TConsoleCommand;
begin
  Command := FCommands.FindCommand(GetCommandName(CmdDesc));
  if Assigned(Command)
    then Result := Command.Handler
    else Result := nil;
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

procedure TConsole.SetCommand(const CmdDesc: string; Value: TOnConsoleCommand);
begin
  FCommands.FindCommand(GetCommandName(CmdDesc)).Free;
  if Assigned(Value) then TConsoleCommand.Create(FCommands, CmdDesc, Value);
end;

function TConsole.HelpHandler(Sender: TObject; Args: array of const): Boolean;
begin
  WriteLn;
  WriteLn('Use command "cmdlist [prefix]" to get commands list');
  WriteLn('Use PageUp, PageDown, Ctrl+Up, Ctrl+Down, Ctrl+Home, Ctrl+End, Escape to navigate console log');
  WriteLn('Use Up/Down to navigate commands history');
  WriteLn('Press Tab to autocomplete command');
  WriteLn('Press Insert to paste from clipboard');
  WriteLn('Press Escape to clear command line');
  Result := true;
end;

function TConsole.CmdListHandler(Sender: TObject; Args: array of const): Boolean;
var
  Prefix: string;
  Commands: TStringList;
  i: Integer;
begin
  if Length(Args) > 0
    then Prefix := string(Args[0].VAnsiString)
    else Prefix := '';
  Commands := GetCommands(Prefix);
  try
    for i := 0 to Commands.Count - 1 do
      WriteLn(FCommands.FindCommand(Commands[i]).Description);
  finally
    FAN(Commands);
  end;
  Result := true;
end;

function TConsole.ExecHandler(Sender: TObject; Args: array of const): Boolean;
var
  CmdFileName: string;
  CmdFile: TStringList;
  i: Integer;
begin
  Result := false;
  CmdFileName := string(Args[0].VAnsiString);
  CmdFile:=GetFileText(string(Args[0].VAnsiString));
  if not Assigned(CmdFile) then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'Console: can''t execute file "%s": file not found', [CmdFileName]);{$ENDIF}
    Exit;
  end;
  try
    for i := 0 to CmdFile.Count - 1 do
      if (Length(CmdFile[i]) > 0) and (CmdFile[i][1] <> ';')
        then Execute(CmdFile[i]);
    Result := true;
  finally
    FAN(CmdFile);
  end;
end;

function TConsole.EchoHandler(Sender: TObject; Args: array of const): Boolean;
begin
  WriteLn(string(Args[0].VAnsiString));
  Result := true;
end;

class function TConsole.GetCommandName(CommandLine: string): string;
begin
  Result := LowerCase(Trim(Tok(' ', CommandLine)));
end;

{ TConsoleCommandArgument }

constructor TConsoleCommandArgument.Create(const Name: string; Options:
    string);
begin
  inherited Create;
  FName := Name;
  if FName[1] = '?' then
  begin
    FOptional := true;
    Delete(FName, 1, 1);
  end;
end;

function TConsoleCommandArgument.GetNextOption(var Options: string): string;
begin
  Result := Trim(Tok(':', Options));
end;

function TConsoleCommandArgument.GetArgument(var CommandLine: string): string;
const
  Delim: array[Boolean] of Char = (' ', '"'); 
var
  IsQuoted: Boolean;
  i: Integer;
begin
  Result := '';
  CommandLine := TrimLeft(CommandLine);
  if CommandLine = '' then Exit;
  IsQuoted := CommandLine[1] = '"';
  i := PosEx(Delim[IsQuoted], CommandLine, 2);
  if i = 0 then i := Length(CommandLine);
  Result := Trim(Copy(CommandLine, 1, i));
  Delete(CommandLine, 1, i);
  if IsQuoted then Result := Copy(Result, 2, Length(Result) - 2);
end;

function TConsoleCommandArgument.GetDescription: string;
begin
  Result := Name + ': ' + GetType;
  if FOptional
    then Result := '[' + Result + ']'
    else Result := '<' + Result + '>';
end;

{ TCCAInteger }

constructor TCCAInteger.Create(const Name: string; Options: string);
begin
  inherited;
  if Options <> ''
    then FMin := StrToInt(GetNextOption(Options))
    else FMin := -MaxInt - 1;
  if Options <> ''
    then FMax := StrToInt(GetNextOption(Options))
    else FMax := MaxInt;
end;

function TCCAInteger.GetType: string;
begin
  Result := 'int';
end;

function TCCAInteger.Parse(var CommandLine: string): Boolean;
begin
  if CommandLine = '' then
  begin
    Result := FOptional;
    FValue.VType := vtInvalid;
    Exit;
  end;
  FValue.VType := vtInteger;
  Result := TryStrToInt(GetArgument(CommandLine), FValue.VInteger) and (FValue.VInteger >= FMin) and (FValue.VInteger <= FMax);
end;

{ TCCAFloat }

constructor TCCAFloat.Create(const Name: string; Options: string);
begin
  inherited;
  if Options <> ''
    then FMin := StrToFloat(GetNextOption(Options))
    else FMin := -MaxSingle;
  if Options <> ''
    then FMax := StrToFloat(GetNextOption(Options))
    else FMax := MaxSingle;
end;

function TCCAFloat.GetType: string;
begin
  Result := 'float'; 
end;

function TCCAFloat.Parse(var CommandLine: string): Boolean;
var
  Val: Single;
begin
  if CommandLine = '' then
  begin
    Result := FOptional;
    FValue.VType := vtInvalid;
    Exit;
  end;
  Result := TryStrToFloat(GetArgument(CommandLine), Val) and  (Val >= FMin) and (Val <= FMax);
  FValue.VType := vtExtended;
  FValue.VExtended^ := Val;
end;

{ TCCAString }

constructor TCCAString.Create(const Name: string; Options: string);
begin
  inherited;
  if GetNextOption(Options) = '*' then FFullLine := true;
end;

function TCCAString.GetType: string;
begin
  Result := 'str';
end;

function TCCAString.Parse(var CommandLine: string): Boolean;
begin
  if CommandLine = '' then
  begin
    Result := FOptional;
    FValue.VType := vtInvalid;
    Exit;
  end;
  if FFullLine then
  begin
    FValueBuffer := TrimLeft(CommandLine);
    CommandLine := '';
  end
    else FValueBuffer := GetArgument(CommandLine);
  FValue.VType := vtAnsiString;
  FValue.VAnsiString := Pointer(FValueBuffer);
  Result := true;
end;

{ TCCAEnum }

constructor TCCAEnum.Create(const Name: string; Options: string);
begin
  inherited;
  while Options <> '' do
  begin
    SetLength(FValues, Length(FValues) + 1);
    FValues[High(FValues)] := LowerCase(GetNextOption(Options));
  end;
end;

function TCCAEnum.GetType: string;
var
  i: Integer;
begin
  Result := '';
  if Length(FValues) = 0 then Exit;
  if Length(FValues) <= 5 then
  begin
    for i := 0 to High(FValues) do
      Result := Result + '|' + FValues[i];
    Delete(Result, 1, 1);
  end
    else Result := FValues[0] + ' .. ' + FValues[High(FValues)];
end;

function TCCAEnum.Parse(var CommandLine: string): Boolean;
var
  Val: string;
  i: Integer;
begin
  if CommandLine = '' then
  begin
    Result := FOptional;
    FValue.VType := vtInvalid;
    Exit;
  end;
  Val := LowerCase(GetArgument(CommandLine));
  for i := 0 to High(FValues) do
    if Val = FValues[i] then
    begin
      FValue.VType := vtInteger;
      FValue.VInteger := i;
      Result := true;
      Exit;
    end;
  Result := false;
end;

{ TConsoleCommand }

constructor TConsoleCommand.Create(Prev: TConsoleCommand; CmdDesc: string; Handler: TOnConsoleCommand);
begin
  inherited Create;
  if Assigned(Prev) then
  begin
    FNext := Prev.FNext;
    FPrev := Prev;
    Prev.FNext := Self;
    if Assigned(FNext) then FNext.FPrev := Self;
  end;
  FName := LowerCase(Trim(Tok(' ', CmdDesc)));
  FHandler := Handler;
  while CmdDesc <> '' do
  begin
    SetLength(FArguments, Length(FArguments) + 1);
    FArguments[High(FArguments)] := CreateArgumentParser(Trim(Tok(' ', CmdDesc)));
    if not Assigned(FArguments[High(FArguments)]) then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'Console: can''t parse command "%s" arguments description', [FName]);{$ENDIF}
      raise Exception.Create('Unable to parse command arguments description');
    end;
  end;
end;

destructor TConsoleCommand.Destroy;
var
  i: Integer;
begin
  if Assigned(FPrev) then FPrev.FNext := FNext;
  if Assigned(FNext) then FNext.FPrev := FPrev;
  for i := 0 to High(FArguments) do
    FArguments[i].Free;
  Finalize(FArguments);
  inherited;
end;

function TConsoleCommand.CreateArgumentParser(ArgumentDesc: string): TConsoleCommandArgument;
var
  Name: string;
  Type_: Char;
begin
  Result := nil;
  Name := Tok('=', ArgumentDesc);
  if (Name = '') or (Length(ArgumentDesc) < 2) then Exit;
  Type_ := UpCase(ArgumentDesc[2]);
  Delete(ArgumentDesc, 1, 2);
  case Type_ of
    'I': Result := TCCAInteger.Create(Name, ArgumentDesc);
    'F': Result := TCCAFloat.Create(Name, ArgumentDesc);
    'S': Result := TCCAString.Create(Name, ArgumentDesc);
    'E': Result := TCCAEnum.Create(Name, ArgumentDesc);
  end;
end;

function TConsoleCommand.Execute(CommandLine: string): Boolean;
var
  i, ArgsCount: Integer;
  Arguments: array of TVarRec;
begin
  Result := false;
  Tok(' ', CommandLine);
  CommandLine := TrimLeft(CommandLine);
  ArgsCount := 0;
  for i := 0 to High(FArguments) do
  begin
    if not FArguments[i].Parse(CommandLine) then
    begin
      Console.WriteLn('Error: invalid argument #' + IntToStr(i + 1));
      Exit;
    end;
    if FArguments[i].Value.VType <> vtInvalid then Inc(ArgsCount);
  end;
  if Assigned(FHandler) then
    try
      SetLength(Arguments, ArgsCount);
      for i := 0 to ArgsCount - 1 do
        Arguments[i] := FArguments[i].Value;
      Result := FHandler(Self, Arguments);
    except
      {$IFDEF VSE_LOG}LogException('in console command "' + Name + '" handler');{$ENDIF}
      {$IFNDEF VSE_DEBUG}Core.StopEngine(StopUserException);{$ENDIF}
    end;
end;

function TConsoleCommand.FindCommand(Name: string): TConsoleCommand;
begin
  Name := LowerCase(Name);
  Result := Self;
  while Assigned(Result) do
    if Result.Name = Name
      then Exit
      else Result := Result.Next;
end;

function TConsoleCommand.GetDescription: string;
var
  i: Integer;
begin
  Result := FName;
  for i := 0 to High(FArguments) do
    Result := Result + ' ' + FArguments[i].Description;
end;

{$IFDEF VSE_LOG}
initialization
  LogOnUpdate := LogUpdateHandler;
{$ENDIF}

end.