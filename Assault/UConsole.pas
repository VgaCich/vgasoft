unit UConsole;

interface

uses
  Windows, Messages, AvL, avlUtils, PakMan;

type
  TConsoleCmdFunction=function(const Args: string): Boolean of object;
  TConsoleCmd=record
    Name, Help: string;
    Func: TConsoleCmdFunction;
    Exist: Boolean;
  end;
  TOnConsoleExec=function(Sender: TObject; const Cmd: string): Boolean of object;
  TConsole=class
  private
    FConsoleHistory, FCommandsHistory: TStringList;
    FCommands: array of TConsoleCmd;
    FCommandsSize: Cardinal;
    FOnAdding: TOnEvent;
    FOnExec: TOnConsoleExec;
    FGenericHelp: string;
    FPakMan: TPakMan;
    FHistoryEnabled, FCapture: Boolean;
    procedure Grow;
    function  FindCmd(Name: string): Integer;
    function  GetHistory(Index: Cardinal): string;
    function  GetHistoryCount: Cardinal;
    function  GetCommandsHistory(Index: Cardinal): string;
    function  GetCommandsHistoryCount: Cardinal;
  protected
    procedure RegisterBuiltIns;
    function  CmdList(const Args: string): Boolean;
    function  Exec(const Args: string): Boolean;
    function  Help(const Args: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function  ExecCommand(const Cmd: string): Boolean;
    procedure AddToConsole(const S: string);
    function  RegisterCommand(const Name, Help: string; Func: TConsoleCmdFunction): Boolean;
    procedure DeleteCommand(Name: string);
    procedure IncrementalFind(CmdPrefix: string; CmdList: TStringList);
    property  History[Index: Cardinal]: string read GetHistory; default;
    property  HistoryCount: Cardinal read GetHistoryCount;
    property  CommandsHistory[Index: Cardinal]: string read GetCommandsHistory;
    property  CommandsHistoryCount: Cardinal read GetCommandsHistoryCount;
    property  OnAdding: TOnEvent read FOnAdding write FOnAdding;
    property  OnExec: TOnConsoleExec read FOnExec write FOnExec;
    property  GenericHelp: string read FGenericHelp write FGenericHelp;
    property  HistoryEnabled: Boolean read FHistoryEnabled write FHistoryEnabled;
    property  Capture: Boolean read FCapture write FCapture;
    property  PakMan: TPakMan read FPakMan write FPakMan;
  end;

var
  Console: TConsole;

const
  ConsoleCommandPrefix='/';

implementation

uses ULog;

const
  CScriptExt='.cfg';
  GenericHelpInit='Console controls:'#13+
              '  PageUp/PageDown - scroll console history up/down'#13+
              '  Home/End - set cursor to beginning/ending of line'#13+
              '  Left/Right - move cursor left/right'#13+
              '  Up/Down - scroll commands history'#13+
              '  Tab - autocompletion/incremental search'#13+
              #13+
              'print "'+ConsoleCommandPrefix+'cmdlist" for list of commands'#13+
              'print "'+ConsoleCommandPrefix+'help <command>" for help about command'#13+
              #13+
              '<arg> - required argument'#13+
              '[arg] - optional argument';
  CmdListHelp='cmdlist <prefix>'#13+
              '  Shows list of commands, starts with prefix'#13+
              'cmdlist'#13+
              '  Shows list of all commands';
  ExecHelp='exec <scriptname>'#13+
           '  Executes script scriptname';
  HelpHelp='help <command>'#13+
           '  Shows help about command'#13+
           'help'#13+
           '  Shows generic help';

constructor TConsole.Create;
begin
  inherited Create;
  FConsoleHistory:=TStringList.Create;
  FCommandsHistory:=TStringList.Create;
  FCommandsSize:=8;
  FGenericHelp:=GenericHelpInit;
  FHistoryEnabled:=true;
  Grow;
  RegisterBuiltIns;
end;

destructor TConsole.Destroy;
begin
  {$IFDEF VSEDEBUG}FConsoleHistory.SaveToFile(ExePath+'Console.log');{$ENDIF}
  FAN(FConsoleHistory);
  FAN(FCommandsHistory);
  Finalize(FCommands);
  inherited Destroy;
end;

function TConsole.ExecCommand(const Cmd: string): Boolean;
var
  CmdName, CmdArgs: string;
  Index: Integer;
begin
  Result:=false;
  if FHistoryEnabled then AddToConsole(Cmd);
  if FHistoryEnabled then FCommandsHistory.Add(Cmd);
  if FCommandsHistory.Count>256 then FCommandsHistory.Delete(0);
  if Assigned(FOnExec) then Result:=FOnExec(Self, Cmd);
  if FCapture then Exit;
  if (Cmd='') or (Cmd[1]<>ConsoleCommandPrefix) then Exit;
  Index:=Pos(' ', Cmd);
  if Index>0 then
  begin
    CmdName:=Copy(Cmd, 2, Index-2);
    CmdArgs:=Trim(Copy(Cmd, Index+1, MaxInt));
  end
  else begin
    CmdName:=Copy(Cmd, 2, MaxInt);
    CmdArgs:='';
  end;
  Index:=FindCmd(CmdName);
  if Index<0 then
  begin
    LogF(llError, 'Command "%s" is not defined', [CmdName]);
    Exit;
  end;
  Result:=FCommands[Index].Func(CmdArgs);
end;

procedure TConsole.AddToConsole(const S: string);
begin
  FConsoleHistory.Add(S);
  if Assigned(FOnAdding) then FOnAdding(Self);
end;

function TConsole.RegisterCommand(const Name, Help: string; Func: TConsoleCmdFunction): Boolean;

  procedure AddCommand(Index: Integer);
  begin
    FCommands[Index].Name:=LowerCase(Name);
    FCommands[Index].Help:=Help;
    FCommands[Index].Func:=Func;
    FCommands[Index].Exist:=true;
  end;

var
  i: Integer;
begin
  Result:=false;
  if Pos(' ', Name)>0 then
  begin
    LogF(llError, 'Registering console command: name contains spaces (%s)', [Name]);
    Exit;
  end;
  for i:=0 to FCommandsSize-1 do
    if not FCommands[i].Exist then
    begin
      AddCommand(i);
      Result:=true;
      Exit;
    end;
  i:=FCommandsSize;
  Grow;
  AddCommand(i);
  Result:=true;
end;

procedure TConsole.DeleteCommand(Name: string);
var
  i: Integer;
begin
  Name:=LowerCase(Name);
  for i:=0 to FCommandsSize-1 do
    if FCommands[i].Exist and (FCommands[i].Name=Name) then
    begin
      FCommands[i].Name:='';
      FCommands[i].Help:='';
      FCommands[i].Func:=nil;
      FCommands[i].Exist:=false;
    end;
end;

procedure TConsole.IncrementalFind(CmdPrefix: string; CmdList: TStringList);
var
  i, PrefixLen: Integer;
begin
  CmdList.Clear;
  CmdPrefix:=LowerCase(CmdPrefix);
  PrefixLen:=Pos(' ', CmdPrefix);
  if PrefixLen>0 then CmdPrefix:=Copy(CmdPrefix, 1, PrefixLen-1);
  PrefixLen:=Length(CmdPrefix);
  for i:=0 to FCommandsSize-1 do
    if FCommands[i].Exist and (Copy(FCommands[i].Name, 1, PrefixLen)=CmdPrefix)
      then CmdList.Add(FCommands[i].Name);
  CmdList.Sort;
end;

//Private

procedure TConsole.Grow;
begin
  FCommandsSize:=2*FCommandsSize;
  SetLength(FCommands, FCommandsSize);
  ZeroMemory(@FCommands[FCommandsSize div 2], SizeOf(TConsoleCmd)*FCommandsSize div 2);
  if FCommandsSize=16 then ZeroMemory(@FCommands[0], SizeOf(TConsoleCmd)*8);
end;

function TConsole.FindCmd(Name: string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  Name:=LowerCase(Name);
  for i:=0 to FCommandsSize-1 do
    if FCommands[i].Exist and (FCommands[i].Name=Name) then
    begin
      Result:=i;
      Exit;
    end;
end;

function TConsole.GetHistory(Index: Cardinal): string;
begin
  if Index<FConsoleHistory.Count
    then Result:=FConsoleHistory[Index]
    else Result:='';
end;

function TConsole.GetHistoryCount: Cardinal;
begin
  Result:=FConsoleHistory.Count;
end;

function TConsole.GetCommandsHistory(Index: Cardinal): string;
begin
  if Index<FCommandsHistory.Count
    then Result:=FCommandsHistory[Index]
    else Result:='';
end;

function TConsole.GetCommandsHistoryCount: Cardinal;
begin
  Result:=FCommandsHistory.Count;
end;

// Protected - commands handlers

procedure TConsole.RegisterBuiltIns;
begin
  RegisterCommand('cmdlist', CmdListHelp, CmdList);
  RegisterCommand('exec', ExecHelp, Exec);
  RegisterCommand('help', HelpHelp, Help);
end;

function TConsole.CmdList(const Args: string): Boolean;
var
  CmdList: TStringList;
  i: Integer;
begin
  Result:=true;
  AddToConsole('');
  AddToConsole('Commands:');
  CmdList:=TStringList.Create;
  try
    IncrementalFind(Args, CmdList);
    for i:=0 to CmdList.Count-1 do
      AddToConsole('    '+CmdList[i]);
  finally
    FAN(CmdList);
  end;
end;

function TConsole.Exec(const Args: string): Boolean;
var
  F: TStream;
  List: TStringList;
  i: Integer;
begin
  Result:=false;
  if not Assigned(PakMan) then Exit;
  F:=PakMan.OpenFile(Args+CScriptExt, ofNoCreate);
  if F=nil then
  begin
    LogF(llError, 'Cannot execute script "%s". File not found', [Args]);
    Exit;
  end;
  try
    List:=TStringList.Create;
    List.LoadFromStream(F);
    HistoryEnabled:=false;
    for i:=0 to List.Count-1 do ExecCommand(List[i]);
  finally
    HistoryEnabled:=true;
    PakMan.CloseFile(F);
    FAN(List);
  end;
end;

function TConsole.Help(const Args: string): Boolean;
var
  Index: Integer;
  CmdName: string;
  List: TStringList;
begin
  Result:=false;
  if Args='' then
  begin
      AddToConsole('');
      List:=TStringList.Create;
      List.Text:=GenericHelp;
      for Index:=0 to List.Count-1 do
        AddToConsole(List[Index]);
      FAN(List);
      Exit;
  end;
  Index:=Pos(' ', Args);
  if Index>0
    then CmdName:=Copy(Args, 1, Index-1)
    else CmdName:=Args;
  Index:=FindCmd(CmdName);
  if Index<0
    then AddToConsole(Format('Command "%s" is not defined', [CmdName]))
    else begin
      AddToConsole('');
      List:=TStringList.Create;
      List.Text:=FCommands[Index].Help;
      for Index:=0 to List.Count-1 do
        AddToConsole(List[Index]);
      FAN(List);
    end;
  Result:=true;
end;

initialization

Console:=TConsole.Create;

finalization

FAN(Console);

end.
