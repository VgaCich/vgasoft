unit VSEConsoleVariables;

interface

uses Windows, AvL, avlUtils, VSEManagers, VSEConsole, VSELog;

type
  TConsoleVariables=class;
  TConsoleVariable=class
  protected
    FName: string;
    FHelp: string;
    FOnChange: TOnEvent;
    FIndex: Integer;
    procedure Change;
  public
    constructor Create(const AName, AHelp: string);
    destructor Destroy; override;
    function  DoCommand(const Args: string): Boolean; virtual; abstract;
    property  Name: string read FName;
    property  OnChange: TOnEvent read FOnChange write FOnChange;
  end;
  TConsoleVariableRec=record
    Name: string;
    Variable: TConsoleVariable;
    Exist: Boolean;
  end;
  TConsoleVariables=class(TManager)
  private
    FVariables: array of TConsoleVariableRec;
    FDestroying: Boolean;
    procedure Grow;
    function  GetVariableInt(Index: Integer): TConsoleVariable;
    function  GetVariableStr(const Name: string): TConsoleVariable;
    function  FindVariable(Name: string): Integer;
    function  AddVariable(Variable: TConsoleVariable): Integer;
    procedure UnregisterVariable(Index: Integer);
    function  DoSet(const Args: string): Boolean;
    function  DoVarList(const Args: string): Boolean;
    function  DoVarHelp(const Args: string): Boolean;
  protected
    procedure Init; override;
    procedure Cleanup; override;
  public
    constructor Create;
    destructor Destroy; override;
    property  Variable[Index: Integer]: TConsoleVariable read GetVariableInt; default;
    property  VarByName[const Name: string]: TConsoleVariable read GetVariableStr;
  end;
  TConsoleVariableInteger=class(TConsoleVariable)
  private
    FVariable: PInteger;
    FMin, FMax, FDefault: Integer;
    FReadOnly: Boolean;
  public
    constructor Create(const AName, AHelp: string; Variable: PInteger; Min, Max, Default: Integer; ReadOnly: Boolean);
    function  DoCommand(const Args: string): Boolean; override;
  end;

var
  ConsoleVariables: TConsoleVariables;

implementation

{TConsoleVariable}

constructor TConsoleVariable.Create(const AName, AHelp: string);
begin
  inherited Create;
  FName:=LowerCase(AName);
  FHelp:=AHelp;
  if not Console.RegisterCommand('var_'+FName, FHelp, DoCommand)
    then LogF(llError, 'Cannot register console command var_%s', [AName]);
  FIndex:=ConsoleVariables.AddVariable(Self);
  if FIndex=-1 then LogF(llError, 'Cannot register console variable %s', [AName]);
end;

destructor TConsoleVariable.Destroy;
begin
  Console.UnregisterCommand('var_'+FName);
  ConsoleVariables.UnregisterVariable(FIndex);
  inherited Destroy;
end;

procedure TConsoleVariable.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{TConsoleVariables}

const
  SetHelp='set <variable> <value>'+#13+
          '  Sets variable to value';
  VarListHelp='varlist <prefix>'#13+
              '  Shows list of variables, starts with prefix'#13+
              'varlist'#13+
              '  Shows list of all variables';
  VarHelpHelp='varhelp <variable>'#13+
              '  Shows help about variable'#13+
              'varhelp'#13+
              '  Shows generic variables help';
  GenericHelpAdd='print "'+ConsoleCommandPrefix+'varhelp" for help about variables'#13;
  GenericVarHelp='Variables may be accessed by "set" command or by'#13+
                 '"var_*" commands, where * is variable name'#13+
                 '"var_*" commands syntax:'#13+
                 'var_*'#13+
                 '  Shows variable * value and info'#13+
                 'var_* <NewValue>'#13+
                 '  Sets variable * to NewValue'#13+
                 #13+
                 'print "'+ConsoleCommandPrefix+'varhelp <variable>" for help about variable'#13+
                 'print "'+ConsoleCommandPrefix+'help set" for help about "set" command';

constructor TConsoleVariables.Create;
var
  S: string;
  i: Integer;
begin
  LogNC(llInfo, 'ConsoleVariables: Create');
  inherited Create;
  SetLength(FVariables, 8);
  ZeroMemory(@FVariables[0], SizeOf(TConsoleVariableRec)*8);
  FDestroying:=false;
  S:=Console.GenericHelp;
  i:=Pos('print', S);
  i:=PosEx('print', S, i+1);
  i:=PosEx(#13, S, i)+1;
  Insert(GenericHelpAdd, S, i);
  Console.GenericHelp:=S;
  Console.RegisterCommand('set', SetHelp, DoSet, true);
  Console.RegisterCommand('varlist', VarListHelp, DoVarList, true);
  Console.RegisterCommand('varhelp', VarHelpHelp, DoVarHelp, true);
end;

destructor TConsoleVariables.Destroy;
var
  i: Integer;
begin
  LogNC(llInfo, 'ConsoleVariables: Destroy');
  FDestroying:=true;
  if ConsoleVariables=Self then ConsoleVariables:=nil;
  for i:=0 to High(FVariables) do
    if FVariables[i].Exist then
    try
      FVariables[i].Variable.Free;
    except
      LogException('in console variable '+FVariables[i].Name+'.Free');
    end;
  Finalize(FVariables);
  Console.UnregisterCommand('set');
  Console.UnregisterCommand('varlist');
  Console.UnregisterCommand('varhelp');
  inherited Destroy;
end;

procedure TConsoleVariables.Init;
begin
  Log(llInfo, 'ConsoleVariables: Initialize');
end;

procedure TConsoleVariables.Cleanup;
var
  i: Integer;
begin
  Log(llInfo, 'ConsoleVariables: Cleanup');
  for i:=0 to High(FVariables) do FVariables[i].Variable.Free;
  Finalize(FVariables);
  SetLength(FVariables, 8);
  ZeroMemory(@FVariables[0], SizeOf(TConsoleVariableRec)*8);
end;

function TConsoleVariables.AddVariable(Variable: TConsoleVariable): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FVariables) do
    if not FVariables[i].Exist then
    begin
      Result:=i;
      Break;
    end;
  if Result=-1 then
  begin
    Result:=Length(FVariables);
    Grow;
  end;
  FVariables[Result].Variable:=Variable;
  FVariables[Result].Name:=Variable.FName;
  FVariables[Result].Exist:=true;
end;

procedure TConsoleVariables.Grow;
begin
  SetLength(FVariables, 2*Length(FVariables));
  ZeroMemory(@FVariables[Length(FVariables) div 2], SizeOf(TConsoleVariableRec)*Length(FVariables) div 2);
end;

function TConsoleVariables.GetVariableInt(Index: Integer): TConsoleVariable;
begin
  Result:=nil;
  if (Index<0) or (Index>High(FVariables)) then Exit;
  if FVariables[Index].Exist then Result:=FVariables[Index].Variable;
end;

function TConsoleVariables.GetVariableStr(const Name: string): TConsoleVariable;
begin
  Result:=GetVariableInt(FindVariable(Name));
end;

function TConsoleVariables.FindVariable(Name: string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  Name:=LowerCase(Name);
  for i:=0 to High(FVariables) do
    if (FVariables[i].Name=Name) and FVariables[i].Exist then
    begin
      Result:=i;
      Exit;
    end;
end;

procedure TConsoleVariables.UnregisterVariable(Index: Integer);
begin
  if (Index<0) or (Index>High(FVariables)) then Exit;
  FVariables[Index].Variable:=nil;
  FVariables[Index].Exist:=false;
end;

function TConsoleVariables.DoSet(const Args: string): Boolean;
var
  Name, Value: string;
  P, V: Integer;
begin
  Result:=false;
  if Args=''then
  begin
    Console.AddToConsole('^1Error: You must specify variable');
    Exit;
  end;
  P:=Pos(' ', Args);
  if P=0 then
  begin
    Console.AddToConsole('^1Error: You must specify value');
    Exit;
  end;
  Name:=Copy(Args, 1, P-1);
  Value:=Trim(Copy(Args, P+1, MaxInt));
  V:=FindVariable(Name);
  if V>-1 then
    try
      Result:=FVariables[V].Variable.DoCommand(Value)
    except
      LogException('in console variable '+FVariables[V].Name+'='+Value);
    end
    else Console.AddToConsole(Format('^1Error: Variable %s not exist', [Name]));
end;

function TConsoleVariables.DoVarList(const Args: string): Boolean;
var
  i, PrefixLen: Integer;
  Prefix: string;
  List: TStringList;
begin
  Result:=true;
  Console.AddToConsole('');
  Console.AddToConsole('Variables:');
  Prefix:=LowerCase(Args);
  PrefixLen:=Length(Prefix);
  List:=TStringList.Create;
  try
    for i:=0 to High(FVariables) do
      if FVariables[i].Exist and (Copy(FVariables[i].Name, 1, PrefixLen)=Prefix)
        then List.Add(FVariables[i].Name);
    List.Sort;
    for i:=0 to List.Count-1 do
      Console.AddToConsole('    '+List[i]);
  finally
    FAN(List);
  end;
end;

function TConsoleVariables.DoVarHelp(const Args: string): Boolean;
var
  Index: Integer;
  VarName: string;
  List: TStringList;
begin
  Result:=false;
  if Args='' then
  begin
      Console.AddToConsole('');
      List:=TStringList.Create;
      List.Text:=GenericVarHelp;
      for Index:=0 to List.Count-1 do
        Console.AddToConsole(List[Index]);
      FAN(List);
      Exit;
  end;
  Index:=Pos(' ', Args);
  if Index>0
    then VarName:=Copy(Args, 1, Index-1)
    else VarName:=Args;
  Index:=FindVariable(VarName);
  if Index<0
    then Console.AddToConsole(Format('^1Error: Variable "%s" is not defined', [VarName]))
    else begin
      Console.AddToConsole('');
      List:=TStringList.Create;
      List.Text:=FVariables[Index].Variable.FHelp;
      for Index:=0 to List.Count-1 do
        Console.AddToConsole(List[Index]);
      FAN(List);
    end;
  Result:=true;
end;

{TConsoleVariableInteger}

constructor TConsoleVariableInteger.Create(const AName, AHelp: string; Variable: PInteger;
  Min, Max, Default: Integer; ReadOnly: Boolean);
begin
  inherited Create(AName, AHelp);
  FVariable:=Variable;
  FMin:=Min;
  FMax:=Max;
  FDefault:=Default;
  FReadOnly:=ReadOnly;
end;

function TConsoleVariableInteger.DoCommand(const Args: string): Boolean;
var
  S: string;
  i: Integer;
begin
  Result:=false;
  if Args='' then
  begin
    if FReadOnly
      then Console.AddToConsole(Format('%s=%d default=%d', [FName, FVariable^, FDefault]))
      else Console.AddToConsole(Format('%s=%d min=%d max=%d default=%d',
        [FName, FVariable^, FMin, FMax, FDefault]));
    Result:=true;
    Exit;
  end;
  if FReadOnly then
  begin
    Console.AddToConsole('^1Error: Variable '+FName+' is read-only');
    Exit;
  end;
  i:=Pos(' ', Args);
  if i>0
    then S:=Copy(Args, 1, i-1)
    else S:=Args;
  i:=StrToInt(S);
  if i>FMax then
  begin
    Console.AddToConsole(Format('^1Error: Value %d is larger than maximum value (%d)', [i, FMax]));
    Exit;
  end;
  if i<FMin then
  begin
    Console.AddToConsole(Format('^1Error: Value %d is smaller than minimum value (%d)', [i, FMin]));
    Exit;
  end;
  FVariable^:=i;
  Result:=true;
  Change;
end;

initialization

ConsoleVariables:=TConsoleVariables.Create;

end.
