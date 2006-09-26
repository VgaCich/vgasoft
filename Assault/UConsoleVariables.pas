unit UConsoleVariables;

interface

uses Windows, AvL, avlUtils, UConsole, ULog;

type
  TConsoleVariables=class;
  TConsoleVariable=class
  protected
    FIndex: Integer;
    FName: string;
    FHelp: string;
  public
    constructor Create(const AName, AHelp: string);
    destructor Destroy; override;
    function  DoCommand(const Args: string): Boolean; virtual; abstract;
    property  Name: string read FName;
  end;
  TConsoleVariableRec=record
    Name: string;
    Variable: TConsoleVariable;
    Exist: Boolean;
  end;
  TConsoleVariables=class
  private
    FVariables: array of TConsoleVariableRec;
    FVariablesSize, FVariablesCount: Integer;
    FDestroying: Boolean;
    procedure Grow;
    function  GetVariableInt(Index: Integer): TConsoleVariable;
    function  GetVariableStr(const Name: string): TConsoleVariable;
    function  FindVariable(Name: string): Integer;
    procedure DeleteVariable(Index: Integer);
    function  DoSet(const Args: string): Boolean;
    function  DoVarList(const Args: string): Boolean;
    function  DoVarHelp(const Args: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function  AddVariable(Variable: TConsoleVariable): Integer;
    property  Count: Integer read FVariablesCount;
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

procedure RegisterCVI(AName, AHelp: string; Variable: PInteger; Min, Max, Default: Integer; ReadOnly: Boolean);

var
  ConsoleVariables: TConsoleVariables;

implementation

{TConsoleVariable}

constructor TConsoleVariable.Create(const AName, AHelp: string);
begin
  inherited Create;
  FName:=LowerCase(AName);
  FHelp:=AHelp;
  FIndex:=-1;
  if not Console.RegisterCommand('var_'+FName, FHelp, DoCommand)
    then LogF(llError, 'Cannot register console variable %s', [AName]);
end;

destructor TConsoleVariable.Destroy;
begin
  Console.DeleteCommand('var_'+FName);
  if FIndex>-1 then ConsoleVariables.DeleteVariable(FIndex);
  inherited Destroy;
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
  inherited Create;
  FVariablesSize:=8;
  FVariablesCount:=0;
  FDestroying:=false;
  Grow;
  S:=Console.GenericHelp;
  i:=Pos('print', S);
  i:=PosEx('print', S, i+1);
  i:=PosEx(#13, S, i)+1;
  Insert(GenericHelpAdd, S, i);
  Console.GenericHelp:=S;
  Console.RegisterCommand('set', SetHelp, DoSet);
  Console.RegisterCommand('varlist', VarListHelp, DoVarList);
  Console.RegisterCommand('varhelp', VarHelpHelp, DoVarHelp);
end;

destructor TConsoleVariables.Destroy;
var
  i: Integer;
begin
  FDestroying:=true;
  for i:=0 to FVariablesCount-1 do
    if FVariables[i].Exist then FAN(FVariables[i].Variable);
  Finalize(FVariables);
  inherited Destroy;
end;

function TConsoleVariables.AddVariable(Variable: TConsoleVariable): Integer;
begin
  Result:=-1;
  if FVariablesCount=FVariablesSize then Grow;
  FVariables[FVariablesCount].Variable:=Variable;
  FVariables[FVariablesCount].Name:=Variable.FName;
  FVariables[FVariablesCount].Exist:=true;
  Variable.FIndex:=FVariablesCount;
  Result:=FVariablesCount;
  Inc(FVariablesCount);
end;

procedure TConsoleVariables.Grow;
begin
  FVariablesSize:=2*FVariablesSize;
  SetLength(FVariables, FVariablesSize);
end;

function TConsoleVariables.GetVariableInt(Index: Integer): TConsoleVariable;
begin
  Result:=nil;
  if (Index<0) or (Index>FVariablesCount-1) then Exit;
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
  for i:=0 to FVariablesCount-1 do
    if (FVariables[i].Name=Name) and FVariables[i].Exist then
    begin
      Result:=i;
      Exit;
    end;
end;

procedure TConsoleVariables.DeleteVariable(Index: Integer);
begin
  if (Index<0) or (Index>FVariablesCount-1) then Exit;
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
    Console.AddToConsole('You must specify variable');
    Exit;
  end;
  P:=Pos(' ', Args);
  if P=0 then
  begin
    Console.AddToConsole('You must specify value');
    Exit;
  end;
  Name:=Copy(Args, 1, P-1);
  Value:=Trim(Copy(Args, P+1, MaxInt));
  V:=FindVariable(Name);
  if V>-1
    then Result:=FVariables[V].Variable.DoCommand(Value)
    else Console.AddToConsole(Format('Variable %s not exist', [Name]));
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
    for i:=0 to FVariablesCount-1 do
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
    then Console.AddToConsole(Format('Variable "%s" is not defined', [VarName]))
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
    Console.AddToConsole('Variable '+FName+' is read-only');
    Exit;
  end;
  i:=Pos(' ', Args);
  if i>0
    then S:=Copy(Args, 1, i-1)
    else S:=Args;
  i:=StrToInt(S);
  if i>FMax then
  begin
    Console.AddToConsole(Format('Value %d is larger than maximum value (%d)', [i, FMax]));
    Exit;
  end;
  if i<FMin then
  begin
    Console.AddToConsole(Format('Value %d is smaller than minimum value (%d)', [i, FMin]));
    Exit;
  end;
  FVariable^:=i;
  Result:=true;
end;

{Register procedures}

procedure RegisterCVI(AName, AHelp: string; Variable: PInteger; Min, Max, Default: Integer; ReadOnly: Boolean);
var
  CVI: TConsoleVariableInteger;
begin
  CVI:=TConsoleVariableInteger.Create(AName, AHelp, Variable, Min, Max, Default, ReadOnly);
  ConsoleVariables.AddVariable(CVI);
end;

initialization

ConsoleVariables:=TConsoleVariables.Create;

finalization;

FAN(ConsoleVariables);

end.