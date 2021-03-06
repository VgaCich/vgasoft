unit VSEManagers;

interface

uses
  AvL, avlUtils, VSELogFile;

type
  TManager=class
  protected
    procedure Init; virtual; abstract;
    procedure Cleanup; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
  end;
  TManagers=class
  private
    FManagersList: TList;
    FDestroying: Boolean;
  protected
    procedure AddManager(Manager: TManager);
    procedure DeleteManager(Manager: TManager);
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitManagers;
    procedure CleanupManagers;
  end;

var
  Managers: TManagers;

implementation

{Log functions - cannot use ULog due to initialization order}

procedure LogNC(const S: string);
begin
  if not CanLog then Exit;
  WriteLn(LogFile, '['+DateTimeToStr(Now)+'] '+S);
end;

procedure LogException(const Comment: string);
begin
  LogNC('Error: Exception "'+ExceptObject.ClassName+'" at '+IntToHex(Cardinal(ExceptAddr), 8)+' with message "'+Exception(ExceptObject).Message+'" '+Comment);
end;

function RemT(const S: ShortString): string;
begin
  Result:=S;
  if Result='' then Exit;
  if UpCase(Result[1])='T' then Delete(Result, 1, 1);
end;

{TManager}

constructor TManager.Create;
begin
  inherited Create;
  if Assigned(Managers)
    then Managers.AddManager(Self)
    else raise Exception.Create('Cannot register manager '+RemT(ClassName));
end;

destructor TManager.Destroy;
begin
  if Assigned(Managers) then Managers.DeleteManager(Self);
  inherited Destroy;
end;

{TManagers}

constructor TManagers.Create;
begin
  LogNC('Creating managers system');
  inherited Create;
  FDestroying:=False;
  FManagersList:=TList.Create;
end;

destructor TManagers.Destroy;
var
  i: Integer;
  ManName: string;
begin
  FDestroying:=true;
  LogNC('Destroying managers system');
  for i:=FManagersList.Count-1 downto 0 do
  try
    ManName:=RemT(TObject(FManagersList[i]).ClassName);
    TManager(FManagersList[i]).Destroy;
  except
    LogException('while destroying manager '+ManName);
  end;
  LogNC(IntToStr(FManagersList.Count)+' managers destroyed');
  FAN(FManagersList);
  inherited Destroy;
end;

procedure TManagers.AddManager(Manager: TManager);
begin
  if FDestroying or (FManagersList.IndexOf(Manager)>=0) then Exit;
  FManagersList.Add(Manager);
end;

procedure TManagers.DeleteManager(Manager: TManager);
begin
  if FDestroying then Exit;
  FManagersList.Remove(Manager);
end;

procedure TManagers.InitManagers;
var
  i: Integer;
begin
  if FDestroying then Exit;
  for i:=0 to FManagersList.Count-1 do
  try
    TManager(FManagersList[i]).Init;
  except
    LogException('while initializing manager '+RemT(TManager(FManagersList[i]).ClassName));
  end;
end;

procedure TManagers.CleanupManagers;
var
  i: Integer;
begin
  if FDestroying then Exit;
  for i:=FManagersList.Count-1 downto 0 do
  try
    TManager(FManagersList[i]).Cleanup;
  except
    LogException('while cleanup manager '+RemT(TManager(FManagersList[i]).ClassName));
  end;
end;

initialization
  Managers:=TManagers.Create;

finalization
  FAN(Managers);

end.
