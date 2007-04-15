unit Shaders;

interface

uses
  Windows, AvL, avlUtils, dglOpenGL;

type
  TShader=class;
  TShaderAttrib=class
  private
    FHandle: Integer;
    FShader: TShader;
  public
    constructor Create(Shader: TShader; const Name: string);
    destructor Destroy; override;
    procedure Value(x: Single); overload;
    procedure Value(x, y: Single); overload;
    procedure Value(x, y, z: Single); overload;
    property Handle: Integer read FHandle;
  end;
  TShaderUniform=class
  private
    FHandle: Integer;
    FShader: TShader;
  public
    constructor Create(Shader: TShader; const Name: string);
    destructor Destroy; override;
    procedure Value(x: Single); overload;
    procedure Value(x, y: Single); overload;
    procedure Value(x, y, z: Single); overload;
    procedure Value(i: Integer); overload;
    property Handle: Integer read FHandle;
  end;
  TShader=class
  private
    FHandle: Integer;
    FLinks: TList;
    FInfoLog: string;
    function  GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function  GetValid: Boolean;
    function  GetInfoLog(Obj: Integer): string;
  protected
    function Compile(const Prog: string; ObjType: Integer): Boolean;
    function Error(Handle: Integer; Param: DWORD): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Load(Data: TStream): Boolean;
    function AddVP(const VP: string): Boolean;
    function AddFP(const FP: string): Boolean;
    function Link: Boolean;
    function GetAttrib(const Name: string): TShaderAttrib;
    function GetUniform(const Name: string): TShaderUniform;
    property Handle: Integer read FHandle;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Valid: Boolean read GetValid;
    property InfoLog: string read FInfoLog;
  end;

implementation

uses
  ULog, UCore, UPakMan;

{TShaderAttrib}

constructor TShaderAttrib.Create(Shader: TShader; const Name: string);
begin
  inherited Create;
  FShader:=Shader;
  FHandle:=glGetAttribLocationARB(Shader.Handle, PChar(Name));
end;

destructor TShaderAttrib.Destroy;
begin
  FShader.FLinks.Remove(Self);
  inherited Destroy;
end;

procedure TShaderAttrib.Value(x: Single);
begin
  glVertexAttrib1fARB(FHandle, x);
end;

procedure TShaderAttrib.Value(x, y: Single);
begin
  glVertexAttrib2fARB(FHandle, x, y);
end;

procedure TShaderAttrib.Value(x, y, z: Single);
begin
  glVertexAttrib3fARB(FHandle, x, y, z);
end;

{TShaderUniform}

constructor TShaderUniform.Create(Shader: TShader; const Name: string);
begin
  inherited Create;
  FShader:=Shader;
  FHandle:=glGetUniformLocationARB(Shader.Handle, PChar(Name));
end;

destructor TShaderUniform.Destroy;
begin
  FShader.FLinks.Remove(Self);
  inherited Destroy;
end;

procedure TShaderUniform.Value(x: Single);
begin
  glUniform1fARB(FHandle, x);
end;

procedure TShaderUniform.Value(x, y: Single);
begin
  glUniform2fARB(FHandle, x, y);
end;

procedure TShaderUniform.Value(x, y, z: Single);
begin
  glUniform3fARB(FHandle, x, y, z);
end;

procedure TShaderUniform.Value(i: Integer);
begin
  glUniform1iARB(FHandle, i);
end;

{TShader}
{public}

constructor TShader.Create;
begin
  if not GL_ARB_shading_language_100 then Exit;
  inherited Create;
  FLinks:=TList.Create;
  FInfoLog:='';
  FHandle:=glCreateProgramObjectARB;
  if FHandle=0
    then Log(llError, 'Shader.Create: cannot create shader');
  FInfoLog:=FInfoLog+GetInfoLog(FHandle);
end;

destructor TShader.Destroy;
var
  i: Integer;
begin
  for i:=0 to FLinks.Count-1 do
    if Assigned(FLinks[i]) then TObject(FLinks[i]).Free;
  FAN(FLinks);
  glDeleteObjectARB(FHandle);
  inherited Destroy;
end;

function TShader.Load(Data: TStream): Boolean;
var
  S, Prog, LastHeader: string;
  List: TStringList;
  i: Integer;

  function Add: Boolean;
  begin
    if LastHeader<>'' then
    begin
      if LastHeader='vertex_shader' then Result:=AddVP(Prog)
      else if LastHeader='fragment_shader' then Result:=AddFP(Prog)
      else begin
        Result:=false;
        Log(llError, 'Shader.Load: unknown object header "'+LastHeader+'"');
      end;
    end
      else Result:=true;
  end;

begin
  Result:=false;
  if not Assigned(Data) then Exit;
  List:=TStringList.Create;
  try
    List.LoadFromStream(Data);
    Prog:='';
    LastHeader:='';
    for i:=0 to List.Count-1 do
    begin
      S:=Trim(List[i]);
      if (Length(S)>0) and (S[1]='!') then
      begin
        if not Add then Exit;
        LastHeader:=LowerCase(Copy(S, 2, MaxInt));
        Prog:='';
      end
        else Prog:=Prog+List[i]+#13#10;
    end;
    if not Add then Exit;
    Result:=true;
  finally
    FAN(List);
  end;
end;

function TShader.AddVP(const VP: string): Boolean;
begin
  Result:=Compile(VP, GL_VERTEX_SHADER_ARB);
end;

function TShader.AddFP(const FP: string): Boolean;
begin
  Result:=Compile(FP, GL_FRAGMENT_SHADER_ARB);
end;

function TShader.Link: Boolean;
begin
  glLinkProgramARB(FHandle);
  FInfoLog:=FInfoLog+GetInfoLog(FHandle);
  Result:=not Error(FHandle, GL_OBJECT_LINK_STATUS_ARB);
  if not Result
    then Log(llError, 'Shader.Link: cannot link shader');
end;

function TShader.GetAttrib(const Name: string): TShaderAttrib;
begin
  Result:=TShaderAttrib.Create(Self, Name);
  FInfoLog:=FInfoLog+GetInfoLog(FHandle);
  FLinks.Add(Result);
end;

function TShader.GetUniform(const Name: string): TShaderUniform;
begin
  Result:=TShaderUniform.Create(Self, Name);
  FInfoLog:=FInfoLog+GetInfoLog(FHandle);
  FLinks.Add(Result);
end;

{protected}

function TShader.Compile(const Prog: string; ObjType: Integer): Boolean;
var
  ShTemp: Integer;
  P: PChar;
begin
  Result:=false;
  if Prog='' then Exit;
  P:=PChar(Prog);
  ShTemp:=glCreateShaderObjectARB(ObjType);
  glShaderSourceARB(ShTemp, 1, @P, nil);
  FInfoLog:=FInfoLog+GetInfoLog(ShTemp);
  glCompileShaderARB(ShTemp);
  FInfoLog:=FInfoLog+GetInfoLog(ShTemp);
  if Error(ShTemp, GL_OBJECT_COMPILE_STATUS_ARB) then Exit;
  glAttachObjectARB(FHandle, ShTemp);
  FInfoLog:=FInfoLog+GetInfoLog(FHandle);
  glDeleteObjectARB(ShTemp);
  Result:=True;
end;

function TShader.Error(Handle: Integer; Param: DWORD): Boolean;
var
  Status: Integer;
begin
  glGetObjectParameterivARB(Handle, Param, @Status);
  Result:=Status=0;
end;

{private}

function TShader.GetEnabled: Boolean;
begin
  Result:=glGetHandleARB(GL_PROGRAM_OBJECT_ARB)=FHandle;
end;

procedure TShader.SetEnabled(Value: Boolean);
begin
  if Value
    then glUseProgramObjectARB(FHandle)
    else glUseProgramObjectARB(0);
end;

function TShader.GetValid: Boolean;
begin
  Result:=not Error(FHandle, GL_OBJECT_VALIDATE_STATUS_ARB);
end;

function TShader.GetInfoLog(Obj: Integer): string;
var
  LogLen, Written: Integer;
begin
  Result:='';
  glGetObjectParameterivARB(Obj, GL_OBJECT_INFO_LOG_LENGTH_ARB, @LogLen);
  if (glGetError<>GL_NO_ERROR) or (LogLen<1) then Exit;
  SetLength(Result, LogLen);
  glGetInfoLogARB(Obj, LogLen, Written, PChar(Result));
  SetLength(Result, Written);
end;

end.
