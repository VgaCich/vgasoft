unit VSETexMan;

interface

uses
  Windows, AvL, avlUtils, avlClasses, dglOpenGL, VSEManagers, VSETexLoader;

type
  TTexMan=class;
  TTextureFilter=(tfNone, tfBilinear, tfTrilinear, tfAnisotropy);
  TTexture=class(TDLCListItem)
  private
    FHandle: TGLuint;
    FTarget: TGLenum;
    FGroup: Cardinal;
    FName: string;
    function  GetValid: Boolean;
    procedure SetActive(Index: Integer; Value: Boolean);
    function  GetActive(Index: Integer): Boolean;
    procedure SetFilter(Value: TTextureFilter);
    function  GetFilter: TTextureFilter;
    procedure SetAnisoLevel(Value: Integer);
    function  GetAnisoLevel: Integer;
    procedure SetMipMap(Value: Boolean);
    function  GetMipMap: Boolean;
    procedure SetClamp(Value: Boolean);
    function  GetClamp: Boolean;
    procedure SetLOD(Value: Integer);
    function  GetLOD: Integer;
  protected
    procedure Mark;
    function  IsMarked: Boolean;
  public
    constructor Create(PrevItem: TTexture; Handle: TGLuint; Target: TGLenum; const Name: string; Group: Cardinal); overload;
    constructor Create(PrevItem: TTexture; Data: TTextureData; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer); overload;
    constructor Create(PrevItem: TTexture; const FileName: string; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer); overload;
    constructor Create(PrevItem: TTexture; const Name: string; Data: TStream; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer); overload;
    destructor Destroy; override;
    property Active[Index: Integer]: Boolean read GetActive write SetActive; default;
    property Filter: TTextureFilter read GetFilter write SetFilter;
    property AnisoLevel: Integer read GetAnisoLevel write SetAnisoLevel;
    property MipMap: Boolean read GetMipMap write SetMipMap;
    property Clamp: Boolean read GetClamp write SetClamp;
    property Handle: Cardinal read FHandle;
    property Valid: Boolean read GetValid;
    property Name: string read FName;
    property Group: Cardinal read FGroup write FGroup;
    property LOD: Integer read GetLOD write SetLOD;
  end;

  TTexMan=class(TManager)
  private
    FTexRoot, UnloadTemp: TTexture;
    FDefaultFilter: array[0..31] of TTextureFilter;
    FDefaultAnisoLevel: array[0..31] of Integer;
    FDefaultMipMap: array[0..31] of Boolean;
    FDefaultClamp: array[0..31] of Boolean;
    FDefaultLOD: array[0..31] of Integer;
  protected
    procedure Init; override;
    procedure Cleanup; override;
    function  CheckName(Texture: TDLCListItem; Name: Integer): Boolean;
    function  UpdateMark(Texture: TDLCListItem; Groups: Integer): Boolean;
    function  UpdateUnload(Texture: TDLCListItem; Groups: Integer): Boolean;
    function  GetDefaultFilter(Group: Cardinal): TTextureFilter;
    procedure SetDefaultFilter(Group: Cardinal; Value: TTextureFilter);
    function  GetDefaultAnisoLevel(Group: Cardinal): Integer;
    procedure SetDefaultAnisoLevel(Group: Cardinal; Value: Integer);
    function  GetDefaultMipMap(Group: Cardinal): Boolean;
    procedure SetDefaultMipMap(Group: Cardinal; Value: Boolean);
    function  GetDefaultClamp(Group: Cardinal): Boolean;
    procedure SetDefaultClamp(Group: Cardinal; Value: Boolean);
    function  GetDefaultLOD(Group: Cardinal): Integer;
    procedure SetDefaultLOD(Group: Cardinal; Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UnbindTexture(Target: TGLenum);
    function  LoadTexture(FileName: string; Group: Cardinal): TTexture; overload;
    function  LoadTexture(FileName: string; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer): TTexture; overload;
    function  LoadTexture(Name: string; Data: TStream; Group: Cardinal): TTexture; overload;
    function  LoadTexture(Name: string; Data: TStream; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer): TTexture; overload;
    function  LoadTexture(Name: string): TTexture; overload;
    function  LoadTexturesList(TexList: TStringList; Group: Cardinal): TTexture; overload;
    function  LoadTexturesList(TexList: TStringList; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer): TTexture; overload;
    procedure UpdateGroupsBegin(Groups: Cardinal);
    procedure UpdateGroupsEnd(Groups: Cardinal);
    procedure SetGroupFilter(Groups: Cardinal; Filter: TTextureFilter; UpdateDefault: Boolean);
    procedure SetGroupAnisoLevel(Groups: Cardinal; AnisoLevel: Integer; UpdateDefault: Boolean);
    procedure SetGroupLOD(Groups: Cardinal; LOD: Integer; UpdateDefault: Boolean);
    property  DefaultFilter[Group: Cardinal]: TTextureFilter read GetDefaultFilter write SetDefaultFilter;
    property  DefaultAnisoLevel[Group: Cardinal]: Integer read GetDefaultAnisoLevel write SetDefaultAnisoLevel;
    property  DefaultMipMap[Group: Cardinal]: Boolean read GetDefaultMipMap write SetDefaultMipMap;
    property  DefaultClamp[Group: Cardinal]: Boolean read GetDefaultClamp write SetDefaultClamp;
    property  DefaultLOD[Group: Cardinal]: Integer read GetDefaultLOD write SetDefaultLOD;
  end;

var
  TexMan: TTexMan;

procedure FreeTex(var Tex: TTexture);

implementation

uses VSELog, VSEPakMan;

{TTexture}

constructor TTexture.Create(PrevItem: TTexture; Handle: TGLuint; Target: TGLenum; const Name: string; Group: Cardinal);
begin
  inherited Create(PrevItem);
  FHandle:=Handle;
  FTarget:=Target;
  FName:=LowerCase(Name);
  FGroup:=Group;

end;

constructor TTexture.Create(PrevItem: TTexture; Data: TTextureData; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer);
begin
  inherited Create(PrevItem);

end;

constructor TTexture.Create(PrevItem: TTexture; const FileName: string; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer);
var
  F: TStream;
begin
  F:=PakMan.OpenFile(FileName, pmNoCreate);
  try
    Create(PrevItem, FileName, F, Group, Filter, AnisoLevel, MipMap, Clamp, LOD);
  finally
    FAN(F);
  end;
end;

constructor TTexture.Create(PrevItem: TTexture; const Name: string; Data: TStream; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer);
begin
  inherited Create(PrevItem);
  //FHandle:=
  //FTarget:=
  FName:=LowerCase(Name);
  FGroup:=Group;
end;

destructor TTexture.Destroy;
begin
  if Valid then glDeleteTextures(1, @FHandle);
  inherited Destroy;
end;

procedure TTexture.Mark;
begin
  FRefCount:=0;
end;

function TTexture.IsMarked: Boolean;
begin
  Result:=FRefCount<=0;
end;

function TTexture.GetValid: Boolean;
begin
  Result:=glIsTexture(FHandle);
end;

procedure TTexture.SetActive(Index: Integer; Value: Boolean);
begin

end;

function TTexture.GetActive(Index: Integer): Boolean;
begin

end;

procedure TTexture.SetFilter(Value: TTextureFilter);
begin

end;

function TTexture.GetFilter: TTextureFilter;
begin

end;

procedure TTexture.SetAnisoLevel(Value: Integer);
begin

end;

function TTexture.GetAnisoLevel: Integer;
begin

end;

procedure TTexture.SetMipMap(Value: Boolean);
begin

end;

function TTexture.GetMipMap: Boolean;
begin

end;

procedure TTexture.SetClamp(Value: Boolean);
begin

end;

function TTexture.GetClamp: Boolean;
begin

end;

procedure TTexture.SetLOD(Value: Integer);
begin

end;

function TTexture.GetLOD: Integer;
begin

end;

{TTexMan}

constructor TTexMan.Create;
begin
  LogNC(llInfo, 'TexMan: Create');
  inherited Create;
  FTexRoot:=TTexture.Create(nil, 0, GL_TEXTURE_2D, '__nulltex', 0);
end;

destructor TTexMan.Destroy;
begin
  LogNC(llInfo, 'TexMan: Destroy');
  FTexRoot.ClearList;
  FAN(FTexRoot);
  if TexMan=Self then TexMan:=nil;
  inherited Destroy;
end;

procedure TTexMan.Init;
begin
  Log(llInfo, 'TexMan: Initialize');
  FTexRoot.ResetList;
end;

procedure TTexMan.Cleanup;
begin
  Log(llInfo, 'TexMan: Cleanup');
  FTexRoot.ClearList;
end;

procedure TTexMan.UnbindTexture(Target: TGLenum);
begin
  glBindTexture(Target, 0);
end;

function TTexMan.LoadTexture(FileName: string; Group: Cardinal): TTexture;
begin
  Result:=LoadTexture(FileName, Group, DefaultFilter[Group], DefaultAnisoLevel[Group], DefaultMipMap[Group], DefaultClamp[Group], DefaultLOD[Group]);
end;

function TTexMan.LoadTexture(FileName: string; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer): TTexture;
begin
  FileName:=LowerCase(FileName);
  Result:=TTexture(FTexRoot.FindItem(CheckName, Integer(FileName)));
  if Assigned(Result)
    then Result.AddRef
    else Result:=TTexture.Create(FTexRoot, FileName, Group, Filter, AnisoLevel, MipMap, Clamp, LOD);
  if Result.Group<>Group then Result.Group:=Result.Group or Group;
end;

function TTexMan.LoadTexture(Name: string; Data: TStream; Group: Cardinal): TTexture;
begin
  Result:=LoadTexture(Name, Data, Group, DefaultFilter[Group], DefaultAnisoLevel[Group], DefaultMipMap[Group], DefaultClamp[Group], DefaultLOD[Group]);
end;

function TTexMan.LoadTexture(Name: string; Data: TStream; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer): TTexture;
begin
  Name:=LowerCase(Name);
  Result:=TTexture(FTexRoot.FindItem(CheckName, Integer(Name)));
  if Assigned(Result)
    then Result.AddRef
    else Result:=TTexture.Create(FTexRoot, Name, Data, Group, Filter, AnisoLevel, MipMap, Clamp, LOD);
  if Result.Group<>Group then Result.Group:=Result.Group or Group;
end;

function TTexMan.LoadTexture(Name: string): TTexture;
begin
  Name:=LowerCase(Name);
  Result:=TTexture(FTexRoot.FindItem(CheckName, Integer(Name)));
  if Assigned(Result) then Result.AddRef;
end;

function TTexMan.LoadTexturesList(TexList: TStringList; Group: Cardinal): TTexture;
begin
  Result:=LoadTexturesList(TexList, Group, DefaultFilter[Group], DefaultAnisoLevel[Group], DefaultMipMap[Group], DefaultClamp[Group], DefaultLOD[Group]);
end;

function TTexMan.LoadTexturesList(TexList: TStringList; Group: Cardinal; Filter: TTextureFilter; AnisoLevel: Integer; MipMap, Clamp: Boolean; LOD: Integer): TTexture;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to TexList.Count-1 do
    Result:=LoadTexture(TexList[i], Group, Filter, AnisoLevel, MipMap, Clamp, LOD);
end;

procedure TTexMan.UpdateGroupsBegin(Groups: Cardinal);
begin
  FTexRoot.FindItem(UpdateMark, Integer(Groups));
end;

procedure TTexMan.UpdateGroupsEnd(Groups: Cardinal);
begin
  FTexRoot.FindItem(UpdateUnload, Integer(Groups));
  FAN(UnloadTemp);
end;

procedure TTexMan.SetGroupFilter(Groups: Cardinal; Filter: TTextureFilter; UpdateDefault: Boolean);
var
  Tex: TTexture;
begin
  if UpdateDefault then DefaultFilter[Groups]:=Filter;
  Tex:=FTexRoot;
  while Tex.Next<>FTexRoot do
  begin
    if (Tex.Group and Groups)<>0 then Tex.Filter:=Filter;
    Tex:=TTexture(Tex.Next);
  end;
end;

procedure TTexMan.SetGroupAnisoLevel(Groups: Cardinal; AnisoLevel: Integer; UpdateDefault: Boolean);
var
  Tex: TTexture;
begin
  if AnisoLevel<0 then Exit;
  if UpdateDefault then DefaultAnisoLevel[Groups]:=AnisoLevel;
  Tex:=FTexRoot;
  while Tex.Next<>FTexRoot do
  begin
    if (Tex.Group and Groups)<>0 then Tex.AnisoLevel:=AnisoLevel;
    Tex:=TTexture(Tex.Next);
  end;
end;

procedure TTexMan.SetGroupLOD(Groups: Cardinal; LOD: Integer; UpdateDefault: Boolean);
var
  Tex: TTexture;
begin
  if LOD<0 then Exit;
  if UpdateDefault then DefaultLOD[Groups]:=LOD;
  Tex:=FTexRoot;
  while Tex.Next<>FTexRoot do
  begin
    if (Tex.Group and Groups)<>0 then Tex.LOD:=LOD;
    Tex:=TTexture(Tex.Next);
  end;
end;

function TTexMan.CheckName(Texture: TDLCListItem; Name: Integer): Boolean;
begin
  Result:=TTexture(Texture).Name=string(Name);
end;

function TTexMan.UpdateMark(Texture: TDLCListItem; Groups: Integer): Boolean;
begin
  Result:=false;
  if (TTexture(Texture).Group and Groups)<>0 then TTexture(Texture).Mark;
end;

function TTexMan.UpdateUnload(Texture: TDLCListItem; Groups: Integer): Boolean;
begin
  Result:=false;
  FAN(UnloadTemp);
  if ((TTexture(Texture).Group and Groups)<>0) and (TTexture(Texture).IsMarked)
    then UnloadTemp:=TTexture(Texture);
end;

function TTexMan.GetDefaultFilter(Group: Cardinal): TTextureFilter;
var
  i: Integer;
begin
  Result:=tfNone;
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0 then
      if FDefaultFilter[i]>Result then Result:=FDefaultFilter[i];
end;

procedure TTexMan.SetDefaultFilter(Group: Cardinal; Value: TTextureFilter);
var
  i: Integer;
begin
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0
      then FDefaultFilter[i]:=Value;
end;

function TTexMan.GetDefaultAnisoLevel(Group: Cardinal): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0 then
      if FDefaultAnisoLevel[i]>Result then Result:=FDefaultAnisoLevel[i];
end;

procedure TTexMan.SetDefaultAnisoLevel(Group: Cardinal; Value: Integer);
var
  i: Integer;
begin
  if Value<0 then Exit;
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0
      then FDefaultAnisoLevel[i]:=Value;
end;

function TTexMan.GetDefaultMipMap(Group: Cardinal): Boolean;
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0 then
    begin
      Result:=FDefaultMipMap[i];
      if Result then Exit;
    end;
end;

procedure TTexMan.SetDefaultMipMap(Group: Cardinal; Value: Boolean);
var
  i: Integer;
begin
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0
      then FDefaultMipMap[i]:=Value;
end;

function TTexMan.GetDefaultClamp(Group: Cardinal): Boolean;
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0 then
    begin
      Result:=FDefaultClamp[i];
      Exit;
    end;
end;

procedure TTexMan.SetDefaultClamp(Group: Cardinal; Value: Boolean);
var
  i: Integer;
begin
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0
      then FDefaultClamp[i]:=Value;
end;

function TTexMan.GetDefaultLOD(Group: Cardinal): Integer;
var
  i: Integer;
begin
  Result:=MaxInt;
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0 then
      if FDefaultLOD[i]<Result then Result:=FDefaultLOD[i];
end;

procedure TTexMan.SetDefaultLOD(Group: Cardinal; Value: Integer);
var
  i: Integer;
begin
  if Value<0 then Exit;
  for i:=0 to 31 do
    if (Group and (1 shl i))<>0
      then FDefaultLOD[i]:=Value;
end;

procedure FreeTex(var Tex: TTexture);
begin
  Tex.Release;
  Tex:=nil;
end;

initialization
  TexMan:=TTexMan.Create;

end.