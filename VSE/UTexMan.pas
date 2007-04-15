unit UTexMan;

interface

uses
  Windows, AvL, avlUtils, dglOpenGL, UManagers, Textures;

type
  TTexMan=class;
  TTexture=class
  private
    FHandle: TGLuint;
    FTarget: TGLenum; 
    FRefCount: Cardinal;
    function  GetValid: Boolean;
  protected
    Next, Prev: TTexture;
    procedure AddRef;
  public
    constructor Create(Handle: TGLuint; Target: TGLenum);
    destructor Destroy; override;
    procedure Bind;
    property Handle: Cardinal read FHandle;
    property Valid: Boolean read GetValid;
  end;

  TTexMan=class(TManager)
  private
    FTexRoot: TTexture; //double-linked cyclic list
    procedure ClearList;
    procedure AddTex(Tex: TTexture);
  protected
    procedure Init; override;
    procedure Cleanup; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UnbindTexture(Target: TGLenum);
  end;

var
  TexMan: TTexMan;

implementation

uses ULog;

{TTexture}

constructor TTexture.Create(Handle: TGLuint; Target: TGLenum);
begin
  inherited Create;
  FRefCount:=1;
  FHandle:=Handle;
  FTarget:=Target;
end;

destructor TTexture.Destroy;
begin
  Dec(FRefCount);
  if FRefCount>0 then Exit;
  Prev.Next:=Next;
  Next.Prev:=Prev;
  if Valid then glDeleteTextures(1, @FHandle);
  inherited Destroy;
end;

procedure TTexture.AddRef;
begin
  Inc(FRefCount);
end;

function TTexture.GetValid: Boolean;
begin
  Result:=glIsTexture(FHandle);
end;

procedure TTexture.Bind;
begin
  glBindTexture(FTarget, FHandle);
end;

{TTexMan}

constructor TTexMan.Create;
begin
  LogNC(llInfo, 'TexMan: Create');
  inherited Create;
  FTexRoot:=TTexture.Create(0, GL_TEXTURE_2D);
  ClearList;
end;

destructor TTexMan.Destroy;
begin
  LogNC(llInfo, 'TexMan: Destroy');
  while FTexRoot.Next<>FTexRoot do FTexRoot.Next.Free;
  FAN(FTexRoot);
  if TexMan=Self then TexMan:=nil;
  inherited Destroy;
end;

procedure TTexMan.Init;
begin
  Log(llInfo, 'TexMan: Initialize');
  ClearList;
end;

procedure TTexMan.Cleanup;
begin
  Log(llInfo, 'TexMan: Cleanup');
  while FTexRoot.Next<>FTexRoot do FTexRoot.Next.Free;
  ClearList;
end;

procedure TTexMan.ClearList;
begin
  FTexRoot.Next:=FTexRoot;
  FTexRoot.Prev:=FTexRoot;
end;

procedure TTexMan.AddTex(Tex: TTexture);
begin
  Tex.Next:=FTexRoot;
  Tex.Prev:=FTexRoot.Prev;
  FTexRoot.Prev:=Tex;
  Tex.Prev.Next:=Tex;
end;

procedure TTexMan.UnbindTexture(Target: TGLenum);
begin
  glBindTexture(Target, 0);
end;

initialization
  TexMan:=TTexMan.Create;

end.