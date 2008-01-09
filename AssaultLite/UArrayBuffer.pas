unit UArrayBuffer;

interface

uses
  Windows, AvL, avlUtils, OpenGL, oglExtensions, avlVectors;

type
  TVertex=packed record
    Vertex: TVector3D;
    Normal: TVector3D;
    TexCoord: TVector2D;
  end;
  TFaceI=packed record
    Vert1, Vert2, Vert3: Integer;
  end;
  TFaceW=packed record
    Vert1, Vert2, Vert3: Word;
  end;
  PVertexArray=^TVertexArray;
  TVertexArray=array[0..MaxInt div SizeOf(TVertex)-1] of TVertex;
  TArrayBuffer=class
  private
    FID: Cardinal;
    FTarget: GLenum;
    FData: Pointer;
    FSize: Integer;
  public
    constructor Create(UseVBO: Boolean = true);
    destructor Destroy; override;
    procedure Map(Access: GLenum);
    function  Unmap: Boolean;
    procedure Bind(Target: GLenum);
    procedure Unbind;
    procedure SetData(Data: Pointer; Size: Integer);
    function  SetSubData(SubData: Pointer; Offset, Size: Integer): Boolean;
    property Data: Pointer read FData;
    property Size: Integer read FSize;
  end;

implementation

{$IFDEF VSE_LOG}uses VSELog;{$ENDIF}

constructor TArrayBuffer.Create(UseVBO: Boolean);
begin
  inherited Create;
  if UseVBO and GL_ARB_vertex_buffer_object then glGenBuffersARB(1, @FID);
end;

destructor TArrayBuffer.Destroy;
begin
  Unmap;
  Unbind;
  if FID<>0 then glDeleteBuffersARB(1, @FID);
  if Assigned(FData) then FreeMem(FData, FSize);
  inherited Destroy;
end;

procedure TArrayBuffer.Map(Access: GLenum);
begin
  if (FID=0) or (FTarget=0) then Exit;
  FData:=glMapBufferARB(FTarget, Access);
end;

function TArrayBuffer.Unmap: Boolean;
begin
  Result:=false;
  if (FID<>0) and Assigned(FData)
    then Result:=glUnmapBufferARB(FTarget);
end;

procedure TArrayBuffer.Bind(Target: GLenum);
begin
  if FID=0 then Exit;
  Unmap;
  glBindBufferARB(Target, FID);
  FTarget:=Target;
end;

procedure TArrayBuffer.Unbind;
begin
  if (FTarget<>0) and (FID<>0) then glBindBufferARB(FTarget, 0);
end;

procedure TArrayBuffer.SetData(Data: Pointer; Size: Integer);
begin
  Unmap;
  if FID=0 then
  begin
    FreeMem(FData, FSize);
    GetMem(FData, Size);
    FSize:=Size;
    Move(Data^, FData^, Size);
  end
  else begin
    if FTarget=0 then FTarget:=GL_ARRAY_BUFFER_ARB;
    Bind(FTarget);
    glBufferDataARB(FTarget, Size, Data, GL_STATIC_DRAW_ARB);
    FData:=nil;
    FSize:=Size;
    Unbind;
  end;
end;

function TArrayBuffer.SetSubData(SubData: Pointer; Offset, Size: Integer): Boolean;
begin
  Result:=(Offset+Size)<FSize;
  if not Result then Exit;
  Unmap;
  if FID=0
    then Move(Data^, IncPtr(FData, Offset)^, Size)
  else begin
    if FTarget=0 then FTarget:=GL_ARRAY_BUFFER_ARB;
    Bind(FTarget);
    glBufferSubDataARB(FTarget, Offset, Size, Data);
    Unbind;
  end;
end;

end.
