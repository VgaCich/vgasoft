unit Terrain;

interface

uses
  AvL, avlUtils, dglOpenGL, OpenGLExt, Textures, avlVectors, UVBO;

type
  TTerrain=class(TObject)
  private
    function  GetHeightMap(X, Y: Word): Byte;
  protected
    FTerrainData: PByteArray;
    FVertexBuffer: PVertexArray;
    FIndexBuffer: PIntegerArray;
    FIndexLen, FTexture: Cardinal;
    FWidth, FHeight: Word;
    FHScale, FVScale: Single;
  public
    constructor Create(Data: TStream; HScale, VScale: Single);
    destructor Destroy; override;
    procedure Draw;
    function  Altitude(X, Y: Single): Single;
    procedure Morph(X, Y, Width, Height: Word; Data: array of SmallInt);
    property Width: Word read FWidth;
    property Height: Word read FHeight;
    property HeightMap[X, Y: Word]: Byte read GetHeightMap;
  end;

implementation

uses
  UGame, ULog, PakMan;

const
  TerrainID: Cardinal=$52545356;

constructor TTerrain.Create(Data: TStream; HScale, VScale: Single);
var
  ID: Cardinal;
  i, j: Cardinal;
  Tex: TStream;
begin
  if not Assigned(Data) then Exit;
  inherited Create;
  FHScale:=HScale;
  FVScale:=VScale;
  Data.Read(ID, 4);
  if (ID<>TerrainID) or (Data.Size<8) then
  begin
    Log(llError, 'Terrain.Create: Data is not terrain data');
    Exit;
  end;
  Data.Read(FWidth, 2);
  Data.Read(FHeight, 2);
  if Data.Size<8+FWidth*FHeight then
  begin
    Log(llError, 'Terrain.Create: Data is too small');
    Exit;
  end;
  GetMem(FTerrainData, FWidth*FHeight);
  Data.Read(FTerrainData^, FWidth*FHeight);
  GetMem(FVertexBuffer, SizeOf(TVertex)*FWidth*FHeight);
  for i:=0 to FWidth*FHeight-1 do
  begin
    FVertexBuffer[i].Vertex.X:=FHScale*(i mod FWidth);
    FVertexBuffer[i].Vertex.Y:=FHScale*(i div FWidth);
    FVertexBuffer[i].Vertex.Z:=FVScale*FTerrainData[i];
  end;
  FIndexLen:=2*(FWidth*FHeight-FWidth+FHeight-1);
  GetMem(FIndexBuffer, FIndexLen*SizeOf(Integer));
  for j:=0 to FHeight-1 do
  begin
    for i:=0 to FWidth-1 do
    begin
      if j<FHeight-1 then
      begin
        FIndexBuffer[2*(j*(FWidth+1)+i)]:=j*FWidth+i;
        FIndexBuffer[2*(j*(FWidth+1)+i)+1]:=(j+1)*FWidth+i;
      end;
      VectorClear(FVertexBuffer[j*FWidth+i].Normal);
      if (i>0) and (j>0) then
        FVertexBuffer[j*FWidth+i].Normal:=VectorAdd(FVertexBuffer[j*FWidth+i].Normal,
          TriangleNormal(FVertexBuffer[j*FWidth+i].Vertex,
                         FVertexBuffer[(j-1)*FWidth+i].Vertex,
                         FVertexBuffer[j*FWidth+i-1].Vertex));
      if (i<FWidth-1) and (j<FHeight-1) then
        FVertexBuffer[j*FWidth+i].Normal:=VectorAdd(FVertexBuffer[j*FWidth+i].Normal,
          TriangleNormal(FVertexBuffer[j*FWidth+i].Vertex,
                         FVertexBuffer[(j+1)*FWidth+i].Vertex,
                         FVertexBuffer[j*FWidth+i+1].Vertex));
      if (i>0) and (j<FHeight-1) then
        FVertexBuffer[j*FWidth+i].Normal:=VectorAdd(FVertexBuffer[j*FWidth+i].Normal, VectorAdd(
          TriangleNormal(FVertexBuffer[j*FWidth+i].Vertex,
                         FVertexBuffer[(j-1)*FWidth+i].Vertex,
                         FVertexBuffer[(j-1)*FWidth+i+1].Vertex),
          TriangleNormal(FVertexBuffer[j*FWidth+i].Vertex,
                         FVertexBuffer[(j-1)*FWidth+i+1].Vertex,
                         FVertexBuffer[j*FWidth+i+1].Vertex)));
      if (i<FWidth-1) and (j>0) then
        FVertexBuffer[j*FWidth+i].Normal:=VectorAdd(FVertexBuffer[j*FWidth+i].Normal, VectorAdd(
          TriangleNormal(FVertexBuffer[j*FWidth+i].Vertex,
                         FVertexBuffer[(j-1)*FWidth+i].Vertex,
                         FVertexBuffer[(j-1)*FWidth+i+1].Vertex),
          TriangleNormal(FVertexBuffer[j*FWidth+i].Vertex,
                         FVertexBuffer[(j-1)*FWidth+i+1].Vertex,
                         FVertexBuffer[j*FWidth+i+1].Vertex)));
      FVertexBuffer[j*FWidth+i].TexCoord.X:=i/32;
      FVertexBuffer[j*FWidth+i].TexCoord.Y:=j/32;  
      VectorNormalize(FVertexBuffer[j*FWidth+i].Normal);
    end;
    if j<FHeight-1 then
    begin
      FIndexBuffer[2*(j*(FWidth+1)+i)]:=(j+1)*FWidth+i-1;
      FIndexBuffer[2*(j*(FWidth+1)+i)+1]:=(j+1)*FWidth;
    end;
  end;
  Tex:=Game.PakMan.OpenFile('Grass.tga', ofNoCreate);
  try
    FTexture:=LoadTexture(Tex, tfTGA, true, GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR);
  finally
    Game.PakMan.CloseFile(Tex);
  end;
end;

destructor TTerrain.Destroy;
begin
  glDeleteTextures(1, @FTexture);
  if FTerrainData<>nil then FreeMem(FTerrainData, FWidth*FHeight);
  if FVertexBuffer<>nil then FreeMem(FVertexBuffer, SizeOf(TVertex)*FWidth*FHeight);
  if FIndexBuffer<>nil then FreeMem(FIndexBuffer, FIndexLen*SizeOf(Integer));
  inherited Destroy;
end;

procedure TTerrain.Draw;
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FTexture);
  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVertex), FVertexBuffer);
  glNormalPointer(GL_FLOAT, SizeOf(TVertex), IncPtr(FVertexBuffer, SizeOf(TVector3D)));
  glTexCoordPointer(2, GL_FLOAT, SizeOf(TVertex), IncPtr(FVertexBuffer, 2*SizeOf(TVector3D)));
  glDrawElements(GL_TRIANGLE_STRIP, FIndexLen, GL_UNSIGNED_INT, FIndexBuffer);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);
end;

function TTerrain.Altitude(X, Y: Single): Single;
begin

end;

procedure TTerrain.Morph(X, Y, Width, Height: Word; Data: array of SmallInt);
begin

end;

function TTerrain.GetHeightMap(X, Y: Word): Byte;
begin
  Result:=FTerrainData[Y*FWidth+X];
end;

end.
