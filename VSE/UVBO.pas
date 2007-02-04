unit UVBO;

interface

uses
  Windows, AvL, avlUtils, dglOpenGL, avlVectors;

type
  TVertex=packed record
    Vertex: TVector3D;
    Normal: TVector3D;
    TexCoord: TVector2D;
  end;
  PVertexArray=^TVertexArray;
  TVertexArray=array[0..MaxInt div SizeOf(TVertex)-1] of TVertex;

implementation

end.
