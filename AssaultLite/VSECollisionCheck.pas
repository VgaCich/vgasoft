unit VSECollisionCheck;

interface

uses
  Windows, AvL, avlUtils, avlMath, avlVectors;

type
  TCollObj=class
  public
    Transform: TMatrix4D;
  end;
  TCollRay=class(TCollObj)
  public
    Start, Dir: TVector3D;
  end;
  TCollSphere=class(TCollObj)
  public
    Center: TVector3D;
    Radius: Single;
  end;
  TCollAABB=class(TCollObj)
  public
    MinCorner, MaxCorner: TVector3D;
  end;
  TCollBox=class(TCollObj)
  public

  end;
  TCollTerrain=class(TCollObj)
  public

  end;

function PointInRect(Point: TPoint; Rect: TRect): Boolean;

implementation

function PointInRect(Point: TPoint; Rect: TRect): Boolean;
begin
  Result:=(Point.X>=Rect.Left) and (Point.X<=Rect.Right) and
          (Point.Y>=Rect.Top) and (Point.Y<=Rect.Bottom);
end;

end.
