unit VSECollisionCheck;

interface

uses
  Windows, AvL, avlUtils, avlMath, avlVectors;

type
  TCollisionInfo=record
  end;
  TCollisionObject=class
  protected
    function DoCollisionCheck(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean; virtual; abstract; //Implements collision checking, return true if check with passed type of object implemented, false otherwise
  public
    Transform: TMatrix4D;
    function CheckCollision(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean; //Check for collision with Obj, check result returned in CollInfo, returns false if collision with passed type of object not implemented 
  end;
  TCollisionRay=class(TCollisionObject)
  public
    Start, Dir: TVector3D;
  end;
  TCollisionSphere=class(TCollisionObject)
  public
    Center: TVector3D;
    Radius: Single;
  end;
  TCollisionAABB=class(TCollisionObject)
  public
    MinCorner, MaxCorner: TVector3D;
  end;
  TCollisionBox=class(TCollisionObject)
  public

  end;
  TCollisionTerrain=class(TCollisionObject)
  public

  end;

function PointInRect(Point: TPoint; Rect: TRect): Boolean;

implementation

function TCollisionObject.CheckCollision(Obj: TCollisionObject; var CollInfo: TCollisionInfo): Boolean;
begin
  Result:=DoCollisionCheck(Obj, CollInfo);
  if not Result then Result:=Obj.DoCollisionCheck(Self, CollInfo);
end;

function PointInRect(Point: TPoint; Rect: TRect): Boolean;
begin
  Result:=(Point.X>=Rect.Left) and (Point.X<=Rect.Right) and
          (Point.Y>=Rect.Top) and (Point.Y<=Rect.Bottom);
end;

end.
