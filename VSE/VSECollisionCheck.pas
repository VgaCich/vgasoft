unit VSECollisionCheck;

interface

uses
  Windows;

function PointInRect(Point: TPoint; Rect: TRect): Boolean;
function PointInCircle(Point: TPoint; Center: TPoint; Radius: Integer): Boolean;

implementation

function PointInRect(Point: TPoint; Rect: TRect): Boolean;
begin
  Result:=(Point.X>=Rect.Left) and (Point.X<=Rect.Right) and
          (Point.Y>=Rect.Top) and (Point.Y<=Rect.Bottom);
end;

function PointInCircle(Point: TPoint; Center: TPoint; Radius: Integer): Boolean;
begin
  Result:=Sqr(Center.X-Point.X)+Sqr(Center.Y-Point.Y)<=Sqr(Radius);
end;

end.
