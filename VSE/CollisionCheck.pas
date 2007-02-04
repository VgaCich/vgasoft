unit CollisionCheck;

interface

uses
  Windows;

function PointInRect(Point: TPoint; Rect: TRect): Boolean;

implementation

function PointInRect(Point: TPoint; Rect: TRect): Boolean;
begin
  Result:=(Point.X>=Rect.Left) and (Point.X<=Rect.Right) and
          (Point.Y>=Rect.Top) and (Point.Y<=Rect.Bottom);
end;

end.
