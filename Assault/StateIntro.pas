unit StateIntro;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures,
  VSEGameStates;

type
  TStateIntro=class(TGameState)
  private
    FTime: Cardinal;
    FSegs: array [0..2] of Integer;
    procedure DrawSegs(X, Y: Single; Segs: Integer);
  protected
    function GetName: string; override;
  public
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
  end;

implementation

uses VSECore, VSEInit;

const
  Chars: array[0..36] of Integer=(
    $03F, $006, $05B, $04F, $066, $06D, $07D, $007, $07F, $06F,
    $0D6, $039, $079, $071, $03D, $076, $00E, $4B0, $038, $136,
    $073, $473, $2C0, $03E, $426, $07C, $058, $05E, $074, $004,
    $054, $05C, $050, $01C, $404, $600, $248);

procedure TStateIntro.DrawSegs(X, Y: Single; Segs: Integer);
const
  SegsCoord: array [0..10, 0..1] of TVector2D=(
    ((X: 5; Y: 0),    (X: 95; Y: 0)),
    ((X: 100; Y: 5),  (X: 100; Y: 95)),
    ((X: 100; Y: 105), (X: 100; Y: 195)),
    ((X: 95; Y: 200),  (X: 5; Y: 200)),
    ((X: 0; Y: 195),   (X: 0; Y: 105)),
    ((X: 0; Y: 95),    (X: 0; Y: 5)),
    ((X: 5; Y: 100),  (X: 95; Y: 100)),
    ((X: 90; Y: 10),   (X: 10; Y: 90)),
    ((X: 10; Y: 10),   (X: 90; Y: 90)),
    ((X: 90; Y: 110),  (X: 10; Y: 190)),
    ((X: 10; Y: 110),  (X: 90; Y: 190)));
var
  i: Integer;
begin
  glPushMatrix;
  glLineWidth(7);
  glEnable(GL_LINE_SMOOTH);
  glTranslatef(X, Y, 0);
  glBegin(GL_LINES);
    for i:=0 to 10 do
      if Segs and (1 shl i)<>0 then
      begin
        glVertex2fv(@SegsCoord[i, 0]);
        glVertex2fv(@SegsCoord[i, 1]);
      end;
  glEnd;
  glPopMatrix;
end;

procedure TStateIntro.Draw;
const
  SVSE='VS Engine 0.1';
var
  i: Integer;
begin
  glClear(GL_COLOR_BUFFER_BIT);
  for i:=0 to 2 do
    DrawSegs(250+i*100, 200, FSegs[i]);
  glPushMatrix;
  glScalef(2, 2, 1);
  if FTime>50 then gleWrite(200-gleTextWidth(SVSE)/2, 250, SVSE);
  glPopMatrix;
end;

procedure TStateIntro.Update;
const
  SVSE: array[0..2] of Integer=($426, $2C0, $079);
var
  i: Integer;
begin
  Inc(FTime);
  for i:=0 to 2 do
    if FTime>(i+1)*15
      then FSegs[i]:=SVSE[i]
      else FSegs[i]:=Chars[Random(37)];
  {for i:=0 to 2 do
    if FSegs[i]<>SVSE[i] then FSegs[i]:=Chars[FTime div 2];}
  if FTime>70 then Core.SwitchState('Menu');
end;

function TStateIntro.Activate: Cardinal;
begin
  gleSelectFont('Default');
  FTime:=0;
  glClearColor(0, 0, 0, 1);
  glColor3d(0, 1, 0);
  glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glEnable(GL_COLOR_MATERIAL);
  gleOrthoMatrix(800, 600);
  Randomize;
  FSegs[0]:=0;
  FSegs[1]:=0;
  FSegs[2]:=0;
  Result:=50;
end;

procedure TStateIntro.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  if Event<>meMove then Core.SwitchState('Menu');
end;

procedure TStateIntro.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  Core.SwitchState('Menu');
end;

function TStateIntro.GetName: string;
begin
  Result:='Intro';
end;

end.
