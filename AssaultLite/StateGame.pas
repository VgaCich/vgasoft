unit StateGame;

interface

uses
  Windows, Messages, AvL, avlUtils, avlMath, OpenGL, VSEOpenGLExt, oglExtensions,
  avlVectors, VSEGameStates, VSETerrain, VSEQuadTree, Sky, VSECamera, VSEMemPak,
  VSESound, VSEBindMan, GameUnit, GamePlayer;

type
  TStateGame=class(TGameState)
  private
    FTerrain: TTerrain;
    FSky: TSky;
    FFont: Cardinal;
    FPlayers: array of TPlayer;
    FUnits: array of TUnit;
    function GetCanResumeGame: Boolean;
  protected
    function  GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
    procedure Resume; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
    procedure CharEvent(C: Char); override;
    procedure NewGame;
    property CanResumeGame: Boolean read GetCanResumeGame;
    property Terrain: TTerrain read FTerrain;
  end;

implementation

uses VSECore, VSETexMan {$IFDEF VSE_LOG}, VSELog{$ENDIF};

constructor TStateGame.Create;
begin
  inherited Create;
  Camera:=TCamera.Create;
  FFont:=TexMan.FontCreate('Courier New', 10);
  FTerrain:=TTerrain.Create;
end;

destructor TStateGame.Destroy;
var
  i: Integer;
begin
  for i:=0 to High(FPlayers) do
    FPlayers[i].Free;
  Finalize(FPlayers);
  FAN(FTerrain);
  FAN(FSky);
  FAN(Camera);
  inherited Destroy;
end;

procedure TStateGame.Draw;
const
  LightPos: array[0..3] of GLfloat = (256, 128, 256, 1);
  LightAmbi: array[0..3] of GLfloat = (0.1, 0.1, 0.1, 1);
  LightDiff: array[0..3] of GLfloat = (1, 1, 1, 1);
var
  i: Integer;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glePerspectiveMatrix(60, Core.ResX, Core.ResY);
  glMatrixMode(GL_PROJECTION);
  Camera.CalcVertex;
  Camera.SetPos;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  FSky.Draw;
  glEnable(GL_DEPTH_TEST);
  glColor(1.0, 1.0, 1.0);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiff);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @LightAmbi);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  FTerrain.Draw;
  for i:=0 to High(FPlayers) do
    FPlayers[i].Draw;
  gleOrthoMatrix(800, 600);
  glColor3f(1, 1, 1);
  TexMan.TextOut(FFont, 5, 5, 'FPS: '+IntToStr(Core.FPS));
  with Camera.Eye do
    TexMan.TextOut(FFont, 5, 25, Format('Pos=%s, %s, %s',
      [FloatToStr2(X, 4, 2), FloatToStr2(Y, 4, 2), FloatToStr2(Z, 4, 2)]));
  with Camera do
    TexMan.TextOut(FFont, 5, 45, Format('Angles=%s, %s',
      [FloatToStr2(XAngle, 4, 2), FloatToStr2(YAngle, 4, 2)]));
end;

procedure TStateGame.Update;
const
  CamBorder=64;

  function CalcShift(const V1, V2: TVector3D): TVector3D;
  var
    D: Single;
  begin
    Result:=VectorSub(V1, V2);
    Result.Y:=0;
    D:=VectorSize(Result);
    VectorNormalize(Result);
    Result:=VectorMultiply(Result, Max(0, 9-D)/2);
  end;

var
  Spd: TVector3D;
  Offs: TVector3D;
  C, S: Single;
  i, j, Pass: Integer;
begin
  VectorClear(Spd);
  if BindMan.BindActive['Fwd'] then Spd.Z:=Spd.Z+1;
  if BindMan.BindActive['Bwd'] then Spd.Z:=Spd.Z-1;
  if BindMan.BindActive['SLeft'] then Spd.X:=Spd.X+1;
  if BindMan.BindActive['SRight'] then Spd.X:=Spd.X-1;
  VectorNormalize(Spd);
  if BindMan.BindActive['SpdDn'] then VectorScale(Spd, 0.2);
  if not BindMan.BindActive['SpdUp'] then VectorScale(Spd, 0.2);
  C:=Cos(Camera.XAngle*DegToRad);
  S:=Sin(Camera.XAngle*DegToRad);
  Offs.Z:=C*Spd.Z+S*Spd.X;
  Offs.X:=C*Spd.X-S*Spd.Z;
  Offs.Y:=0;
  Camera.Eye:=VectorAdd(Camera.Eye, Offs);
  if Camera.Eye.X<CamBorder then Camera.Eye.X:=CamBorder;
  if Camera.Eye.X>FTerrain.Width-CamBorder then Camera.Eye.X:=FTerrain.Width-CamBorder;
  if Camera.Eye.Z<CamBorder then Camera.Eye.Z:=CamBorder;
  if Camera.Eye.Z>FTerrain.Height-CamBorder then Camera.Eye.Z:=FTerrain.Height-CamBorder;
  Camera.Eye.Y:=FTerrain.Altitude(Camera.Eye.X, Camera.Eye.Z)+4;
  FSky.Update;
  for i:=0 to High(FPlayers) do
    FPlayers[i].Update;
  for Pass:=0 to 2 do
  begin
    for i:=0 to High(FUnits) do
    begin
      Offs:=CalcShift(FUnits[i].Pos, Camera.Eye);
      Camera.Eye:=VectorSub(Camera.Eye, VectorMultiply(Offs, 0.1));
      for j:=0 to High(FUnits) do
      begin
        Offs:=CalcShift(FUnits[i].Pos, FUnits[j].Pos);
        FUnits[i].Pos:=VectorAdd(FUnits[i].Pos, Offs);
        FUnits[j].Pos:=VectorSub(FUnits[j].Pos, Offs);
      end;
    end;
  end;
  if Camera.Eye.X<CamBorder then Camera.Eye.X:=CamBorder;
  if Camera.Eye.X>FTerrain.Width-CamBorder then Camera.Eye.X:=FTerrain.Width-CamBorder;
  if Camera.Eye.Z<CamBorder then Camera.Eye.Z:=CamBorder;
  if Camera.Eye.Z>FTerrain.Height-CamBorder then Camera.Eye.Z:=FTerrain.Height-CamBorder;
  Camera.Eye.Y:=FTerrain.Altitude(Camera.Eye.X, Camera.Eye.Z)+4;
end;

function TStateGame.Activate: Cardinal;
begin
  Result:=20;
  glClearColor(0, 0, 0, 1);
  Core.MouseCapture:=true;
  ShowCursor(false);
  //Sound.PlayMusic('Music.xm');
end;

procedure TStateGame.Deactivate;
begin
  ShowCursor(true);
  Core.MouseCapture:=false;
  //Sound.StopMusic;
end;

procedure TStateGame.Resume;
begin

end;

procedure TStateGame.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  if Event<>meMove then Exit;
  Camera.XAngle:=Camera.XAngle+X/10;
  Camera.YAngle:=Camera.YAngle-Y/10;
  if Camera.YAngle<-89 then Camera.YAngle:=-89;
  if Camera.YAngle>89 then Camera.YAngle:=89;
end;

procedure TStateGame.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if Event=keDown then
  begin
    case Button of
      VK_ESCAPE: Core.SwitchState('Menu');
    end;
  end;
end;

procedure TStateGame.CharEvent(C: Char);
begin

end;

procedure TStateGame.NewGame;
var
  i: Integer;
begin
  Camera.Eye:=VectorSetValue(256, 1, 256);
  Camera.XAngle:=-45;
  Camera.YAngle:=25;
  //FTerrain.Texture:=TexMan.GetTex('Grass');
  if not Assigned(FSky) then FSky:=TSky.Create;
  for i:=0 to High(FPlayers) do
    FPlayers[i].Free;
  Finalize(FPlayers);
  Finalize(FUnits);
  SetLength(FPlayers, 2);
  for i:=0 to High(FPlayers) do
    FPlayers[i]:=TPlayer.Create(4);
  for i:=0 to FPlayers[0].UnitsCount-1 do
    FPlayers[0].Units[i].Pos:=VectorAdd(FPlayers[0].Units[i].Pos, VectorSetValue(-8, 0, 0));
  SetLength(FUnits, 8);
  for i:=0 to 3 do
  begin
    FUnits[i]:=FPlayers[0].Units[i];
    FUnits[4+i]:=FPlayers[1].Units[i];
  end;
end;

function TStateGame.GetName: string;
begin
  Result:='Game';
end;

function TStategame.GetCanResumeGame: Boolean;
begin
  Result:=Length(FPlayers)>0;
end;

end.
