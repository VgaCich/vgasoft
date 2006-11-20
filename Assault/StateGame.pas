unit StateGame;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures, PakMan,
  GameStates, UConsole, Unit3DS, ULog, ConsoleSound, Shaders, Terrain;

type
  TStateGame=class(TGameState)
  private
    FText: string;
    FAngle: Single;
    FMenu: Boolean;
    FModel: T3DModel;
    FConsoleSound: TConsoleSound;
//    FShader: TShader;
//    FTerrain: TTerrain;
    function GetName: string; override;
    function  SetModel(const Args: string): Boolean;
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
  end;

implementation

uses UGame;

constructor TStateGame.Create;
var
  F: TStream;
begin
  inherited Create;
  FModel:=T3DModel.Create;
  Console.RegisterCommand('setmodel', '', SetModel);
  FAngle:=0;
  FConsoleSound:=TConsoleSound.Create;
{  FShader:=TShader.Create;
  F:=Game.PakMan.OpenFile('Bump2.shd', ofNoCreate);
  try
    FShader.Load(F);
    FShader.Link;
    if not FShader.Valid
      then Log('StateGame.Create: Shader is not valid');
    Log('Shader log: '+FShader.InfoLog);
  finally
    Game.PakMan.CloseFile(F);
  end;}
{  F:=Game.PakMan.OpenFile('TestTerrain.vtr', ofNoCreate);
  try
    FTerrain:=TTerrain.Create(F, 0.01, 0.001);
  finally
    Game.PakMan.CloseFile(F);
  end; }
end;

destructor TStateGame.Destroy;
begin
//  FAN(FTerrain);
//  FAN(FShader);
  FAN(FModel);
  FAN(FConsoleSound);
  inherited Destroy;
end;

procedure TStateGame.Draw;
var
  S: string;
const
  LightPos: array[0..3] of GLfloat=(0, 0, 100, 1.0);
  LightDiffuse: array[0..3] of GLfloat=(0.7, 0.7, 0.7, 1);
  LightSpecular: array[0..3] of GLfloat=(0.1, 0.1, 0.1, 1);
//  LightAmbient: array[0..3] of GLfloat=(0, 0, 0, 1);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
  glePerspectiveMatrix(Game.ResX, Game.ResY);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSpecular);
  glTranslatef(0, 0, -1);
  glRotatef(FAngle*10, 0, 1, 0);
  glRotatef(FAngle, 1, 0, 0);
//  FShader.Enabled:=true;
  FModel.Draw;
//  FTerrain.Draw;
//  FShader.Enabled:=false;
  gleSelectFont('Default');
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  gleOrthoMatrix(800, 600);
  glColor4d(0, 1, 0, 1);
  S:='FPS: '+IntToStr(Game.FPS);
  gleWrite(800-gleTextWidth(S), 5, S);
  gleWrite(5, 580, FText);
  if FMenu
    then glColor3d(0, 1, 1)
    else glColor3d(0, 1, 0);
  gleWrite(755, 580, 'Menu');
end;

procedure TStateGame.Update;
var
  Pos: TPoint;
begin
  GetCursorPos(Pos);
  Pos.X:=Pos.X*800 div Game.ResX;
  Pos.Y:=Pos.Y*600 div Game.ResY;
  FMenu:=(Pos.X>750) and (Pos.Y>580);
  FAngle:=FAngle+0.2;
  if FAngle>360 then FAngle:=FAngle-360;
end;

function TStateGame.Activate: Cardinal;
begin
  glClearColor(0, 0, 0, 1);
  Result:=20;
end;

procedure TStateGame.Deactivate;
begin

end;

procedure TStateGame.Resume;
begin

end;

procedure TStateGame.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
const
  Events: array[TMouseEvent] of string=('Down', 'Up', 'Move', 'Wheel');
begin
  FText:=Format('Button: %d, Event: %s, X=%d, Y=%d', [Button, Events[Event], X, Y]);
  if FMenu and (Button=1) and (Event=meUp) then Game.SwitchState(Game.FindState('Menu'));
end;

procedure TStateGame.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if (Button=192) and (Event=keUp) then Game.SwitchState(Game.FindState('Console'));
end;

function TStateGame.GetName: string;
begin
  Result:='Game';
end;

function TStateGame.SetModel(const Args: string): Boolean;
var
  ModelData: TStream;
begin
  Result:=false;
  ModelData:=Game.PakMan.OpenFile(Args+'.3ds', ofNoCreate);
  try
    if Assigned(ModelData) then FModel.Load(ModelData);
  finally
    Game.PakMan.CloseFile(ModelData);
  end;
  Result:=true;
end;

end.
