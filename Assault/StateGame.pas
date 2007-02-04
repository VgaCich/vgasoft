unit StateGame;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures, PakMan,
  GameStates, UConsole, Unit3DS, ULog, ConsoleSound, Shaders, Terrain, StateMenu, UGUI;

type
  TStateGame=class(TGameState)
  private
    FText: TStringList;
    FAngle: Single;
    FButtonMenu: TMenuButton;
    FGUI: TGUI;
    FModel: T3DModel;
    FConsoleSound: TConsoleSound;
    FCursor: TCursor;
//    FShader: TShader;
//    FTerrain: TTerrain;
    function  GetName: string; override;
    function  SetModel(const Args: string): Boolean;
    procedure MenuClick(Sender: TObject);
    procedure AddText(const S: string);
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
  end;

implementation

uses UCore;

constructor TStateGame.Create;
var
  F: TStream;
begin
  inherited Create;
  FCursor:=TCursor.Create;
  FCursor.Load('LTCur.vcr');
  FModel:=T3DModel.Create;
  Console.RegisterCommand('setmodel', '', SetModel);
  FAngle:=0;
  FConsoleSound:=TConsoleSound.Create;
  FGUI:=TGUI.Create(800, 600);
  FButtonMenu:=TMenuButton.Create(745, 575, 50, 20, nil, 'Menu');
  FButtonMenu.OnClick:=MenuClick;
  FGUI.AddForm(FButtonMenu);
  FGUI.GrabFocus(FButtonMenu);
  FText:=TStringList.Create;
{  FShader:=TShader.Create;
  F:=Core.PakMan.OpenFile('Bump2.shd', ofNoCreate);
  try
    FShader.Load(F);
    FShader.Link;
    if not FShader.Valid
      then Log('StateGame.Create: Shader is not valid');
    Log('Shader log: '+FShader.InfoLog);
  finally
    Core.PakMan.CloseFile(F);
  end;}
{  F:=Core.PakMan.OpenFile('TestTerrain.vtr', ofNoCreate);
  try
    FTerrain:=TTerrain.Create(F, 0.01, 0.001);
  finally
    Core.PakMan.CloseFile(F);
  end; }
end;

destructor TStateGame.Destroy;
begin
//  FAN(FTerrain);
//  FAN(FShader);
  FAN(FCursor);
  Console.UnregisterCommand('setmodel');
  FAN(FText);
  FAN(FGUI);
  FAN(FModel);
  FAN(FConsoleSound);
  inherited Destroy;
end;

procedure TStateGame.Draw;
var
  S: string;
  i: Integer;
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
  glePerspectiveMatrix(Core.ResX, Core.ResY);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSpecular);
  glTranslatef(0, 0, -1);
  glRotatef(FAngle*10, 0, 1, 0);
  glRotatef(FAngle, 1, 0, 0);
  glColor4d(0, 1, 0, 1);
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
  S:='FPS: '+IntToStr(Core.FPS);
  gleWrite(800-gleTextWidth(S), 5, S);
  glColor4d(0, 1, 0, 0.3);
  for i:=0 to 29 do
    gleWrite(5, i*20, FText[i]);
  FGUI.Draw;
  FCursor.Draw;
end;

procedure TStateGame.Update;
begin
  FGUI.Update;
  FCursor.Update;
  FAngle:=FAngle+0.2;
  if FAngle>360 then FAngle:=FAngle-360;
end;

function TStateGame.Activate: Cardinal;
begin
  glClearColor(0, 0, 0, 1);
  ShowCursor(false);
  Result:=20;
end;

procedure TStateGame.Deactivate;
begin
  ShowCursor(true);
end;

procedure TStateGame.Resume;
begin

end;

procedure TStateGame.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
const
  Events: array[TMouseEvent] of string=('Down', 'Up', 'Move', 'Wheel');
begin
  AddText(Format('MouseEvent(Button=%d, Event=%s, X=%d, Y=%d)', [Button, Events[Event], X, Y]));
  FGUI.MouseEvent(Button, Event, X, Y);
end;

procedure TStateGame.KeyEvent(Button: Integer; Event: TKeyEvent);
const
  Events: array[TKeyEvent] of string=('Down', 'Up');
begin
  AddText(Format('KeyEvent(Button=%d, Event=%s)', [Button, Events[Event]]));
  if (Button=192) and (Event=keUp) then Core.SwitchState(Core.FindState('Console'));
  FGUI.KeyEvent(Button, Event);
end;

procedure TStateGame.CharEvent(C: Char);
begin
  AddText('CharEvent(C='+C+')');
  FGUI.CharEvent(C);
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
  ModelData:=Core.PakMan.OpenFile(Args+'.3ds', ofNoCreate);
  try
    if Assigned(ModelData) then FModel.Load(ModelData);
  finally
    Core.PakMan.CloseFile(ModelData);
  end;
  Result:=true;
end;

procedure TStateGame.MenuClick(Sender: TObject);
begin
  Core.SwitchState(Core.FindState('Menu'));
end;

procedure TStateGame.AddText(const S: string);
begin
  FText.Add(S);
  if FText.Count>30 then FText.Delete(0);
end;

end.
