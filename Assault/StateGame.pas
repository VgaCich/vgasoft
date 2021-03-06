unit StateGame;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures,
  VSEPakMan, VSEGameStates, VSEConsole, Unit3DS, VSELog, Shaders, StateMenu,
  VSEGUI;

type
  TStateGame=class(TGameState)
  private
    FText: TStringList;
    FAngle: Single;
    FButtonMenu: TMenuButton;
    FGUI: TGUI;
    FModel: T3DModel;
    FCursor: TCursor;
    FShader: TShader;
    FUniformEyePos, FUniformMainTex: TShaderUniform;
//    FTerrain: TTerrain;
    function  SetModel(const Args: string): Boolean;
    procedure MenuClick(Sender: TObject);
    procedure AddText(const S: string);
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
  end;

implementation

uses VSECore;

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
  FGUI:=TGUI.Create(800, 600);
  FButtonMenu:=TMenuButton.Create(745, 575, 50, 20, nil, 'Menu');
  FButtonMenu.OnClick:=MenuClick;
  FGUI.AddForm(FButtonMenu);
  FGUI.GrabFocus(FButtonMenu);
  FText:=TStringList.Create;
  FShader:=TShader.Create;
  F:=PakMan.OpenFile('Test.shd', pmNoCreate);
  try
    FShader.Load(F);
    FShader.Link;
    if not FShader.Valid
      then Log(llError, 'StateGame.Create: Shader is not valid');
    Log(llInfo, 'Shader log: '+FShader.InfoLog);
    FUniformEyePos:=FShader.GetUniform('eyePos');
    FUniformMainTex:=FShader.GetUniform('mainTex');
  finally
    FAN(F);
  end;
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
  FAN(FShader);
  FAN(FCursor);
  Console.UnregisterCommand('setmodel');
  FAN(FText);
  FAN(FGUI);
  FAN(FModel);
  inherited Destroy;
end;

procedure TStateGame.Draw;
var
  S: string;
  i, X, Y: Integer;
const
  LightPos: array[0..3] of GLfloat=(0, 0, 0, 1.0);
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
  FShader.Enabled:=true;
  FUniformEyePos.Value(0, 0, 0);
  FUniformMainTex.Value(0);
  FModel.Draw;
//  FTerrain.Draw;
  FShader.Enabled:=false;
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
  glLineWidth(1);
  for i:=0 to 16 do
  begin
    glBegin(GL_LINES);
      glVertex2d(600+i*12, 400);
      glVertex2d(600+i*12, 528);
      glVertex2d(600, 400+i*8);
      glVertex2d(792, 400+i*8);
    glEnd;
  end;
  for i:=0 to 255 do
    if Core.KeyPressed[i] then
    begin
      X:=i mod 16;
      Y:=i div 16;
      glBegin(GL_QUADS);
        glVertex2d(600+X*12, 400+Y*8);
        glVertex2d(612+X*12, 400+Y*8);
        glVertex2d(612+X*12, 408+Y*8);
        glVertex2d(600+X*12, 408+Y*8);
      glEnd;
    end;
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
  AddText(Format('MouseEvent(Button=%d, Event=%s, X=%d, Y=%d)', [Button, Events[Event], X, Y]));
  FGUI.MouseEvent(Button, Event, X, Y);
end;

procedure TStateGame.KeyEvent(Button: Integer; Event: TKeyEvent);
const
  Events: array[TKeyEvent] of string=('Down', 'Up');
begin
  AddText(Format('KeyEvent(Button=%d, Event=%s)', [Button, Events[Event]]));
  if (Button=192) and (Event=keUp) then Core.SwitchState('Console');
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
  ModelData:=PakMan.OpenFile(Args+'.3ds', pmNoCreate);
  try
    if Assigned(ModelData) then FModel.Load(ModelData);
  finally
    FAN(ModelData);
  end;
  Result:=true;
end;

procedure TStateGame.MenuClick(Sender: TObject);
begin
  Core.SwitchState('Menu');
end;

procedure TStateGame.AddText(const S: string);
begin
  FText.Add(S);
  if FText.Count>30 then FText.Delete(0);
end;

end.
