unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures,
  GameStates, StateLoad, ULog, UGUI;

type
  TStateMenu=class(TGameState)
  private
    FMenu: array of string;
    FActive, FLast, FTop: Integer;
    FCurTex: Cardinal;
    function GetName: string; override;
    function Load(ProgFunc: TProgFunc): Boolean;
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

uses UGame, PakMan;

constructor TStateMenu.Create;
var
  Tex: TStream;
begin
  inherited Create;
  Tex:=Game.PakMan.OpenFile('Cursor.tga', ofNoCreate);
  FCurTex:=LoadTexture(Tex, tfTGA, false, GL_NEAREST, GL_NEAREST);
  Game.PakMan.CloseFile(Tex);
end;

destructor TStateMenu.Destroy;
begin
  glDeleteTextures(1, @FCurTex);
  inherited Destroy;
end;

procedure TStateMenu.Draw;
var
  i: Integer;
begin
  glClear(GL_COLOR_BUFFER_BIT);
  gleOrthoMatrix(800, 600);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glBindTexture(GL_TEXTURE_2D, 0);
  glEnable(GL_COLOR_MATERIAL);
  for i:=0 to Length(FMenu)-1 do
  begin
    if i=FActive
      then glColor3d(1, 0.5, 0.5)
      else glColor3d(0.5, 1, 0.5);
    glBegin(GL_QUADS);
      glVertex2d(350, FTop+i*50);
      glVertex2d(450, FTop+i*50);
      glVertex2d(450, FTop+i*50+30);
      glVertex2d(350, FTop+i*50+30);
    glEnd;
    if i=FActive
      then glColor3d(1, 0, 0)
      else glColor3d(0, 1, 0);
    glBegin(GL_LINE_LOOP);
      glVertex2d(350, FTop+i*50);
      glVertex2d(450, FTop+i*50);
      glVertex2d(450, FTop+i*50+30);
      glVertex2d(350, FTop+i*50+30);
    glEnd;
    glBegin(GL_POINTS);
      glVertex2d(350, FTop+i*50);
      glVertex2d(450, FTop+i*50);
      glVertex2d(450, FTop+i*50+30);
      glVertex2d(350, FTop+i*50+30);
    glEnd;
    if i=FLast
      then glColor3d(0, 1, 0)
      else glColor3d(0, 0.7, 0);
    gleWrite(400-gleTextWidth(FMenu[i])/2, FTop+i*50+8, FMenu[i]);
  end;
  DrawCursor;
end;

procedure TStateMenu.Update;
var
  Pos: TPoint;
  i: Integer;

  function PointInRect(X, Y, W, H: Integer): Boolean;
  begin
    Result:=(Pos.X>=X) and (Pos.X<=X+W) and (Pos.Y>=Y) and (Pos.Y<=Y+H);
  end;

begin
  GetCursorPos(Pos);
  Pos.X:=Pos.X*800 div Game.ResX;
  Pos.Y:=Pos.Y*600 div Game.ResY;
  FActive:=-1;
  for i:=0 to Length(FMenu)-1 do
    if PointInRect(350, FTop+i*50, 100, 30) then FActive:=i;
end;

function TStateMenu.Activate: Cardinal;
begin
  SetLength(FMenu, 5);
  FMenu[0]:='Intro';
  FMenu[1]:='Load';
  FMenu[2]:='Game';
  FMenu[3]:='Console';
  FMenu[4]:='Exit';
  FTop:=(600-Length(FMenu)*50) div 2;
  FLast:=-1;
  SetCursor(FCurTex, 32);
  gleSelectFont('Default');
  glClearColor(0, 0, 0, 0);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glLineWidth(4);
  glPointSize(4);
  ShowCursor(false);
  Result:=50;
end;

procedure TStateMenu.Deactivate;
begin
  Finalize(FMenu);
  ShowCursor(true);
end;

procedure TStateMenu.Resume;
begin

end;

function TStateMenu.GetName: string;
begin
  Result:='Menu';
end;

function TStateMenu.Load(ProgFunc: TProgFunc): Boolean;
var
  i: Integer;

  function GetState: string;
  begin
    case i of
      0..30: Result:='Loading terrain...';
      31..60: Result:='Loading models...';
      61..90: Result:='Loading textures...';
      91..100: Result:='Initializing...';
    end;
  end;

begin
  for i:=0 to 100 do
  begin
    ProgFunc(i, GetState);
    Sleep(100);
  end;
end;

procedure TStateMenu.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
var
  SL: TStateLoad;
begin
  if Button=1 then
  case Event of
    meDown: FLast:=FActive;
    meUp:
      begin
        if FActive=FLast then
          case FActive of
            0: Game.SwitchState(Game.FindState('Intro'));
            1: begin
                 SL:=Game.GetState(Game.FindState('Load')) as TStateLoad;
                 SL.LoadFunc:=Load;
                 Game.SwitchState(Game.FindState('Load'));
               end;
            2: Game.SwitchState(Game.FindState('Game'));
            3: Game.SwitchState(Game.FindState('Console'));
            4: Game.StopEngine;
          end;
        FLast:=-1;
      end;
  end;
end;

procedure TStateMenu.KeyEvent(Button: Integer; Event: TKeyEvent);
begin

end;

end.
