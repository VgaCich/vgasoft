unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlMath, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures,
  GameStates, StateLoad, ULog, UGUI, GUIWidgets;

type
  TStateMenu=class(TGameState)
  private
    FMenu: TGUI;
    FCurTex: Cardinal;
  protected
    function GetName: string; override;
    function Load(ProgFunc: TProgFunc): Boolean;
    procedure IntroClick(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure GameClick(Sender: TObject);
    procedure ConsoleClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
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
  TMenuButton=class(TGUIButtonBase)
  private
    FHilight, FDown: Boolean;
    FCaption: string;
  protected
    procedure EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer); override;
    procedure Draw; override;
  public
    constructor Create(Rect: TRect; Parent: TGUIWidget; Caption: string);
    property Caption: string read FCaption write FCaption;
  end;

implementation

uses UGame, PakMan;

constructor TStateMenu.Create;
var
  Tex: TStream;
  Btn: TMenuButton;
begin
  inherited Create;
  Tex:=Game.PakMan.OpenFile('Cursor.tga', ofNoCreate);
  FCurTex:=LoadTexture(Tex, tfTGA, false, GL_NEAREST, GL_NEAREST);
  Game.PakMan.CloseFile(Tex);
  FMenu:=TGUI.Create(800, 600);
  Btn:=TMenuButton.Create(Rect(350, 170, 450, 200), nil, 'Intro');
  Btn.OnClick:=IntroClick;
  FMenu.AddForm(Btn);
  Btn:=TMenuButton.Create(Rect(350, 220, 450, 250), nil, 'Load');
  Btn.OnClick:=LoadClick;
  FMenu.AddForm(Btn);
  Btn:=TMenuButton.Create(Rect(350, 270, 450, 300), nil, 'Game');
  Btn.OnClick:=GameClick;
  FMenu.AddForm(Btn);
  Btn:=TMenuButton.Create(Rect(350, 320, 450, 350), nil, 'Console');
  Btn.OnClick:=ConsoleClick;
  FMenu.AddForm(Btn);
  Btn:=TMenuButton.Create(Rect(350, 370, 450, 400), nil, 'Exit');
  Btn.OnClick:=ExitClick;
  FMenu.AddForm(Btn);
end;

destructor TStateMenu.Destroy;
begin
  glDeleteTextures(1, @FCurTex);
  FAN(FMenu);
  inherited Destroy;
end;

procedure TStateMenu.Draw;
begin
  glClear(GL_COLOR_BUFFER_BIT);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glBindTexture(GL_TEXTURE_2D, 0);
  glEnable(GL_COLOR_MATERIAL);
  FMenu.Draw;
  DrawCursor;
end;

procedure TStateMenu.Update;
begin
  FMenu.Update;
end;

function TStateMenu.Activate: Cardinal;
begin
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
  ShowCursor(true);
end;

procedure TStateMenu.Resume;
begin

end;

procedure TStateMenu.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  FMenu.MouseEvent(Button, Event, X, Y);
end;

procedure TStateMenu.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  FMenu.KeyEvent(Button, Event);
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
  Result:=true;
end;

procedure TStateMenu.IntroClick(Sender: TObject);
begin
  Game.SwitchState(Game.FindState('Intro'));
end;

procedure TStateMenu.LoadClick(Sender: TObject);
var
  SL: Integer;
begin
  SL:=Game.FindState('Load');
  (Game.GetState(SL) as TStateLoad).LoadFunc:=Load;
  Game.SwitchState(SL);
end;

procedure TStateMenu.GameClick(Sender: TObject);
begin
  Game.SwitchState(Game.FindState('Game'));
end;

procedure TStateMenu.ConsoleClick(Sender: TObject);
begin
  Game.SwitchState(Game.FindState('Console'));
end;

procedure TStateMenu.ExitClick(Sender: TObject);
begin
  Game.StopEngine;
end;

constructor TMenuButton.Create(Rect: TRect; Parent: TGUIWidget; Caption: string);
begin
  inherited Create(Rect, Parent);
  FCaption:=Caption;
end;

procedure TMenuButton.EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer);
begin
  inherited EventHandler(Event, Button, X, Y, Tag);
  case Event of
    reMouseEnter: FHilight:=true;
    reMouseLeave: begin
      FHilight:=false;
      FDown:=false;
    end;
    reMouseDown: FDown:=true;
    reMouseUp: FDown:=false;
  end;
end;

procedure TMenuButton.Draw;
begin
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glLineWidth(4);
  glPointSize(4);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glEnable(GL_COLOR_MATERIAL);
  if FHilight
    then glColor3d(1, 0.5, 0.5)
    else glColor3d(0.5, 1, 0.5);
  glBegin(GL_QUADS);
    glVertex2d(FRect.Left, FRect.Top);
    glVertex2d(FRect.Right, FRect.Top);
    glVertex2d(FRect.Right, FRect.Bottom);
    glVertex2d(FRect.Left, FRect.Bottom);
  glEnd;
  if FHilight
    then glColor3d(1, 0, 0)
    else glColor3d(0, 1, 0);
  glBegin(GL_LINE_LOOP);
    glVertex2d(FRect.Left, FRect.Top);
    glVertex2d(FRect.Right, FRect.Top);
    glVertex2d(FRect.Right, FRect.Bottom);
    glVertex2d(FRect.Left, FRect.Bottom);
  glEnd;
  glBegin(GL_POINTS);
    glVertex2d(FRect.Left, FRect.Top);
    glVertex2d(FRect.Right, FRect.Top);
    glVertex2d(FRect.Right, FRect.Bottom);
    glVertex2d(FRect.Left, FRect.Bottom);
  glEnd;
  if FDown
    then glColor3d(0, 1, 0)
    else glColor3d(0, 0.7, 0);
  gleWrite(Mean([FRect.Left, FRect.Right])-gleTextWidth(FCaption)/2, Mean([FRect.Top, FRect.Bottom])-8, FCaption);
  glEnable(GL_SCISSOR_TEST);
  glClearColor(1, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT);
end;

end.
