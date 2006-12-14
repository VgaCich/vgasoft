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
  TMenuForm=class(TGUIWidget)
  private
    FHilightClose, FDownClose: Boolean;
    FCaption: string;
  protected
    procedure AdjustRect(ARect: TRect); override;
    procedure EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer); override;
    procedure HandleClose(Event: TRegionEvent; Button, X, Y, Tag: Integer);
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
  Frm: TMenuForm;
begin
  inherited Create;
  Tex:=Game.PakMan.OpenFile('Cursor.tga', ofNoCreate);
  FCurTex:=LoadTexture(Tex, tfTGA, false, GL_NEAREST, GL_NEAREST);
  Game.PakMan.CloseFile(Tex);
  FMenu:=TGUI.Create(800, 600);
  Frm:=TMenuForm.Create(Rect(300, 130, 500, 430), nil, 'Assault 0.1');
  Btn:=TMenuButton.Create(Rect(350, 170, 450, 200), Frm, 'Intro');
  Btn.OnClick:=IntroClick;
  Btn:=TMenuButton.Create(Rect(350, 220, 450, 250), Frm, 'Load');
  Btn.OnClick:=LoadClick;
  Btn:=TMenuButton.Create(Rect(350, 270, 450, 300), Frm, 'Game');
  Btn.OnClick:=GameClick;
  Btn:=TMenuButton.Create(Rect(350, 320, 450, 350), Frm, 'Console');
  Btn.OnClick:=ConsoleClick;
  Btn:=TMenuButton.Create(Rect(350, 370, 450, 400), Frm, 'Exit');
  Btn.OnClick:=ExitClick;
  FMenu.AddForm(Frm);
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
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glLineWidth(2);
  glPointSize(2);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
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
end;

constructor TMenuForm.Create(Rect: TRect; Parent: TGUIWidget; Caption: string);
begin
  inherited Create(Rect, Parent);
  FCaption:=Caption;
  SetLength(FRegions, 1);
  FRegions[0].Rect:=AvL.Rect(Rect.Right-20, Rect.Top+5, Rect.Right-5, Rect.Top+20);
  FRegions[0].Handler:=HandleClose;
end;

procedure TMenuForm.AdjustRect(ARect: TRect);
begin
  inherited AdjustRect(ARect);
  FRegions[0].Rect:=AvL.Rect(FRect.Right-20, FRect.Top+5, FRect.Right-5, FRect.Top+20);
end;

procedure TMenuForm.EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer);
begin
  inherited;
end;

procedure TMenuForm.HandleClose(Event: TRegionEvent; Button, X, Y, Tag: Integer);
begin
  case Event of
    reMouseEnter: FHilightClose:=true;
    reMouseLeave: begin
      FHilightClose:=false;
      FDownClose:=false;
    end;
    reMouseDown: FDownClose:=true;
    reMouseUp: FDownClose:=false;
    reMouseClick: if Button=1 then Game.StopEngine;
  end;
end;

procedure TMenuForm.Draw;
begin
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glLineWidth(1);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_COLOR_MATERIAL);
  glBegin(GL_QUADS);
    glColor3d(0.5, 1, 0.5);
    glVertex2d(FRect.Left, FRect.Top);
    glVertex2d(FRect.Right, FRect.Top);
    glVertex2d(FRect.Right, FRect.Bottom);
    glVertex2d(FRect.Left, FRect.Bottom);
    glColor3d(0, 1, 0.5);
    glVertex2d(FRect.Left, FRect.Top);
    glVertex2d(FRect.Right, FRect.Top);
    glVertex2d(FRect.Right, FRect.Top+25);
    glVertex2d(FRect.Left, FRect.Top+25);
  glEnd;
  glColor3d(1, 1, 0);
  glBegin(GL_LINE_LOOP);
    glVertex2d(FRect.Left, FRect.Top);
    glVertex2d(FRect.Right, FRect.Top);
    glVertex2d(FRect.Right, FRect.Bottom);
    glVertex2d(FRect.Left, FRect.Bottom);
  glEnd;
  glColor3d(1, 1, 0);
  gleWrite(FRect.Left+5, FRect.Top+5, FCaption);
  if FHilightClose
    then glColor3d(1, 0, 0)
    else glColor3d(0.8, 0, 0);
  if FDownClose then glColor3d(1, 0.2, 0.2);
  glBegin(GL_QUADS);
    glVertex2d(FRegions[0].Rect.Left, FRegions[0].Rect.Top);
    glVertex2d(FRegions[0].Rect.Right, FRegions[0].Rect.Top);
    glVertex2d(FRegions[0].Rect.Right, FRegions[0].Rect.Bottom);
    glVertex2d(FRegions[0].Rect.Left, FRegions[0].Rect.Bottom);
  glEnd;
  glColor3d(0, 0, 0);
  glBegin(GL_LINE_LOOP);
    glVertex2d(FRegions[0].Rect.Left, FRegions[0].Rect.Top);
    glVertex2d(FRegions[0].Rect.Right, FRegions[0].Rect.Top);
    glVertex2d(FRegions[0].Rect.Right, FRegions[0].Rect.Bottom);
    glVertex2d(FRegions[0].Rect.Left, FRegions[0].Rect.Bottom);
  glEnd;
  glBegin(GL_LINES);
    glVertex2d(FRegions[0].Rect.Left+2, FRegions[0].Rect.Top+2);
    glVertex2d(FRegions[0].Rect.Right-2, FRegions[0].Rect.Bottom-2);
    glVertex2d(FRegions[0].Rect.Right-2, FRegions[0].Rect.Top+2);
    glVertex2d(FRegions[0].Rect.Left+2, FRegions[0].Rect.Bottom-2);
  glEnd;
end;

end.
