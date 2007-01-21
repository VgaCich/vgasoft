unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures,
  GameStates, StateLoad, ULog, UGUI, GUIWidgets;

type
  TMenuLabel=class;
  TStateMenu=class(TGameState)
  private
    FMenu: TGUI;
    FCursor: TCursor;
    FLbl: TMenuLabel;
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
    constructor Create(Left, Top, Width, Height: Integer; Parent: TGUIWidget; Caption: string);
    property Caption: string read FCaption write FCaption;
  end;
  TMenuForm=class(TGUIWidget)
  private
    FHilightClose, FDownClose, FDragging: Boolean;
    FDragPoint: TPoint;
    FCaption: string;
  protected
    procedure AdjustRect(AWidth, AHeight: Integer); override;
    procedure EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer); override;
    procedure HandleClose(Event: TRegionEvent; Button, X, Y, Tag: Integer);
    procedure Draw; override;
  public
    constructor Create(Left, Top, Width, Height: Integer; Parent: TGUIWidget; Caption: string);
    property Caption: string read FCaption write FCaption;
  end;
  TMenuLabel=class(TGUIWidget)
  private
    FCaption: string;
    FColor: TColor;
  protected
    procedure Draw; override;
  public
    constructor Create(Left, Top, Width, Height: Integer; Parent: TGUIWidget; Caption: string);
    property Caption: string read FCaption write FCaption;
    property Color: TColor read FColor write FColor;
  end;

implementation

uses UGame, PakMan;

constructor TStateMenu.Create;
var
  Btn: TMenuButton;
  Frm: TMenuForm;
begin
  inherited Create;
  FCursor:=TCursor.Create;
  FCursor.Load('Cursor.vcr');
  FMenu:=TGUI.Create(800, 600);
  FLbl:=TMenuLabel.Create(5, 5, 100, 20, nil, 'Label');
  FLbl.Color:=clRed;
  Frm:=TMenuForm.Create(300, 130, 200, 300, nil, 'Assault 0.1');
  Btn:=TMenuButton.Create(50, 50, 100, 30, Frm, 'Intro');
  Btn.OnClick:=IntroClick;
  Btn:=TMenuButton.Create(50, 100, 100, 30, Frm, 'Load');
  Btn.OnClick:=LoadClick;
  Btn:=TMenuButton.Create(50, 150, 100, 30, Frm, 'Game');
  Btn.OnClick:=GameClick;
  Btn:=TMenuButton.Create(50, 200, 100, 30, Frm, 'Console');
  Btn.OnClick:=ConsoleClick;
  Btn:=TMenuButton.Create(50, 250, 100, 30, Frm, 'Exit');
  Btn.OnClick:=ExitClick;
  FMenu.AddForm(Frm);
  FMenu.AddForm(FLbl);
  FMenu.SetModal(Frm);
end;

destructor TStateMenu.Destroy;
begin
  FAN(FCursor);
  FAN(FMenu);
  inherited Destroy;
end;

procedure TStateMenu.Draw;
begin
  glClear(GL_COLOR_BUFFER_BIT);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glBindTexture(GL_TEXTURE_2D, 0);
  glEnable(GL_COLOR_MATERIAL);
  glDisable(GL_DEPTH_TEST);
  FMenu.Draw;
  FCursor.Draw;
end;

procedure TStateMenu.Update;
begin
  FMenu.Update;
  FCursor.Update;
end;

function TStateMenu.Activate: Cardinal;
begin
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
  FLbl.Caption:=Format('X=%d Y=%d', [X, Y]);
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

constructor TMenuButton.Create(Left, Top, Width, Height: Integer; Parent: TGUIWidget; Caption: string);
begin
  inherited Create(Left, Top, Width, Height, Parent);
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
    glVertex2d(0, 0);
    glVertex2d(Width, 0);
    glVertex2d(Width, Height);
    glVertex2d(0, Height);
  glEnd;
  if FHilight
    then glColor3d(1, 0, 0)
    else glColor3d(0, 1, 0);
  glBegin(GL_LINE_LOOP);
    glVertex2d(0, 0);
    glVertex2d(Width, 0);
    glVertex2d(Width, Height);
    glVertex2d(0, Height);
  glEnd;
  glBegin(GL_POINTS);
    glVertex2d(0, 0);
    glVertex2d(Width, 0);
    glVertex2d(Width, Height);
    glVertex2d(0, Height);
  glEnd;
  if FDown
    then glColor3d(0, 1, 0)
    else glColor3d(0, 0.7, 0);
  gleWrite((Width div 2)-gleTextWidth(FCaption)/2, (Height div 2)-8, FCaption);
end;

constructor TMenuForm.Create(Left, Top, Width, Height: Integer; Parent: TGUIWidget; Caption: string);
begin
  inherited Create(Left, Top, Width, Height, Parent);
  FCaption:=Caption;
  SetLength(FRegions, 1);
  FRegions[0].Left:=Width-20;
  FRegions[0].Top:=5;
  FRegions[0].Width:=15;
  FRegions[0].Height:=15;
  FRegions[0].Handler:=HandleClose;
  FDragging:=false;
end;

procedure TMenuForm.AdjustRect(AWidth, AHeight: Integer);
begin
  inherited AdjustRect(AWidth, AHeight);
  FRegions[0].Left:=Width-20;
end;

procedure TMenuForm.EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer);
begin
  inherited;
  case Event of
    reMouseDown:
      if (Button=1) and (Y<=25) then
      begin
        FDragging:=true;
        FDragPoint.X:=X;
        FDragPoint.Y:=Y;
      end;
    reMouseUp: if Button=1 then FDragging:=false;
    reMouseMove:
      if FDragging then
      begin
        Left:=Left+X-FDragPoint.X;
        Top:=Top+Y-FDragPoint.Y;
      end;
  end;
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
    glVertex2d(0, 0);
    glVertex2d(Width, 0);
    glVertex2d(Width, Height);
    glVertex2d(0, Height);
    if FDragging
      then glColor3d(0, 1, 0.5)
      else glColor3d(0, 0.9, 0.5);
    glVertex2d(0, 0);
    glVertex2d(Width, 0);
    glVertex2d(Width, 25);
    glVertex2d(0, 25);
  glEnd;
  glColor3d(1, 1, 0);
  glBegin(GL_LINE_LOOP);
    glVertex2d(0, 0);
    glVertex2d(Width, 0);
    glVertex2d(Width, Height);
    glVertex2d(0, Height);
  glEnd;
  glColor3d(1, 1, 0);
  gleWrite(5, 5, FCaption);
  if FHilightClose
    then glColor3d(1, 0, 0)
    else glColor3d(0.8, 0, 0);
  if FDownClose then glColor3d(1, 0.2, 0.2);
  with FRegions[0] do
  begin
    glBegin(GL_QUADS);
      glVertex2d(Left, Top);
      glVertex2d(Left+Width, Top);
      glVertex2d(Left+Width, Top+Height);
      glVertex2d(Left, Top+Height);
    glEnd;
    glColor3d(0, 0, 0);
    glBegin(GL_LINE_LOOP);
      glVertex2d(Left, Top);
      glVertex2d(Left+Width, Top);
      glVertex2d(Left+Width, Top+Height);
      glVertex2d(Left, Top+Height);
    glEnd;
    glBegin(GL_LINES);
      glVertex2d(Left+2, Top+2);
      glVertex2d(Left+Width-2, Top+Height-2);
      glVertex2d(Left+Width-2, Top+2);
      glVertex2d(Left+2, Top+Height-2);
    glEnd;
  end;
end;

constructor TMenuLabel.Create(Left, Top, Width, Height: Integer; Parent: TGUIWidget; Caption: string);
begin
  inherited Create(Left, Top, Width, Height, Parent);
  FCaption:=Caption;
end;

procedure TMenuLabel.Draw;
begin
  glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
  glEnable(GL_COLOR_MATERIAL);
  glColor3ub(FColor and $FF, (FColor and $FF00) shr 8, (FColor and $FF0000) shr 16);
  gleWrite(0, (Height div 2)-8, FCaption);
end;

end.
