unit UGUI;

interface

uses
  Windows, AvL, avlUtils, dglOpenGL, OpenGLExt, GameStates, CollisionCheck;

type
  TRegionEvent=(reMouseEnter, reMouseLeave, reMouseMove, reMouseDown, reMouseUp, reMouseClick, reMouseWheel);
  TRegionHandler=procedure(Event: TRegionEvent; Button, X, Y, Tag: Integer) of object;
  TRegion=record
    Rect: TRect;
    Handler: TRegionHandler;
    Tag: Integer;
    MouseIn, Exist: Boolean;
  end;
  TMouseButtons=array[0..2] of Integer;
  TGUIWidget=class
  protected
    FRect: TRect;
    FTag: Integer;
    FWidgets: TList;
    FRegions: array of TRegion;
    function  AddWidget(Widget: TGUIWidget): Integer;
    procedure DeleteWidget(Index: Integer);
  public
    constructor Create(Rect: TRect; Parent: TGUIWidget);
    destructor Destroy; override;
    procedure EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer); virtual;
    procedure Draw; virtual;
    property Rect: TRect read FRect;
    property Tag: Integer read FTag write FTag;
  end;
  TGUI=class
  private
    FVSW, FVSH, FActive, FLastActive: Integer;
    FLast: TMouseButtons;
    FDestroying: Boolean;
    FForms: TList;
  protected
    procedure MapCursor(var Cursor: TPoint);
    function  FormAt(Point: TPoint): Integer;
    procedure SendEvent(Form: Integer; Event: TRegionEvent; Button, X, Y: Integer);
  public
    constructor Create(VirtScreenWidth, VirtScreenHeight: Integer);
    Destructor Destroy; override;
    procedure Update;
    procedure Draw;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
    procedure KeyEvent(Button: Integer; Event: TKeyEvent);
    procedure CharEvent(C: Char);
    function  AddForm(Form: TGUIWidget): Integer;
    procedure DeleteForm(Index: Integer);
  end;

procedure SetCursor(Tex: Cardinal; Size: Single);
procedure DrawCursor;

implementation

uses
  UGame, ULog;

{TGUIWidget}

constructor TGUIWidget.Create(Rect: TRect; Parent: TGUIWidget);
begin

end;

destructor TGUIWidget.Destroy;
begin

end;

function TGUIWidget.AddWidget(Widget: TGUIWidget): Integer;
begin

end;

procedure TGUIWidget.DeleteWidget(Index: Integer);
begin

end;

procedure TGUIWidget.EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer);
begin

end;

procedure TGUIWidget.Draw;
begin

end;

{TGUI}

constructor TGUI.Create(VirtScreenWidth, VirtScreenHeight: Integer);
begin
  inherited Create;
  FVSW:=VirtScreenWidth;
  FVSH:=VirtScreenHeight;
  FActive:=-1;
  FLastActive:=-1;
  FLast[0]:=-1;
  FLast[1]:=-1;
  FLast[2]:=-1;
  FForms:=TList.Create;
end;

destructor TGUI.Destroy;
var
  i: Integer;
begin
  FDestroying:=true;
  for i:=0 to FForms.Count-1 do TObject(FForms[i]).Free;
  FAN(FForms);
  inherited Destroy;
end;

procedure TGUI.MapCursor(var Cursor: TPoint);
begin
  Cursor.X:=Round(Cursor.X*FVSW/Game.ResX);
  Cursor.Y:=Round(Cursor.Y*FVSH/Game.ResY);
end;

function TGUI.FormAt(Point: TPoint): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to FForms.Count-1 do
    if PointInRect(Point, TGUIWidget(FForms[i]).Rect)
      then Result:=i;
end;

procedure TGUI.SendEvent(Form: Integer; Event: TRegionEvent; Button, X, Y: Integer);
begin
  if (Form<0) or (Form>=FForms.Count) then Exit;
  TGUIWidget(FForms[Form]).EventHandler(Event, Button, X, Y, TGUIWidget(FForms[Form]).Tag);
end;

procedure TGUI.Update;
var
  Cursor: TPoint;
  i: Integer;
begin
  GetCursorPos(Cursor);
  MapCursor(Cursor);
  FLastActive:=FActive;
  FActive:=FormAt(Cursor);
end;

procedure TGUI.Draw;
var
  i: Integer;
begin
  glPushMatrix;
  gleOrthoMatrix(FVSW, FVSH);
  for i:=0 to FForms.Count-1 do TGUIWidget(FForms[i]).Draw;
  glPopMatrix;
end;

procedure TGUI.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
var
  Cursor: TPoint;
  Form: Integer;
begin
  Cursor.X:=X;
  Cursor.Y:=Y;
  MapCursor(Cursor);
  Form:=FormAt(Cursor);
  case Event of
    meDown: begin
      FLast[Button]:=FActive;
      SendEvent(Form, reMouseDown, Button, Cursor.X, Cursor.Y);
    end;
    meUp: begin
      SendEvent(Form, reMouseUp, Button, Cursor.X, Cursor.Y);
      if (FActive>=0) and (FLast[Button]=FActive)
        then SendEvent(Form, reMouseClick, Button, Cursor.X, Cursor.Y)
        else FLast[Button]:=-1;
    end;
    meMove: if (FActive>=0) and (FLastActive>=0) then
      begin
        if FActive<>FLastActive then
        begin
          SendEvent(FLastActive, reMouseLeave, 0, 0, 0);
          SendEvent(FActive, reMouseEnter, 0, Cursor.X, Cursor.Y);
        end
          else SendEvent(FActive, reMouseMove, 0, Cursor.X, Cursor.Y);
      end;
    meWheel: SendEvent(Form, reMouseWheel, Button, Cursor.X, Cursor.Y);
  end;
end;

procedure TGUI.KeyEvent(Button: Integer; Event: TKeyEvent);
begin

end;

procedure TGUI.CharEvent(C: Char);
begin

end;

function TGUI.AddForm(Form: TGUIWidget): Integer;
begin
  Result:=-1;
  if FForms.IndexOf(Form)>-1 then Exit;
  Result:=FForms.Add(Form);
end;

procedure TGUI.DeleteForm(Index: Integer);
begin
  if (Index>=0) and (Index<FForms.Count) then FForms.Delete(Index);
end;

{Misc functions}

var
  CursorTex: Cardinal;
  CursorSize: Single;

procedure SetCursor(Tex: Cardinal; Size: Single);
begin
  CursorTex:=Tex;
  CursorSize:=Size;
end;

procedure DrawCursor;
var
   P: TPoint;
begin
  glPushMatrix;
  glPushAttrib(GL_ENABLE_BIT or GL_TEXTURE_BIT or GL_CURRENT_BIT or GL_COLOR_BUFFER_BIT or GL_LIGHTING_BIT);
  gleOrthoMatrix(Game.ResX, Game.ResY);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, CursorTex);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glDisable(GL_COLOR_MATERIAL);
  GetCursorPos(P);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 1); glVertex2f(P.X, P.Y);
    glTexCoord2f(0, 0); glVertex2f(P.X, P.Y+CursorSize);
    glTexCoord2f(1, 0); glVertex2f(P.X+CursorSize, P.Y+CursorSize);
    glTexCoord2f(1, 1); glVertex2f(P.X+CursorSize, P.Y);
  glEnd;
  glPopAttrib;
  glPopMatrix;
end;

end.