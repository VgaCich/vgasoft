unit UGUI;

interface

uses
  Windows, AvL, avlUtils, dglOpenGL, OpenGLExt, GameStates, CollisionCheck;

type
  TRegionEvent=(reMouseEnter, reMouseLeave, reMouseMove, reMouseDown, reMouseUp,
    reMouseClick, reMouseWheel, reChar, reKeyDown, reKeyUp);
  TRegionHandler=procedure(Event: TRegionEvent; Button, X, Y, Tag: Integer) of object;
  TRegion=record
    Rect: TRect;
    Handler: TRegionHandler;
    Tag: Integer;
  end;
  TMouseButtons=array[0..2] of Integer;
  TGUI=class;
  TGUIWidget=class
  protected
    FRect: TRect;
    FTag, FWidgetActive, FRegionActive, FWidgetLastActive, FRegionLastActive: Integer;
    FDestroying: Boolean;
    FParent: TGUIWidget;
    FParentGUI: TGUI;
    FWidgets: TList;
    FRegions: array of TRegion;
    procedure SetRect(ARect: TRect);
    procedure AdjustRect(ARect: TRect); virtual;
    procedure AddWidget(Widget: TGUIWidget);
    procedure DeleteWidget(Widget: TGUIWidget);
    procedure SetParentGUI(AGUI: TGUI);
    function  WidgetAt(Point: TPoint): Integer;
    function  RegionAt(Point: TPoint): Integer;
    procedure SendEvent(Region, Widget: Integer; Event: TRegionEvent; Button, X, Y: Integer);
    procedure SetActive(Region, Widget: Integer);
    procedure EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer); virtual;
    procedure DrawWidget;
    procedure Draw; virtual; abstract;
  public
    constructor Create(Rect: TRect; Parent: TGUIWidget);
    destructor Destroy; override;
    property Rect: TRect read FRect write SetRect;
    property Tag: Integer read FTag write FTag;
  end;
  TGUI=class
  private
    FVSW, FVSH, FActive, FLastActive, FModal: Integer;
    FLast: TMouseButtons;
    FFocus: TGUIWidget;
    FForms: TList;
  protected
    procedure MapCursor(var Cursor: TPoint);
    function  FormAt(Point: TPoint): Integer;
    procedure SendEvent(Form: Integer; Event: TRegionEvent; Button, X, Y: Integer);
  public
    constructor Create(VirtScreenWidth, VirtScreenHeight: Integer);
    destructor Destroy; override;
    procedure Update;
    procedure Draw;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
    procedure KeyEvent(Button: Integer; Event: TKeyEvent);
    procedure CharEvent(C: Char);
    procedure AddForm(Form: TGUIWidget);
    procedure DeleteForm(Form: TGUIWidget);
    procedure GrabFocus(Widget: TGUIWidget);
    procedure SetModal(Form: TGUIWidget);
  end;

procedure SetCursor(Tex: Cardinal; Size: Single);
procedure DrawCursor;

implementation

uses
  UGame, ULog;

{TGUIWidget}

constructor TGUIWidget.Create(Rect: TRect; Parent: TGUIWidget);
begin
  inherited Create;
  FRect:=Rect;
  FWidgets:=TList.Create;
  FParent:=Parent;
  FWidgetActive:=-1;
  FRegionActive:=-1;
  FWidgetLastActive:=-1;
  FRegionLastActive:=-1;
  if Assigned(FParent) then FParent.AddWidget(Self);
end;

destructor TGUIWidget.Destroy;
var
  i: Integer;
begin
  FDestroying:=true;
  for i:=0 to FWidgets.Count-1 do
    TObject(FWidgets[i]).Free;
  FAN(FWidgets);
  Finalize(FRegions);
  if Assigned(FParent) then FParent.DeleteWidget(Self);
  inherited Destroy;
end;

procedure TGUIWidget.SetRect(ARect: TRect);
begin
  FRect:=ARect;
  if Assigned(FParent) then AdjustRect(FParent.Rect);
end;

procedure TGUIWidget.AdjustRect(ARect: TRect);
var
  i: Integer;
begin
  if FRect.Left<ARect.Left then FRect.Left:=ARect.Left;
  if FRect.Top<ARect.Top then FRect.Top:=ARect.Top;
  if FRect.Right>ARect.Right then FRect.Right:=ARect.Right;
  if FRect.Bottom>ARect.Bottom then FRect.Bottom:=ARect.Bottom;
  for i:=0 to FWidgets.Count-1 do TGUIWidget(FWidgets[i]).AdjustRect(FRect);
end;

procedure TGUIWidget.AddWidget(Widget: TGUIWidget);
begin
  FWidgets.Insert(0, Widget);
  Widget.SetParentGUI(FParentGUI);
  Widget.AdjustRect(FRect);
end;

procedure TGUIWidget.DeleteWidget(Widget: TGUIWidget);
begin
  if not FDestroying then FWidgets.Remove(Widget);
end;

procedure TGUIWidget.SetParentGUI(AGUI: TGUI);
var
  i: Integer;
begin
  FParentGUI:=AGUI;
  for i:=0 to FWidgets.Count-1 do TGUIWidget(FWidgets[i]).SetParentGUI(AGUI);
end;

function TGUIWidget.WidgetAt(Point: TPoint): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to FWidgets.Count-1 do
    if PointInRect(Point, TGUIWidget(FWidgets[i]).Rect)
      then Result:=i;
end;

function TGUIWidget.RegionAt(Point: TPoint): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FRegions) do
    if PointInRect(Point, FRegions[i].Rect)
      then Result:=i;
end;

procedure TGUIWidget.SendEvent(Region, Widget: Integer; Event: TRegionEvent; Button, X, Y: Integer);
begin
  if (Region>=0) and (Region<=High(FRegions))
    then FRegions[Region].Handler(Event, Button, X, Y, FRegions[Region].Tag);
  if (Widget>=0) and (Widget<FWidgets.Count)
    then TGUIWidget(FWidgets[Widget]).EventHandler(Event, Button, X, Y, TGUIWidget(FWidgets[Widget]).Tag);
end;

procedure TGUIWidget.SetActive(Region, Widget: Integer);
begin
  FRegionLastActive:=FRegionActive;
  FRegionActive:=Region;
  FWidgetLastActive:=FWidgetActive;
  FWidgetActive:=Widget;
end;

procedure TGUIWidget.EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer);
var
  Widget, Region: Integer;
  Cursor: TPoint;
begin
  Cursor.X:=X;
  Cursor.Y:=Y;
  Widget:=WidgetAt(Cursor);
  Region:=RegionAt(Cursor);
  case Event of
    reMouseEnter, reMouseDown, reMouseUp, reMouseClick, reMouseWheel: begin
      SendEvent(Region, Widget, Event, Button, X, Y);
      if Event=reMouseEnter then SetActive(Region, Widget);
    end;
    reMouseMove: begin
      SetActive(Region, Widget);
      if FRegionActive<>FRegionLastActive then
      begin
        SendEvent(FRegionLastActive, -1, reMouseLeave, 0, 0, 0);
        SendEvent(FRegionActive, -1, reMouseEnter, 0, X, Y);
      end
        else SendEvent(FRegionActive, -1, reMouseMove, 0, X, Y);
      if FWidgetActive<>FWidgetLastActive then
      begin
        SendEvent(-1, FWidgetLastActive, reMouseLeave, 0, 0, 0);
        SendEvent(-1, FWidgetActive, reMouseEnter, 0, X, Y);
      end
        else SendEvent(-1, FWidgetActive, reMouseMove, 0, X, Y);
    end;
    reMouseLeave: begin
      SendEvent(FRegionActive, FWidgetActive, reMouseLeave, 0, 0, 0);
      SetActive(-1, -1);
    end;
  end;
end;

procedure TGUIWidget.DrawWidget;
var
  i, X, Y, W, H: Integer;
begin
{  X:=Round(Rect.Left/FParentGUI.FVSW*Game.ResX);
  W:=Round((Rect.Right-Rect.Left)/FParentGUI.FVSW*Game.ResX);
  Y:=Round((FParentGUI.FVSH-Rect.Bottom)/FParentGUI.FVSH*Game.ResY);
  H:=Round((Rect.Bottom-Rect.Top)/FParentGUI.FVSH*Game.ResY);
  glScissor(X, Y, W, H); }
  Draw;
  for i:=0 to FWidgets.Count-1 do
    TGUIWidget(FWidgets[i]).DrawWidget;
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
  FModal:=-1;
  FFocus:=nil;
  FForms:=TList.Create;
end;

destructor TGUI.Destroy;
var
  i: Integer;
begin
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
  if FModal>-1 then
  begin
    if PointInRect(Point, TGUIWidget(FForms[FModal]).Rect) then Result:=FModal;
    Exit;
  end;
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
begin
  GetCursorPos(Cursor);
  MapCursor(Cursor);
  FLastActive:=FActive;
  FActive:=FormAt(Cursor);
  if FActive<>FLastActive then
  begin
    SendEvent(FLastActive, reMouseLeave, 0, 0, 0);
    SendEvent(FActive, reMouseEnter, 0, Cursor.X, Cursor.Y);
  end
end;

procedure TGUI.Draw;
var
  i: Integer;
begin
  glPushMatrix;
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  gleOrthoMatrix(FVSW, FVSH);
//  glEnable(GL_SCISSOR_TEST);
  for i:=0 to FForms.Count-1 do TGUIWidget(FForms[i]).DrawWidget;
  glPopAttrib;
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
      if Button<3 then FLast[Button]:=FActive;
      SendEvent(Form, reMouseDown, Button, Cursor.X, Cursor.Y);
    end;
    meUp: begin
      SendEvent(Form, reMouseUp, Button, Cursor.X, Cursor.Y);
      if (FActive>=0) and (Button<3) and (FLast[Button]=FActive)
        then SendEvent(Form, reMouseClick, Button, Cursor.X, Cursor.Y)
        else FLast[Button]:=-1;
    end;
    meMove: SendEvent(FActive, reMouseMove, 0, Cursor.X, Cursor.Y);
    meWheel: SendEvent(Form, reMouseWheel, Button, Cursor.X, Cursor.Y);
  end;
end;

procedure TGUI.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if Assigned(FFocus) then
  case Event of
    keUp: FFocus.EventHandler(reKeyUp, Button, 0, 0, FFocus.Tag);
    keDown: FFocus.EventHandler(reKeyDown, Button, 0, 0, FFocus.Tag);
  end;
end;

procedure TGUI.CharEvent(C: Char);
begin
  if Assigned(FFocus)
    then FFocus.EventHandler(reChar, Integer(C), 0, 0, FFocus.Tag);
end;

procedure TGUI.AddForm(Form: TGUIWidget);
var
  ModalTemp: Pointer;
begin
  if FForms.IndexOf(Form)>-1 then Exit;
  ModalTemp:=nil;
  if FModal>-1 then ModalTemp:=FForms[FModal];
  FForms.Insert(0, Form);
  FModal:=FForms.IndexOf(ModalTemp);
  Form.SetParentGUI(Self);
  Form.AdjustRect(Rect(0, 0, FVSW, FVSH));
end;

procedure TGUI.DeleteForm(Form: TGUIWidget);
var
  ModalTemp: Pointer;
begin
  ModalTemp:=nil;
  if FModal>-1 then ModalTemp:=FForms[FModal];
  FForms.Remove(Form);
  FModal:=FForms.IndexOf(ModalTemp);
end;

procedure TGUI.GrabFocus(Widget: TGUIWidget);
begin
  FFocus:=Widget;
end;

procedure TGUI.SetModal(Form: TGUIWidget);
begin
  FModal:=FForms.IndexOf(Form);
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