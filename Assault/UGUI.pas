unit UGUI;

interface

uses
  Windows, AvL, avlUtils, dglOpenGL, OpenGLExt, GameStates, CollisionCheck;

type
  TRegionEvent=(reMouseEnter, reMouseLeave, reMouseMove, reMouseDown, reMouseUp,
    reMouseClick, reMouseWheel, reChar, reKeyDown, reKeyUp);
  TRegionHandler=procedure(Event: TRegionEvent; Button, X, Y, Tag: Integer) of object;
  TRegion=record
    Left, Top, Width, Height: Cardinal;
    Handler: TRegionHandler;
    Tag: Integer;
  end;
  TMouseButtons=array[0..2] of Integer;
  TGUI=class;
  TGUIWidget=class
  protected
    FLeft, FTop, FWidth, FHeight: Integer;
    FTag, FWidgetActive, FRegionActive, FWidgetLastActive, FRegionLastActive: Integer;
    FDestroying, FEnabled: Boolean;
    FParent: TGUIWidget;
    FParentGUI: TGUI;
    FWidgetLast, FRegionLast: TMouseButtons;
    FWidgets: TList;
    FRegions: array of TRegion;
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    procedure AdjustRect(AWidth, AHeight: Integer); virtual;
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
    constructor Create(Left, Top, Width, Height: Integer; Parent: TGUIWidget);
    destructor Destroy; override;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Tag: Integer read FTag write FTag;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;
  TGUI=class
  private
    FVSW, FVSH, FActive, FLastActive, FModal: Integer;
    FLast: TMouseButtons;
    FFocus: TGUIWidget;
    FForms: TList;
    FEnabled: Boolean;
  protected
    procedure SetEnabled(Value: Boolean);
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
    procedure SetTopForm(Form: TGUIWidget);
    property VirtScreenWidth: Integer read FVSW;
    property VirtScreenHeight: Integer read FVSH;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

procedure SetCursor(Tex: Cardinal; Size: Single);
procedure DrawCursor;

implementation

{$B-}

uses
  UGame, ULog;

{TGUIWidget}

constructor TGUIWidget.Create(Left, Top, Width, Height: Integer; Parent: TGUIWidget);
begin
  inherited Create;
  FLeft:=Left;
  FTop:=Top;
  FWidth:=Width;
  FHeight:=Height;
  FEnabled:=true;
  FWidgets:=TList.Create;
  FParent:=Parent;
  FWidgetActive:=-1;
  FRegionActive:=-1;
  FWidgetLastActive:=-1;
  FRegionLastActive:=-1;
  FRegionLast[0]:=-1;
  FRegionLast[1]:=-1;
  FRegionLast[2]:=-1;
  FWidgetLast[0]:=-1;
  FWidgetLast[1]:=-1;
  FWidgetLast[2]:=-1;
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

procedure TGUIWidget.SetLeft(Value: Integer);
begin
  if Value<>FLeft then
  begin
    FLeft:=Value;
    if Assigned(FParent)
      then AdjustRect(FParent.Width, FParent.Height)
      else AdjustRect(FParentGUI.VirtScreenWidth, FParentGUI.VirtScreenHeight);
  end;
end;

procedure TGUIWidget.SetTop(Value: Integer);
begin
  if Value<>FTop then
  begin
    FTop:=Value;
    if Assigned(FParent)
      then AdjustRect(FParent.Width, FParent.Height)
      else AdjustRect(FParentGUI.VirtScreenWidth, FParentGUI.VirtScreenHeight);
  end;
end;

procedure TGUIWidget.SetWidth(Value: Integer);
begin
  if Value<>FWidth then
  begin
    FWidth:=Value;
    if Assigned(FParent)
      then AdjustRect(FParent.Width, FParent.Height)
      else AdjustRect(FParentGUI.VirtScreenWidth, FParentGUI.VirtScreenHeight);
  end;
end;

procedure TGUIWidget.SetHeight(Value: Integer);
begin
  if Value<>FHeight then
  begin
    FLeft:=Value;
    if Assigned(FParent)
      then AdjustRect(FParent.Width, FParent.Height)
      else AdjustRect(FParentGUI.VirtScreenWidth, FParentGUI.VirtScreenHeight);
  end;
end;

procedure TGUIWidget.SetEnabled(Value: Boolean);
var
  i: Integer;
begin
  FEnabled:=Value;
  for i:=0 to FWidgets.Count-1 do
    TGUIWidget(FWidgets[i]).Enabled:=Value;
end;

procedure TGUIWidget.AdjustRect(AWidth, AHeight: Integer);
var
  i: Integer;
begin
  if FLeft+FWidth>AWidth then FLeft:=AWidth-FWidth;
  if FLeft<0 then FLeft:=0;
  if FTop+FHeight>AHeight then FTop:=AHeight-FHeight;
  if FTop<0 then FTop:=0;
  if FWidth>AWidth-FLeft then FWidth:=AWidth-FLeft;
  if FHeight>AHeight-FTop then FHeight:=AHeight-FTop;
  for i:=0 to FWidgets.Count-1 do TGUIWidget(FWidgets[i]).AdjustRect(Width, Height);
end;

procedure TGUIWidget.AddWidget(Widget: TGUIWidget);
begin
  FWidgets.Insert(0, Widget);
  Widget.SetParentGUI(FParentGUI);
  Widget.AdjustRect(FWidth, FHeight);
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
    with TGUIWidget(FWidgets[i]) do
      if PointInRect(Point, Rect(Left, Top, Left+Width, Top+Height)) then
      begin
        Result:=i;
        Exit;
      end;
end;

function TGUIWidget.RegionAt(Point: TPoint): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FRegions) do
    with FRegions[i] do
      if PointInRect(Point, Rect(Left, Top, Left+Width, Top+Height)) then
      begin
        Result:=i;
        Exit;
      end;
end;

procedure TGUIWidget.SendEvent(Region, Widget: Integer; Event: TRegionEvent; Button, X, Y: Integer);
begin
  if (Region>=0) and (Region<=High(FRegions)) then
    with FRegions[Region] do
      Handler(Event, Button, X-Left, Y-Top, Tag);
  if (Widget>=0) and (Widget<FWidgets.Count) and TGUIWidget(FWidgets[Widget]).Enabled then
    with TGUIWidget(FWidgets[Widget]) do
      EventHandler(Event, Button, X-Left, Y-Top, Tag);
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
begin
  if not FEnabled then Exit;
  Widget:=WidgetAt(Point(X, Y));
  Region:=RegionAt(Point(X, Y));
  case Event of
    reMouseEnter, reMouseDown, reMouseUp, reMouseWheel: begin
      SendEvent(Region, Widget, Event, Button, X, Y);
      if Event=reMouseEnter then SetActive(Region, Widget);
      if (Button>0) and (Button<=3) then
        case Event of
          reMouseDown: begin
            FWidgetLast[Button-1]:=Widget;
            FRegionLast[Button-1]:=Region;
          end;
          reMouseUp: begin
            if FWidgetLast[Button-1]<>Widget then Widget:=-1;
            if FRegionLast[Button-1]<>Region then Region:=-1;
            FWidgetLast[Button-1]:=-1;
            FRegionLast[Button-1]:=-1;
            SendEvent(Region, Widget, reMouseClick, Button, X, Y)
          end;
        end;
    end;
    reMouseMove: begin
      SetActive(Region, Widget);
      if FRegionActive<>FRegionLastActive then
      begin
        SendEvent(FRegionLastActive, -1, reMouseLeave, 0, X, Y);
        SendEvent(FRegionActive, -1, reMouseEnter, 0, X, Y);
      end
        else SendEvent(FRegionActive, -1, reMouseMove, 0, X, Y);
      if FWidgetActive<>FWidgetLastActive then
      begin
        SendEvent(-1, FWidgetLastActive, reMouseLeave, 0, X, Y);
        SendEvent(-1, FWidgetActive, reMouseEnter, 0, X, Y);
      end
        else SendEvent(-1, FWidgetActive, reMouseMove, 0, X, Y);
    end;
    reMouseLeave: begin
      SendEvent(FRegionActive, FWidgetActive, reMouseLeave, 0, X, Y);
      SetActive(-1, -1);
    end;
  end;
end;

procedure TGUIWidget.DrawWidget;
var
  i: Integer;
begin
  if not FEnabled then Exit;
  glPushMatrix;
  glTranslatef(Left, Top, 0);
  Draw;
  for i:=0 to FWidgets.Count-1 do
    TGUIWidget(FWidgets[i]).DrawWidget;
  glPopMatrix;
end;

{TGUI}

constructor TGUI.Create(VirtScreenWidth, VirtScreenHeight: Integer);
begin
  inherited Create;
  FVSW:=VirtScreenWidth;
  FVSH:=VirtScreenHeight;
  FEnabled:=true;
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

procedure TGUI.SetEnabled(Value: Boolean);
var
  i: Integer;
begin
  FEnabled:=Value;
  for i:=0 to FForms.Count-1 do
    TGUIWidget(FForms[i]).Enabled:=Value;
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
    with TGUIWidget(FForms[FModal]) do
      if PointInRect(Point, Rect(Left, Top, Left+Width, Top+Height)) then
      begin
        Result:=FModal;
        Exit;
      end;
  for i:=0 to FForms.Count-1 do
    with TGUIWidget(FForms[i]) do
      if PointInRect(Point, Rect(Left, Top, Left+Width, Top+Height)) then
      begin
        Result:=i;
        Exit;
      end;
end;

procedure TGUI.SendEvent(Form: Integer; Event: TRegionEvent; Button, X, Y: Integer);
begin
  if (Form<0) or (Form>=FForms.Count) and not TGUIWidget(FForms[Form]).Enabled then Exit;
  with TGUIWidget(FForms[Form]) do
    EventHandler(Event, Button, X-Left, Y-Top, Tag);
end;
                   
procedure TGUI.Update;
var
  Cursor: TPoint;
begin
  if not FEnabled then Exit;
  GetCursorPos(Cursor);
  MapCursor(Cursor);
  FLastActive:=FActive;
  FActive:=FormAt(Cursor);
  if FActive<>FLastActive then
  begin
    SendEvent(FLastActive, reMouseLeave, 0, Cursor.X, Cursor.Y);
    SendEvent(FActive, reMouseEnter, 0, Cursor.X, Cursor.Y);
  end
end;

procedure TGUI.Draw;
var
  i: Integer;
begin
  if not FEnabled then Exit;
  glPushMatrix;
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  gleOrthoMatrix(FVSW, FVSH);
  for i:=FForms.Count-1 downto 0 do TGUIWidget(FForms[i]).DrawWidget;
  glPopAttrib;
  glPopMatrix;
end;

procedure TGUI.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
var
  Cursor: TPoint;
  Form: Integer;
begin
  if not FEnabled then Exit;
  Cursor.X:=X;
  Cursor.Y:=Y;
  MapCursor(Cursor);
  Form:=FormAt(Cursor);
  case Event of
    meDown: begin
      if (Button>0) and (Button<=3) then FLast[Button-1]:=FActive;
      SendEvent(Form, reMouseDown, Button, Cursor.X, Cursor.Y);
    end;
    meUp: begin
      SendEvent(Form, reMouseUp, Button, Cursor.X, Cursor.Y);
      if (Button>0) and (Button<=3) then
      begin
        if (FActive>=0) and (FLast[Button-1]=FActive)
          then SendEvent(Form, reMouseClick, Button, Cursor.X, Cursor.Y);
        FLast[Button-1]:=-1;
      end;
    end;
    meMove: SendEvent(FActive, reMouseMove, 0, Cursor.X, Cursor.Y);
    meWheel: SendEvent(Form, reMouseWheel, Button, Cursor.X, Cursor.Y);
  end;
end;

procedure TGUI.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if not FEnabled then Exit;
  if Assigned(FFocus) and FFocus.Enabled then
  case Event of
    keUp: FFocus.EventHandler(reKeyUp, Button, 0, 0, FFocus.Tag);
    keDown: FFocus.EventHandler(reKeyDown, Button, 0, 0, FFocus.Tag);
  end;
end;

procedure TGUI.CharEvent(C: Char);
begin
  if not FEnabled then Exit;
  if Assigned(FFocus) and FFocus.Enabled
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
  Form.AdjustRect(FVSW, FVSH);
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

procedure TGUI.SetTopForm(Form: TGUIWidget);
begin
  FForms.Remove(Form);
  FForms.Insert(0, Form);
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