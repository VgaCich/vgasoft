unit VSEGUI;

interface

uses
  Windows, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, VSETexMan,
  VSEGameStates, VSECollisionCheck;

type
  TBtnType=(btPush, btCheck, btRadio); //Button type: push button, check box, radio button
  PBtn=^TBtn;
  TGUIOnClick=procedure(Btn: PBtn) of object;
  TBtn=record //Button
    Caption: string; //Button caption
    Type_: TBtnType; //Button type
    X, Y, Width, Height, Group, Tag: Integer; //X, Y, Width, Height: button bounds; Group: radio button group; Tag: custom information
    OnClick: TGUIOnClick; //Button click event handler, don't override if Typ=btCheck or Typ=btRadio
    Checked, Enabled: Boolean; //Checked: true if check box or radio button checked; Enabled: enable button
  end;
  TLabelAlign=(laLeft, laCenter, laRight); //Label aligning
  PLbl=^TLbl;
  TLbl=record //Label
    Caption: string; //Label text
    X, Y, Width: Integer; //Label bounds (height depends from form font)
    Align: TLabelAlign; //Text aligning
    Color: TColor; //Text color
  end;
  TBtnStates=(bsHilight, bsPushed, bsTabStop); //Button state - highlighted (mouse over), pushed, selected from keyboard
  TBtnState=set of TBtnStates;
  TVirtScreenAlign=(vsaLeftTop, vsaCenter, vsaRightBottom);
  TMenuItem=record //Menu item
    Caption: string; //Item button caption
    Tag: Integer; //Item button tag
    OnClick: TGUIOnClick; //Item nutton click event handler
  end;
  TMenuItems=array of Integer;
  TGUIForm=class //Form
  private
    FVirtScreenAlign: TVirtScreenAlign;
    FVirtScreenVAlign: TVirtScreenAlign;
    FVSWider: Boolean; //internally used
    FVSScale, FVSDelta: Double; //internally used
    FActive, FLastActive, FLast, FTabStop: Integer; //internally used
    FButtons: array of TBtn; //internally used
    FRects: array of TRect; //internally used
    FLabels: array of TLbl; //internally used
  protected
    FVirtScrW, FVirtScrH: Integer; //Virtual screen resolution
    FCaption: string; //Form caption
    FX, FY, FWidth, FHeight: Integer; //Form position and size
    FFont: Cardinal; //Form font
    function  GetButton(Index: Integer): PBtn;
    function  GetLabel(Index: Integer): PLbl;
    procedure CheckClick(Btn: PBtn);
    procedure RadioClick(Btn: PBtn);
    procedure MapCursor(var Cursor: TPoint);
    function  BtnAt(Point: TPoint): Integer;
    procedure PaintRect(Rect: TRect); //Paints rectangle frame
    procedure FillRect(Rect: TRect); //Paints rectangle
    procedure DrawForm; dynamic; //Override for custom form drawing
    procedure DrawButton(const Btn: TBtn; State: TBtnState); dynamic; //Override for custom buttons drawing
    procedure DrawRect(const Rect: TRect); dynamic; //Override for custom rectangles drawing
    procedure DrawLabel(const Lbl: TLbl); dynamic; //Override for custom labels drawing
  public
    constructor Create(VirtScrW, VirtScrH, X, Y, Width, Height: Integer; Font: Cardinal); //Creates form; VertScr*: virtual screen resolution (all dimensions defined in virtual screen coordinates); X, Y, Width, Height: form bounds; Font: form font
    destructor Destroy; override;
    function  AddButton(Btn: TBtn): Integer; //Add button, returns button index
    function  AddLabel(const Lbl: TLbl): Integer; //Add label, returns label index
    procedure AddRect(const Rect: TRect); //Add rectangle (visual frame)
    procedure Draw; //Draw form
    procedure Update; //dynamic; //Update form
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); dynamic; //Process mouse event
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); dynamic;
    procedure CharEvent(C: Char); dynamic; //Process char event
    property Button[Index: Integer]: PBtn read GetButton; //Buttons array
    property Lbl[Index: Integer]: PLbl read GetLabel; //Labels array
    property Caption: string read FCaption write FCaption; //Form caption
    property VirtScreenAlign: TVirtScreenAlign read FVirtScreenAlign write FVirtScreenAlign; //Virtual screen align
    property VirtScreenVAlign: TVirtScreenAlign read FVirtScreenVAlign write FVirtScreenVAlign; //Virtual screen vertical align
  end;

function CreateSelect(Form: TGUIForm; X, Y, Width, Height: Integer; OnChange: TGUIOnClick; const PrevCaption, NextCaption: string): Integer; //Creates select control, returns select state label index; distinguish prev & next buttons in handler by Btn^.Tag (-1 for prev, 1 for next)
function CreateMenu(Form: TGUIForm; X, Y, BtnWidth, BtnHeight, BtnSpacing: Integer; Items: array of TMenuItem): TMenuItems; //Creates menu from buttons, returns butttons' indexes

implementation

uses
  VSECore;

const
  DeltaLeft: array[TVirtScreenAlign] of Double=(0, -1, -2);
  DeltaRight: array[TVirtScreenAlign] of Double=(2, 1, 0);

constructor TGUIForm.Create(VirtScrW, VirtScrH, X, Y, Width, Height: Integer; Font: Cardinal);
begin
  inherited Create;
  FVirtScrW:=VirtScrW;
  FVirtScrH:=VirtScrH;
  FVirtScreenAlign:=vsaCenter;
  FVirtScreenVAlign:=vsaCenter;
  FX:=X;
  FY:=Y;
  FWidth:=Width;
  FHeight:=Height;
  FFont:=Font;
  FActive:=-1;
  FLastActive:=-1;
  FLast:=-1;
  FTabStop:=-1;
end;

destructor TGUIForm.Destroy;
begin
  Finalize(FButtons);
  Finalize(FRects);
end;

function TGUIForm.AddButton(Btn: TBtn): Integer;
begin
  Result:=Length(FButtons);
  SetLength(FButtons, Result+1);
  if Btn.Type_=btCheck then Btn.OnClick:=CheckClick;
  if Btn.Type_=btRadio then Btn.OnClick:=RadioClick;
  FButtons[Result]:=Btn;
end;

function TGUIForm.AddLabel(const Lbl: TLbl): Integer;
begin
  Result:=Length(FLabels);
  SetLength(FLabels, Result+1);
  FLabels[Result]:=Lbl;
end;

procedure TGUIForm.AddRect(const Rect: TRect);
begin
  SetLength(FRects, Length(FRects)+1);
  FRects[High(FRects)]:=Rect;
end;

procedure TGUIForm.Draw;

  function BtnState(Index: Integer): TBtnState;
  begin
    Result:=[];
    if Index=FActive then Result:=Result + [bsHilight];
    if Index=FLast then Result:=Result + [bsPushed];
    if Index=FTabStop then Result:=Result + [bsTabStop];
  end;

var
  i: Integer;
begin
  glPushMatrix;
  glPushAttrib(GL_ENABLE_BIT or GL_LINE_BIT or GL_LIGHTING_BIT or GL_CURRENT_BIT or GL_COLOR_BUFFER_BIT);
  if FVSWider
    then gleOrthoMatrix2(0, DeltaLeft[FVirtScreenVAlign]*FVSDelta, FVirtScrW, FVirtScrH+DeltaRight[FVirtScreenVAlign]*FVSDelta)
    else gleOrthoMatrix2(DeltaLeft[FVirtScreenAlign]*FVSDelta, 0, FVirtScrW+DeltaRight[FVirtScreenAlign]*FVSDelta, FVirtScrH);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_COLOR_MATERIAL);
  DrawForm;
  for i:=0 to High(FRects) do DrawRect(FRects[i]);
  for i:=0 to High(FLabels) do DrawLabel(FLabels[i]);
  for i:=0 to High(FButtons) do DrawButton(FButtons[i], BtnState(i));
  glPopAttrib;
  glPopMatrix;
end;

procedure TGUIForm.Update;
var
  Cursor: TPoint;
begin
  FVSWider:=FVirtScrW/FVirtScrH>Core.ResolutionX/Core.ResolutionY;
  if FVSWider then
  begin
    FVSScale:=Core.ResolutionX/FVirtScrW;
    FVSDelta:=(Core.ResolutionY/FVSScale-FVirtScrH)/2;
  end
  else begin
    FVSScale:=Core.ResolutionY/FVirtScrH;
    FVSDelta:=(Core.ResolutionX/FVSScale-FVirtScrW)/2;
  end;
  Cursor:=Core.MouseCursor;
  MapCursor(Cursor);
  FLastActive:=FActive;
  FActive:=BtnAt(Cursor);
end;

procedure TGUIForm.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
var
  Cursor: TPoint;
begin
  Cursor.X:=X;
  Cursor.Y:=Y;
  MapCursor(Cursor);
  case Event of
    meDown: begin
      if Button=1 then FLast:=FActive;
      FTabStop:=-1;
    end;
    meUp: begin
      if Button=1 then
      begin
        if (FActive>=0) and (FLast=FActive) and FButtons[FActive].Enabled and Assigned(FButtons[FActive].OnClick) then
        begin
          FLast:=-1;
          FButtons[FActive].OnClick(@FButtons[FActive]);
        end
          else FLast:=-1;
      end;
      FTabStop:=-1;
    end;
  end;
end;

procedure TGUIForm.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Event=keDown) then
  begin
    case Key of
      VK_TAB: if Core.KeyPressed[VK_SHIFT]
        then Dec(FTabStop)
        else Inc(FTabStop);
      VK_UP: Dec(FTabStop);
      VK_DOWN: Inc(FTabStop);
      VK_SHIFT, VK_SPACE:;
      else begin
        FTabStop:=-1;
        Exit;
      end;
    end;
    if FTabStop<0 then FTabStop:=High(FButtons);
    if FTabStop>High(FButtons) then FTabStop:=0;
  end
    else
      if (Key=VK_SPACE) and FButtons[FTabStop].Enabled and Assigned(FButtons[FTabStop].OnClick) then
        begin
          FButtons[FTabStop].OnClick(@FButtons[FTabStop]);
          Exit;
        end;
end;

procedure TGUIForm.CharEvent(C: Char);
begin

end;

procedure TGUIForm.PaintRect(Rect: TRect);
begin
  glBegin(GL_LINE_LOOP);
    glVertex(FX+Rect.Left, FY+Rect.Top);
    glVertex(FX+Rect.Right, FY+Rect.Top);
    glVertex(FX+Rect.Right, FY+Rect.Bottom);
    glVertex(FX+Rect.Left, FY+Rect.Bottom);
  glEnd;
end;

procedure TGUIForm.FillRect(Rect: TRect);
begin
  glBegin(GL_QUADS);
    glVertex(FX+Rect.Left, FY+Rect.Top);
    glVertex(FX+Rect.Right, FY+Rect.Top);
    glVertex(FX+Rect.Right, FY+Rect.Bottom);
    glVertex(FX+Rect.Left, FY+Rect.Bottom);
  glEnd;
end;

procedure TGUIForm.DrawForm;
begin
  glBlendFunc(GL_ONE, GL_ZERO);
  glLineWidth(1);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_COLOR_MATERIAL);
  glColor3f(0.5, 1, 0.5);
  FillRect(Rect(0, 0, FWidth, FHeight));
  glColor3f(0, 0.9, 0.5);
  FillRect(Rect(0, 0, FWidth, TexMan.TextHeight(FFont)+6));
  glColor3f(1, 1, 0);
  PaintRect(Rect(0, 0, FWidth, FHeight));
//  glColor3d(1, 1, 0);
  TexMan.TextOut(FFont, FX+5, FY+3, FCaption);
end;

procedure TGUIForm.DrawButton(const Btn: TBtn; State: TBtnState);
var
  TextX, TextY: Integer;
  Text: string;
begin
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_COLOR_MATERIAL);
  case Btn.Type_ of
    btPush:
      begin
        glLineWidth(2);
        TextX:=Btn.X+Btn.Width;
        TextY:=Btn.Y+Btn.Height;
        if (bsHilight in State) and Btn.Enabled
          then glColor3f(1, 0.5, 0.5)
          else glColor3f(0.5, 1, 0.5);
        FillRect(Rect(Btn.X, Btn.Y, TextX, TextY));
        if bsHilight in State
          then glColor3f(1, 0, 0)
          else glColor3f(0, 1, 0);
        if not Btn.Enabled then glColor3f(0.3, 1, 0.3);
        PaintRect(Rect(Btn.X, Btn.Y, TextX, TextY));
        TextX:=Max((Btn.Width-TexMan.TextWidth(FFont, Btn.Caption)) div 2, 0);
        TextY:=(Btn.Height-TexMan.TextHeight(FFont)) div 2;
        if bsPushed in State
          then glColor3f(0, 1, 0)
          else glColor3f(0, 0.7, 0);
      end;
    btCheck, btRadio:
      begin
        glLineWidth(2);
        TextX:=Btn.X+Btn.Height;
        TextY:=Btn.Y+Btn.Height;
        if (bsHilight in State) and Btn.Enabled
          then glColor3f(1, 0.5, 0.5)
          else glColor3f(0, 1, 0);
        if Btn.Checked then FillRect(Rect(Btn.X+3, Btn.Y+3, TextX-3, TextY-3));
        if bsHilight in State
          then glColor3f(1, 0, 0)
          else glColor3f(0, 1, 0);
        if not Btn.Enabled then glColor3f(0.3, 1, 0.3);
        PaintRect(Rect(Btn.X, Btn.Y, TextX, TextY));
        TextX:=Min(Btn.Height+5, Btn.Width);
        TextY:=(Btn.Height-TexMan.TextHeight(FFont)) div 2;
        if bsHilight in State
          then glColor3f(0, 0.8, 0)
          else glColor3f(0, 0.7, 0);
      end;
  end;
  Text:=Btn.Caption;
  while (Text<>'') and (TexMan.TextWidth(FFont, Text)+TextX>Btn.Width) do Delete(Text, Length(Text), 1);
  if not Btn.Enabled then glColor3f(0.3, 1, 0.3);
  TexMan.TextOut(FFont, FX+Btn.X+TextX, FY+Btn.Y+TextY, Text);
  if bsTabStop in State then
  begin
    glLineStipple(1, $F0F0);
    glEnable(GL_LINE_STIPPLE);
    glColor(0.5, 0, 0);
    PaintRect(Rect(Btn.X, Btn.Y, Btn.X+Btn.Width, Btn.Y+Btn.Height));
    glDisable(GL_LINE_STIPPLE);
  end;
end;

procedure TGUIForm.DrawRect(const Rect: TRect);
begin
  glLineWidth(1);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_COLOR_MATERIAL);
  glColor3f(0, 1, 0);
  PaintRect(Rect);
end;

procedure TGUIForm.DrawLabel(const Lbl: TLbl);
var
  Text: string;
  TextX: Integer;
begin
  with Lbl do
  begin
    gleColor(Color);
    Text:=Caption;
    while (Text<>'') and (TexMan.TextWidth(FFont, Text)>Width) do Delete(Text, Length(Text), 1);
    case Align of
      laLeft: TextX:=X;
      laCenter: TextX:=X+(Width-TexMan.TextWidth(FFont, Text)) div 2;
      laRight: TextX:=X+Width-TexMan.TextWidth(FFont, Text);
    end;
    TexMan.TextOut(FFont, FX+TextX, FY+Y, Text);
  end;
end;

function TGUIForm.GetButton(Index: Integer): PBtn;
begin
  Result:=nil;
  if (Index<0) or (Index>High(FButtons)) then Exit;
  Result:=@FButtons[Index];
end;

function TGUIForm.GetLabel(Index: Integer): PLbl;
begin
  Result:=nil;
  if (Index<0) or (Index>High(FLabels)) then Exit;
  Result:=@FLabels[Index];
end;

//{$O-} //Due to optimizer bug in Delphi 7.1
procedure TGUIForm.CheckClick(Btn: PBtn);
begin
  if Btn^.Checked
    then Btn^.Checked:=false
    else Btn^.Checked:=true;
end;
//{$O+}

procedure TGUIForm.RadioClick(Btn: PBtn);
var
  i: Integer;
begin
  for i:=0 to High(FButtons) do
    if (FButtons[i].Type_=btRadio) and (FButtons[i].Group=Btn^.Group)
      then FButtons[i].Checked:=false;
  Btn^.Checked:=true;
end;

procedure TGUIForm.MapCursor(var Cursor: TPoint);
begin
  if FVSWider then
  begin
    Cursor.X:=Round(Cursor.X/FVSScale)-FX;
    Cursor.Y:=Round(Cursor.Y/FVSScale+DeltaLeft[FVirtScreenVAlign]*FVSDelta)-FY;
  end
  else begin
    Cursor.X:=Round(Cursor.X/FVSScale+DeltaLeft[FVirtScreenAlign]*FVSDelta)-FX;
    Cursor.Y:=Round(Cursor.Y/FVSScale)-FY;
  end;
end;

function TGUIForm.BtnAt(Point: TPoint): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FButtons) do
    with FButtons[i] do
      if PointInRect(Point, Rect(X, Y, X+Width, Y+Height)) then
      begin
        Result:=i;
        Exit;
      end;
end;

function CreateSelect(Form: TGUIForm; X, Y, Width, Height: Integer; OnChange: TGUIOnClick; const PrevCaption, NextCaption: string): Integer;
var
  Lbl: TLbl;
  Btn: TBtn;
begin
  Btn.Type_:=btPush;
  Btn.X:=X;
  Btn.Y:=Y;
  Btn.Width:=Height;
  Btn.Height:=Height;
  Btn.OnClick:=OnChange;
  Btn.Enabled:=true;
  Btn.Caption:=PrevCaption;
  Btn.Tag:=-1;
  Form.AddButton(Btn);
  Btn.X:=X+Width-Height;
  Btn.Caption:=NextCaption;
  Btn.Tag:=1;
  Form.AddButton(Btn);
  Lbl.X:=X+Height;
  Lbl.Y:=Y+(Height-TexMan.TextHeight(Form.FFont)) div 2;
  Lbl.Width:=Width-2*Height;
  Lbl.Align:=laCenter;
  Lbl.Color:=Integer($FF00B200);
  Result:=Form.AddLabel(Lbl);
  Form.AddRect(Rect(X+Height, Y, X+Width-Height, Y+Height));
end;

function CreateMenu(Form: TGUIForm; X, Y, BtnWidth, BtnHeight, BtnSpacing: Integer; Items: array of TMenuItem): TMenuItems;
var
  Btn: TBtn;
  i: Integer;
begin
  SetLength(Result, Length(Items));
  Btn.Type_:=btPush;
  Btn.X:=X;
  Btn.Width:=BtnWidth;
  Btn.Height:=BtnHeight;
  Btn.Enabled:=true;
  for i:=0 to High(Items) do
  begin
    Btn.Y:=Y+i*(BtnHeight+BtnSpacing);
    Btn.Caption:=Items[i].Caption;
    Btn.Tag:=Items[i].Tag;
    Btn.OnClick:=Items[i].OnClick;
    Result[i]:=Form.AddButton(Btn);
  end;
end;

end.
