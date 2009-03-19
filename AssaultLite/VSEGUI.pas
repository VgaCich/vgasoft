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
    Typ: TBtnType; //Button type
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
    procedure DrawButton(Btn: TBtn; State: TBtnState); dynamic; //Override for custom buttons drawing
    procedure DrawRect(Rect: TRect); dynamic; //Override for custom rectangles drawing
    procedure DrawLabel(Lbl: TLbl); dynamic; //Override for custom labels drawing
  public
    constructor Create(VirtScrW, VirtScrH, X, Y, Width, Height: Integer; Font: Cardinal); //Creates form; VertScr*: virtual screen resolution (all dimensions defined in virtual screen coordinates); X, Y, Width, Height: form bounds; Font: form font
    destructor Destroy; override;
    function  AddButton(Btn: TBtn): Integer; //Add button, returns button index
    function  AddLabel(Lbl: TLbl): Integer; //Add label, returns label index
    procedure AddRect(Rect: TRect); //Add rectangle (visual frame)
    procedure Draw; //Draw form
    procedure Update; //dynamic; //Update form
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); dynamic; //Process mouse event
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); dynamic; //Process keyboard event
    procedure CharEvent(C: Char); dynamic; //Process char event
    property Button[Index: Integer]: PBtn read GetButton; //Buttons array
    property Lbl[Index: Integer]: PLbl read GetLabel; //Labels array
    property Caption: string read FCaption write FCaption; //Form caption
    property VirtScreenAlign: TVirtScreenAlign read FVirtScreenAlign write FVirtScreenAlign; //Virtual screen align
    property VirtScreenVAlign: TVirtScreenAlign read FVirtScreenVAlign write FVirtScreenVAlign; //Virtual screen vertical align
  end;

function CreateSelect(Form: TGUIForm; X_, Y_, W, H: Integer; OnChange: TGUIOnClick; const PrevCapt, NextCapt: string): Integer; //Creates select control, returns select state label index; distinguish prev & next buttons in handler by Btn^.Tag (-1 for prev, 1 for next)

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
  if Btn.Typ=btCheck then Btn.OnClick:=CheckClick;
  if Btn.Typ=btRadio then Btn.OnClick:=RadioClick;
  FButtons[Result]:=Btn;
end;

function TGUIForm.AddLabel(Lbl: TLbl): Integer;
begin
  Result:=Length(FLabels);
  SetLength(FLabels, Result+1);
  FLabels[Result]:=Lbl;
end;

procedure TGUIForm.AddRect(Rect: TRect);
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
  glPushAttrib(GL_ENABLE_BIT);
  if FVSWider
    then gleOrthoMatrix2(0, DeltaLeft[FVirtScreenVAlign]*FVSDelta, FVirtScrW, FVirtScrH+DeltaRight[FVirtScreenVAlign]*FVSDelta)
    else gleOrthoMatrix2(DeltaLeft[FVirtScreenAlign]*FVSDelta, 0, FVirtScrW+DeltaRight[FVirtScreenAlign]*FVSDelta, FVirtScrH);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
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
  FVSWider:=FVirtScrW/FVirtScrH>Core.ResX/Core.ResY;
  if FVSWider then
  begin
    FVSScale:=Core.ResX/FVirtScrW;
    FVSDelta:=(Core.ResY/FVSScale-FVirtScrH)/2;
  end
  else begin
    FVSScale:=Core.ResY/FVirtScrH;
    FVSDelta:=(Core.ResX/FVSScale-FVirtScrW)/2;
  end;
  GetCursorPos(Cursor);
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
    meDown: if Button=1 then FLast:=FActive;
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
    end;
  end;
end;

procedure TGUIForm.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if (Event=keDown) then
  begin
    case Button of
      VK_TAB: if Core.KeyPressed[VK_SHIFT]
        then Dec(FTabStop)
        else Inc(FTabStop);
      VK_UP: Dec(FTabStop);
      VK_DOWN: Inc(FTabStop);
      VK_SPACE: if FButtons[FTabStop].Enabled and Assigned(FButtons[FTabStop].OnClick) then
        begin
          FButtons[FTabStop].OnClick(@FButtons[FTabStop]);
          Exit;
        end;
      VK_SHIFT:;
      else begin
        FTabStop:=-1;
        Exit;
      end;
    end;
    if FTabStop<0 then FTabStop:=High(FButtons);
    if FTabStop>High(FButtons) then FTabStop:=0;
  end;
end;

procedure TGUIForm.CharEvent(C: Char);
begin

end;

procedure TGUIForm.PaintRect(Rect: TRect);
begin
  glBegin(GL_LINE_LOOP);
    glVertex2f(FX+Rect.Left, FY+Rect.Top);
    glVertex2f(FX+Rect.Right, FY+Rect.Top);
    glVertex2f(FX+Rect.Right, FY+Rect.Bottom);
    glVertex2f(FX+Rect.Left, FY+Rect.Bottom);
  glEnd;
end;

procedure TGUIForm.FillRect(Rect: TRect);
begin
  glBegin(GL_QUADS);
    glVertex2f(FX+Rect.Left, FY+Rect.Top);
    glVertex2f(FX+Rect.Right, FY+Rect.Top);
    glVertex2f(FX+Rect.Right, FY+Rect.Bottom);
    glVertex2f(FX+Rect.Left, FY+Rect.Bottom);
  glEnd;
end;

procedure TGUIForm.DrawForm;
begin
  glBlendFunc(GL_ONE, GL_ZERO);
  glLineWidth(1);
  glEnable(GL_LINE_SMOOTH);
  glColor3f(0.5, 1, 0.5);
  FillRect(Rect(0, 0, FWidth, FHeight));
  glColor3f(0, 0.9, 0.5);
  FillRect(Rect(0, 0, FWidth, TexMan.TextHeight(FFont)+6));
  glColor3f(1, 1, 0);
  PaintRect(Rect(0, 0, FWidth, FHeight));
//  glColor3d(1, 1, 0);
  TexMan.TextOut(FFont, FX+5, FY+3, FCaption);
end;

procedure TGUIForm.DrawButton(Btn: TBtn; State: TBtnState);
var
  TextX, TextY: Integer;
  Text: string;
begin
//  glLineWidth(1);
  glEnable(GL_LINE_SMOOTH);
  case Btn.Typ of
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
        TextX:=Max((Btn.Width-TexMan.TextLen(FFont, Btn.Caption)) div 2, 0);
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
  while (Text<>'') and (TexMan.TextLen(FFont, Text)+TextX>Btn.Width) do Delete(Text, Length(Text), 1);
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

procedure TGUIForm.DrawRect(Rect: TRect);
begin
  glLineWidth(1);
  glEnable(GL_LINE_SMOOTH);
  glColor3f(0, 1, 0);
  PaintRect(Rect);
end;

procedure TGUIForm.DrawLabel(Lbl: TLbl);
var
  Text: string;
  TextX: Integer;
begin
  with Lbl do
  begin
    gleColor(Color);
    Text:=Caption;
    while (Text<>'') and (TexMan.TextLen(FFont, Text)>Width) do Delete(Text, Length(Text), 1);
    case Align of
      laLeft: TextX:=X;
      laCenter: TextX:=X+(Width-TexMan.TextLen(FFont, Text)) div 2;
      laRight: TextX:=X+Width-TexMan.TextLen(FFont, Text);
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
    if (FButtons[i].Typ=btRadio) and (FButtons[i].Group=Btn^.Group)
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

function CreateSelect(Form: TGUIForm; X_, Y_, W, H: Integer; OnChange: TGUIOnClick; const PrevCapt, NextCapt: string): Integer;
var
  Lbl: TLbl;
  Btn: TBtn;
begin
  with Btn do
  begin
    Typ:=btPush;
    Y:=Y_;
    Width:=H;
    Height:=H;
    OnClick:=OnChange;
    Enabled:=true;
    X:=X_;
    Caption:=PrevCapt;
    Tag:=-1;
    Form.AddButton(Btn);
    X:=X+W-H;
    Caption:=NextCapt;
    Tag:=1;
    Form.AddButton(Btn);
  end;
  with Lbl do
  begin
    X:=X_+H;
    Y:=Y_+(H-TexMan.TextHeight(Form.FFont)) div 2;
    Width:=W-2*H;
    Align:=laCenter;
    Color:=Integer($FF00B200);
    Result:=Form.AddLabel(Lbl);
  end;
  Form.AddRect(Rect(X_+H, Y_, X_+W-H, Y_+H));
end;

end.
