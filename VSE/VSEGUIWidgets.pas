unit VSEGUIWidgets;

interface

uses
  Windows, AvL, avlUtils, dglOpenGL, OpenGLExt, VSEGUI;

type
  TOnButtonClick=procedure(Sender: TObject; Button: Integer) of object;
  TGUIButtonBase=class(TGUIWidget)
  private
    FOnClick: TOnEvent;
    FOnButtonClick: TOnButtonClick;
  protected
    procedure EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer); override;
  public
    property OnClick: TOnEvent read FOnClick write FOnClick;
    property OnButtonClick: TOnButtonClick read FOnButtonClick write FOnButtonClick;
  end;

implementation

procedure TGUIButtonBase.EventHandler(Event: TRegionEvent; Button, X, Y, Tag: Integer);
begin
  inherited EventHandler(Event, Button, X, Y, Tag);
  if (Event=reKeyDown) and ((Button=VK_SPACE) or (Button=VK_RETURN)) then
  begin
    Event:=reMouseClick;
    Button:=1;
  end;
  if Event=reMouseClick then
  begin
    FParentGUI.GrabFocus(Self);
    if (Button=1) and Assigned(FOnClick) then FOnClick(Self);
    if Assigned(FOnButtonClick) then FOnButtonClick(Self, Button);
  end;
end;

end.
