unit FormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

const
  WM_XBUTTONDOWN=$20B;

type
  TMainForm = class(TForm)
    BindsList: TListView;
    BLoad: TButton;
    BSave: TButton;
    BAdd: TButton;
    BRemove: TButton;
    BUp: TButton;
    BDown: TButton;
    ListEdit: TEdit;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure BAddClick(Sender: TObject);
    procedure BDownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BindsListDblClick(Sender: TObject);
    procedure BindsListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BLoadClick(Sender: TObject);
    procedure BRemoveClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure BUpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ListEditExit(Sender: TObject);
    procedure ListEditKeyPress(Sender: TObject; var Key: Char);
  private
    FKeyNames: array[0..255] of string;
    FReadKey, FMouseFilter: Boolean;
    FItem: Integer;
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  VK_XBUTTON4=5; //Mouse button 4
  VK_XBUTTON5=6; //Mouse button 5
  VK_MWHEELUP=VK_F23; //Mouse wheel up
  VK_MWHEELDOWN=VK_F24; //Mouse wheel down

function Tok(const Sep: string; var S: string): string;

  function IsOneOf(C: Char; const S: string): Boolean;
  var
    i: integer;
  begin
    Result:=false;
    for i:=1 to Length(S) do
    begin
      if C=Copy(S, i, 1) then
      begin
        Result:=true;
        Exit;
      end;
    end;
  end;

var
  C: Char;
begin
  Result:='';
  if S='' then Exit;
  C:=S[1];
  while IsOneOf(C, Sep) do
  begin
    Delete(S, 1, 1);
    if S='' then Exit;
    C:=S[1];
  end;
  while (not IsOneOf(C, Sep)) and (S<>'') do
  begin
    Result:=Result+C;
    Delete(S, 1, 1);
    if S='' then Exit;
    C:=S[1];
  end;
end;

procedure TMainForm.AppMessage(var Msg: TMsg; var Handled: Boolean);
const
  Map: array[1..2] of Integer = (VK_XBUTTON4, VK_XBUTTON5);
begin
  if (Msg.message=WM_XBUTTONDOWN) and FReadKey and (HiWord(Msg.wParam)<=2) then
  begin
    BindsList.Items[FItem].Data:=Pointer(Map[HiWord(Msg.wParam)]);
    BindsList.Items[FItem].SubItems[1]:=FKeyNames[Map[HiWord(Msg.wParam)]];
    Handled:=true;
    FReadKey:=false;
  end;
end;

procedure TMainForm.BAddClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item:=BindsList.Items.Insert(BindsList.ItemIndex+1);
  Item.SubItems.Add('');
  Item.SubItems.Add('');
  Item.EditCaption;
end;

procedure TMainForm.BDownClick(Sender: TObject);
var
  Item: TListItem;
begin
  if BindsList.ItemIndex<BindsList.Items.Count-1 then
    with BindsList do
    begin
      Items.BeginUpdate;
      Item:=TListItem.Create(Items);
      Item.Assign(Items[ItemIndex]);
      Items[ItemIndex].Assign(Items[ItemIndex+1]);
      Items[ItemIndex+1].Assign(Item);
      Item.Free;
      ItemIndex:=ItemIndex+1;
      Items.EndUpdate;
    end;
  BindsList.SetFocus;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to 255 do
  begin
    SetLength(FKeyNames[i], 101);
    SetLength(FKeyNames[i], GetKeyNameText(MapVirtualKey(i, 0) shl 16, @FKeyNames[i][1], 100));
    if FKeyNames[i]='' then FKeyNames[i]:='VK #'+IntToStr(i);
  end;
  FKeyNames[0]:='---';
  FKeyNames[VK_LBUTTON]:='Left MB';
  FKeyNames[VK_RBUTTON]:='Right MB';
  FKeyNames[VK_MBUTTON]:='Middle MB';
  FKeyNames[VK_XBUTTON4]:='MB 4';
  FKeyNames[VK_XBUTTON5]:='MB 5';
  FKeyNames[VK_MWHEELUP]:='Wheel Up';
  FKeyNames[VK_MWHEELDOWN]:='Wheel Down';
  FKeyNames[$03]:='Cancel';
  FKeyNames[$0C]:='Clear';
  FKeyNames[$13]:='Pause';
  FKeyNames[$20]:='Space';
  FKeyNames[$21]:='Page Up';
  FKeyNames[$22]:='Page Down';
  FKeyNames[$23]:='End';
  FKeyNames[$24]:='Home';
  FKeyNames[$25]:='Left';
  FKeyNames[$26]:='Up';
  FKeyNames[$27]:='Right';
  FKeyNames[$28]:='Down';
  FKeyNames[$29]:='Select';
  FKeyNames[$2D]:='Insert';
  FKeyNames[$2E]:='Delete';
  FKeyNames[$5B]:='Left Win';
  FKeyNames[$5C]:='Right Win';
  FKeyNames[$5D]:='Apps';
  FKeyNames[$6F]:='Num /';
  FKeyNames[$90]:='Num Lock';
  Application.OnMessage:=AppMessage;
end;

procedure TMainForm.BindsListDblClick(Sender: TObject);
var
  P: TPoint;
  Item: TListItem;
  X: Integer;
  Col: Integer;
begin
  GetCursorPos(P);
  P:=BindsList.ScreenToClient(P);
  Item:=BindsList.GetItemAt(P.X, P.Y);
  if not Assigned(Item) then Exit;
  X:=0;
  Col:=0;
  while X+BindsList.Column[Col].Width<P.X do
  begin
    X:=X+BindsList.Column[Col].Width;
    Inc(Col);
  end;
  if Col=0 then
  begin
    Item.EditCaption;
    Exit;
  end;
  if Col=2 then
  begin
    Item.SubItems[1]:='<press key>';
    FReadKey:=true;
    FMouseFilter:=true;
    FItem:=BindsList.ItemIndex;
    Exit;
  end;
  P:=Point(X+BindsList.BorderWidth+BevelWidth, Item.Top+1);
  BindsList.ClientToParent(P);
  ListEdit.SetBounds(P.X, P.Y, BindsList.Column[Col].Width, 17);
  ListEdit.Text:=Item.SubItems[Col-1];
  ListEdit.Tag:=Col-1;
  ListEdit.Visible:=true;
  ListEdit.SetFocus;
  ListEdit.SelectAll;
end;

procedure TMainForm.BindsListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  Map: array[TMouseButton] of Integer = (VK_LBUTTON, VK_RBUTTON, VK_MBUTTON);
begin
  if FMouseFilter and (Button=mbLeft) then
  begin
    FMouseFilter:=false;
    Exit;
  end;
  if FReadKey then
  begin
    BindsList.Items[FItem].Data:=Pointer(Map[Button]);
    BindsList.Items[FItem].SubItems[1]:=FKeyNames[Map[Button]];
    FReadKey:=false;
  end;
end;

procedure TMainForm.BLoadClick(Sender: TObject);
var
  List: TStringList;
  i, Key: Integer;
  S: string;
  Item: TListItem;
begin
  if not OpenDialog.Execute then Exit;
  SaveDialog.FileName:=ExtractFileName(OpenDialog.FileName);
  SaveDialog.InitialDir:=ExtractFilePath(OpenDialog.FileName);
  List:=TStringList.Create;
  try
    List.LoadFromFile(OpenDialog.FileName);
    BindsList.Items.BeginUpdate;
    BindsList.Clear;
    for i:=0 to List.Count-1 do
    begin
      S:=List[i];
      Item:=BindsList.Items.Add;
      Item.Caption:=Tok(',', S);
      Item.SubItems.Add(Tok(',', S));
      Key:=StrToInt(Tok(',', S));
      if Key>255 then Key:=255;
      if Key<0 then Key:=0;
      Item.Data:=Pointer(Key);
      Item.SubItems.Add(FKeyNames[Key]);
    end;
  finally
    List.Free;
    BindsList.Items.EndUpdate;
  end;
end;

procedure TMainForm.BRemoveClick(Sender: TObject);
begin
  BindsList.Selected.Delete;
end;

procedure TMainForm.BSaveClick(Sender: TObject);
var
  List: TStringList;
  i: Integer;
begin
  if not SaveDialog.Execute then Exit;
  List:=TStringList.Create;
  try
    for i:=0 to BindsList.Items.Count-1 do
      with BindsList.Items[i] do
        List.Add(Caption+','+SubItems[0]+','+IntToStr(Integer(Data)));
    List.SaveToFile(SaveDialog.FileName);
  finally
    List.Free;
  end;
end;

procedure TMainForm.BUpClick(Sender: TObject);
var
  Item: TListItem;
begin
  if BindsList.ItemIndex>0 then
    with BindsList do
    begin
      Items.BeginUpdate;
      Item:=TListItem.Create(Items);
      Item.Assign(Items[ItemIndex]);
      Items[ItemIndex].Assign(Items[ItemIndex-1]);
      Items[ItemIndex-1].Assign(Item);
      Item.Free;
      ItemIndex:=ItemIndex-1;
      Items.EndUpdate;
    end;
  BindsList.SetFocus;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FReadKey then
  begin
    if Key=VK_ESCAPE then
    begin
      BindsList.Items[FItem].SubItems[1]:=FKeyNames[Integer(BindsList.Items[FItem].Data)];
      FReadKey:=false;
      Exit;
    end;
    BindsList.Items[FItem].Data:=Pointer(Key);
    BindsList.Items[FItem].SubItems[1]:=FKeyNames[Key];
    FReadKey:=false;
    Key:=0;
  end;
end;

procedure TMainForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if FReadKey then
  begin
    BindsList.Items[FItem].Data:=Pointer(VK_MWHEELDOWN);
    BindsList.Items[FItem].SubItems[1]:=FKeyNames[VK_MWHEELDOWN];
    FReadKey:=false;
  end;
end;

procedure TMainForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if FReadKey then
  begin
    BindsList.Items[FItem].Data:=Pointer(VK_MWHEELUP);
    BindsList.Items[FItem].SubItems[1]:=FKeyNames[VK_MWHEELUP];
    FReadKey:=false;
  end;
end;

procedure TMainForm.ListEditExit(Sender: TObject);
begin
  BindsList.Selected.SubItems[ListEdit.Tag]:=ListEdit.Text;
  ListEdit.Visible:=false;
  BindsList.SetFocus;
end;

procedure TMainForm.ListEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then ListEditExit(ListEdit);
end;

end.
