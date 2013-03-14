unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ExtDlgs, Registry, JPEG, IniFiles, StrKey;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Image1: TImage;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    Button3: TButton;
    CheckBox1: TCheckBox;
    OpenDialog1: TOpenPictureDialog;
    Button4: TButton;
    Button5: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog2: TOpenDialog;
    Button6: TButton;
    ComboBox2: TComboBox;
    CheckBox2: TCheckBox;
    Button7: TButton;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    CheckBox3: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddFile(const FileName: string; State: Integer);
    function RBState: Integer;
    procedure SetRBState(State: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure LabeledEdit1KeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure ListBox1KeyPress(Sender: TObject; var Key: Char);
  private
    function LoadHKHDConf: TStringList;
    procedure TagToText(Edit: TLabeledEdit);
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ChangeWallpaper;

var
  Form1: TForm1;
  Files: TStringList;
  OldIndex: integer;
  ListFile, HKHDFile: string;

const
  AutoIntervals: array[0..8] of
    record
      Name: shortstring; Value: Integer;
    end = ((Name: 'Change every 5 min'; Value: 300),
           (Name: 'Change every 10 min'; Value: 600),
           (Name: 'Change every 30 min'; Value: 1800),
           (Name: 'Change every hour'; Value: 3600),
           (Name: 'Change every 2 hour'; Value: 7200),
           (Name: 'Change every 3 hour'; Value: 10800),
           (Name: 'Change every 5 hour'; Value: 18000),
           (Name: 'Change every 12 hour'; Value: 43200),
           (Name: 'Change every 24 hour'; Value: 86400));

implementation

{$R *.DFM}
{$R Manifest.res}

procedure SetWallpaper(const Wallpaper, Style: string);
var
  Reg: TRegistry;
begin
  Reg:=TRegistry.Create;
  try
    Reg.OpenKey('Control Panel\Desktop', false);
    Reg.WriteString('Wallpaper', Wallpaper);
    Reg.WriteString('TileWallpaper', Style);
    Reg.WriteString('WallpaperStyle', Style);
    SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
  finally
    Reg.Free;
  end;
end;

procedure ChangeWallpaper;
var
  Method, Index: Integer;
begin
  Files:=TStringList.Create;
  try
    Files.LoadFromFile(ListFile);
    Method:=StrToInt(Files[0]);
    OldIndex:=StrToInt(Files[1]);
    Files.Delete(0);
    Files.Delete(0);
    case Method of
      0: repeat
           Index:=Random(Files.Count);
         until Index<>OldIndex;
      1: if OldIndex=Files.Count-1
           then Index:=0
           else Index:=OldIndex+1;
      2: if OldIndex=0
           then Index:=Files.Count-1
           else Index:=OldIndex-1;
      else Exit;
    end;
    SetWallpaper(Copy(Files[Index], 2, MaxInt), Files[Index][1]);
    Files.LoadFromFile(ListFile);
    Files[1]:=IntToStr(Index);
    Files.SaveToFile(ListFile);
  finally
    Files.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, Interval: integer;
  Temp: TStringList;
  Reg: TRegistry;
begin
  Application.Title:=Caption;
  InitStrKey;
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Run');
    CheckBox1.Checked:=Reg.ValueExists('VSWallChangerAutorun');
    CheckBox2.Checked:=Reg.ValueExists('hkhd');
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
  Files:=TStringList.Create;
  Temp:=TStringList.Create;
  try
    Temp.LoadFromFile(ListFile);
    ComboBox1.ItemIndex:=StrToInt(Temp[0]);
    OldIndex:=StrToInt(Temp[1]);
    for i:=2 to Temp.Count-1 do
      AddFile(Copy(Temp[i], 2, MaxInt), StrToInt(Temp[i][1]));
  finally
    Temp.Free;
  end;
  if Files.Count>0 then
  begin
    ListBox1.ItemIndex:=OldIndex;
    ListBox1.Selected[Oldindex]:=true;
    SetRBState(Integer(Files.Objects[OldIndex]));
    ListBox1.OnClick(ListBox1);
  end;
  ComboBox2.Items.Objects[0]:=TObject(0);
  for i:=0 to High(AutoIntervals) do
    ComboBox2.Items.AddObject(AutoIntervals[i].Name, TObject(AutoIntervals[i].Value));
  HKHDFile:=ExtractFilePath(Application.ExeName)+'\hkhd.dat';
  Temp:=LoadHKHDConf;
  try
    Interval:=StrToInt(Temp[0]);
    for i:=0 to ComboBox2.Items.Count-1 do
      if Integer(ComboBox2.Items.Objects[i])=Interval then
      begin
        ComboBox2.ItemIndex:=i;
        Break;
      end;
    if ComboBox2.ItemIndex=-1
      then ComboBox2.ItemIndex:=ComboBox2.Items.AddObject('Current ['+IntToStr(Interval)+'s]', TObject(Interval));
    LabeledEdit1.Tag:=StrToInt(Temp[1]);
    TagToText(LabeledEdit1);
    LabeledEdit2.Tag:=StrToInt(Temp[2]);
    TagToText(LabeledEdit2);
    if not FileExists(HKHDFile)
      then Temp.SaveToFile(HKHDFile);
  finally
    Temp.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i, Temp: integer;
  Msg: string;
begin
  if CheckBox3.Checked
    then Msg:='Really delete items and files?'
    else Msg:='Really delete items?';
  if MessageDlg(Msg, mtWarning, mbOKCancel, 0) = mrCancel
    then Exit;
  Temp:=ListBox1.ItemIndex;
  for i:=ListBox1.Items.Count-1 downto 0 do
    if ListBox1.Selected[i] then
    begin
      if ListBox1.SelCount=0 then Break;
      if CheckBox3.Checked then DeleteFile(Files[i]);
      ListBox1.Items.Delete(i);
      Files.Delete(i);
    end;
  if Temp>ListBox1.Items.Count-1 then Temp:=ListBox1.Items.Count-1;
  OldIndex:=Temp;
  if ListBox1.Items.Count=0 then Exit;
  SetRBState(Integer(Files.Objects[OldIndex]));
  ListBox1.Selected[Temp]:=true;
  ListBox1.OnClick(ListBox1);
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if (ListBox1.Items.Count=0) or not FileExists(Files[ListBox1.ItemIndex])
    then Exit;
  Files.Objects[OldIndex]:=TObject(RBState);
  OldIndex:=ListBox1.ItemIndex;
  Image1.Picture.LoadFromFile(Files[ListBox1.ItemIndex]);
  SetRBState(Integer(Files.Objects[ListBox1.ItemIndex]));
end;

procedure TForm1.ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Source=Sender
    then Accept:=true
    else Accept:=false;
end;

procedure TForm1.ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  P: TPoint;
  Item: Integer;
begin
  P.X:=X;
  P.Y:=Y;
  if ListBox1.ItemAtPos(P, false)=ListBox1.Items.Count
    then Item:=ListBox1.Items.Count-1
    else Item:=ListBox1.ItemAtPos(P, false);
  Files.Move(ListBox1.ItemIndex, Item);
  ListBox1.Items.Move(ListBox1.ItemIndex, Item);
  OldIndex:=Item;
  SetRBState(Integer(Files.Objects[Item]));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
begin
  if not OpenDialog1.Execute then Exit;
  for i:=0 to OpenDialog1.Files.Count-1 do
    AddFile(OpenDialog1.Files[i], RBState);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  Files.Insert(0, IntToStr(ListBox1.ItemIndex));
  Files.Insert(0, IntToStr(ComboBox1.ItemIndex));
  for i:=2 to Files.Count-1 do
    Files[i]:=IntToStr(Integer(Files.Objects[i]))+Files[i];
  Files.SaveToFile(ListFile);
  Files.Free;
end;

procedure TForm1.AddFile(const FileName: string; State: Integer);
begin
  if not FileExists(FileName) then Exit;
  Files.AddObject(FileName, TObject(State));
  ListBox1.Items.Add(ExtractFileName(FileName));
end;

function TForm1.RBState: Integer;
begin
  Result:=0;
  if RadioButton2.Checked then Result:=1;
  if RadioButton3.Checked then Result:=2;
end;

procedure TForm1.SetRBState(State: Integer);
begin
  case State of
    0: RadioButton1.Checked:=true;
    1: RadioButton2.Checked:=true;
    2: RadioButton3.Checked:=true;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var
  Reg: TRegistry;
begin
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', true);
    if CheckBox1.Checked=Reg.ValueExists('VSWallChangerAutorun') then Exit;
    if CheckBox1.Checked
      then Reg.WriteString('VSWallChangerAutorun', '"'+Application.ExeName+'" /change')
      else Reg.DeleteValue('VSWallChangerAutorun');
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  SetWallpaper(Files[ListBox1.ItemIndex], IntToStr(Integer(Files.Objects[ListBox1.ItemIndex])));
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  List: TStringList;
  i, St: Integer;
  S: string;
  B: Boolean;
begin
  if not OpenDialog2.Execute then Exit;
  List:=TStringList.Create;
  try
    List.LoadFromFile(OpenDialog2.FileName);
    i:=-1;
    B:=false;
    while i<List.Count-1 do
    begin
      Inc(i);
      if List[i]='' then Continue;
      if List[i][1]='[' then
        if UpperCase(List[i])='[WALLPAPER]'
          then B:=true
          else B:=false;
      if (not B) or (UpperCase(List[i])='[WALLPAPER]') then
      begin
        List.Delete(i);
        Dec(i);
      end;
    end;
    Files.Clear;
    ListBox1.Clear;
    St:=0;
    for i:=0 to List.Count-1 do
    begin
      if List[i]='' then Continue;
      if List[i]='*' then
      begin
        AddFile(S, St);
        St:=0;
      end;
      if List[i][1]='1'
        then S:=Copy(List[i], 3, MaxInt);
      if UpperCase(List[i])='T=2' then St:=1;
      if UpperCase(List[i])='T=3' then St:=2;
    end;
  finally
    List.Free;
  end;
  OldIndex:=0;
  if Files.Count=0 then Exit;
  ListBox1.ItemIndex:=OldIndex;
  ListBox1.Selected[OldIndex]:=true;
  SetRBState(Integer(Files.Objects[OldIndex]));
  ListBox1.OnClick(ListBox1);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  List: TStringList;
  i, Index: Integer;
  B: Boolean;
begin
  if not SaveDialog1.Execute then Exit;
  List:=TStringList.Create;
  try
    List.LoadFromFile(SaveDialog1.FileName);
    i:=-1;
    B:=false;
    while i<List.Count-1 do
    begin
      Inc(i);
      if List[i]='' then Continue;
      if List[i][1]='[' then
        if UpperCase(List[i])='[WALLPAPER]' then
        begin
          B:=true;
          Index:=i+1;
        end
          else B:=false;
      if B and (UpCase(List[i][1])<>'[') then
      begin
        List.Delete(i);
        Dec(i);
      end;
    end;
    for i:=0 to Files.Count-1 do
    begin
      List.Insert(Index, '1='+Files[i]);
      Inc(Index);
      case Integer(Files.Objects[i]) of
        1: List.Insert(Index, 'T=2');
        2: List.Insert(Index, 'T=3');
      end;
      if Integer(Files.Objects[i])>0 then Inc(Index);
      List.Insert(Index, '*');
      Inc(Index);
    end;
    List.SaveToFile(SaveDialog1.FileName);
  finally
    List.Free;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  Temp: TStringList;
begin
  Temp:=LoadHKHDConf;
  try
    Temp[3]:=IntToStr(Integer(Files.Objects[ListBox1.ItemIndex]))+Files[ListBox1.ItemIndex];
    Temp.SaveToFile(HKHDFile);
    PostMessage(FindWindow('hkhdclass', ''), WM_USER, 0, 0);
  finally
    Temp.Free;
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  Temp: TStringList;
begin
  Temp:=LoadHKHDConf;
  try
    Temp[1]:=IntToStr(LabeledEdit1.Tag);
    Temp[2]:=IntToStr(LabeledEdit2.Tag);
    Temp.SaveToFile(HKHDFile);
    PostMessage(FindWindow('hkhdclass', ''), WM_USER, 2, 0);
  finally
    Temp.Free;
  end;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
var
  Reg: TRegistry;
begin
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', true);
    if CheckBox2.Checked=Reg.ValueExists('hkhd') then Exit;
    if CheckBox2.Checked
      then Reg.WriteString('hkhd', '"'+ExtractFilePath(Application.ExeName)+'hkhd.exe"')
      else Reg.DeleteValue('hkhd');
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
var
  Temp: TStringList;
begin
  Temp:=LoadHKHDConf;
  try
    Temp[0]:=IntToStr(Integer(ComboBox2.Items.Objects[ComboBox2.ItemIndex]));
    Temp.SaveToFile(HKHDFile);
    PostMessage(FindWindow('hkhdclass', ''), WM_USER, 1, 0);
  finally
    Temp.Free;
  end;
end;

procedure TForm1.ListBox1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13
    then ListBox1DblClick(Sender);
end;

function TForm1.LoadHKHDConf: TStringList;
begin
  Result:=TStringList.Create;
  try
    Result.LoadFromFile(HKHDFile);
  except
  end;
  if Result.Count<>4 then
  begin
    Result.Clear;
    if (ComboBox2.ItemIndex>=0) and (ComboBox2.ItemIndex<ComboBox2.Items.Count)
      then Result.Add(IntToStr(Integer(ComboBox2.Items.Objects[ComboBox2.ItemIndex])))
      else Result.Add('0');
    Result.Add('346');
    Result.Add('323');
    if Files.Count>0
      then Result.Add(IntToStr(Integer(Files.Objects[ListBox1.ItemIndex]))+Files[ListBox1.ItemIndex])
      else Result.Add('0');
  end;
end;

procedure TForm1.LabeledEdit1KeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
var
  Modifier: Word;
begin
  if (Key in [VK_SHIFT, VK_CONTROL, VK_MENU]) or (KeyToStr(Key)='')
    then Exit;
  Modifier:=0;
  if Key<>VK_BACK then
  begin
    if ssShift in Shift then
      Modifier:=Modifier or MOD_SHIFT;
    if ssCtrl in Shift then
      Modifier:=Modifier or MOD_CONTROL;
    if ssAlt in Shift then
      Modifier:=Modifier or MOD_ALT;
    Modifier:=Modifier shl 8;
  end
    else Key:=0;
  (Sender as TLabeledEdit).Tag:=Modifier or (Key and $FF);
  TagToText(Sender as TLabeledEdit);
end;

procedure TForm1.TagToText(Edit: TLabeledEdit);
var
  Temp: string;
begin
  Temp:='';
  if Edit.Tag and (MOD_SHIFT shl 8) <> 0 then
    Temp:=Temp+'*';
  if Edit.Tag and (MOD_CONTROL shl 8) <> 0 then
    Temp:=Temp+'^';
  if Edit.Tag and (MOD_ALT shl 8) <> 0 then
    Temp:=Temp+'&';
  Edit.Text:=Temp+KeyToStr(Edit.Tag and $FF);
end;

end.
