unit avlFormReader;

interface

uses
  Windows, Messages, AvL, Base64, avlUtils;

//TODO: Update to current AvL
  
{$DEFINE TANIMATE}
{$DEFINE TBUTTON}
{$DEFINE TCHECKBOX}
{$DEFINE TCOMBOBOX}
{$DEFINE TDATETIMEPICKER}
{$DEFINE TEDIT}
{$DEFINE TFILELISTBOX}
{$DEFINE TFORM}
{.$DEFINE TGRAPHICCONTROL} // Quite useless, only as ancestor 
{$DEFINE TGROUPBOX}
{$DEFINE THEADERCONTROL}
{$DEFINE THOTKEY}
{$DEFINE TIMAGE}
{$DEFINE TIPEDIT}
{$DEFINE TLABEL}
{$DEFINE TLABELEDEDIT}
{$DEFINE TLISTBOX}
{$DEFINE TLISTVIEW}
{$DEFINE TMDICHILDFORM}
{$DEFINE TMDIFORM}
{$DEFINE TMEMO}
{$DEFINE TMONTHCALENDAR}
{$DEFINE TPANEL}
{$DEFINE TPROGRESSBAR}
{$DEFINE TRADIOBUTTON}
{$DEFINE TRICHEDIT}
{$DEFINE TSCROLLBAR}
{$DEFINE TSIMPLEPANEL}
{$DEFINE TSPEEDBUTTON}
{.$DEFINE TSPINEDIT} // Buggy and useless
{$DEFINE TSTATUSBAR}
{$DEFINE TTABCONTROL}
{$DEFINE TTOOLBAR}
{$DEFINE TTRACKBAR}
{$DEFINE TTREEVIEW}
{$DEFINE TUPDOWN}

type
  TControlProperties = class;
  TDataStream = class(TCustomMemoryStream)
  private
    FParent: TControlProperties;
  public
    constructor Create(const Data: string; Parent: TControlProperties);
    destructor Destroy; override;
  end;
  TControlProperties = class(TStringList)
  private
    FProperty: string;
    FData: TDataStream;
  public
    constructor Create;
    procedure Clear;
    function Find(const PropName: string): Boolean;
    function Bool: Boolean; overload;
    function Bool(const PropName: string): Boolean; overload;
    function Int: Integer; overload;
    function Int(const PropName: string): Integer; overload;
    function Str: string; overload;
    function Str(const PropName: string): string; overload;
    function Data: TDataStream; overload;
    function Data(const PropName: string): TDataStream; overload;
    function Item: string;
  end;
  TAddControlFunc=function(Properties: TControlProperties; Parent: TWinControl): TWinControl of object;
  TControlType = record
    Name: string;
    AddFunc: TAddControlFunc;
  end;
  TOnEventHandlerRequired=function(Sender: TObject; const HandlerName: string): TMethod of object;
  TFormReader=class(TObject)
  private
    FControls: TStringList;
    FControlTypes: array of TControlType;
    FOnEventHandlerRequired: TOnEventHandlerRequired;
    function  GetControl(const Name: string): TWinControl;
    function AddControl(const Name, CtrlType: string; Properties: TControlProperties; CurCtrl: TWinControl): TWinControl;
  protected
    {$IFDEF TANIMATE} function AddAnimate(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TBUTTON} function AddButton(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TCHECKBOX} function AddCheckBox(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TCOMBOBOX} function AddComboBox(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TDATETIMEPICKER} function AddDateTimePicker(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IF DEFINED(TEDIT) OR DEFINED(TLABELEDEDIT) OR DEFINED(TSPINBOX)} procedure SetEditProperties(Edit: TEdit; Properties: TControlProperties); {$IFEND}
    {$IFDEF TEDIT} function AddEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TFILELISTBOX} function AddFileListBox(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IF DEFINED(TFORM) OR DEFINED(TMDICHILDFORM) OR DEFINED(TMDIFORM)}procedure SetFormProperties(Form: TForm; Properties: TControlProperties);{$IFEND}
    {$IFDEF TFORM} function AddForm(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TGRAPHICCONTROL} function AddGraphicControl(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TGROUPBOX} function AddGroupBox(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF THEADERCONTROL} function AddHeaderControl(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF THOTKEY} function AddHotKey(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TIMAGE} function AddImage(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TIPEDIT} function AddIPEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IF DEFINED(TLABEL) OR DEFINED(TLABELEDEDIT)} procedure SetLabelProperties(Lbl: TLabel; Properties: TControlProperties); {$IFEND}
    {$IFDEF TLABEL} function AddLabel(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TLABELEDEDIT} function AddLabeledEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TLISTBOX} function AddListBox(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TLISTVIEW} function AddListView(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TMDICHILDFORM} function AddMDIChildForm(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TMDIFORM} function AddMDIForm(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TMEMO} function AddMemo(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TMONTHCALENDAR} function AddMonthCalendar(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TPANEL} function AddPanel(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TPROGRESSBAR} function AddProgressBar(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TRADIOBUTTON} function AddRadioButton(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TRICHEDIT} function AddRichEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSCROLLBAR} function AddScrollBar(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSIMPLEPANEL} function AddSimplePanel(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSPEEDBUTTON} function AddSpeedButton(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSPINEDIT} function AddSpinEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSTATUSBAR} function AddStatusBar(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TTABCONTROL} function AddTabControl(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TTOOLBAR} function AddToolBar(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TTRACKBAR} function AddTrackBar(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TTREEVIEW} function AddTreeView(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TUPDOWN} function AddUpDown(Properties: TControlProperties; Parent: TWinControl): TWinControl; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadForm(Data: TStream); virtual;
    function  GetHandler(const HandlerName: string): TOnEvent;
    procedure AddControlType(const TypeName: string; AddFunc: TAddControlFunc);
    procedure GetControlsList(List: TStringList);
    procedure GetTypesList(List: TStringList);
    procedure SetWinControlProperties(Control: TWinControl; Properties: TControlProperties);
    procedure DeleteControl(Control: TWinControl);
    property  Control[const Name: string]: TWinControl read GetControl; default;
    property  OnEventHandlerRequired: TOnEventHandlerRequired read FOnEventHandlerRequired write FOnEventHandlerrequired;
  end;

function GetField(S: string; N: Integer): string;

implementation

const
  ItemSep = '|';
  FieldSep = '`';

function GetField(S: string; N: Integer): string;
begin
  repeat
    Result := Tok(FieldSep, S);
    Dec(N);
  until N < 0;
end;

function LoadImageList(Data: TCustomMemoryStream): TImageList;
var
  S: string;
begin
  try
    S := UniTempFile;
    Data.SaveToFile(S);
    Result := TImageList.Create;
    Result.AddMasked(LoadImage(0, PChar(S), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE or LR_LOADTRANSPARENT), clFuchsia);
  finally
    DeleteFile(S);
    Data.Free;
  end;
end;

{ TDataStream }

constructor TDataStream.Create(const Data: string; Parent: TControlProperties);
var
  Mem: Pointer;
begin
  inherited Create;
  FParent := Parent;
  Parent.FData := Self;
  GetMem(Mem, 3 * (Length(Data) div 4 + 1));
  SetPointer(Mem, Base64Decode(PChar(Data), Mem, Length(Data)));
end;

destructor TDataStream.Destroy;
begin
  FParent.FData := nil;
  FreeMem(Memory);
  inherited;
end;

{TControlProperties}

constructor TControlProperties.Create;
begin
  FProperty := '';
  FData := nil;
end;

procedure TControlProperties.Clear;
begin
  FProperty := '';
  FData.Free;
  inherited;
end;

function TControlProperties.Find(const PropName: string): Boolean;
begin
  FProperty := Values[PropName];
  Result := FProperty <> '';
end;

function TControlProperties.Bool: Boolean;
begin
  Result := (FProperty <> '') and (StrToInt(FProperty) <> 0);
end;

function TControlProperties.Bool(const PropName: string): Boolean;
begin
  Find(PropName);
  Result := Bool;
end;

function TControlProperties.Data: TDataStream;
begin
  if Assigned(FData) then
    Result := FData
  else if FProperty <> '' then
    Result := TDataStream.Create(FProperty, Self)
  else Result := nil;
end;

function TControlProperties.Data(const PropName: string): TDataStream;
begin
  Find(PropName);
  Result := Data;
end;

function TControlProperties.Int: Integer;
begin
  if FProperty <> '' then
    Result := StrToInt(FProperty)
  else
    Result := 0;
end;

function TControlProperties.Int(const PropName: string): Integer;
begin
  Find(PropName);
  Result := Int;
end;

function TControlProperties.Str: string;
begin
  Result := FProperty;
end;

function TControlProperties.Str(const PropName: string): string;
begin
  Find(PropName);
  Result := Str;
end;

function TControlProperties.Item: string;
begin
  Result := Tok(ItemSep, FProperty);
end;

{TFormReader}

constructor TFormReader.Create;
begin
  inherited Create;
  FControls := TStringList.Create;
  FControls.Duplicates := dupError;
  {$IFDEF TANIMATE} AddControlType('TAnimate', AddAnimate); {$ENDIF}
  {$IFDEF TBUTTON} AddControlType('TButton', AddButton); {$ENDIF}
  {$IFDEF TCHECKBOX} AddControlType('TCheckBox', AddCheckBox); {$ENDIF}
  {$IFDEF TCOMBOBOX} AddControlType('TComboBox', AddComboBox); {$ENDIF}
  {$IFDEF TDATETIMEPICKER} AddControlType('TDateTimePicker', AddDateTimePicker); {$ENDIF}
  {$IFDEF TEDIT} AddControlType('TEdit', AddEdit); {$ENDIF}
  {$IFDEF TFILELISTBOX} AddControlType('TFileListBox', AddFileListBox); {$ENDIF}
  {$IFDEF TFORM} AddControlType('TForm', AddForm); {$ENDIF}
  {$IFDEF TGRAPHICCONTROL} AddControlType('TGraphicControl', AddGraphicControl); {$ENDIF}
  {$IFDEF TGROUPBOX} AddControlType('TGroupBox', AddGroupBox); {$ENDIF}
  {$IFDEF THEADERCONTROL} AddControlType('THeaderControl', AddHeaderControl); {$ENDIF}
  {$IFDEF THOTKEY} AddControlType('THotKey', AddHotKey); {$ENDIF}
  {$IFDEF TIMAGE} AddControlType('TImage', AddImage); {$ENDIF}
  {$IFDEF TIPEDIT} AddControlType('TIPEdit', AddIPEdit); {$ENDIF}
  {$IFDEF TLABEL} AddControlType('TLabel', AddLabel); {$ENDIF}
  {$IFDEF TLABELEDEDIT} AddControlType('TLabeledEdit', AddLabeledEdit); {$ENDIF}
  {$IFDEF TLISTBOX} AddControlType('TListBox', AddListBox); {$ENDIF}
  {$IFDEF TLISTVIEW} AddControlType('TListView', AddListView); {$ENDIF}
  {$IFDEF TMDICHILDFORM} AddControlType('TMDIChildForm', AddMDIChildForm); {$ENDIF}
  {$IFDEF TMDIFORM} AddControlType('TMDIForm', AddMDIForm); {$ENDIF}
  {$IFDEF TMEMO} AddControlType('TMemo', AddMemo); {$ENDIF}
  {$IFDEF TMONTHCALENDAR} AddControlType('TMonthCalendar', AddMonthCalendar); {$ENDIF}
  {$IFDEF TPANEL} AddControlType('TPanel', AddPanel); {$ENDIF}
  {$IFDEF TPROGRESSBAR} AddControlType('TProgressBar', AddProgressBar); {$ENDIF}
  {$IFDEF TRADIOBUTTON} AddControlType('TRadioButton', AddRadioButton); {$ENDIF}
  {$IFDEF TRICHEDIT} AddControlType('TRichEdit', AddRichEdit); {$ENDIF}
  {$IFDEF TSCROLLBAR} AddControlType('TScrollBar', AddScrollBar); {$ENDIF}
  {$IFDEF TSIMPLEPANEL} AddControlType('TSimplePanel', AddSimplePanel); {$ENDIF}
  {$IFDEF TSPEEDBUTTON} AddControlType('TSpeedButton', AddSpeedButton); {$ENDIF}
  {$IFDEF TSPINEDIT} AddControlType('TSpinEdit', AddSpinEdit); {$ENDIF}
  {$IFDEF TSTATUSBAR} AddControlType('TStatusBar', AddStatusBar); {$ENDIF}
  {$IFDEF TTABCONTROL} AddControlType('TTabControl', AddTabControl); {$ENDIF}
  {$IFDEF TTOOLBAR} AddControlType('TToolBar', AddToolBar); {$ENDIF}
  {$IFDEF TTRACKBAR} AddControlType('TTrackBar', AddTrackBar); {$ENDIF}
  {$IFDEF TTREEVIEW} AddControlType('TTreeView', AddTreeView); {$ENDIF}
  {$IFDEF TUPDOWN} AddControlType('TUpDown', AddUpDown); {$ENDIF}
end;

destructor TFormReader.Destroy;
begin
  Finalize(FControlTypes);
  FAN(FControls);
end;

function TFormReader.GetControl(const Name: string): TWinControl;
begin
  Result := FControls.Objects[FControls.IndexOf(Name)] as TWinControl;
end;

function TFormReader.AddControl(const Name, CtrlType: string; Properties: TControlProperties; CurCtrl: TWinControl): TWinControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to High(FControlTypes) do
    if SameText(FControlTypes[i].Name, CtrlType) then
    begin
      Result := FControlTypes[i].AddFunc(Properties, CurCtrl);
      FControls.AddObject(Name, Result);
    end
end;

procedure TFormReader.ReadForm(Data: TStream);
var
  CtrlStack: array of TWinControl;

  procedure PushControl(Control: TWinControl);
  begin
    SetLength(CtrlStack, Length(CtrlStack) + 1);
    CtrlStack[High(CtrlStack)] := Control;
  end;

  function PopControl: TWinControl;
  begin
    Result := CtrlStack[High(CtrlStack)];
    SetLength(CtrlStack, High(CtrlStack));
  end;

var
  i: Cardinal;
  Lines: TStringList;
  Properties: TControlProperties;
  S, ControlName, ControlType: string;
  CurControl: TWinControl;
  IsProperty: Boolean;
begin
  Lines := TStringList.Create;
  Properties := TControlProperties.Create;
  try
    Lines.LoadFromStream(Data);
    CurControl := nil;
    IsProperty := false;
    ControlName := '';
    ControlType := '';
    for i := 0 to Lines.Count - 1 do
    begin
      S := TrimLeft(Lines[i]);
      if (S = '') or (S[1] = '#') then Continue;
      if IsProperty then
        if SameText(TrimRight(S), 'PropEnd') then
        begin
          IsProperty := false;
          PushControl(CurControl);
          CurControl := AddControl(ControlName, ControlType, Properties, CurControl);
          if not Assigned(CurControl) then
            raise Exception.Create('Can''t create control ' + ControlName + ': ' + ControlType);
          Properties.Clear;
        end
          else Properties.Add(S)
      else
        if SameText(TrimRight(S), 'End') then
          CurControl := PopControl
        else if SameText(Copy(S, 1, FirstDelimiter(' ', S) - 1), 'Control') then
        begin
          IsProperty := true;
          ControlName := Trim(Copy(S, FirstDelimiter(' ', S) + 1, FirstDelimiter(':', S) - FirstDelimiter(' ', S) - 1));
          ControlType := Trim(Copy(S, FirstDelimiter(':', S) + 1, MaxInt));
        end;
    end;
  finally
    FAN(Properties);
    FAN(Lines);
  end;
end;

function TFormReader.GetHandler(const HandlerName: string): TOnEvent;
begin
  if Assigned(FOnEventHandlerRequired) then
    Result := TOnEvent(FOnEventHandlerRequired(Self, HandlerName))
  else
    Result := nil;
end;

procedure TFormReader.AddControlType(const TypeName: string; AddFunc: TAddControlFunc);
var
  i: Integer;
begin
  for i := 0 to High(FControlTypes) do
    if SameText(FControlTypes[i].Name, TypeName) then
    begin
      FControlTypes[i].AddFunc := AddFunc;
      Exit;
    end;
  SetLength(FControlTypes, Length(FControlTypes) + 1);
  FControlTypes[High(FControlTypes)].Name := TypeName;
  FControlTypes[High(FControlTypes)].AddFunc := AddFunc;
end;

procedure TFormReader.GetControlsList(List: TStringList);
begin
  List.Assign(FControls);
end;

procedure TFormReader.GetTypesList(List: TStringList);
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to High(FControlTypes) do
    List.Add(FControlTypes[i].Name);
end;

procedure TFormReader.DeleteControl(Control: TWinControl);
var
  i: Integer;
begin
  for i := 0 to FControls.Count - 1 do
    if FControls.Objects[i] = Control then
    begin
      FControls.Delete(i);
      Exit;
    end;
end;

procedure TFormReader.SetWinControlProperties(Control: TWinControl; Properties: TControlProperties);
var
  S: string;
begin
  with Properties, Control do
  begin
    if Find('Color') then Color := Int;
    if Find('Cursor') then Cursor := Int;
    if Find('Style') then Style := Style or Int;
    if Find('ExStyle') then ExStyle := ExStyle or Int;
    if Find('Tag') then Tag := Int;
    if Find('TextColor') then TextColor := Int;
    if Find('Left') then Left := Int;
    if Find('Top') then Top := Int;
    if Find('Width') then Width := Int;
    if Find('Height') then Height := Int;
    if Find('Ctl3D') then Ctl3D := Bool;
    if Find('Enabled') then Enabled := Bool;
    if Find('ShowHint') then ShowHint := Bool;
    if Find('Visible') then Visible := Bool;
    if Find('Hint') then Hint := Str;
    if Find('TagEx') then TagEx := Str;
    if Find('Font') then StrToFont(Str, Font);
    if Find('CustomData') then
    try
      CustomData := AllocMem(Data.Size);
      Data.Read(CustomData^, Data.Size);
    finally
      Data.Free;
    end;
    if Find('Icon') then
    try
      S := UniTempFile;
      Data.SaveToFile(S);
      Icon := LoadImage(0, PChar(S), IMAGE_ICON, 0, 0, LR_LOADFROMFILE or LR_LOADTRANSPARENT);
    finally
      DeleteFile(S);
      Data.Free;
    end;
    if Find('OnClick') then OnClick := TOnEvent(GetHandler(Str));
    if Find('OnDblClick') then OnDblClick := TOnEvent(GetHandler(Str));
    if Find('OnChange') then OnChange := TOnEvent(GetHandler(Str));
    if Find('OnCreate') then OnCreate := TOnEvent(GetHandler(Str));
    if Find('OnClose') then OnClose := TOnREvent(GetHandler(Str));
    if Find('OnDestroy') then OnDestroy := TOnEvent(GetHandler(Str));
    if Find('OnKeyDown') then OnKeyDown := TOnKey(GetHandler(Str));
    if Find('OnKeyUp') then OnKeyUp := TOnKey(GetHandler(Str));
    if Find('OnMessage') then OnMessage := TOnMessage(GetHandler(Str));
    if Find('OnMinimize') then OnMinimize := TOnREvent(GetHandler(Str));
    if Find('OnMouseDown') then OnMouseDown := TOnMouse(GetHandler(Str));
    if Find('OnMouseMove') then OnMouseMove := TOnMouseMove(GetHandler(Str));
    if Find('OnMouseUp') then OnMouseUp := TOnMouse(GetHandler(Str));
    if Find('OnPaint') then OnPaint := TOnEvent(GetHandler(Str));
    if Find('OnResize') then OnResize := TOnEvent(GetHandler(Str));
    if Find('OnShow') then OnShow := TOnEvent(GetHandler(Str));
    if Find('OnHide') then OnHide := TOnEvent(GetHandler(Str));
  end;
end;

{$IFDEF TANIMATE}
function TFormReader.AddAnimate(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TAnimate.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TAnimate) do
  begin
    if Find('CommonAVI') then CommonAVI := TCommonAVI(Int);
    if Find('StartFrame') then StartFrame := Int;
    if Find('StopFrame') then StopFrame := Int;
  end;
end;
{$ENDIF}

{$IFDEF TBUTTON}
function TFormReader.AddButton(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TButton.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TButton) do
  begin
    if Find('Default') then Default := Bool;
    if Find('Flat') then Flat := Bool;
  end;
end;
{$ENDIF}

{$IFDEF TCHECKBOX}
function TFormReader.AddCheckBox(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TCheckBox.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  if Properties.Find('Checked') then (Result as TCheckBox).Checked := Properties.Bool;
end;
{$ENDIF}

{$IFDEF TCOMBOBOX}
function TFormReader.AddComboBox(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TComboBox.Create(Parent, TComboBoxStyle(Properties.Int('ComboStyle')));
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TComboBox) do
  begin
    if Find('Items') then
      while Str <> '' do
        ItemAdd(Item);
    if Find('Text') then Text := Str;
    if Find('ItemIndex') then ItemIndex := Int;
    if Find('DroppedDown') then DroppedDown := Bool;
  end;
end;
{$ENDIF}

{$IFDEF TDATETIMEPICKER}
function TFormReader.AddDateTimePicker(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result:=TDateTimePicker.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TDateTimePicker) do
  begin
    if Find('DateFormat') then DateFormat := TDTDateFormat(Int);
    if Find('Kind') then Kind := TDateTimeKind(Int);
    if Find('DateTime') then DateTime := StrToFloat(Str);
  end;
end;
{$ENDIF}

{$IF DEFINED(TEDIT) OR DEFINED(TLABELEDEDIT) OR DEFINED(TSPINEDIT)}
procedure TFormReader.SetEditProperties(Edit: TEdit; Properties: TControlProperties);
begin
  with Properties, Edit do
  begin
    if Find('BorderStyle') then BorderStyle := TBorderStyle(Int);
    if Find('Flat') then Flat := Bool;
    if Find('MarginWidth') then MarginWidth := Int;
    if Find('MaxLength') then MaxLength := Int;
    if Find('SelStart') then SelStart := Int;
    if Find('SelLength') then SelLength := Int;
    if Find('SelText') then SelText := Str;
    if Find('PasswordChar') then PasswordChar := Str[1];
    if Find('ReadOnly') then ReadOnly := Bool;
  end;
end;
{$IFEND}

{$IFDEF TEDIT}
function TFormReader.AddEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TEdit.Create(Parent, Properties.Str('Text'));
  SetWinControlProperties(Result, Properties);
  SetEditProperties(Result as TEdit, Properties);
end;
{$ENDIF}

{$IFDEF TFILELISTBOX}
function TFormReader.AddFileListBox(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TFileListBox.Create(Parent, AddTrailingBackslash(Properties.Str('Path')));
  SetWinControlProperties(Result, Properties);
  if Properties.Find('Mask') then (Result as TFileListBox).Mask := Properties.Str;
  (Result as TFileListBox).Update;
end;
{$ENDIF}

{$IF DEFINED(TFORM) OR DEFINED(TMDICHILDFORM) OR DEFINED(TMDIFORM)}
procedure TFormReader.SetFormProperties(Form: TForm; Properties: TControlProperties);
begin
  with Properties, Form do
  begin
    Width := 2 * Width - ClientWidth;
    Height := 2 * Height - ClientHeight;
    if Find('AlphaBlendValue') then AlphaBlendValue := Int;
    if Find('AlphaBlend') then AlphaBlend := Bool;
    if Find('TransparentColorValue') then TransparentColorValue := Int;
    if Find('TransparentColor') then TransparentColor := Bool;
    if Find('BorderStyle') then BorderStyle := TFormBorderStyle(Int);
    if Find('Position') then Position := Int;
    if Find('WindowState') then WindowState := Int;
    if Find('BorderIcons') then
    begin
      if (Int and $1) <> 0
        then BorderIcons := [biSystemMenu]
        else BorderIcons := [];
      if (Int and $2) <> 0 then BorderIcons := BorderIcons + [biMinimize];
      if (Int and $4) <> 0 then BorderIcons := BorderIcons + [biMaximize];
      if (Int and $8) <> 0 then BorderIcons := BorderIcons + [biHelp];
    end;
  end;
end;
{$IFEND}

{$IFDEF TFORM}
function TFormReader.AddForm(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TForm.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  SetFormProperties(Result as TForm, Properties);
end;
{$ENDIF}

{$IFDEF TGRAPHICCONTROL}
function TFormReader.AddGraphicControl(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TGraphicControl.Create(Parent);
  SetWinControlProperties(Result, Properties);
end;
{$ENDIF}

{$IFDEF TGROUPBOX}
function TFormReader.AddGroupBox(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TGroupBox.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
end;
{$ENDIF}

{$IFDEF THEADERCONTROL}
function TFormReader.AddHeaderControl(Properties: TControlProperties; Parent: TWinControl): TWinControl;

  procedure AddSection(const Sect: string);
  begin
    (Result as THeaderControl).SectionAdd(GetField(Sect, 0), StrToInt(GetField(Sect, 1)));
  end;

begin
  Result := THeaderControl.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties do
    if Find('Sections') then
      while Str <> '' do
        AddSection(Item);
end;
{$ENDIF}

{$IFDEF THOTKEY}
function TFormReader.AddHotKey(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := THotKey.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as THotKey) do
  begin
    if Find('HotKey') then HotKey := Int;
    if Find('Modifiers') then
    begin
      if (Int and $1) <> 0
        then Modifiers := [mShift]
        else Modifiers := [];
      if (Int and $2) <> 0 then Modifiers := Modifiers + [mCtrl];
      if (Int and $4) <> 0 then Modifiers := Modifiers + [mAlt];
      if (Int and $8) <> 0 then Modifiers := Modifiers + [mExt];
    end;
  end;
end;
{$ENDIF}

{$IFDEF TIMAGE}
function TFormReader.AddImage(Properties: TControlProperties; Parent: TWinControl): TWinControl;
var
  S: string;
begin
  Result := TImage.Create(Parent);
  SetWinControlProperties(Result, Properties);
  if Properties.Find('Image') then
  try
    S := UniTempFile;
    Properties.Data.SaveToFile(S);
    (Result as TImage).LoadFromFile(S);
  finally
    DeleteFile(S);
    Properties.Data.Free;
  end;
end;
{$ENDIF}

{$IFDEF TIPEDIT}
function TFormReader.AddIPEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TIPEdit.Create(Parent, '');
  SetWinControlProperties(Result, Properties);
  if Properties.Find('IP') then Result.Perform(WM_USER+101, 0, Properties.Int);
end;
{$ENDIF}

{$IF DEFINED(TLABEL) OR DEFINED(TLABELEDEDIT)}
procedure TFormReader.SetLabelProperties(Lbl: TLabel; Properties: TControlProperties);
begin
  with Properties, Lbl do
  begin
    if Find('Alignment') then Alignment := Int;
    if Find('Transparent') then Transparent := Bool;
    if Find('BkMode') then BkMode := TBkMode(Int);
  end;
end;
{$IFEND}

{$IFDEF TLABEL}
function TFormReader.AddLabel(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TLabel.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  SetLabelProperties(Result as TLabel, Properties);
  if Properties.Find('BorderStyle') then (Result as TLabel).BorderStyle := TBorderStyle(Properties.Int);
end;
{$ENDIF}

{$IFDEF TLABELEDEDIT}
function TFormReader.AddLabeledEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TLabeledEdit.Create(Parent, Properties.Str('Caption'), Properties.Str('Text'));
  SetWinControlProperties(Result, Properties);
  SetEditProperties(Result as TLabeledEdit, Properties);
  SetLabelProperties((Result as TLabeledEdit).EditLabel, Properties);
  if Properties.Find('LabelBorderStyle') then (Result as TLabeledEdit).EditLabel.BorderStyle := TBorderStyle(Properties.Int);
end;
{$ENDIF}

{$IFDEF TLISTBOX}
function TFormReader.AddListBox(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TListBox.Create(Parent, TListBoxStyle(Properties.Int('ListStyle')));
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TListBox) do
  begin
    if Find('Items') then
      while Str <> '' do
        ItemAdd(Item);
    if Find('Sorted') then Sorted := Bool;
  end;
end;
{$ENDIF}

{$IFDEF TLISTVIEW}
function TFormReader.AddListView(Properties: TControlProperties; Parent: TWinControl): TWinControl;

  procedure AddColumn(const Col: string);
  begin
    (Result as TListView).ColumnAddEx(GetField(Col, 0), StrToInt(GetField(Col, 1)), TTextAlign(StrToInt(GetField(Col, 2))));
  end;

  procedure AddItem(const Item: string);
  begin
    with Result as TListView do
      ItemImageIndex[ItemAdd(GetField(Item, 0))] := StrToInt(GetField(Item, 1));
  end;

var Idx: Integer;

  procedure AddItemEx(const Item: string);
  var
    i: Integer;
  begin
    with Result as TListView do
      for i := 1 to ColumnCount - 1 do
        Items[Idx, i] := GetField(Item, i - 1);
    Inc(Idx);
  end;

begin
  Result:=TListView.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TListView) do
  begin
    if Find('HotTrack') then HotTrack := Bool;
    if Find('FlatScrollBars') then FlatScrollBars := Bool;
    if Find('OptionsEx') then OptionsEx := Int;
    if Find('ViewStyle') then ViewStyle := Int;
    if Find('SmallImages') then SmallImages := LoadImageList(Data);
    if Find('LargeImages') then LargeImages := LoadImageList(Data);
    if Find('StateImages') then StateImages := LoadImageList(Data);
    if Find('OnCompare') then OnCompare := TLVCompareEvent(GetHandler(Str));
    if Find('Columns') then
      while Str <> '' do
        AddColumn(Item);
    if Find('Items') then
      while Str <> '' do
        AddItem(Item);
    Idx := 0;
    if Find('ItemsEx') then
      while Str <> '' do
        AddItemEx(Item);
    if Find('SortType') then SortType := TSortType(Int);
    if Find('SelectedIndex') then SelectedIndex := Int;
  end;
end;
{$ENDIF}

{$IFDEF TMDICHILDFORM}
function TFormReader.AddMDIChildForm(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TMDIChildForm.Create(Parent as TMDIForm, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  SetFormProperties(Result as TForm, Properties);
end;
{$ENDIF}

{$IFDEF TMDIFORM}
function TFormReader.AddMDIForm(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TMDIForm.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  SetFormProperties(Result as TForm, Properties);
end;
{$ENDIF}

{$IFDEF TMEMO}
function TFormReader.AddMemo(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TMemo.Create(Parent, Properties.Str('Text'));
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TMemo) do
  begin
    if Find('TextEx') then Text := Base64Decode(Str);
    if Find('MarginWidth') then MarginWidth := Int;
    if Find('MaxLength') then MaxLength := Int;
    if Find('SelStart') then SelStart := Int;
    if Find('SelLength') then SelLength := Int;
    if Find('SelText') then SelText := Str;
    if Find('PasswordChar') then PasswordChar := Str[1];
    if Find('ReadOnly') then ReadOnly := Bool;
  end;
end;
{$ENDIF}

{$IFDEF TMONTHCALENDAR}
function TFormReader.AddMonthCalendar(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TMonthCalendar.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TMonthCalendar) do
  begin
    if Find('BorderStyle') then BorderStyle := TBorderStyle(Int);
    if Find('DateTime') then DateTime := StrToFloat(Str)
  end;
end;
{$ENDIF}

{$IFDEF TPANEL}
function TFormReader.AddPanel(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TPanel.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  if Properties.Find('Bevel') then (Result as TPanel).Bevel := TBevelType(Properties.Int);
end;
{$ENDIF}

{$IFDEF TPROGRESSBAR}
function TFormReader.AddProgressBar(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TProgressBar.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TProgressBar) do
  begin
    if Find('Min') then Min := Int;
    if Find('Max') then Max := Int;
    if Find('Step') then Step := Int;
    if Find('Position') then Position := Int;
  end;
end;
{$ENDIF}

{$IFDEF TRADIOBUTTON}
function TFormReader.AddRadioButton(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TRadioButton.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  if Properties.Find('Checked') then (Result as TRadioButton).Checked := Properties.Bool;
end;
{$ENDIF}

{$IFDEF TRICHEDIT}
function TFormReader.AddRichEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TRichEdit.Create(Parent, Properties.Str('Text'));
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TRichEdit) do
  begin
    if Find('TextEx') then
    try
      LoadFromStream(Data);
    finally
      Data.Free;
    end;
    if Find('Color') then Color := Int;
    if Find('MaxLength') then MaxLength := Int;
    if Find('SelStart') then SelStart := Int;
  end;
end;
{$ENDIF}

{$IFDEF TSCROLLBAR}
function TFormReader.AddScrollBar(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TScrollBar.Create(Parent, Properties.Bool('Horizontal'));
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TScrollBar) do
  begin
    if Find('Min') then Min := Int;
    if Find('Max') then Max := Int;
    if Find('Position') then Position := Int;
  end;
end;
{$ENDIF}

{$IFDEF TSIMPLEPANEL}
function TFormReader.AddSimplePanel(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TSimplePanel.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  if Properties.Find('Border') then (Result as TSimplePanel).Border := Properties.Int;
end;
{$ENDIF}

{$IFDEF TSPEEDBUTTON}
function TFormReader.AddSpeedButton(Properties: TControlProperties; Parent: TWinControl): TWinControl;
var
  S: string;
begin
  Result := TSpeedButton.Create(Parent, Properties.Str('Caption'));
  SetWinControlProperties(Result, Properties);
  if Properties.Find('Glyph') then
  try
    S := UniTempFile;
    Properties.Data.SaveToFile(S);
    (Result as TSpeedButton).Glyph := LoadImage(0, PChar(S), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE or LR_LOADTRANSPARENT);
  finally
    DeleteFile(S);
    Properties.Data.Free;
  end;
end;
{$ENDIF}

{$IFDEF TSPINEDIT}
function TFormReader.AddSpinEdit(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TSpinEdit.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TSpinEdit) do
  begin
    SetEditProperties(EditBox, Properties);
    if Find('Min') then Min := Int;
    if Find('Max') then Max := Int;
    if Find('Increment') then Increment := Int;
    if Find('Position') then Position := Int;
    if Find('OnEditChange') then EditBox.OnChange := TOnEvent(GetHandler(Str));
    if Find('OnEditKeyDown') then EditBox.OnKeyDown := TOnKey(GetHandler(Str));
    if Find('OnEditKeyUp') then EditBox.OnKeyUp := TOnKey(GetHandler(Str));
    if Find('OnEditMessage') then EditBox.OnMessage := TOnMessage(GetHandler(Str));
  end;
end;
{$ENDIF}

{$IFDEF TSTATUSBAR}
function TFormReader.AddStatusBar(Properties: TControlProperties; Parent: TWinControl): TWinControl;
var
  Idx: Integer;

  procedure SetText(const Text: string);
  begin
    (Result as TStatusBar).SetPartText(Idx, StrToInt(GetField(Text, 1)), GetField(Text, 0));
    Inc(Idx);
  end;

var
  P: array of Integer;
begin
  Result := TStatusBar.Create(Parent, Properties.Str('SimpleText'));
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TStatusBar) do
  begin
    if Find('SimplePanel') then SimplePanel := Bool;
    if Find('Parts') then
    begin
      while Str <> '' do
      begin
        SetLength(P, Length(P) + 1);
        P[High(P)] := StrToInt(Item);
      end;
      SetParts(Length(P), P);
    end;
    Idx := 0;
    if Find('PartsText') then
      while Str <> '' do
        SetText(Item);
  end;
end;
{$ENDIF}

{$IFDEF TTABCONTROL}
function TFormReader.AddTabControl(Properties: TControlProperties; Parent: TWinControl): TWinControl;

  procedure AddTab(const Tab: string);
  begin
    with (Result as TTabControl) do
      TabImageIndex(TabAdd(GetField(Tab, 0)), StrToInt(GetField(Tab, 1)));
  end;

begin
  Result := TTabControl.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TTabControl) do
  begin
    if Find('TabStyle') then Style := TTabStyle(Int);
    if Find('TabPosition') then TabPosition := TTabPosition(Int);
    if Find('Images') then Images := LoadImageList(Data);
    if Find('Tabs') then
      while Str <> '' do
        AddTab(Item);
  end;
end;
{$ENDIF}

{$IFDEF TTOOLBAR}
function TFormReader.AddToolBar(Properties: TControlProperties; Parent: TWinControl): TWinControl;

  procedure AddButton(const Btn: string);
  var
    Idx: Integer;
  begin
    with Result as TToolBar do
    begin
      Idx := ButtonAdd(GetField(Btn, 0), StrToInt(GetField(Btn, 1)));
      ButtonCheck[Idx] := StrToInt(GetField(Btn, 2)) <> 0;
      ButtonPressed[Idx] := StrToInt(GetField(Btn, 3)) <> 0;
    end;
  end;

begin
  Result := TToolBar.Create(Parent, Properties.Bool('Flat'));
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TToolBar) do
  begin
    if Find('StandartImages') and Bool then
    begin
      Find('LargeImages');
      StandartImages(Bool);
    end;
    if Find('Images') then Images := LoadImageList(Data);
    if Find('Buttons') then
      while Str <> '' do
        AddButton(Item);
  end;
end;
{$ENDIF}

{$IFDEF TTRACKBAR}
function TFormReader.AddTrackBar(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TTrackBar.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TTrackBar) do
  begin
    if Find('Min') then Min := Int;
    if Find('Max') then Max := Int;
    if Find('Position') then Position := Int;
  end;
end;
{$ENDIF}

{$IFDEF TTREEVIEW}
function TFormReader.AddTreeView(Properties: TControlProperties; Parent: TWinControl): TWinControl;

  function ParseNode(var Nodes: string): string;
  var
    i, l: Integer;
  begin
    l := 0;
    for i := 1 to Length(Nodes) do
      if (l <= 0) and (Nodes[i] = ItemSep) then
        Break
      else if Nodes[i] = '<' then
        Inc(l)
      else if Nodes[i] = '>' then
        Dec(l);
    Result := Copy(Nodes, 1, i - 1);
    Delete(Nodes, 1, i);
  end;

  function Name(const Node: string): string;
  var
    i: Integer;
  begin
    for i := 1 to Length(Node) do
      if Node[i] = '<' then Break;
    Result := Copy(Node, 1, i - 1);
  end;

  function SubNodes(const Node: string): string;
  var
    b: Integer;
  begin
    b := Pos('<', Node);
    if b = 0 then
      Result := ''
    else
      Result := Copy(Node, b + 1, LastDelimiter('>', Node) - b - 1);
  end;

  procedure ReadNode(Parent: Integer; Nodes: string);
  var
    Node: string;
  begin
    while Nodes <> '' do
    begin
      Node := ParseNode(Nodes);
      ReadNode((Result as TTreeView).ItemInsert(Parent, Name(Node)), SubNodes(Node));
    end;
  end;

begin
  Result := TTreeView.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TTreeView) do
  begin
    if Find('Images') then Images := LoadImageList(Data);
    if Find('StateImages') then StateImages := LoadImageList(Data);
    if Find('Items') then ReadNode(Integer(TVI_ROOT), Str);
  end;
end;
{$ENDIF}

{$IFDEF TUPDOWN}
function TFormReader.AddUpDown(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := TUpDown.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Properties, (Result as TUpDown) do
  begin
    if Find('Associate') then Associate := Control[Str];
    if Find('Min') then Min := Int;
    if Find('Max') then Max := Int;
    if Find('Increment') then Increment := Int;
    if Find('Position') then Position := Int;
  end;
end;
{$ENDIF}

end.
