//(c)VgaSoft, 2004-2011
unit avlFormReader;

interface

uses
  Windows, Messages, AvL, avlUtils;
  
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
  TOnEventHandlerRequired=function(Sender: TObject; const HandlerName: string): TMethod of object;
  TAddControlFunc=function(Properties: TStringList; Parent: TWinControl): TWinControl of object;
  TFormReader=class(TObject)
  private
    FControls: TList;
    FAddControlFuncs: array of TAddControlFunc;
    FControlsNames, FControlsTypes, FControlTypeNames, FDataList: TStringList;
    FFormData: TStream;
    FCtrlStack: array of TWinControl;
    function  GetControl(const Name: string): TWinControl;
    function  GetControlType(const Name: string): string;
    procedure PushControl(Control: TWinControl);
    function  PopControl: TWinControl;
    function  AddControl(const ControlName, ControlType: string; Properties: TStringList; CurCtrl: TWinControl): TWinControl;
    procedure SetFormData(Data: TStream);
  protected
    S: string;
    I: Integer;
    B: Boolean;
    Data: TMemoryStream;
    SData: TStringList;
    FOnEventHandlerRequired: TOnEventHandlerRequired;
    {$IFDEF TANIMATE} function AddAnimate(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TBUTTON} function AddButton(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TCHECKBOX} function AddCheckBox(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TCOMBOBOX} function AddComboBox(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TDATETIMEPICKER} function AddDateTimePicker(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IF DEFINED(TEDIT) OR DEFINED(TLABELEDEDIT) OR DEFINED(TSPINBOX)} procedure SetEditProperties(Edit: TEdit; Properties: TStringList); {$IFEND}
    {$IFDEF TEDIT} function AddEdit(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TFILELISTBOX} function AddFileListBox(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IF DEFINED(TFORM) OR DEFINED(TMDICHILDFORM) OR DEFINED(TMDIFORM)}procedure SetFormProperties(Form: TWinControl; Properties: TStringList);{$IFEND}
    {$IFDEF TFORM} function AddForm(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TGRAPHICCONTROL} function AddGraphicControl(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TGROUPBOX} function AddGroupBox(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF THEADERCONTROL} function AddHeaderControl(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF THOTKEY} function AddHotKey(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TIMAGE} function AddImage(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TIPEDIT} function AddIPEdit(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IF DEFINED(TLABEL) OR DEFINED(TLABELEDEDIT)} procedure SetLabelProperties(Lbl: TLabel; Properties: TStringList); {$IFEND}
    {$IFDEF TLABEL} function AddLabel(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TLABELEDEDIT} function AddLabeledEdit(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TLISTBOX} function AddListBox(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TLISTVIEW} function AddListView(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TMDICHILDFORM} function AddMDIChildForm(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TMDIFORM} function AddMDIForm(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TMEMO} function AddMemo(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TMONTHCALENDAR} function AddMonthCalendar(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TPANEL} function AddPanel(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TPROGRESSBAR} function AddProgressBar(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TRADIOBUTTON} function AddRadioButton(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TRICHEDIT} function AddRichEdit(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSCROLLBAR} function AddScrollBar(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSIMPLEPANEL} function AddSimplePanel(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSPEEDBUTTON} function AddSpeedButton(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSPINEDIT} function AddSpinEdit(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TSTATUSBAR} function AddStatusBar(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TTABCONTROL} function AddTabControl(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TTOOLBAR} function AddToolBar(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TTRACKBAR} function AddTrackBar(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TTREEVIEW} function AddTreeView(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
    {$IFDEF TUPDOWN} function AddUpDown(Properties: TStringList; Parent: TWinControl): TWinControl; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function ReadForm: Boolean; virtual;
    function GetData(const DataName: string; Data: TStream): Boolean;
    function  GetHandler(const HandlerName: string): TOnEvent;
    procedure AddControlType(const TypeName: string; AddControlFunc: TAddControlFunc);
    function  GetStrProperty(const PropertyName: string; Properties: TStringList; out Res: string): Boolean;
    function  GetIntProperty(const PropertyName: string; Properties: TStringList; out Res: Integer): Boolean;
    function  GetBoolProperty(const PropertyName: string; Properties: TStringList; out Res: Boolean): Boolean;
    procedure GetControlsList(List: TStringList);
    procedure GetTypesList(List: TStringList);
    procedure SetWinControlProperties(Control: TWinControl; Properties: TStringList);
    procedure DeleteControlFromList(Control: TWinControl);
    property  FormData: TStream read FFormData write SetFormData;
    property  OnEventHandlerRequired: TOnEventHandlerRequired read FOnEventHandlerRequired write FOnEventHandlerrequired;
    property  Control[const Name: string]: TWinControl read GetControl; default;
    property  ControlType[const Name: string]: string read GetControlType;
  end;

implementation

type
  TDataTableItem=packed record
    Offset, Size: Cardinal;
  end;

constructor TFormReader.Create;
begin
  inherited Create;
  FControls:=TList.Create;
  FControlsNames:=TStringList.Create;
  FControlsTypes:=TStringList.Create;
  FControlTypeNames:=TStringList.Create;
  FDataList:=TStringList.Create;
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
  Finalize(FAddControlFuncs);
  FreeList(FControls);
  FAN(FDataList);
  FAN(FControlsNames);
  FAN(FControlsTypes);
  FAN(FControlTypeNames);
end;

function TFormReader.GetControl(const Name: string): TWinControl;
begin
  Result:=FControls[FControlsNames.IndexOf(Name)];
end;

function TFormReader.GetControlType(const Name: string): string;
begin
  Result:=FControlsTypes[FControlsNames.IndexOf(Name)];
end;

procedure TFormReader.PushControl(Control: TWinControl);
begin
  SetLength(FCtrlStack, High(FCtrlStack)+2);
  FCtrlStack[High(FCtrlStack)]:=Control;
end;

function TFormReader.PopControl: TWinControl;
begin
  Result:=FCtrlStack[High(FCtrlStack)];
  SetLength(FCtrlStack, High(FCtrlStack));
end;

function TFormReader.AddControl(const ControlName, ControlType: string; Properties: TStringList; CurCtrl: TWinControl): TWinControl;
var
  Index: Integer;
begin
  Result:=nil;
  Index:=FControlTypeNames.IndexOf(ControlType);
  if Index<0 then Exit;
  Result:=FAddControlFuncs[Index](Properties, CurCtrl);
  FControls.Add(Result);
  FControlsNames.Add(ControlName);
  FControlsTypes.Add(ControlType);
end;

procedure TFormReader.SetFormData(Data: TStream);
var
  Size: Cardinal;
  S: string;
begin
  FFormData:=Data;
  Data.Seek(-SizeOf(Size), soFromEnd);
  Data.Read(Size, SizeOf(Size));
  Data.Seek(-(SizeOf(Size)+Size), soFromEnd);
  Data.Read(Size, SizeOf(Size));
  Data.Seek(Size*SizeOf(TDataTableItem), soFromCurrent);
  Data.Read(Size, SizeOf(Size));
  SetLength(S, Size);
  Data.Read(S[1], Size);
  FDataList.Text:=S;
  Data.Position:=0;
end;

function TFormReader.ReadForm: Boolean;
var
  Size, i: Cardinal;
  Controls, Properties: TStringList;
  S, ControlName, ControlType: string;
  CurControl: TWinControl;
  IsProperty: Boolean;
begin
  Result:=false;
  FFormData.Seek(-SizeOf(Size), soFromEnd);
  FFormData.Read(Size, SizeOf(Size));
  FFormData.Position:=0;
  Size:=FFormData.Size-SizeOf(Size)-Size;
  SetLength(S, Size);
  FFormData.Read(S[1], Size);
  try
    Controls:=TStringList.Create;
    Properties:=TStringList.Create;
    CurControl:=nil;
    Controls.Text:=S;
    IsProperty:=false;
    ControlName:='';
    ControlType:='';
    for i:=0 to Controls.Count-1 do
    begin
      S:=Controls[i];
      S:=TrimLeft(S);
      if (Length(S)=0) or (S[1]='#') then Continue;
      if IsProperty then
        if TrimRight(S)='PropEnd' then
        begin
          IsProperty:=false;
          PushControl(CurControl);
          CurControl:=AddControl(ControlName, ControlType, Properties, CurControl);
          if not Assigned(CurControl)
            then raise Exception.Create('Couldn''t create control '+ControlName+': '+ControlType);
          Properties.Clear;
        end
          else Properties.Add(S);
      if not IsProperty and (TrimRight(S)='End') then
        CurControl:=PopControl;
      if not IsProperty and (Copy(S, 1, FirstDelimiter(' ', S)-1)='Control') then
      begin
        IsProperty:=true;
        ControlName:=Trim(Copy(S, FirstDelimiter(' ', S)+1, FirstDelimiter(':', S)-1-FirstDelimiter(' ', S)));
        ControlType:=Trim(Copy(S, FirstDelimiter(':', S)+1, MaxInt));
      end;
    end;
    Result:=true;
  finally
    FAN(Properties);
    FAN(Controls);
  end;
end;

function TFormReader.GetData(const DataName: string; Data: TStream): Boolean;
var
  DataCount, DataStart: Cardinal;
  DataIndex: Integer;
  DataTableItem: TDataTableItem;
begin
  Result:=false;
  DataIndex:=FDataList.IndexOf(DataName);
  if DataIndex<0 then Exit;
  FFormData.Seek(-SizeOf(DataStart), soFromEnd);
  FFormData.Read(DataStart, SizeOf(DataStart));
  DataStart:=FFormData.Size-DataStart-SizeOf(DataStart);
  FFormData.Position:=DataStart;
  FFormData.Read(DataCount, SizeOf(DataCount));
  if DataIndex>DataCount-1 then Exit;
  FFormData.Seek(DataIndex*SizeOf(DataTableItem), soFromCurrent);
  FFormData.Read(DataTableItem, SizeOf(DataTableItem));
  FFormData.Position:=DataTableItem.Offset+DataStart;
  Data.CopyFrom(FFormData, DataTableItem.Size);
  Data.Seek(0, soFromBeginning);
  Result:=true;
end;

function TFormReader.GetHandler(const HandlerName: string): TOnEvent;
begin
  if Assigned(FOnEventHandlerRequired)
    then Result:=TOnEvent(FOnEventHandlerRequired(Self, HandlerName))
    else Result:=nil;
end;

procedure TFormReader.AddControlType(const TypeName: string; AddControlFunc: TAddControlFunc);
begin
  FControlTypeNames.Add(TypeName);
  SetLength(FAddControlFuncs, High(FAddControlFuncs)+2);
  FAddControlFuncs[High(FAddControlFuncs)]:=AddControlFunc;
end;

function TFormReader.GetStrProperty(const PropertyName: string; Properties: TStringList; out Res: string): Boolean;
begin
  Res:=Properties.Values[PropertyName];
  Result:=Properties.IndexOfName(PropertyName)>=0; 
end;

function TFormReader.GetIntProperty(const PropertyName: string; Properties: TStringList; out Res: Integer): Boolean;
var
  S: string;
begin
  Res:=0;
  Result:=GetStrProperty(PropertyName, Properties, S);
  if not Result then Exit;
  Res:=StrToInt(Trim(S));
end;

function TFormReader.GetBoolProperty(const PropertyName: string; Properties: TStringList; out Res: Boolean): Boolean;
var
  S: string;
begin
  Res:=false;
  Result:=GetStrProperty(PropertyName, Properties, S);
  if not Result then Exit;
  Res:=StrToInt(Trim(S))<>0;
end;

procedure TFormReader.GetControlsList(List: TStringList);
begin
  List.Assign(FControlsNames);
end;

procedure TFormReader.GetTypesList(List: TStringList);
begin
  List.Assign(FControlTypeNames);
end;

procedure TFormReader.SetWinControlProperties(Control: TWinControl; Properties: TStringList);
begin
  with Control do
  begin
    if GetIntProperty('Color', Properties, I) then Color:=I;
    if GetIntProperty('Cursor', Properties, I) then Cursor:=I;
    if GetIntProperty('Style', Properties, I) then Style:=Style or I;
    if GetIntProperty('ExStyle', Properties, I) then ExStyle:=ExStyle or I;
    if GetIntProperty('Tag', Properties, I) then Tag:=I;
    if GetIntProperty('TextColor', Properties, I) then TextColor:=I;
    if GetIntProperty('Left', Properties, I) then Left:=I;
    if GetIntProperty('Top', Properties, I) then Top:=I;
    if GetIntProperty('Width', Properties, I) then Width:=I;
    if GetIntProperty('Height', Properties, I) then Height:=I;
    if GetBoolProperty('Ctl3D', Properties, B) then Ctl3D:=B;
    if GetBoolProperty('Enabled', Properties, B) then Enabled:=B;
    if GetBoolProperty('ShowHint', Properties, B) then ShowHint:=B;
    if GetBoolProperty('Visible', Properties, B) then Visible:=B;
    if GetStrProperty('Hint', Properties, S) then Hint:=S;
    if GetStrProperty('TagEx', Properties, S) then TagEx:=S;
    if GetStrProperty('Font', Properties, S) then StrToFont(S, Font);
    Data:=TMemoryStream.Create;
    try
      if GetStrProperty('CustomData', Properties, S) then
      begin
        GetData(S, Data);
        GetMem(Pointer(I), Data.Size);
        CustomData:=Pointer(I);
        Data.Read(CustomData^, Data.Size);
      end;
      if GetStrProperty('Icon', Properties, S) then
      begin
        Data.Clear;
        GetData(S, Data);
        try
          S:=UniTempFile;
          Data.SaveToFile(S);
          Icon:=LoadImage(hInstance, PChar(S), IMAGE_ICON, 0, 0, LR_LOADFROMFILE or LR_LOADTRANSPARENT);
        finally
          DeleteFile(S);
        end;
      end;
    finally
      FAN(Data);
    end;
    if GetStrProperty('OnClick', Properties, S) then OnClick:=TOnEvent(GetHandler(S));
    if GetStrProperty('OnDblClick', Properties, S) then OnDblClick:=TOnEvent(GetHandler(S));
    if GetStrProperty('OnChange', Properties, S) then OnChange:=TOnEvent(GetHandler(S));
    if GetStrProperty('OnCreate', Properties, S) then OnCreate:=TOnEvent(GetHandler(S));
    if GetStrProperty('OnClose', Properties, S) then OnClose:=TOnREvent(GetHandler(S));
    if GetStrProperty('OnDestroy', Properties, S) then OnDestroy:=TOnEvent(GetHandler(S));
    if GetStrProperty('OnKeyDown', Properties, S) then OnKeyDown:=TOnKey(GetHandler(S));
    if GetStrProperty('OnKeyUp', Properties, S) then OnKeyUp:=TOnKey(GetHandler(S));
    if GetStrProperty('OnMessage', Properties, S) then OnMessage:=TOnMessage(GetHandler(S));
    if GetStrProperty('OnMinimize', Properties, S) then OnMinimize:=TOnREvent(GetHandler(S));
    if GetStrProperty('OnMouseDown', Properties, S) then OnMouseDown:=TOnMouse(GetHandler(S));
    if GetStrProperty('OnMouseMove', Properties, S) then OnMouseMove:=TOnMouseMove(GetHandler(S));
    if GetStrProperty('OnMouseUp', Properties, S) then OnMouseUp:=TOnMouse(GetHandler(S));
    if GetStrProperty('OnPaint', Properties, S) then OnPaint:=TOnEvent(GetHandler(S));
    if GetStrProperty('OnResize', Properties, S) then OnResize:=TOnEvent(GetHandler(S));
    if GetStrProperty('OnShow', Properties, S) then OnShow:=TOnEvent(GetHandler(S));
    if GetStrProperty('OnHide', Properties, S) then OnHide:=TOnEvent(GetHandler(S));
  end;
end;

procedure TFormReader.DeleteControlFromList(Control: TWinControl);
var
  i: Integer;
begin
  i:=0;
  while i<FControls.Count-1 do
  begin
    if FControls[i]=Control then
    begin
      FControls.Delete(i);
      FControlsNames.Delete(i);
      FControlsTypes.Delete(i);
    end
      else Inc(i);
  end;
end;

{$IFDEF TANIMATE}
function TFormReader.AddAnimate(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TAnimate.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TAnimate do
  begin
    if GetIntProperty('CommonAVI', Properties, I) then CommonAVI:=TCommonAVI(I);
    if GetIntProperty('StartFrame', Properties, I) then StartFrame:=I;
    if GetIntProperty('StopFrame', Properties, I) then StopFrame:=I;
  end;
end;
{$ENDIF}

{$IFDEF TBUTTON}
function TFormReader.AddButton(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Caption', Properties, S);
  Result:=TButton.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  with Result as TButton do
  begin
    if GetBoolProperty('Default', Properties, B) then Default:=B;
    if GetBoolProperty('Flat', Properties, B) then Flat:=B;
  end;
end;
{$ENDIF}

{$IFDEF TCHECKBOX}
function TFormReader.AddCheckBox(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Caption', Properties, S);
  Result:=TCheckBox.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  if GetBoolProperty('Checked', Properties, B) then (Result as TCheckBox).Checked:=B;
end;
{$ENDIF}

{$IFDEF TCOMBOBOX}
function TFormReader.AddComboBox(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  j: Integer;
begin
  GetIntProperty('ComboStyle', Properties, I);
  Result:=TComboBox.Create(Parent, TComboBoxStyle(I));
  SetWinControlProperties(Result, Properties);
  with Result as TComboBox do
  begin
    if GetStrProperty('Items', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      SData:=TStringList.Create;
      GetData(S, Data);
      SData.LoadFromStream(Data);
      for j:=0 to SData.Count-1 do
        ItemAdd(SData[j]);
    finally
      FAN(Data);
      FAN(SData);
    end;
    if GetStrProperty('Text', Properties, S) then Text:=S;
    if GetIntProperty('ItemIndex', Properties, I) then ItemIndex:=I;
    if GetBoolProperty('DroppedDown', Properties, B) then DroppedDown:=B;
  end;
end;
{$ENDIF}

{$IFDEF TDATETIMEPICKER}
function TFormReader.AddDateTimePicker(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  DT: TDateTime;
begin
  Result:=TDateTimePicker.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TDateTimePicker do
  begin
    if GetIntProperty('DateFormat', Properties, I) then DateFormat:=TDTDateFormat(I);
    if GetIntProperty('Kind', Properties, I) then Kind:=TDateTimeKind(I);
    if GetStrProperty('DateTime', Properties, S) then
    begin
      Data:=TMemoryStream.Create;
      try
        GetData(S, Data);
        Data.Read(DT, SizeOf(DT));
        DateTime:=DT;
      finally
        FAN(Data);
      end;
    end;
  end;
end;
{$ENDIF}

{$IF DEFINED(TEDIT) OR DEFINED(TLABELEDEDIT) OR DEFINED(TSPINEDIT)}
procedure TFormReader.SetEditProperties(Edit: TEdit; Properties: TStringList);
begin
  with Edit do
  begin
    if GetIntProperty('BorderStyle', Properties, I) then BorderStyle:=TBorderStyle(I);
    if GetBoolProperty('Flat', Properties, B) then Flat:=B;
    if GetIntProperty('MarginWidth', Properties, I) then MarginWidth:=I;
    if GetIntProperty('MaxLength', Properties, I) then MaxLength:=I;
    if GetIntProperty('SelStart', Properties, I) then SelStart:=I;
    if GetIntProperty('SelLength', Properties, I) then SelLength:=I;
    if GetStrProperty('SelText', Properties, S) then SelText:=S;
    if GetStrProperty('PasswordChar', Properties, S) then PasswordChar:=S[1];
    if GetBoolProperty('ReadOnly', Properties, B) then ReadOnly:=B;
  end;
end;
{$IFEND}

{$IFDEF TEDIT}
function TFormReader.AddEdit(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Text', Properties, S);
  Result:=TEdit.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  SetEditProperties(Result as TEdit, Properties);
end;
{$ENDIF}

{$IFDEF TFILELISTBOX}
function TFormReader.AddFileListBox(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Path', Properties, S);
  Result:=TFileListBox.Create(Parent, AddTrailingBackslash(S));
  SetWinControlProperties(Result, Properties);
  if GetStrProperty('Mask', Properties, S) then (Result as TFileListBox).Mask:=S;
  (Result as TFileListBox).Update;
end;
{$ENDIF}

{$IF DEFINED(TFORM) OR DEFINED(TMDICHILDFORM) OR DEFINED(TMDIFORM)}
procedure TFormReader.SetFormProperties(Form: TWinControl; Properties: TStringList);
begin
  with Form as TForm do
  begin
    Width:=2*Width-ClientWidth;
    Height:=2*Height-ClientHeight;
    if GetStrProperty('Caption', Properties, S) then Caption:=S;
    if GetIntProperty('AlphaBlendValue', Properties, I) then AlphaBlendValue:=I;
    if GetBoolProperty('AlphaBlend', Properties, B) then AlphaBlend:=B;
    if GetIntProperty('TransparentColorValue', Properties, I) then TransparentColorValue:=I;
    if GetBoolProperty('TransparentColor', Properties, B) then TransparentColor:=B;
    if GetIntProperty('BorderStyle', Properties, I) then BorderStyle:=TFormBorderStyle(I);
    if GetIntProperty('Position', Properties, I) then Position:=I;
    if GetIntProperty('WindowState', Properties, I) then WindowState:=I;
    if GetIntProperty('BorderIcons', Properties, I) then
    begin
      if (I and $1)<>0
        then BorderIcons:=[biSystemMenu]
        else BorderIcons:=[];
      if (I and $2)<>0 then BorderIcons:=BorderIcons+[biMinimize];
      if (I and $4)<>0 then BorderIcons:=BorderIcons+[biMaximize];
      if (I and $8)<>0 then BorderIcons:=BorderIcons+[biHelp];
    end;
  end;
end;
{$IFEND}

{$IFDEF TFORM}
function TFormReader.AddForm(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TForm.Create(Parent, '');
  SetWinControlProperties(Result, Properties);
  SetFormProperties(Result, Properties);
end;
{$ENDIF}

{$IFDEF TGRAPHICCONTROL}
function TFormReader.AddGraphicControl(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TGraphicControl.Create(Parent);
  SetWinControlProperties(Result, Properties);
end;
{$ENDIF}

{$IFDEF TGROUPBOX}
function TFormReader.AddGroupBox(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Caption', Properties, S);
  Result:=TGroupBox.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
end;
{$ENDIF}

{$IFDEF THEADERCONTROL}
function TFormReader.AddHeaderControl(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  j: Integer;
begin
  Result:=THeaderControl.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as THeaderControl do
  begin
    if GetStrProperty('Sections', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      SData:=TStringList.Create;
      GetData(S, Data);
      SData.LoadFromStream(Data);
      for j:=0 to SData.Count-1 do
        SectionAdd(Copy(SData[j], 1, LastDelimiter('|', SData[j])-1),
                   StrToInt(Copy(SData[j], LastDelimiter('|', SData[j])+1, MaxInt)));
    finally
      FAN(Data);
      FAN(SData);
    end;
  end;
end;
{$ENDIF}

{$IFDEF THOTKEY}
function TFormReader.AddHotKey(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=THotKey.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as THotKey do
  begin
    if GetIntProperty('HotKey', Properties, I) then HotKey:=I;
    if GetIntProperty('Modifiers', Properties, I) then
    begin
      if (I and $1)<>0
        then Modifiers:=[mShift]
        else Modifiers:=[];
      if (I and $2)<>0 then Modifiers:=Modifiers+[mCtrl];
      if (I and $4)<>0 then Modifiers:=Modifiers+[mAlt];
      if (I and $8)<>0 then Modifiers:=Modifiers+[mExt];
    end;
  end;
end;
{$ENDIF}

{$IFDEF TIMAGE}
function TFormReader.AddImage(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TImage.Create(Parent);
  SetWinControlProperties(Result, Properties);
  if GetStrProperty('Image', Properties, S) then
  try
    Data:=TMemoryStream.Create;
    GetData(S, Data);
    S:=UniTempFile;
    Data.SaveToFile(S);
    (Result as TImage).LoadFromFile(S);
  finally
    FAN(Data);
    DeleteFile(S);
  end;
end;
{$ENDIF}

{$IFDEF TIPEDIT}
function TFormReader.AddIPEdit(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TIPEdit.Create(Parent, '');
  SetWinControlProperties(Result, Properties);
  if GetIntProperty('IP', Properties, I) then Result.Perform(WM_USER+101, 0, I); 
end;
{$ENDIF}

{$IF DEFINED(TLABEL) OR DEFINED(TLABELEDEDIT)}
procedure TFormReader.SetLabelProperties(Lbl: TLabel; Properties: TStringList);
begin
  with Lbl do
  begin
    if GetIntProperty('Alignment', Properties, I) then Alignment:=I;
    if GetBoolProperty('Transparent', Properties, B) then Transparent:=B;
    if GetIntProperty('BkMode', Properties, I) then BkMode:=TBkMode(I);
  end;
end;
{$IFEND}

{$IFDEF TLABEL}
function TFormReader.AddLabel(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Caption', Properties, S);
  Result:=TLabel.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  SetLabelProperties(Result as TLabel, Properties);
  if GetIntProperty('BorderStyle', Properties, I) then (Result as TLabel).BorderStyle:=TBorderStyle(I);
end;
{$ENDIF}

{$IFDEF TLABELEDEDIT}
function TFormReader.AddLabeledEdit(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  S1: string;
begin
  GetStrProperty('Caption', Properties, S);
  GetStrProperty('Text', Properties, S1);
  Result:=TLabeledEdit.Create(Parent, S, S1);
  SetWinControlProperties(Result, Properties);
  SetEditProperties(Result as TLabeledEdit, Properties);
  SetLabelProperties((Result as TLabeledEdit).EditLabel, Properties);
  if GetIntProperty('LabelBorderStyle', Properties, I) then (Result as TLabeledEdit).EditLabel.BorderStyle:=TBorderStyle(I);
end;
{$ENDIF}

{$IFDEF TLISTBOX}
function TFormReader.AddListBox(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  j: Integer;
begin
  GetIntProperty('ListStyle', Properties, I);
  Result:=TListBox.Create(Parent, TListBoxStyle(I));
  SetWinControlProperties(Result, Properties);
  with Result as TListBox do
  begin
    if GetStrProperty('Items', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      SData:=TStringList.Create;
      GetData(S, Data);
      SData.LoadFromStream(Data);
      for j:=0 to SData.Count-1 do
        ItemAdd(SData[j]);
    finally
      FAN(Data);
      FAN(SData);
    end;
    if GetBoolProperty('Sorted', Properties, B) then Sorted:=B;
  end;
end;
{$ENDIF}

{$IFDEF TLISTVIEW}
function TFormReader.AddListView(Properties: TStringList; Parent: TWinControl): TWinControl;

  function ReadImages: TImageList;
  begin
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      S:=UniTempFile;
      Data.SaveToFile(S);
      Result:=TImageList.Create;
      Result.AddMasked(LoadImage(hInstance, PChar(S), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE or LR_LOADTRANSPARENT), clFuchsia);
    finally
      FAN(Data);
      DeleteFile(S);
    end;
  end;

var
  j, k, W: Integer;
  Capt: string;
  A: TTextAlign;
begin
  Result:=TListView.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TListView do
  begin
    if GetBoolProperty('HotTrack', Properties, B) then HotTrack:=B;
    if GetBoolProperty('FlatScrollBars', Properties, B) then FlatScrollBars:=B;
    if GetIntProperty('OptionsEx', Properties, I) then OptionsEx:=I;
    if GetIntProperty('ViewStyle', Properties, I) then ViewStyle:=I;
    if GetStrProperty('SmallImages', Properties, S) then SmallImages:=ReadImages;
    if GetStrProperty('LargeImages', Properties, S) then LargeImages:=ReadImages;
    if GetStrProperty('StateImages', Properties, S) then StateImages:=ReadImages;
    if GetStrProperty('OnCompare', Properties, S) then OnCompare:=TLVCompareEvent(GetHandler(S));
    if GetStrProperty('Columns', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      SData:=TStringList.Create;
      SData.LoadFromStream(Data);
      for j:=0 to SData.Count-1 do
      begin
        S:=SData[j];
        Capt:=Tok('|', S);
        W:=StrToInt(Tok('|', S));
        A:=TTextAlign(StrToInt(Tok('|', S)));
        ColumnAddEx(Capt, W, A);
      end;
    finally
      FAN(SData);
      FAN(Data);
    end;
    if GetStrProperty('Items', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      SData:=TStringList.Create;
      SData.LoadFromStream(Data);
      for j:=0 to SData.Count-1 do
      begin
        S:=SData[j];
        Capt:=Tok('|', S);
        W:=ItemAdd(Capt);
        ItemImageIndex[W]:=StrToInt(Tok('|', S));
      end;
    finally
      FAN(SData);
      FAN(Data);
    end;
    if GetStrProperty('ItemsEx', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      SData:=TStringList.Create;
      SData.LoadFromStream(Data);
      for j:=0 to SData.Count-1 do
      begin
        S:=SData[j];
        for k:=0 to ColumnCount-1 do
          Items[j, k]:=Tok('|', S);
      end;
    finally
      FAN(SData);
      FAN(Data);
    end;
    if GetIntProperty('SortType', Properties, I) then SortType:=TSortType(I);
    if GetIntProperty('SelectedIndex', Properties, I) then SelectedIndex:=I;
  end;
end;
{$ENDIF}

{$IFDEF TMDICHILDFORM}
function TFormReader.AddMDIChildForm(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TMDIChildForm.Create(Parent as TMDIForm, '');
  SetWinControlProperties(Result, Properties);
  SetFormProperties(Result, Properties);
end;
{$ENDIF}

{$IFDEF TMDIFORM}
function TFormReader.AddMDIForm(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TMDIForm.Create(Parent, '');
  SetWinControlProperties(Result, Properties);
  SetFormProperties(Result, Properties);
end;
{$ENDIF}

{$IFDEF TMEMO}
function TFormReader.AddMemo(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Text', Properties, S);
  Result:=TMemo.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  with Result as TMemo do
  begin
    if GetStrProperty('TextData', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      SetLength(S, Data.Size);
      Data.Read(S[1], Data.Size);
      Text:=S;
    finally
      FAN(Data);
    end;
    if GetIntProperty('MarginWidth', Properties, I) then MarginWidth:=I;
    if GetIntProperty('MaxLength', Properties, I) then MaxLength:=I;
    if GetIntProperty('SelStart', Properties, I) then SelStart:=I;
    if GetIntProperty('SelLength', Properties, I) then SelLength:=I;
    if GetStrProperty('SelText', Properties, S) then SelText:=S;
    if GetStrProperty('PasswordChar', Properties, S) then PasswordChar:=S[1];
    if GetBoolProperty('ReadOnly', Properties, B) then ReadOnly:=B;
  end;
end;
{$ENDIF}

{$IFDEF TMONTHCALENDAR}
function TFormReader.AddMonthCalendar(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  DT: TDateTime;
begin
  Result:=TMonthCalendar.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TMonthCalendar do
  begin
    if GetIntProperty('BorderStyle', Properties, I) then BorderStyle:=TBorderStyle(I);
    if GetStrProperty('DateTime', Properties, S) then
    begin
      Data:=TMemoryStream.Create;
      try
        GetData(S, Data);
        Data.Read(DT, SizeOf(DT));
        DateTime:=DT;
      finally
        FAN(Data);
      end;
    end;
  end;
end;
{$ENDIF}

{$IFDEF TPANEL}
function TFormReader.AddPanel(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Caption', Properties, S);
  Result:=TPanel.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  if GetIntProperty('Bevel', Properties, I) then (Result as TPanel).Bevel:=TBevelType(I);
end;
{$ENDIF}

{$IFDEF TPROGRESSBAR}
function TFormReader.AddProgressBar(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TProgressBar.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TProgressBar do
  begin
    if GetIntProperty('Min', Properties, I) then Min:=I;
    if GetIntProperty('Max', Properties, I) then Max:=I;
    if GetIntProperty('Step', Properties, I) then Step:=I;
    if GetIntProperty('Position', Properties, I) then Position:=I;
  end;
end;
{$ENDIF}

{$IFDEF TRADIOBUTTON}
function TFormReader.AddRadioButton(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Caption', Properties, S);
  Result:=TRadioButton.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  if GetBoolProperty('Checked', Properties, B) then (Result as TRadioButton).Checked:=B;
end;
{$ENDIF}

{$IFDEF TRICHEDIT}
function TFormReader.AddRichEdit(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Text', Properties, S);
  Result:=TRichEdit.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  with Result as TRichEdit do
  begin
    if GetStrProperty('LoadFrom', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      LoadFromStream(Data);
    finally
      FAN(Data);
    end;
    if GetIntProperty('Color', Properties, I) then Color:=I;
    if GetIntProperty('MaxLength', Properties, I) then MaxLength:=I;
    if GetIntProperty('SelStart', Properties, I) then SelStart:=I;
  end;
end;
{$ENDIF}

{$IFDEF TSCROLLBAR}
function TFormReader.AddScrollBar(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetBoolProperty('Horizontal', Properties, B); 
  Result:=TScrollBar.Create(Parent, B);
  SetWinControlProperties(Result, Properties);
  with Result as TScrollBar do
  begin
    if GetIntProperty('Min', Properties, I) then Min:=I;
    if GetIntProperty('Max', Properties, I) then Max:=I;
    if GetIntProperty('Position', Properties, I) then Position:=I;
  end;
end;
{$ENDIF}

{$IFDEF TSIMPLEPANEL}
function TFormReader.AddSimplePanel(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Caption', Properties, S);
  Result:=TSimplePanel.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  if GetIntProperty('Border', Properties, I) then (Result as TSimplePanel).Border:=I;
end;
{$ENDIF}

{$IFDEF TSPEEDBUTTON}
function TFormReader.AddSpeedButton(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  GetStrProperty('Caption', Properties, S);
  Result:=TSpeedButton.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  if GetStrProperty('Glyph', Properties, S) then
  try
    Data:=TMemoryStream.Create;
    GetData(S, Data);
    S:=UniTempFile;
    Data.SaveToFile(S);
    (Result as TSpeedButton).Glyph:=LoadImage(hInstance, PChar(S), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE or LR_LOADTRANSPARENT);
  finally
    FAN(Data);
    DeleteFile(S);
  end;
end;
{$ENDIF}

{$IFDEF TSPINEDIT}
function TFormReader.AddSpinEdit(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TSpinEdit.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TSpinEdit do
  begin
    SetEditProperties(EditBox, Properties);
    if GetIntProperty('Min', Properties, I) then Min:=I;
    if GetIntProperty('Max', Properties, I) then Max:=I;
    if GetIntProperty('Increment', Properties, I) then Increment:=I;
    if GetIntProperty('Position', Properties, I) then Position:=I;
    if GetStrProperty('OnEditChange', Properties, S) then EditBox.OnChange:=TOnEvent(GetHandler(S));
    if GetStrProperty('OnEditKeyDown', Properties, S) then EditBox.OnKeyDown:=TOnKey(GetHandler(S));
    if GetStrProperty('OnEditKeyUp', Properties, S) then EditBox.OnKeyUp:=TOnKey(GetHandler(S));
    if GetStrProperty('OnEditMessage', Properties, S) then EditBox.OnMessage:=TOnMessage(GetHandler(S));
  end;
end;
{$ENDIF}

{$IFDEF TSTATUSBAR}
function TFormReader.AddStatusBar(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  j: Integer;
  P: array of Integer;
  T: string;
begin
  GetStrProperty('SimpleText', Properties, S);
  Result:=TStatusBar.Create(Parent, S);
  SetWinControlProperties(Result, Properties);
  with Result as TStatusBar do
  begin
    if GetBoolProperty('SimplePanel', Properties, B) then SimplePanel:=B;
    if GetStrProperty('Parts', Properties, S) then
    begin
      T:=Tok('|', S);
      repeat
        SetLength(P, Length(P)+1);
        P[High(P)]:=StrToInt(T);
        T:=Tok('|', S);
      until T='';
      SetParts(Length(P), P);
      Finalize(P);
    end;
    if GetStrProperty('PartsText', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      SData:=TStringList.Create;
      SData.LoadFromStream(Data);
      for j:=0 to SData.Count-1 do
        SetPartText(j, StrToInt(Copy(SData[j], LastDelimiter('|', SData[j])+1, MaxInt)),
                    Copy(SData[j], 1, LastDelimiter('|', SData[j])-1));
    finally
      FAN(SData);
      FAN(Data);
    end;
  end;
end;
{$ENDIF}

{$IFDEF TTABCONTROL}
function TFormReader.AddTabControl(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  j, Index: Integer;
  Imgs: TImageList;
begin
  Result:=TTabControl.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TTabControl do
  begin
    if GetIntProperty('TabStyle', Properties, I) then Style:=TTabStyle(I);
    if GetIntProperty('TabPosition', Properties, I) then TabPosition:=TTabPosition(I);
    if GetStrProperty('Images', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      S:=UniTempFile;
      Data.SaveToFile(S);
      Imgs:=TImageList.Create;
      Imgs.AddMasked(LoadImage(hInstance, PChar(S), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE or LR_LOADTRANSPARENT), clFuchsia);
      Images:=Imgs;
    finally
      FAN(Data);
      DeleteFile(S);
    end;
    if GetStrProperty('Tabs', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      SData:=TStringList.Create;
      SData.LoadFromStream(Data);
      for j:=0 to SData.Count-1 do
      begin
        Index:=TabAdd(Copy(SData[j], 1, LastDelimiter('|', SData[j])-1));
        TabImageIndex(Index, StrToInt(Copy(SData[j], LastDelimiter('|', SData[j])+1, MaxInt)));
      end;
    finally
      FAN(SData);
      FAN(Data);
    end;
  end;
end;
{$ENDIF}

{$IFDEF TTOOLBAR}
function TFormReader.AddToolBar(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  Imgs: TImageList;
  j, Index: Integer;
  S, Capt: string;
begin
  GetBoolProperty('Flat', Properties, B);
  Result:=TToolBar.Create(Parent, B);
  SetWinControlProperties(Result, Properties);
  with Result as TToolBar do
  begin
    if GetBoolProperty('StandartImages', Properties, B) and B then
    begin
      GetBoolProperty('LargeImages', Properties, B);
      StandartImages(B);
    end;
    if GetStrProperty('Images', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      S:=UniTempFile;
      Data.SaveToFile(S);
      Imgs:=TImageList.Create;
      Imgs.AddMasked(LoadImage(hInstance, PChar(S), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE or LR_LOADTRANSPARENT), clFuchsia);
      Images:=Imgs;
    finally
      FAN(Data);
      DeleteFile(S);
    end;
    if GetStrProperty('Buttons', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      SData:=TStringList.Create;
      SData.LoadFromStream(Data);
      for j:=0 to SData.Count-1 do
      begin
        S:=SData[j];
        Capt:=Tok('|', S);
        Index:=ButtonAdd(Capt, StrToInt(Tok('|', S)));
        ButtonCheck[Index]:=StrToInt(Tok('|', S))<>0;
        ButtonPressed[Index]:=StrToInt(Tok('|', S))<>0;
      end;
    finally
      FAN(SData);
      FAN(Data);
    end;
  end;
end;
{$ENDIF}

{$IFDEF TTRACKBAR}
function TFormReader.AddTrackBar(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TTrackBar.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TTrackBar do
  begin
    if GetIntProperty('Min', Properties, I) then Min:=I;
    if GetIntProperty('Max', Properties, I) then Max:=I;
    if GetIntProperty('Position', Properties, I) then Position:=I;
  end;
end;
{$ENDIF}

{$IFDEF TTREEVIEW}
function TFormReader.AddTreeView(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  Line: Integer;

  function ReadImages: TImageList;
  begin
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      S:=UniTempFile;
      Data.SaveToFile(S);
      Result:=TImageList.Create;
      Result.AddMasked(LoadImage(hInstance, PChar(S), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE or LR_LOADTRANSPARENT), clFuchsia);
    finally
      FAN(Data);
      DeleteFile(S);
    end;
  end;

  procedure ReadNode(Parent: Integer);
  begin
    while (Line<SData.Count) and (Trim(SData[Line])='') do Inc(Line);
    if Line>=SData.Count then Exit;
    Parent:=(Result as TTreeView).ItemInsert(Parent, TrimLeft(SData[Line]));
    Inc(Line);
    while (Line<SData.Count) and (Trim(SData[Line])<>'') do ReadNode(Parent);
    Inc(Line);
  end;

begin
  Result:=TTreeView.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TTreeView do
  begin
    if GetStrProperty('Images', Properties, S) then Images:=ReadImages;
    if GetStrProperty('StateImages', Properties, S) then StateImages:=ReadImages;
    if GetStrProperty('Items', Properties, S) then
    try
      Data:=TMemoryStream.Create;
      GetData(S, Data);
      SData:=TStringList.Create;
      SData.LoadFromStream(Data);
      Line:=0;
      while Line<SData.Count do ReadNode(Integer(TVI_ROOT));
    finally
      FAN(SData);
      FAN(Data);
    end;
  end;
end;
{$ENDIF}

{$IFDEF TUPDOWN}
function TFormReader.AddUpDown(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=TUpDown.Create(Parent);
  SetWinControlProperties(Result, Properties);
  with Result as TUpDown do
  begin
    if GetStrProperty('Associate', Properties, S) then Associate:=Control[S];
    if GetIntProperty('Min', Properties, I) then Min:=I;
    if GetIntProperty('Max', Properties, I) then Max:=I;
    if GetIntProperty('Increment', Properties, I) then Increment:=I;
    if GetIntProperty('Position', Properties, I) then Position:=I;
  end;
end;
{$ENDIF}

end.
