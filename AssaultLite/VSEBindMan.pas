unit VSEBindMan;

interface

uses
  Windows, AvL, avlUtils, VSEGameStates, VSEGUI, VSEMemPak;

type
  TBindEvent=(beNone, beDown, beUp);
  PEventQueue=^TEventQueue;
  TEventQueue=record
    Next: PEventQueue;
    Event: TBindEvent;
    Age: Byte;
  end;
  TBinding=record
    Name, Descript: string;
    Key: Byte;
    Events: PEventQueue;
  end;
  TBindManCfgForm=class(TGUIForm) // Keys configuration form
  private
    FKeyNames: array[0..255] of string;
    FLabels, FButtons: array of Integer;
    FPageLabel, FPage, FPages, FActive: Integer;
    FOnClose: TOnEvent;
    procedure ChangePage(Btn: PBtn);
    procedure FillKeys;
    procedure KeyBtnClick(Btn: PBtn);
    function KeyToStr(Key: Integer): string;
    procedure CloseClick(Btn: PBtn);
    procedure SetKey(Key: Integer);
  public
    constructor Create(VirtScrW, VirtScrH, X, Y, Width, Height: Integer; Font: Cardinal; const CloseCapt: string);
    destructor Destroy; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
    property  OnClose: TOnEvent read FOnClose write FOnClose; //Triggered at click on 'close' button
  end;
  TBindMan=class
  private
    FBindings: array of TBinding;
    FQueuePool: array of TEventQueue;
    function GetBindActive(Name: string): Boolean;
    function FindBinding(const Name: string): Integer;
    function NewEvent(Event_: TBindEvent): PEventQueue;
    procedure SaveBindings;
  public
    constructor Create; //internally used
    destructor Destroy; override; //internally used
    procedure MouseEvent(Button: Integer; Event: TMouseEvent); //internally used
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); //internally used
    procedure Update; //internally used
    function  GetBindEvent(const Name: string): TBindEvent; //Get oldest event from queue for binding, returns beNone if no events
    property  BindActive[Name: string]: Boolean read GetBindActive; //True if binded key pressed, mouse wheel up/down cannot be pressed, only events
  end;

var
  BindMan: TBindMan; //Global variable for accessing to Bindings Manager

implementation

uses
  VSEInit, VSECore, VSETexMan{$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  SBindCfg = 'Bind.cfg';
  VK_XBUTTON4=5; //Mouse button 4
  VK_XBUTTON5=6; //Mouse button 5
  VK_MWHEELUP=VK_F23; //Mouse wheel up
  VK_MWHEELDOWN=VK_F24; //Mouse wheel down
  MBtnMap: array[mbLeft..mbX2] of Integer = (VK_LBUTTON, VK_RBUTTON, VK_MBUTTON, VK_XBUTTON4, VK_XBUTTON5);
  MaxEventAge=5;
  DeadEvent=255;
  PageLabel='%d/%d';

{ TBindMan }

constructor TBindMan.Create;
var
  Binds: TStringList;
  S: string;
  i, Idx: Integer;
begin
  inherited Create;
  {$IFDEF VSE_LOG}Log(llInfo, 'BindMan: Create');{$ENDIF}
  Binds:=GetFileText(SBindCfg);
  if not Assigned(Binds) then Exit;
  try
    SetLength(FBindings, Binds.Count);
    for i:=0 to Binds.Count-1 do
      with FBindings[i] do
      begin
        S:=Binds[i];
        Name:=Tok(',', S);
        Descript:=Tok(',', S);
        Key:=StrToInt(Tok(',', S));
        Events:=nil;
      end;
    Binds.Text:=VSEInit.Bindings;
    for i:=0 to Binds.Count-1 do
    begin
      S:=Copy(Binds[i], 1, FirstDelimiter('=', Binds[i])-1);
      Idx:=FindBinding(S);
      if Idx>-1 then FBindings[Idx].Key:=StrToInt(Binds.Values[S]);
    end;
  finally
    FAN(Binds);
  end;
  {$IFDEF VSE_LOG}Log(llInfo, 'BindMan: Loaded '+IntToStr(Length(FBindings))+' bindings');{$ENDIF}
  SetLength(FQueuePool, 3*MaxEventAge*Length(FBindings));
  for i:=0 to High(FQueuePool) do
    FQueuePool[i].Age:=DeadEvent;
end;

destructor TBindMan.Destroy;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'BindMan: Destroy');{$ENDIF}
  Finalize(FBindings);
  Finalize(FQueuePool);
  inherited Destroy;
end;

{function TBindMan.CreateConfigForm(VirtScrW, VirtScrH, X, Y, Width, Height: Integer; Font: Cardinal): TBindManCfgForm;
begin
  //SaveBindings;
end;}

function TBindMan.FindBinding(const Name: string): Integer;
var
  i: Integer;
begin
  for i:=0 to High(FBindings) do
    if FBindings[i].Name=Name then
    begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
  {$IFDEF VSE_LOG}Log(llError, 'BindMan: Bind "'+Name+'" not found');{$ENDIF}
end;

function TBindMan.GetBindActive(Name: string): Boolean;
var
  i: Integer;
begin
  Result:=false;
  i:=FindBinding(Name);
  if i>-1 then Result:=Core.KeyPressed[FBindings[i].Key];
end;

function TBindMan.GetBindEvent(const Name: string): TBindEvent;
var
  i: Integer;
begin
  Result:=beNone;
  i:=FindBinding(Name);
  if (i>-1) and Assigned(FBindings[i].Events) then
    with FBindings[i] do
    begin
      Result:=Events^.Event;
      Events^.Age:=DeadEvent;
      Events:=Events^.Next;
    end;
end;

procedure TBindMan.KeyEvent(Button: Integer; Event: TKeyEvent);
const
  EvMap: array[keDown..keUp] of TBindEvent = (beDown, beUp);
var
  i: Integer;
  Ev: PEventQueue;
begin
  for i:=0 to High(FBindings) do
    with FBindings[i] do
      if Key=Button then
      begin
        if Assigned(Events) then
        begin
          Ev:=Events;
          while Assigned(Ev^.Next) do Ev:=Ev^.Next;
          Ev^.Next:=NewEvent(EvMap[Event]);
        end
          else Events:=NewEvent(EvMap[Event]);
        Exit;
      end;
end;

procedure TBindMan.MouseEvent(Button: Integer; Event: TMouseEvent);
const
  EvMap: array[meDown..meUp] of TKeyEvent = (keDown, keUp);
var
  Key: Integer;
begin
  if Event in [meDown, meUp] then KeyEvent(MBtnMap[Button], EvMap[Event]);
  if Event=meWheel then
  begin
    if Button>=0
      then Key:=VK_MWHEELUP
    else begin
      Key:=VK_MWHEELDOWN;
      Button:=-Button;
    end;
    while Button>0 do
    begin
      KeyEvent(Key, keDown);
      KeyEvent(Key, keUp);
      Dec(Button);
    end;
  end;
end;

function TBindMan.NewEvent(Event_: TBindEvent): PEventQueue;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to High(FQueuePool) do
    if FQueuePool[i].Age=DeadEvent then
    begin
      Result:=@FQueuePool[i];
      with Result^ do
      begin
        Event:=Event_;
        Next:=nil;
        Age:=0;
      end;
      Exit;
    end;
  {$IFDEF VSE_LOG}Log(llError, 'BindMan: Event pool overflow');{$ENDIF}
end;

procedure TBindMan.Update;
var
  i: Integer;
  Event: PEventQueue;
begin
  for i:=0 to High(FBindings) do
    with FBindings[i] do
    begin
      while Assigned(Events) and (Events^.Age>=MaxEventAge) do
      begin
        Events^.Age:=DeadEvent;
        Events:=Events^.Next;
      end;
      Event:=Events;
      while Assigned(Event) do
      begin
        Inc(Event^.Age);
        Event:=Event^.Next;
      end;
    end;
end;

procedure TBindMan.SaveBindings;
var
  i: Integer;
begin
  VSEInit.Bindings:='';
  for i:=0 to High(FBindings) do
    VSEInit.Bindings:=VSEInit.Bindings+FBindings[i].Name+'='+IntToStr(FBindings[i].Key)+#13#10;
end;

{ TBindManCfgForm }

constructor TBindManCfgForm.Create(VirtScrW, VirtScrH, X, Y, Width, Height: Integer; Font: Cardinal; const CloseCapt: string);
var
  Btn: TBtn;
  Lbl: TLbl;
  BHeight, i: Integer;
begin
  inherited Create(VirtScrW, VirtScrH, X, Y, Width, Height, Font);
  FActive:=-1;
  for i:=0 to 255 do
  begin
    SetLength(FKeyNames[i], 101);
    SetLength(FKeyNames[i], GetKeyNameText(MapVirtualKey(i, 0) shl 16, @FKeyNames[i][1], 100));
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
  BHeight:=TexMan.TextHeight(Font)+10;
  SetLength(FLabels, Min(Length(BindMan.FBindings), (Height-20-TexMan.TextHeight(Font)) div (BHeight+10)));
  SetLength(FButtons, Length(FLabels));
  FPages:=High(BindMan.FBindings) div Length(FLabels);
  with Btn do
  begin
    Typ:=btPush;
    X:=2*FWidth div 3;
    Width:=FWidth-X-10;
    Height:=BHeight;
    OnClick:=KeyBtnClick;
    Enabled:=true;
  end;
  with Lbl do
  begin
    X:=10;
    Width:=Btn.X-20;
    Align:=laLeft;
    Color:=$FF00B200;
  end;
  for i:=0 to High(FLabels) do
  begin
    with Btn do
    begin
      Y:=20+TexMan.TextHeight(Font)+i*(BHeight+10);
      Tag:=i;
    end;
    Lbl.Y:=25+TexMan.TextHeight(Font)+i*(BHeight+10);
    FButtons[i]:=AddButton(Btn);
    FLabels[i]:=AddLabel(Lbl);
  end;
  FillKeys;
  if FPages>0 then
  begin
    FPageLabel:=CreateSelect(Self, 10, Height-40, Min(Width div 2, Width-140), 30, ChangePage, '<', '>');
    Self.Lbl[FPageLabel]^.Caption:=Format(PageLabel, [FPage+1, FPages+1]);
  end;
  with Btn do
  begin
    Width:=120;
    Y:=FHeight-40;
    X:=FWidth-130;
    Caption:=CloseCapt;
    OnClick:=CloseClick;
    AddButton(Btn);
  end;
end;

procedure TBindManCfgForm.ChangePage(Btn: PBtn);
begin
  FPage:=Min(FPages, Max(0, FPage+Btn^.Tag));
  Lbl[FPageLabel].Caption:=Format(PageLabel, [FPage+1, FPages+1]);
  FillKeys;
end;

procedure TBindManCfgForm.FillKeys;
var
  i: Integer;
begin
  for i:=0 to High(FLabels) do
  begin
    if FPage*Length(FLabels)+i>High(BindMan.FBindings) then
    begin
      Lbl[FLabels[i]]^.Caption:='';
      with Button[FButtons[i]]^ do
      begin
        Caption:='';
        Enabled:=false;
      end;
      Continue;
    end;
    Lbl[FLabels[i]]^.Caption:=BindMan.FBindings[FPage*Length(FLabels)+i].Descript;
    with Button[FButtons[i]]^ do
    begin
      Caption:=KeyToStr(BindMan.FBindings[FPage*Length(FLabels)+i].Key);
      Enabled:=true;
    end;
  end;
end;

procedure TBindManCfgForm.KeyBtnClick(Btn: PBtn);
begin
  FActive:=FPage*Length(FLabels)+Btn^.Tag;
  Btn^.Caption:='???';
end;

procedure TBindManCfgForm.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if FActive>-1 then
  begin
    if Event<>keDown then Exit;
    if Button=VK_ESCAPE then
    begin
      FActive:=-1;
      FillKeys;
    end
      else if Button=VK_BACK
        then SetKey(0)
        else SetKey(Button);
  end
  else begin
    if (Event=keDown) and (Button=VK_ESCAPE)
      then CloseClick(nil)
      else inherited KeyEvent(Button, Event);
  end;
end;

function TBindManCfgForm.KeyToStr(Key: Integer): string;
begin
  Result:=FKeyNames[Key];
  if Result='' then Result:='VK #'+IntToStr(Key);
end;

procedure TBindManCfgForm.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  if FActive>-1 then
  begin
    if not (Event in [meDown, meWheel]) then Exit;
    if Event=meWheel then
    begin
      if Button>0
        then SetKey(VK_MWHEELUP)
        else SetKey(VK_MWHEELDOWN);
    end
      else SetKey(MBtnMap[Button]);
  end
    else inherited MouseEvent(Button, Event, X, Y);
end;

procedure TBindManCfgForm.CloseClick(Btn: PBtn);
begin
  BindMan.SaveBindings;
  if Assigned(FOnClose) then FOnClose(Self);
end;

destructor TBindManCfgForm.Destroy;
begin
  Finalize(FLabels);
  Finalize(FButtons);
  inherited Destroy;
end;

procedure TBindManCfgForm.SetKey(Key: Integer);
var
  i: Integer;
begin
  if Key in [VK_SNAPSHOT] then Exit;
  for i:=0 to High(BindMan.FBindings) do
    if (i<>FActive) and (BindMan.FBindings[i].Key=Key)
      then BindMan.FBindings[i].Key:=BindMan.FBindings[FActive].Key;
  BindMan.FBindings[FActive].Key:=Key;
  FActive:=-1;
  FillKeys;
end;

end.