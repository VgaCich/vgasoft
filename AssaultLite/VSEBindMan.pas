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
    Name, Description: string;
    Key: Byte;
    Events: PEventQueue;
  end;
  TBindMan=class
  private
    FBindings: array of TBinding;
    FQueuePool: array of TEventQueue;
    FScrollStateClicks: Integer;
    FScrollStateUp: Boolean;
    function GetBindActive(Name: string): Boolean;
    function FindBinding(const Name: string): Integer;
    procedure LoadBindings;
    function NewEvent(Event_: TBindEvent): PEventQueue;
    procedure SaveBindings;
  public
    constructor Create; //internally used
    destructor Destroy; override; //internally used
    procedure MouseEvent(Button: Integer; Event: TMouseEvent); //internally used
    procedure KeyEvent(Key: Integer; Event: TKeyEvent);
    procedure Update; //internally used
    procedure ResetEvents; //internally used
    function  GetBindKeyName(const BindName: string): string; //Get name of binded to BindName key
    function  GetBindEvent(const Name: string): TBindEvent; //Get oldest event from queue for binding, returns beNone if no events
    property  BindActive[Name: string]: Boolean read GetBindActive; //True if binded key pressed, mouse wheel up/down cannot be pressed, only events
  end;
  TBindManCfgForm=class(TGUIForm) // Keys configuration form
  private
    FLabels, FButtons: array of Integer;
    FPageLabel, FPage, FPages, FActive: Integer;
    FOnClose: TOnEvent;
    procedure ChangePage(Btn: PBtn);
    procedure FillKeys;
    procedure KeyBtnClick(Btn: PBtn);
    procedure CloseClick(Btn: PBtn);
    procedure DefaultClick(Btn: PBtn);
    procedure SetKey(Key: Integer);
  public
    constructor Create(VirtScrW, VirtScrH, X, Y, Width, Height: Integer; Font: Cardinal; const DefaultCapt, CloseCapt: string);
    destructor Destroy; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    property  OnClose: TOnEvent read FOnClose write FOnClose; //Triggered at click on 'close' button
  end;

function KeyToStr(Key: Integer): string; //Get name for key code
function ProcessKeyTags(const S: string): string; //Replaces tags $BindName$ by bind BindMane key name, $$ by $

var
  BindMan: TBindMan; //Global variable for accessing to Bindings Manager

implementation

uses
  VSEInit, VSECore, VSETexMan{$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  SSectionBindings = 'Bindings';
  VK_XBUTTON4=5; //Mouse button 4
  VK_XBUTTON5=6; //Mouse button 5
  VK_MWHEELUP=VK_F23; //Mouse wheel up
  VK_MWHEELDOWN=VK_F24; //Mouse wheel down
  MBtnMap: array[mbLeft..mbX2] of Integer = (VK_LBUTTON, VK_RBUTTON, VK_MBUTTON, VK_XBUTTON4, VK_XBUTTON5);
  MaxEventAge=5;
  DeadEvent=255;
  PageLabel='%d/%d';
  TagDelim='$';

var
  KeyNames: array[0..255] of string;

procedure InitKeyNames;
var
  i: Integer;
begin
  for i:=0 to 255 do
  begin
    SetLength(KeyNames[i], 101);
    SetLength(KeyNames[i], GetKeyNameText(MapVirtualKey(i, 0) shl 16, @KeyNames[i][1], 100));
  end;
  KeyNames[0]:='---';
  KeyNames[VK_LBUTTON]:='Left MB';
  KeyNames[VK_RBUTTON]:='Right MB';
  KeyNames[VK_MBUTTON]:='Middle MB';
  KeyNames[VK_XBUTTON4]:='MB 4';
  KeyNames[VK_XBUTTON5]:='MB 5';
  KeyNames[VK_MWHEELUP]:='Wheel Up';
  KeyNames[VK_MWHEELDOWN]:='Wheel Down';
  KeyNames[$03]:='Cancel';
  KeyNames[$0C]:='Clear';
  KeyNames[$13]:='Pause';
  KeyNames[$20]:='Space';
  KeyNames[$21]:='Page Up';
  KeyNames[$22]:='Page Down';
  KeyNames[$23]:='End';
  KeyNames[$24]:='Home';
  KeyNames[$25]:='Left';
  KeyNames[$26]:='Up';
  KeyNames[$27]:='Right';
  KeyNames[$28]:='Down';
  KeyNames[$29]:='Select';
  KeyNames[$2D]:='Insert';
  KeyNames[$2E]:='Delete';
  KeyNames[$5B]:='Left Win';
  KeyNames[$5C]:='Right Win';
  KeyNames[$5D]:='Apps';
  KeyNames[$6F]:='Num /';
  KeyNames[$90]:='Num Lock';
end;

function KeyToStr(Key: Integer): string;
begin
  Result:=KeyNames[Key];
  if Result='' then Result:='VK #'+IntToStr(Key);
end;

function ProcessKeyTags(const S: string): string;
var
  CurPos, Idx: Integer;
begin
  Result:='';
  CurPos:=0;
  Idx:=Pos(TagDelim, S);
  while Idx>0 do
  begin
    Result:=Result+Copy(S, CurPos+1, Idx-CurPos-1);
    if Idx=Length(S) then Break;
    if S[Idx+1]=TagDelim then
    begin
      Result:=Result+TagDelim;
      CurPos:=Idx+1;
    end
    else begin
      CurPos:=PosEx(TagDelim, S, Idx+1);
      if CurPos=0 then Exit;
      Result:=Result+BindMan.GetBindKeyName(Copy(S, Idx+1, CurPos-Idx-1));
    end;
    Idx:=PosEx(TagDelim, S, CurPos+1);
  end;
  Result:=Result+Copy(S, CurPos+1, MaxInt);
end;

{ TBindMan }

constructor TBindMan.Create;
var
  i: Integer;
begin
  inherited Create;
  {$IFDEF VSE_LOG}Log(llInfo, 'BindMan: Create');{$ENDIF}
  LoadBindings;
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
  {$IFDEF VSE_LOG}Log(llWarning, 'BindMan: Bind "'+Name+'" not found');{$ENDIF}
end;

function TBindMan.GetBindActive(Name: string): Boolean;
var
  i: Integer;
begin
  Result:=false;
  i:=FindBinding(Name);
  if i>-1 then
    with FBindings[i] do
    begin
      if Key in [VK_MWHEELUP, VK_MWHEELDOWN] then
      begin
        if (FScrollStateClicks>0) and (((Key=VK_MWHEELUP) and FScrollStateUp) or
                                      ((Key=VK_MWHEELDOWN) and not FScrollStateUp))
          then Result:=true;
      end
        else
        {$IFDEF VSE_CONSOLE}if not Console.Active or (Key in [VK_LBUTTON, VK_RBUTTON, VK_MBUTTON, VK_XBUTTON4, VK_XBUTTON5]) then{$ENDIF}
          Result:=Core.KeyPressed[Key];
    end;
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

function TBindMan.GetBindKeyName(const BindName: string): string;
var
  i: Integer;
begin
  i:=FindBinding(BindName);
  if i>-1
    then Result:=KeyToStr(FBindings[i].Key)
    else Result:=''; 
end;

procedure TBindMan.KeyEvent(Key: Integer; Event: TKeyEvent);
const
  EvMap: array[keDown..keUp] of TBindEvent = (beDown, beUp);
var
  i: Integer;
  Ev: PEventQueue;
begin
  for i:=0 to High(FBindings) do
    if FBindings[i].Key=Key then
      with FBindings[i] do
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

procedure TBindMan.LoadBindings;
var
  BindingsList: TStringList;
  S: string;
  i: Integer;
  Idx: Integer;
begin
  SetLength(FBindings, Length(InitSettings.Bindings));
  for i:=0 to High(InitSettings.Bindings) do
    with FBindings[i] do
    begin
      Name:=InitSettings.Bindings[i].Name;
      Description:=InitSettings.Bindings[i].Description;
      Key:=InitSettings.Bindings[i].Key;
      Events:=nil;
    end;
  BindingsList:=Settings.ReadSection(SSectionBindings);
  try
     for i:=0 to BindingsList.Count-1 do
    begin
      S:=Trim(Copy(BindingsList[i], 1, FirstDelimiter('=', BindingsList[i])-1));
      Idx:=FindBinding(S);
      if Idx>-1 then FBindings[Idx].Key:=StrToInt(BindingsList.Values[S]);
    end;
  finally
    FAN(BindingsList);
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
    FScrollStateClicks:=Button+1;
    FScrollStateUp:=Key=VK_MWHEELUP;
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
  if FScrollStateClicks>0 then Dec(FScrollStateClicks);
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

procedure TBindMan.ResetEvents;
var
  i: Integer;
begin
  FScrollStateClicks:=0;
  for i:=0 to High(FBindings) do
    with FBindings[i] do
      while Assigned(Events) do
      begin
        Events^.Age:=DeadEvent;
        Events:=Events^.Next;
      end;
end;

procedure TBindMan.SaveBindings;
var
  i: Integer;
begin
  for i:=0 to High(FBindings) do
    Settings.Int[SSectionBindings, FBindings[i].Name]:=FBindings[i].Key;
end;

{ TBindManCfgForm }

constructor TBindManCfgForm.Create(VirtScrW, VirtScrH, X, Y, Width, Height: Integer; Font: Cardinal; const DefaultCapt, CloseCapt: string);
var
  Btn: TBtn;
  Lbl: TLbl;
  BHeight, i: Integer;
begin
  inherited Create(VirtScrW, VirtScrH, X, Y, Width, Height, Font);
  FActive:=-1;
  BHeight:=TexMan.TextHeight(Font)+10;
  SetLength(FLabels, Min(Length(BindMan.FBindings), (Height-20-TexMan.TextHeight(Font)) div (BHeight+10)));
  SetLength(FButtons, Length(FLabels));
  FPages:=High(BindMan.FBindings) div Max(Length(FLabels), 1);
  with Btn do
  begin
    Type_:=btPush;
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
    Color:=Integer($FF00B200);
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
    FPageLabel:=CreateSelect(Self, 10, Height-40, Min(Width div 2, Width-280), 30, ChangePage, '<', '>');
    Self.Lbl[FPageLabel]^.Caption:=Format(PageLabel, [FPage+1, FPages+1]);
  end;
  with Btn do
  begin
    Width:=120;
    Y:=FHeight-40;
    X:=FWidth-260;
    Caption:=DefaultCapt;
    OnClick:=DefaultClick;
    AddButton(Btn);
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
    Lbl[FLabels[i]]^.Caption:=BindMan.FBindings[FPage*Length(FLabels)+i].Description;
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

procedure TBindManCfgForm.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if FActive>-1 then
  begin
    if Event<>keUp then Exit;
    if Key=VK_ESCAPE then
    begin
      FActive:=-1;
      FillKeys;
    end
      else if Key=VK_BACK
        then SetKey(0)
        else SetKey(Key);
  end
  else begin
    if (Event=keUp) and (Key=VK_ESCAPE)
      then CloseClick(nil)
      else inherited KeyEvent(Key, Event);
  end;
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

procedure TBindManCfgForm.DefaultClick(Btn: PBtn);
begin
  Settings.EraseSection(SSectionBindings);
  BindMan.LoadBindings;
  FillKeys;
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

initialization
  InitKeyNames;

end.