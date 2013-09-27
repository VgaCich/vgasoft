unit MulticastEvent;

{ ACHTUNG
Поддержка мультипоточности не предусмотрена
1) EventBroadcaster допустимо использовать только в главном потоке
2) Подписываться на события/посылать события допустимо только из того же потока,
в котором создано событие, создавать событие допустимо в любом потоке
3) Событиям, созданные не в главном потоке, нельзя присваивать BroadcastID,
отличный от пустой строки, в том числе в вызове конструктора
}

{
Именование событий (выбор EventId):
1) Эвенты именовать в стиле 'UnitName.ClassName.EventName'. ClassName - имя
класса, определяющего событие (т.е. сендер если известен сендер, либо листенер,
если известен листенер)
2) Объявлять в классе ClassName константу, содержащую Id
3) Подписываться используя эту константу
Если неприменимо (т.е. неизвестен ни сендер, ни листенер) - объявлять константу
в выделенном модуле <modulename>
}

(*TODO:
1) Cross-thread events
2)
#include "boost/signals.hpp"

class Button
{
public:
    boost::signal<void()> OnPressed; //Сигнал
};
*)

interface

uses SysUtils;

type
  TMulticastEventBase=class
  protected
    type
      TListener = class
      private
        Prev, Next: TListener;
        IsBroadcaster: Boolean;
      public
        Method: TMethod;
        constructor Create(aPrev: TListener);
        destructor Destroy; override;
        procedure Clear;
      end;
  private
    FBroadcastId: AnsiString;
    FBroadcastIdHash: Byte;
    FListeners: TListener;
    FCurListener: TListener;
    procedure SetBroadcastId(const Value: AnsiString);
  protected
    function FindListener(ListenerMethod: TMethod): TListener;
    procedure DoAddListener(ListenerMethod: TMethod);
    procedure DoRemoveListener(ListenerMethod: TMethod);
    function GetBroadcastSender: TMethod;
    function GetNextListener: Boolean;
    function ResetListener: Boolean;
    property Listener: TListener read FCurListener;
  public
    constructor Create; overload;
    constructor Create(aBroadcastId: AnsiString); overload;
    destructor Destroy; override;
    procedure RemoveListenersByOwner(Owner: TObject);
    property BroadcastId: AnsiString read FBroadcastId write SetBroadcastId;
  end;
  CMulticastEvent=class of TMulticastEventBase;

  {$M+}
  TMulticastEvent=class(TMulticastEventBase)
  public
    type
      TEventMethod=procedure (Sender: TObject) of object;
  published
    procedure AddListener(Listener: TEventMethod);
    procedure RemoveListener(Listener: TEventMethod);
    procedure Send(Sender: TObject);
  end;

  TMulticastEvent<T0>=class(TMulticastEventBase)
  public
    type
      TEventMethod=procedure (Sender: TObject; P0: T0) of object;
  published
    procedure AddListener(Listener: TEventMethod);
    procedure RemoveListener(Listener: TEventMethod);
    procedure Send(Sender: TObject; P0: T0);
  end;

  TMulticastEvent<T0, T1>=class(TMulticastEventBase)
  public
    type
      TEventMethod=procedure (Sender: TObject; P0: T0; P1: T1) of object;
  published
    procedure AddListener(Listener: TEventMethod);
    procedure RemoveListener(Listener: TEventMethod);
    procedure Send(Sender: TObject; P0: T0; P1: T1);
  end;

  TMulticastEvent<T0, T1, T2>=class(TMulticastEventBase)
  public
    type
      TEventMethod=procedure (Sender: TObject; P0: T0; P1: T1; P2: T2) of object;
  published
    procedure AddListener(Listener: TEventMethod);
    procedure RemoveListener(Listener: TEventMethod);
    procedure Send(Sender: TObject; P0: T0; P1: T1; P2: T2);
  end;

  TMulticastEvent<T0, T1, T2, T3>=class(TMulticastEventBase)
  public
    type
      TEventMethod=procedure (Sender: TObject; P0: T0; P1: T1; P2: T2; P3: T3) of object;
  published
    procedure AddListener(Listener: TEventMethod);
    procedure RemoveListener(Listener: TEventMethod);
    procedure Send(Sender: TObject; P0: T0; P1: T1; P2: T2; P3: T3);
  end;

  TMulticastEvent<T0, T1, T2, T3, T4>=class(TMulticastEventBase)
  public
    type
      TEventMethod=procedure (Sender: TObject; P0: T0; P1: T1; P2: T2; P3: T3; P4: T4) of object;
  published
    procedure AddListener(Listener: TEventMethod);
    procedure RemoveListener(Listener: TEventMethod);
    procedure Send(Sender: TObject; P0: T0; P1: T1; P2: T2; P3: T3; P4: T4);
  end;

  TMulticastEvent<T0, T1, T2, T3, T4, T5>=class(TMulticastEventBase)
  public
    type
      TEventMethod=procedure (Sender: TObject; P0: T0; P1: T1; P2: T2; P3: T3; P4: T4; P5: T5) of object;
  published
    procedure AddListener(Listener: TEventMethod);
    procedure RemoveListener(Listener: TEventMethod);
    procedure Send(Sender: TObject; P0: T0; P1: T1; P2: T2; P3: T3; P4: T4; P5: T5);
  end;

  TEventBroadcaster=class
  private
    type
      PEventRec=^TEventRec;
      TEventRec=record
        EventId: AnsiString;
        EventSender: TMulticastEventBase;
      end;
  private
    FEvents: array[Byte] of array of TEventRec;
    function FindEvent(EventHash: Byte; const EventId: AnsiString): PEventRec;
  protected
    procedure DoAddListener(const EventId: AnsiString; Listener: TMethod;
        EventSignature: CMulticastEvent);
    procedure DoRemoveListener(const EventId: AnsiString; Listener: TMethod;
        EventSignature: CMulticastEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddListener(const EventId: AnsiString; Listener: TMulticastEvent.TEventMethod); overload;
    procedure AddListener<T0>(const EventId: AnsiString; Listener: TMulticastEvent<T0>.TEventMethod); overload;
    procedure AddListener<T0, T1>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1>.TEventMethod); overload;
    procedure AddListener<T0, T1, T2>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1, T2>.TEventMethod); overload;
    procedure AddListener<T0, T1, T2, T3>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1, T2, T3>.TEventMethod); overload;
    procedure AddListener<T0, T1, T2, T3, T4>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1, T2, T3, T4>.TEventMethod); overload;
    procedure AddListener<T0, T1, T2, T3, T4, T5>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1, T2, T3, T4, T5>.TEventMethod); overload;
    procedure RemoveListenersByOwner(Owner: TObject);
    procedure RemoveListener(const EventId: AnsiString; Listener: TMulticastEvent.TEventMethod); overload;
    procedure RemoveListener<T0>(const EventId: AnsiString; Listener: TMulticastEvent<T0>.TEventMethod); overload;
    procedure RemoveListener<T0, T1>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1>.TEventMethod); overload;
    procedure RemoveListener<T0, T1, T2>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1, T2>.TEventMethod); overload;
    procedure RemoveListener<T0, T1, T2, T3>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1, T2, T3>.TEventMethod); overload;
    procedure RemoveListener<T0, T1, T2, T3, T4>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1, T2, T3, T4>.TEventMethod); overload;
    procedure RemoveListener<T0, T1, T2, T3, T4, T5>(const EventId: AnsiString; Listener: TMulticastEvent<T0, T1, T2, T3, T4, T5>.TEventMethod); overload;
  end;

var
  EventBroadcaster: TEventBroadcaster = nil;

implementation

{$B-}

const
  HashTable: array[0..$FF] of Byte = (
    $01, $57, $31, $0C, $B0, $B2, $66, $A6, $79, $C1, $06, $54, $F9, $E6, $2C, $A3,
    $0E, $C5, $D5, $B5, $A1, $55, $DA, $50, $40, $EF, $18, $E2, $EC, $8E, $26, $C8,
    $6E, $B1, $68, $67, $8D, $FD, $FF, $32, $4D, $65, $51, $12, $2D, $60, $1F, $DE,
    $19, $6B, $BE, $46, $56, $ED, $F0, $22, $48, $F2, $14, $D6, $F4, $E3, $95, $EB,
    $61, $EA, $39, $16, $3C, $FA, $52, $AF, $D0, $05, $7F, $C7, $6F, $3E, $87, $F8,
    $AE, $A9, $D3, $3A, $42, $9A, $6A, $C3, $F5, $AB, $11, $BB, $B6, $B3, $00, $F3,
    $84, $38, $94, $4B, $80, $85, $9E, $64, $82, $7E, $5B, $0D, $99, $F6, $D8, $DB,
    $77, $44, $DF, $4E, $53, $58, $C9, $63, $7A, $0B, $5C, $20, $88, $72, $34, $0A,
    $8A, $1E, $30, $B7, $9C, $23, $3D, $1A, $8F, $4A, $FB, $5E, $81, $A2, $3F, $98,
    $AA, $07, $73, $A7, $F1, $CE, $03, $96, $37, $3B, $97, $DC, $5A, $35, $17, $83,
    $7D, $AD, $0F, $EE, $4F, $5F, $59, $10, $69, $89, $E1, $E0, $D9, $A0, $25, $7B,
    $76, $49, $02, $9D, $2E, $74, $09, $91, $86, $E4, $CF, $D4, $CA, $D7, $45, $E5,
    $1B, $BC, $43, $7C, $A8, $FC, $2A, $04, $1D, $6C, $15, $F7, $13, $CD, $27, $CB,
    $E9, $28, $BA, $93, $C6, $C0, $9B, $21, $A4, $BF, $62, $CC, $A5, $B4, $75, $4C,
    $8C, $24, $D2, $AC, $29, $36, $9F, $08, $B9, $E8, $71, $C4, $E7, $2F, $92, $78,
    $33, $41, $1C, $90, $FE, $DD, $5D, $BD, $C2, $8B, $70, $2B, $47, $6D, $B8, $D1);

function StringHash(const S: AnsiString): Byte;
var
  i: Integer;
begin
  Result:=0;
  for i:=1 to Length(S) do
    Result:=HashTable[Result xor Ord(S[i])];
end;

{ TMulticastEventBase }

procedure TMulticastEventBase.DoAddListener(ListenerMethod: TMethod);
var
  Listener: TListener;
begin
  if Assigned(FindListener(ListenerMethod)) then Exit;
  Listener:=FListeners;
  while Assigned(Listener.Next) do Listener:=Listener.Next;
  Listener.Next:=TListener.Create(Listener);
  Listener.Next.Method:=ListenerMethod;
end;

constructor TMulticastEventBase.Create(aBroadcastId: AnsiString);
begin
  Create;
  BroadcastId:=aBroadcastId;
end;

constructor TMulticastEventBase.Create;
begin
  inherited;
  FListeners:=TListener.Create(nil);
  FListeners.Method.Code:=nil;
end;

destructor TMulticastEventBase.Destroy;
begin
  //Finalize(FListeners);
  FListeners.Clear;
  FreeAndNil(FListeners);
  inherited;
end;

function TMulticastEventBase.FindListener(ListenerMethod: TMethod): TListener;
begin
  Result:=FListeners.Next;
  while Assigned(Result) do
  begin
    if (Result.Method.Code=ListenerMethod.Code) and
       (Result.Method.Data=ListenerMethod.Data) then Exit;
    Result:=Result.Next;
  end;
  Result:=nil;
end;

function TMulticastEventBase.GetBroadcastSender: TMethod;
var
  EventRec: TEventBroadcaster.PEventRec;
begin
  Result.Code:=nil;
  if not Assigned(EventBroadcaster) then Exit;
  EventRec:=EventBroadcaster.FindEvent(FBroadcastIdHash, FBroadcastId);
  if not Assigned(EventRec) then Exit;
  if not EventRec.EventSender.ClassNameIs(Self.ClassName)
    then raise Exception.Create('BroadcastEvent.FindEvent: wrong signature "'+Self.ClassName+'" for EventId "'+FBroadcastId+'"');
  Result.Code:=EventRec.EventSender.MethodAddress('Send');
  Result.Data:=EventRec.EventSender;
end;

procedure TMulticastEventBase.RemoveListenersByOwner(Owner: TObject);
var
  Listener: TListener;
begin
  Listener:=FListeners;
  while Assigned(Listener.Next) do
    if Listener.Next.Method.Data=Owner
      then DoRemoveListener(Listener.Next.Method)
      else Listener:=Listener.Next;
end;

procedure TMulticastEventBase.DoRemoveListener(ListenerMethod: TMethod);
var
  Listener: TListener;
begin
  Listener:=FindListener(ListenerMethod);
  if Assigned(Listener) then
  begin
    if Listener=FCurListener then FCurListener:=Listener.Prev;
    Listener.Free;
  end;
end;

function TMulticastEventBase.GetNextListener: Boolean;
begin
  Result:=Assigned(FCurListener.Next);
  if Result then FCurListener:=FCurListener.Next;
end;

function TMulticastEventBase.ResetListener: Boolean;
begin
  FCurListener:=FListeners;
  if (FBroadcastId<>'') and not Assigned(FCurListener.Method.Code)
    then FListeners.Method:=GetBroadcastSender;
  Result:=Assigned(FCurListener.Method.Code);
end;

procedure TMulticastEventBase.SetBroadcastId(const Value: AnsiString);
begin
  if FBroadcastId=Value then Exit;
  FBroadcastId:=Value;
  FBroadcastIdHash:=StringHash(Value);
  FListeners.Method:=GetBroadcastSender;
end;

{ TMulticastEventBase.TListener }

constructor TMulticastEventBase.TListener.Create(aPrev: TListener);
begin
  inherited Create;
  Prev:=aPrev;
  Next:=nil;
end;

destructor TMulticastEventBase.TListener.Destroy;
begin
  if Assigned(Prev) then Prev.Next:=Next;
  if Assigned(Next) then Next.Prev:=Prev;
  inherited;
end;

procedure TMulticastEventBase.TListener.Clear;
begin
  while Assigned(Next) do Next.Free;
  while Assigned(Prev) do Prev.Free;
end;

{ TMulticastEvent }

procedure TMulticastEvent.AddListener(Listener: TEventMethod);
begin
  DoAddListener(TMethod(Listener));
end;

procedure TMulticastEvent.RemoveListener(Listener: TEventMethod);
begin
  DoRemoveListener(TMethod(Listener));
end;

procedure TMulticastEvent.Send(Sender: TObject);
begin
  if ResetListener then TEventMethod(Listener.Method)(Sender);
  while GetNextListener do
    TEventMethod(Listener.Method)(Sender);
end;

{ TMulticastEvent<T0> }

procedure TMulticastEvent<T0>.AddListener(Listener: TEventMethod);
begin
  DoAddListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0>.RemoveListener(Listener: TEventMethod);
begin
  DoRemoveListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0>.Send(Sender: TObject; P0: T0);
begin
  if ResetListener then TEventMethod(Listener.Method)(Sender, P0);
  while GetNextListener do
    TEventMethod(Listener.Method)(Sender, P0);
end;

{ TMulticastEvent<T0, T1> }

procedure TMulticastEvent<T0, T1>.AddListener(Listener: TEventMethod);
begin
  DoAddListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1>.RemoveListener(Listener: TEventMethod);
begin
  DoRemoveListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1>.Send(Sender: TObject; P0: T0; P1: T1);
begin
  if ResetListener then TEventMethod(Listener.Method)(Sender, P0, P1);
  while GetNextListener do
    TEventMethod(Listener.Method)(Sender, P0, P1);
end;

{ TMulticastEvent<T0, T1, T2> }

procedure TMulticastEvent<T0, T1, T2>.AddListener(Listener: TEventMethod);
begin
  DoAddListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1, T2>.RemoveListener(Listener: TEventMethod);
begin
  DoRemoveListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1, T2>.Send(Sender: TObject; P0: T0; P1: T1;
  P2: T2);
begin
  if ResetListener then TEventMethod(Listener.Method)(Sender, P0, P1, P2);
  while GetNextListener do
    TEventMethod(Listener.Method)(Sender, P0, P1, P2);
end;

{ TMulticastEvent<T0, T1, T2, T3> }

procedure TMulticastEvent<T0, T1, T2, T3>.AddListener(Listener: TEventMethod);
begin
  DoAddListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1, T2, T3>.RemoveListener(
  Listener: TEventMethod);
begin
  DoRemoveListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1, T2, T3>.Send(Sender: TObject; P0: T0; P1: T1;
  P2: T2; P3: T3);
begin
  if ResetListener then TEventMethod(Listener.Method)(Sender, P0, P1, P2, P3);
  while GetNextListener do
    TEventMethod(Listener.Method)(Sender, P0, P1, P2, P3);
end;

{ TMulticastEvent<T0, T1, T2, T3, T4> }

procedure TMulticastEvent<T0, T1, T2, T3, T4>.AddListener(
  Listener: TEventMethod);
begin
  DoAddListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1, T2, T3, T4>.RemoveListener(
  Listener: TEventMethod);
begin
  DoRemoveListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1, T2, T3, T4>.Send(Sender: TObject; P0: T0;
  P1: T1; P2: T2; P3: T3; P4: T4);
begin
  if ResetListener then TEventMethod(Listener.Method)(Sender, P0, P1, P2, P3, P4);
  while GetNextListener do
    TEventMethod(Listener.Method)(Sender, P0, P1, P2, P3, P4);
end;

{ TMulticastEvent<T0, T1, T2, T3, T4, T5> }

procedure TMulticastEvent<T0, T1, T2, T3, T4, T5>.AddListener(
  Listener: TEventMethod);
begin
  DoAddListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1, T2, T3, T4, T5>.RemoveListener(
  Listener: TEventMethod);
begin
  DoRemoveListener(TMethod(Listener));
end;

procedure TMulticastEvent<T0, T1, T2, T3, T4, T5>.Send(Sender: TObject; P0: T0;
  P1: T1; P2: T2; P3: T3; P4: T4; P5: T5);
begin
  if ResetListener then TEventMethod(Listener.Method)(Sender, P0, P1, P2, P3, P4, P5);
  while GetNextListener do
    TEventMethod(Listener.Method)(Sender, P0, P1, P2, P3, P4, P5);
end;

{ TEventBroadcaster }

procedure TEventBroadcaster.DoAddListener(const EventId: AnsiString; Listener:
    TMethod; EventSignature: CMulticastEvent);
var
  EventRec: PEventRec;
  EventHash: Byte;
begin
  EventHash:=StringHash(EventId);
  EventRec:=FindEvent(EventHash, EventId);
  if EventRec=nil then
  begin
    SetLength(FEvents[EventHash], Length(FEvents[EventHash])+1);
    EventRec:=@FEvents[EventHash][High(FEvents[EventHash])];
    EventRec.EventId:=EventId;
    EventRec.EventSender:=EventSignature.Create;;
  end;
  if not EventRec.EventSender.ClassNameIs(EventSignature.ClassName)
    then raise Exception.Create('EventBroadcaster.AddListener: wrong signature "'+EventSignature.ClassName+'" for EventId "'+EventId+'"');
  EventRec.EventSender.DoAddListener(Listener);
end;

procedure TEventBroadcaster.AddListener(const EventId: AnsiString;
  Listener: TMulticastEvent.TEventMethod);
begin
  DoAddListener(EventId, TMethod(Listener), TMulticastEvent);
end;

procedure TEventBroadcaster.AddListener<T0>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0>.TEventMethod);
begin
  DoAddListener(EventId, TMethod(Listener), TMulticastEvent<T0>);
end;

procedure TEventBroadcaster.AddListener<T0, T1>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1>.TEventMethod);
begin
  DoAddListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1>);
end;

procedure TEventBroadcaster.AddListener<T0, T1, T2>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1, T2>.TEventMethod);
begin
  DoAddListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1, T2>);
end;

procedure TEventBroadcaster.AddListener<T0, T1, T2, T3>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1, T2, T3>.TEventMethod);
begin
  DoAddListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1, T2, T3>);
end;

procedure TEventBroadcaster.AddListener<T0, T1, T2, T3, T4>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1, T2, T3, T4>.TEventMethod);
begin
  DoAddListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1, T2, T3, T4>);
end;

procedure TEventBroadcaster.AddListener<T0, T1, T2, T3, T4, T5>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1, T2, T3, T4, T5>.TEventMethod);
begin
  DoAddListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1, T2, T3, T4, T5>);
end;

procedure TEventBroadcaster.RemoveListener(const EventId: AnsiString;
  Listener: TMulticastEvent.TEventMethod);
begin
  DoRemoveListener(EventId, TMethod(Listener), TMulticastEvent);
end;

procedure TEventBroadcaster.RemoveListener<T0>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0>.TEventMethod);
begin
  DoRemoveListener(EventId, TMethod(Listener), TMulticastEvent<T0>);
end;

procedure TEventBroadcaster.RemoveListener<T0, T1>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1>.TEventMethod);
begin
  DoRemoveListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1>);
end;

procedure TEventBroadcaster.RemoveListener<T0, T1, T2>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1, T2>.TEventMethod);
begin
  DoRemoveListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1, T2>);
end;

procedure TEventBroadcaster.RemoveListener<T0, T1, T2, T3>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1, T2, T3>.TEventMethod);
begin
  DoRemoveListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1, T2, T3>);
end;

procedure TEventBroadcaster.RemoveListener<T0, T1, T2, T3, T4>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1, T2, T3, T4>.TEventMethod);
begin
  DoRemoveListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1, T2, T3, T4>);
end;

procedure TEventBroadcaster.RemoveListener<T0, T1, T2, T3, T4, T5>(const EventId: AnsiString;
  Listener: TMulticastEvent<T0, T1, T2, T3, T4, T5>.TEventMethod);
begin
  DoRemoveListener(EventId, TMethod(Listener), TMulticastEvent<T0, T1, T2, T3, T4, T5>);
end;

constructor TEventBroadcaster.Create;
begin
  inherited;
  if Assigned(EventBroadcaster) then
    raise Exception.Create('EventBroadcaster: only one instance allowed');
end;

destructor TEventBroadcaster.Destroy;
var
  i, j: Integer;
begin
  for i:=Low(FEvents) to High(FEvents) do
  begin
    for j:=0 to High(FEvents[i]) do
    begin
      FreeAndNil(FEvents[i][j].EventSender);
      FEvents[i][j].EventId:='';
    end;
    Finalize(FEvents[i]);
  end;
  inherited;
end;

function TEventBroadcaster.FindEvent(EventHash: Byte; const EventId: AnsiString): PEventRec;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to High(FEvents[EventHash]) do
    if FEvents[EventHash][i].EventId=EventId then
      Result:=@FEvents[EventHash][i];
end;

procedure TEventBroadcaster.DoRemoveListener(const EventId: AnsiString;
    Listener: TMethod; EventSignature: CMulticastEvent);
var
  EventRec: PEventRec;
begin
  EventRec:=FindEvent(StringHash(EventId), EventId);
  if not Assigned(EventRec) then Exit;
  if not EventRec.EventSender.ClassNameIs(EventSignature.ClassName)
    then raise Exception.Create('EventBroadcaster.RemoveListener: wrong signature "'+EventSignature.ClassName+'" for EventId "'+EventId+'"');
  EventRec.EventSender.DoRemoveListener(Listener);
end;

procedure TEventBroadcaster.RemoveListenersByOwner(Owner: TObject);
var
  i, j: Integer;
begin
  for i:=0 to High(FEvents) do
    for j:=0 to High(FEvents[i]) do
      FEvents[i][j].EventSender.RemoveListenersByOwner(Owner);
end;

initialization

  EventBroadcaster:=TEventBroadcaster.Create;

finalization

  FreeAndNil(EventBroadcaster);

end.

