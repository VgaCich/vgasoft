unit COMPort;

interface

uses
  Windows, SysUtils, Classes;

type
  TFlowControl = (fcNone, fcHardware, fcSoftware, fcBoth);
  TCOMPort=class
  private
    FHandle: THandle;
    FDCB: TDCB;
    function GetBitRate: Integer;
    function GetDataBits: Integer;
    function GetParity: Integer;
    function GetStopBits: Integer;
    procedure SetBitRate(const Value: Integer);
    procedure SetDataBits(const Value: Integer);
    procedure SetFlowControl(const Value: TFlowControl);
    procedure SetParity(const Value: Integer);
    procedure SetStopBits(const Value: Integer);
    procedure UpdateState;
  public
    constructor Create(const Port: string; BufSize: Integer = 1024);
    destructor Destroy; override;
    function Read(out Data; Count: Integer): Integer;
    function Write(const Data; Count: Integer): Integer;
    procedure Purge(RX: Boolean = true; TX: Boolean = true);
    property BitRate: Integer read GetBitRate write SetBitRate;
    property DataBits: Integer read GetDataBits write SetDataBits;
    property FlowControl: TFlowControl write SetFlowControl;
    property Parity: Integer read GetParity write SetParity;
    property StopBits: Integer read GetStopBits write SetStopBits;
  end;

procedure EnumCOMPorts(List: TStrings);

implementation

procedure EnumCOMPorts(List: TStrings);
var
  i: Integer;
  PortHandle: THandle;
begin
  List.BeginUpdate;
  List.Clear;
  for i := 1 to 256 do
  begin
    PortHandle:=CreateFile(PChar('COM'+IntToStr(i)), GENERIC_READ or GENERIC_WRITE,
      0, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
    if PortHandle<>INVALID_HANDLE_VALUE then
    begin
      List.AddObject('COM'+IntToStr(i), TObject(i));
      CloseHandle(PortHandle);
    end;
  end;
  List.EndUpdate;
end;

{ TCOMPort }

const
  dcb_Binary = $00000001;
  dcb_ParityCheck = $00000002;
  dcb_OutxCtsFlow = $00000004;
  dcb_OutxDsrFlow = $00000008;
  dcb_DtrControlMask = $00000030;
  dcb_DtrControlDisable = $00000000;
  dcb_DtrControlEnable = $00000010;
  dcb_DtrControlHandshake = $00000020;
  dcb_DsrSensitvity = $00000040;
  dcb_TXContinueOnXoff = $00000080;
  dcb_OutX = $00000100;
  dcb_InX = $00000200;
  dcb_ErrorChar = $00000400;
  dcb_NullStrip = $00000800;
  dcb_RtsControlMask = $00003000;
  dcb_RtsControlDisable = $00000000;
  dcb_RtsControlEnable = $00001000;
  dcb_RtsControlHandshake = $00002000;
  dcb_RtsControlToggle = $00003000;
  dcb_AbortOnError = $00004000;
  dcb_Reserveds = $FFFF8000;

constructor TCOMPort.Create(const Port: string; BufSize: Integer = 1024);
var
  Timeouts: TCommTimeouts;
begin
  inherited Create;
  FHandle:=FileOpen(Port, fmOpenReadWrite);
  if FHandle=INVALID_HANDLE_VALUE then raise Exception.Create('Couldn''t open port '+Port+#13+SysErrorMessage(GetLastError));
  ZeroMemory(@FDCB, SizeOf(FDCB));
  FDCB.DCBlength:=SizeOf(FDCB);
  GetCommState(FHandle, FDCB);
  with FDCB do
  begin
    Flags:=dcb_Binary;
    XonLim:=256;
    XoffLim:=16;
    XonChar:=#17;
    XoffChar:=#19;
  end;
  UpdateState;
  if not SetupComm(FHandle, BufSize, BufSize) then raise Exception.Create('Couldn''t setup port'#13+SysErrorMessage(GetLastError));
  with Timeouts do
  begin
    Timeouts.ReadIntervalTimeout:=MAXDWORD;
    Timeouts.ReadTotalTimeoutMultiplier:=0;
    Timeouts.ReadTotalTimeoutConstant:=0;
    Timeouts.WriteTotalTimeoutMultiplier:=50;
    Timeouts.WriteTotalTimeoutConstant:=1000;
  end;
  if not SetCommTimeouts(FHandle, Timeouts) then raise Exception.Create('Couldn''t set port timeouts'#13+SysErrorMessage(GetLastError));
end;

destructor TCOMPort.Destroy;
begin
  CloseHandle(FHandle);
  inherited;
end;

function TCOMPort.GetBitRate: Integer;
begin
  Result:=FDCB.BaudRate;
end;

function TCOMPort.GetDataBits: Integer;
begin
  Result:=FDCB.ByteSize;
end;

function TCOMPort.GetParity: Integer;
begin
  Result:=FDCB.Parity;;
end;

function TCOMPort.GetStopBits: Integer;
begin
  Result:=FDCB.StopBits;
end;

procedure TCOMPort.Purge(RX: Boolean = true; TX: Boolean = true);
var
  Flags: Cardinal;
begin
  Flags:=0;
  if RX then Flags:=Flags or PURGE_RXABORT or PURGE_RXCLEAR;
  if TX then Flags:=Flags or PURGE_TXABORT or PURGE_TXCLEAR;
  PurgeComm(FHandle, Flags);
end;

function TCOMPort.Read(out Data; Count: Integer): Integer;
begin
  Result:=FileRead(FHandle, Data, Count);
end;

procedure TCOMPort.SetBitRate(const Value: Integer);
begin
  FDCB.BaudRate:=Value;
  UpdateState;
end;

procedure TCOMPort.SetDataBits(const Value: Integer);
begin
  FDCB.ByteSize:=Value;
  UpdateState;
end;

procedure TCOMPort.SetFlowControl(const Value: TFlowControl);
begin
  FDCB.Flags:=FDCB.Flags and not (dcb_OutxCtsFlow or dcb_OutxDsrFlow or
    dcb_DtrControlMask or dcb_OutX or dcb_InX or dcb_RtsControlMask);
  case Value of
    fcNone: ;
    fcHardware: FDCB.Flags:=FDCB.Flags or dcb_OutxCtsFlow or dcb_RtsControlHandshake;
    fcSoftware: FDCB.Flags:=FDCB.Flags or dcb_OutX or dcb_InX;
    fcBoth: FDCB.Flags:=FDCB.Flags or dcb_OutxCtsFlow or dcb_RtsControlHandshake
      or dcb_OutX or dcb_InX;
  end;
  UpdateState;
end;

procedure TCOMPort.SetParity(const Value: Integer);
begin
  FDCB.Parity:=Value;
  UpdateState;
end;

procedure TCOMPort.SetStopBits(const Value: Integer);
begin
  FDCB.StopBits:=Value;
  UpdateState;
end;

procedure TCOMPort.UpdateState;
begin
  if not SetCommState(FHandle, FDCB) then
    raise Exception.Create('Couldn''t set port state'#13+SysErrorMessage(GetLastError));
end;

function TCOMPort.Write(const Data; Count: Integer): Integer;
begin
  Result:=FileWrite(FHandle, Data, Count);
end;

end.
