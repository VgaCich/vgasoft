unit CPUGraph;

interface

uses Windows, SysInfoAPI;

const
  GraphLength = 16;
  GraphHeight = 16;

type
  TCPULoadGraph = class
  private
    FGraphData: array[0..GraphLength-1] of Integer;
    FCurIndex: Cardinal;
    FMemoryStatus: TMemoryStatusEx;
    FIconDC: HDC;
    FIconMask, FIconBitmap: HBITMAP;
    FCPULoadQuery, FCPULoadCounter: THandle;
    FIconPen: HPEN;
    function GetCPULoad: Integer;
    procedure InitIcon(ParentWindow: THandle; Color: Cardinal);
    function InitCPULoad: Boolean;
  public
    constructor Create(ParentWindow: THandle; Color: Cardinal);
    destructor Destroy; override;
    procedure Update;
    function GetIcon: hIcon;
    function GetCurrentLoad: Integer;
    function GetTotalRAM: Integer;
    function GetFreeRAM: Integer;
  end;

implementation

constructor TCPULoadGraph.Create(ParentWindow: THandle; Color: Cardinal);
begin
  if not InitCPULoad then raise TObject.Create;
  InitIcon(ParentWindow, Color);
  FMemoryStatus.dwLength:=SizeOf(FMemoryStatus);
end;

destructor TCPULoadGraph.Destroy;
begin
  if FCPULoadQuery<>0 then
  begin
    PdhCloseQuery(FCPULoadQuery);
    DeleteDC(FIconDC);
    DeleteObject(FIconPen);
    DeleteObject(FIconMask);
    DeleteObject(FIconBitmap);
  end;
  inherited;
end;

function TCPULoadGraph.GetCPULoad: Integer;
var
  CounterValue: TPdhFmtCounterValue;
begin
  Result:=0;
  if (PdhCollectQueryData(FCPULoadQuery)=ERROR_SUCCESS) and
     (PdhGetFormattedCounterValue(FCPULoadCounter, PDH_FMT_DOUBLE, nil, CounterValue)=ERROR_SUCCESS)
    then Result:=Round(CounterValue.doubleValue);
end;

function TCPULoadGraph.GetIcon: hIcon;
var
  i: Integer;
  IconInfo: TIconInfo;
begin
  SelectObject(FIconDC, FIconBitmap);
  SelectObject(FIconDC, GetStockObject(BLACK_BRUSH));
  PatBlt(FIconDC, 0, 0, GraphLength, GraphHeight, PATCOPY);
  SelectObject(FIconDC, FIconPen);
  for i:=0 to GraphLength-1 do
  begin
    MoveToEx(FIconDC, i, GraphHeight, nil);
    LineTo(FIconDC, i, GraphHeight-(GraphHeight*FGraphData[(FCurIndex+i) mod GraphLength]+50) div 100);
  end;
  with IconInfo do
  begin
    fIcon:=true;
    hbmMask:=FIconMask;
    hbmColor:=FIconBitmap;
  end;
  Result:=CreateIconIndirect(IconInfo);
end;

procedure TCPULoadGraph.InitIcon(ParentWindow: THandle; Color: Cardinal);
var
  DC: HDC;
  Buf: array[0..(GraphLength*GraphHeight div 8)-1] of Byte;
begin
  DC:=GetDC(ParentWindow);
  ZeroMemory(@Buf[0], Length(Buf));
  FIconMask:=CreateBitmap(GraphLength, GraphHeight, 1, 1, @Buf[0]);
  FIconBitmap:=CreateCompatibleBitmap(DC, GraphLength, GraphHeight);
  FIconDC:=CreateCompatibleDC(DC);
  ReleaseDC(ParentWindow, DC);
  FIconPen:=CreatePen(PS_SOLID, 1, Color);
end;

function TCPULoadGraph.InitCPULoad: Boolean;
begin
  Result:=false;
  if PdhLib = 0 then Exit;
  if PdhOpenQuery(nil, 0, FCPULoadQuery)=ERROR_SUCCESS then
  begin
    PdhAddEnglishCounter(FCPULoadQuery, '\Processor(_Total)\% Processor Time', 0, FCPULoadCounter);
    PdhCollectQueryData(FCPULoadQuery);
  end
    else FCPULoadQuery:=0;
  Result:=FCPULoadQuery<>0;
end;

procedure TCPULoadGraph.Update;
begin
  FGraphData[FCurIndex]:=GetCPULoad;
  FCurIndex:=(FCurIndex+1) mod GraphLength;
  GlobalMemoryStatusEx(FMemoryStatus);
end;

function TCPULoadGraph.GetCurrentLoad: Integer;
begin
  Result:=FGraphData[(FCurIndex-1) mod GraphLength];
end;

function TCPULoadGraph.GetFreeRAM: Integer;
begin
  Result:=FMemoryStatus.ullAvailPhys div 1048576;
end;

function TCPULoadGraph.GetTotalRAM: Integer;
begin
  Result:=(FMemoryStatus.ullTotalPhys+655360) div 1048576;
end;

end.
