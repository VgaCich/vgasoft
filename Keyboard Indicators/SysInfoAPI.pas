unit SysInfoAPI;

interface

uses Windows;

// PDH API

type
  TPdhFmtCounterValue = record
    CStatus: Cardinal;
    case Integer of
      0: (longValue: Integer);
      1: (doubleValue: Double);
      2: (largeValue: Int64);
      3: (AnsiStringValue: PChar);
      4: (WideStringValue: PWideChar);
  end;

const
  PDH_FMT_RAW          = $00000010;
  PDH_FMT_ANSI         = $00000020;
  PDH_FMT_UNICODE      = $00000040;
  PDH_FMT_LONG         = $00000100;
  PDH_FMT_DOUBLE       = $00000200;
  PDH_FMT_LARGE        = $00000400;
  PDH_FMT_NOSCALE      = $00001000;
  PDH_FMT_1000         = $00002000;
  PDH_FMT_NODATA       = $00004000;
  PDH_FMT_NOCAP100     = $00008000;
  PERF_DETAIL_COSTLY   = $00010000;
  PERF_DETAIL_STANDARD = $0000FFFF;

var
  PdhOpenQuery: function (szDataSource: PChar; dwUserData: Cardinal; var phQuery: THandle): Integer; stdcall = nil;
  PdhCloseQuery: function (hQuery: THandle): Integer; stdcall = nil;
  PdhAddEnglishCounter: function (hQuery: THandle; szFullCounterPath: PChar; dwUserData: Cardinal; var phCounter: THandle): Integer; stdcall = nil;
  PdhCollectQueryData: function (hQuery: THandle): Integer; stdcall = nil;
  PdhGetFormattedCounterValue: function (hCounter: THandle; dwFormat: Cardinal; lpdwType: PCardinal; var pValue: TPdhFmtCounterValue): Integer; stdcall = nil;

// Memory API

type
  TMemoryStatusEx = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: Int64;
    ullAvailPhys: Int64;
    ullTotalPageFile: Int64;
    ullAvailPageFile: Int64;
    ullTotalVirtual: Int64;
    ullAvailVirtual: Int64;
    ullAvailExtendedVirtual: Int64;
  end;
  TGlobalMemoryStatusEx = procedure(var lpBuffer: TMemoryStatusEx); stdcall;// external kernel32;

var
  GlobalMemoryStatusEx: TGlobalMemoryStatusEx = nil;

implementation

var
  PdhLib, Kernel32Lib: hModule;

procedure GlobalMemoryStatusWrapper(var lpBuffer: TMemoryStatusEx); stdcall;
var
  Status: TMemoryStatus;
begin
  GlobalMemoryStatus(Status);
  with lpBuffer do
  begin
    dwLength := Status.dwLength;
    dwMemoryLoad := Status.dwMemoryLoad;
    with Status do
    begin
      ullTotalPhys := dwTotalPhys;
      ullAvailPhys := dwAvailPhys;
      ullTotalPageFile := dwTotalPageFile;
      ullAvailPageFile := dwAvailPageFile;
      ullTotalVirtual := dwTotalVirtual;
      ullAvailVirtual := dwAvailVirtual;
    end;
    ullAvailExtendedVirtual := 0;
  end;
end;

initialization

  PdhLib := LoadLibrary('pdh.dll');
  if PdhLib <> 0 then
  begin
    PdhOpenQuery := GetProcAddress(PdhLib, 'PdhOpenQueryA');
    PdhCloseQuery := GetProcAddress(PdhLib, 'PdhCloseQuery');
    PdhAddEnglishCounter := GetProcAddress(PdhLib, 'PdhAdd009CounterA');
    PdhCollectQueryData := GetProcAddress(PdhLib, 'PdhCollectQueryData');
    PdhGetFormattedCounterValue := GetProcAddress(PdhLib, 'PdhGetFormattedCounterValue');
  end;
  Kernel32Lib := LoadLibrary('kernel32.dll');
  if Kernel32Lib <> 0 then
    GlobalMemoryStatusEx := GetProcAddress(Kernel32Lib, 'GlobalMemoryStatusEx');
  if not Assigned(GlobalMemoryStatusEx) then
    GlobalMemoryStatusEx := @GlobalMemoryStatusWrapper;

finalization

  if PdhLib <> 0 then FreeLibrary(PdhLib);
  if Kernel32Lib <> 0 then FreeLibrary(Kernel32Lib);

end.