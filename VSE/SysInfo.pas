unit SysInfo;

interface

uses
  Windows, AvL, avlUtils, dglOpenGL, VSELog;

procedure LogSysInfo;
function GetCPU: string;
function GetMemory: Cardinal;
function GetMemoryFree: Cardinal;

implementation

uses VSECore;

procedure LogSysInfo;
begin
  GetWinVer;
  LogRaw('');
  Log(llInfo, 'System:');
  LogF(llInfo, '%s (%d.%d.%d %s)', [Win32Type, Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]);
  Log(llInfo, 'CPU: '+GetCPU);
  LogF(llInfo, 'Memory: total %s, free %s', [SizeToStr(GetMemory), SizeToStr(GetMemoryFree)]);
  Log(llInfo, 'GL_VENDOR='+string(glGetString(GL_VENDOR)));
  Log(llInfo, 'GL_RENDERER='+string(glGetString(GL_RENDERER)));
  Log(llInfo, 'GL_VERSION='+string(glGetString(GL_VERSION)));
  Log(llInfo, 'GL_EXTENSIONS='+string(glGetString(GL_EXTENSIONS)));
  if WGL_ARB_extensions_string
    then Log(llInfo, 'WGL_EXTENSIONS='+string(wglGetExtensionsStringARB(Core.DC)));
  LogRaw('');
end;

function GetCPU: string;
var
  CPUName: array [0..95] of Char;

  procedure GetCPUName;
  asm
    mov eax, $80000002
    db $0F, $A2
    mov dword ptr[CPUName], eax
    mov dword ptr[CPUName+4], ebx
    mov dword ptr[CPUName+8], ecx
    mov dword ptr[CPUName+12], edx

    mov eax, $80000003
    db $0F, $A2
    mov dword ptr[CPUName+16], eax
    mov dword ptr[CPUName+20], ebx
    mov dword ptr[CPUName+24], ecx
    mov dword ptr[CPUName+28], edx

    mov eax, $80000004
    db $0F, $A2
    mov dword ptr[CPUName+32], eax
    mov dword ptr[CPUName+36], ebx
    mov dword ptr[CPUName+40], ecx
    mov dword ptr[CPUName+44], edx
  end;

begin
  try
    GetCPUName;
    Result:=CPUName;
  except
    Result:='Error while detecting CPU!'
  end;
  Result:=PChar(Trim(Result));
end;

function GetMemory: Cardinal;
var
  MemStatus: TMemoryStatus;
begin
  MemStatus.dwLength:=SizeOf(MemStatus);
  GlobalMemoryStatus(MemStatus);
  Result:=MemStatus.dwTotalPhys+655360; //With dos base memory
end;

function GetMemoryFree: Cardinal;
var
  MemStatus: TMemoryStatus;
begin
  MemStatus.dwLength:=SizeOf(MemStatus);
  GlobalMemoryStatus(MemStatus);
  Result:=MemStatus.dwAvailPhys;
end;

end.