unit VSESysInfo;

interface

uses
  Windows, AvL, avlUtils, OpenGL, oglExtensions, VSELog;

procedure LogSysInfo;
function GetCPU: string;
function GetMemory: Int64;
function GetMemoryFree: Int64;

implementation

uses VSECore;

type
  TMemoryStatusEx=record
    dwLength:DWORD;
    dwMemoryLoad:DWORD;
    ullTotalPhys:Int64;
    ullAvailPhys:Int64;
    ullTotalPageFile:Int64;
    ullAvailPageFile:Int64;
    ullTotalVirtual:Int64;
    ullAvailVirtual:Int64;
    ullAvailExtendedVirtual:Int64;
  end;
procedure GlobalMemoryStatusEx(var lpBuffer:TMemoryStatusEx); stdcall; external kernel32;

procedure LogSysInfo;
var
  Tmp: Integer;
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
  Log(llInfo, 'VSync control support: '+BoolToStr(WGL_EXT_swap_control));
  Log(llInfo, 'Multitexturing support: '+BoolToStr(GL_ARB_multitexture));
  Log(llInfo, 'FBO support: '+BoolToStr(GL_EXT_framebuffer_object));
  Log(llInfo, 'VBO support: '+BoolToStr(GL_ARB_vertex_buffer_object));
  Log(llInfo, 'GLSL support: '+BoolToStr(GL_ARB_shading_language_100));
  Log(llInfo, 'Maximum texture units: '+IntToStr(glMaxTextureUnits));
  Log(llInfo, 'Maximum texture size: '+IntToStr(glMaxTextureSize));
  Log(llInfo, 'Maximum anisotropy filter: '+IntToStr(glMaxAnisotropy));
  Log(llInfo, 'Maximum texture image units: '+IntToStr(glMaxTextureImageUnits));
  glGetIntegerv(GL_MAX_VERTEX_ATTRIBS_ARB, @Tmp);
  Log(llInfo, 'Maximum vertex attribs: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB, @Tmp);
  Log(llInfo, 'Maximum vertex uniform components: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_VARYING_FLOATS_ARB, @Tmp);
  Log(llInfo, 'Maximum varying floats: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB, @Tmp);
  Log(llInfo, 'Maximum vertex texture image units: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB, @Tmp);
  Log(llInfo, 'Maximum combined texture image units: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_TEXTURE_COORDS_ARB, @Tmp);
  Log(llInfo, 'Maximum texture coords: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB, @Tmp);
  Log(llInfo, 'Maximum fragment uniform components: '+IntToStr(Tmp));
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
  Result:=Trim(Result);
end;

function GetMemory: Int64;
var
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength:=SizeOf(MemStatus);
  GlobalMemoryStatusEx(MemStatus);
  Result:=MemStatus.ullTotalPhys+655360; //With dos base memory
end;

function GetMemoryFree: Int64;
var
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength:=SizeOf(MemStatus);
  GlobalMemoryStatusEx(MemStatus);
  Result:=MemStatus.ullAvailPhys;
end;

end.
