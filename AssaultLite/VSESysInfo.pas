unit VSESysInfo;

interface

uses
  Windows, AvL, avlUtils, OpenGL, oglExtensions, VSELog;

procedure LogSysInfo;
function GetCPU: string;
function GetMemory: Int64;
function GetMemoryFree: Int64;

implementation

uses VSECore, VSESound;

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
  LogRaw('System:');
  LogRaw(Format('%s (%d.%d.%d %s)', [Win32Type, Win32MajorVersion, Win32MinorVersion, Win32BuildNumber, Win32CSDVersion]));
  LogRaw('CPU: '+GetCPU);
  LogRaw(Format('Memory: total %s, free %s', [SizeToStr(GetMemory), SizeToStr(GetMemoryFree)]));
  LogRaw('');
  LogRaw('OpenGL capabilities:');
  LogRaw('GL_VENDOR='+string(glGetString(GL_VENDOR)));
  LogRaw('GL_RENDERER='+string(glGetString(GL_RENDERER)));
  LogRaw('GL_VERSION='+string(glGetString(GL_VERSION)));
  LogRaw('GL_EXTENSIONS='+string(glGetString(GL_EXTENSIONS)));
  LogRaw('VSync control support: '+BoolToStr(WGL_EXT_swap_control));
  LogRaw('Multitexturing support: '+BoolToStr(GL_ARB_multitexture));
  LogRaw('FBO support: '+BoolToStr(GL_EXT_framebuffer_object));
  LogRaw('VBO support: '+BoolToStr(GL_ARB_vertex_buffer_object));
  LogRaw('GLSL support: '+BoolToStr(GL_ARB_shading_language_100));
  LogRaw('Maximum texture units: '+IntToStr(glMaxTextureUnits));
  LogRaw('Maximum texture size: '+IntToStr(glMaxTextureSize));
  LogRaw('Maximum anisotropy filter: '+IntToStr(glMaxAnisotropy));
  LogRaw('Maximum texture image units: '+IntToStr(glMaxTextureImageUnits));
  glGetIntegerv(GL_MAX_VERTEX_ATTRIBS_ARB, @Tmp);
  LogRaw('Maximum vertex attribs: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB, @Tmp);
  LogRaw('Maximum vertex uniform components: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_VARYING_FLOATS_ARB, @Tmp);
  LogRaw('Maximum varying floats: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB, @Tmp);
  LogRaw('Maximum vertex texture image units: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB, @Tmp);
  LogRaw('Maximum combined texture image units: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_TEXTURE_COORDS_ARB, @Tmp);
  LogRaw('Maximum texture coords: '+IntToStr(Tmp));
  glGetIntegerv(GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB, @Tmp);
  LogRaw('Maximum fragment uniform components: '+IntToStr(Tmp));
  Sound.LogCaps;
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
