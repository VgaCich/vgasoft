unit OpenGLExt;

interface

uses Windows, OpenGL, oglExtensions, AvL, avlVectors{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TUpdateMatrixProc=procedure(Width, Height: Integer);
  TUpdateMatrixMethod=procedure(Width, Height: Integer) of object;
  TResolution=record
    Width, Height: Cardinal;
    RefreshRates: array of Cardinal;
  end;
  TResolutions=array of TResolution;

function  gleGoFullscreen(Width, Height, Refresh, depth: Integer): Boolean;
procedure gleGoBack;
function  gleSetPix(DC: HDC; Depth: Cardinal): HGLRC;
procedure gleSetGL;
procedure gleResizeWnd(Width, Height: Integer);
procedure glePerspectiveMatrix(FOV: Single; Width, Height: Integer);
procedure gleOrthoMatrix(Width, Height: Integer);
function  gleError(GLError: Cardinal): string;
procedure gleColor(Color: TColor);
function  gleColorTo4f(Color: TColor): TVector4f;
function  gleGetResolutions: TResolutions;

implementation

function gleGoFullscreen(Width, Height, Refresh, Depth: Integer): Boolean;
var
  DM: DevMode;
begin
  {$IFDEF VSE_LOG}LogF(llInfo, 'Entering fullscreen. Resolution %dx%d@%d', [Width, Height, Refresh]);{$ENDIF}
  ZeroMemory(@DM, SizeOf(DM));
  DM.dmSize:=SizeOf(DM);
  DM.dmBitsPerPel:=Depth;
  DM.dmPelsWidth:=Width;
  DM.dmPelsHeight:=Height;
  DM.dmDisplayFrequency:=Refresh;
  DM.dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  if Refresh>0 then DM.dmFields:=DM.dmFields or DM_DISPLAYFREQUENCY;
  Result:=ChangeDisplaySettings(DM, CDS_TEST)=DISP_CHANGE_SUCCESSFUL;
  if Result then ChangeDisplaySettings(DM, CDS_FULLSCREEN);
end;

procedure gleGoBack;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Leaving fullscreen');{$ENDIF}
  ChangeDisplaySettings(DevMode(nil^), CDS_FULLSCREEN);
end;

function gleSetPix(DC: HDC; Depth: Cardinal): HGLRC;
var
  PFD: TPIXELFORMATDESCRIPTOR;
  PixelFormat: Cardinal;
begin
  Result:=0;
  //Log(llInfo, 'Init OpenGL');
  //InitOpenGL;
  {$IFDEF VSE_LOG}Log(llInfo, 'Setting pixel format');{$ENDIF}
  ZeroMemory(@PFD, SizeOf(TPIXELFORMATDESCRIPTOR));
  with PFD do
  begin
    nSize:=SizeOf(TPIXELFORMATDESCRIPTOR);
    nVersion:=1;
    dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or PFD_SWAP_EXCHANGE;
    iPixelType:=PFD_TYPE_RGBA;
    cColorBits:=Depth;
    cDepthBits:=24;
    iLayerType:=PFD_MAIN_PLANE;
  end;
  PixelFormat:=ChoosePixelFormat(DC, @PFD);
  if PixelFormat=0 then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Unable to find a suitable pixel format');{$ENDIF}
    Exit;
  end;
  if not SetPixelFormat(DC, PixelFormat, @PFD) then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Unable to set the pixel format');{$ENDIF}
    Exit;
  end;
  {$IFDEF VSE_LOG}Log(llInfo, 'Creating rendering context');{$ENDIF}
  Result:=wglCreateContext(DC);
  if Result=0 then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Unable to create an OpenGL rendering context');{$ENDIF}
    Exit;
  end;
  {$IFDEF VSE_LOG}Log(llInfo, 'Activating rendering context');{$ENDIF}
  if not wglMakeCurrent(DC, Result) then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Unable to activate OpenGL rendering context');{$ENDIF}
    wglDeleteContext(Result);
    Result:=0;
    Exit;
  end;
  {$IFDEF VSE_LOG}Log(llInfo, 'Reading extensions');{$ENDIF}
  ReadExtensions;
  //Log(llInfo, 'Read implementation properties');
  //ReadImplementationProperties;
end;

procedure gleSetGL;
begin
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glEnable(GL_COLOR_MATERIAL);
  glShadeModel(GL_SMOOTH);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
end;

procedure gleResizeWnd(Width, Height: Integer);
begin
//  LogF('gleResizeWnd: width=%d, height=%d', [Width, Height]);
  if Height=0 then Height:=1;
  glViewport(0, 0, Width, Height);
end;

procedure glePerspectiveMatrix(FOV: Single; Width, Height: Integer);
begin
  if Height<1 then Height:=1;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(FOV, Width/Height, 0.1, 10000.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure gleOrthoMatrix(Width, Height: Integer);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, Width, Height, 0, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

function gleError(GLError: Cardinal): string;
begin
  case GLError of
    GL_NO_ERROR: Result:='No errors';
    GL_INVALID_ENUM: Result:='Invalid enumeration';
    GL_INVALID_VALUE: Result:='Invalid value';
    GL_INVALID_OPERATION: Result:='Invalid operation';
    GL_STACK_OVERFLOW: Result:='Stack overflow';
    GL_STACK_UNDERFLOW: Result:='Stack underflow';
    GL_OUT_OF_MEMORY: Result:='Out of memory';
    else Result:='Unknown error';
  end;
end;

procedure gleColor(Color: TColor);
var
  Clr: packed array[0..3] of Byte absolute Color;
begin
  glColor4ub(Clr[0], Clr[1], Clr[2], Clr[3]);
end;

function gleColorTo4f(Color: TColor): TVector4f;
var
  Clr: packed array[0..3] of Byte absolute Color;
begin
  with Result do
  begin
    Red:=Clr[0]/255;
    Green:=Clr[1]/255;
    Blue:=Clr[2]/255;
    Alpha:=Clr[3]/255;
  end;
end;

function gleGetResolutions: TResolutions;

  procedure AddResolution(Width, Height, Refresh: Cardinal);
  var
    i, j: Integer;
  begin
    for i:=0 to High(Result) do
      if (Result[i].Width=Width) and (Result[i].Height=Height)
        then Break;
    if Length(Result)=0 then i:=0;
    if i=Length(Result) then SetLength(Result, i+1);
    Result[i].Width:=Width;
    Result[i].Height:=Height;
    for j:=0 to High(Result[i].RefreshRates) do
      if Result[i].RefreshRates[j]=Refresh then Exit;
    SetLength(Result[i].RefreshRates, Length(Result[i].RefreshRates)+1);
    Result[i].RefreshRates[High(Result[i].RefreshRates)]:=Refresh;
  end;

  function Compare(A, B: TResolution): Boolean;
  begin
    Result:=A.Width>B.Width;
    if A.Width=B.Width then Result:=A.Height>B.Height;
  end;

var
  i, j, SC: Integer;
  TmpR: TResolution;
  TmpF: Cardinal;
  DM: TDevMode;
begin
  ZeroMemory(@DM, SizeOf(DM));
  i:=1;
  while EnumDisplaySettings(nil, i, DM) do
  begin
    if (DM.dmPelsWidth >= 640) and (DM.dmPelsHeight >= 480) and (DM.dmDisplayFrequency <> 1)
      then AddResolution(DM.dmPelsWidth, DM.dmPelsHeight, DM.dmDisplayFrequency);
    Inc(i);
  end;
  repeat
    SC:=0;
    for i:=0 to High(Result)-1 do
      if Compare(Result[i], Result[i+1]) then
      begin
        TmpR:=Result[i];
        Result[i]:=Result[i+1];
        Result[i+1]:=TmpR;
        Inc(SC);
      end;
  until SC=0;
  for i:=0 to High(Result) do
    repeat
      SC:=0;
      for j:=0 to High(Result[i].RefreshRates)-1 do
        if Result[i].RefreshRates[j]>Result[i].RefreshRates[j+1] then
        begin
          TmpF:=Result[i].RefreshRates[j];
          Result[i].RefreshRates[j]:=Result[i].RefreshRates[j+1];
          Result[i].RefreshRates[j+1]:=TmpF;
          Inc(SC);
        end;
    until SC=0;
end;

end.
