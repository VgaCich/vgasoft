unit OpenGLExt;

interface

uses Windows, dglOpenGL, AvL, Textures, ULog;

type
  TUpdateMatrixProc=procedure(Width, Height: Integer);
  TUpdateMatrixMethod=procedure(Width, Height: Integer) of object;

function  gleGoFullscreen(Width, Height, Refresh, depth: Integer): Boolean;
procedure gleGoBack;
function  gleSetPix(DC: HDC; Depth: Cardinal): HGLRC;
procedure gleSetGL;
procedure gleDefaultUpdateMatrixProc(Width, Height: Integer);
procedure gleOrthoUpdateMatrixProc(Width, Height: Integer);
procedure gleSetUpdateMatrixProc(Proc: TUpdateMatrixProc);
procedure gleSetUpdateMatrixMethod(Method: TUpdateMatrixMethod);
procedure gleResizeWnd(Width, Height: Integer);
function  gleError(GLError: Cardinal): string;
function  gleLoadFont(const FontID: string; FontData: TStream): Boolean;
procedure gleFreeFonts;
procedure gleFreeFont(const FontID: string);
procedure gleSelectFont(FontID: string);
function  gleTextWidth(const Text: string): Double;
procedure gleWrite(const Text: string); overload;
procedure gleWrite(X, Y: Double; const Text: string); overload;
{procedure glCreateFont(Font: HFONT; DC: HDC; BaZe: Cardinal; Deviation, Extrusion: GLFloat; PolygonZ: Boolean); overload;
procedure glCreateFont(Font: HFONT; DC: HDC; BaZe: Cardinal); overload;
procedure glCreateFontO(Font: HFONT; DC: HDC; BaZe: Cardinal; Deviation, Extrusion: GLFloat; PolygonZ: Boolean); overload;
procedure glCreateFontB(Font: HFONT; DC: HDC; BaZe: Cardinal); overload;
procedure glWrite(BaZe: Cardinal; PhraZe: String; TeXTurinG: Boolean);
procedure glCopyTexImage2D(target: GLEnum; level: GLint; internalFormat: GLEnum; x, y: GLint; width, height: GLsizei; border: GLint); stdcall; external 'opengl32.dll';}

implementation

function gleGoFullscreen(Width, Height, Refresh, Depth: Integer): Boolean;
var
  DM: DevMode;
begin
  LogF('Entering fullscreen. Resolution %dx%d@%d', [Width, Height, Refresh]);
  ZeroMemory(@DM, SizeOf(DM));
  DM.dmSize:=SizeOf(DM);
  DM.dmBitsPerPel:=Depth;
  DM.dmPelsWidth:=Width;
  DM.dmPelsHeight:=Height;
  DM.dmDisplayFrequency:=Refresh;
  DM.dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  if Refresh>0 then DM.dmFields:=DM.dmFields or DM_DISPLAYFREQUENCY;
  Result:=False;
  if ChangeDisplaySettings(DM, CDS_FULLSCREEN)<>DISP_CHANGE_SUCCESSFUL then Exit;
  Result:=True;
end;

procedure gleGoBack;
begin
  Log('Leaving fullscreen');
  ChangeDisplaySettings(DevMode(nil^), CDS_FULLSCREEN);
end;

function gleSetPix(DC: HDC; Depth: Cardinal): HGLRC;
var
  PFD: TPIXELFORMATDESCRIPTOR;
  PixelFormat: Cardinal;
begin
  Log('Init OpenGL');
  InitOpenGL;
  Log('Setting pixel format');
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
    Log('Unable to find a suitable pixel format');
    Halt(1);
  end;
  if not SetPixelFormat(DC, PixelFormat, @PFD) then
  begin
    Log('Unable to set the pixel format');
    Halt(1);
  end;
  Log('Creating rendering context');
  Result:=wglCreateContext(DC);
  if Result=0 then
  begin
    Log('Unable to create an OpenGL rendering context');
    Halt(1);
  end;
  Log('Activating rendering context');
  if not wglMakeCurrent(DC, Result) then
  begin
    Log('Unable to activate OpenGL rendering context');
    Halt(1);
  end;
  Log('Reading extensions');
  ReadExtensions;
  Log('Read implementation properties');
  ReadImplementationProperties;
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

var
  UpdateMatrixProc: TUpdateMatrixProc;
  UpdateMatrixMethod: TUpdateMatrixMethod;

procedure gleDefaultUpdateMatrixProc(Width, Height: Integer);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(70.0, Width/Height, 0.1, 10000.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure gleOrthoUpdateMatrixProc(Width, Height: Integer);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, 800, 600, 0, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure gleInvokeUpdateMatrixMethod(Width, Height: Integer);
begin
  UpdateMatrixMethod(Width, Height);
end;

procedure gleSetUpdateMatrixProc(Proc: TUpdateMatrixProc);
begin
  UpdateMatrixProc:=Proc;
end;

procedure gleSetUpdateMatrixMethod(Method: TUpdateMatrixMethod);
begin
  gleSetUpdateMatrixProc(gleInvokeUpdateMatrixMethod);
  UpdateMatrixMethod:=Method;
end;

procedure gleResizeWnd(Width, Height: Integer);
begin
//  LogF('gleResizeWnd: width=%d, height=%d', [Width, Height]);
  if Height=0 then Height:=1;
  glViewport(0, 0, Width, Height);
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

type
  TFontRec=record
    FontTex, FontBase: Cardinal;
    Exist: Boolean;
    FontWidth: packed array[0..255] of Byte;
    ID: string;
  end;

var
  Fonts: array of TFontRec;
  CurFont: Integer;

function  gleLoadFont(const FontID: string; FontData: TStream): Boolean;
var
  FI, i: Integer;
  X, Y, XS: glFloat;
begin
  Result:=false;
  Log('Loading font ID='+FontID);
  FI:=-1;
  for i:=0 to Length(Fonts)-1 do
    if not Fonts[i].Exist then
    begin
      FI:=i;
      Break;
    end;
  if FI<0 then
  begin
    SetLength(Fonts, Length(Fonts)+1);
    FI:=Length(Fonts)-1;
  end;
  Fonts[FI].ID:=LowerCase(FontID);
  Fonts[FI].FontTex:=LoadTexture(FontData, tfFNT, true, GL_LINEAR_MIPMAP_NEAREST, GL_LINEAR);
  if Fonts[FI].FontTex=0 then
  begin
    Log('Unable to load font texture');
    Exit;
  end;
  Move(LastFNTHdr.FontWidth, Fonts[FI].FontWidth, 256);
  Fonts[FI].FontBase:=glGenLists(256);
  for i:=0 to 255 do
  begin
    X:=(i mod 16) / 16;
    Y:=(i div 16) / 16;
    glNewList(Fonts[FI].FontBase+i, GL_COMPILE);
    glBegin(GL_QUADS);
      XS:=(16-Fonts[FI].FontWidth[i])/512;
      glTexCoord2f(X+XS,1-Y);
      glVertex2i(0, 0);
      glTexCoord2f(X+XS+Fonts[FI].FontWidth[i]/256 + 1/512, 1-Y);
      glVertex2i(Fonts[FI].FontWidth[i], 0);
      glTexCoord2f(X+XS+Fonts[FI].FontWidth[i]/256 + 1/512, 0.9375-Y);
      glVertex2i(Fonts[FI].FontWidth[i], 16);
      glTexCoord2f(X+XS, 0.9375-Y);
      glVertex2i(0, 16);
    glEnd;
    glTranslatef(Fonts[FI].FontWidth[i]+1, 0, 0);
    glEndList;
  end;
  Fonts[FI].Exist:=true;
  Result:=true;
end;

procedure GleFreeFonts;
var
  i: Integer;
begin
  if Length(Fonts)=0 then Exit;
  LogNC('Freeing fonts:');
  for i:=0 to Length(Fonts)-1 do
    if Fonts[i].Exist then
    begin
      LogNC('  '+Fonts[i].ID);
      glDeleteTextures(1, @(Fonts[i].FontTex));
      glDeleteLists(Fonts[i].FontBase, 256);
    end;
  CurFont:=-1;
  Finalize(Fonts);
end;

procedure gleFreeFont(const FontID: string);
var
  OldFont: Integer;
begin
  Log('Freeing font ID='+FontID);
  OldFont:=CurFont;
  gleSelectFont(FontID);
  if (CurFont=OldFont) and (LowerCase(FontID)<>Fonts[CurFont].ID) or not Fonts[CurFont].Exist then Exit;
  glDeleteTextures(1, @(Fonts[CurFont].FontTex));
  glDeleteLists(Fonts[CurFont].FontBase, 256);
  Fonts[CurFont].Exist:=false;
  Fonts[CurFont].ID:='';
  CurFont:=OldFont;
end;

procedure gleSelectFont(FontID: string);
var
  i: Integer;
begin
  FontID:=LowerCase(FontID);
  if FontID=Fonts[CurFont].ID then Exit;
  for i:=0 to Length(Fonts)-1 do
    if Fonts[i].Exist and (FontID=Fonts[i].ID) then
    begin
      CurFont:=i;
      Break;
    end;
end;

function  gleTextWidth(const Text: string): Double;
var
  i: Integer;
begin
  Result:=0;
  if not Fonts[CurFont].Exist then Exit;
  for i:=1 to Length(Text) do Result:=Result+Fonts[CurFont].FontWidth[Ord(Text[i])]+1;
end;

procedure gleWrite(const Text: string);
begin
  if not Fonts[CurFont].Exist then Exit;
  glPushAttrib(GL_ENABLE_BIT or GL_TEXTURE_BIT or GL_CURRENT_BIT or GL_COLOR_BUFFER_BIT);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, Fonts[CurFont].FontTex);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
  glPushMatrix;
    glListBase(Fonts[CurFont].FontBase);
    glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));
  glPopMatrix;
  glPopAttrib;
end;

procedure gleWrite(X, Y: Double; const Text: string);
begin
  if not Fonts[CurFont].Exist then Exit;
  glPushAttrib(GL_ENABLE_BIT or GL_TEXTURE_BIT or GL_CURRENT_BIT or GL_COLOR_BUFFER_BIT);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, Fonts[CurFont].FontTex);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
  glPushMatrix;
    glTranslated(X, Y, 0);
    glListBase(Fonts[CurFont].FontBase);
    glCallLists(Length(Text), GL_UNSIGNED_BYTE, PChar(Text));
  glPopMatrix;
  glPopAttrib;
end;

{procedure glCreateFont(Font: HFONT; DC: HDC; BaZe: Cardinal; Deviation, Extrusion: GLFloat; PolygonZ: Boolean); overload;
begin
glCreateFontO(Font, DC, BaZe, Deviation, Extrusion, PolygonZ);
end;

procedure glCreateFont(Font: HFONT; DC: HDC; BaZe: Cardinal); overload;
begin
glCreateFontB(Font, DC, BaZe);
end;

procedure glCreateFontO(Font: HFONT; DC: HDC; BaZe: Cardinal; Deviation, Extrusion: GLFloat; PolygonZ: Boolean);
begin
SelectObject(DC, Font);
if polygonZ then wglUseFontOutlines(DC, 0, 256, BaZe, Deviation, Extrusion, WGL_FONT_POLYGONS, nil) else wglUseFontOutlines(DC, 0, 256, BaZe, Deviation, Extrusion, WGL_FONT_LINES, nil);
glListBase(BaZe);
end;

procedure glCreateFontB(Font: HFONT; DC: HDC; BaZe: Cardinal);
begin
SelectObject(DC, Font);
wglUseFontBitmaps(DC, 0, 256, BaZe);
glListBase(BaZe);
end;

procedure glWrite(BaZe: Cardinal; PhraZe: String; TeXTurinG: Boolean);
begin
if not TeXTurinG then glDisable(GL_TEXTURE_2D);
glListBase(BaZe);
glCallLists(Length(PhraZe), GL_UNSIGNED_BYTE, PChar(PhraZe));
if not TeXTurinG then glEnable(GL_TEXTURE_2D);
end; }

initialization

  gleSetUpdateMatrixProc(gleDefaultUpdateMatrixProc);

finalization

  gleFreeFonts;

end.
