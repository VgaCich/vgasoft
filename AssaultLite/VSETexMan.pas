unit VSETexMan;

interface

uses
  Windows, AvL, avlUtils, avlMath, OpenGL, oglExtensions, VSEOpenGLExt, SynTex, VSEMemPak;

type
  TTexture=record //internally used
    ID: Cardinal;
    Name: string;
  end;
  PFont=^TFont; //internally used
  TFont=record //internally used
    Tex,  List: Cardinal;
    Width: array [0..255] of ShortInt;
    Height: ShortInt;
    Size: Integer;
    Bold: Boolean;
    Name: string;
  end;
  TRTTMethod=(rttCopy, rttFBO); //Render-to-Texture method - CopyTexture (slow), FrameBuffer Object
  TRTTInfo=record //internally used
    Method: TRTTMethod;
    FBO, RBODepth, Color, Depth: Cardinal;
    RTWidth, RTHeight: Integer;
    Exist: Boolean;
  end;
  TTexMan=class
  private
    FCount, FMaxChannel: Integer;
    FTextures: array of TTexture;
    FFonts: array of PFont;
    FRTTs: array of TRTTInfo;
    FTexCache: string;
    FRTTMethod: TRTTMethod;
    {$IFDEF VSE_LOG}FNoLogLostTex: Boolean;{$ENDIF}
    procedure CreateFontTex(Font: Cardinal);
  public
    constructor Create; //internally used
    destructor Destroy; override; //internally used
    function  LoadCache: Boolean; //Load texture cache
    procedure ClearCache; //Clear texture cache
    procedure Store(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string); //SynTex interacting
    function  Load(Sender: TObject; var Reg: TSynTexRegister; TexSize: Integer; const Name: string): Boolean; //SynTex interacting
    function  AddTexture(Name: string; Data: Pointer; Width, Height: Integer; Comps, Format: GLenum; Clamp, MipMap: Boolean): Cardinal; //Add texture from memory
    function  GetTex(Name: string): Cardinal; //Get texture ID by texture name
    procedure Bind(ID: Cardinal; Channel: Integer = 0); //Set current texture in specified texture channel
    procedure Unbind(Channel: Integer = 0); //Remove texture from specified texture channel
    {Render-To-Texture (RTT)}
    function  InitRTT(Width, Height: Integer): Cardinal; //Init Render-To-Texture target with specified dimencions; returns RTT target ID
    procedure FreeRTT(RTT: Cardinal); //Remove RTT target
    function  RTTBegin(RTT: Cardinal; TexColor: Cardinal; TexDepth: Cardinal = 0): Boolean; //Start render to RTT target; TexColor, TexDepth - target textures; returns true if successfully started
    procedure RTTEnd(RTT: Cardinal); //End render to RTT target
    {Font engine}
    function  FontCreate(Name: string; Size: Integer; Bold: Boolean=false): Cardinal; //Create font; Name - font name, Size - font size, Bold - normal/bold font; returns font ID
    procedure RebuildFonts; //Rebuild font textures for current screen resolution
    procedure TextOut(Font: Cardinal; X, Y: Single; const Text: string); //Draw text; Font - font ID, X, Y - coordinates of left upper corner of text, Text - text for draw
    function  TextLen(Font: Cardinal; const Text: string): Integer; //Length of space, needed for drawing text
    function  TextHeight(Font: Cardinal): Integer; //Height of space, needed for drawing text
    {properties}
    property RTTMethod: TRTTMethod read FRTTMethod write FRTTMethod; //Method, used for RTT; default: autodetect
  end;

var
  UseCache: Boolean; //Enable texture cache
  CacheDir: string; //Texture cache path
  TexMan: TTexMan; //Global variable for accessing to Texture Manager

implementation

uses
  VSECore{$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  TexCapDelta=16;

constructor TTexMan.Create;
begin
  inherited Create;
  {$IFDEF VSE_LOG}Log(llInfo, 'TexMan: Create');{$ENDIF}
  FTexCache:=CacheDir+'Tex\';
  if UseCache then ForceDirectories(FTexCache);
  FCount:=0;
  SetLength(FTextures, TexCapDelta);
  FMaxChannel:=Max(glMaxTextureUnits, glMaxTextureImageUnits)-1;
  if GL_EXT_framebuffer_object
    then FRTTMethod:=rttFBO
    else FRTTMethod:=rttCopy;
end;

destructor TTexMan.Destroy;
var
  i: Integer;
begin
  {$IFDEF VSE_LOG}LogF(llInfo, 'TexMan: Destroy (%d textures, %d fonts)', [FCount, Length(FFonts)]);{$ENDIF}
  for i:=0 to High(FFonts) do
  begin
    glDeleteLists(FFonts[i]^.List, 256);
    Dispose(FFonts[i]);
  end;
  Finalize(FFonts);
  for i:=0 to High(FRTTs) do FreeRTT(i);
  Finalize(FRTTs);
  for i:=0 to FCount-1 do
    glDeleteTextures(1, @FTextures[i].ID);
  Finalize(FTextures);
  inherited Destroy;
end;

function TTexMan.LoadCache: Boolean;
var
  Lst: TStringList;
  i: Integer;
  TexFile: TMemoryStream;
  Size: Integer;
begin
  Result:=false;
  if UseCache and FileExists(FTexCache+'Tex.lst') then
  begin
    {$IFDEF VSE_LOG}Log(llInfo, 'TexMan: Found texture cache; loading');{$ENDIF}
    Lst:=TStringList.Create;
    try
      Lst.LoadFromFile(FTexCache+'Tex.lst');
      for i:=0 to Lst.Count-1 do
      begin
        TexFile:=TMemoryStream.Create;
        try
          TexFile.LoadFromFile(FTexCache+Lst[i]);
          TexFile.Read(Size, 4);
          Store(nil, TSynTexRegister(IncPtr(TexFile.Memory, 4)), Size, Lst[i]);
        finally
          FAN(TexFile);
        end;
      end;
      Result:=true;
    finally
      FAN(Lst);
    end;
  end;
end;

procedure TTexMan.ClearCache;
begin
  DeleteDir(FTexCache);
end;

function TTexMan.AddTexture(Name: string; Data: Pointer; Width, Height: Integer; Comps, Format: GLenum; Clamp, MipMap: Boolean): Cardinal;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'TexMan: Adding texture '+Name);{$ENDIF}
  Name:=UpperCase(Name);
  {$IFDEF VSE_LOG}FNoLogLostTex:=true;{$ENDIF}
  Result:=GetTex(Name);
  {$IFDEF VSE_LOG}FNoLogLostTex:=false;{$ENDIF}
  if Result=0 then
  begin
    if FCount=High(FTextures) then SetLength(FTextures, Length(FTextures)+TexCapDelta);
    FTextures[FCount].Name:=Name;
    glGenTextures(1, @Result);
    FTextures[FCount].ID:=Result;
    Inc(FCount);
  end;
  glBindTexture(GL_TEXTURE_2D, Result);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  if Clamp then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  end;
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  if MipMap then
  begin
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    gluBuild2DMipmaps(GL_TEXTURE_2D, Comps, Width, Height, Format, GL_UNSIGNED_BYTE, Data)
  end
  else begin
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, Comps, Width, Height, 0, Format, GL_UNSIGNED_BYTE, Data);
  end;
end;

procedure TTexMan.Store(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string);
var
  TexFile: TMemoryStream;
  LstFile: TFileStream;
begin
  if UseCache and Assigned(Sender) then
  begin
    {$IFDEF VSE_LOG}Log(llInfo, 'TexMan: Caching texture '+Name);{$ENDIF}
    TexFile:=TMemoryStream.Create;
    try
      TexFile.Write(TexSize, 4);
      TexFile.Write(Reg[0], TexSize*TexSize*SizeOf(TRGBA));
      TexFile.SaveToFile(FTexCache+Name);
      if FileExists(FTexCache+'Tex.lst')
        then LstFile:=TFileStream.Create(FTexCache+'Tex.lst', fmOpenWrite)
        else LstFile:=TFileStream.Create(FTexCache+'Tex.lst', fmCreate);
      LstFile.Seek(0, soFromEnd);
      LstFile.Write((Name+#13#10)[1], Length(Name)+2);
    finally
      FAN(TexFile);
      FAN(LstFile);
    end;
  end;
  AddTexture(Name, @Reg[0], TexSize, TexSize, GL_RGBA8, GL_RGBA, false, true);
end;

function TTexMan.Load(Sender: TObject; var Reg: TSynTexRegister; TexSize: Integer; const Name: string): Boolean;
var
  ID: Cardinal;
  W, H: Integer;
begin
  Result:=false;
  ID:=GetTex(Name);
  if ID=0 then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'TexMan: cannot load texture '+Name+' for SynTex: texture not exists');{$ENDIF}
    Exit;
  end;
  glBindTexture(GL_TEXTURE_2D, ID);
  glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, PGLInt(@W));
  glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, PGLInt(@H));
  glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPONENTS, PGLInt(@ID));
  if (W<>TexSize) or (H<>TexSize) or (ID<>GL_RGBA8) then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'TexMan: cannot load texture '+Name+' for SynTex: incorrect texture format');{$ENDIF}
    Exit;
  end;
  glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Reg[0]);
  Result:=true;
end;

function TTexMan.GetTex(Name: string): Cardinal;
var
  i: Integer;
begin
  Result:=0;
  Name:=UpperCase(Name);
  for i:=0 to FCount-1 do
    if FTextures[i].Name=Name then
    begin
      Result:=FTextures[i].ID;
      Exit;
    end;
  {$IFDEF VSE_LOG}if not FNoLogLostTex
    then Log(llError, 'TexMan.GetTex('+Name+'): cannot find texture');{$ENDIF}
end;

procedure TTexMan.Bind(ID: Cardinal; Channel: Integer);
begin
  if not (Channel in [0..FMaxChannel]) then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'TexMan: cannot bind texture %d to channel %d', [ID, Channel]);{$ENDIF}
    Exit;
  end;
  glActiveTextureARB(GL_TEXTURE0_ARB+Channel);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, ID);
end;

procedure TTexMan.Unbind(Channel: Integer);
begin
  if not (Channel in [0..FMaxChannel]) then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'TexMan: cannot unbind texture from channel %d', [Channel]);{$ENDIF}
    Exit;
  end;
  glActiveTextureARB(GL_TEXTURE0_ARB+Channel);
  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_TEXTURE_2D);
end;

function TTexMan.InitRTT(Width, Height: Integer): Cardinal;
var
  i: Cardinal;
begin
  Result:=$FFFFFFFF;
  if Length(FRTTs)>0 then
    for i:=0 to High(FRTTs) do
      if not FRTTs[i].Exist then
      begin
        Result:=i;
        Break;
      end;
  if Result=$FFFFFFFF then
  begin
    Result:=Length(FRTTs);
    SetLength(FRTTs, Result+1);
  end;
  ZeroMemory(@FRTTs[Result], SizeOf(TRTTInfo));
  with FRTTs[Result] do
  begin
    Method:=FRTTMethod;
    RTWidth:=Width;
    RTHeight:=Height;
    Exist:=true;
    if Method=rttFBO then
    begin
      glGenFramebuffersEXT(1, @FBO);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FBO);
      glGenRenderbuffersEXT(1, @RBODepth);
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, RBODepth);
      glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT24_ARB, Width, Height);
	    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, RBODepth);
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
    end;
  end;
end;

procedure TTexMan.FreeRTT(RTT: Cardinal);
begin
  if (RTT>High(FRTTs)) or not FRTTs[RTT].Exist then Exit;
  with FRTTs[RTT] do
  begin
    if Method=rttFBO then
    begin
      if FBO<>0 then glDeleteRenderbuffersEXT(1, @FBO);
      if RBODepth<>0 then glDeleteRenderbuffersEXT(1, @RBODepth);
    end;
    Exist:=false;
  end;
end;

function TTexMan.RTTBegin(RTT: Cardinal; TexColor, TexDepth: Cardinal): Boolean;
begin
  Result:=false;
  if (RTT>High(FRTTs)) or (TexColor=0) then Exit;
  with FRTTs[RTT] do
  begin
    glViewport(0, 0, RTWidth, RTHeight);
    Color:=TexColor;
    Depth:=TexDepth;
    if Method=rttFBO then
    begin
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FBO);
      glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, TexColor, 0);
      if TexDepth<>0 then
      begin
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, 0);
        glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, TexDepth, 0);
      end
      else begin
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, RBODepth);
      end;
      if glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT)<>GL_FRAMEBUFFER_COMPLETE_EXT then
      begin
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
        Exit;
      end;
    end;
    Result:=true;
  end;
end;

procedure TTexMan.RTTEnd(RTT: Cardinal);
var
  FMT: Cardinal;
begin
  if RTT>High(FRTTs) then Exit;
  with FRTTs[RTT] do
  begin
    case Method of
      rttCopy:
        begin
          Bind(Color);
          glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPONENTS, PGLInt(@Fmt));
          glCopyTexImage2D(GL_TEXTURE_2D, 0, Fmt, 0, 0, RTWidth, RTHeight, 0);
          if Depth<>0 then
          begin
            Bind(Depth);
            glGetTexLevelParameter(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPONENTS, PGLInt(@Fmt));
            glCopyTexImage2D(GL_TEXTURE_2D, 0, Fmt, 0, 0, RTWidth, RTHeight, 0);
          end;
          Unbind;
        end;
      rttFBO:
        begin
          glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, 0, 0);
          if Depth<>0 then glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D, 0, 0);
          glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
        end;
    end;
  end;
  glViewport(0, 0, Core.ResolutionX, Core.ResolutionY);
end;

procedure TTexMan.CreateFontTex(Font: Cardinal);
const
  Weight: array[Boolean] of Integer=(400, 700);
var
  i: Integer;
  FNT: HFONT;
  MDC: HDC;
  BMP: HBITMAP;
  BI: BITMAPINFO;
  Pix: PByteArray;
  Data: PByteArray;
  CS: TSize;
  k, s, t: Single;
  CharSize, FontTexSize: Integer;
begin
  with FFonts[Font]^ do
  try
    k:=Core.ResolutionY/600;
    FNT:=CreateFont(-MulDiv(Size, Ceil(k*GetDeviceCaps(Core.DC, LOGPIXELSY)), 72), 0, 0,
      0, Weight[Bold], 0, 0, 0, RUSSIAN_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
      ANTIALIASED_QUALITY, 0, PChar(Name));
    FontTexSize:=128;
    CharSize:=MulDiv(Size, Ceil(k*GetDeviceCaps(Core.DC, LOGPIXELSY)), 72)*16+16;
    while (FontTexSize<CharSize) and (FontTexSize<=glMaxTextureSize) do FontTexSize:=FontTexSize*2;
    ZeroMemory(@BI, SizeOf(BI));
    with BI.bmiHeader do
    begin
      biSize:=SizeOf(BITMAPINFOHEADER);
      biWidth:=FontTexSize;
      biHeight:=FontTexSize;
      biPlanes:=1;
      biBitCount:=24;
      biSizeImage:=biWidth*biHeight*biBitCount div 8;
    end;
    MDC:=CreateCompatibleDC(Core.DC);
    BMP:=CreateDIBSection(MDC, BI, DIB_RGB_COLORS, Pointer(Pix), 0, 0);
    ZeroMemory(Pix, FontTexSize*FontTexSize*3);
    SelectObject(MDC, BMP);
    SelectObject(MDC, FNT);
    SetBkMode(MDC, TRANSPARENT);
    SetTextColor(MDC, $FFFFFF);
    for i:=0 to 255 do
      Windows.TextOut(MDC, i mod 16 * (FontTexSize div 16), i div 16 * (FontTexSize div 16), @Char(i), 1);
    GetMem(Data, FontTexSize*FontTexSize);
    for i:=0 to FontTexSize*FontTexSize-1 do
      Data[i]:=Pix[i*3];
    glBindTexture(GL_TEXTURE_2D, Tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, FontTexSize, FontTexSize, 0, GL_ALPHA, GL_UNSIGNED_BYTE, Data);
    Height:=0;
    for i:=0 to 255 do
    begin
      glNewList(List+Cardinal(i), GL_COMPILE);
      s:=(i mod 16)/16;
      t:=(i div 16)/16;
      GetTextExtentPoint32(MDC, @Char(i), 1, CS);
      Width[i]:=Ceil(CS.cx/k);
      Height:=Max(Ceil(CS.cy/k), Height);
      glBegin(GL_QUADS);
        glTexCoord2f(s, 1-t);
        glVertex2f(0, 0);
        glTexCoord2f(s+CS.cx/FontTexSize, 1-t);
        glVertex2f(CS.cx/k, 0);
        glTexCoord2f(s+CS.cx/FontTexSize, 1-t-CS.cy/FontTexSize);
        glVertex2f(CS.cx/k, CS.cy/k);
        glTexCoord2f(s, 1-t-CS.cy/FontTexSize);
        glVertex2f(0, CS.cy/k);
      glEnd;
      glTranslatef(Width[i], 0, 0);
      glEndList;
    end;
  finally
    FreeMem(Data);
    DeleteObject(FNT);
    DeleteObject(BMP);
    DeleteDC(MDC);
  end;
end;

function TTexMan.FontCreate(Name: string; Size: Integer; Bold: Boolean): Cardinal;
const
  BoolStr: array[Boolean] of Char = ('N', 'B');
var
  i: Integer;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'TexMan: Creating font '+Name+': '+IntToStr(Size)+BoolStr[Bold]);{$ENDIF}
  Name:=UpperCase(Name);
  for i:=0 to High(FFonts) do
    if (FFonts[i]^.Name=Name) and (FFonts[i]^.Size=Size) and (FFonts[i]^.Bold=Bold) then
    begin
      Result:=i;
      Exit;
    end;
  Result:=Length(FFonts);
  SetLength(FFonts, Result+1);
  New(FFonts[Result]);
  FFonts[Result]^.Size:=Size;
  FFonts[Result]^.Bold:=Bold;
  FFonts[Result]^.Name:=Name;
  FFonts[Result]^.Tex:=AddTexture('__FONT_'+Name+IntToStr(Size)+BoolStr[Bold], nil, 1, 1, GL_ALPHA8, GL_ALPHA, true, false);
  FFonts[Result]^.List:=glGenLists(256);
  CreateFontTex(Result);
end;

procedure TTexMan.RebuildFonts;
var
  i: Integer;
begin
  for i:=0 to High(FFonts) do CreateFontTex(i);
end;

procedure TTexMan.TextOut(Font: Cardinal; X, Y: Single; const Text: string);
var
  i: Integer;
begin
  if (Font>=Cardinal(Length(FFonts))) or (FFonts[Font]=nil) then Exit;
  glPushAttrib(GL_ENABLE_BIT or GL_COLOR_BUFFER_BIT or GL_TEXTURE);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);
  glEnable(GL_ALPHA_TEST);
  glEnable(GL_BLEND);
  glAlphaFunc(GL_GEQUAL, 0.1);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glListBase(FFonts[Font]^.List);
  Bind(FFonts[Font]^.Tex, 0);
  glPushMatrix;
    glTranslatef(X, Y, 0);
    for i:=1 to Length(Text) do
      glCallLists(1, GL_UNSIGNED_BYTE, @Text[i]);
  glPopMatrix;
  glPopAttrib;
end;

function TTexMan.TextLen(Font: Cardinal; const Text: string): Integer;
var
  i: Integer;
begin
  Result:=0;
  if (Font>=Cardinal(Length(FFonts))) or (FFonts[Font]=nil) then Exit;
  for i:=1 to Length(Text) do
    Result:=Result+FFonts[Font]^.Width[Byte(Text[i])];
end;

function TTexMan.TextHeight(Font: Cardinal): Integer;
begin
  Result:=0;
  if (Font>=Cardinal(Length(FFonts))) or (FFonts[Font]=nil) then Exit;
  Result:=FFonts[Font]^.Height;
end;

end.
