unit Textures;

interface

uses Windows, AvL, dglOpenGL, J2000Dec, VSELog, VSEPakMan;

type
  TTexFormat=(tfJ2K, tfTGA, tfBMP, tfFNT);
  TFNTHdr=packed record
    ID: Cardinal;
    Width, Height: Word;
    Bits, MipLevels: Byte;
    FontWidth: packed array[0..255] of Byte;
  end;

function LoadTexture(Data: TStream; Format: TTexFormat;
  MipMap: Boolean; MinFilter, MagFilter: GLenum): Cardinal;

function FmtByExt(Name: string): TTexFormat;

const
  FNTID=$4E465356;

var
  LastFNTHdr: TFNTHdr;

implementation

uses OpenGLExt;

type
  TRGB=packed record
    R, G, B: Byte;
  end;
  TRGBA=packed record
    R, G, B, A: Byte;
  end;
  PRGBArray = ^TRGBArray;
  TRGBArray = packed array[0..$2AAAAAA9] of TRGB;
  PRGBAArray = ^TRGBAArray;
  TRGBAArray = packed array[0..$1FFFFFFE] of TRGBA;

var
  J2kLib: hModule;

function LoadJ2K(Data: TStream; var P: Pointer; var Width, Height, Fmt: Word): Boolean;
var
  Img: PJ2kImage;
  CP: PJ2kCP;
  i, j, Depth: Integer;
  D: PByteArray;
  DRGB: PRGBArray;
  DRGBA: PRGBAArray;
begin
  Result:=false;
  if J2kLib=0 then
  begin
    J2kLib:=LoadLibrary(J2kDLL);
    if (J2kLib=0) or not J2kInit(J2kLib) then
    begin
      Log(llError, 'LoadJ2k: loading j2k library failed');
      Exit;
    end;
  end;
  if Data.Size<=0 then
  begin
    Log(llError, 'LoadJ2k: Data is void');
    Exit;
  end;
  Img:=nil;
  CP:=nil;
  try
    try
      GetMem(P, Data.Size);
      Data.Seek(0, soFromBeginning);
      Data.Read(P^, Data.Size);
      if j2k_decode(P, Data.Size, Img, CP)=0 then
      begin
        Log(llError, 'LoadJ2k: j2k_decode failed');
        Exit;
      end;
    finally;
      if P<>nil then FreeMem(P);
    end;
    Width:=Img.x1-Img.x0;
    Height:=Img.y1-Img.y0;
    case Img.NumComps of
      1: begin Depth:=1; Fmt:=GL_LUMINANCE; end;
      3: begin Depth:=3; Fmt:=GL_RGB; end;
      4: begin Depth:=4; Fmt:=GL_RGBA; end;
      else begin
        Log(llError, 'LoadJ2k: invalid NumComps');
        Exit;
      end;
    end;
    for i:=0 to Img.NumComps-1 do
      if (Img.Comps[i].Prec<>8) or (Img.Comps[i].Sgnd<>0) or (Img.Comps[i].dx<>1) or (Img.Comps[i].dy<>1) then
      begin
        Log(llError, 'LoadJ2k: invalid Comps format');
        Exit;
      end;
    GetMem(P, Width*Height*Depth);
    D:=P;
    DRGB:=P;
    DRGBA:=P;
    for i:=0 to Height-1 do
      for j:=0 to Width-1 do
        case Img.NumComps of
          1: begin
               D[i*Width+j]:=Img.Comps[0].Data[(Height-i-1)*Width+j];
             end;
          3: begin
               DRGB[i*Width+j].R:=Img.Comps[0].Data[(Height-i-1)*Width+j];
               DRGB[i*Width+j].G:=Img.Comps[1].Data[(Height-i-1)*Width+j];
               DRGB[i*Width+j].B:=Img.Comps[2].Data[(Height-i-1)*Width+j];
             end;
          4: begin
               DRGBA[i*Width+j].R:=Img.Comps[0].Data[(Height-i-1)*Width+j];
               DRGBA[i*Width+j].G:=Img.Comps[1].Data[(Height-i-1)*Width+j];
               DRGBA[i*Width+j].B:=Img.Comps[2].Data[(Height-i-1)*Width+j];
               DRGBA[i*Width+j].A:=Img.Comps[3].Data[(Height-i-1)*Width+j];
             end;
        end;
    Result:=true;
  finally
    j2k_release(Img, CP);
  end;
end;

function LoadTGA(Data: TStream; var P: Pointer; var Width, Height, Fmt: Word): Boolean;
var
  TGAHeader: packed record
    FileType: Byte;
    ColorMapType: Byte;
    ImageType: Byte;
    ColorMapSpec: array[0..4] of Byte;
    OrigX: array [0..1] of Byte;
    OrigY: array [0..1] of Byte;
    Width: array [0..1] of Byte;
    Height: array [0..1] of Byte;
    BPP: Byte;
    ImageInfo: Byte;
  end;
  CompImage: Pointer;
  ColorDepth: Cardinal;
  ImageSize: Integer;
  BufferIndex, CurrentByte, CurrentPixel: Cardinal;
  i: Cardinal;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;

  procedure CopySwapPixel(const Source, Destination : Pointer);
  asm
    push ebx
    mov bl,[eax+0]
    mov bh,[eax+1]
    mov [edx+2],bl
    mov [edx+1],bh
    mov bl,[eax+2]
    mov bh,[eax+3]
    mov [edx+0],bl
    mov [edx+3],bh
    pop ebx
  end;

begin
  Result:=false;
  if Data.Size<=0 then
  begin
    Log(llError, 'LoadTGA: Data is void');
  end;
  Data.ReadBuffer(TGAHeader, SizeOf(TGAHeader));  // FileHeader
  // Only support 24, 32 bit images
  if (TGAHeader.ImageType<>2) and (TGAHeader.ImageType<>10) then
  begin
    Log(llError, 'LoadTGA: invalid ImageType');
    Exit;
  end;
  if TGAHeader.ColorMapType<>0 then
  begin
    Log(llError, 'LoadTGA: invalid ColorMapType');
    Exit;
  end;
  // Get the width, height, and color depth
  Width:=TGAHeader.Width[0] or (TGAHeader.Width[1] shl 8);
  Height:=TGAHeader.Height[0] or (TGAHeader.Height[1] shl 8);
  ColorDepth:=TGAHeader.BPP;
  ImageSize:=Width*Height*(ColorDepth div 8);
  if ColorDepth<24 then
  begin
    Log(llError, 'LoadTGA: invalid ColorDepth');
    Exit;
  end;
  GetMem(P, ImageSize);
  if TGAHeader.ImageType=2 then   // Standard 24, 32 bit TGA file
  begin
    Data.ReadBuffer(P^, ImageSize);
    // TGAs are stored BGR and not RGB, so swap the R and B bytes.
    // 32 bit TGA files have alpha channel and gets loaded differently
    if TGAHeader.BPP=24 then
    begin
      for i:=0 to Width*Height-1 do
      begin
        Front:=Pointer(Cardinal(P)+i*3);
        Back:=Pointer(Cardinal(P)+i*3+2);
        Temp:=Front^;
        Front^:=Back^;
        Back^:=Temp;
      end;
      Fmt:=GL_RGB;
    end
    else begin
      for i:=0 to Width*Height-1 do
      begin
        Front:=Pointer(Cardinal(P)+i*4);
        Back:=Pointer(Cardinal(P)+i*4+2);
        Temp:=Front^;
        Front^:=Back^;
        Back^:=Temp;
      end;
      Fmt:=GL_RGBA;
    end;
  end;
  // Compressed 24, 32 bit TGA files
  if TGAHeader.ImageType = 10 then
  begin
    ColorDepth:=ColorDepth div 8;
    CurrentByte:=0;
    CurrentPixel:=0;
    BufferIndex:=0;
    ImageSize:=Data.Size-SizeOf(TGAHeader);
    GetMem(CompImage, ImageSize);
    Data.ReadBuffer(CompImage^, ImageSize);   // load compressed data into memory
    // Extract pixel information from compressed data
    repeat
      Front:=Pointer(Cardinal(CompImage)+BufferIndex);
      Inc(BufferIndex);
      if Front^<128 then
      begin
        for i:=0 to Front^ do
        begin
          CopySwapPixel(Pointer(Cardinal(CompImage)+BufferIndex+i*ColorDepth), Pointer(Cardinal(P)+CurrentByte));
          CurrentByte:=CurrentByte+ColorDepth;
          Inc(CurrentPixel);
        end;
        BufferIndex:=BufferIndex+(Front^+1)*ColorDepth;
      end
      else begin
        for i:=0 to Front^-128 do
        begin
          CopySwapPixel(Pointer(Cardinal(CompImage)+BufferIndex), Pointer(Cardinal(P)+CurrentByte));
          CurrentByte:=CurrentByte+ColorDepth;
          Inc(CurrentPixel);
        end;
        BufferIndex:=BufferIndex+ColorDepth;
      end;
    until CurrentPixel>=Width*Height;
    if ColorDepth=3
      then Fmt:=GL_RGB
      else Fmt:=GL_RGBA;
  end;
  Result:=true;
end;

function LoadBMP(Data: TStream; var P: Pointer; var Width, Height, Fmt: Word): Boolean;
var
  FileHeader: TBitmapFileHeader;
  InfoHeader: TBitmapInfoHeader;
  Palette: array of TRGBQuad;
  BitmapLength: LongWord;
  PaletteLength: LongWord;
  Src, Dst: PByteArray;
  i: Cardinal;
begin
  Result:=false;
  if Data.Size<=0 then
  begin
    Log(llError, 'LoadBMP: Data is void');
    Exit;
  end;
  Data.ReadBuffer(FileHeader, SizeOf(FileHeader));  // FileHeader
  if FileHeader.bfType<>$4D42 then
  begin
    Log(llError, 'LoadBMP: Data is not BMP');
    Exit;
  end;
  Data.ReadBuffer(InfoHeader, SizeOf(InfoHeader));  // InfoHeader
  PaletteLength:=InfoHeader.biClrUsed;
  if PaletteLength=0 then PaletteLength:=(FileHeader.bfOffBits-54) div 4;
  SetLength(Palette, PaletteLength);
  Data.ReadBuffer(Palette[0], PaletteLength*4);     // Load palette
  Width:=InfoHeader.biWidth;
  Height:=InfoHeader.biHeight;
  BitmapLength:=InfoHeader.biSizeImage;
  if BitmapLength=0 then BitmapLength:=Width*Height*InfoHeader.biBitCount div 8;
  //Unsupported formats: multiplanes, compressed
  if (InfoHeader.biPlanes<>1) or (InfoHeader.biCompression<>0) then
  begin
    Log(llError, 'LoadBMP: Unsupported format');
    Exit;
  end;
  GetMem(Src, BitmapLength);
  try
    GetMem(Dst, Width*Height*3);
    Data.ReadBuffer(Src^, BitmapLength);            // Bitmap Data
    case InfoHeader.biBitCount of
       4: for i:=0 to BitmapLength-1 do         //16-colors
          begin
            Dst[6*i]:=Palette[(Src[i] and $F0) shr 4].rgbRed;
            Dst[6*i+1]:=Palette[(Src[i] and $F0) shr 4].rgbGreen;
            Dst[6*i+2]:=Palette[(Src[i] and $F0) shr 4].rgbBlue;
            Dst[6*i+3]:=Palette[Src[i] and $0F].rgbRed;
            Dst[6*i+4]:=Palette[Src[i] and $0F].rgbGreen;
            Dst[6*i+5]:=Palette[Src[i] and $0F].rgbBlue;
          end;
       8: for i:=0 to BitmapLength-1 do         //256-colors
          begin
            Dst[3*i]:=Palette[Src[i]].rgbRed;
            Dst[3*i+1]:=Palette[Src[i]].rgbGreen;
            Dst[3*i+2]:=Palette[Src[i]].rgbBlue;
          end;
      24: for i:=0 to BitmapLength div 3 do       //24-bit BGR
          begin
            Dst[3*i]:=Src[3*i+2];
            Dst[3*i+1]:=Src[3*i+1];
            Dst[3*i+2]:=Src[3*i];
          end;
      else Exit;
    end;
    P:=Dst;
    Fmt:=GL_RGB;
    Result:=true;
  finally
    if Src<>nil then FreeMem(Src);
  end;
end;

function LoadFNT(Data: TStream; var P: Pointer; var Width, Height, Fmt: Word): Boolean;
var
  Dst, Src: PByteArray;
  i, Sz: Cardinal;
begin
  Result:=false;
  if Data.Size<SizeOf(TFNTHdr) then
  begin
    Log(llError, 'LoadFNT: Data is too small');
    Exit;
  end;
  Data.Read(LastFNTHdr, SizeOf(TFNTHdr));
  if LastFNTHdr.ID<>FNTID then
  begin
    Log(llError, 'LoadFNT: Data is not FNT');
    Exit;
  end;
  Width:=LastFNTHdr.Width;
  Height:=LastFNTHdr.Height;
  Sz:=0;
  for i:=0 to LastFNTHdr.MipLevels-1 do
  begin
    Sz:=Sz+Width*Height*LastFNTHdr.Bits div 8;
    Width:=Width div 2;
    Height:=Height div 2;
  end;
  Width:=LastFNTHdr.Width;
  Height:=LastFNTHdr.Height;
  GetMem(Src, Sz);
  try
    GetMem(Dst, Sz*(8 div LastFNTHdr.Bits));
    Data.Read(Src^, Sz);
    for i:=0 to Sz-1 do
      case LastFNTHdr.Bits of
        1: begin
             Dst[8*i]:=((Src[i] and $80) shr 7)*255;
             Dst[8*i+1]:=((Src[i] and $40) shr 6)*255;
             Dst[8*i+2]:=((Src[i] and $20) shr 5)*255;
             Dst[8*i+3]:=((Src[i] and $10) shr 4)*255;
             Dst[8*i+4]:=((Src[i] and $08) shr 3)*255;
             Dst[8*i+5]:=((Src[i] and $04) shr 2)*255;
             Dst[8*i+6]:=((Src[i] and $02) shr 1)*255;
             Dst[8*i+7]:=(Src[i] and $01)*255;
           end;
        2: begin
             Dst[4*i]:=((Src[i] and $C0) shr 6)*85;
             Dst[4*i+1]:=((Src[i] and $30) shr 4)*85;
             Dst[4*i+2]:=((Src[i] and $0C) shr 2)*85;
             Dst[4*i+3]:=(Src[i] and $03)*85;
           end;
        4: begin
             Dst[2*i]:=((Src[i] and $F0) shr 4)*17;
             Dst[2*i+1]:=(Src[i] and $0F)*17;
           end;
        8: Dst[i]:=Src[i];
        else Exit;
      end;
    P:=Dst;
    Fmt:=GL_ALPHA;
    Result:=true;
  finally
    if Src<>nil then FreeMem(Src);
  end;
end;

function LoadTexture(Data: TStream; Format: TTexFormat;
  MipMap: Boolean; MinFilter, MagFilter: GLenum): Cardinal;
var
  Width, Height, Fmt: Word;
  P, P2: Pointer;
  Res: Boolean;
  GLErr, i: Cardinal;
begin
  Result:=0;
  if not Assigned(Data) then
  begin
    Log(llError, 'LoadTexture: datastream is undefined');
    Exit;
  end;
  P:=nil;
  case Format of
    tfJ2K: Res:=LoadJ2K(Data, P, Width, Height, Fmt);
    tfTGA: Res:=LoadTGA(Data, P, Width, Height, Fmt);
    tfBMP: Res:=LoadBMP(Data, P, Width, Height, Fmt);
    tfFNT: Res:=LoadFNT(Data, P, Width, Height, Fmt);
    else begin
      Log(llError, 'LoadTexture: Unsupported format');
      Exit;
    end;
  end;
  if not Res then
  begin
    Log(llError, 'LoadTexture: Load<TYPE> failed');
    Exit;
  end;
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, MagFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, MinFilter);
  if MipMap
    then gluBuild2DMipmaps(GL_TEXTURE_2D, Fmt, Width, Height, Fmt, GL_UNSIGNED_BYTE, P)
    else glTexImage2D(GL_TEXTURE_2D, 0, Fmt, Width, Height, 0, Fmt, GL_UNSIGNED_BYTE, P); 
  if Format=tfFNT then
  begin
    P2:=P;
    i:=0;
    while true do
    begin
      glTexImage2D(GL_TEXTURE_2D, i, Fmt, Width, Height, 0, Fmt, GL_UNSIGNED_BYTE, P2);
      if i=LastFNTHdr.MipLevels-1 then Break;
      Cardinal(P2):=Cardinal(P2)+Width*Height;
      Width:=Width div 2;
      Height:=Height div 2;
      Inc(i);
    end;
  end;
  if P<>nil then FreeMem(P);
  GLErr:=glGetError;
  if GLErr<>GL_NO_ERROR then
  begin
    Result:=0;
    Log(llError, 'LoadTexture: OpenGL: '+gleError(GLErr));
  end;
end;

function FmtByExt(Name: string): TTexFormat;
begin
  Name:=LowerCase(Copy(Name, LastDelimiter('.', Name)+1, MaxInt));
  if Name='j2k' then Result:=tfJ2K;
  if Name='tga' then Result:=tfTGA;
  if Name='bmp' then Result:=tfBMP;
  if Name='fnt' then Result:=tfFNT;
end;

initialization

finalization
  if J2kLib<>0 then FreeLibrary(J2kLib);

end.
