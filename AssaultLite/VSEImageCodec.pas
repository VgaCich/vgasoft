unit VSEImageCodec;

interface

uses
  Windows, AvL, avlUtils, GDIPAPI, avlIStreamAdapter{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TImageFormat = (ifBMP, ifJPEG, ifGIF, ifPNG, ifTIFF); // Image formats
  TPixelFormat = (pfGS8bit, pfBGR24bit, pfRGBA32bit, pfBGRA32bit); // Pixel data format: 8-bit grayscale, 24-bit BGR, 32-bit RGBA and BGRA
  TSaveFunction = function(Bitmap, UserData: Pointer; EncCLSID: PGUID; EncParams: PEncoderParameters): TStatus; // used internally
  TImageCodec = class
  private
    FWidth, FHeight: Cardinal;
    FStride: Integer;
    FPixelFormat: TPixelFormat;
    FPixels: PByteArray;
    function LoadImage(Bitmap: Pointer; Status: TStatus): Boolean;
    function SaveImage(SaveFunction: TSaveFunction; UserData: Pointer; ImageFormat: TImageFormat; Quality: Cardinal): Boolean;
    procedure SetPixels(Value: PByteArray);
  public
    constructor Create; overload; // Create codec with no image
    constructor Create(Width, Height: Cardinal; PixelFormat: TPixelFormat; Stride: Integer = 0); overload; // Create codec with void image
    destructor Destroy; override;
    function Load(const FileName: string): Boolean; overload; // Load image from file; returns true if successful
    function Load(Stream: TStream): Boolean; overload; // Load image from stream; returns true if successful
    function Load(Mem: Pointer; Size: Cardinal): Boolean; overload; // Load image from memory; returns true if successful
    function LoadRaw(Stream: TStream): Boolean; // Load raw image data; returns true if successful
    function Save(const FileName: string; ImageFormat: TImageFormat; Quality: Cardinal = 0): Boolean; overload; // Save image to file in specified format; returns true if successful
    function Save(Stream: TStream; ImageFormat: TImageFormat; Quality: Cardinal = 0): Boolean; overload; // Save image to stream in specified format; returns true if successful
    procedure SaveRaw(Stream: TStream); // Save raw image data; returns true if successful
    procedure Pack; // Remove rows alignment
    property Width: Cardinal read FWidth; // Image width
    property Height: Cardinal read FHeight; // Image height
    property Stride: Integer read FStride; // Image stride - size of row in bytes, including alignment
    property PixelFormat: TPixelFormat read FPixelFormat; // Format of pixel data
    property Pixels: PByteArray read FPixels write SetPixels; // Pointer to raw pixels array
  end;

const
  ImageFormatMime: array[TImageFormat] of string = ('image/bmp', 'image/jpeg', 'image/gif', 'image/png', 'image/tiff');
  ImageFormatExtension: array[TImageFormat] of string = ('.bmp', '.jpg', '.gif', '.png', '.tif');

implementation

const
  BitDepths: array[TPixelFormat] of Integer = (8, 24, 32, 32);
  PixelFormats: array[TPixelFormat] of Integer = (PixelFormat8bppIndexed, PixelFormat24bppRGB, 0, PixelFormat32bppARGB);
  RawMagic: Cardinal = $57415249;

function GetEncoderCLSID(const Format: string): TGUID;
var
  i, Num, Size: Cardinal;
  ImageCodecsInfo: PImageCodecsInfo;
begin
  ZeroMemory(@Result, SizeOf(Result));
  GdipGetImageEncodersSize(Num, Size);
  if Size = 0 then Exit;
  GetMem(ImageCodecsInfo, Size);
  GdipGetImageEncoders(Num, Size, PImageCodecInfo(ImageCodecsInfo));
  for i := 0 to Num-1 do
    if ImageCodecsInfo[i].MimeType = Format then
    begin
      Result := ImageCodecsInfo[i].Clsid;
      Break;
    end;
  FreeMem(ImageCodecsInfo);
end;

function GSPalette: TColorPalette256;
var
  i: Integer;
begin
  with Result do
  begin
    Flags := PaletteFlagsGrayScale;
    Count := 256;
    for i := 0 to 255 do
      Entries[i] := i or (i shl 8) or (i shl 16) or $FF000000;
  end;
end;

function SaveToFileFunction(Bitmap, UserData: Pointer; EncCLSID: PGUID; EncParams: PEncoderParameters): TStatus;
begin
  Result := GdipSaveImageToFile(Bitmap, UserData, EncCLSID, EncParams);
end;

function SaveToStreamFunction(Bitmap, UserData: Pointer; EncCLSID: PGUID; EncParams: PEncoderParameters): TStatus;
var
  StreamAdapter: IStream;
begin
  TIStreamAdapter.Create(TStream(UserData)).GetInterface(IStream, StreamAdapter);
  Result := GdipSaveImageToStream(Bitmap, StreamAdapter, EncCLSID, EncParams);
end;

{ TImageCodec }

constructor TImageCodec.Create(Width, Height: Cardinal; PixelFormat: TPixelFormat; Stride: Integer);
begin
  FWidth := Width;
  FHeight := Height;
  FPixelFormat := PixelFormat;
  if Stride = 0 then Stride := Width * BitDepths[PixelFormat] div 8;
  FStride := Stride;
  GetMem(FPixels, Stride * Height);
end;

constructor TImageCodec.Create;
begin

end;

destructor TImageCodec.Destroy;
begin
  if Assigned(FPixels) then FreeMem(FPixels);
  inherited;
end;

function TImageCodec.Load(const FileName: string): Boolean;
var
  Bitmap: Pointer;
  Status: TStatus;
begin
  Status := GdipCreateBitmapFromFile(PWideChar(WideString(FileName)), Bitmap);
  Result := LoadImage(Bitmap, Status);
  {$IFDEF VSE_LOG}if not Result then LogF(llError, 'ImageCodec.Load("%s"): failed', [FileName]);{$ENDIF}
end;

function TImageCodec.Load(Mem: Pointer; Size: Cardinal): Boolean;
var
  Bitmap: Pointer;
  Status: TStatus;
  Glob: HGLOBAL;
  Stream: IStream;
begin
  Result := false;
  Glob := CreateIStreamOnMemory(Mem, Size, Stream);
  try
    if (Glob <> 0) and Assigned(Stream) then
    begin
      Status := GdipCreateBitmapFromStream(Stream, Bitmap);
      Result := LoadImage(Bitmap, Status);
    end;
  finally
    if Glob <> 0 then GlobalFree(Glob);
  end;
  {$IFDEF VSE_LOG}if not Result then Log(llError, 'ImageCodec.Load(Buffer): failed');{$ENDIF}
end;

function TImageCodec.Load(Stream: TStream): Boolean;
var
  Bitmap: Pointer;
  Status: TStatus;
  StreamAdapter: IStream;
begin
  TIStreamAdapter.Create(Stream).GetInterface(IStream, StreamAdapter);
  Status := GdipCreateBitmapFromStream(StreamAdapter, Bitmap);
  Result := LoadImage(Bitmap, Status);
  {$IFDEF VSE_LOG}if not Result then Log(llError, 'ImageCodec.Load(Stream): failed');{$ENDIF}
end;

function TImageCodec.LoadImage(Bitmap: Pointer; Status: TStatus): Boolean;
var
  BitmapData: TBitmapData;
  i, PixFormat, PalSize: Integer;
  Palette: TColorPalette256;
  Rect: TGPRect;
begin
  Result := false;
  if Status <> Ok then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'ImageCodec.LoadImage: can''t create GDI+ bitmap (Status=%d)', [Integer(Status)]);{$ENDIF}
    Exit;
  end;
  try
    if GdipGetImageWidth(Bitmap, FWidth) <> Ok then Exit;
    if GdipGetImageHeight(Bitmap, FHeight) <> Ok then Exit;
    if GdipGetImagePixelFormat(Bitmap, PixFormat) <> Ok then Exit;
    case (PixFormat and $FF00) shr 8 of
      1, 4, 8: FPixelFormat := pfGS8bit;
      16, 24: FPixelFormat := pfBGR24bit;
      32: FPixelFormat := pfBGRA32bit;
      else Exit;
    end;
    if PixFormat and PixelFormatIndexed <> 0 then
    begin
      if GdipGetImagePaletteSize(Bitmap, PalSize) <> Ok then Exit;
      if GdipGetImagePalette(Bitmap, Palette, Min(PalSize, SizeOf(Palette))) <> Ok then Exit;
      with Palette do
        for i := 0 to Count - 1 do
          if Entries[i] <> Entries[i] or ((Entries[i] shl 8) and $00FFFF00) then
          begin
            FPixelFormat := pfBGR24bit;
            Break;
          end;
    end;
    with Rect do
    begin
      X := 0;
      Y := 0;
      Width := FWidth;
      Height := FHeight;
    end;
    ZeroMemory(@BitmapData, SizeOf(BitmapData));
    Status := GdipBitmapLockBits(Bitmap, Rect, ImageLockModeRead, PixelFormats[FPixelFormat], @BitmapData);
    if (Status = InvalidParameter) and (FPixelFormat = pfGS8bit) then
    begin
      {$IFDEF VSE_LOG}Log(llWarning, 'ImageCodec.LoadImage: can''t lock bitmap data as GS8, retrying locking as BGR24');{$ENDIF}
      FPixelFormat := pfBGR24bit;
      Status := GdipBitmapLockBits(Bitmap, Rect, ImageLockModeRead, PixelFormats[FPixelFormat], @BitmapData);
    end;
    if Status <> Ok then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'ImageCodec.LoadImage: can''t lock bitmap data (Status=%d)', [Integer(Status)]);{$ENDIF}
      Exit;
    end;
    try
      FStride := Abs(BitmapData.Stride);
      ReallocMem(FPixels, FStride * FHeight);
      Move(BitmapData.Scan0^, FPixels^, FStride * FHeight);
    finally
      GdipBitmapUnlockBits(Bitmap, @BitmapData);
    end;
    Result := true;
  finally
    GdipDisposeImage(Bitmap);
  end;
end;

function TImageCodec.LoadRaw(Stream: TStream): Boolean;
var
  Magic: Cardinal;
begin
  Result := false;
  if (Stream.Read(Magic, SizeOf(Magic)) < SizeOf(Magic)) or (Magic <> RawMagic) then Exit;
  Stream.Read(FWidth, SizeOf(FWidth));
  Stream.Read(FHeight, SizeOf(FHeight));
  Stream.Read(FStride, SizeOf(FStride));
  Stream.Read(FPixelFormat, SizeOf(FPixelFormat));
  if (Stream.Size - Stream.Position < FStride * FHeight) or
    not (FPixelFormat in [Low(TPixelFormat) .. High(TPixelFormat)]) then Exit;
  ReallocMem(FPixels, FStride * FHeight);
  Stream.Read(FPixels^, FStride * FHeight);
  Result := true;
end;

function TImageCodec.Save(const FileName: string; ImageFormat: TImageFormat; Quality: Cardinal): Boolean;
begin
  Result := SaveImage(SaveToFileFunction, PWideChar(WideString(FileName)), ImageFormat, Quality);
  {$IFDEF VSE_LOG}if not Result then LogF(llError, 'ImageCodec.Save("%s"): failed', [FileName]);{$ENDIF}
end;

function TImageCodec.Save(Stream: TStream; ImageFormat: TImageFormat; Quality: Cardinal): Boolean;
begin
  Result := SaveImage(SaveToStreamFunction, Pointer(Stream), ImageFormat, Quality);
  {$IFDEF VSE_LOG}if not Result then Log(llError, 'ImageCodec.Save(Stream): failed');{$ENDIF}
end;

function TImageCodec.SaveImage(SaveFunction: TSaveFunction; UserData: Pointer; ImageFormat: TImageFormat; Quality: Cardinal): Boolean;
var
  Status: TStatus;
  Bitmap: Pointer;
  EncCLSID: TGUID;
  EncParams: TEncoderParameters;
  PEncParams: PEncoderParameters;
begin
  Result := false;
  if not Assigned(FPixels) then Exit;
  Status := GdipCreateBitmapFromScan0(FWidth, FHeight, FStride, PixelFormats[FPixelFormat], FPixels, Bitmap);
  if Status <> Ok then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'ImageCodec.SaveImage: can''t create GDI+ bitmap (Status=%d)', [Integer(Status)]);{$ENDIF}
    Exit;
  end;
  try
    if FPixelFormat = pfGS8bit then GdipSetImagePalette(Bitmap, GSPalette);
    GdipImageRotateFlip(Bitmap, RotateNoneFlipY);
    EncCLSID := GetEncoderCLSID(ImageFormatMime[ImageFormat]);
    if EncCLSID.D1 = 0 then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'ImageCodec.SaveImage: GDI+ encoder for %s is not available', [ImageFormatMime[ImageFormat]]);{$ENDIF}
      Exit;
    end;
    if Quality > 0 then
    begin
      EncParams.Count := 1;
      with EncParams.Parameter[0] do
      begin
        Guid := EncoderQuality;
        NumberOfValues := 1;
        Type_ := EncoderParameterValueTypeLong;
        Value := @Quality;
      end;
      PEncParams := @EncParams;
    end
      else PEncParams := nil;
    Status := SaveFunction(Bitmap, UserData, @EncCLSID, PEncParams);
    if Status <> Ok then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'ImageCodec.SaveImage: can''t save image (Status=%d)', [Integer(Status)]);{$ENDIF}
      Exit;
    end;
    Result := true;
  finally
    GdipDisposeImage(Bitmap);
  end;
end;

procedure TImageCodec.SaveRaw(Stream: TStream);
begin
  Stream.Write(RawMagic, SizeOf(RawMagic));
  Stream.Write(FWidth, SizeOf(FWidth));
  Stream.Write(FHeight, SizeOf(FHeight));
  Stream.Write(FStride, SizeOf(FStride));
  Stream.Write(FPixelFormat, SizeOf(FPixelFormat));
  Stream.Write(FPixels^, FStride * FHeight);
end;

procedure TImageCodec.Pack;
var
  i, RowSize: Integer;
begin
  RowSize := FWidth * BitDepths[FPixelFormat] div 8;
  if FStride > RowSize then
    for i := 1 to FHeight - 1 do
      Move(IncPtr(FPixels, i * FStride)^, IncPtr(FPixels, i * RowSize)^, RowSize);
  FStride := RowSize;
end;

procedure TImageCodec.SetPixels(Value: PByteArray);
begin
  Move(Value^, FPixels^, FStride * FHeight);
end;

var
  GdiplusStartupInput: TGdiplusStartupInput = (GdiplusVersion: 1; DebugEventCallback: nil; SuppressBackgroundThread: false; SuppressExternalCodecs: false);
  GdiplusToken: ULONG = 0;

initialization
  {$IFDEF VSE_LOG}LogF(llInfo, 'GDI+ initialization: status=%d, token=%08x', [Integer({$ENDIF}GdiplusStartup(GdiplusToken, @GdiplusStartupInput, nil){$IFDEF VSE_LOG}), GdiplusToken]){$ENDIF};

finalization
  GdiplusShutdown(GdiplusToken);

end.