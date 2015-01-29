unit VSEImageCodec;

interface

uses
  Windows, AvL, avlUtils, avlIStreamAdapter{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TImageFormat = (ifBMP, ifJPEG, ifGIF, ifPNG, ifTIFF); // Image formats
  TPixelFormat = (pfGS8bit, pfBGR24bit, pfRGBA32bit, pfBGRA32bit); // Pixel data format: 8-bit grayscale, 24-bit BGR, 32-bit RGBA and BGRA
  TImageData = record // Raw image data structure
    Width, Height: Cardinal; // Image dimensions
    Stride: Integer; // Size of row in bytes, including alignment
    PixelFormat: TPixelFormat; // Format of pixel data
    Pixels: PByte; // Pointer to raw pixels array
  end;

const
  ImageFormatMime: array[TImageFormat] of string = ('image/bmp', 'image/jpeg', 'image/gif', 'image/png', 'image/tiff');
  ImageFormatExtension: array[TImageFormat] of string = ('.bmp', '.jpg', '.gif', '.png', '.tif');

function SaveImageToFile(ImageData: TImageData; const FileName: string; ImageFormat: TImageFormat; Quality: Cardinal = 0): Boolean; // Saves ImageData to file in specified format, returns true if successful
function SaveImageToStream(ImageData: TImageData; Stream: TStream; ImageFormat: TImageFormat; Quality: Cardinal = 0): Boolean; // Saves ImageData to stream in specified format, returns true if successful
function LoadImageFromFile(const FileName: string; out ImageData: TImageData): Boolean; // Loads ImageData from file, returns true if successful
function LoadImageFromStream(Stream: TStream; out ImageData: TImageData): Boolean; // Loads ImageData from stream, returns true if successful
procedure SaveImageData(const ImageData: TImageData; Stream: TStream); // Save raw image data to stream
function LoadImageData(var ImageData: TImageData; Stream: TStream): Boolean; // Load raw image data from stream
function ImageDataRowSize(const ImageData: TImageData): Integer; // Size of image row in bytes
procedure InitImageData(var ImageData: TImageData); // Initializes TImageData structure
procedure FreeImageData(var ImageData: TImageData); // Frees TImageData structore

implementation

{$I GDIPAPI.inc}

const
  BitDepth: array[TPixelFormat] of Integer = (8, 24, 32, 32);
  PixelFormat: array[TPixelFormat] of Integer = (PixelFormat8bppIndexed, PixelFormat24bppRGB, 0, PixelFormat32bppARGB);

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

function SaveImageToFile(ImageData: TImageData; const FileName: string; ImageFormat: TImageFormat; Quality: Cardinal = 0): Boolean;
var
  Status: TStatus;
  Bitmap: Pointer;
  EncCLSID: TGUID;
  EncParams: TEncoderParameters;
  PEncParams: PEncoderParameters;
  i: Integer;
begin
  Result := false;
  if ImageData.Stride = 0 then ImageData.Stride := ImageDataRowSize(ImageData);
  Status := GdipCreateBitmapFromScan0(ImageData.Width, ImageData.Height, ImageData.Stride, PixelFormat[ImageData.PixelFormat], ImageData.Pixels, Bitmap);
  if Status <> Ok then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'SaveImageToFile("%s"): can''t create GDI+ bitmap (Status=%d)', [FileName, Integer(Status)]);{$ENDIF}
    Exit;
  end;
  try
    if ImageData.PixelFormat = pfGS8bit then GdipSetImagePalette(Bitmap, GSPalette);
    GdipImageRotateFlip(Bitmap, RotateNoneFlipY);
    EncCLSID := GetEncoderCLSID(ImageFormatMime[ImageFormat]);
    if EncCLSID.D1 = 0 then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'SaveImageToFile("%s"): GDI+ encoder for %s is not available', [FileName, ImageFormatMime[ImageFormat]]);{$ENDIF}
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
    Status := GdipSaveImageToFile(Bitmap, PWideChar(WideString(FileName)), @EncCLSID, PEncParams);
    if Status <> Ok then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'SaveImageToFile("%s"): can''t save image to file (Status=%d)', [FileName, Integer(Status)]);{$ENDIF}
      Exit;
    end;
    Result := true;
  finally
    GdipDisposeImage(Bitmap);
  end;
end;

function SaveImageToStream(ImageData: TImageData; Stream: TStream; ImageFormat: TImageFormat; Quality: Cardinal = 0): Boolean;
var
  Status: TStatus;
  Bitmap: Pointer;
  EncCLSID: TGUID;
  EncParams: TEncoderParameters;
  PEncParams: PEncoderParameters;
  i: Integer;
  StreamAdapter: IStream;
begin
  Result := false;
  if ImageData.Stride = 0 then ImageData.Stride := ImageDataRowSize(ImageData);
  Status := GdipCreateBitmapFromScan0(ImageData.Width, ImageData.Height, ImageData.Stride, PixelFormat[ImageData.PixelFormat], ImageData.Pixels, Bitmap);
  if Status <> Ok then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'SaveImageToStream: can''t create GDI+ bitmap (Status=%d)', [Integer(Status)]);{$ENDIF}
    Exit;
  end;
  try
    if ImageData.PixelFormat = pfGS8bit then GdipSetImagePalette(Bitmap, GSPalette);
    GdipImageRotateFlip(Bitmap, RotateNoneFlipY);
    EncCLSID := GetEncoderCLSID(ImageFormatMime[ImageFormat]);
    if EncCLSID.D1 = 0 then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'SaveImageToStream: GDI+ encoder for %s is not available', [ImageFormatMime[ImageFormat]]);{$ENDIF}
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
    TIStreamAdapter.Create(Stream).GetInterface(IStream, StreamAdapter);
    Status := GdipSaveImageToStream(Bitmap, StreamAdapter, @EncCLSID, PEncParams);
    if Status <> Ok then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'SaveImageToStream: can''t save image to stream (Status=%d)', [Integer(Status)]);{$ENDIF}
      Exit;
    end;
    Result := true;
  finally
    GdipDisposeImage(Bitmap);
  end;
end;

function LoadImageFromFile(const FileName: string; out ImageData: TImageData): Boolean;
var
  Bitmap: Pointer;
  BitmapData: TBitmapData;
  i, PixFormat, PalSize: Integer;
  Palette: TColorPalette256;
  Rect: TGPRect;
  Status: TStatus;
begin
  Result := false;
  Status := GdipCreateBitmapFromFile(PWideChar(WideString(FileName)), Bitmap);
  if Status <> Ok then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'LoadImageFromFile("%s"): can''t create GDI+ bitmap (Status=%d)', [FileName, Integer(Status)]);{$ENDIF}
    Exit;
  end;
  try
    with ImageData do
    begin
      if GdipGetImageWidth(Bitmap, Width) <> Ok then Exit;
      if GdipGetImageHeight(Bitmap, Height) <> Ok then Exit;
      if GdipGetImagePixelFormat(Bitmap, PixFormat) <> Ok then Exit;
      case (PixFormat and $FF00) shr 8 of
        1, 4, 8: PixelFormat := pfGS8bit;
        16, 24: PixelFormat := pfBGR24bit;
        32: PixelFormat := pfBGRA32bit;
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
              PixelFormat := pfBGR24bit;
              Break;
            end;
      end;
    end;
    with Rect do
    begin
      X := 0;
      Y := 0;
      Width := ImageData.Width;
      Height := ImageData.Height;
    end;
    ZeroMemory(@BitmapData, SizeOf(BitmapData));
    Status := GdipBitmapLockBits(Bitmap, Rect, ImageLockModeRead, PixelFormat[ImageData.PixelFormat], @BitmapData);
    if (Status = InvalidParameter) and (ImageData.PixelFormat = pfGS8bit) then
    begin
      {$IFDEF VSE_LOG}LogF(llWarning, 'LoadImageFromFile("%s"): can''t lock bitmap data as GS8, retrying locking as BGR24', [FileName]);{$ENDIF}
      ImageData.PixelFormat := pfBGR24bit;
      Status := GdipBitmapLockBits(Bitmap, Rect, ImageLockModeRead, PixelFormat[ImageData.PixelFormat], @BitmapData);
    end;
    if Status <> Ok then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'LoadImageFromFile("%s"): can''t lock bitmap data (Status=%d)', [FileName, Integer(Status)]);{$ENDIF}
      Exit;
    end;
    try
      ImageData.Stride := Abs(BitmapData.Stride);
      ReallocMem(ImageData.Pixels, ImageData.Stride * ImageData.Height);
      Move(BitmapData.Scan0^, ImageData.Pixels^, ImageData.Stride * ImageData.Height);
    finally
      GdipBitmapUnlockBits(Bitmap, @BitmapData);
    end;
    Result := true;
  finally
    {$IFDEF VSE_LOG}if not Result then LogF(llError, 'LoadImageFromFile("%s"): image loading failed (Status=%d)', [FileName, Integer(Status)]);{$ENDIF}
    GdipDisposeImage(Bitmap);
  end;
end;

function LoadImageFromStream(Stream: TStream; out ImageData: TImageData): Boolean;
var
  Bitmap: Pointer;
  BitmapData: TBitmapData;
  i, PixFormat, PalSize: Integer;
  Palette: TColorPalette256;
  Rect: TGPRect;
  Status: TStatus;
  StreamAdapter: IStream;
begin
  Result := false;
  TIStreamAdapter.Create(Stream).GetInterface(IStream, StreamAdapter);
  Status := GdipCreateBitmapFromStream(StreamAdapter, Bitmap);
  if Status <> Ok then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'LoadImageFromStream: can''t create GDI+ bitmap (Status=%d)', [Integer(Status)]);{$ENDIF}
    Exit;
  end;
  try
    with ImageData do
    begin
      if GdipGetImageWidth(Bitmap, Width) <> Ok then Exit;
      if GdipGetImageHeight(Bitmap, Height) <> Ok then Exit;
      if GdipGetImagePixelFormat(Bitmap, PixFormat) <> Ok then Exit;
      case (PixFormat and $FF00) shr 8 of
        1, 4, 8: PixelFormat := pfGS8bit;
        16, 24: PixelFormat := pfBGR24bit;
        32: PixelFormat := pfBGRA32bit;
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
              PixelFormat := pfBGR24bit;
              Break;
            end;
      end;
    end;
    with Rect do
    begin
      X := 0;
      Y := 0;
      Width := ImageData.Width;
      Height := ImageData.Height;
    end;
    ZeroMemory(@BitmapData, SizeOf(BitmapData));
    Status := GdipBitmapLockBits(Bitmap, Rect, ImageLockModeRead, PixelFormat[ImageData.PixelFormat], @BitmapData);
    if (Status = InvalidParameter) and (ImageData.PixelFormat = pfGS8bit) then
    begin
      {$IFDEF VSE_LOG}Log(llWarning, 'LoadImageFromStream: can''t lock bitmap data as GS8, retrying locking as BGR24');{$ENDIF}
      ImageData.PixelFormat := pfBGR24bit;
      Status := GdipBitmapLockBits(Bitmap, Rect, ImageLockModeRead, PixelFormat[ImageData.PixelFormat], @BitmapData);
    end;
    if Status <> Ok then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'LoadImageFromStream: can''t lock bitmap data (Status=%d)', [Integer(Status)]);{$ENDIF}
      Exit;
    end;
    try
      ImageData.Stride := Abs(BitmapData.Stride);
      ReallocMem(ImageData.Pixels, ImageData.Stride * ImageData.Height);
      Move(BitmapData.Scan0^, ImageData.Pixels^, ImageData.Stride * ImageData.Height);
    finally
      GdipBitmapUnlockBits(Bitmap, @BitmapData);
    end;
    Result := true;
  finally
    {$IFDEF VSE_LOG}if not Result then LogF(llError, 'LoadImageFromStream: image loading failed (Status=%d)', [Integer(Status)]);{$ENDIF}
    GdipDisposeImage(Bitmap);
  end;
end;

procedure SaveImageData(const ImageData: TImageData; Stream: TStream);
begin
  Stream.Write(ImageData, SizeOf(ImageData) - SizeOf(ImageData.Pixels));
  Stream.Write(ImageData.Pixels^, ImageData.Stride * ImageData.Height);
end;

function LoadImageData(var ImageData: TImageData; Stream: TStream): Boolean;
begin
  Result := false;
  if Stream.Size - Stream.Position < SizeOf(ImageData) - SizeOf(ImageData.Pixels) then Exit;
  Stream.Read(ImageData, SizeOf(ImageData) - SizeOf(ImageData.Pixels));
  if (Stream.Size - Stream.Position < ImageData.Stride * ImageData.Height) or
    not (ImageData.PixelFormat in [Low(TPixelFormat) .. High(TPixelFormat)]) then Exit;
  ReallocMem(ImageData.Pixels, ImageData.Stride * ImageData.Height);
  Stream.Read(ImageData.Pixels^, ImageData.Stride * ImageData.Height);
  Result := true;
end;

function ImageDataRowSize(const ImageData: TImageData): Integer;
begin
  Result := ImageData.Width * BitDepth[ImageData.PixelFormat] div 8;
end;

procedure InitImageData(var ImageData: TImageData);
begin
  ZeroMemory(@ImageData, SizeOf(ImageData));
end;

procedure FreeImageData(var ImageData: TImageData);
begin
  if Assigned(ImageData.Pixels) then FreeMem(ImageData.Pixels);
  ZeroMemory(@ImageData, SizeOf(ImageData));
end;

var
  GdiplusStartupInput: TGdiplusStartupInput = (GdiplusVersion: 1; DebugEventCallback: nil; SuppressBackgroundThread: false; SuppressExternalCodecs: false);
  GdiplusToken: ULONG = 0;

initialization
  {$IFDEF VSE_LOG}LogF(llInfo, 'GDI+ initialization: status=%d, token=%08x', [Integer({$ENDIF}GdiplusStartup(GdiplusToken, @GdiplusStartupInput, nil){$IFDEF VSE_LOG}), GdiplusToken]){$ENDIF};

finalization
  GdiplusShutdown(GdiplusToken);

end.