{(c)VgaSoft, 2004}
unit avlLZMADec;

interface

uses
  Windows, AVL, avlCustomDecompressor;

type

{TLZMADecompressor}

  TLZMADecompressor = class;
  TLZMADecompressorCallbackData = record
    Callback: Pointer;
    Instance: TLZMADecompressor;
  end;
  TLZMADecompressor = class(TCustomDecompressor)
  private
    FReachedEnd: Boolean;
    FCallbackData: TLZMADecompressorCallbackData;
    FLzmaInternalData: Pointer;
    FHeapBase: Pointer;
    FHeapSize: Cardinal;
    FBuffer: array[0..65535] of Byte;
    procedure DestroyHeap;
    procedure DoRead(var Buffer: Pointer; var BufferSize: Cardinal);
    procedure ProcessHeader;
  public
    destructor Destroy; override;
    procedure DecompressInto(var Buffer; Count: Longint); override;
    procedure Reset; override;
  end;
  
implementation

const
  SLZMADataError = 'lzma: Compressed data is corrupted (%d)';
  
procedure OutOfMemoryError;
begin
  raise EOutOfMemory.Create('Out of memory');
end;

procedure LZMAInternalError(const Msg: String);
begin
  raise ECompressInternalError.Create('lzma: ' + Msg);
end;

procedure LZMADataError(const Id: Integer);
begin
  raise ECompressDataError.CreateFmt(SLZMADataError, [Id]);
end;

{TLZMADecompressor}

{$L LzmaDecode.obj}

type
  TLzmaInCallback = record
    Read: function(obj: Pointer; var buffer: Pointer; var bufferSize: Cardinal): Integer;
  end;

const
  LZMA_RESULT_OK = 0;
  LZMA_RESULT_DATA_ERROR = 1;
  LZMA_RESULT_NOT_ENOUGH_MEM = 2;

function LzmaGetInternalSize(lc, lp: Integer): Cardinal; external;
function LzmaDecoderInit(buffer: Pointer; bufferSize: Cardinal;
  lc, lp, pb: Integer; var dictionary; dictionarySize: Cardinal;
  var inCallback: TLzmaInCallback): Integer; external;
function LzmaDecode(buffer: Pointer; var outStream; outSize: Cardinal;
  var outSizeProcessed: Cardinal): Integer; external;

function ReadFunc(obj: Pointer; var buffer: Pointer; var bufferSize: Cardinal): Integer;
begin
  TLZMADecompressorCallbackData(obj^).Instance.DoRead(buffer, bufferSize);
  { Don't bother returning any sort of failure code, because if DoRead failed,
    it would've raised an exception }
  Result := LZMA_RESULT_OK;
end;

destructor TLZMADecompressor.Destroy;
begin
  DestroyHeap;
  inherited;
end;

procedure TLZMADecompressor.DestroyHeap;
begin
  FLzmaInternalData := nil;
  FHeapSize := 0;
  if Assigned(FHeapBase) then begin
    VirtualFree(FHeapBase, 0, MEM_RELEASE);
    FHeapBase := nil;
  end;
end;

procedure TLZMADecompressor.DoRead(var Buffer: Pointer; var BufferSize: Cardinal);
begin
  Buffer := @FBuffer;
  BufferSize := 0;
  if not FReachedEnd then begin
    BufferSize := ReadProc(FBuffer, SizeOf(FBuffer));
    if BufferSize = 0 then
      FReachedEnd := True;  { not really necessary, but for consistency }
  end;
end;

procedure TLZMADecompressor.ProcessHeader;
var
  Props: Byte;
  DictionarySize: Longint;
  lc, lp, pb: Integer;
  InternalSize, NewHeapSize: Cardinal;
  InternalData, Dictionary: Pointer;
  Code: Integer;
begin
  { Read header fields }
  if ReadProc(Props, SizeOf(Props)) <> SizeOf(Props) then
    LZMADataError(1);
  if ReadProc(DictionarySize, SizeOf(DictionarySize)) <> SizeOf(DictionarySize) then
    LZMADataError(2);
  if (DictionarySize < 0) or (DictionarySize > 32 shl 20) then
    { sanity check: we only use dictionary sizes <= 32 MB }
    LZMADataError(7);

  { Crack Props }
  if Props >= (9 * 5 * 5) then
    LZMADataError(3);
  pb := 0;
  while Props >= (9 * 5) do begin
    Inc(pb);
    Dec(Props, (9 * 5));
  end;
  lp := 0;
  while Props >= 9 do begin
    Inc(lp);
    Dec(Props, 9);
  end;
  lc := Props;

  { Figure out how much memory we need and allocate it }
  InternalSize := LzmaGetInternalSize(lc, lp);
  if InternalSize and 3 <> 0 then
    InternalSize := (InternalSize or 3) + 1;  { round up to DWORD boundary }
  NewHeapSize := InternalSize + Cardinal(DictionarySize);
  if FHeapSize <> NewHeapSize then begin
    DestroyHeap;
    FHeapBase := VirtualAlloc(nil, NewHeapSize, MEM_COMMIT, PAGE_READWRITE);
    if FHeapBase = nil then
      OutOfMemoryError;
    FHeapSize := NewHeapSize;
  end;
  InternalData := FHeapBase;
  Dictionary := Pointer(Cardinal(InternalData) + InternalSize);

  { Now initialize }
  TLzmaInCallback(FCallbackData.Callback).Read := ReadFunc;
  FCallbackData.Instance := Self;
  Code := LzmaDecoderInit(InternalData, InternalSize, lc, lp, pb,
    Dictionary^, DictionarySize, TLzmaInCallback(FCallbackData.Callback));
  case Code of
    LZMA_RESULT_OK: ;
    LZMA_RESULT_DATA_ERROR: LZMADataError(4);
  else
    LZMAInternalError(Format('LzmaDecoderInit failed (%d)', [Code]));
  end;
  FLzmaInternalData := InternalData;
end;

procedure TLZMADecompressor.DecompressInto(var Buffer; Count: Longint);
var
  Code: Integer;
  OutProcessed: Cardinal;
begin
  if FLzmaInternalData = nil then
    ProcessHeader;
  Code := LzmaDecode(FLzmaInternalData, Buffer, Count, OutProcessed);
  case Code of
    LZMA_RESULT_OK: ;
    LZMA_RESULT_DATA_ERROR: LZMADataError(5);
  else
    LZMAInternalError(Format('LzmaDecode failed (%d)', [Code]));
  end;
  if OutProcessed <> Cardinal(Count) then
    LZMADataError(6);
end;

procedure TLZMADecompressor.Reset;
begin
  FLzmaInternalData := nil;
  FReachedEnd := False;
end;

end.
