unit USound;

interface

uses 
  Windows, AvL, avlUtils, bass;

type
  TChannelAttributes3D=record
    Mode: DWORD;
    Min, Max: Single;
    IAngle, OAngle, OutVol: DWORD;
  end;

  TChannelPosition3D=record
    Pos, Orient, Vel: BASS_3DVECTOR;
  end;

  TChannelAttributes=record
    Freq, Volume, Pan: Integer;
  end;

  TSound=class
  private
    function  GetDeviceName: string;
    procedure SetDeviceName(Name: string);
    function  GetDevice: DWORD;
    procedure SetDevice(Dev: DWORD);
    function  GetVolume: DWORD;
    procedure SetVolume(Vol: DWORD);
    function  GetConfig(Option: DWORD): DWORD;
    procedure SetConfig(Option, Value: DWORD);
  public
    class procedure ListDevices(List: TStringList);
    class function  ErrorName(ErrorCode: Integer): string;
    class function  LastError: Integer;
    class function  LastErrorName: string;
    constructor Create; overload;
    constructor Create(Device: Integer; Freq: DWORD=44100; Flags: DWORD=BASS_DEVICE_3D; CLSID: PGUID=nil); overload;
    constructor Create(Device: string; Freq: DWORD=44100; Flags: DWORD=BASS_DEVICE_3D; CLSID: PGUID=nil); overload;
    destructor Destroy; override;
    procedure Start;
    procedure Pause;
    procedure Stop;
    procedure Update;
    property DeviceName: string read GetDeviceName write SetDeviceName;
    property Device: DWORD read GetDevice write SetDevice;
    property Volume: DWORD read GetVolume write SetVolume;
    property Config[Option: DWORD]: DWORD read GetConfig write SetConfig;
  end;

  TSoundChannel=class
  private
    function  GetAttributes3D: TChannelAttributes3D;
    procedure SetAttributes3D(Attr: TChannelAttributes3D);
    function  GetPosition3D: TChannelPosition3D;
    procedure SetPosition3D(Pos: TChannelPosition3D);
    function  GetAttributes: TChannelAttributes;
    procedure SetAttributes(Attr: TChannelAttributes);
    function  GetDevice: DWORD;
    procedure SetDevice(Dev: DWORD);
    function  GetEAXMix: Single;
    procedure SetEAXMix(Mix: Single);
    function  GetPosition: Int64;
    procedure SetPosition(Pos: Int64);
    function  GetActive: DWORD;
    function  GetSliding: DWORD;
  protected
    FHandle: DWORD;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Play(Restart: Boolean=false);
    procedure Pause;
    procedure Stop;
    procedure Attrib3D(Mode: DWORD; Min, Max: Single; IAngle, OAngle, OutVol: DWORD);
    procedure Pos3D(const Pos, Orient, Vel: BASS_3DVECTOR);
    procedure Attrib(Freq, Volume, Pan: Integer);
    procedure PreBuf(Length: DWORD);
    procedure SlideAttributes(Freq, Volume, Pan: Integer; Time: DWORD);
    function  BytesToSeconds(Pos: Int64): Single;
    function  SecondsToBytes(Pos: Single): Int64;
    property Handle: DWORD read FHandle;
    property Attributes3D: TChannelAttributes3D read GetAttributes3D write SetAttributes3D;
    property Position3D: TChannelPosition3D read GetPosition3D write SetPosition3D;
    property Attributes: TChannelAttributes read GetAttributes write SetAttributes;
    property Device: DWORD read GetDevice write SetDevice;
    property EAXMix: Single read GetEAXMix write SetEAXMix;
    property Position: Int64 read GetPosition write SetPosition;
    property Active: DWORD read GetActive;
    property Sliding: DWORD read GetSliding;
  end;

  TSoundStream=class(TSoundChannel)
  public
    constructor Create(Data: TStream; Flags: DWORD=0);
    destructor Destroy; override;
    function  FilePosition(Mode: DWORD): DWORD;
  end;

implementation

uses
  VSECore, VSELog, VSEPakMan;

{Channels manager}
var
  SoundChannels: TList;

{TSound}
{class functions}

class procedure TSound.ListDevices(List: TStringList);
var
  i: Integer;
begin
  List.Clear;
  i:=0;
  while BASS_GetDeviceDescription(i)<>nil do
  begin
    List.Add(string(BASS_GetDeviceDescription(i)));
    Inc(i);
  end;
end;

class function TSound.ErrorName(ErrorCode: Integer): string;
begin
  case ErrorCode of
    BASS_OK: Result:='OK';
    BASS_ERROR_MEM: Result:='ERROR_MEM';
    BASS_ERROR_FILEOPEN: Result:='ERROR_FILEOPEN';
    BASS_ERROR_DRIVER: Result:='ERROR_DRIVER';
    BASS_ERROR_BUFLOST: Result:='ERROR_BUFLOST';
    BASS_ERROR_HANDLE: Result:='ERROR_HANDLE';
    BASS_ERROR_FORMAT: Result:='ERROR_FORMAT';
    BASS_ERROR_POSITION: Result:='ERROR_POSITION';
    BASS_ERROR_INIT: Result:='ERROR_INIT';
    BASS_ERROR_START: Result:='ERROR_START';
    BASS_ERROR_ALREADY: Result:='ERROR_ALREADY';
    BASS_ERROR_NOPAUSE: Result:='ERROR_NOPAUSE';
    BASS_ERROR_NOCHAN: Result:='ERROR_NOCHAN';
    BASS_ERROR_ILLTYPE: Result:='ERROR_ILLTYPE';
    BASS_ERROR_ILLPARAM: Result:='ERROR_ILLPARAM';
    BASS_ERROR_NO3D: Result:='ERROR_NO3D';
    BASS_ERROR_NOEAX: Result:='ERROR_NOEAX';
    BASS_ERROR_DEVICE: Result:='ERROR_DEVICE';
    BASS_ERROR_NOPLAY: Result:='ERROR_NOPLAY';
    BASS_ERROR_FREQ: Result:='ERROR_FREQ';
    BASS_ERROR_NOTFILE: Result:='ERROR_NOTFILE';
    BASS_ERROR_NOHW: Result:='ERROR_NOHW';
    BASS_ERROR_EMPTY: Result:='ERROR_EMPTY';
    BASS_ERROR_NONET: Result:='ERROR_NONET';
    BASS_ERROR_CREATE: Result:='ERROR_CREATE';
    BASS_ERROR_NOFX: Result:='ERROR_NOFX';
    BASS_ERROR_PLAYING: Result:='ERROR_PLAYING';
    BASS_ERROR_NOTAVAIL: Result:='ERROR_NOTAVAIL';
    BASS_ERROR_DECODE: Result:='ERROR_DECODE';
    BASS_ERROR_DX: Result:='ERROR_DX';
    BASS_ERROR_TIMEOUT: Result:='ERROR_TIMEOUT';
    BASS_ERROR_FILEFORM: Result:='ERROR_FILEFORM';
    BASS_ERROR_SPEAKER: Result:='ERROR_SPEAKER';
    BASS_ERROR_VERSION: Result:='ERROR_VERSION';
    BASS_ERROR_CODEC: Result:='ERROR_CODEC';
    BASS_ERROR_UNKNOWN: Result:='ERROR_UNKNOWN';
    else Result:='Unknown error code '+IntToStr(ErrorCode);
  end;
end;

class function TSound.LastError: Integer;
begin
  Result:=BASS_ErrorGetCode;
end;

class function TSound.LastErrorName: string;
begin
  Result:=ErrorName(LastError);
end;

{Public}

constructor TSound.Create;
begin
  Create(-1);
end;

constructor TSound.Create(Device: Integer; Freq: DWORD=44100; Flags: DWORD=BASS_DEVICE_3D; CLSID: PGUID=nil);
begin
  inherited Create;
  if HiWord(BASS_GetVersion)<>BASSVERSION then
  begin
    Log(llError, 'Sound.Create: incorrect BASS version');
    Exit;
  end;
  if not BASS_Init(Device, Freq, Flags, Core.Handle, CLSID) then
  begin
    LogF(llError, 'Sound.Create: BASS initializing error (%s)', [LastErrorName]);
    Exit;
  end;
end;

constructor TSound.Create(Device: string; Freq: DWORD=44100; Flags: DWORD=BASS_DEVICE_3D; CLSID: PGUID=nil);
var
  List: TStringList;
  i: Integer;
begin
  Device:=LowerCase(Device);
  if Device='default' then
  begin
    Create(-1, Freq, Flags, CLSID);
    Exit;
  end;
  List:=TStringList.Create;
  try
    ListDevices(List);
    for i:=0 to List.Count-1 do
      if Device=LowerCase(List[i]) then
      begin
        Create(i, Freq, Flags, CLSID);
        Exit;
      end;
    LogF(llError, 'Sound.Create: Device %s not found, using default', [Device]);
    Create(-1, Freq, Flags, CLSID);
  finally
    FAN(List);
  end;
end;

destructor TSound.Destroy;
var
  i: Integer;
begin
  for i:=0 to SoundChannels.Count-1 do
    TSoundChannel(SoundChannels[i]).Free;
  SoundChannels.Clear;
  Stop;
  if not BASS_Free
    then LogF(llError, 'Sound.Destroy: BASS freeing error (%s)', [LastErrorName]);
  inherited Destroy;
end;

procedure TSound.Start;
begin
  if not BASS_Start
    then Log(llError, 'Sound.Start: '+LastErrorName);
end;

procedure TSound.Pause;
begin
  if not BASS_Pause
    then Log(llError, 'Sound.Pause: '+LastErrorName);
end;

procedure TSound.Stop;
begin
  if not BASS_Stop
    then Log(llError, 'Sound.Stop: '+LastErrorName);
end;

procedure TSound.Update;
begin
  if not BASS_Update
    then Log(llError, 'Sound.Update: '+LastErrorName);
end;

{Private}

function TSound.GetDeviceName: string;
begin
  Result:=string(BASS_GetDeviceDescription(Device));
end;

procedure TSound.SetDeviceName(Name: string);
var
  List: TStringList;
  i: Integer;
begin
  Name:=LowerCase(Name);
  List:=TStringList.Create;
  try
    ListDevices(List);
    for i:=0 to List.Count-1 do
      if Name=LowerCase(List[i]) then
      begin
        Device:=i;
        Exit;
      end;
    LogF(llError, 'Sound.SetDeviceName: Device %s not found', [Name]);
  finally
    FAN(List);
  end;
end;

function TSound.GetDevice: DWORD;
begin
  Result:=BASS_GetDevice;
  if Result=$FFFFFFFF
    then Log(llError, 'Sound.SetDevice: '+LastErrorName);
end;

procedure TSound.SetDevice(Dev: DWORD);
begin
  if not BASS_SetDevice(Dev)
    then Log(llError, 'Sound.GetDevice: '+LastErrorName);
end;

function TSound.GetVolume: DWORD;
begin
  Result:=BASS_GetVolume;
  if Result=$FFFFFFFF
    then Log(llError, 'Sound.GetVolume: '+LastErrorName);
end;

procedure TSound.SetVolume(Vol: Cardinal);
begin
  if not BASS_SetVolume(Vol)
    then Log(llError, 'Sound.SetVolume: '+LastErrorName);
end;

function TSound.GetConfig(Option: DWORD): DWORD;
begin
  Result:=BASS_GetConfig(Option);
  if Result=$FFFFFFFF
    then LogF(llError, 'Sound.GetConfig[%d]: %s', [Option, LastErrorName]);
end;

procedure TSound.SetConfig(Option, Value: DWORD);
begin
  if BASS_SetConfig(Option, Value)=$FFFFFFFF
    then LogF(llError, 'Sound.SetConfig[%d]: %s', [Option, LastErrorName]);
end;

{TSoundChannel}
{Public}

constructor TSoundChannel.Create;
begin
  inherited Create;
  FHandle:=0;
  SoundChannels.Add(Self);
end;

destructor TSoundChannel.Destroy;
begin
  SoundChannels.Remove(Self);
  inherited Destroy;
end;

procedure TSoundChannel.Play(Restart: Boolean=false);
begin
  if not BASS_ChannelPlay(FHandle, Restart)
    then Log(llError, 'SoundChannel.Play: '+TSound.LastErrorName);
end;

procedure TSoundChannel.Pause;
begin
  if not BASS_ChannelPause(FHandle)
    then Log(llError, 'SoundChannel.Pause: '+TSound.LastErrorName);
end;

procedure TSoundChannel.Stop;
begin
  if not BASS_ChannelStop(FHandle)
    then Log(llError, 'SoundChannel.Stop: '+TSound.LastErrorName);
end;

procedure TSoundChannel.Attrib3D(Mode: DWORD; Min, Max: Single; IAngle, OAngle, OutVol: DWORD);
begin
  if not BASS_ChannelSet3DAttributes(FHandle, Mode, Min, Max, IAngle, OAngle, OutVol)
    then Log(llError, 'SoundChannel.Attrib3D: '+TSound.LastErrorName);
end;

procedure TSoundChannel.Pos3D(const Pos, Orient, Vel: BASS_3DVECTOR);
begin
  if not BASS_ChannelSet3DPosition(FHandle, Pos, Orient, Vel)
    then Log(llError, 'SoundChannel.Pos3D: '+TSound.LastErrorName);
end;

procedure TSoundChannel.Attrib(Freq, Volume, Pan: Integer);
begin
  if not BASS_ChannelSetAttributes(FHandle, Freq, Volume, Pan)
    then Log(llError, 'SoundChannel.Attrib: '+TSound.LastErrorName);
end;

procedure TSoundChannel.PreBuf(Length: DWORD);
begin
  if not BASS_ChannelPreBuf(FHandle, Length)
    then Log(llError, 'SoundChannel.PreBuf: '+TSound.LastErrorName);
end;

procedure TSoundChannel.SlideAttributes(Freq, Volume, Pan: Integer; Time: DWORD);
begin
  if not BASS_ChannelSlideAttributes(FHandle, Freq, Volume, Pan, Time)
    then Log(llError, 'SoundChannel.SlideAttributes: '+TSound.LastErrorName);
end;

function TSoundChannel.BytesToSeconds(Pos: Int64): Single;
begin
  Result:=BASS_ChannelBytes2Seconds(FHandle, Pos);
  if Result<0
    then Log(llError, 'SoundChannel.BytesToSeconds: '+TSound.LastErrorName);
end;

function TSoundChannel.SecondsToBytes(Pos: Single): Int64;
begin
  Result:=BASS_ChannelSeconds2Bytes(FHandle, Pos);
  if Result=-1
    then Log(llError, 'SoundChannel.SecondsToBytes: '+TSound.LastErrorName);
end;

{Private}

function TSoundChannel.GetAttributes3D: TChannelAttributes3D;
begin
  if not BASS_ChannelGet3DAttributes(FHandle, Result.Mode, Result.Min, Result.Max, Result.IAngle, Result.OAngle, Result.OutVol)
    then Log(llError, 'SoundChannel.GetAttributes3D: '+TSound.LastErrorName);
end;

procedure TSoundChannel.SetAttributes3D(Attr: TChannelAttributes3D);
begin
  if not BASS_ChannelSet3DAttributes(FHandle, Attr.Mode, Attr.Min, Attr.Max, Attr.IAngle, Attr.OAngle, Attr.OutVol)
    then Log(llError, 'SoundChannel.SetAttributes3D: '+TSound.LastErrorName);
end;

function TSoundChannel.GetPosition3D: TChannelPosition3D;
begin
  if not BASS_ChannelGet3DPosition(FHandle, Result.Pos, Result.Orient, Result.Vel)
    then Log(llError, 'SoundChannel.GetPosition3D: '+TSound.LastErrorName);
end;

procedure TSoundChannel.SetPosition3D(Pos: TChannelPosition3D);
begin
  if not BASS_ChannelSet3DPosition(FHandle, Pos.Pos, Pos.Orient, Pos.Vel)
    then Log(llError, 'SoundChannel.SetPosition3D: '+TSound.LastErrorName);
end;

function TSoundChannel.GetAttributes: TChannelAttributes;
begin
  if not BASS_ChannelGetAttributes(FHandle, DWORD(Result.Freq), DWORD(Result.Volume), Result.Pan)
    then Log(llError, 'SoundChannel.GetAttributes: '+TSound.LastErrorName);
end;

procedure TSoundChannel.SetAttributes(Attr: TChannelAttributes);
begin
  if not BASS_ChannelSetAttributes(FHandle, Attr.Freq, Attr.Volume, Attr.Pan)
    then Log(llError, 'SoundChannel.SetAttributes: '+TSound.LastErrorName);
end;

function TSoundChannel.GetDevice: DWORD;
begin
  Result:=BASS_ChannelGetDevice(FHandle);
  if Result=$FFFFFFFF
    then Log(llError, 'SoundChannel.GetDevice: '+TSound.LastErrorName);
end;

procedure TSoundChannel.SetDevice(Dev: DWORD);
begin
  if not BASS_ChannelSetDevice(FHandle, Dev)
    then Log(llError, 'SoundChannel.SetDevice: '+TSound.LastErrorName);
end;

function TSoundChannel.GetEAXMix: Single;
begin
  if not BASS_ChannelGetEAXMix(FHandle, Result)
    then Log(llError, 'SoundChannel.GetEAXMix: '+TSound.LastErrorName);
end;

procedure TSoundChannel.SetEAXMix(Mix: Single);
begin
  if not BASS_ChannelSetEAXMix(FHandle, Mix)
    then Log(llError, 'SoundChannel.SetEAXMix: '+TSound.LastErrorName);
end;

function TSoundChannel.GetPosition: Int64;
begin
  Result:=BASS_ChannelGetPosition(FHandle);
  if Result=-1
    then Log(llError, 'SoundChannel.GetPosition: '+TSound.LastErrorName);
end;

procedure TSoundChannel.SetPosition(Pos: Int64);
begin
  if not BASS_ChannelSetPosition(FHandle, Pos)
    then Log(llError, 'SoundChannel.SetPosition: '+TSound.LastErrorName);
end;

function TSoundChannel.GetActive: DWORD;
begin
  Result:=BASS_ChannelIsActive(FHandle);
end;

function TSoundChannel.GetSliding: DWORD;
begin
  Result:=BASS_ChannelIsSliding(FHandle);
end;

{TSoundStream}

function SoundStreamCallback(Action, P1, P2, User: DWORD): DWORD; stdcall;
var
  Data: TStream absolute User;
begin
  Result:=0;
  if Data=nil then
  begin
    Log(llError, 'SoundStreamCallback: no source provided');
    Exit;
  end;
  case Action of
    BASS_FILE_LEN: Result:=Data.Size;
    BASS_FILE_CLOSE: FAN(Data);
    BASS_FILE_READ: Result:=Data.Read(Pointer(P2)^, P1);
    BASS_FILE_SEEK: LongBool(Result):=Data.Seek(P1, soFromBeginning)<>P1;
  end;
end;  

constructor TSoundStream.Create(Data: TStream; Flags: DWORD);
begin
  inherited Create;
  FHandle:=BASS_StreamCreateFileUser(Data is TPakLZMAStream, Flags, SoundStreamCallback, DWORD(Pointer(Data)));
  if FHandle=0
    then Log(llError, 'SoundStream.Create: '+TSound.LastErrorName);
end;

destructor TSoundStream.Destroy;
begin
  Stop;
  if not BASS_StreamFree(FHandle)
    then Log(llError, 'SoundStream.Destroy: '+TSound.LastErrorName);
  inherited Destroy;
end;

function TSoundStream.FilePosition(Mode: DWORD): DWORD;
begin
  Result:=BASS_StreamGetFilePosition(FHandle, Mode);
  if Result=DWORD(-1)
    then Log(llError, 'SoundStream.FilePosition: '+TSound.LastErrorName);
end;

{for initialization/finalization}

var
  i: Integer;

initialization

SoundChannels:=TList.Create;

finalization

for i:=0 to SoundChannels.Count-1 do
  TSoundChannel(SoundChannels[i]).Free;
FAN(SoundChannels);

end.
