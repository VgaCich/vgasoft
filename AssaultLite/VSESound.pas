unit VSESound;

interface

uses
  Windows, AvL, avlUtils, MMSystem, DirectSound;

//{$I dsound.inc}

type
  TSound=class
  private
    FDirectSound: IDirectSound;
    FMusicBuffer: IDirectSoundBuffer;
    FMusicFile: TCustomMemoryStream;
    FMusicPCM: TWaveFormatEx;
    FMusicBufferDesc: TDSBufferDesc;
    FEnableBGM: Boolean;
    procedure SetEnableBGM(Value: Boolean);
    {$IFDEF VSE_CONSOLE}function SetBGMHandler(Sender: TObject; Args: array of const): Boolean;{$ENDIF}
  public
    constructor Create; //internally used
    destructor Destroy; override; //internally used
    {$IFDEF VSE_LOG}procedure LogCaps;{$ENDIF}
    procedure Update; //internally used
    procedure PlayMusic(const FileName: string); //Play music from file
    procedure StopMusic; //Stop music
    property EnableBGM: Boolean read FEnableBGM write SetEnableBGM;
  end;

var
  Sound: TSound;  //Global variable for access to Sound Engine

implementation

uses
  VSEInit, VSECore, VSEMemPak{$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{uFMOD}

const
  SNameEnableBGM = 'EnableBGM';
  XM_MEMORY=1;
  uFMOD_BUFFER_SIZE=262144;
  uFMOD_MixRate = 44100;
  SSectionSound='Sound';

{$L dsufmod.obj}
function uFMOD_DSPlaySong(lpXM: Pointer; param, fdwSong: Integer;
  lpDSBuffer: IDirectSoundBuffer): Integer; stdcall; external;

{TSound}

constructor TSound.Create;
begin
  inherited Create;
  {$IFDEF VSE_LOG}Log(llInfo, 'Sound: Create');{$ENDIF}
  {$IFDEF VSE_CONSOLE}Console.OnCommand['setbgm ?val=eoff:on']:=SetBGMHandler;{$ENDIF}
  if DirectSoundCreate(nil, FDirectSound, nil)<>S_OK then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Sound: Cannot initialize DirectSound');{$ENDIF}
    Exit;
  end;
  if FDirectSound.SetCooperativeLevel(Core.Handle, DSSCL_PRIORITY)<>S_OK then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Sound: Cannot set cooperative level');{$ENDIF}
    FDirectSound:=nil;
    Exit;
  end;
  with FMusicPCM do
  begin
    wFormatTag:=WAVE_FORMAT_PCM;
    nChannels:=2;
    nSamplesPerSec:=uFMOD_MixRate;
    nAvgBytesPerSec:=uFMOD_MixRate*4;
    nBlockAlign:=4;
    wBitsPerSample:=16;
    cbSize:=0;
  end;
	with FMusicBufferDesc do
	begin
		dwSize:=SizeOf(FMusicBufferDesc);
		dwFlags:=DSBCAPS_STATIC or DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2;
		dwBufferBytes:=uFMOD_BUFFER_SIZE;
		lpwfxFormat:=@FMusicPCM;
	end;
	if FDirectSound.CreateSoundBuffer(FMusicBufferDesc, FMusicBuffer, nil)<>S_OK then
	begin
    {$IFDEF VSE_LOG}Log(llError, 'Sound: Cannot create secondary buffer');{$ENDIF}
    FMusicBuffer:=nil;
  end;
  if Settings.FirstRun
    then Settings.Bool[SSectionSound, SNameEnableBGM]:=true;
  EnableBGM:=Settings.Bool[SSectionSound, SNameEnableBGM];
end;

destructor TSound.Destroy;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Sound: Destroy');{$ENDIF}
  Settings.Bool[SSectionSound, SNameEnableBGM]:=EnableBGM;
  StopMusic;
  FMusicBuffer:=nil;
  FDirectSound:=nil;
  inherited Destroy;
end;

{$IFDEF VSE_LOG}
procedure TSound.LogCaps;
const
  Flags: array[0..10] of record Name: string; Value: DWORD; end = (
    (Name: 'CONTINUOUSRATE'; Value: $00000010),
    (Name: 'EMULDRIVER'; Value: $00000020),
    (Name: 'CERTIFIED'; Value: $00000040),
    (Name: 'PRIMARYMONO'; Value: $00000001),
    (Name: 'PRIMARYSTEREO'; Value: $00000002),
    (Name: 'PRIMARY8BIT'; Value: $00000004),
    (Name: 'PRIMARY16BIT'; Value: $00000008),
    (Name: 'SECONDARYMONO'; Value: $00000100),
    (Name: 'SECONDARYSTEREO'; Value: $00000200),
    (Name: 'SECONDARY8BIT'; Value: $00000400),
    (Name: 'SECONDARY16BIT'; Value: $00000800));
var
  Caps: TDSCaps;
  i: Integer;
  S: string;
begin
  Caps.dwSize:=SizeOf(Caps);
  if FDirectSound.GetCaps(Caps)<>S_OK then
  begin
    Log(llError, 'Sound: Cannot retrieve DirectSound capabilities');
    Exit;
  end;
  with Caps do
  begin
    LogRaw(llInfo, '');
    LogRaw(llInfo, 'DirectSound capabilities:');
    LogRaw(llInfo, Format('Hardware secondary buffers sample rate: min=%d, max=%d', [dwMinSecondarySampleRate, dwMaxSecondarySampleRate]));
    LogRaw(llInfo, 'Primary buffers: '+IntToStr(dwPrimaryBuffers));
    LogRaw(llInfo, Format('Hardware secondary buffers: total=%d, static=%d, streaming=%d', [dwMaxHwMixingAllBuffers, dwMaxHwMixingStaticBuffers,	dwMaxHwMixingStreamingBuffers]));
    LogRaw(llInfo, Format('Free hardware secondary buffers: total=%d, static=%d, streaming=%d', [dwFreeHwMixingAllBuffers, dwFreeHwMixingStaticBuffers,	dwFreeHwMixingStreamingBuffers]));
    LogRaw(llInfo, Format('Hardware secondary 3D buffers: total=%d, static=%d, streaming=%d', [dwMaxHw3DAllBuffers, dwMaxHw3DStaticBuffers,	dwMaxHw3DStreamingBuffers]));
    LogRaw(llInfo, Format('Free hardware secondary 3D buffers: total=%d, static=%d, streaming=%d', [dwFreeHw3DAllBuffers, dwFreeHw3DStaticBuffers,	dwFreeHw3DStreamingBuffers]));
    LogRaw(llInfo, Format('Hardware memory: max=%d, free=%d, contig=%d', [dwTotalHwMemBytes,	dwFreeHwMemBytes, dwMaxContigFreeHwMemBytes]));
    LogRaw(llInfo, 'Hardware buffers transfer rate: '+IntToStr(dwUnlockTransferRateHwBuffers));
    LogRaw(llInfo, 'CPU overhead: '+IntToStr(dwPlayCpuOverheadSwBuffers));
    S:='Flags: ';
    for i:=0 to High(Flags) do
      if dwFlags and Flags[i].Value <> 0
        then S:=S+Flags[i].Name+' ';
    LogRaw(llInfo, S);
  end;
end;
{$ENDIF}

procedure TSound.Update;
begin

end;

procedure TSound.PlayMusic(const FileName: string);
begin
  if not Assigned(FMusicBuffer) then Exit;
  if Assigned(FMusicFile) then StopMusic;
  FMusicFile:=GetFile(FileName);
  if not Assigned(FMusicFile) then Exit;
  if EnableBGM
    then uFMOD_DSPlaySong(FMusicFile.Memory, FMusicFile.Size, XM_MEMORY, FMusicBuffer);
end;

procedure TSound.StopMusic;
begin
  uFMOD_DSPlaySong(nil, 0, 0, nil);
  FAN(FMusicFile);
end;

procedure TSound.SetEnableBGM(Value: Boolean);
begin
  if Value=FEnableBGM
    then Exit;
  FEnableBGM:=Value;
  if not FEnableBGM
    then StopMusic
    else if Assigned(FMusicBuffer) and Assigned(FMusicFile)
      then uFMOD_DSPlaySong(FMusicFile.Memory, FMusicFile.Size, XM_MEMORY, FMusicBuffer);
end;

{$IFDEF VSE_CONSOLE}
const
  BoolState: array[Boolean] of string = ('off', 'on');

function TSound.SetBGMHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args)>0
    then EnableBGM:=Boolean(Args[0].VInteger)
    else Console.WriteLn('BGM: '+BoolState[EnableBGM]);
  Result:=true;
end;
{$ENDIF}

end.
