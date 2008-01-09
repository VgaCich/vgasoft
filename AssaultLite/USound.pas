unit USound;

interface

uses
  Windows, AvL, avlUtils;

{$I dsound.inc}

type
  TSound=class
  private
    FDirectSound: IDirectSound;
    FMusicBuffer: IDirectSoundBuffer;
    FMusicFile: TCustomMemoryStream;
    FMusicPCM: TWaveFormatEx;
    FMusicBufferDesc: TDSBufferDesc;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PlayMusic(const FileName: string);
    procedure StopMusic;
  end;

var
  Sound: TSound;

implementation

uses
  VSECore, MemPak {$IFDEF VSE_LOG}, VSELog{$ENDIF};

{uFMOD}

const
	XM_MEMORY=1;
	uFMOD_BUFFER_SIZE=262144;
  uFMOD_MixRate = 44100;

{$L dsufmod.obj}
function uFMOD_DSPlaySong(lpXM: Pointer; param, fdwSong: Integer;
  lpDSBuffer: IDirectSoundBuffer): Integer; stdcall; external;

{TSound}

constructor TSound.Create;
begin
  inherited Create;
  {$IFDEF VSE_LOG}Log(llInfo, 'Sound: Create');{$ENDIF}
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
	if FDirectSound.CreateSoundBuffer(@FMusicBufferDesc, FMusicBuffer, nil)<>S_OK then
	begin
    {$IFDEF VSE_LOG}Log(llError, 'Sound: Cannot create secondary buffer');{$ENDIF}
    FMusicBuffer:=nil;
  end;
end;

destructor TSound.Destroy;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Sound: Destroy');{$ENDIF}
  StopMusic;
  FMusicBuffer:=nil;
  FDirectSound:=nil;
  inherited Destroy;
end;

procedure TSound.PlayMusic(const FileName: string);
begin
  if not Assigned(FMusicBuffer) then Exit;
  if Assigned(FMusicFile) then StopMusic;
  FMusicFile:=GetFile(FileName);
  if not Assigned(FMusicFile) then Exit;
	uFMOD_DSPlaySong(FMusicFile.Memory, FMusicFile.Size, XM_MEMORY, FMusicBuffer);
end;

procedure TSound.StopMusic;
begin
  uFMOD_DSPlaySong(nil, 0, 0, nil);
  FAN(FMusicFile);
end;

end.
