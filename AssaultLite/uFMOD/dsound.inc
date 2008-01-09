type
	IDirectSound = interface;
	IDirectSoundBuffer = interface;

	PTWaveFormatEx = ^TWaveFormatEx;
	TWaveFormatEx = packed record
		wFormatTag: Word;
		nChannels: Word;
		nSamplesPerSec: Integer;
		nAvgBytesPerSec: Integer;
		nBlockAlign: Word;
		wBitsPerSample: Word;
		cbSize: Integer;
	end;

	PTDSBufferDesc = ^TDSBufferDesc;
	TDSBufferDesc = packed record
		dwSize: Integer;
		dwFlags: Integer;
		dwBufferBytes: Integer;
		dwReserved: Integer;
		lpwfxFormat: PTWaveFormatEx;
	end;

	PTDSCaps = ^TDSCaps;
	TDSCaps = packed record
		dwSize: Integer;
		dwFlags: Integer;
		dwMinSecondarySampleRate: Integer;
		dwMaxSecondarySampleRate: Integer;
		dwPrimaryBuffers: Integer;
		dwMaxHwMixingAllBuffers: Integer;
		dwMaxHwMixingStaticBuffers: Integer;
		dwMaxHwMixingStreamingBuffers: Integer;
		dwFreeHwMixingAllBuffers: Integer;
		dwFreeHwMixingStaticBuffers: Integer;
		dwFreeHwMixingStreamingBuffers: Integer;
		dwMaxHw3DAllBuffers: Integer;
		dwMaxHw3DStaticBuffers: Integer;
		dwMaxHw3DStreamingBuffers: Integer;
		dwFreeHw3DAllBuffers: Integer;
		dwFreeHw3DStaticBuffers: Integer;
		dwFreeHw3DStreamingBuffers: Integer;
		dwTotalHwMemBytes: Integer;
		dwFreeHwMemBytes: Integer;
		dwMaxContigFreeHwMemBytes: Integer;
		dwUnlockTransferRateHwBuffers: Integer;
		dwPlayCpuOverheadSwBuffers: Integer;
		dwReserved1: Integer;
		dwReserved2: Integer;
	end;

	PTDSBCaps = ^TDSBCaps;
	TDSBCaps = packed record
		dwSize: Integer;
		dwFlags: Integer;
		dwBufferBytes: Integer;
		dwUnlockTransferRate: Integer;
		dwPlayCpuOverhead: Integer;
	end;

	IDirectSound = interface (IUnknown)
		['{279AFA83-4981-11CE-A521-0020AF0BE560}']
		function CreateSoundBuffer(lpDSBufferDesc: PTDSBufferDesc;
			out lpIDirectSoundBuffer: IDirectSoundBuffer;
			pUnkOuter: Pointer): Integer; stdcall;
		function GetCaps(var lpDSCaps: PTDSCaps): Integer; stdcall;
		function DuplicateSoundBuffer(lpDsbOriginal: IDirectSoundBuffer;
			out lpDsbDuplicate: IDirectSoundBuffer): Integer; stdcall;
		function SetCooperativeLevel(hwnd: Integer; dwLevel: Integer): Integer; stdcall;
		function Compact: Integer; stdcall;
		function GetSpeakerConfig(var lpdwSpeakerConfig: Integer): Integer; stdcall;
		function SetSpeakerConfig(dwSpeakerConfig: Integer): Integer; stdcall;
		function Initialize(lpGuid: Pointer): Integer; stdcall;
	end;

	IDirectSoundBuffer = interface (IUnknown)
		['{279AFA85-4981-11CE-A521-0020AF0BE560}']
		function GetCaps(var lpDSCaps: TDSBCaps): Integer; stdcall;
		function GetCurrentPosition
			(lpdwCapturePosition, lpdwReadPosition : Pointer): Integer; stdcall;
		function GetFormat(lpwfxFormat: PTWaveFormatEx; dwSizeAllocated: Integer;
			lpdwSizeWritten: Pointer): Integer; stdcall;
		function GetVolume(var lplVolume: Integer): Integer; stdcall;
		function GetPan(var lplPan: Integer): Integer; stdcall;
		function GetFrequency(var lpdwFrequency: Integer): Integer; stdcall;
		function GetStatus(var lpdwStatus: Integer): Integer; stdcall;
		function Initialize(lpDirectSound: IDirectSound;
			const lpcDSBufferDesc: TDSBufferDesc): Integer; stdcall;
		function Lock(dwWriteCursor, dwWriteBytes: Integer;
			var lplpvAudioPtr1: Pointer; var lpdwAudioBytes1: Integer;
			var lplpvAudioPtr2: Pointer; var lpdwAudioBytes2: Integer;
			dwFlags: Integer): Integer; stdcall;
		function Play(dwReserved1,dwReserved2,dwFlags: Integer): Integer; stdcall;
		function SetCurrentPosition(dwPosition: Integer): Integer; stdcall;
		function SetFormat(lpcfxFormat: PTWaveFormatEx): Integer; stdcall;
		function SetVolume(lVolume: Integer): Integer; stdcall;
		function SetPan(lPan: Integer): Integer; stdcall;
		function SetFrequency(dwFrequency: Integer): Integer; stdcall;
		function Stop: Integer; stdcall;
		function Unlock(lpvAudioPtr1: Pointer; dwAudioBytes1: Integer;
			lpvAudioPtr2: Pointer; dwAudioBytes2: Integer): Integer; stdcall;
		function Restore: Integer; stdcall;
	end;

const
	DSSCL_NORMAL = 1;
	DSSCL_PRIORITY = 2;
	DSSCL_EXCLUSIVE = 3;
	DSSCL_WRITEPRIMARY = 4;
	TDSBufferDesc_size = 20;
	DSBCAPS_PRIMARYBUFFER = 1;
	DSBCAPS_STATIC = 2;
	DSBCAPS_LOCHARDWARE = 4;
	DSBCAPS_LOCSOFTWARE = 8;
	DSBCAPS_CTRL3D = $10;
	DSBCAPS_CTRLFREQUENCY = $20;
	DSBCAPS_CTRLPAN = $40;
	DSBCAPS_CTRLVOLUME = $80;
	DSBCAPS_CTRLPOSITIONNOTIFY = $100;
	DSBCAPS_STICKYFOCUS = $4000;
	DSBCAPS_GLOBALFOCUS = $8000;
	DSBCAPS_GETCURRENTPOSITION2 = $10000;
	DSBCAPS_MUTE3DATMAXDISTANCE = $20000;
	DSBCAPS_LOCDEFER = $40000;
	WAVE_FORMAT_PCM = 1;

function DirectSoundCreate(
	lpGuid: Pointer;
	out ppDS: IDirectSound;
	pUnkOuter: Pointer):
	Integer; stdcall; external 'dsound.dll' index 1;