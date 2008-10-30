//Must be first unit in projects, except of memory manager etc
unit VSEInit;

interface

uses AvL, avlUtils{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TInitStates=procedure;

const
  VSECaptVer='VgaSoft Engine 0.1';

var //Start engine settings
  InitStates: TInitStates; //Init states procedure pointer
  Caption: string='VgaSoft Engine'; //Engine window caption
  Version: string='0.1'; //Application version
  CaptionVer: string; //Application name with version; default: Caption+' '+Version
  ResX: Integer=640; //Horizontal resolution
  ResY: Integer=480; //Vertical resolution
  Refresh: Integer=0; //Screen refresh rate, fullscreen only
  Depth: Integer=32; //Color depth
  Fullscreen: Boolean=false; //Fullscreen mode
  VSync: Integer=1; //Vertical synchronization
  //SoundDevice: string='default';

procedure LoadINI(const FileName: string); //Load settings from ini file
procedure SaveINI(const FileName: string); //Save settings to ini file

implementation

procedure LoadINI(const FileName: string);
var
  INI: TIniFile;
begin
  INI:=TIniFile.Create(FileName);
  try
    ResX:=INI.ReadInteger('Settings', 'ResX', ResX);
    ResY:=INI.ReadInteger('Settings', 'ResY', ResY);
    Refresh:=INI.ReadInteger('Settings', 'Refresh', Refresh);
    Depth:=INI.ReadInteger('Settings', 'Depth', Depth);
    Fullscreen:=INI.ReadBool('Settings', 'Fullscreen', Fullscreen);
    VSync:=INI.ReadInteger('Settings', 'VSync', VSync);
    //SoundDevice:=INI.ReadString('Settings', 'SoundDevice', SoundDevice);
  finally
    FAN(INI);
  end;
end;

procedure SaveINI(const FileName: string);
var
  INI: TIniFile;
begin
  INI:=TIniFile.Create(FileName);
  try
    INI.WriteInteger('Settings', 'ResX', ResX);
    INI.WriteInteger('Settings', 'ResY', ResY);
    INI.WriteInteger('Settings', 'Refresh', Refresh);
    INI.WriteInteger('Settings', 'Depth', Depth);
    INI.WriteBool('Settings', 'Fullscreen', Fullscreen);
    INI.WriteInteger('Settings', 'VSync', VSync);
    //INI.WriteString('Settings', 'SoundDevice', SoundDevice);
  finally
    FAN(INI);
  end;
end;

end.
