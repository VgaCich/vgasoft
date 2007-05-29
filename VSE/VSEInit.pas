//Must be first unit in projects, except of memory manager etc
unit VSEInit;

interface

uses AvL, avlUtils, VSELogFile, VSEManagers, VSEPakMan, VSEConsole, VSEConsoleVariables;

type
  TInitStates=procedure;

const
  VSECaptVer='VgaSoft Engine 0.1';

var
  InitStates: TInitStates;
  DoAutoexec: Boolean=true;
  Caption: string='VgaSoft Engine';
  Version: string='0.1';
  CaptionVer: string;
  BaseDir: string;
  ResX: Integer=640;
  ResY: Integer=480;
  Refresh: Integer=0;
  Depth: Integer=16;
  Fullscreen: Boolean=false;
  VSync: Integer=1;
  SoundDevice: string='default';
  FontsList: string='';

procedure LoadINI(const FileName: string);
procedure SaveINI(const FileName: string);

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
    SoundDevice:=INI.ReadString('Settings', 'SoundDevice', SoundDevice);
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
    INI.WriteString('Settings', 'SoundDevice', SoundDevice);
  finally
    FAN(INI);
  end;
end;

initialization
  if BaseDir='' then BaseDir:=ExePath+'Data';

end.
