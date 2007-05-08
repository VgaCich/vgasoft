//Must be first unit in projects, except of memory manager etc
unit VSEInit;

interface

uses AvL, avlUtils, ULogFile, UManagers, UPakMan, UConsole, UConsoleVariables;

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
  BaseDir, IniFileName: string;
  UseINI: Boolean=true;
  ResX: Integer=640;
  ResY: Integer=480;
  Refresh: Integer=0;
  Depth: Integer=16;
  Fullscreen: Boolean=false;
  VSync: Integer=1;
  SoundDevice: string='default';
  FontsList: string='';     

implementation

initialization
  if BaseDir='' then BaseDir:=ExePath+'Data';
  if IniFileName='' then IniFileName:=ChangeFileExt(FullExeName, '.ini');

end.
