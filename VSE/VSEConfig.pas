unit VSEConfig;

interface

const
  VSECaptVer='VgaSoft Engine 0.1';

var
  InitStates: procedure;
  DoAutoexec: Boolean=true;
  Caption: string='VgaSoft Engine';
  Version: string='0.1';
  CaptionVer: string;
  BaseDir: string='Data';
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

end.
