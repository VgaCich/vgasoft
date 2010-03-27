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
  Bindings: string='';

procedure LoadINI; //Load settings from ini file
procedure SaveINI; //Save settings to ini file
function GetINI: TIniFile; //Return TINiFile for save/read user settings
function CheckINI: Boolean; //Return true if ini file exists

implementation

const
  SBind = 'Bind';
  SSettings = 'Settings';

procedure LoadINI;
var
  INI: TIniFile;
  Bind: TStringList;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Loading settings from INI file');{$ENDIF}
  INI:=GetINI;
  try
    ResX:=INI.ReadInteger(SSettings, 'ResX', ResX);
    ResY:=INI.ReadInteger(SSettings, 'ResY', ResY);
    Refresh:=INI.ReadInteger(SSettings, 'Refresh', Refresh);
    Depth:=INI.ReadInteger(SSettings, 'Depth', Depth);
    Fullscreen:=INI.ReadBool(SSettings, 'Fullscreen', Fullscreen);
    VSync:=INI.ReadInteger(SSettings, 'VSync', VSync);
    //SoundDevice:=INI.ReadString(SSettings, 'SoundDevice', SoundDevice);
    if INI.SectionExists(SBind) then
    begin
      Bind:=TStringList.Create;
      try
        INI.ReadSectionValues(SBind, Bind);
        Bindings:=Bind.Text;
      finally
        FAN(Bind);
      end;
    end;
  finally
    FAN(INI);
  end;
end;

procedure SaveINI;
var
  INI: TIniFile;
  Bind: TStringList;
  i: Integer;
  Name: string;
begin
  {$IFDEF VSE_LOG}Log(llInfo, 'Saving settings to INI file');{$ENDIF}
  INI:=GetINI;
  try
    INI.WriteInteger(SSettings, 'ResX', ResX);
    INI.WriteInteger(SSettings, 'ResY', ResY);
    INI.WriteInteger(SSettings, 'Refresh', Refresh);
    INI.WriteInteger(SSettings, 'Depth', Depth);
    INI.WriteBool(SSettings, 'Fullscreen', Fullscreen);
    INI.WriteInteger(SSettings, 'VSync', VSync);
    //INI.WriteString(SSettings, 'SoundDevice', SoundDevice);
    if Bindings<>'' then
    begin
      Bind:=TStringList.Create;
      try
        Bind.Text:=Bindings;
        for i:=0 to Bind.Count-1 do
        begin
          Name:=Copy(Bind[i], 1, FirstDelimiter('=', Bind[i])-1);
          INI.WriteString(SBind, Name, Bind.Values[Name]);
        end;
      finally
        FAN(Bind);
      end;
    end;
  finally
    FAN(INI);
  end;
end;

function GetINI: TIniFile;
begin
  Result:=TIniFile.Create(ChangeFileExt(FullExeName, '.ini'));
end;

function CheckINI: Boolean;
begin
  Result:=FileExists(ChangeFileExt(FullExeName, '.ini'));
end;

end.
