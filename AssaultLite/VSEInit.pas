//Must be first unit in project, except of memory manager etc
unit VSEInit;

interface

uses AvL, avlUtils{$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  SSectionSettings = 'Settings';
  SNameColorDepth = 'ColorDepth';
  SNameFullscreen = 'Fullscreen';
  SNameRefreshRate = 'RefreshRate';
  SNameResolutionY = 'ResolutionY';
  SNameResolutionX = 'ResolutionX';
  SNameVSync = 'VSync';

type
  TInitStates=procedure;
  TBinding=record
    Name, Description: string;
    Key: Byte;
  end;
  TInitSettings=record //Engine settings
    InitStates: TInitStates; //Init states procedure pointer
    Caption: string; //Engine window caption
    Version: string; //Application version
    ResolutionX: Integer; //Horizontal resolution
    ResolutionY: Integer; //Vertical resolution
    RefreshRate: Integer; //Screen refresh rate, fullscreen only
    ColorDepth: Integer; //Color depth
    Fullscreen: Boolean; //Fullscreen mode
    VSync: Boolean; //Vertical synchronization
    Bindings: array of TBinding;
  end;
  TSettings=class //Interface to engine's ini file
  private
    FFirstRun: Boolean;
    FIni: TIniFile;
    function GetBool(const Section, Name: string): Boolean;
    function GetInt(const Section, Name: string): Integer;
    function GetStr(const Section, Name: string): string;
    procedure SetBool(const Section, Name: string; const Value: Boolean);
    procedure SetInt(const Section, Name: string; const Value: Integer);
    procedure SetStr(const Section, Name: string; const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function ReadSection(const Section: string): TStringList; //Read section contents to TStringList
    procedure EraseSection(const Section: string); //Erase section
    property FirstRun: Boolean read FFirstRun; //True if ini file wasn't exist at time of engine's start
    property Bool[const Section, Name: string]: Boolean read GetBool write SetBool; //Read/write Boolean value
    property Int[const Section, Name: string]: Integer read GetInt write SetInt; //Read/write Integer value
    property Str[const Section, Name: string]: string read GetStr write SetStr; //Read/write String value
  end;

const
  VSECaptVer='VgaSoft Engine 0.1';

var
  InitSettings: TInitSettings = (
    InitStates: nil;
    Caption: '';
    Version: '';
    ResolutionX: 640;
    ResolutionY: 480;
    RefreshRate: 0;
    ColorDepth: 32;
    Fullscreen: false;
    VSync: true;
    Bindings: nil);
  Settings: TSettings;

procedure SetBindings(Bindings: array of TBinding); //Set BindMan configuration

implementation

procedure SetBindings(Bindings: array of TBinding);
var
  i: Integer;
begin
  SetLength(InitSettings.Bindings, Length(Bindings));
  for i:=0 to Length(Bindings)-1 do
    InitSettings.Bindings[i]:=Bindings[i];
end;

constructor TSettings.Create;
var
  IniName: string;
begin
  inherited;
  IniName:=ChangeFileExt(FullExeName, '.ini');
  FFirstRun:=not FileExists(IniName);
  FIni:=TIniFile.Create(IniName);
  if not FFirstRun then
    with InitSettings do
    begin
      {$IFDEF VSE_LOG}Log(llInfo, 'Loading settings from ini file');{$ENDIF}
      ResolutionX:=FIni.ReadInteger(SSectionSettings, SNameResolutionX, ResolutionX);
      ResolutionY:=FIni.ReadInteger(SSectionSettings, SNameResolutionY, ResolutionY);
      RefreshRate:=FIni.ReadInteger(SSectionSettings, SNameRefreshRate, RefreshRate);
      ColorDepth:=FIni.ReadInteger(SSectionSettings, SNameColorDepth, ColorDepth);
      Fullscreen:=FIni.ReadBool(SSectionSettings, SNameFullscreen, Fullscreen);
      VSync:=FIni.ReadBool(SSectionSettings, SNameVSync, VSync);
    end;
end;

destructor TSettings.Destroy;
begin
  FAN(FIni);
  inherited;
end;

procedure TSettings.EraseSection(const Section: string);
begin
  FIni.EraseSection(Section);
end;

function TSettings.GetBool(const Section, Name: string): Boolean;
begin
  Result:=FIni.ReadBool(Section, Name, false);
end;

function TSettings.GetInt(const Section, Name: string): Integer;
begin
  Result:=FIni.ReadInteger(Section, Name, 0);
end;

function TSettings.GetStr(const Section, Name: string): string;
begin
  Result:=FIni.ReadString(Section, Name, '');
end;

function TSettings.ReadSection(const Section: string): TStringList;
begin
  Result:=TStringList.Create;
  FIni.ReadSectionValues(Section, Result);
end;

procedure TSettings.SetBool(const Section, Name: string; const Value: Boolean);
begin
  FIni.WriteBool(Section, Name, Value);
end;

procedure TSettings.SetInt(const Section, Name: string; const Value: Integer);
begin
  FIni.WriteInteger(Section, Name, Value);
end;

procedure TSettings.SetStr(const Section, Name: string; const Value: string);
begin
  FIni.WriteString(Section, Name, Value);
end;

initialization
  Settings:=TSettings.Create;

finalization
  FAN(Settings);

end.
