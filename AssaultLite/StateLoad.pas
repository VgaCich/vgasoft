unit StateLoad;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, avlVectors,
  VSEGameStates, SynTex, SynTexFilters, VSEMemPak, VSETexMan, StateGame;

type
  TLoadStage=procedure of object;
  TStateLoad=class(TGameState)
  protected
    FFont: Cardinal;
    FGame: TStateGame;
    FLoadStage: TLoadStage;
    FLevelName: string;
    function GetName: string; override;
    procedure LoadTerrain;
    procedure StartGame;
    procedure STStore(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string);
  public
    constructor Create;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
    procedure SysNotify(Notify: TSysNotify); override;
    property LevelName: string read FLevelName write FLevelName;
  end;

implementation

uses VSECore {$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  SLoad='Загрузка уровня %s...';
  STitle='Assault Lite';

constructor TStateLoad.Create;
begin
  inherited Create;
  FFont:=TexMan.FontCreate('Arial', 20, true);
  FGame:=TStateGame(Core.GetState(Core.FindState('Game')));
end;

procedure TStateLoad.Draw;
var
  S: string;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  S:=Format(SLoad, [FLevelName]);
  TexMan.TextOut(FFont, 400-TexMan.TextLen(FFont, S)/2, 500, S);
  TexMan.TextOut(FFont, 400-TexMan.TextLen(FFont, STitle)/2, 250, STitle);
  if not Assigned(FLoadStage) then FLoadStage:=LoadTerrain;
end;

procedure TStateLoad.Update;
begin
  if Assigned(FLoadStage) then FLoadStage;
end;

function TStateLoad.Activate: Cardinal;
begin
  FLoadStage:=nil;
  Result:=50;
end;

procedure TStateLoad.Deactivate;
begin

end;

procedure TStateLoad.SysNotify(Notify: TSysNotify);
begin
  if Notify<>snUpdateOverload
    then inherited SysNotify(Notify);
end;

function TStateLoad.GetName: string;
begin
  Result:='Load';
end;

procedure TStateLoad.LoadTerrain;
var
  STCode: TStream;
  ST: TSynTex;
  STF: TSynTexFilters;
begin
  STCode:=GetFile(FLevelName+'.stc');
  ST:=nil;
  STF:=nil;
  if STCode=nil then
  begin
    {$IFDEF VSE_LOG}LogF(llError, 'Level %s textures synthesizing code not found', [FLevelName]);{$ENDIF}
    Core.StopEngine;
    Exit;
  end;
  try
    ST:=TSynTex.Create(512);
    STF:=TSynTexFilters.Create(ST);
    ST.Code:=STCode;
    ST.OnStore:=STStore;
    if not ST.Synthesize then
    begin
      {$IFDEF VSE_LOG}LogF(llError, 'Level %s textures synthesizing failed', [FLevelName]);{$ENDIF}
      Core.StopEngine;
      Exit;
    end;
  finally
    FAN(STF);
    FAN(ST);
    FAN(STCode);
  end;
  FGame.Terrain.Texture:=TexMan.GetTex('Grass');
  FGame.NewGame;
  FLoadStage:=StartGame;
end;

procedure TStateLoad.StartGame;
begin
  Core.SwitchState('Game');
  FLoadStage:=nil;
end;

procedure TStateLoad.STStore(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string);
begin
  if Name='Terrain'
    then FGame.Terrain.Load(Reg, TexSize, 1, 1/4);
end;

end.
