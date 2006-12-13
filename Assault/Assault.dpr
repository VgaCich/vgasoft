program Assault;

{$R *.res}

uses
  SysSfIni,{ FastMM4,} VSEConfig, VSEMain, States;

begin
  VSEConfig.InitStates:=States.InitStates;
  Caption:='Assault';
  Version:='0.1';
  CaptionVer:=Caption+' '+Version;
  VSEStart;
end.
