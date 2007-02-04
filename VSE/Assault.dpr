program Assault;

{$R *.res}

uses
  {FastMM4,} VSEConfig, VSEMain, States;

begin
  VSEConfig.InitStates:=States.InitStates;
  Caption:='Assault';
  Version:='0.1';
  VSEStart;
end.
