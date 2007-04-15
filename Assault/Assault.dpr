program Assault;

{$R *.res}

uses
  {FastMM4,} VSEInit, VSEMain, States;

begin
  VSEInit.InitStates:=States.InitStates;
  Caption:='Assault';
  Version:='0.1';
  FontsList:='Fonts.ini';
  repeat
    NeedRestart:=false;
    VSEStart;
  until NeedRestart=false;
end.
