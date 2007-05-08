program Assault;

{$R *.res}

uses
  {FastMM4,} Windows, VSEInit, VSEMain, States, UPakMan;

begin
  VSEInit.InitStates:=States.InitStates;
  Caption:='Assault';
  Version:='0.1';
  FontsList:='Fonts.ini';
  repeat
    NeedRestart:=false;
    ShowCursor(false);
    VSEStart;
    ShowCursor(true);
  until NeedRestart=false;
end.
