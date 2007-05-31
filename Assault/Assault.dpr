program Assault;

{$R *.res}

uses
  LeakDetect, Windows, avlUtils, VSEInit, VSECore, States;

begin
  VSEInit.InitStates:=States.InitStates;
  Caption:='Assault';
  Version:='0.1';
  FontsList:='Fonts.ini';
  LoadINI(ChangeFileExt(FullExeName, '.ini'));
  repeat
    NeedRestart:=false;
    ShowCursor(false);
    VSEStart;
    ShowCursor(true);
  until NeedRestart=false;
  SaveINI(ChangeFileExt(FullExeName, '.ini'));
end.
