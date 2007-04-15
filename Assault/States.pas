unit States;

interface

uses
  UCore, ULog, GameStates, StateIntro, StateMenu, StateGame, StateLoad, StateConsole;

procedure InitStates;

var
  NeedRestart: Boolean=false;

implementation

procedure InitStates;
var
  Intro: Cardinal;
begin
  Intro:=Core.AddState(TStateIntro.Create);
  Core.AddState(TStateMenu.Create);
  Core.AddState(TStateLoad.Create);
  Core.AddState(TStateGame.Create);
  Core.AddState(TStateConsole.Create);
  Core.SwitchState(Intro);
end;

end.
