unit States;

interface

uses
  UGame, ULog, GameStates, StateIntro, StateMenu, StateGame, StateLoad, StateConsole;

procedure InitStates;

implementation

procedure InitStates;
var
  Intro: Cardinal;
begin
  Log(llInfo, 'Init states');
  Intro:=Game.AddState(TStateIntro.Create);
  Game.AddState(TStateMenu.Create);
  Game.AddState(TStateLoad.Create);
  Game.AddState(TStateGame.Create);
  Game.AddState(TStateConsole.Create);
  Game.SwitchState(Intro);
end;

end.
