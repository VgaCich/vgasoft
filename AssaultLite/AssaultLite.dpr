program AssaultLite;

uses
  {$IFDEF VER150}SysSfIni, {$ENDIF}{$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows,
  AvL, avlUtils, VSEInit, {$IFDEF VSE_LOG}VSELog, {$ENDIF}VSECore, VSETexMan,
  StateStart, StateMenu, StateGame, StateLoad;

procedure InitStates;
begin
  Core.AddState(TStateGame.Create);
  Core.AddState(TStateLoad.Create);
  Core.AddState(TStateMenu.Create);
  Core.SwitchState(Core.AddState(TStateStart.Create));
end;

const
  SUseCache='Включить использование кэша?'#13#10+
            'Это ускорит загрузку игры при следующем запуске, но потребует'#13#10+
            'несколько десятков мегабайт места на вашем жестком диске';
  SUseFullscreen='Запустить игру в полноэкранном режиме?';
  Bindings: array[0..5] of TBinding = (
    (Name: 'Fwd'; Description: 'Вперед'; Key: Ord('W')),
    (Name: 'Bwd'; Description: 'Назад'; Key: Ord('S')),
    (Name: 'SLeft'; Description: 'Влево'; Key: Ord('A')),
    (Name: 'SRight'; Description: 'Вправо'; Key: Ord('D')),
    (Name: 'SpdUp'; Description: 'Быстрее'; Key: VK_SHIFT),
    (Name: 'SpdDn'; Description: 'Медленнее'; Key: VK_CONTROL));

begin
  InitSettings.InitStates:=InitStates;
  InitSettings.Caption:='Assault Lite';
  InitSettings.Version:='0.1';
  SetBindings(Bindings);
  CacheDir:=ExePath+'Cache\';
  if Settings.FirstRun then
  begin
    VSETexMan.UseCache:=MessageDlg(SUseCache, InitSettings.Caption, MB_ICONQUESTION or MB_YESNO)=ID_YES;
    InitSettings.Fullscreen:=MessageDlg(SUseFullscreen, InitSettings.Caption, MB_ICONQUESTION or MB_YESNO)=ID_YES;
    if InitSettings.Fullscreen then
    begin
      InitSettings.ResolutionX:=Screen.Width;
      InitSettings.ResolutionY:=Screen.Height;
    end
    else begin
      InitSettings.ResolutionX:=800;
      InitSettings.ResolutionY:=600;
    end;
  end
    else VSETexMan.UseCache:=DirectoryExists(CacheDir);
  VSEStart;
end.
