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
  SUseCache='�������� ������������� ����?'#13#10+
            '��� ������� �������� ���� ��� ��������� �������, �� ���������'#13#10+
            '��������� �������� �������� ����� �� ����� ������� �����';
  SUseFullscreen='��������� ���� � ������������� ������?';
  Bindings: array[0..5] of TBinding = (
    (Name: 'Fwd'; Description: '������'; Key: Ord('W')),
    (Name: 'Bwd'; Description: '�����'; Key: Ord('S')),
    (Name: 'SLeft'; Description: '�����'; Key: Ord('A')),
    (Name: 'SRight'; Description: '������'; Key: Ord('D')),
    (Name: 'SpdUp'; Description: '�������'; Key: VK_SHIFT),
    (Name: 'SpdDn'; Description: '���������'; Key: VK_CONTROL));

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
