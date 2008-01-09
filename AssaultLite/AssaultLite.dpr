program AssaultLite;

uses
  {$IFDEF VER_150}SysSfIni, {$ENDIF}{$IFDEF DEBUGMEM}FastMM4,{$ENDIF} Windows,
  AvL, avlUtils, VSEInit, VSECore, UTexMan, StateStart, StateMenu, StateGame,
  StateLoad;

procedure InitStates;
begin
  Core.AddState(TStateGame.Create);
  Core.AddState(TStateLoad.Create);
  Core.AddState(TStateMenu.Create);
  Core.SwitchState(Core.AddState(TStateStart.Create));
end;

const
  SUseCache='�������� ������������� ����?'#13#10+
            '��� ������� �������� ���� ��� �������� �������, �� ���������'#13#10+
            '��������� �������� �������� ����� �� ����� ������� �����';
  SUseFullscreen='��������� ���� � ������������� ������?';

var
  Ini: string;

begin
  VSEInit.InitStates:=InitStates;
  Caption:='Assault Lite';
  Version:='0.1';
  Ini:=ChangeFileExt(FullExeName, '.ini');
  ResX:=800;
  ResY:=600;
  CacheDir:=ExePath+'Cache\';
  if not FileExists(Ini) then
  begin
    UTexMan.UseCache:=MessageDlg(SUseCache, Caption, MB_ICONQUESTION or MB_YESNO)=ID_YES;
    Fullscreen:=MessageDlg(SUseFullscreen, Caption, MB_ICONQUESTION or MB_YESNO)=ID_YES;
    if Fullscreen then
    begin
      ResX:=Screen.Width;
      ResY:=Screen.Height;
    end;
  end
  else begin
    LoadINI(Ini);
    UTexMan.UseCache:=DirectoryExists(CacheDir);
  end;
  VSEStart;
  SaveINI(Ini);
  Ini:='';
end.
