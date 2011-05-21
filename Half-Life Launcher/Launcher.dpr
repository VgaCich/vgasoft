program Launcher;

uses
  Windows, AvL, avlUtils, OneInstance, MForm;

{$R manifest.res}
{$R Images.res}

var
  PrevInstWnd: THandle;

begin
  InitCommonControls;
  if IsRunning then Exit;
  MainForm:=TMainForm.Create;
  MainForm.Run;
end.