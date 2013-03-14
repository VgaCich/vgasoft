program WallChng;

uses
  Windows,
  Forms,
  SysUtils,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

var
  F: Text;

begin
  Randomize;
  ListFile:=ExtractFilePath(Application.ExeName)+'\list.dat';
  if not FileExists(ListFile) then
  begin
    AssignFile(F, ListFile);
    Rewrite(F);
    WriteLn(F, '0');
    WriteLn(F, '0');
    CloseFile(F);
  end;
  if ParamCount>0 then
  begin
    if GetKeyState(Ord('N'))>=0 then ChangeWallpaper;
    Exit;
  end;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
