program IPL;

uses
  Windows, AvL, avlUtils;

{$R *.res}
var
  Key, SubKey: HKey;
  List, Keys: TStringList;
  S: string;
  i: integer;

procedure Fin;
begin
  if SubKey<>0 then RegKeyClose(SubKey);
  if Key<>0 then RegKeyClose(Key);
  if Assigned(List) then List.SaveToFile(ExePath+'IPL'+DateToStr(Now)+'.txt');
  FAN(List);
  FAN(Keys);
  Halt;
end;

procedure Err(Error: string);
begin
  MessageBox(0, PAnsiChar('Error: '+Error), 'IPL', MB_ICONERROR or MB_OK);
  List.Clear;
  List.Add('Error: '+Error);
  Fin;
end;

begin
  try
    List:=TStringList.Create;
    Keys:=TStringList.Create;
    List.Clear;
    Key:=RegKeyOpenRead(HKEY_LOCAL_MACHINE, 'software\microsoft\windows\currentversion\uninstall');
    if Key=0 then Err('Unable to open key HKLM\Software\Microsoft\Windows\CurrentVersion\Uninstall');
    RegKeyGetKeyNamesStr(Key, S);
    Keys.Text:=S;
    for i:=0 to Keys.Count-1 do
    begin
      SubKey:=RegKeyOpenRead(Key, Keys[i]);
      if SubKey=0 then
      begin
        List.Add('Error: Unable to open key HKLM\Software\Microsoft\Windows\CurrentVersion\Uninstall\'+Keys[i]);
        Continue;
      end;
      if RegKeyValExists(SubKey, 'DisplayName')
        then List.Add(RegKeyGetStr(SubKey, 'DisplayName'));
      RegKeyClose(SubKey);
    end;
  finally
    List.Sort;
    List.Insert(0, DateToStr(Now)+' '+TimeToStr(Now));
    List.Insert(1, 'List of installed programs:');
    List.Insert(2, '');
    Fin;
  end
end.
