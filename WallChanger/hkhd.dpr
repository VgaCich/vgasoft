program hkhd;

uses
  Windows, Messages;

var
  hWnd: THandle;
  UpdateInterval: Cardinal = 0;
  UpdateLock: Cardinal = 0;
  KeyDef: Word;
  KeyChg: Word;
  KeyDefID, KeyChgID: Word;
  DefWP: string;
  ExePath: string;

function ExtractFilePath(const FileName: string): string;
var
  I: Integer;
begin
  I := Length(FileName);
  while (I > 1) and not (FileName[I] in ['\', ':']) do Dec(I);
  if FileName[I] in ['\', ':']
    then Result := Copy(FileName, 1, I)
    else Result:='';
end;

function FileExists(const FileName: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(FileName));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code = 0);
end;

function RegKeyOpenWrite( Key: HKey; const SubKey: String ): HKey;
begin
  if RegOpenKeyEx(Key, PChar(SubKey), 0, KEY_READ or KEY_WRITE, Result) <> ERROR_SUCCESS then Result := 0;
end;

function RegKeySetStr( Key: HKey; const ValueName: String; const Value: String ): Boolean;
begin
  Result := (Key <> 0) and (RegSetValueEx( Key, PChar( ValueName ), 0, REG_SZ, PChar(Value),
             Length( Value ) + 1 ) = ERROR_SUCCESS);
end;

procedure SetHotKeys;
begin
  if KeyDef<>0 then RegisterHotKey(hWnd, KeyDefID, Hi(KeyDef), Lo(KeyDef));
  if KeyChg<>0 then RegisterHotKey(hWnd, KeyChgID, Hi(KeyChg), Lo(KeyChg));
end;

procedure RemoveHotKeys;
begin
  UnregisterHotKey(hWnd, KeyDefID);
  UnregisterHotKey(hWnd, KeyChgID);
end;

procedure LoadSettings;
var
  F: TextFile;
begin
  if not FileExists(ExePath+'hkhd.dat') then
  begin
    PostMessage(hWnd, WM_DESTROY, 0, 0);
    Exit;
  end;
  AssignFile(F, ExePath+'hkhd.dat');
  try
    Reset(F);
    ReadLn(F, UpdateInterval);
    ReadLn(F, KeyDef);
    ReadLn(F, KeyChg);
    ReadLn(F, DefWP);
  finally
    CloseFile(F);
  end;
end;

procedure SetWallpaper(WP: string);
var
  Index: string;
  Key: HKey;
begin
  if Length(WP)<2 then Exit;
  Index:=WP[1];
  WP:=Copy(WP, 2, MaxInt);
  Key:=RegKeyOpenWrite(HKEY_CURRENT_USER, 'Control Panel\Desktop');
  try
    RegKeySetStr(Key, 'Wallpaper', WP);
    RegKeySetStr(Key, 'TileWallpaper', Index);
    RegKeySetStr(Key, 'WallpaperStyle', Index);
  finally
    RegCloseKey(Key);
  end;
  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE);
end;

procedure ChangeWallpaper;
var
  F: TextFile;
  Wallpapers: array of string;
  Method, Index, OldIndex: Integer;
begin
  if not FileExists(ExePath+'list.dat') then Exit;
  AssignFile(F, ExePath+'list.dat');
  try
    Reset(F);
    ReadLn(F, Method);
    ReadLn(F, OldIndex);
    Index:=0;
    while not EOF(F) do
    begin
      if Index>High(Wallpapers)
        then SetLength(Wallpapers, Length(Wallpapers)+50);
      ReadLn(F, Wallpapers[Index]);
      Inc(Index);
    end;
    SetLength(Wallpapers, Index);
  finally
    CloseFile(F);
  end;
  case Method of
    0: begin
         repeat
           Index:=Random(Length(Wallpapers));
         until (Index<>OldIndex) or (Length(Wallpapers)<2);
       end;
    1: if OldIndex=High(Wallpapers)
         then Index:=0
         else Index:=OldIndex+1;
    2: if OldIndex=0
         then Index:=High(Wallpapers)
         else Index:=OldIndex-1;
    else Exit;
  end;
  SetWallpaper(Wallpapers[Index]);
  AssignFile(F, ExePath+'list.dat');
  try
    Rewrite(F);
    WriteLn(F, Method);
    WriteLn(F, Index);
    for Index:=0 to High(Wallpapers) do
      WriteLn(F, Wallpapers[Index]);
  finally
    CloseFile(F);
  end;
end;

function GetTime: Integer;
var
  Time: TSystemTime;
begin
  GetLocalTime(Time);
  Result:=Time.wHour*3600+Time.wMinute*60+Time.wSecond;
end;

function WindowProc(hWnd: THandle; uMsg, wParam, lParam: Integer): Integer; stdcall; export;
begin
  Result := 0;
  case uMsg of
  WM_USER:
    begin
      LoadSettings;
      case wParam of
        1: ; //Interval updated
        2: begin //Hotkeys updated
          RemoveHotKeys;
          SetHotKeys;
        end;
      end;
    end;
  WM_HOTKEY:
    begin
      if wParam=KeyDefID
        then SetWallpaper(DefWP)
      else if wParam=KeyChgID
        then ChangeWallpaper;
    end;
  WM_TIMER:
    if UpdateLock>0
      then Dec(UpdateLock)
      else if (UpdateInterval>0) and (GetTime mod UpdateInterval < 10) then
      begin
        ChangeWallpaper;
        UpdateLock:=15;
      end;
  WM_DESTROY:
    begin
      PostQuitMessage(0);
      Exit;
    end;
  end;
  Result := DefWindowProc(hWnd, uMsg, wParam, lParam);
end;

var
  WndClass: TWndClass;
  Msg: TMsg;

begin
  Randomize;
  FillChar(WndClass, SizeOf(WndClass), 0);
  with WndClass do begin
    hInstance      := SysInit.hInstance;
    lpszClassName  := 'hkhdclass';
    lpfnWndProc    := @WindowProc;
  end;
  Windows.RegisterClass(WndClass);
  hWnd := CreateWindow('hkhdclass', '', 0, 0, 0, 0, 0, 0, 0, hInstance, NIL);
  if hWnd = 0 then begin
    MessageBox(0, 'Initialization failed', NIL, ID_OK);
    Exit;
  end;
  ShowWindow(hWnd, SW_HIDE);
  SetLength(ExePath, MAX_PATH);
  SetLength(ExePath, GetModuleFileName(hInstance, PChar(ExePath), MAX_PATH));
  ExePath:=ExtractFilePath(ExePath);
  SetTimer(hWnd, 1, 1000, nil);
  KeyDefID:=GlobalAddAtom('vs.hkhd.hotkey.setdefaultwallpaper');
  KeyChgID:=GlobalAddAtom('vs.hkhd.hotkey.changewallpaper');
  LoadSettings;
  SetHotKeys;
  while GetMessage(Msg, 0, 0, 0) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
  RemoveHotKeys;
  GlobalDeleteAtom(KeyDefID);
  GlobalDeleteAtom(KeyChgID);
end.

