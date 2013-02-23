unit MainForm;

interface

uses
  Windows, Messages, ShellAPI, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ComCtrls, Menus, XPMan, COMPort, ExtCtrls, IniFiles,
  Registry;

type
  TSettingsItem = record
    Name: string;
    Value: Integer;
  end;
  TTextMode = (tmASCII, tmASCIILF, tmASCIICR, tmASCIICRLF, tmASCIIZ, tmHex, tmInt8, tmInt16, tmInt32);
  TFormMain = class(TForm)
    TermLog: TRichEdit;
    CBPort: TComboBox;
    CBBitRate: TComboBox;
    CBDataBits: TComboBox;
    CBStopBits: TComboBox;
    CBParity: TComboBox;
    BtnConnect: TButton;
    Label1: TLabel;
    CBRecvMode: TComboBox;
    CBSendMode: TComboBox;
    Label2: TLabel;
    GroupBoxPort: TGroupBox;
    GroupBoxText: TGroupBox;
    CBShowCaps: TCheckBox;
    CBFlowCtrl: TComboBox;
    CBSend: TComboBox;
    MenuLog: TPopupMenu;
    MISelectAll: TMenuItem;
    MICopy: TMenuItem;
    N1: TMenuItem;
    MISendFile: TMenuItem;
    MIClear: TMenuItem;
    OpenDialog: TOpenDialog;
    N2: TMenuItem;
    MIAbout: TMenuItem;
    XPManifest: TXPManifest;
    RecvTimer: TTimer;
    MIStoreSettings: TMenuItem;
    MISSNowhere: TMenuItem;
    MISSIniFile: TMenuItem;
    MISSRegistry: TMenuItem;
    procedure BtnConnectClick(Sender: TObject);
    procedure CBBitRateChange(Sender: TObject);
    procedure CBDataBitsChange(Sender: TObject);
    procedure CBFlowCtrlChange(Sender: TObject);
    procedure CBParityChange(Sender: TObject);
    procedure CBPortDropDown(Sender: TObject);
    procedure CBSendKeyPress(Sender: TObject; var Key: Char);
    procedure CBStopBitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuLogPopup(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIClearClick(Sender: TObject);
    procedure MICopyClick(Sender: TObject);
    procedure MISelectAllClick(Sender: TObject);
    procedure MISendFileClick(Sender: TObject);
    procedure MISSIniFileClick(Sender: TObject);
    procedure MISSNowhereClick(Sender: TObject);
    procedure MISSRegistryClick(Sender: TObject);
    procedure RecvTimerTimer(Sender: TObject);
  private
    FCOMPort: TCOMPort;
    FLogUpdateState: Boolean;
    FRecvBuffer: string;
    procedure AddToLog(Text, Caption: string; Color: TColor);
    procedure FillComboBox(Combo: TComboBox; Settings: array of TSettingsItem;
        DefItem: Cardinal = 0);
    function GetComboValue(Combo: TComboBox): Integer;
    function GetIniName: string;
    function GetSettings: TCustomIniFile;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SendString(S: string);
    procedure SetLogUpdateState(IsUpdating: Boolean);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

const
  CaptionRecv = 'Recv: ';
  CaptionSend = 'Send: ';
  AboutCaption = 'About ';
  CRLF = #13#10;
  AboutText = 'VgaSoft Terminal 1.0'+CRLF+CRLF+
              'Copyright '#169' VgaSoft, 2013'+CRLF+
              'vgasoft@gmail.com';
  AboutIcon = 'MAINICON';
  BitRatesList: array[0..14] of TSettingsItem = (
    (Name: '110 Baud'; Value: CBR_110),
    (Name: '300 Baud'; Value: CBR_300),
    (Name: '600 Baud'; Value: CBR_600),
    (Name: '1200 Baud'; Value: CBR_1200),
    (Name: '2400 Baud'; Value: CBR_2400),
    (Name: '4800 Baud'; Value: CBR_4800),
    (Name: '9600 Baud'; Value: CBR_9600),
    (Name: '14.4k Baud'; Value: CBR_14400),
    (Name: '19.2k Baud'; Value: CBR_19200),
    (Name: '38.4k Baud'; Value: CBR_38400),
    (Name: '56k Baud'; Value: CBR_56000),
    (Name: '57.6k Baud'; Value: CBR_57600),
    (Name: '115.2k Baud'; Value: CBR_115200),
    (Name: '128k Baud'; Value: CBR_128000),
    (Name: '256k Baud'; Value: CBR_256000));
  DataBitsList: array[0..4] of TSettingsItem = (
    (Name: '5 Bits'; Value: 5),
    (Name: '6 Bits'; Value: 6),
    (Name: '7 Bits'; Value: 7),
    (Name: '8 Bits'; Value: 8),
    (Name: '16 Bits'; Value: 16));
  StopBitsList: array[0..2] of TSettingsItem = (
    (Name: '1 stop bit'; Value: ONESTOPBIT),
    (Name: '1.5 stop bits'; Value: ONE5STOPBITS),
    (Name: '2 stop bits'; Value: TWOSTOPBITS));
  ParityList: array[0..4] of TSettingsItem = (
    (Name: 'No parity'; Value: NOPARITY),
    (Name: 'Odd parity'; Value: ODDPARITY),
    (Name: 'Even parity'; Value: EVENPARITY),
    (Name: 'Mark parity'; Value: MARKPARITY),
    (Name: 'Space parity'; Value: SPACEPARITY));
  FlowCtrlList: array[TFlowControl] of TSettingsItem = (
    (Name: 'None'; Value: Integer(fcNone)),
    (Name: 'Hardware'; Value: Integer(fcHardware)),
    (Name: 'Software'; Value: Integer(fcSoftware)),
    (Name: 'HW+SW'; Value: Integer(fcBoth)));
  SendTextModeList: array[TTextMode] of TSettingsItem = (
    (Name: 'ASCII'; Value: Integer(tmASCII)),
    (Name: 'ASCII+LF'; Value: Integer(tmASCIILF)),
    (Name: 'ASCII+CR'; Value: Integer(tmASCIICR)),
    (Name: 'ASCII+CRLF'; Value: Integer(tmASCIICRLF)),
    (Name: 'ASCIIZ'; Value: Integer(tmASCIIZ)),
    (Name: 'Hex'; Value: Integer(tmHex)),
    (Name: 'Int8'; Value: Integer(tmInt8)),
    (Name: 'Int16'; Value: Integer(tmInt16)),
    (Name: 'Int32'; Value: Integer(tmInt32)));
  RecvTextModeList: array[0..4] of TSettingsItem = (
    (Name: 'ASCII'; Value: Integer(tmASCII)),
    (Name: 'Hex'; Value: Integer(tmHex)),
    (Name: 'Int8'; Value: Integer(tmInt8)),
    (Name: 'Int16'; Value: Integer(tmInt16)),
    (Name: 'Int32'; Value: Integer(tmInt32)));
  IniSectMainForm = 'MainForm';
  IniSectSettings = 'Settings';
  IniSectHistory = 'History';
  RegKeyPath = 'Software\VSTerm';

var
  MaxHistory: Integer = 10;
  DefaultBitRate: Integer = 6;
  DefaultDataBits: Integer = 3;
  DefaultStopBits: Integer = 0;
  DefaultParity: Integer = 0;
  DefaultFlowCtrl: Integer = Integer(fcNone);
  DefaultSendTextMode: Integer = Integer(tmASCII);
  DefaultRecvTextMode: Integer = 0;

{$R *.dfm}

procedure TFormMain.AddToLog(Text, Caption: string; Color: TColor);
var
  Index: Integer;
begin
  for Index:=1 to Length(Text) do
    if Text[Index] in [#$00, #$09, #$0A, #$0B, #$0D, #$AD]
      then Text[Index]:=#$20;
  if CBShowCaps.Checked then
  begin
    Index:=TermLog.Lines.Add(Caption+Text);
    TermLog.SelStart:=SendMessage(TermLog.Handle, EM_LINEINDEX, Index, 0);
    TermLog.SelLength:=Length(Caption);
    TermLog.SelAttributes.Color:=clGray;
    TermLog.SelStart:=TermLog.SelStart+TermLog.SelLength;
    TermLog.SelLength:=Length(Text);
    TermLog.SelAttributes.Color:=Color;
    TermLog.SelLength:=0;
  end
  else begin
    Index:=TermLog.Lines.Add(Text);
    TermLog.SelStart:=SendMessage(TermLog.Handle, EM_LINEINDEX, Index, 0);
    TermLog.SelLength:=Length(Text);
    TermLog.SelAttributes.Color:=Color;
    TermLog.SelLength:=0;
  end;
  TermLog.Perform(EM_SCROLLCARET, 0, 0);
end;

procedure TFormMain.BtnConnectClick(Sender: TObject);

  procedure EnableControls(Enable: Boolean);
  begin
    CBPort.Enabled:=Enable;
    MISendFile.Enabled:=not Enable;
    RecvTimer.Enabled:=not Enable;
  end;

begin
  if Assigned(FCOMPort) then
  begin
    FCOMPort.Purge;
    FreeAndNil(FCOMPort);
    FRecvBuffer:='';
    EnableControls(true);
    BtnConnect.Caption:='Connect';
  end
  else begin
    FCOMPort:=TCOMPort.Create(GetComboValue(CBPort), 4096);
    try
      FCOMPort.BitRate:=GetComboValue(CBBitRate);
      FCOMPort.DataBits:=GetComboValue(CBDataBits);
      FCOMPort.StopBits:=GetComboValue(CBStopBits);
      FCOMPort.Parity:=GetComboValue(CBParity);
      FCOMPort.FlowControl:=TFlowControl(GetComboValue(CBFlowCtrl));
      EnableControls(false);
      BtnConnect.Caption:='Disconnect';
    except
      on E: Exception do
      begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        FreeAndNil(FCOMPort);
        Exit;
      end;
    end;
  end;
end;

procedure TFormMain.CBBitRateChange(Sender: TObject);
begin
  if Assigned(FCOMPort) then FCOMPort.BitRate:=GetComboValue(Sender as TComboBox);
end;

procedure TFormMain.CBDataBitsChange(Sender: TObject);
begin
  if Assigned(FCOMPort) then FCOMPort.DataBits:=GetComboValue(Sender as TComboBox);
end;

procedure TFormMain.CBFlowCtrlChange(Sender: TObject);
begin
  if Assigned(FCOMPort) then FCOMPort.FlowControl:=TFlowControl(GetComboValue(Sender as TComboBox));
end;

procedure TFormMain.CBParityChange(Sender: TObject);
begin
  if Assigned(FCOMPort) then FCOMPort.Parity:=GetComboValue(Sender as TComboBox);
end;

procedure TFormMain.CBPortDropDown(Sender: TObject);
var
  CurPort, i: Integer;
begin
  if (CBPort.ItemIndex>=0) and (CBPort.ItemIndex<CBPort.Items.Count)
    then CurPort:=Integer(CBPort.Items.Objects[CBPort.ItemIndex])
    else CurPort:=-1;
  EnumCOMPorts(CBPort.Items);
  if CurPort<>-1 then
    for i:=0 to CBPort.Items.Count-1 do
      if Integer(CBPort.Items.Objects[i])=CurPort then
      begin
        CBPort.ItemIndex:=i;
        Break;
      end;
end;

procedure TFormMain.CBSendKeyPress(Sender: TObject; var Key: Char);
var
  i: Integer;
  S: string;
begin
  if (Key=Chr(VK_RETURN)) and Assigned (FCOMPort) then
  begin
    S:=CBSend.Text;
    CBSend.Text:='';
    SendString(S);
    if S='' then Exit;
    CBSend.Items.Insert(0, S);
    S:=Trim(S);
    for i:=CBSend.Items.Count-1 downto 1 do
      if Trim(CBSend.Items[i])=S then
        CBSend.Items.Delete(i);
    while CBSend.Items.Count>MaxHistory do
      CBSend.Items.Delete(CBSend.Items.Count-1);
  end;
end;

procedure TFormMain.CBStopBitsChange(Sender: TObject);
begin
  if Assigned(FCOMPort) then FCOMPort.StopBits:=GetComboValue(Sender as TComboBox);
end;

procedure TFormMain.FillComboBox(Combo: TComboBox; Settings: array of
    TSettingsItem; DefItem: Cardinal = 0);
var
  i: Integer;
begin
  Combo.Items.BeginUpdate;
  Combo.Items.Clear;
  for i:=Low(Settings) to High(Settings) do
    Combo.Items.AddObject(Settings[i].Name, TObject(Settings[i].Value));
  Combo.Items.EndUpdate;
  if DefItem<Combo.Items.Count
    then Combo.ItemIndex:=DefItem;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.Title:=FormMain.Caption;
  TermLog.MaxLength:=$200000;
  FRecvBuffer:='';
  FLogUpdateState:=false;
  CBPortDropDown(CBPort);
  if CBPort.Items.Count>0
    then CBPort.ItemIndex:=0;
  LoadSettings;
  FillComboBox(CBBitRate, BitRatesList, DefaultBitRate);
  FillComboBox(CBDataBits, DataBitsList, DefaultDataBits);
  FillComboBox(CBStopBits, StopBitsList, DefaultStopBits);
  FillComboBox(CBParity, ParityList, DefaultParity);
  FillComboBox(CBFlowCtrl, FlowCtrlList, DefaultFlowCtrl);
  FillComboBox(CBSendMode, SendTextModeList, DefaultSendTextMode);
  FillComboBox(CBRecvMode, RecvTextModeList, DefaultRecvTextMode);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FCOMPort) then BtnConnectClick(nil);
  SaveSettings;
end;

function TFormMain.GetComboValue(Combo: TComboBox): Integer;
begin
  if (Combo.ItemIndex>=0) and (Combo.ItemIndex<Combo.Items.Count)
    then Result:=Integer(Combo.Items.Objects[Combo.ItemIndex])
    else Result:=0;
end;

function TFormMain.GetIniName: string;
begin
  Result:=ChangeFileExt(ExpandFileName(Application.ExeName), '.ini');
end;

function TFormMain.GetSettings: TCustomIniFile;

  function RegKeyExists(const Path: string): Boolean;
  var
    Reg: TRegistry;
  begin
    Result:=false;
    Reg:=TRegistry.Create(KEY_READ);
    try
      Reg.RootKey:=HKEY_CURRENT_USER;
      Result:=Reg.KeyExists(Path);
    finally
      FreeAndNil(Reg);
    end;
  end;

begin
  if FileExists(GetIniName) then Result:=TIniFile.Create(GetIniName)
  else if RegKeyExists(RegKeyPath) then Result:=TRegistryIniFile.Create(RegKeyPath)
  else Result:=nil;
end;

procedure TFormMain.LoadSettings;
var
  Settings: TCustomIniFile;
  i, Port: Integer;
begin
  Settings:=GetSettings;
  if not Assigned(Settings) then Exit;
  try
    WindowState:=TWindowState(Settings.ReadInteger(IniSectMainForm, 'State', Integer(WindowState)));
    if WindowState<>wsMaximized then SetBounds(Left, Top,
      Settings.ReadInteger(IniSectMainForm, 'Width', Width),
      Settings.ReadInteger(IniSectMainForm, 'Height', Height));
    MaxHistory:=Settings.ReadInteger(IniSectSettings, 'MaxHistory', MaxHistory);
    DefaultBitRate:=Settings.ReadInteger(IniSectSettings, 'BitRate', DefaultBitRate);
    DefaultDataBits:=Settings.ReadInteger(IniSectSettings, 'DataBits', DefaultDataBits);
    DefaultStopBits:=Settings.ReadInteger(IniSectSettings, 'StopBits', DefaultStopBits);
    DefaultParity:=Settings.ReadInteger(IniSectSettings, 'Parity', DefaultParity);
    DefaultFlowCtrl:=Settings.ReadInteger(IniSectSettings, 'FlowCtrl', DefaultFlowCtrl);
    DefaultSendTextMode:=Settings.ReadInteger(IniSectSettings, 'SendMode', DefaultSendTextMode);
    DefaultRecvTextMode:=Settings.ReadInteger(IniSectSettings, 'RecvMode', DefaultRecvTextMode);
    CBShowCaps.Checked:=Settings.ReadBool(IniSectSettings, 'ShowCaps', false);
    for i:=0 to MaxHistory do
      if Settings.ValueExists(IniSectHistory, IntToStr(i))
        then CBSend.Items.Add(Settings.ReadString(IniSectHistory, IntToStr(i), ''));
    Port:=Settings.ReadInteger(IniSectSettings, 'Port', 1);
    for i:=0 to CBPort.Items.Count-1 do
      if Integer(CBPort.Items.Objects[i])=Port then
      begin
        CBPort.ItemIndex:=0;
        Break;
      end;
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TFormMain.MenuLogPopup(Sender: TObject);
var
  Sets: TObject;
begin
  Sets:=GetSettings;
  try
    if not Assigned(Sets) then MISSNowhere.Checked:=true
    else if Sets is TIniFile then MISSIniFile.Checked:=true
    else if Sets is TRegistryIniFile then MISSRegistry.Checked:=true;
  finally
    FreeAndNil(Sets);
  end;
end;

procedure TFormMain.MIAboutClick(Sender: TObject);
var
  Version: TOSVersionInfo;
  MsgBoxParamsW: TMsgBoxParamsW;
  MsgBoxParamsA: TMsgBoxParamsA;
begin
  Version.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(Version);
  if Version.dwPlatformId = VER_PLATFORM_WIN32_NT then
  begin
    FillChar(MsgBoxParamsW, SizeOf(MsgBoxParamsW), #0);
    with MsgBoxParamsW do
    begin
      cbSize := SizeOf(MsgBoxParamsW);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText  := AboutText;
      lpszCaption := PWideChar(WideString(AboutCaption+Caption));
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectW(MsgBoxParamsW);
  end
  else begin
    FillChar(MsgBoxParamsA, SizeOf(MsgBoxParamsA), #0);
    with MsgBoxParamsA do
    begin
      cbSize := SizeOf(MsgBoxParamsA);
      hwndOwner := Handle;
      hInstance := SysInit.hInstance;
      lpszText  := AboutText;
      lpszCaption := PAnsiChar(AboutCaption+Caption);
      lpszIcon := AboutIcon;
      dwStyle := MB_USERICON;
    end;
    MessageBoxIndirectA(MsgBoxParamsA);
  end;
end;

procedure TFormMain.MIClearClick(Sender: TObject);
begin
  TermLog.Clear;
end;

procedure TFormMain.MICopyClick(Sender: TObject);
begin
  TermLog.CopyToClipboard;
end;

procedure TFormMain.MISelectAllClick(Sender: TObject);
begin
  TermLog.SelectAll;
end;

procedure TFormMain.MISendFileClick(Sender: TObject);
var
  F: TFileStream;
  Buf: array[0..255] of Byte;
  Len, Done: Integer;
begin
  if not Assigned(FCOMPort) or not OpenDialog.Execute or not FileExists(OpenDialog.FileName) then Exit;
  F:=TFileStream.Create(OpenDialog.FileName, fmOpenRead);
  try
    AddToLog('File "'+OpenDialog.FileName+'"', CaptionSend, clNavy);
    while F.Position<F.Size do
    begin
      Len:=F.Read(Buf[0], Length(Buf));
      Done:=0;
      while Done<Len do
        Done:=Done+FCOMPort.Write(Buf[Done], Len-Done);
      Application.ProcessMessages;
    end;
    AddToLog('File "'+OpenDialog.FileName+'" sent', CaptionSend, clNavy);
  finally
    FreeAndNil(F);
  end;
end;

procedure TFormMain.MISSIniFileClick(Sender: TObject);
begin
  MISSNowhereClick(Sender);
  CloseHandle(FileCreate(GetIniName, fmCreate));
end;

procedure TFormMain.MISSNowhereClick(Sender: TObject);
var
  Reg: TRegistry;
begin
  if (Sender<>MISSINIFile) and FileExists(GetIniName) then DeleteFile(GetIniName);
  if Sender<>MISSRegistry then
  begin
    Reg:=TRegistry.Create;
    try
      Reg.RootKey:=HKEY_CURRENT_USER;
      if Reg.KeyExists(RegKeyPath) then Reg.DeleteKey(RegKeyPath);
    finally
      FreeAndNil(Reg);
    end;
  end;
end;

procedure TFormMain.MISSRegistryClick(Sender: TObject);
var
  Reg: TRegistry;
begin
  MISSNowhereClick(Sender);
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_CURRENT_USER;
    if not Reg.KeyExists(RegKeyPath) then Reg.CreateKey(RegKeyPath);
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TFormMain.RecvTimerTimer(Sender: TObject);

  function ReadToBuffer: Integer;
  const
    ChunkSize=256;
  var
    BytesRead: Integer;
  begin
    Result:=0;
    repeat
      SetLength(FRecvBuffer, Length(FRecvBuffer)+ChunkSize);
      BytesRead:=FCOMPort.Read(FRecvBuffer[Length(FRecvBuffer)-ChunkSize+1], ChunkSize);
      Result:=Result+BytesRead;
    until BytesRead<ChunkSize;
    SetLength(FRecvBuffer, Length(FRecvBuffer)-ChunkSize+BytesRead);
  end;

  function HexByte(b: Byte): Char;
  begin
    case b of
      0..9: Result:=Chr(Ord('0')+b);
      10..15: Result:=Chr(Ord('A')-10+b);
      else Result:='?';
    end;
  end;

  function ConvertToHex(const S: string; Count: Integer): string;
  var
    B: Byte;
    i: Integer;
  begin
    SetLength(Result, 16*3+16);
    FillMemory(@Result[1], Length(Result), $20);
    for i:=0 to Count-1 do
    begin
      B:=Ord(S[i+1]);
      Result[3*i+1]:=HexByte((B shr 4) and $0F);
      Result[3*i+2]:=HexByte(B and $0F);
      Result[3*16+1+i]:=Chr(B);
    end;
  end;

  function ReadLine: string;
  var
    i: Integer;
  begin
    i:=1;
    while i<=Length(FRecvBuffer) do
      if FRecvBuffer[i] in [#10, #13]
        then Break
        else Inc(i);
    if i<=Length(FRecvBuffer) then
    begin
      Result:=Copy(FRecvBuffer, 1, i-1);
      if (i<Length(FRecvBuffer)) and (FRecvBuffer[i]=#13) and (FRecvBuffer[i+1]=#10)
        then Inc(i);
      Delete(FRecvBuffer, 1, i);
    end
      else Result:='';
  end; 

var
  NewData, i: Integer;
begin
  if not Assigned(FCOMPort) then Exit;
  NewData:=ReadToBuffer;
  if NewData>0 then SetLogUpdateState(true);
  case TTextMode(GetComboValue(CBRecvMode)) of
    tmHex: begin
      while Length(FRecvBuffer)>16 do
      begin
        AddToLog(ConvertToHex(FRecvBuffer, 16), CaptionRecv, clMaroon);
        Delete(FRecvBuffer, 1, 16);
      end;
      if (NewData=0) and (Length(FRecvBuffer)>0) then
      begin
        AddToLog(ConvertToHex(FRecvBuffer, Length(FRecvBuffer)), CaptionRecv, clMaroon);
        FRecvBuffer:='';
      end;
    end;
    tmInt8: begin
      for i:=0 to Length(FRecvBuffer)-1 do
        AddToLog(IntToStr(Ord(FRecvBuffer[i+1])), CaptionRecv, clMaroon);
      FRecvBuffer:='';
    end;
    tmInt16: begin
      NewData:=Length(FRecvBuffer) div 2;
      for i:=0 to NewData-1 do
        AddToLog(IntToStr(Word(PWord(@FRecvBuffer[2*i+1])^)), CaptionRecv, clMaroon);
      Delete(FRecvBuffer, 1, 2*NewData);
    end;
    tmInt32: begin
      NewData:=Length(FRecvBuffer) div 4;
      for i:=0 to NewData-1 do
        AddToLog(IntToStr(Integer(PInteger(@FRecvBuffer[4*i+1])^)), CaptionRecv, clMaroon);
      Delete(FRecvBuffer, 1, 4*NewData);
    end;
  else
    while (Pos(#10, FRecvBuffer)>0) or (Pos(#13, FRecvBuffer)>0) do
      AddToLog(ReadLine, CaptionRecv, clMaroon);
    if (NewData=0) and (Length(FRecvBuffer)>0) then
    begin
      AddToLog(FRecvBuffer, CaptionRecv, clMaroon);
      FRecvBuffer:='';
    end;
  end;
  SetLogUpdateState(false);
end;

procedure TFormMain.SaveSettings;
var
  Settings: TCustomIniFile;
  i: Integer;
begin
  Settings:=GetSettings;
  if not Assigned(Settings) then Exit;
  try
    Settings.WriteInteger(IniSectMainForm, 'State', Integer(WindowState));
    if WindowState<>wsMaximized then
    begin
      Settings.WriteInteger(IniSectMainForm, 'Width', Width);
      Settings.WriteInteger(IniSectMainForm, 'Height', Height);
    end;
    Settings.WriteInteger(IniSectSettings, 'MaxHistory', MaxHistory);
    Settings.WriteInteger(IniSectSettings, 'Port', GetComboValue(CBPort));
    Settings.WriteInteger(IniSectSettings, 'BitRate', CBBitRate.ItemIndex);
    Settings.WriteInteger(IniSectSettings, 'DataBits', CBDataBits.ItemIndex);
    Settings.WriteInteger(IniSectSettings, 'StopBits', CBStopBits.ItemIndex);
    Settings.WriteInteger(IniSectSettings, 'Parity', CBParity.ItemIndex);
    Settings.WriteInteger(IniSectSettings, 'FlowCtrl', CBFlowCtrl.ItemIndex);
    Settings.WriteInteger(IniSectSettings, 'SendMode', CBSendMode.ItemIndex);
    Settings.WriteInteger(IniSectSettings, 'RecvMode', CBRecvMode.ItemIndex);
    Settings.WriteBool(IniSectSettings, 'ShowCaps', CBShowCaps.Checked);
    Settings.EraseSection(IniSectHistory);
    for i:=0 to CBSend.Items.Count-1 do
      Settings.WriteString(IniSectHistory, IntToStr(i), CBSend.Items[i]);
  finally
    FreeAndNil(Settings);
  end;
end;

procedure TFormMain.SendString(S: string);

  function HexChar(c: Char): Byte;
  begin
    case c of
      '0'..'9':  Result := Byte(c) - Byte('0');
      'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
    else
      Result := 0;
    end;
  end;

var
  TempByte: Byte;
  TempWord: Word;
  TempDWord: DWORD;
  i, Len: Integer;
begin
  AddToLog(S, CaptionSend, clGreen);
  case TTextMode(GetComboValue(CBSendMode)) of
    tmASCII: FCOMPort.Write(S[1], Length(S));
    tmASCIILF: begin
      S:=S+#10;
      FCOMPort.Write(S[1], Length(S));
    end;
    tmASCIICR: begin
      S:=S+#13;
      FCOMPort.Write(S[1], Length(S));
    end;
    tmASCIICRLF: begin
      S:=S+CRLF;
      FCOMPort.Write(S[1], Length(S));
    end;
    tmASCIIZ: begin
      S:=S+#0;
      FCOMPort.Write(S[1], Length(S));
    end;
    tmHex: begin
      for i:=Length(S) downto 1 do
        if not (S[i] in ['a'..'f', 'A'..'F', '0'..'9'])
          then Delete(S, i, 1);
      Len:=Length(S) div 2;
      for i:=1 to Len do
        S[i]:=Chr((HexChar(S[2*i-1]) shl 4) or HexChar(S[2*i]));
      SetLength(S, Len);
      FCOMPort.Write(S[1], Length(S));
    end;
    tmInt8: begin
      TempByte:=StrToInt(S);
      FCOMPort.Write(TempByte, SizeOf(TempByte));
    end;
    tmInt16: begin
      TempWord:=StrToInt(S);
      FCOMPort.Write(TempWord, SizeOf(TempWord));
    end;
    tmInt32: begin
      TempDWord:=StrToInt64(S);
      FCOMPort.Write(TempDWord, SizeOf(TempDWord));
    end;
  end;
end;

procedure TFormMain.SetLogUpdateState(IsUpdating: Boolean);
begin
  if IsUpdating=FLogUpdateState then Exit;
  FLogUpdateState:=IsUpdating;
  if IsUpdating
    then TermLog.Lines.BeginUpdate
    else TermLog.Lines.EndUpdate;
end;

end.
