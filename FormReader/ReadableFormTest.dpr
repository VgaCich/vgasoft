program ReadableFormTest;

uses
  SysSfIni, AvL, Windows, Messages, avlUtils, avlReadableForm, LeakDetect;

type
  TMainForm=class(TReadableForm)
  protected
    procedure WMCommand(var Msg: TWMCommand); message WM_COMMAND;
  published
    Animate: TAnimate;
    FileListBox: TFileListBox;
    HotKey: THotKey;
    ToolBar: TToolBar;
    StatusBar: TStatusBar;
    LabeledEdit: TLabeledEdit;
    procedure FormShow(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure ComboChange(Sender: TObject);
    procedure HotKeyChange(Sender: TObject);
  end;

var
   FD: TMemoryStream;
   MainForm: TMainForm;

procedure TMainForm.WMCommand(var Msg: TWMCommand);
begin
  if Assigned(ToolBar) and (Msg.Ctl=ToolBar.Handle) then
  begin
    case Msg.ItemID of
      0: StatusBar.SetPartText(2, 0, 'Open');
      1: StatusBar.SetPartText(2, 0, 'Save');
      2: Animate.CommonAVI:=aviFindComputer;
      3: Animate.CommonAVI:=aviFindFile;
      4: StatusBar.SetPartText(2, 0, 'FormReader + ReadableForm testing application (c) Vga 2004-2023');
      5: Close;
    end;
    Animate.Play(0, 100, 0);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Caption:=Caption+': '+IntToStr(Handle);
  Animate.Play(0, 100, 0);
  FileListBox.Update;
end;

procedure TMainForm.ButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ComboChange(Sender: TObject);
begin
  Caption:=(Sender as TComboBox).Items[(Sender as TComboBox).ItemIndex];
end;

procedure TMainForm.HotKeyChange(Sender: TObject);
var
  HK: Byte;
  Modifiers: TModifiers;
  S: string;
begin
  HK:=HotKey.HotKey;
  Modifiers:=HotKey.Modifiers;
  S:='';
  if mCtrl in Modifiers then S:='Ctrl+';
  if mShift in Modifiers then S:=S+'Shift+';
  if mAlt in Modifiers then S:=S+'Alt+';
  if mExt in Modifiers then S:=S+'Ext+';
  Caption:=S+Chr(HK);
end;

var
  FormFile: string = 'TestForm.txt';

begin
  if ParamCount=1 then FormFile:=ParamStr(1);
  if not FileExists(FormFile) then
  begin
    ShowMessage('File "'+FormFile+'" not found');
    Exit;
  end;
  InitCommonControls;
  FD:=TMemoryStream.Create;
  try
    FD.LoadFromFile(FormFile);
    try
      MainForm:=TMainForm.Create(nil, FD);
      try
        MainForm.Run;
      finally
        FAN(MainForm);
      end;
    except
      MessageBox(0, PChar(Exception(ExceptObject).Message), 'Error', 0);
    end;
  finally
    FAN(FD);
  end;
end.
