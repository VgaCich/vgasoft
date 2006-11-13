program ReadableFormTest;

uses
  Windows, Messages, AvL, avlUtils, avlReadableForm;

type
  TMainForm=class(TReadableForm)
  published
    Animate: TAnimate;
    FileListBox: TFileListBox;
    HotKey: THotKey;
    procedure FormShow(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure ComboChange(Sender: TObject);
    procedure HotKeyChange(Sender: TObject);
  end;

var
   FD: TMemoryStream;
   MainForm: TMainForm;

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

begin
  if not FileExists('TestForm.fdt') then
  begin
    ShowMessage('File FormData.fdt not found');
    Exit;
  end;
  InitCommonControls;
  try
    FD:=TMemoryStream.Create;
    FD.LoadFromFile('TestForm.fdt');
    try
      MainForm:=TMainForm.Create(nil, FD);
    except
      on E: Exception do ShowMessage(E.Message);
    end;
    MainForm.Run;
  finally
    FAN(MainForm);
    FAN(FD);
  end;
end.
