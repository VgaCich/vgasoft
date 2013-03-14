object Form1: TForm1
  Left = 191
  Top = 113
  Width = 472
  Height = 399
  Caption = 'Wallpapers Changer'
  Color = clBtnFace
  Constraints.MinHeight = 399
  Constraints.MinWidth = 472
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    464
    365)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 256
    Top = 8
    Width = 200
    Height = 150
    Anchors = [akTop, akRight]
    Center = True
    IncrementalDisplay = True
    Proportional = True
    Stretch = True
    OnDblClick = ListBox1DblClick
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 249
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    DragMode = dmAutomatic
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
    OnDragDrop = ListBox1DragDrop
    OnDragOver = ListBox1DragOver
    OnKeyPress = ListBox1KeyPress
  end
  object RadioButton1: TRadioButton
    Left = 258
    Top = 168
    Width = 71
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Center'
    Checked = True
    TabOrder = 1
    TabStop = True
  end
  object RadioButton2: TRadioButton
    Left = 258
    Top = 184
    Width = 71
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Tile'
    TabOrder = 2
  end
  object RadioButton3: TRadioButton
    Left = 258
    Top = 200
    Width = 71
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Stretch'
    TabOrder = 3
  end
  object Button1: TButton
    Left = 8
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Remove'
    TabOrder = 5
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 256
    Top = 296
    Width = 201
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 6
    Text = 'Random'
    Items.Strings = (
      'Random'
      'In order'
      'In order (reverse) ')
  end
  object Button3: TButton
    Left = 384
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Exit'
    TabOrder = 7
    OnClick = Button3Click
  end
  object CheckBox1: TCheckBox
    Left = 256
    Top = 328
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Autostart'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object Button4: TButton
    Left = 384
    Top = 168
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Import'
    TabOrder = 9
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 384
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Export'
    TabOrder = 10
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 168
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Set as default'
    TabOrder = 11
    OnClick = Button6Click
  end
  object ComboBox2: TComboBox
    Left = 256
    Top = 264
    Width = 201
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    ItemHeight = 13
    TabOrder = 12
    OnChange = ComboBox2Change
    Items.Strings = (
      'No autochange')
  end
  object CheckBox2: TCheckBox
    Left = 256
    Top = 344
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Autostart HKHD'
    TabOrder = 13
    OnClick = CheckBox2Click
  end
  object Button7: TButton
    Left = 384
    Top = 232
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Set hotkeys'
    TabOrder = 14
    OnClick = Button7Click
  end
  object LabeledEdit1: TLabeledEdit
    Left = 256
    Top = 232
    Width = 57
    Height = 21
    Anchors = [akTop, akRight]
    EditLabel.Width = 53
    EditLabel.Height = 13
    EditLabel.Caption = 'Set Default'
    ReadOnly = True
    TabOrder = 15
    OnKeyDown = LabeledEdit1KeyDown
  end
  object LabeledEdit2: TLabeledEdit
    Left = 320
    Top = 232
    Width = 57
    Height = 21
    Anchors = [akTop, akRight]
    EditLabel.Width = 58
    EditLabel.Height = 13
    EditLabel.Caption = 'Change WP'
    ReadOnly = True
    TabOrder = 16
    OnKeyDown = LabeledEdit1KeyDown
  end
  object CheckBox3: TCheckBox
    Left = 90
    Top = 341
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    TabOrder = 17
  end
  object OpenDialog1: TOpenPictureDialog
    Filter = 
      'All supported|*.bmp;*.jpg|Bitmap files|*.bmp|JPEG files|*3.jpg|A' +
      'll files|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 8
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Aston config files|*.rc'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select config file (Plugins.rc)'
    Left = 72
    Top = 8
  end
  object OpenDialog2: TOpenDialog
    Filter = 'Aston config files|*.rc'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select config file (Plugins.rc)'
    Left = 136
    Top = 8
  end
end
