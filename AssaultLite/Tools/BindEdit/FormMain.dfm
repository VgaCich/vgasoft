object MainForm: TMainForm
  Left = 192
  Top = 114
  Width = 499
  Height = 418
  Caption = 'Bind Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  DesignSize = (
    491
    384)
  PixelsPerInch = 96
  TextHeight = 13
  object BindsList: TListView
    Left = 0
    Top = 0
    Width = 491
    Height = 345
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Bind ID'
        Width = 75
      end
      item
        Caption = 'Description'
        Width = 337
      end
      item
        Caption = 'Default Key'
        Width = 75
      end>
    ColumnClick = False
    GridLines = True
    RowSelect = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = BindsListDblClick
    OnKeyDown = FormKeyDown
    OnMouseDown = BindsListMouseDown
  end
  object BLoad: TButton
    Left = 8
    Top = 352
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load'
    TabOrder = 1
    OnClick = BLoadClick
  end
  object BSave: TButton
    Left = 80
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 2
    OnClick = BSaveClick
  end
  object BAdd: TButton
    Left = 176
    Top = 352
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add'
    TabOrder = 3
    OnClick = BAddClick
  end
  object BRemove: TButton
    Left = 248
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Remove'
    TabOrder = 4
    OnClick = BRemoveClick
  end
  object BUp: TButton
    Left = 336
    Top = 352
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Up'
    TabOrder = 5
    OnClick = BUpClick
  end
  object BDown: TButton
    Left = 408
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Down'
    TabOrder = 6
    OnClick = BDownClick
  end
  object ListEdit: TEdit
    Left = 24
    Top = 32
    Width = 121
    Height = 17
    TabStop = False
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 7
    Text = 'ListEdit'
    Visible = False
    OnExit = ListEditExit
    OnKeyPress = ListEditKeyPress
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.cfg'
    Filter = 'Binds configuration file|Bind.cfg'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 24
    Top = 64
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.cfg'
    FileName = 'Bind.cfg'
    Filter = 'Binds configuration file|Bind.cfg'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 96
    Top = 64
  end
end
