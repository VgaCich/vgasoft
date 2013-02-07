object FormMain: TFormMain
  Left = 192
  Top = 114
  Width = 608
  Height = 402
  Caption = 'VSTerm'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    600
    368)
  PixelsPerInch = 96
  TextHeight = 13
  object TermLog: TRichEdit
    Left = 8
    Top = 8
    Width = 433
    Height = 321
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    PlainText = True
    PopupMenu = MenuLog
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object GroupBoxPort: TGroupBox
    Left = 448
    Top = 8
    Width = 145
    Height = 225
    Anchors = [akTop, akRight]
    Caption = 'Port Settings'
    TabOrder = 1
    object BtnConnect: TButton
      Left = 8
      Top = 192
      Width = 129
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = BtnConnectClick
    end
    object CBPort: TComboBox
      Left = 8
      Top = 16
      Width = 129
      Height = 21
      Hint = 'Port'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnDropDown = CBPortDropDown
    end
    object CBBitRate: TComboBox
      Left = 8
      Top = 44
      Width = 129
      Height = 21
      Hint = 'Bitrate'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnChange = CBBitRateChange
    end
    object CBDataBits: TComboBox
      Left = 8
      Top = 73
      Width = 129
      Height = 21
      Hint = 'Data bits'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnChange = CBDataBitsChange
    end
    object CBStopBits: TComboBox
      Left = 8
      Top = 102
      Width = 129
      Height = 21
      Hint = 'Stop bits'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnChange = CBStopBitsChange
    end
    object CBParity: TComboBox
      Left = 8
      Top = 131
      Width = 129
      Height = 21
      Hint = 'Parity'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnChange = CBParityChange
    end
    object CBFlowCtrl: TComboBox
      Left = 8
      Top = 160
      Width = 129
      Height = 21
      Hint = 'Flow control'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnChange = CBFlowCtrlChange
    end
  end
  object GroupBoxText: TGroupBox
    Left = 448
    Top = 240
    Width = 145
    Height = 121
    Anchors = [akTop, akRight]
    Caption = 'Text Settings'
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 43
      Height = 13
      Caption = 'Receive:'
    end
    object Label2: TLabel
      Left = 8
      Top = 54
      Width = 28
      Height = 13
      Caption = 'Send:'
    end
    object CBRecvMode: TComboBox
      Left = 8
      Top = 30
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object CBSendMode: TComboBox
      Left = 8
      Top = 68
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object CBShowCaps: TCheckBox
      Left = 8
      Top = 96
      Width = 97
      Height = 17
      Caption = 'Captions'
      TabOrder = 2
    end
  end
  object CBSend: TComboBox
    Left = 8
    Top = 336
    Width = 433
    Height = 21
    Hint = 'Text to send'
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnKeyPress = CBSendKeyPress
  end
  object MenuLog: TPopupMenu
    OnPopup = MenuLogPopup
    Left = 24
    Top = 16
    object MISelectAll: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = MISelectAllClick
    end
    object MICopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = MICopyClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MISendFile: TMenuItem
      Caption = 'Send file...'
      Enabled = False
      ShortCut = 16463
      OnClick = MISendFileClick
    end
    object MIClear: TMenuItem
      Caption = 'Clear'
      ShortCut = 16462
      OnClick = MIClearClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MIStoreSettings: TMenuItem
      Caption = 'Store settings'
      object MISSNowhere: TMenuItem
        AutoCheck = True
        Caption = 'Nowhere'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = MISSNowhereClick
      end
      object MISSIniFile: TMenuItem
        AutoCheck = True
        Caption = 'In INI file'
        GroupIndex = 1
        RadioItem = True
        OnClick = MISSIniFileClick
      end
      object MISSRegistry: TMenuItem
        AutoCheck = True
        Caption = 'In registry'
        GroupIndex = 1
        RadioItem = True
        OnClick = MISSRegistryClick
      end
    end
    object MIAbout: TMenuItem
      Caption = 'About...'
      ShortCut = 112
      OnClick = MIAboutClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All files|*.*'
    Options = [ofFileMustExist, ofEnableSizing]
    Left = 24
    Top = 64
  end
  object XPManifest: TXPManifest
    Left = 24
    Top = 112
  end
  object RecvTimer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = RecvTimerTimer
    Left = 24
    Top = 160
  end
end
