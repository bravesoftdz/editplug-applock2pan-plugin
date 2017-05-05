object Form1: TForm1
  Left = 975
  Top = 17
  BorderStyle = bsNone
  Caption = 'AppLock'
  ClientHeight = 654
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 345
    Width = 304
    Height = 2
    Cursor = crVSplit
    Align = alTop
    Color = clMaroon
    ParentColor = False
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 304
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object bNoTitle: TSpeedButton
      Left = 134
      Top = 0
      Width = 57
      Height = 17
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Caption = 'No Title'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      OnClick = bNoTitleClick
    end
    object bLock: TButton
      Left = 2
      Top = 2
      Width = 71
      Height = 15
      Caption = 'Lock App...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = bLockClick
    end
    object bUnlock: TButton
      Left = 76
      Top = 2
      Width = 55
      Height = 15
      Caption = 'Unlock'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = bUnlockClick
    end
  end
  object WinPanTop: TPanel
    Left = 0
    Top = 17
    Width = 304
    Height = 328
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Top Window Panel'
    PopupMenu = Popup
    TabOrder = 1
    OnResize = WinPanTopResize
  end
  object WinPanBottom: TPanel
    Left = 0
    Top = 347
    Width = 304
    Height = 307
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Bottom Window Panel'
    PopupMenu = Popup
    TabOrder = 2
    OnResize = WinPanBottomResize
    object Label1: TLabel
      Left = 128
      Top = 4
      Width = 163
      Height = 13
      Caption = '(adjust height with splitter above)'
    end
  end
  object Popup: TPopupMenu
    OnPopup = PopupPopup
    Left = 62
    Top = 50
    object mTopPan: TMenuItem
      Caption = 'Top Panel'
      object TMenuItem
      end
    end
    object mBottomPan: TMenuItem
      Caption = 'Bottom Panel'
      object TMenuItem
      end
    end
  end
end
