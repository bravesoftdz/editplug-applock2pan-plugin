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
    Top = 309
    Width = 304
    Height = 2
    Cursor = crVSplit
    Align = alTop
    Color = clMaroon
    ParentColor = False
  end
  object HeadPanel: TPanel
    Left = 0
    Top = 0
    Width = 304
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object bNoTitle: TSpeedButton
      Left = 182
      Top = 0
      Width = 51
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
    object bBorder: TSpeedButton
      Left = 236
      Top = 0
      Width = 45
      Height = 17
      AllowAllUp = True
      GroupIndex = 2
      Down = True
      Caption = 'Border'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      OnClick = bBorderClick
    end
    object bLock: TButton
      Left = 2
      Top = 0
      Width = 71
      Height = 17
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
      Left = 136
      Top = 0
      Width = 45
      Height = 17
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
    object edFilter: TEdit
      Left = 72
      Top = 1
      Width = 64
      Height = 16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = 'f i l t e r'
      OnEnter = edFilterEnter
      OnKeyPress = edFilterKeyPress
    end
  end
  object WinPanTop: TPanel
    Left = 0
    Top = 17
    Width = 304
    Height = 292
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Top Window Panel'
    PopupMenu = Popup
    TabOrder = 1
    OnResize = WinPanTopResize
  end
  object WinPanBottom: TPanel
    Left = 0
    Top = 311
    Width = 304
    Height = 343
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
    object mStayOnTop: TMenuItem
      Caption = 'Just StayOnTop'
      object TMenuItem
      end
    end
    object mUndoStayOnTop: TMenuItem
      Caption = 'Undo StayOnTop'
      object TMenuItem
      end
    end
  end
end
