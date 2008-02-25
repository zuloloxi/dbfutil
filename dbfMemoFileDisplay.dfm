object frmMemoFileDisplay: TfrmMemoFileDisplay
  Left = 204
  Top = 164
  ClientHeight = 453
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Andale Mono'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object pnlButtons: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 376
      Top = 5
      Width = 123
      Height = 33
      TabOrder = 0
      object rbtAsciiMode: TRadioButton
        Left = 5
        Top = 12
        Width = 68
        Height = 17
        Caption = 'Ascii'
        TabOrder = 0
      end
      object rbtHexMode: TRadioButton
        Left = 72
        Top = 11
        Width = 49
        Height = 17
        Caption = 'Hex'
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
  end
  object PanelSplitterTop1: TPanelSplitterTop
    Left = 0
    Top = 41
    Width = 688
    TabOrder = 1
  end
  object pnlMain: TPanel
    Left = 0
    Top = 44
    Width = 688
    Height = 409
    Align = alClient
    TabOrder = 2
    object heMemo: TKHexEditor
      Left = 1
      Top = 1
      Width = 686
      Height = 407
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Andale Mono'
      Font.Pitch = fpFixed
      Font.Style = [fsBold]
      TabOrder = 0
      Size = 0
    end
  end
end
