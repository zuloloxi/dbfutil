object frmScanFields: TfrmScanFields
  Left = 254
  Top = 186
  Caption = 'Scan Fields'
  ClientHeight = 453
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlue
  Font.Height = -13
  Font.Name = 'Andale Mono'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 15
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 453
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    object pnlButtons: TPanel
      Left = 1
      Top = 411
      Width = 686
      Height = 41
      Align = alBottom
      TabOrder = 0
      object btnScan: TButton
        Left = 544
        Top = 10
        Width = 75
        Height = 25
        Caption = '&Scan'
        TabOrder = 0
        OnClick = btnScanClick
      end
    end
    object PanelSplitterBottom1: TPanelSplitterBottom
      Left = 1
      Top = 408
      Width = 686
      Align = alBottom
      TabOrder = 1
    end
    object pnlTop: TPanel
      Left = 1
      Top = 1
      Width = 686
      Height = 407
      Align = alClient
      TabOrder = 2
      object pnlEntry: TPanel
        Left = 1
        Top = 1
        Width = 684
        Height = 72
        Align = alTop
        TabOrder = 0
        object ednRecord: TFnpNumericEdit
          Left = 112
          Top = 16
          Width = 105
          Height = 23
          TabOrder = 0
          Text = '0'
          Decimals = 0
        end
      end
      object PanelSplitterTop1: TPanelSplitterTop
        Left = 1
        Top = 403
        Width = 684
        Align = alBottom
        TabOrder = 1
      end
      object pnlCentre: TPanel
        Left = 1
        Top = 73
        Width = 684
        Height = 330
        Align = alClient
        TabOrder = 2
        object tlbStats: TNewTextListBox
          Left = 1
          Top = 1
          Width = 682
          Height = 328
          Align = alClient
          ItemHeight = 15
          TabOrder = 0
        end
      end
    end
  end
end
