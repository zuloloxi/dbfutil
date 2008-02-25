object frmConvertFixDBV: TfrmConvertFixDBV
  Left = 200
  Top = 134
  Caption = 'Convert and Fix DBF / DBV'
  ClientHeight = 453
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlue
  Font.Height = -13
  Font.Name = 'Andale Mono'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
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
      DesignSize = (
        686
        41)
      object btnConvert: TBitBtn
        Left = 584
        Top = 10
        Width = 93
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Convert'
        TabOrder = 0
        OnClick = btnConvertClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          04000000000080000000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00999999999999
          99999FFFFFFF8888FFF997FFFFCCCC8888F9977FCCCCCCCC8889977CCCC3CCCC
          C889977CCCC3CCCCC88997CCCC33CCCCC38997CCCC333CCCC38997CCC333CCCC
          C38997CCC3CCCCCC3389977C33333CCC38F9977C33333C3C3FF99777C33C33C3
          7FF9977777CCCC7777F997777777777777799999999999999999}
      end
      object btnRepair: TBitBtn
        Left = 472
        Top = 10
        Width = 93
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Repair'
        TabOrder = 1
        OnClick = btnRepairClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          333333333333333333333333333333333333333FF33333333FF3330033333333
          00333377FF33333377FF300003333330000337777FFFFFF7777F000000000000
          000077777777777777770F88FFFF8FFF88F07F333F33333333370FFF9FFF8FFF
          FF707F337FF333FFFFF70FF999FF800000037F377733377777730FFF9FFF0888
          80337F3373337F3337330FFFFFFF088803337FFFFFFF7FFF7333700000000000
          3333777777777777F33333333339399939333333333337773333333333333393
          3333333333333373333333333333933393333333333333333333333333333393
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
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
      object pnlLeft: TPanel
        Left = 1
        Top = 1
        Width = 336
        Height = 405
        Align = alLeft
        TabOrder = 0
        object Panel1: TPanel
          Left = 1
          Top = 1
          Width = 334
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Panel1'
          TabOrder = 0
        end
        object PanelSplitterTop1: TPanelSplitterTop
          Left = 1
          Top = 42
          Width = 334
          TabOrder = 1
        end
        object tlbFields: TNewTextListBox
          Left = 1
          Top = 45
          Width = 334
          Height = 359
          Align = alClient
          ItemHeight = 15
          TabOrder = 2
        end
      end
      object pnlRight: TPanel
        Left = 337
        Top = 1
        Width = 348
        Height = 405
        Align = alClient
        TabOrder = 1
        object Panel2: TPanel
          Left = 1
          Top = 1
          Width = 346
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Panel2'
          TabOrder = 0
        end
        object PanelSplitterTop2: TPanelSplitterTop
          Left = 1
          Top = 42
          Width = 346
          TabOrder = 1
        end
        object tlbNewFields: TNewTextListBox
          Left = 1
          Top = 45
          Width = 346
          Height = 359
          Align = alClient
          ItemHeight = 15
          TabOrder = 2
        end
      end
    end
  end
end
