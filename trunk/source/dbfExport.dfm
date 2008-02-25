object frmExport: TfrmExport
  Left = 202
  Top = 242
  Caption = 'dbUtil Export'
  ClientHeight = 257
  ClientWidth = 477
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Andale Mono'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 477
    Height = 257
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    object pnlButtons: TPanel
      Left = 1
      Top = 215
      Width = 475
      Height = 41
      Align = alBottom
      TabOrder = 0
      object btnExport: TBitBtn
        Left = 422
        Top = 10
        Width = 75
        Height = 25
        Caption = '&Export'
        TabOrder = 0
        OnClick = btnExportClick
      end
    end
    object PanelSplitterBottom1: TPanelSplitterBottom
      Left = 1
      Top = 212
      Width = 475
      Align = alBottom
      TabOrder = 1
    end
    object pnlTop: TPanel
      Left = 1
      Top = 1
      Width = 475
      Height = 211
      Align = alClient
      TabOrder = 2
      object Label1: TLabel
        Left = 24
        Top = 51
        Width = 120
        Height = 15
        Alignment = taRightJustify
        Caption = 'Output Filename'
      end
      object Label2: TLabel
        Left = 111
        Top = 116
        Width = 128
        Height = 15
        Alignment = taRightJustify
        Caption = 'Exported Records'
      end
      object FnpNumericEdit1: TFnpNumericEdit
        Left = 240
        Top = 112
        Width = 97
        Height = 23
        TabOrder = 0
        Text = '0.00'
      end
      object sleOutFileName: TJvEdit
        Left = 158
        Top = 45
        Width = 233
        Height = 23
        Modified = False
        TabOrder = 1
      end
    end
  end
end
