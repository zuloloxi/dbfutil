object frmScanInfoDBV: TfrmScanInfoDBV
  Left = 192
  Top = 138
  Caption = 'Scan Info DBV'
  ClientHeight = 453
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
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
      object btnScan: TButton
        Left = 600
        Top = 10
        Width = 75
        Height = 25
        Caption = '&Scan'
        TabOrder = 0
        OnClick = btnScanClick
      end
    end
    object pnlTop: TPanel
      Left = 1
      Top = 1
      Width = 686
      Height = 41
      Align = alTop
      TabOrder = 1
    end
    object PanelSplitterBottom1: TPanelSplitterBottom
      Left = 1
      Top = 408
      Width = 686
      Align = alBottom
      TabOrder = 2
    end
    object PanelSplitterTop1: TPanelSplitterTop
      Left = 1
      Top = 405
      Width = 686
      Align = alBottom
      TabOrder = 3
    end
    object grdMain: TngStringGrid
      Left = 1
      Top = 42
      Width = 686
      Height = 363
      Align = alClient
      TabOrder = 4
      CaseSensitive = False
      AlignmentHorz = taLeftJustify
      AlignmentVert = taTopJustify
      ProportionalScrollBars = True
      ExtendedKeys = False
      SortOnClick = True
      ArrowSort = True
      FooterFont.Charset = DEFAULT_CHARSET
      FooterFont.Color = clWindowText
      FooterFont.Height = -11
      FooterFont.Name = 'Tahoma'
      FooterFont.Style = []
      PrintOptions.Orientation = poPortrait
      PrintOptions.PageTitleMargin = 0
      PrintOptions.PageFooter = 'date|time|page'
      PrintOptions.HeaderSize = 10
      PrintOptions.FooterSize = 7
      PrintOptions.DateFormat = 'd-mmm-yyyy'
      PrintOptions.TimeFormat = 'h:nn'
      PrintOptions.FromRow = 0
      PrintOptions.ToRow = 0
      PrintOptions.BorderStyle = bsNone
      PrintOptions.MarginBottom = 0
      PrintOptions.MarginLeft = 0
      PrintOptions.MarginTop = 0
      PrintOptions.MarginRight = 0
      WordWrap = False
      SelectedX = 0
      SelectedY = 0
      ExplicitLeft = 200
      ExplicitTop = 160
      ExplicitWidth = 320
      ExplicitHeight = 120
    end
  end
end
