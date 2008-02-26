{***********************************************************************
*                                                                      *
*       dbfMemoFileDisplay.pas                                         *
*                                                                      *
*       (C) Copyright 1982-2003  Bruce K. Christensen                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Unit dbfMemoFileDisplay ;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  ExtCtrls ,  StdCtrls, Grids ,

  bcClasses        ,
  bcPanelSplitters ,

  dbfCommon    ,
  dbfStructure ,

  KHexEditor   ;

type
  TfrmMemoFileDisplay = class(TForm)
    pnlButtons: TPanel;
    PanelSplitterTop1: TPanelSplitterTop;
    pnlMain: TPanel;
    GroupBox1: TGroupBox;
    rbtAsciiMode: TRadioButton;
    rbtHexMode: TRadioButton;
    heMemo: TKHexEditor;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  Protected
    oOwner : TComponent ;

  Private
     xDbfPtr : pTxBase ;

  Public
    Constructor Create(aOwner : TComponent ;
                       xPtr   : pTxBase     ) ; reintroduce ;

  End ;

Var
  frmMemoFileDisplay : TfrmMemoFileDisplay ;

implementation

{$R *.dfm}

Constructor TfrmMemoFileDisplay.Create(aOwner : TComponent ;
                                       xPtr   : pTxBase     ) ;
  Begin
    CheckDatabasePtr(xPtr) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr ;
  End ;


{***********************************************************************
*                                                                      *
*       TfrmMemoFileDisplay.FormShow                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMemoFileDisplay.FormShow(Sender: TObject);
  Begin  { TfrmMemoFileDisplay.FormShow }
    xdbfPtr^.GetMemoFileVar^.SeekToStart ;
    heMemo.LoadFromStream(xdbfPtr^.GetMemoFileVar^) ;
  End ;  { TfrmMemoFileDisplay.FormShow }


{***********************************************************************
*                                                                      *
*       TfrmMemoFileDisplay.FormClose                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMemoFileDisplay.FormClose(    Sender : TObject      ;
                                        var Action : TCloseAction  ) ;
  Begin  { TfrmMemoFileDisplay.FormClose }
    If heMemo.Modified then
      Begin
        // heMemo.SaveToFile(xdbfPtr^.GetMemoFileName) ;
        xdbfPtr^.GetMemoFileVar^.SeekToStart ;
        heMemo.SaveToStream(xdbfPtr^.GetMemoFileVar^) ;
      End ;
  End ;  { TfrmMemoFileDisplay.FormClose }

End.
