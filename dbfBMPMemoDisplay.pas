{***********************************************************************
*                                                                      *
*       dbfBMPMemoDisplay.pas                                          *
*                                                                      *
*       (C) Copyright 1982-2003 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfBMPMemoDisplay ;

Interface

Uses
  Windows , Messages , SysUtils , Classes , Graphics , Controls , Forms ,
  Dialogs , ExtCtrls , ComCtrls ,

  bcClasses         ,
  bcScreenUtilities ,

  dbfStructure ,
  dbfCommon      ;

Type
  TfmBMPMemoDisplay = class(TForm)
    Panel1: TPanel;
    ImageBMP: TImage;

    procedure FormActivate(Sender: TObject);

  Protected
    oOwner : TComponent ;

  Private
    xDbfPtr : pTxBase ;
    nMemo   : Integer ;

  Public
    Constructor Create(AOwner  : TComponent ;
                       xPtr    : pTxBase    ;
                       nMemoNo : Integer     ) ; Reintroduce ;
    Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfmBMPMemoDisplay.Create                                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfmBMPMemoDisplay.Create(aOwner  : TComponent ;
                                     xPtr    : pTxBase    ;
                                     nMemoNo : Integer     ) ;
  Begin  { TfmBMPMemoDisplay.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr    ;
    nMemo   := nMemoNo ;
  End ;  { TfmBMPMemoDisplay.Create }


{***********************************************************************
*                                                                      *
*       TfmBMPMemoDisplay.Destroy                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfmBMPMemoDisplay.Destroy ;
  Begin  { TfmBMPMemoDisplay.Destroy }
    Inherited Destroy ;
  End ;  { TfmBMPMemoDisplay.Destroy }


{***********************************************************************
*                                                                      *
*       TfmBMPMemoDisplay.FormCreate                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmBMPMemoDisplay.FormActivate(Sender: TObject);
  Begin  { TfmBMPMemoDisplay.FormActivate }
    FormCentre(@Self) ;

    With xDbfPtr^ , GetDataAreaPtr^ do
      If nMemo > 0 then
        Begin
          Try
            ReadMemoBMP(nMemo) ;
            ImageBMP.Picture.LoadFromFile(GetBlobFileName) ;
          Except
            ShowMessage('Blob could not be read.') ;
          End ;
        End ;
  End ;  { TfmBMPMemoDisplay.FormActivate }

End.
    