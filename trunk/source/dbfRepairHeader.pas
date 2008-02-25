{***********************************************************************
*                                                                      *
*       dbfRepairHeader.pas                                            *
*                                                                      *
*       (C) Copyright 1990-1999 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfRepairHeader ;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls ,

  bcClasses         ,
  bcDialogUtilities ,
  bcNumericEdit     ,
  bcPanelSplitters  ,

  dbfCommon    ,
  dbfStructure   ;

Type
  TfrmRepairHeader = Class(TForm)
    Panel1: TPanel;
    PanelSplitterTop1: TPanelSplitterTop;
    Panel2: TPanel;
    PanelSplitterTop2: TPanelSplitterTop;
    Panel3: TPanel;
    btnFixRecLen: TBitBtn;
    ednCurrentRecLen: TFnpNumericEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ednCalcRecLen: TFnpNumericEdit;
    Label4: TLabel;
    ednCurrentHeaderLen: TFnpNumericEdit;
    Label5: TLabel;
    ednCalcHeaderLen: TFnpNumericEdit;
    btnFixHeaderLen: TBitBtn;
    btnRecordCnt: TBitBtn;
    Label6: TLabel;
    ednCurrentRecordCnt: TFnpNumericEdit;
    Label7: TLabel;
    ednCalcRecordCnt: TFnpNumericEdit;
    sbrStatus: TStatusBar;

    Procedure RepairHeaderButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnFixRecLenClick(Sender: TObject);
    procedure btnFixHeaderLenClick(Sender: TObject);
    procedure btnRecordCntClick(Sender: TObject);

  Protected
    oOwner : TComponent ;

  Private
    xDbfPtr : pTxBase ;

    Procedure DisplayScreen ;
    Procedure UpdateDataHeader ;
    Procedure SetStatusText(cMsg : String) ;

  Public
    Constructor Create(aOwner : TComponent ;
                       xPtr   : pTxBase   ) ; reintroduce ;
    Destructor Destroy ; override ;
  End;


Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TRepairHeaderForm.Create                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Constructor TfrmRepairHeader.Create(aOwner : TComponent ;
                                    xPtr   : pTxBase   ) ;
  Begin
    CheckDatabasePtr(xPtr) ;

    oOwner := AOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr ;
  End ;


{***********************************************************************
*                                                                      *
*       TRepairHeaderForm.Destroy                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Destructor TfrmRepairHeader.Destroy ;
  Begin
    Inherited Destroy ;
  End ;

{***********************************************************************
*                                                                      *
*       TfrmRepairHeader.SetStatusText                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairHeader.SetStatusText(cMsg : String) ;
  Begin  { TfrmRepairHeader.SetStatusText }
    sbrStatus.SimpleText := '' ;
    Sleep(250) ;
    sbrStatus.SimpleText := 'Header written.' ;
  End ;  { TfrmRepairHeader.SetStatusText }


{***********************************************************************
*                                                                      *
*       TRepairHeaderForm.RepairHeaderButtonClick                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairHeader.RepairHeaderButtonClick(Sender: TObject);
  Begin
    xDbfPtr^.RepairHeader;
  End;


{***********************************************************************
*                                                                      *
*       TfrmRepairHeader.FormActivate                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairHeader.FormActivate(Sender : TObject) ;
  Begin  { TfrmRepairHeader.FormActivate }
    DisplayScreen ;
  End ;  { TfrmRepairHeader.FormActivate }


{***********************************************************************
*                                                                      *
*       TfrmRepairHeader.DisplayScreen                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairHeader.DisplayScreen ;
  Begin  { TfrmRepairHeader.DisplayScreen }
    With xDbfPtr^ do
      Begin
        ednCurrentRecLen.SetInteger(GetRecordSize) ;
        ednCalcRecLen.SetInteger(CalcRecordSize) ;

        ednCurrentHeaderLen.SetInteger(GetHeaderSize) ;
        ednCalcHeaderLen.SetInteger(CalcHeaderSize) ;

        ednCurrentRecordCnt.SetInteger(GetTotalRecords) ;
        ednCalcRecordCnt.SetInteger(CalcTotalRecords) ;
      End ;  { With xDbfPtr^ do }
  End ;  { TfrmRepairHeader.DisplayScreen }


{***********************************************************************
*                                                                      *
*       TfrmRepairHeader.UpdateDataHeader                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairHeader.UpdateDataHeader ;
  Begin  { TfrmRepairHeader.UpdateDataHeader }
    With xDbfPtr^ do
      If WriteHeader then
        SetStatusText('Header written.')
      Else
        ShowErrorMessage('Unable to write header.') ;

    DisplayScreen ;
  End ;  { TfrmRepairHeader.UpdateDataHeader }


{***********************************************************************
*                                                                      *
*       TRepairHeaderForm.RepairHeaderButtonClick                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairHeader.btnFixRecLenClick(Sender: TObject);
  Begin  { TfrmRepairHeader.btnFixRecLenClick }
    With xDbfPtr^ do
      Begin
        SetRecordSize(CalcRecordSize) ;
      End ;  { With xDbfPtr^ do }

    UpdateDataHeader ;
  End ;  { TfrmRepairHeader.btnFixRecLenClick }


{***********************************************************************
*                                                                      *
*       TRepairHeaderForm.btnFixHeaderLenClick                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairHeader.btnFixHeaderLenClick(Sender : TObject) ;
  Begin  { TfrmRepairHeader.btnFixHeaderLenClick }
    With xDbfPtr^ do
      Begin
        SetHeaderSize(CalcHeaderSize) ;
      End ;  { With xDbfPtr^ do }

    UpdateDataHeader ;
  End ;  { TfrmRepairHeader.btnFixHeaderLenClick }


{***********************************************************************
*                                                                      *
*       TRepairHeaderForm.btnFixHeaderLenClick                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairHeader.btnRecordCntClick(Sender: TObject);
  Begin  { TfrmRepairHeader.btnRecordCntClick }
    With xDbfPtr^ do
      Begin
        SetTotalRecords(CalcTotalRecords) ;
      End ;  { With xDbfPtr^ do }

    UpdateDataHeader ;
  End ;  { TfrmRepairHeader.btnRecordCntClick }

End.
