{***********************************************************************
*                                                                      *
*       dbfHeaderEdit.pas                                              *
*                                                                      *
*       (C) Copyright 1990-2005 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfHeaderEdit ;

Interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons ,

  JvEdit       ,
  JvExStdCtrls ,
  JvListBox    ,

  bcClasses         ,
  bcMemoryUtilities ,
  bcPopCalendar     ,
  bcStringUtilities ,
  bcNewList         ,
  bcPanelSplitters  ,

  dbfCommon    ,
  dbfStructure   ;

type
  TfrmHeaderEdit = class(TForm)
    pnlMain: TPanel;
    pnlFileName: TPanel;
    Label4: TLabel;
    edtFileName: TJvEdit;
    PanelSplitterTop1: TPanelSplitterTop;
    pnlBottom: TPanel;
    PanelSplitterBottom1: TPanelSplitterBottom;
    pnlCentre: TPanel;
    Label1: TLabel;
    edLastUpdate: TJvEdit;
    btnLastUpdate: TBitBtn;
    pnlRight: TPanel;
    pnlRightTop: TPanel;
    PanelSplitterTop2: TPanelSplitterTop;
    btnFieldScan: TBitBtn;
    tlbErrors: TNewTextListBox;
    procedure btnLastUpdateClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure btnFieldScanClick(Sender: TObject);

  Protected
    oOwner : TComponent ;

  Private  { Private declarations }
    xDbfPtr : pTxBase ;

  Public  { Public declarations }
    Constructor Create(AOwner : TComponent ;
                       xPtr   : pTxBase     ) ; ReIntroduce ;
    Destructor Destroy ; override ;
    
    Procedure PopupCalendar(edt : TJvEdit) ;
  End;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfmHeaderEdit.Create                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Constructor TfrmHeaderEdit.Create(aOwner : TComponent ;
                                  xPtr   : pTxBase     ) ;
  Begin
    CheckDatabasePtr(xPtr) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr ;
  End ;


{***********************************************************************
*                                                                      *
*       TfmHeaderEdit.Destroy                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Destructor TfrmHeaderEdit.Destroy ;
  Begin  { TfrmHeaderEdit.Destroy }
    Inherited Destroy ;
  End ;  { TfrmHeaderEdit.Destroy }


{***********************************************************************
*                                                                      *
*       TfmHeaderEdit.PopupCalendar                                    *
*                                                                      *
*         Pop up calendar for specified date edit.                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmHeaderEdit.PopupCalendar(edt : TJvEdit) ;
  Var
    dDate : TDateTime ;

  Begin  { TfrmHeaderEdit.PopupCalendar }
    dDate := ShowPopCal(StrToDateChk(edt.Text)) ;
    If dDate > 0 then
      edt.Text := DateToStr(dDate)  // a valid date entered
    Else
      edt.Text := DateToStr(SysUtils.Date) ;                            
  End ;  { TfrmHeaderEdit.PopupCalendar }


{***********************************************************************
*                                                                      *
*       TfmHeaderEdit.btnLastUpdateClick                               *
*                                                                      *
*         Pop up calendar for Last Update date.                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmHeaderEdit.btnLastUpdateClick(Sender : TObject) ;
  Begin  { TfrmHeaderEdit.btnLastUpdateClick }
    PopupCalendar(edLastUpdate) ;
  End ;  { TfrmHeaderEdit.btnLastUpdateClick }


{***********************************************************************
*                                                                      *
*       TfmHeaderEdit.FormActivate                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmHeaderEdit.FormActivate(Sender : TObject) ;
  Begin  { TfrmHeaderEdit.FormActivate }
    edtFileName.Text := xDbfPtr^.GetDataFileName ;
    edtFileName.Repaint ;

    With xDbfPtr^ do
      If GetLastUpdate(True) > 0 then
        edLastUpdate.Text := DateToStr(GetLastUpdate(False))
      Else
        Begin
          edLastUpdate.Text := DateToStr(SysUtils.Date) ;
        End ;
  End ;  { TfrmHeaderEdit.FormActivate }


{***********************************************************************
*                                                                      *
*       TfmHeaderEdit.FormShow                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmHeaderEdit.FormShow(Sender : TObject) ;
  Begin  { TfrmHeaderEdit.FormShow }
    xDbfPtr^.GetLastUpdate(True) ;
  End ;  { TfrmHeaderEdit.FormShow }


{***********************************************************************
*                                                                      *
*       TfrmHeaderEdit.btnFieldScanClick                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmHeaderEdit.btnFieldScanClick(Sender: TObject);
  Var
    nField : Integer ;
    
  Begin  { TfrmHeaderEdit.btnFieldScanClick }
    tlbErrors.Clear ;

    With xDbfPtr^ do
      For nField := 0 to GetFieldCount do
        Begin
          If not VerifyHeaderField(nField) then
            Begin
              tlbErrors.Items.Add(FieldCreateStr(nField));
            End ;
        End ;
  End ;  { TfrmHeaderEdit.btnFieldScanClick }

End.
