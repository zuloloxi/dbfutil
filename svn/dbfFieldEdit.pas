{***********************************************************************
*                                                                      *
*       dbfFieldEdit.pas                                               *
*                                                                      *
*       (C) Copyright 1982-2003 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfFieldEdit ;

interface

Uses
  Windows , Messages , SysUtils , Classes  , Graphics , Controls ,
  Forms   , Dialogs  , StdCtrls , ExtCtrls , Buttons  ,

  bcClasses         ,
  bcStringUtilities ,
  bcScreenUtilities ,

  dbfCommon    ,
  dbfStructure   ;

type
  TfmFieldEdit =
    Class(TForm)
      pnlMain: TPanel;
      edCurrent: TEdit;
      Label1: TLabel;
      edNew: TEdit;
      Label2: TLabel;
      btnPost: TBitBtn;
      procedure btnPostClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    Protected
      oOwner : TComponent ;

    Private
      xDbfPtr : pTxBase ;

      nField    : Integer ;
      cFldName  : String  ;
      cFldType  : Char    ;
      nFldWidth : Integer ;
      nFldDec   : Integer ;

    Public
      Constructor Create(AOwner : TComponent ;
                         xPtr   : pTxBase    ;
                         nFld   : Integer     ) ; Reintroduce ;
      Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfmFieldEdit.Create                                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfmFieldEdit.Create(AOwner : TComponent ;
                                xPtr   : pTxBase    ;
                                nFld   : Integer     ) ;
  Begin  { TfmFieldEdit.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := AOwner ;
    Inherited Create(oOwner) ;

    nField  := nFld ;
    xDbfPtr := xPtr ;
  End ;  { TfmFieldEdit.Create }


{***********************************************************************
*                                                                      *
*       TfmFieldEdit.Destroy                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfmFieldEdit.Destroy ;
  Begin  { TfmFieldEdit.Destroy }
    Inherited Destroy ;
  End ;  { TfmFieldEdit.Destroy }


{***********************************************************************
*                                                                      *
*       TfmFieldEdit.FormActivate                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldEdit.FormActivate(Sender : TObject) ;
  Begin  { TfmFieldEdit.FormActivate }
    FormCentre(@Self) ;
    
    With xDbfPtr^ do
      Begin
        cFldName  := GetFieldName(nField) ;
        cFldType  := GetFieldType(nField) ;
        nFldWidth := GetFieldWidth(nField) ;
        nFldDec   := GetFieldDecimals(nField) ;

        Caption  := 'Editing field: ' + GetFieldName(nField) ;

        edCurrent.Text      := GetFldStr(nField) ;
        edCurrent.MaxLength := GetFieldWidth(nField) ;
      End ;
  End ;  { TfmFieldEdit.FormActivate }


{***********************************************************************
*                                                                      *
*       TfmFieldEdit.btnPostClick                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldEdit.btnPostClick(Sender : TObject) ;
  Var
    cFieldStr : String ;

  Begin  { TfmFieldEdit.btnPostClick }
    cFieldStr := edNew.Text ;
    Case cFldType of
      'C' : cFieldStr := PadR(cFieldStr , nFldWidth) ;
      'N' : cFieldStr := PadL(cFieldStr , nFldWidth) ;
      'L' : cFieldStr := AnsiUpperCase(cFieldStr) ;
    End ;

    With xDbfPtr^ do
      Begin
        SetFldStr(cFieldStr , nField) ;
        PutCurrentRecord ;
      End ;
  End ;  { TfmFieldEdit.btnPostClick }

End.
