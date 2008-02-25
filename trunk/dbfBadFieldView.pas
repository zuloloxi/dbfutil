{***********************************************************************
*                                                                      *
*       dbfBadFieldView.pas                                            *
*                                                                      *
*       (C) Copyright 1982-2003 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfBadFieldView ;

Interface

Uses
  Windows , Messages , SysUtils , Classes  , Graphics , Controls ,
  Forms   , Dialogs  , ExtCtrls , StdCtrls ,

  JvEdit       ,
  JvExStdCtrls ,
  JvListBox    ,

  bcClasses  ,
  bcNewList  ,
  bcStringUtilities ,

  dbfRecordDisplay ,
  dbfStructure     ,
  dbfCommon          ;

Type
  TfmBadFieldView = class(TForm)
    pnlMain      : TPanel          ;
    pnlTop       : TPanel          ;
    pnlSplitter  : TPanel          ;
    pnlBottom    : TPanel          ;
    edFieldName  : TJvEdit         ;
    lbRecs       : TNewTextListBox ;
    edtFieldType : TJvEdit         ;

    procedure lbRecsDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

    Protected
      oOwner : TComponent ;

    Private
      xDataBasePtr : pTxBase  ;
      nField       : Integer    ;

    Public
      Constructor Create(AOwner : TComponent ;
                         xPtr   : pTxBase    ;
                         nFld   : Integer     ) ; ReIntroduce ;
      Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfmBadFieldlist.Create                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfmBadFieldView.Create(aOwner : TComponent ;
                                   xPtr   : pTxBase    ;
                                   nFld   : Integer     ) ;
  Begin  { TfmBadFieldlist.Create }
    Inherited Create(oOwner) ;
    
    CheckDatabasePtr(xPtr) ;

    oOwner := AOwner ;

    xDataBasePtr := xPtr   ;
    nField       := nFld   ;
  End ;  { TfmBadFieldlist.Create }


{***********************************************************************
*                                                                      *
*       TfmBadFieldlist.Destroy                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfmBadFieldView.Destroy ;
  Begin  { TfmBadFieldlist.Destroy }
    Inherited Destroy ;
  End ;  { TfmBadFieldlist.Destroy }


{***********************************************************************
*                                                                      *
*       TfmBadFieldView.FormActivate                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmBadFieldView.FormActivate(Sender : TObject) ;
  Var
    nI     : Integer ;
    nCount : Integer ;

  Begin  { TfmBadFieldView.FormActivate }
    HourGlassCursor ;

    lbRecs.Clear ;
    lbRecs.Items.Clear ;

    With xDataBasePtr^ do
      Begin
        edFieldName.Text  := GetFieldName(nField) ;
        edtFieldType.Text := GetFieldType(nField) ;

        edFieldName.Repaint  ;
        edtFieldType.Repaint ;
      End ;

    With xDataBasePtr^ , GetDataAreaPtr^ do
      Begin
        nCount := dbfFieldLists[nField].Count ;
        
        If nCount > 0 then
          Begin
            For nI := 0 to (nCount - 1) do
              lbRecs.Items.Add(PadLeft(IntToStr(dbfFieldLists[nField].Items[nI]) , dbfMaxNameLen)) ;
          End
        Else
          lbRecs.Items.Add('< No items >') ;
      End ;

    lbRecs.Repaint ;
    NormalCursor ;  { Restore cursor to normal. }
  End ;  { TfmBadFieldView.FormActivate }


{***********************************************************************
*                                                                      *
*       TfmBadFieldView.lbRecsDblClick                                 *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmBadFieldView.lbRecsDblClick(Sender : TObject) ;
  Var
    nRec : Integer ;

  Begin  { TfmBadFieldView.lbRecsDblClick }
    nRec := StrToInt(Trim(lbRecs.Items[lbRecs.ItemIndex])) ;

    With TfrmRecordDisplay.Create(Application , xDataBasePtr , nRec) do
      Try
        SetRecEnable(False) ;
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfmBadFieldView.lbRecsDblClick }

End.
