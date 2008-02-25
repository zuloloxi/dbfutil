{***********************************************************************
*                                                                      *
*       dbfScanInfoDBV.pas                                             *
*                                                                      *
*       (C) Copyright 1982-1999 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfScanInfoDBV ;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids ,
  StdCtrls,

  JvGrids , JvExGrids ,

  bcClasses        ,
  bcLists          ,
  ngColourGrid     ,
  bcMathUtilities      ,
  bcPanelSplitters ,
  ngStringGrid     ,

  dbfCommon    ,
  dbfStructure, ngSortGrid   ;

Type
  TfrmScanInfoDBV = Class(TForm)
    pnlMain    : TPanel ;
    pnlButtons : TPanel ;
    pnlTop     : TPanel ;

    PanelSplitterBottom1 : TPanelSplitterBottom ;
    PanelSplitterTop1    : TPanelSplitterTop    ;
    btnScan: TButton;
    grdMain: TngStringGrid;

    Procedure grdMainDrawCell(Sender : TObject        ;
                              aCol ,
                              aRow   : Integer        ;
                              Rect   : TRect          ;
                              State  : TGridDrawState  ) ;
    procedure btnScanClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  Protected
    xDbfPtr : pTxBase  ;
    oOwner  : TComponent ;

  Private
    oRecList     : TIntegerList ;
    oFldList     : TIntegerList ;

    oOffset      : TIntegerList ;
    oSize        : TIntegerList ;
    oSignature   : TIntegerList ;
    oCompression : TIntegerList ;

    Procedure BuildLists ;

  Public
    Constructor Create(aOwner : TComponent ;
                       xPtr   : pTxBase     ) ; reintroduce ;

    Destructor Destroy ; override ;
  End ;


Implementation
  Const
    { Special row numbers }
    nTitleRow = 0 ;

    { Column numbers }
    nCount       = 0 ;
    nRecordNo    = 1 ;
    nFieldNo     = 2 ;
    nOffset      = 3 ;
    nSize        = 4 ;
    nSignature   = 5 ;
    nCompression = 6 ;

    ColumnNames : Array[nCount..nCompression] of String[11] =
                      ('No.'        ,
                       'Record'     ,
                       'Field No'   ,
                       'Offset'     ,
                       'Size'       ,
                       'Signature'  ,
                       'Compression' ) ;

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TRecordDisplayForm.Create                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmScanInfoDBV.Create(aOwner : TComponent ;
                                   xPtr   : pTxBase    ) ;
  Begin  { TfrmScanInfoDBV.Create }
    CheckDatabasePtr(xPtr) ;

    Inherited Create(oOwner) ;

    xDbfPtr := xPtr   ;
    oOwner  := aOwner ;

    oRecList     := TIntegerList.Create ;
    oFldList     := TIntegerList.Create ;

    oOffset      := TIntegerList.Create ;
    oSize        := TIntegerList.Create ;
    oSignature   := TIntegerList.Create ;
    oCompression := TIntegerList.Create ;
  End ;  { TfrmScanInfoDBV.Create }


{***********************************************************************
*                                                                      *
*       TfrmScanInfoDBV.Destroy                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmScanInfoDBV.Destroy ;
  Begin  { TfrmScanInfoDBV.Destroy }
    FreeAndNil(oRecList)     ;
    FreeAndNil(oFldList)     ;
    FreeAndNil(oOffset)      ;
    FreeAndNil(oSize)        ;
    FreeAndNil(oSignature)   ;
    FreeAndNil(oCompression) ;

    Inherited Destroy ;
  End ;  { TfrmScanInfoDBV.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmScanInfoDBV.grdMainDrawCell                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmScanInfoDBV.grdMainDrawCell(Sender : TObject        ;
                                          aCol ,
                                          aRow   : Integer        ;
                                          Rect   : TRect          ;
                                          State  : TGridDrawState  ) ;
  Var
    cCell  : String     ;
//    oAlign : TAlignment ;

  Begin  { TfrmScanInfoDBV.grdMainDrawCell }
    cCell  := ''     ;

    With (Sender as TngStringGrid).Canvas do
      Begin
        If (aRow = nTitleRow) then
          Begin
            cCell  := ColumnNames[aCol] ;
//            oAlign := taCenter          ;
          End
        Else
          Begin
            Case aCol of
              nCount       : cCell := IntToStr(aRow) ;
              nRecordNo    : cCell := IntToStr(oRecList[aRow - 1]) ;
              nFieldNo     : ;
              nOffset      : ;
              nSize        : ;
              nSignature   : ;
              nCompression : ;
            End ;

//            oAlign := taRightJustify ;
          End ;

        // (Sender as TngStringGrid).DrawStr(Rect , cCell , oAlign) ;
        (Sender as TngStringGrid).Cells[aCol , aRow] := cCell ;
      End ;
  End ;  { TfrmScanInfoDBV.grdMainDrawCell }


{***********************************************************************
*                                                                      *
*       TfrmScanInfoDBV.BuildLists                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmScanInfoDBV.BuildLists ;
  Var
    nRec : Integer ;
    nFld : Integer ;

  Begin  { TfrmScanInfoDBV.BuildLists }
    oRecList.Clear     ;
    oFldList.Clear     ;
    oOffset.Clear      ;
    oSize.Clear        ;
    oSignature.Clear   ;
    oCompression.Clear ;

    With xDbfPtr^ do
      For nRec := 1 to GetTotalRecords do
        Begin
          GetRecord(nRec) ;

          For nFld := 1 to GetFieldCount do
            Begin
            End ;
          oRecList.Add(nRec) ;
        End ;

    grdMain.RowCount := oRecList.Count + 1 ;
    Repaint ;
  End ;  { TfrmScanInfoDBV.BuildLists }


{***********************************************************************
*                                                                      *
*       TfrmScanInfoDBV.btnScanClick                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmScanInfoDBV.btnScanClick(Sender : TObject) ;
  Begin  { TfrmScanInfoDBV.btnScanClick }
    BuildLists ;
  End ;  { TfrmScanInfoDBV.btnScanClick }

  
{***********************************************************************
*                                                                      *
*       TfrmScanInfoDBV.FormActivate                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmScanInfoDBV.FormActivate(Sender : TObject) ;
  Var
    nCol : Integer ;

  Begin  { TfrmScanInfoDBV.FormActivate }
    For nCol := nCount to nCompression do
      With grdMain do
        Begin
          SetColumnWidth(nCol ,
                         MaxInteger(GetTextWidthInt(1) ,
                                    (GetTextWidth(ColumnNames[nCol]))* 2) +
                                      (GridLineWidth + 1) * 4) ;
        End ;
        
    BuildLists ;
  End ;  { TfrmScanInfoDBV.FormActivate }

End.
