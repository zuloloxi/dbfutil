{***********************************************************************
*                                                                      *
*       dbfDataGrid.pas                                                *
*                                                                      *
*       (C) Copyright 1982-2003 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfDataGrid ;

interface

uses
  Windows , Messages , SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids, StdCtrls, DBCtrls,

  bcClasses         ,
  bcMathUtilities   ,
  bcStringUtilities ,

  ngSortGrid   ,
  ngStringGrid ,

  dbfCommon    ,
  dbfStructure   ;

Type
  TfrmDataGrid =
    Class(TForm)
      pnlMain   : TPanel      ;
      pnlBottom : TPanel      ;
      pnlTop    : TPanel      ;
      pnlMiddle : TPanel      ;
      sbrNavigate : TScrollBar ;
    grdData: TngStringGrid;
//    grdData: TngStringGrid;

    procedure FormActivate(Sender: TObject);
    procedure grdDataDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sbrNavigateChange(Sender: TObject);
    procedure Navigator1NavNextProc(Sender: TObject);
    procedure Navigator1NavPriorProc(Sender: TObject);
    procedure Navigator1NavFirstProc(Sender: TObject);
    procedure Navigator1NavLastProc(Sender: TObject);
    function Navigator1BofProc(Sender: TObject): Boolean;
    function Navigator1EofProc(Sender: TObject): Boolean;

    Protected
      oOwner  : TComponent ;

    Private
      xDbfPtr : pTxBase ;

      nFirstRec : Integer ;

    Public
      Constructor Create(aOwner : TComponent ;
                         xPtr   : pTxBase     ) ; reintroduce ;
      Destructor Destroy ; override ;
    End;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfrmDataGrid.Create                                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmDataGrid.Create(aOwner : TComponent ;
                                xPtr   : pTxBase     ) ;
  Begin  { TfrmDataGrid.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner  := aOwner ;
    xDbfPtr := xPtr   ;

    nFirstRec := 1 ;

    Inherited Create(aOwner) ;
  End ;  { TfrmDataGrid.Create }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.Destroy                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmDataGrid.Destroy ;
  Begin  { TfrmDataGrid.Destroy }
    Inherited Destroy ;
  End ;  { TfrmDataGrid.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.FormActivate                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmDataGrid.FormActivate(Sender : TObject) ;
  Var
    nCol   ,
    nWidth ,
    nField   : Integer ;

    cField : String ;

  Begin  { TfrmDataGrid.FormActivate }
    With grdData , xDbfPtr^ do
      Begin
        ColCount := xDbfPtr^.GetFieldCount + 2 ;

        For nField := 0 to GetFieldCount do
          Begin
            nCol := nField + 1 ;

            cField := GetFieldName(nField) ;

            nWidth := MaxInteger(Length(cField) , GetFieldWidth(nField)) + 1 ;
            nWidth := GetTextWidth(StrMake('H' , nWidth)) + (GridLineWidth * 2) ;

            SetColumnWidth(nCol , nWidth) ;
          End ;
      End ;
  End ;  { TfrmDataGrid.FormActivate }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.grdDataDrawCell                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmDataGrid.grdDataDrawCell(Sender : TObject        ;
                                       aCol ,
                                       aRow   : Integer        ;
                                       Rect   : TRect          ;
                                       State  : TGridDrawState  ) ;
  Var
    x    ,
    nRec ,
    nField   : Integer    ;
    bShowRed : Boolean    ;
    cStr     : String     ;
    oAlign   : TAlignment ;

    oColour : TColor ;

  Begin  { TfrmDataGrid.grdDataDrawCell }
    cStr := '' ;

    bShowRed := False                ;
    nField   := aCol - 1             ;
    oAlign   := taRightJustify       ;

    With (Sender as TngStringGrid) , Canvas , xDbfPtr^ do
      Begin
        nRec := aRow - FixedRows + nFirstRec - 1 ;
        If (nRec > xDbfPtr^.GetRecordCount) then
          Exit ;

        Case aRow of
          0 : Begin
                cStr := GetFieldName(nField) ;
                oAlign := taCenter ;
              End ;

          1 : If ValidFieldNumber(nField) then
                Begin
                  cStr := GetFieldType(nField) ;
                  oAlign := taCenter ;
                End ;
        Else
          Begin
            GetRecord(nRec) ;

            Case aCol of
              0 : If nRec > 0 then
                    cStr := IntToStr(nRec)
                  Else
                    cStr := '' ;
            Else
              If ValidFieldNumber(nField) then
                Begin
                  cStr := GetFieldDisplayString(GetRecordPtr ,
                                                nField       ,
                                                False         ) ;

                  Case xDbfPtr^.GetFieldType(nField) of
                    'C' : oAlign := taLeftJustify ;

                    'B' ,
                    'F' ,
                    'I' ,
                    'N'   : oAlign := taRightJustify ;

                    'D' ,
                    'Y'   : oAlign := taCenter ;
                  Else
                    oAlign := taLeftJustify ;
                  End ;
                End
              Else
                Begin
                  ShowMessage('adfafasdf') ;
                End ;
            End ;
          End ;
        End ;

        oColour := Font.Color ;
        If bShowRed then
          Font.Color := clRed
        Else
          Font.Color := clBlue ;

        Case oAlign of
          taLeftJustify : x := 2 ;

          taCenter :
            Begin
              x := (Rect.Right - Rect.Left - TextWidth(cStr)) div 2 ;
              If x < 0 then
                x := 2 ;
            End ;

          taRightJustify :
            Begin
              x := (Rect.Right - Rect.Left - TextWidth(cStr) - 2);
              If x < 0 then
                x := 2 ;
            End ;
        Else
          x := 2 ;
        End ;

        If Length(Trim(cStr)) > 0 then
          // TextOut(Rect.Left + x , Rect.Top + grdData.VerticalCentrePos(aRow) , cStr) ;
          TextOut(Rect.Left + x , (Rect.Top + 2) , cStr) ;
        Font.Color := oColour ;
      End ;
  End ;  { TfrmDataGrid.grdDataDrawCell }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.sbrNavigateChange                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmDataGrid.sbrNavigateChange(Sender : TObject) ;
  Begin  { TfrmDataGrid.sbrNavigateChange }
    nFirstRec := nFirstRec + (grdData.RowCount - 2) ;
    grdData.Repaint ;
  End ;  { TfrmDataGrid.sbrNavigateChange }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.Navigator1NavNextProc                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmDataGrid.Navigator1NavNextProc(Sender : TObject) ;
  Begin  { TfrmDataGrid.Navigator1NavNextProc }
    If xDbfPtr <> nil then
      With xDbfPtr^ do
        Begin
          SetRecordNo(nFirstRec) ;

          If (nFirstRec + 1) > (nFirstRec + (grdData.RowCount - 2) + 1) then
            Begin
              If not IsAtEOF(nFirstRec) then
                Begin
                  nFirstRec := nFirstRec + 1 ;
                  SetRecordNo(nFirstRec) ;
                  grdData.Repaint ;
                End ;
            End ;
        End ;
  End ;  { TfrmDataGrid.Navigator1NavNextProc }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.Navigator1NavPriorProc                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmDataGrid.Navigator1NavPriorProc(Sender : TObject) ;
  Begin  { TfrmDataGrid.Navigator1NavPriorProc }
    With xDbfPtr^ do
      If not IsAtBOF(nFirstRec) then
        Begin
          nFirstRec := nFirstRec - 1 ;
          If nFirstRec < 1 then
            nFirstRec := 1 ;
          xDbfPtr^.SetRecordNo(nFirstRec) ;
          grdData.Repaint ;
        End ;
  End ;  { TfrmDataGrid.Navigator1NavPriorProc }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.Navigator1NavFirstProc                            *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

Procedure TfrmDataGrid.Navigator1NavFirstProc(Sender: TObject);
  Begin  { TfrmDataGrid.Navigator1NavFirstProc }
    With xDbfPtr^ do
      If not IsAtBOF(nFirstRec) then
        Begin
          nFirstRec := 1 ;
          SetRecordNo(nFirstRec) ;
          grdData.Repaint ;
        End ;
  End ;  { TfrmDataGrid.Navigator1NavFirstProc }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.Navigator1NavLastProc                             *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

Procedure TfrmDataGrid.Navigator1NavLastProc(Sender: TObject);
  Begin  { TfrmDataGrid.Navigator1NavLastProc }
    With xDbfPtr^ do
      If not IsAtEOF(nFirstRec) then
        Begin
          nFirstRec := GetRecordCount - (grdData.RowCount - 2) + 1 ;
          If nFirstRec < 1 then
            nFirstRec := 1 ;
          SetRecordNo(nFirstRec) ;
          grdData.Repaint ;
        End ;
  End ;  { TfrmDataGrid.Navigator1NavLastProc }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.Navigator1BofProc                                 *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

Function TfrmDataGrid.Navigator1BofProc(Sender: TObject) : Boolean ;
  Begin  { TfrmDataGrid.Navigator1BofProc }
    With xDbfPtr^ do
      If GetRecordCount > 0 then
        Result := (nFirstRec = 1)
      Else
        Result := True ;
  End ;  { TfrmDataGrid.Navigator1BofProc }


{***********************************************************************
*                                                                      *
*       TfrmDataGrid.Navigator1EofProc                                 *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

Function TfrmDataGrid.Navigator1EofProc(Sender: TObject) : Boolean ;
  Begin  { TfrmDataGrid.Navigator1EofProc }
    With xDbfPtr^ do
      If GetRecordCount > 0 then
        Result := (nFirstRec = GetRecordCount)
      Else
        Result := True ;
  End ;  { TfrmDataGrid.Navigator1EofProc }

End.
