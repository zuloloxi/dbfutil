{***********************************************************************
*                                                                      *
*       dbfStatistics.pas                                              *
*                                                                      *
*       (C) Copyright 1982-1999 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Unit dbfStatistics ;

Interface

Uses
  Windows , Messages , SysUtils , Classes  , Graphics , Controls ,
  Forms   , Dialogs  , ExtCtrls , StdCtrls , Buttons  , DB       ,

  JvListBox    ,
  JvExStdCtrls ,

  bcClasses     ,
  bcDialogUtilities ,
  bcMathUtilities   ,
  bcNewList     ,
  bcNumericEdit ,
  bcStringUtilities ,

  CreateComponents ,

  dbfCommon ,
  dbfStructure ;

Type
  TfmStatistics = Class(TForm)
    btnScan : TBitBtn ;

    Label1 : TLabel ;

    pnlMain        : TPanel ;
    pnlBottom      : TPanel ;
    pnlTop         : TPanel ;
    Panel2         : TPanel ;
    pnlRight       : TPanel ;
    pnlLeft        : TPanel ;
    Panel1         : TPanel ;
    pnlBottomRight : TPanel ;

    enRecsUsed: TFnpNumericEdit;
    lbFields: TNewTextListBox;

    Procedure btnScanClick(Sender: TObject);
    Procedure lbFieldsClick(Sender: TObject);
    Procedure lbFieldsDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  Protected
    Procedure CreateNumericFlds(nDecimals : Integer) ;
    Procedure CreateStringFlds(nWidth : Integer) ;
    Procedure CreateDateFields(nField : Integer) ;

  Private  { Private declarations }
    xDataBasePtr : pTxBase ;

    cFldType : Char     ;
    nFldDec  : Integer  ;
    cFldStr  : String   ;
    nField   : Integer  ;
    nFloat   : Extended ;

    enMinN : TFnpNumericEdit ;
    enMaxN : TFnpNumericEdit ;
    enAvgN : TFnpNumericEdit ;

    esMinS : TEdit ;
    esMaxS : TEdit ;

    edMinD : TEdit ;
    edMaxD : TEdit ;
    edAvgD : TEdit ;

    lbOne   : TLabel ;
    lbTwo   : TLabel ;
    lbThree : TLabel ;

  Public
    Constructor Create(AOwner : TComponent ;
                       xPtr   : pTxBase     ) ; reIntroduce ;
    Destructor Destroy ; override ;

    Procedure ClearFlds ;
    Procedure FreeFlds  ;
    Procedure ZapFields ;
  End ;

Type
  pTFieldStat = ^TFieldStat ;
  TFieldStat = Record
                 tFldType : TFieldType ;
                 nSize    : Integer    ;  { size of stat field type           }
                 nRecords : Integer    ;  { record uses in this field's stats }
                 pMin     : Pointer    ;
                 pMax     : Pointer    ;
                 pAvg     : Pointer    ;
               End ;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfmStatistics.Create                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfmStatistics.Create(aOwner : TComponent ;
                                 xPtr   : pTxBase    ) ;
  Begin  { TfmStatistics.Create }
    CheckDatabasePtr(xPtr) ;
    
    Inherited Create(aOwner) ;

    xDataBasePtr := xPtr ;

    ZapFields ;
  End ;  { TfmStatistics.Create }


{***********************************************************************
*                                                                      *
*       TfmStatistics.Destroy                                          *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfmStatistics.Destroy ;
  Begin  { TfmStatistics.Destroy }
    FreeFlds ;

    Try
      Inherited Destroy ;
    Except
      ShowMessage('Destroy exception.') ;
    End ;
  End ;  { TfmStatistics.Destroy }


{***********************************************************************
*                                                                      *
*       TfmStatistics.btnScanClick                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmStatistics.btnScanClick(Sender : TObject) ;
  Var
    nRec : Integer ;
    pRec : Pointer ;

    StatData :
      Record
        MinB ,
        MaxB  : Integer ;

        MinC ,
        MaxC  : String ;

        MinD ,
        MaxD ,
        AvgD   : TDateTime ;

        MinF ,
        MaxF  : Extended ;

        MinG ,
        MaxG  : String[10] ;

        MinI ,
        MaxI  : Integer ;

        MinL ,
        MaxL  : String[10] ;

        MinM ,
        MaxM  : Integer ;

        MinN ,
        MaxN ,
        AvgN   : Extended ;

        MinP ,
        MaxP  : String[10] ;

        MinT ,
        MaxT ,
        AvgT  : TDateTime ;

        MinY ,
        MaxY  : Extended ;

        dDate    : TDateTime ;
        nDays    : Double    ;
        nDayRecs : Integer   ;
      End ;

  Begin  { TfmStatistics.btnScanClick }
    enRecsUsed.AsInteger := 0 ;

    FreeFlds ;

    FillChar(StatData , SizeOf(StatData) , 0) ;

    nField := lbFields.ItemIndex ;
    With xDataBasePtr^ do
      Case GetFieldType(nField) of
        'C' : CreateStringFlds(GetFieldWidth(nField)) ;
        'D' ,
        'T'   : CreateDateFields(nField) ;

        'F' ,
        'N' : CreateNumericFlds(GetFieldDecimals(nField)) ;
      End ;

    With xDataBasePtr^  , StatData do
      Begin
        For nRec := 1 to GetTotalRecords do
          Begin
            enRecsUsed.AsInteger := nRec ;
            enRecsUsed.Repaint ;

            GetRecord(nRec) ;
            pRec := GetRecordPtr ;

            cFldType := GetFieldType(nField)     ;
            cFldStr  := GetFldStr(nField)        ;
            nFldDec  := GetFieldDecimals(nField) ;

            Case cFldType of
              'B' : Begin
                    End ;

              'C' : Begin
                      If nRec = 1 then
                        Begin
                          MinC := cFldStr ;
                          MaxC := cFldStr ;
                        End
                      Else
                        Begin
                          If (cFldStr < MinC) and (Length(Trim(cFldStr)) > 0) then
                            MinC := cFldStr ;

                          If cFldStr > MaxC then
                            MaxC := cFldStr ;
                        End ;
                    End ;

              'D' : Begin
                      dDate := GetFieldDate(nField) ;
                      If nRec = 1 then
                        Begin
                          MinD := dDate ;
                          MaxD := MinD ;
                        End
                      Else
                        Begin
                          If (dDate < MinD) and (dDate <> 0) then
                            MinD := dDate ;

                          If dDate > MaxD then
                            MaxD := dDate ;
                        End ;
                    End ;

              'F' ,
              'N'  : Begin
                      nFloat := XStrToFloat(cFldStr) ;
                      If nRec = 1 then
                        Begin
                          MinN := nFloat ;
                          MaxN := nFloat ;
                          AvgN := nFloat ;
                        End
                      Else
                        Begin
                          If nFloat < MinN then
                            MinN := nFloat ;

                          If nFloat > MaxN then
                            MaxN := nFloat ;

                          AvgN := AvgN + nFloat ;
                        End ;
                    End ;

              'G' : Begin
                    End ;

              'I' : Begin
                    End ;

              'L' : Begin
                    End ;

              'M' : Begin
                    End ;

              'P' : Begin
                    End ;

              'T' : Begin
                      dDate := GetDateTimeField(pRec  ,
                                                nField ) ;
                      If nRec = 1 then
                        Begin
                          MinT := dDate ;
                          MaxT := MinT ;
                        End
                      Else
                        Begin
                          If (dDate < MinT) and (dDate <> 0) then
                            MinT := dDate ;

                          If dDate > MaxT then
                            MaxT := dDate ;
                        End ;
                    End ;

              'Y' : Begin
                    End ;
            Else
              Begin
              End ;
            End ;
          End ;

        { Set the display components. }
        Case cFldType of
          'C' : begin
                  esMinS.Text := MinC ;
                  esMaxS.Text := MaxC ;
                End ;

          'D' : Begin
                  nDays    := 0 ;
                  nDayRecs := 0 ;

                  For nRec := 1 to GetTotalRecords do
                    Begin
                      enRecsUsed.AsInteger := nRec ;
                      enRecsUsed.Repaint ;

                      GetRecord(nRec) ;
                      pRec := GetRecordPtr ;

                      dDate := GetFieldDate(nField) ;
                      If not dbfIsBlankDate(pRec , nField) then
                        Begin
                          nDays := nDays + (dDate - MinD) ;
                          Inc(nDayRecs) ;
                        End ;
                    End ;

                  If nDayRecs = 0 then
                    nDays := 0
                  Else
                    nDays := nDays / nDayRecs ;

                  AvgD := MinD + nDays ;
                  edMinD.Text := DateToStrBlank(MinD) ;
                  edMaxD.Text := DateToStrBlank(MaxD) ;
                  edAvgD.Text := DateToStrBlank(AvgD) ;
                End ;

          'T' : Begin
                  nDays    := 0 ;
                  nDayRecs := 0 ;

                  For nRec := 1 to GetTotalRecords do
                    Begin
                      enRecsUsed.AsInteger := nRec ;
                      enRecsUsed.Repaint ;

                      GetRecord(nRec) ;
                      pRec := GetRecordPtr ;

                      dDate := GetDateTimeField(pRec  ,
                                                nField ) ;
                      If dDate <> 0 then
                        Begin
                          nDays := nDays + (dDate - MinT) ;
                          Inc(nDayRecs) ;
                        End ;
                    End ;

                  If nDayRecs = 0 then
                    nDays := 0
                  Else
                    nDays := nDays / nDayRecs ;

                  AvgT := MinT + nDays ;
                  edMinD.Text := DateToEnglishBlank(MinT) ;
                  edMaxD.Text := DateToEnglishBlank(MaxT) ;
                  edAvgD.Text := DateToEnglishBlank(AvgT) ;
                End ;

          'F' ,
          'N'   : Begin
                    enMinN.Decimals := nFldDec ;
                    enMaxN.Decimals := nFldDec ;
                    enAvgN.Decimals := nFldDec ;

                    enMinN.AsFloat  := MinN ;
                    enMaxN.AsFloat  := MaxN ;
                    If GetFieldDecimals(nField) > 0 then
                      enAvgN.AsFloat  := (AvgN / GetTotalRecords)
                    Else
                      enAvgN.AsInteger := Trunc(AvgN) div GetTotalRecords ;
                  End ;
        End ;
      End ;
  End ;  { TfmStatistics.btnScanClick }


{***********************************************************************
*                                                                      *
*       TfmStatistics.CreateNumericFlds                                *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmStatistics.CreateNumericFlds(nDecimals : Integer) ;
  Begin  { TfmStatistics.CreateNumericFlds }
    CreateNumericEdit(enMinN        ,
                      pnlRight      ,
                      10            ,
                      'Andale Mono' ,
                      clBlue        ,
                       32           ,
                      102           ,
                      120           ,
                      nDecimals      ) ;

    CreateLabel(lbOne    ,
                pnlRight ,
                10       ,
                'Andale Mono'  ,
                clBlue   ,
                 32      ,
                 -102    ,
                'Min'     ) ;

    CreateNumericEdit(enMaxN    ,
                      pnlRight  ,
                      10        ,
                      'Andale Mono'   ,
                      clBlue    ,
                       72       ,
                      102       ,
                      120       ,
                      nDecimals  ) ;

    CreateLabel(lbTwo    ,
                pnlRight ,
                10       ,
                'Andale Mono'  ,
                clBlue   ,
                 72      ,
                 -102    ,
                'Max'     ) ;

    CreateNumericEdit(enAvgN    ,
                      pnlRight  ,
                      10        ,
                      'Andale Mono'   ,
                      clBlue    ,
                      112       ,
                      102       ,
                      120       ,
                      nDecimals  ) ;

    CreateLabel(lbThree  ,
                pnlRight ,
                10       ,
                'Andale Mono'  ,
                clBlue   ,
                 112     ,
                 -102    ,
                'Avg'     ) ;

    lbOne.Repaint   ;
    lbTwo.Repaint   ;
    lbThree.Repaint ;

    enMinN.Repaint ;
    enMaxN.Repaint ;
    enAvgN.Repaint ;
  End ;  { TfmStatistics.CreateNumericFlds }


{***********************************************************************
*                                                                      *
*       TfmStatistics.ClearFlds                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmStatistics.ClearFlds ;
  Begin  { TfmStatistics.ClearFlds }
    If (edMinD <> nil) then
      edMinD.Clear ;
    If (edMaxD <> nil) then
      edMaxD.Clear ;
    If (edAvgD <> nil) then
      edAvgD.Clear ;

    If (enMinN <> nil) then
      enMinN.Clear ;
    If (enMaxN <> nil) then
      enMaxN.Clear ;
    If (enAvgN <> nil) then
      enAvgN.Clear ;

    If (esMinS <> nil) then
      esMinS.Clear ;
    If (esMaxS <> nil) then
      esMaxS.Clear ;
  End ;  { TfmStatistics.ClearFlds }


{***********************************************************************
*                                                                      *
*       TfmStatistics.FreeFlds                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmStatistics.FreeFlds ;
  Begin  { TfmStatistics.FreeFlds }
    FreeAndNil(lbOne)   ;
    FreeAndNil(lbTwo)   ;
    FreeAndNil(lbThree) ;
    FreeAndNil(edMinD)  ;
    FreeAndNil(edMaxD)  ;
    FreeAndNil(edAvgD)  ;
    FreeAndNil(enMinN)  ;
    FreeAndNil(enMaxN)  ;
    FreeAndNil(enAvgN)  ;
    FreeAndNil(esMinS)  ;
    FreeAndNil(esMaxS)  ;

    ZapFields ;
  End ;  { TfmStatistics.FreeFlds }


{***********************************************************************
*                                                                      *
*       TfmStatistics.ZapFields                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmStatistics.ZapFields ;
  Begin  { TfmStatistics.ZapFields }
    Try
      edMinD := nil ;
      edMaxD := nil ;
      edAvgD := nil ;

      enMinN := nil ;
      enMaxN := nil ;
      enAvgN := nil ;

      esMinS := nil ;
      esMaxS := nil ;

      lbOne   := nil ;
      lbTwo   := nil ;
      lbThree := nil ;
    Except
      ShowErrorMessage('Exception in TfmStatistics.ZapFields') ;
    End ;
  End ;  { TfmStatistics.ZapFields }


{***********************************************************************
*                                                                      *
*       TfmStatistics.CreateStringFlds                                 *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmStatistics.CreateStringFlds(nWidth : Integer) ;
  Const
    nFontSize       = 10  ;
    nMaxWidthPixels = 200 ;

  Var
    nWidthPixels : Integer ;

  Begin  { TfmStatistics.CreateStringFlds }
    nWidthPixels := MaxInteger((nWidth * nFontSize) , nMaxWidthPixels) ;
    CreateStringEdit(esMinS       ,
                     pnlRight     ,
                     nFontSize    ,
                     'Andale Mono'      ,
                     clBlue       ,
                      32          ,
                      50          ,
                     nWidthPixels  ) ;

    CreateLabel(lbOne     ,
                pnlRight  ,
                nFontSize ,
                'Andale Mono'   ,
                clBlue    ,
                 32       ,
                -50       ,
                'Min'      ) ;

    CreateStringEdit(esMaxS       ,
                     pnlRight     ,
                     nFontSize    ,
                     'Andale Mono'      ,
                     clBlue       ,
                      72          ,
                      50          ,
                     nWidthPixels  ) ;

    CreateLabel(lbTwo     ,
                pnlRight  ,
                nFontSize ,
                'Andale Mono'   ,
                clBlue    ,
                 72       ,
                -50       ,
                'Max'      ) ;


    lbOne.Repaint   ;
    lbTwo.Repaint   ;

    esMinS.Repaint ;
    esMaxS.Repaint ;
  End ;  { TfmStatistics.CreateStringFlds }


{***********************************************************************
*                                                                      *
*       TfmStatistics.CreateDateFields                                 *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmStatistics.CreateDateFields(nField : Integer) ;
  Const
    nFontSize       = 10  ;
    nWidth          = 24  ;
    nMaxWidthPixels = 200 ;

  Var
    nWidthPixels : Integer ;

  Begin  { TfmStatistics.CreateDateFields }
    nWidthPixels := MinInteger((nWidth * nFontSize) , nMaxWidthPixels) ;
    CreateStringEdit(edMinD       ,
                     pnlRight     ,
                     nFontSize    ,
                     'Andale Mono'      ,
                     clBlue       ,
                      32          ,
                      50          ,
                     nWidthPixels  ) ;

    CreateLabel(lbOne     ,
                pnlRight  ,
                nFontSize ,
                'Andale Mono'   ,
                clBlue    ,
                 32       ,
                -50       ,
                'Min'      ) ;

    CreateStringEdit(edMaxD       ,
                     pnlRight     ,
                     nFontSize    ,
                     'Andale Mono'      ,
                     clBlue       ,
                      72          ,
                      50          ,
                     nWidthPixels  ) ;

    CreateLabel(lbTwo     ,
                pnlRight  ,
                nFontSize ,
                'Andale Mono'   ,
                clBlue    ,
                 72       ,
                -50       ,
                'Max'      ) ;


    CreateStringEdit(edAvgD       ,
                     pnlRight     ,
                     nFontSize    ,
                     'Andale Mono'      ,
                     clBlue       ,
                     112          ,
                      50          ,
                     nWidthPixels  ) ;

    CreateLabel(lbThree   ,
                pnlRight  ,
                nFontSize ,
                'Andale Mono'   ,
                clBlue    ,
                112       ,
                -50       ,
                'Avg'      ) ;

    lbOne.Repaint   ;
    lbTwo.Repaint   ;
    lbThree.Repaint ;

    edMinD.Repaint ;
    edMaxD.Repaint ;
    edAvgD.Repaint ;
  End ;  { TfmStatistics.CreateDateFlds }


{***********************************************************************
*                                                                      *
*       TfmStatistics.lbFieldsClick                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmStatistics.lbFieldsClick(Sender : TObject) ;
  Begin  { TfmStatistics.lbFieldsClick }
    FreeFlds ;

    enRecsUsed.AsInteger := 0 ;
    enRecsUsed.Repaint ;
  End ;  { TfmStatistics.lbFieldsClick }


{***********************************************************************
*                                                                      *
*       TfmStatistics.lbFieldsDblClick                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfmStatistics.lbFieldsDblClick(Sender : TObject) ;
  Begin  { TfmStatistics.lbFieldsDblClick }
    btnScanClick(Sender) ;
  End ;  { TfmStatistics.lbFieldsDblClick }


{***********************************************************************
*                                                                      *
*       TfmStatistics.FormActivate                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}


Procedure TfmStatistics.FormActivate(Sender : TObject) ;
  Var
    nField : Integer ;

  Begin  { TfmStatistics.FormActivate }
    lbFields.Clear ;
    With xDataBasePtr^ do
      For nField := 0 to GetFieldCount do
        With lbFields.Items do
          Add(Pad(GetFieldName(nField) , 12) + GetFieldType(nField) + '  ' +
              IntToStrBlankPad(GetFieldWidth(nField) , 4) + ' ' +
              IntToStrBlankPad(GetFieldDecimals(nField) , 2)) ;

    lbFields.ItemIndex   := 0 ;
    enRecsUsed.AsInteger := 0 ;
  End;  { TfmStatistics.FormActivate }

End.
