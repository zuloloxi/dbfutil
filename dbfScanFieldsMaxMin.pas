{***********************************************************************
*                                                                      *
*       dbfScanFieldsMaxMin.pas                                        *
*                                                                      *
*       (C) Copyright 1982-2001 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfScanFieldsMaxMin ;

Interface
  Uses
    SysUtils ,

    bcClasses ,
    
    dbfStructure ;

  Type
    TOnRecordScan = Procedure(const RecNo : Integer  ) ;

  Procedure ScanFields(pData     : pTxBase       ;
                       pRecProc  : TOnRecordScan ;
                       bContinue : Boolean        ) ;
  Type
    TFldType = (oBoolean  ,
                oCurrency ,
                oDateTime ,
                oDouble   ,
                oInteger  ,
                oPicture  ,
                oString    ) ;

    TFldRec = packed
      Record
        vFldType : TFldType ;

        Case TFldType of
          oBoolean  : (bBoolean  : Boolean    ) ;
          oCurrency : (nCurrency : Currency   ) ;
          oDateTime : (nDateTime : TDateTime  ) ;
          oDouble   : (nDouble   : Double     ) ;
          oInteger  : (nInteger  : Int64      ) ;
          oPicture  : (nPicture  : Int64      ) ;
          oString   : (cString   : ShortString) ;
      End ;


    pTScanRec = ^TScanRec ;
    TScanRec = Array[1..dbfMaxFields] of TFldRec ;

    TFldWidth = Packed
      Record
        nMinWidth : Integer ;
        nMaxWidth : Integer ;
      End ;

    TaFldWidth = Array[1..dbfMaxFields] of TFldWidth ;

  Var
    LoRec : TScanRec ;
    HiRec : TScanRec ;

    WidthRec : TaFldWidth ;
    
Implementation

{***********************************************************************
*                                                                      *
*       InitializeStrucs                                               *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure InitializeStrucs ;
  Begin  { InitializeStrucs }
    FillChar(LoRec , SizeOf(LoRec) , 0) ;
    FillChar(HiRec , SizeOf(HiRec) , 0) ;
    FillChar(WidthRec , SizeOf(WidthRec) , 0) ;
  End ;  { InitializeStrucs }


{***********************************************************************
*                                                                      *
*       SetFieldTypes                                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function SetFieldTypes(pData : pTxBase) : Integer ;
  Var
    nResult : Integer ;
    nField  : Integer ;

  Begin  { SetFieldTypes }
    InitializeStrucs ;

    nResult := 0 ;

    With pData^ do
      For nField := 0 to GetFieldCount do
        Case GetFieldType(nField) of
          'B' : Begin
                  HiRec[nField].vFldType := oInteger ;
                  LoRec[nField].vFldType := oInteger ;
                  nResult := nResult + 1 ;
                End ;

          'C' : Begin
                  HiRec[nField].vFldType := oString ;
                  LoRec[nField].vFldType := oString ;
                  nResult := nResult + 1 ;
                End ;

          'D' ,
          'T'   : Begin
                    HiRec[nField].vFldType := oDateTime ;
                    LoRec[nField].vFldType := oDateTime ;
                    nResult := nResult + 1 ;
                  End ;

          'F' : Begin
                  HiRec[nField].vFldType := oDouble ;
                  LoRec[nField].vFldType := oDouble ;
                  nResult := nResult + 1 ;
                End ;

          'L' : Begin
                  HiRec[nField].vFldType := oBoolean ;
                  LoRec[nField].vFldType := oBoolean ;
                  nResult := nResult + 1 ;
                End ;

          'I' ,
          'G' ,
          'M'   : Begin
                    HiRec[nField].vFldType := oInteger ;
                    LoRec[nField].vFldType := oInteger ;
                    nResult := nResult + 1 ;
                  End ;

          'N' : Begin
                  If GetFieldDecimals(nField) = 0 then
                    Begin
                      HiRec[nField].vFldType := oInteger ;
                      LoRec[nField].vFldType := oInteger ;
                    End
                  Else
                    Begin
                      HiRec[nField].vFldType := oDouble ;
                      LoRec[nField].vFldType := oDouble ;
                    End ;
                  nResult := nResult + 1 ;
                End ;

          'P' : Begin
                  HiRec[nField].vFldType := oPicture ;
                  LoRec[nField].vFldType := oPicture ;
                  nResult := nResult + 1 ;
                End ;

          'Y' : Begin
                  HiRec[nField].vFldType := oCurrency ;
                  LoRec[nField].vFldType := oCurrency ;
                  nResult := nResult + 1 ;
                End ;
        End ;

    Result := nResult ;
  End ;  { SetFieldTypes }


{***********************************************************************
*                                                                      *
*       ScanFields                                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure ScanFields(pData     : pTxBase       ;
                     pRecProc  : TOnRecordScan ;
                     bContinue : Boolean        ) ;
  Var
    nField    : Integer   ;
    nRec      : Integer   ;
    nInt      : Integer   ;
    bBool     : Boolean   ;
    nFloat    : Double    ;
    cStr      : String    ;
    nWidth    : Integer   ;
    dDateTime : TDateTime ;

//    nFieldsScanned : Integer ;

  Begin  { ScanFields }
    InitializeStrucs ;
    
    { nFieldsScanned := } SetFieldTypes(pData) ;

    With pData^ do
      For nRec := 1 to GetTotalRecords do
        If bContinue then
          Begin
            pRecProc(nRec) ;
            If GetRecord(nRec) then
              For nField := 0 to GetFieldCount do
                Case LoRec[nField].vFldType of
                  oBoolean :
                    Begin
                      bBool := FldAsBoolean(nField) ;

                      If (bBool < LoRec[nField].bBoolean) or (nRec = 1) then
                        LoRec[nField].bBoolean := bBool ;

                      If (bBool > LoRec[nField].bBoolean) or (nRec = 1) then
                        HiRec[nField].bBoolean := bBool ;
                    End ;

                  oDateTime :
                    Begin
                      dDateTime := GetFldDate(nField) ;

                      If (dDateTime < LoRec[nField].nDateTime) or (nRec = 1) then
                        LoRec[nField].nDateTime := dDateTime ;

                      If (dDateTime > LoRec[nField].nDateTime) or (nRec = 1) then
                        HiRec[nField].nDateTime := dDateTime ;
                    End ;

                  oDouble :
                    Begin
                      nFloat := FldAsDouble(nField) ;
                      If (nFloat < LoRec[nField].nDouble) or (nRec = 1) then
                        LoRec[nField].nDouble := nFloat ;

                      If (nFloat > HiRec[nField].nDouble) or (nRec = 1) then
                        HiRec[nField].nDouble := nFloat ;
                    End ;

                  oInteger :
                    Begin
                      nInt := GetFldInteger(nField) ;

                      If (nInt < LoRec[nField].nInteger) or (nRec = 1) then
                        LoRec[nField].nInteger := nInt ;

                      If (nInt > LoRec[nField].nInteger) or (nRec = 1) then
                        HiRec[nField].nInteger := nInt ;
                    End ;

                  oPicture :
                    Begin
                      nInt := GetFldInteger(nField) ;

                      If (nInt < LoRec[nField].nPicture) or (nRec = 1) then
                        LoRec[nField].nPicture := nInt ;

                      If (nInt > LoRec[nField].nPicture) or (nRec = 1) then
                        HiRec[nField].nPicture := nInt ;
                    End ;

                  oString :
                    Begin
                      cStr := Trim(pData^.GetFldStr(nField)) ;
                      If (cStr < LoRec[nField].cString) or (nRec = 1) then
                        LoRec[nField].cString := cStr ;

                      If (cStr > HiRec[nField].cString) or (nRec = 1) then
                        HiRec[nField].cString := cStr ;

                      nWidth := Length(cStr) ;
                      If (nWidth < WidthRec[nField].nMinWidth) or (nRec = 1) then
                         WidthRec[nField].nMinWidth := nWidth ;
                      If (nWidth > WidthRec[nField].nMaxWidth) or (nRec = 1) then
                         WidthRec[nField].nMaxWidth := nWidth ;
                    End ;
                Else
                End ; { Case LoRec[nField].vFldType of }
          End ;
  End ;  { ScanFields }

End.
