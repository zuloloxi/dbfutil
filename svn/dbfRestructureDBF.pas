{***********************************************************************
*                                                                      *
*       pdxConversionToDBF.pas                                         *
*                                                                      *
*       (C) Copyright 1990-2001 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfRestructureDBF ;

Interface

Uses
  Windows , Messages , SysUtils , Classes , Graphics , Controls ,
  Forms   , Dialogs  , ExtCtrls , StdCtrls, Buttons, ComCtrls,

  bcClasses         ,
  bcNumericEdit     ,
  bcScreenUtilities ,
  bcStringUtilities ,

  dbfConstant  ,
  dbfStructure   ;

Type
  TfrmRestructureDBF =
    Class(TForm)
      pnlMain      : TPanel ;
      pnlButtons   : TPanel ;
      pnlSplitter1 : TPanel ;

      btnOK : TBitBtn ;

      enfCurrRec   : TFnpNumericEdit ;
      enfTotalRecs : TFnpNumericEdit ;
      enfPerc      : TFnpNumericEdit ;

      Label1 : TLabel ;
      Label2 : TLabel ;
      Label3 : TLabel ;
      Label4 : TLabel ;
      Label5 : TLabel ;

      ProgressBar1 : TProgressBar ;

      edtPdx: TEdit;
      edtxBase: TEdit;
    enfSkippedRecords: TFnpNumericEdit;
    Label6: TLabel;
    ednAddedRecords: TFnpNumericEdit;
    Label7: TLabel;
    ednFieldIdx: TFnpNumericEdit;
    ednFieldCount: TFnpNumericEdit;
    Label8: TLabel;

      Procedure FormPaint(Sender : TObject) ;
      Procedure btnOKClick(Sender : TObject) ;

      Protected
        oOwner : TComponent ;

        nDeleted    : Integer    ;
        nAddRecords : Integer    ;

      Private
        xDbfPtr : pTxBase ;

        oFlds    : TStringList ;
        oNewFlds : TStringList ;  { Name      1 10 }
                                  { Type     12  1 }
                                  { Width    14  3 }
                                  { Decimals 18  2 }


        oCurrentFields : TStringList ;
        oAddedFields   : TStringList ;
        oDeletedFields : TStringList ;

        oOldFields : TIntegerList ;

        bFieldCurrent : Boolean ;
        bFieldAdded   : Boolean ;
        bFieldDeleted : Boolean ;

        Procedure RepaintAll ;
        Procedure CreateDBF ;

      Public
        Constructor Create(aOwner      : TComponent     ;
                           xPtr        : pTxBase        ;
                           oFldListPtr : TStringListPtr  ) ; reintroduce ;
        Destructor Destroy ; override ;
    End ;

Implementation

{$R *.DFM}

Uses
  bcFileUtilities ,

  dbfResources ;
    
{***********************************************************************
*                                                                      *
*       TfrmConvertToDBF.Create                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmRestructureDBF.Create(aOwner      : TComponent     ;
                                      xPtr        : pTxBase        ;
                                      oFldListPtr : TStringListPtr  ) ;
  Var
    nField : Integer ;
    cStr   : String  ;

  Begin  { TfrmConvertToDBF.Create }
    If xPtr = nil then
      Raise Exception.Create('Database pointer is nil.') ;

    oOwner  := AOwner ;
    xDbfPtr := xPtr   ;

    oFlds := TStringList.Create ;
    oFlds.Clear ;
    oFlds.Assign(oFldListPtr^) ;

    oOldFields     := TIntegerList.Create ;
    oNewFlds       := TStringList.Create  ;
    oCurrentFields := TStringList.Create  ;
    oAddedFields   := TStringList.Create  ;
    oDeletedFields := TStringList.Create  ;

    oNewFlds.Clear       ;
    oCurrentFields.Clear ;
    oAddedFields.Clear   ;
    oDeletedFields.Clear ;

    oOldFields.Clear ;

    For nField := 0 to (oFlds.Count - 1) do
      Begin
        cStr := oFlds[nField] ;

        bFieldCurrent := (cStr[11]= ' ') ;
        bFieldAdded   := (cStr[11]= 'N') ;
        bFieldDeleted := (cStr[11]= '*') ;

        If bFieldDeleted then
          oDeletedFields.Add(cStr)
        Else
          Begin
            If bFieldAdded then
              oAddedFields.Add(cStr) ;

            oCurrentFields.Add(cStr) ;
            oOldFields.Add(nField) ;
          End ;
      End ;  { For nField := 0 to (oFlds.Count - 1) do }


    nDeleted    := 0 ;
    nAddRecords := 0 ;

    Inherited Create(oOwner) ;
  End ;  { TfrmConvertToDBF.Create }


{***********************************************************************
*                                                                      *
*       TfrmConvertToDBF.Destroy                                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmRestructureDBF.Destroy ;
  Begin  { TfrmConvertToDBF.Destroy }
    FreeAndNil(oNewFlds)       ;
    FreeAndNil(oCurrentFields) ;
    FreeAndNil(oAddedFields)   ;
    FreeAndNil(oDeletedFields) ;
    FreeAndNil(oFlds)          ;

    Inherited Destroy ;
  End ;  { TfrmConvertToDBF.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmConvertToDBF.RepaintAll                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructureDBF.RepaintAll ;
  Begin  { TfrmConvertToDBF.RepaintAll }
    // RepaintForm(@Self) ;
  End ;  { TfrmConvertToDBF.RepaintAll }


{***********************************************************************
*                                                                      *
*       TfrmConvertToDBF.CreateDBF                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructureDBF.CreateDBF ;
  Var
    oDBF     : TxBase  ;
    nRec     : Integer ;
    nRecNum  : Integer ;
    pRec     : Pointer ;
    cxFile   : String  ;

    nField   : Integer ;

    cFldStr   : String  ;
    cFldName  : String  ;
    cFldType  : Char    ;
    nWidth    : Integer ;
    nDecimals : Integer ;
    nMemo     : Integer ;

  Begin  { TfrmConvertToDBF.CreateDBF }
    oDBF := TxBase.Create(oOwner) ;
    With oDBF do
      Try
        CreateDatabase(ForceExtension(xDbfPtr^.GetDataFileName , 'CBF') ,
                       xDbfPtr^.GetSignature                            ,
                       oCurrentFields                                    ) ;
        cxFile := GetDataFileName ;

        edtxBase.Text := cxFile ;
        edtPdx.Text   := xDbfPtr^.GetDataFileName ;

        edtPdx.Repaint   ;
        edtxBase.Repaint ;

        SetSignature(xDbfPtr^.GetSignature) ;

        pRec := GetRecordPtr ;
        BlankRecord(pRec) ;

        enfTotalRecs.AsInteger := xDbfPtr^.GetTotalRecords ;
        enfTotalRecs.Repaint ;

        For nRec := 1 to xDbfPtr^.GetTotalRecords do
          Begin
            If xDbfPtr^.GetRecord(nRec) then
              Begin
                If AllOneChar(xDbfPtr^.GetRecordPtr  ,
                              xDbfPtr^.GetRecordSize ,
                              #$00                    ) then
                  Begin
                    Inc(nDeleted , 1) ;
                    enfSkippedRecords.AsInteger := nDeleted ;
                    enfSkippedRecords.Repaint ;
                  End
                Else
                  Begin
                    enfCurrRec.AsInteger := nRec ;
                    enfCurrRec.Repaint ;
                    enfPerc.AsInteger := ((nRec * 100) div enfTotalRecs.AsInteger) ;
                    enfPerc.Repaint ;
                    ProgressBar1.Position := enfPerc.AsInteger ;

                    pRec := GetRecordPtr ;
                    BlankRecord(pRec) ;

                    ednFieldCount.AsInteger := GetFieldCount ;
                    ednFieldCount.Repaint ;

                    For nField := 0 to GetFieldCount do
                      Begin
                        ednFieldIdx.AsInteger := nField ;
                        ednFieldIdx.Repaint ;

                        cFldName  := GetFieldName(nField)       ;
                        cFldType  := GetFieldType(nField)       ;
                        nWidth    := GetFieldWidth(nField)      ;
                        nDecimals := GetFieldDecimals(nField)   ;

                        If nWidth < 1 then
                          Raise Exception.Create(errMsgInvalidFieldWidth) ;

                        With xDbfPtr^ do
                          cFldStr := GetFieldStr(GetRecordPtr , nField , False) ;

                        Case cFldType of
                          'B' : Begin
                                  cFldStr := Trim(cFldStr) ;
                                  If Length(cFldStr) = 0 then
                                    nMemo := 0
                                  Else
                                    nMemo := StrToInt(cFldStr) ;

                                  SetMemoField(pRec   ,
                                               nMemo  ,
                                               nField  ) ;
                                End ;

                          'C' : Begin
                                  SetFieldStr(pRec    ,
                                              cFldStr ,
                                              nField   ) ;
                                End ;

                          'D': Begin
                                 cFldStr := RemoveSlashes(cFldStr) ;
                                 If cFldStr = '00000000' then
                                   cFldStr := '        ' ;
                                 SetFieldStr(pRec    ,
                                             cFldStr ,
                                             nField   ) ;
                               End ;

                          'L': Begin
                                 If nWidth <> 1 then
                                   Raise Exception.Create(errMsgInvalidFieldWidth) ;

                                 If cFldStr = 'True' then
                                   SetFieldLogical(pRec   ,
                                                   True   ,
                                                   nField  )
                                 Else
                                   If cFldStr = 'False' then
                                     SetFieldLogical(pRec   ,
                                                     False  ,
                                                     nField  )
                                   Else
                                     SetFieldStr(pRec   ,
                                                 ' '    ,
                                                 nField  ) ;
                               End ;

                          'N' ,
                          'F' : Begin
                                  cFldStr := Trim(cFldStr) ;
                                  If Length(cFldStr) = 0 then
                                    cFldStr := '0' ;

                                  If nDecimals = 0 then
                                    SetFieldInteger(pRec              ,
                                                    StrToInt(cFldStr) ,
                                                    nField             )
                                  Else
                                    Begin
                                      If nWidth <= (nDecimals + 1) then
                                        Raise Exception.Create(errMsgInvalidFieldWidth) ;

                                      SetFieldFloat(pRec                ,
                                                    StrToFloat(cFldStr) ,
                                                    nField               ) ;
                                    End ;
                                End ;

                          'T': Begin
                                 cFldStr := RemoveSlashes(cFldStr) ;
                                 If cFldStr = '00000000' then
                                   cFldStr := '        ' ;
                                 SetFieldStr(pRec    ,
                                             cFldStr ,
                                             nField   ) ;
                               End ;
                        End ;  { Case cFldType of }
                      End ;

                    If AddRecord(pRec , nRecNum) then
                      Begin
                        Inc(nAddRecords , 1) ;
                        ednAddedRecords.AsInteger := nAddRecords ;
                        ednAddedRecords.Repaint ;
                      End
                    Else
                      Begin
                      End ;
                  End ;
              End
            Else
              ShowMessage(errMsgInvalidGetRecord) ;
          End ;  { For nRec := 1 to xDbfPtr^.GetTotalRecords do }

      Finally
        Free ;
      End ;  { With oDBF do - Try }
  End ;  { TfrmConvertToDBF.CreateDBF }


{***********************************************************************
*                                                                      *
*       TfrmConvertToDBF.FormActivate                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructureDBF.FormPaint(Sender: TObject);
  Begin  { TfrmConvertToDBF.FormPaint }
    enfCurrRec.AsInteger        := 0 ;
    enfTotalRecs.AsInteger      := 0 ;
    enfPerc.AsInteger           := 0 ;
    enfSkippedRecords.AsInteger := 0 ;
    ednAddedRecords.AsInteger   := 0 ;
    RepaintAll ;

    CreateDBF ;
  End ;  { TfrmConvertToDBF.FormPaint }


{***********************************************************************
*                                                                      *
*       TfrmConvertToDBF.btnOKClick                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructureDBF.btnOKClick(Sender : TObject) ;
  Begin  { TfrmConvertToDBF.btnOKClick }
    Close ;
  End ;  { TfrmConvertToDBF.btnOKClick }

End.
