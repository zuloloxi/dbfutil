{***********************************************************************
*                                                                      *
*       dbfFixFields.pas                                               *
*                                                                      *
*       (C) Copyright 1982-2001  Bruce K. Christensen                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfFixFields ;

Interface

Uses
  Windows , Messages , SysUtils , Classes  , Graphics , Controls ,
  Forms   , Dialogs  , ExtCtrls , StdCtrls , Buttons  ,

  bcClasses       ,
  bcDialogUtilities ,
  bcFileUtilities ,
  bcNumericEdit   ,
  bcScreenUtilities ,
  bcStringUtilities ,

  dbfCommon    ,
  dbfConstant  ,
  dbfStructure   ;

Type
  TfmFixFields = class(TForm)
    pnlMain     : TPanel  ;
    pnlBottom   : TPanel  ;
    btnScanFix  : TBitBtn ;
    pnlSplitter : TPanel  ;
    pnlTop      : TPanel  ;
    pnlTopName  : TPanel  ;
    pnlSplitter1 : TPanel;
    btnPercent: TBitBtn;

    neB: TFnpNumericEdit ;
    neC: TFnpNumericEdit ;
    neD: TFnpNumericEdit ;
    neF: TFnpNumericEdit ;
    neG: TFnpNumericEdit ;
    neI: TFnpNumericEdit ;
    neL: TFnpNumericEdit ;
    neM: TFnpNumericEdit ;
    neN: TFnpNumericEdit ;
    neP: TFnpNumericEdit ;
    neT: TFnpNumericEdit ;
    neY: TFnpNumericEdit ;

    nCurrentRec   : TFnpNumericEdit ;
    enTotalRecs   : TFnpNumericEdit ;
    neTotalErrors : TFnpNumericEdit ;
    neDelete      : TFnpNumericEdit ;

    cbB : TCheckBox ;
    cbC : TCheckBox ;
    cbD : TCheckBox ;
    cbF : TCheckBox ;
    cbG : TCheckBox ;
    cbI : TCheckBox ;
    cbL : TCheckBox ;
    cbM : TCheckBox ;
    cbN : TCheckBox ;
    cbP : TCheckBox ;
    cbT : TCheckBox ;
    cbY : TCheckBox ;

    Label1 : TLabel ;
    Label2 : TLabel ;
    Label3 : TLabel ;
    Label4 : TLabel ;

    edtFileName : TEdit ;

    Procedure btnScanFixClick(Sender : TObject) ;
    procedure FormActivate(Sender: TObject);

  Protected
    oOwner : TComponent ;

  Private  { Private declarations }
    xDbfPtr: pTxBase ;

    cbArray : Array[1..NoOfFieldTypes] of pTCheckBox       ;
    neArray : Array[1..NoOfFieldTypes] of pTFnpNumericEdit ;

    cFieldTypes : String ;

    Procedure ZeroCounts  ;
    Procedure TotalCounts ;

    Procedure UpdateFieldCount(nField : Integer) ;

    Procedure ExtraSetup ;

    {$HINTS OFF}
    Function ValidateField(nField : Integer) : Boolean ;
    {$HINTS ON}

  Public  { Public declarations }
    Constructor Create(aOwner : TComponent ;
                       xPtr   : pTxBase     ) ; ReIntroduce ;
    Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfmFixFields.Create                                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfmFixFields.Create(aOwner : TComponent ;
                                xPtr   : pTxBase    ) ;
  Begin  { TfmFixFields.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr  ;

    cbArray[ 1] := @cbB ;
    cbArray[ 2] := @cbC ;
    cbArray[ 3] := @cbD ;
    cbArray[ 4] := @cbF ;
    cbArray[ 5] := @cbG ;
    cbArray[ 6] := @cbI ;
    cbArray[ 7] := @cbL ;
    cbArray[ 8] := @cbM ;
    cbArray[ 9] := @cbN ;
    cbArray[10] := @cbP ;
    cbArray[11] := @cbT ;
    cbArray[12] := @cbY ;

    neArray[ 1] := @neB ;
    neArray[ 2] := @neC ;
    neArray[ 3] := @neD ;
    neArray[ 4] := @neF ;
    neArray[ 5] := @neG ;
    neArray[ 6] := @neI ;
    neArray[ 7] := @neL ;
    neArray[ 8] := @neM ;
    neArray[ 9] := @neN ;
    neArray[10] := @neP ;
    neArray[11] := @neT ;
    neArray[12] := @neY ;
  End ;  { TfmFixFields.Create }


{***********************************************************************
*                                                                      *
*       TfmFixFields.Destroy                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfmFixFields.Destroy ;
  Begin  { TfmFixFields.Destroy }
    Inherited Destroy ;
  End ;  { TfmFixFields.Destroy }


{***********************************************************************
*                                                                      *
*       TfmFixFields.ValidateField                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TfmFixFields.ValidateField(nField : Integer) : Boolean ;
  Begin  { TfmFixFields.ValidateField }
    With xDbfPtr^ do
      If PosStr(GetFieldType(nField) , AllFields) > 0 then
        Result := cbArray[GetFieldIdxType(nField)]^.Checked
      Else
        Result := False ;
  End ;  { TfmFixFields.ValidateField }


{***********************************************************************
*                                                                      *
*       TfmFixFields.UpdateFieldCount                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFixFields.UpdateFieldCount(nField : Integer) ;
  Begin  { TfmFixFields.UpdateFieldCount }
    With xDbfPtr^ do
      Begin
        If nField = 0 then
          neDelete.IncInteger
        Else
          neArray[GetFieldIdxType(nField)]^.IncInteger ;
      End ;

    TotalCounts ;
  End ;  { TfmFixFields.UpdateFieldCount }


{***********************************************************************
*                                                                      *
*       TfmFixFields.ZeroCounts                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFixFields.ZeroCounts ;
  Var
    nI : Integer ;

  Begin  { TfmFixFields.ZeroCounts }
    For nI := 1 to NoOfFieldTypes do
      neArray[nI]^.AsInteger := 0 ;

    neTotalErrors.AsInteger := 0 ;
    neDelete.AsInteger      := 0 ;
  End ;  { TfmFixFields.ZeroCounts }


{***********************************************************************
*                                                                      *
*       TfmFixFields.TotalCounts                                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFixFields.TotalCounts ;
  Var
    nI ,
    nSum : Integer ;

  Begin  { TfmFixFields.TotalCounts }
    nSum := 0 ;
    For nI := 1 to NoOfFieldTypes do
      nSum := nSum + neArray[nI]^.AsInteger ;
    nSum := nSum + neDelete.AsInteger ;

    neTotalErrors.SetInteger(nSum) ;
  End ;  { TfmFixFields.TotalCounts }


{***********************************************************************
*                                                                      *
*       TfmFixFields.btnScanFixClick                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfmFixFields.btnScanFixClick(Sender : TObject) ;
  Var
    nRec    ,
    nPerc   ,
    nField  ,
    nRecNum   : Integer ;

    cFixedDataName : String ;
    cFixedMemoName : String ;

  Begin  { TfmFixFields.btnScanFixClick }
    ZeroCounts ;

    With xDbfPtr^ do
      Begin
        cFixedDataName := ForceExtension(GetDataFileName  , dbfFixedDataFileExt) ;
        cFixedMemoName := ForceExtension(GetMemoFileName  , dbfFixedMemoFileExt) ;

        EraseFile(cFixedDataName) ;
        EraseFile(cFixedMemoName) ;

        FixedFile := TxBase.Create(nil) ;
        Duplicate(FixedFilePtr   ,
                  cFixedDataName ,
                  cFixedMemoName  ) ;

        { Should already be 0, but set it anyway. }
        FixedFile.SetTotalRecords(0) ;

        ZapCounts ;

        neC.SetInteger(0) ;

        GetGoodListPtr^.Clear    ;
        GetBadListPtr^.Clear     ;
        GetBadNameListPtr^.Clear ;

        For nRec := 1 to GetTotalRecords do
          Begin
            If not GetRecord(nRec) then
              ShowErrorMessage('Error reading record [' + IntToStr(nRec) + ']') ;

            If nRec = GetTotalRecords then
              nPerc := 100
            Else
              nPerc := (100 * nRec) div GetTotalRecords ;

            If nPerc >= 1 then
              btnPerCent.Caption := IntToStr(nPerc) + '%' ;
            nCurrentRec.AsInteger := nRec ;

            Application.ProcessMessages ;

            Move(xDbfPtr^.GetRecordPtr^ , FixedFile.GetRecordPtr^ , GetRecordSize) ;

            With FixedFile do
              Begin
                For nField := 0 to GetFieldCount do
                  If FixField(GetRecordPtr , nField) then
                    UpdateFieldCount(nField) ;

                AddRecord(GetRecordPtr , nRecNum) ;
              End ;
          End ;

        FreeAndNil(FixedFile) ;
      End ;
  End ;  { TfmFixFields.btnScanFixClick }


{***********************************************************************
*                                                                      *
*       TfmFixFields.ExtraSetup                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFixFields.ExtraSetup ;
  Var
    nI ,
    nPos ,
    nField : Word ;
    cFldType : Char ;
    cStr : String ;

  Begin  { TfmFixFields.ExtraSetup }
    ZeroCounts ;

    For nI := 1 to NoOfFieldTypes do
      Begin
        cbArray[nI]^.Checked := False ;
        cbArray[nI]^.Enabled := False ;

        neArray[nI]^.Enabled := False ;
      End ;

    edtFileName.Text := xDbfPtr^.GetDataFileName ;

    cFieldTypes := '' ;
    For nI := 1 to NoOfFieldTypes do
      cFieldTypes := ' ' + cFieldTypes ;

    With xDbfPtr^ do
      For nField := 1 to GetFieldCount do
        Begin
          cFldType := GetFieldType(nField) ;

          nPos := GetFieldIdxType(nField) ;
          If nPos > 0 then
            cFieldTypes[nPos] := cFldType 
          Else
            Begin
              //  This condition better NEVER happen.
            End ;

          cbArray[nPos]^.Checked := True ;
          cbArray[nPos]^.Enabled := True ;
          neArray[nPos]^.Enabled := True ;
        End ;

    cFieldTypes := KillChar(cFieldTypes , ' ') ;
    cStr := '' ;
    For nI := 1 to Length(cFieldTypes) do
      cStr := cStr + cFieldTypes[nI] + ' ' ;
    pnlBottom.Caption := 'Field Types [' + Copy(cStr , 1 , Length(cStr) - 1) + ']' ;
  End ;  { TfmFixFields.ExtraSetup }


{***********************************************************************
*                                                                      *
*       TfmFixFields.btnScanFixClick                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}


Procedure TfmFixFields.FormActivate(Sender : TObject) ;
  Begin  { TfmFixFields.FormActivate }
    FormCentre(@Self) ;

    cFieldTypes := '' ;

    enTotalRecs.AsInteger := xDbfPtr^.GetTotalRecords ;
    nCurrentRec.AsInteger := 0 ;

    ExtraSetup ;
  End ;  { TfmFixFields.FormActivate }

End.

