{***********************************************************************
*                                                                      *
*       dbfValidateFile.pas                                            *
*                                                                      *
*       (C) Copyright 1982-2004 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfValidateFile ;

Interface
  Uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls,
    Forms, Dialogs, StdCtrls, bcStringUtilities , ExtCtrls,

    W95Meter, Buttons, Grids,

    JvComponent     ,
    JvComponentBase ,
    JvExStdCtrls    ,
    JvListBox       ,
    JvThread        ,

    bcClasses         ,
    bcDialogUtilities ,
    bcNewList         ,
    bcNumericEdit     ,

    dbfConstant      ,
    dbfBadFieldView  ,
    dbfCommon        ,
    dbfRecordDisplay ,
    dbfStructure     ,

    CheckLst ;

Type
  pTLabel = ^TLabel ;

  TfrmValidateFile = Class(TForm)
    btnPercent: TBitBtn;
    edtFileName: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;

    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pnlBottom: TPanel;
    pnlBottomLeft: TPanel;
    pnlFieldTypes: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlSplitter1: TPanel;
    pnlTop: TPanel;
    SplitRecords: TButton;

    grpRecNo      : TGroupBox ;
    gbxFieldCheck : TGroupBox ;
    GroupBox2     : TGroupBox ;

    Panel6         : TPanel ;
    Panel7         : TPanel ;
    pnlFieldsCheck : TPanel ;
    pnlSplitter3   : TPanel ;
    pnlBottomRight : TPanel ;

    btnScan     : TBitBtn ;
    btnGoodPerc : TBitBtn ;
    btnBadPerc  : TBitBtn ;
    btnReset    : TBitBtn ;

    tlbBadFields : TNewTextListBox ;
    tlbBadRec    : TNewTextListBox ;

    lblB : TLabel ;
    lblC : TLabel ;
    lblD : TLabel ;
    lblF : TLabel ;
    lblG : TLabel ;
    lblI : TLabel ;
    lblL : TLabel ;
    lblM : TLabel ;
    lblN : TLabel ;
    lblP : TLabel ;
    lblT : TLabel ;
    lblY : TLabel ;

    BadDelete        : TFnpNumericEdit ;
    BadFields        : TFnpNumericEdit ;
    CurrentRecordBox : TFnpNumericEdit ;
    enBadRecs        : TFnpNumericEdit ;
    enGoodRecs       : TFnpNumericEdit ;
    neBadRecords     : TFnpNumericEdit ;
    neTotalRecords   : TFnpNumericEdit ;

    BadB : TFnpNumericEdit ;
    BadC : TFnpNumericEdit ;
    BadD : TFnpNumericEdit ;
    BadF : TFnpNumericEdit ;
    BadG : TFnpNumericEdit ;
    BadI : TFnpNumericEdit ;
    BadL : TFnpNumericEdit ;
    BadM : TFnpNumericEdit ;
    BadN : TFnpNumericEdit ;
    BadP : TFnpNumericEdit ;
    BadT : TFnpNumericEdit ;
    BadU : TFnpNumericEdit ;
    BadY : TFnpNumericEdit ;

    clbFieldsCheck: TCheckListBox;
    thrdValidate: TJvThread;

    Procedure ScanButtonClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure tlbBadRecDblClick(Sender: TObject);
    Procedure SplitRecordsClick(Sender: TObject);
    Procedure tlbBadFieldsDblClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure clbFieldsCheckClickCheck(Sender: TObject);
    procedure abfThreadTerminate(Sender: TObject);
    procedure thrdValidateExecute(Sender: TObject; Params: Pointer);

  Protected
    oOwner  : TComponent ;
    xDbfPtr : pTxBase  ;
    nBar    : Integer    ;

    oBaseThread : TJvBaseThread ;

    bThreadIsSuspended : Boolean   ;
    bIsExecuting       : Boolean   ;

  Private  { Private declarations }
    ednEnabled : Array[1..NoOfFieldTypes] of Boolean          ;
    ednEdits   : Array[1..NoOfFieldTypes] of pTFnpNumericEdit ;
    ednLabels  : Array[1..NoOfFieldTypes] of pTLabel          ;

    Procedure SetFieldTypes ;

  Public  { Public declarations }
    Constructor Create(AOwner : TComponent ;
                       xPtr   : pTxBase     ) ; reIntroduce ;
    Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}
Uses
  bcFormTools     ,
  bcFileUtilities ,

  dbfResources ;
    
{***********************************************************************
*                                                                      *
*       TfrmValidateFile.Create                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmValidateFile.Create(aOwner : TComponent ;
                                    xPtr   : pTxBase     ) ;
  Begin  { TfrmValidateFile.Create }
    CheckDatabasePtr(xPtr) ;

    Inherited Create(AOwner) ;

    oOwner := AOwner ;
    xDbfPtr := xPtr ;

    SetFieldTypes ;

    bIsExecuting       := False ;
    bThreadIsSuspended := False ;
  End ;  { TfrmValidateFile.Create }


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.Destroy                                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmValidateFile.Destroy ;
  Begin  { TfrmValidateFile.Destroy }
    Inherited Destroy ;
  End ;  { TfrmValidateFile.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.SetFieldTypes                                 *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmValidateFile.SetFieldTypes ;
  Type
    aLeftTop = array[1..12 , 1..2] of Integer ;

  Const
     nLeftTop : aLeftTop = ((21 ,  16) , (114 ,  16) , (207 ,  16) ,
                            (21 ,  48) , (114 ,  48) , (207 ,  48) ,
                            (21 ,  80) , (114 ,  80) , (207 ,  80) ,
                            (21 , 112) , (114 , 112) , (207 , 112)  );
  Var
    nIdx        ,
    nCnt        ,
    nLeft       ,
    nFieldType    : Integer ;
    cFieldTypes   : String  ;

  Begin  { TfrmValidateFile.SetFieldTypes}
    ednLabels[ 1] := @lblB ;
    ednLabels[ 2] := @lblC ;
    ednLabels[ 3] := @lblD ;
    ednLabels[ 4] := @lblF ;
    ednLabels[ 5] := @lblG ;
    ednLabels[ 6] := @lblI ;
    ednLabels[ 7] := @lblL ;
    ednLabels[ 8] := @lblM ;
    ednLabels[ 9] := @lblN ;
    ednLabels[10] := @lblP ;
    ednLabels[11] := @lblT ;
    ednLabels[12] := @lblY ;

    ednEdits[ 1] := @BadB ;
    ednEdits[ 2] := @BadC ;
    ednEdits[ 3] := @BadD ;
    ednEdits[ 4] := @BadF ;
    ednEdits[ 5] := @BadG ;
    ednEdits[ 6] := @BadI ;
    ednEdits[ 7] := @BadL ;
    ednEdits[ 8] := @BadM ;
    ednEdits[ 9] := @BadN ;
    ednEdits[10] := @BadP ;
    ednEdits[11] := @BadT ;
    ednEdits[12] := @BadY ;

    With xDbfPtr^ do
      Begin
        cFieldTypes := GetAllFieldTypes ;

        For nFieldType := 1 to NoOfFieldTypes do
          Begin
            nIdx := PosStr(LegalFieldTypes[nFieldType] , cFieldTypes) ;
            ednEnabled[nFieldType] := (nIdx <> 0) ;
          End ;
      End ;

    nCnt := 1 ;
    For nIdx := 1 to NoOfFieldTypes do
      Begin
        ednEdits[nIdx]^.Enabled  := ednEnabled[nIdx] ;
        ednEdits[nIdx]^.Visible  := ednEnabled[nIdx] ;
        ednEdits[nIdx]^.ShowHint := ednEnabled[nIdx] ;

        ednLabels[nIdx]^.Enabled := False ;
        ednLabels[nIdx]^.Visible := False ;

        If ednEnabled[nIdx] then
          Begin
            With ednEdits[nIdx]^ do
              Begin
                nLeft := nLeftTop[nCnt , 1]  ;
                Left  := nLeft               ;
                Top   := nLeftTop[nCnt , 2]  ;
                Hint  := rsHintErrMsgs[nIdx].cHintMsg ;

                With ednLabels[nCnt]^ do
                  Begin
                    Caption   := LegalFieldTypes[nIdx] ;
                    Width     := 12            ;
                    Left      := nLeft - Width ;
                    Alignment := taCenter      ;
                    Enabled   := True          ;
                    Visible   := True          ;
                  End ;

                nCnt := nCnt + 1 ;
              End ;
          End ;
      End ;
  End ;  { TfrmValidateFile.SetFieldTypes }


{***********************************************************************
*                                                                      *
*       SetMeter                                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure SetMeter(nPerc    : Integer ;
                   nRec     : Integer ;
                   xFormPtr : Pointer  ) ;
  Var
    nType : Integer ;
    cFieldTypes : String[NoOfFieldTypes] ;

  Begin  { SetMeter }
    With TfrmValidateFile(xFormPtr^) , xDbfPtr^ Do
      Begin
        btnPercent.Caption := IntToStr(nPerc) + '%';
        CurrentRecordBox.AsInteger := nRec;

        cFieldTypes := GetAllFieldTypes ;
        For nType := 1 to GetNoFieldTypes do
          ednEdits[PosStr(cFieldTypes[nType] ,
                   LegalFieldTypes)                   ]^.AsInteger :=
            GetFieldErrors(cFieldTypes[nType]) ;

        BadU.AsInteger         := GetFieldErrors('*') ;
        BadDelete.AsInteger    := GetBadFieldNo(0)    ;
        neBadRecords.AsInteger := GetBadRecordCnt     ;
        BadFields.AsInteger    := GetTotalFieldErrors ;

        RepaintEdits(TfrmValidateFile(xFormPtr^)) ;
        Update ;
      End ;
  End ;  { SetMeter }


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.ScanButtonClick                               *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmValidateFile.ScanButtonClick(Sender : TObject) ;
  Begin  { TfrmValidateFile.ScanButtonClick }
    If (not bIsExecuting) then
      Begin
        oBaseThread := thrdValidate.Execute(Self) ;
        bIsExecuting := True ;
      End ;

    If bThreadIsSuspended then
      Begin
        btnScan.Caption := '&Stop' ;
        btnScan.Repaint ;

        clbFieldsCheck.Enabled := True ;

        bThreadIsSuspended := False ;
        thrdValidate.Resume(oBaseThread) ;
      End
    Else
      Begin
        NormalCursor ;
        clbFieldsCheck.Enabled := False ;

        btnScan.Caption := '&Scan'  ;
        btnScan.Repaint ;

        bThreadIsSuspended := True ;
        thrdValidate.Suspend ;
      End ;
  End ;  { TfrmValidateFile.ScanButtonClick }


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.FormActivate                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmValidateFile.FormActivate(Sender: TObject);
  Var
    nField : Integer ;

  Begin  { TfrmValidateFile.FormActivate }
    BlankEdits(Self) ;
    BlankLists(Self) ;

    xDbfPtr^.ResetFieldRecNoLists ;

    edtFileName.Text         := xDbfPtr^.GetDataFileName ;
    neTotalRecords.AsInteger := xDbfPtr^.GetTotalRecords ;

    With xDbfPtr^ , clbFieldsCheck , Items do
      Begin
        BeginUpdate ;
        For nField := 0 to GetFieldCount do
          Add(GetFieldName(nField)) ;
        EndUpdate ;
      End ;

    nBar := 1 ;
  End ;  { TfrmValidateFile.FormActivate }


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.tlbBadRecDblClick                             *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmValidateFile.tlbBadRecDblClick(Sender : TObject) ;
  Var
    nRec : Integer ;

  Begin  { TfrmValidateFile.tlbBadRecDblClick }
    With tlbBadRec Do
      nRec := StrToInt(Trim(Items.Strings[ItemIndex])) ;

    With TfrmRecordDisplay.Create(Self , xDbfPtr , nRec) Do
      Try
        SetRecEnable(False) ;

        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmValidateFile.tlbBadRecDblClick }


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.SplitRecordsClick                             *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmValidateFile.SplitRecordsClick(Sender : TObject) ;
  Var
    cGoodDataName ,
    cGoodMemoName ,
    cBadDataName  ,
    cBadMemoName    : String ;

    nI        ,
    nRecNo    ,
    nGoodRecs ,
    nBadRecs    : Integer ;

  Begin  { TfrmValidateFile.SplitRecordsClick }
    Try
      HourGlassCursor ;  { Show hourglass cursor }

      With xDbfPtr^ Do
        Begin
          cGoodDataName := ForceExtension(GetDataFileName, dbfGoodDataFileExt) ;
          cGoodMemoName := ForceExtension(GetMemoFileName, dbfGoodMemoFileExt) ;

          cBadDataName := ForceExtension(GetDataFileName, dbfBadDataFileExt) ;
          cBadMemoName := ForceExtension(GetMemoFileName, dbfBadMemoFileExt) ;

          nBadRecs  := GetBadListPtr^.Count  ;
          nGoodRecs := GetGoodListPtr^.Count ;

          If nGoodRecs > 0 then
            Begin
              Duplicate(@GoodFile     ,
                        cGoodDataName ,
                        cGoodMemoName  ) ;

              { Should already be 0, but set it anyway. }
              GoodFile.SetTotalRecords(0);
              With xDbfPtr^ Do
                For nI := 1 To nGoodRecs Do
                  Begin
                    enGoodRecs.AsInteger := nI ;
                    enGoodRecs.RePaint ;

                    If nI = nGoodRecs then
                      btnGoodPerc.Caption := '100%'
                    Else
                      btnGoodPerc.Caption := IntToStr((nI * 100) Div nGoodRecs) + '%';

                    If GetRecord(StrToInt(Trim(GetGoodListPtr^.Strings[nI - 1]))) then
                      Begin
                        With GoodFile Do
                          Begin
                            If not AddRecord(xDbfPtr^.GetRecordPtr , nRecNo) then
                              MessageDlg('Good record [' +
                                         IntToStr(xDbfPtr^.GetRecordNo) +
                                         '] not added.'                   ,
                                         mtError ,
                                         [mbOK]  ,
                                         0        ) ;
                          End ;
                      End
                    Else
                      Begin
                        CheckRecordBytesRead ;
                      End ;
                  End ;
              GoodFile.WriteHeader;
            End ;

          If nBadRecs > 0 then
            Begin
              Duplicate(@BadFile     ,
                        cBadDataName ,
                        cBadMemoName  ) ;

               { Should already be 0, but set it anyway. }
              BadFile.SetTotalRecords(0);
              With xDbfPtr^ Do
                For nI := 1 To nBadRecs Do
                  Begin
                    enBadRecs.AsInteger := nI;
                    enBadRecs.RePaint;

                    If nI = nBadRecs then
                      btnBadPerc.Caption := '100%'
                    Else
                      btnBadPerc.Caption := IntToStr((nI * 100) div nBadRecs) + '%';

                    If GetRecord(StrToInt(Trim(GetBadListPtr^.Strings[nI - 1]))) then
                      Begin
                        With BadFile Do
                          Begin
                            If not AddRecord(xDbfPtr^.GetRecordPtr , nRecNo) then
                              Begin
                                ShowMessage('Bad record [' +
                                  IntToStr(xDbfPtr^.GetRecordNo) +
                                  '] not added.');
                              End ;
                          End ;
                      End
                    Else
                      CheckRecordBytesRead ;
                  End ;
              BadFile.WriteHeader ;
            End ;

          FreeAndNil(GoodFile) ;
          FreeAndNil(BadFile)  ;
        End ;

    Finally
      NormalCursor ;  { Restore cursor to normal. }
    End ;
  End ;  { TfrmValidateFile.SplitRecordsClick }


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.BadFieldListBoxClick                          *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmValidateFile.tlbBadFieldsDblClick(Sender : TObject) ;
  Var
    nField : Integer ;
    cField : String  ;

  Begin  { TfrmValidateFile.BadFieldListBoxDblClick }
    With tlbBadFields , xDbfPtr^ Do
      Try
        cField := Items.Strings[ItemIndex] ;
        cField := Copy(cField , 1 , xDbfPtr^.GetNameMaxWidth) ;
        cField := UpperTrim(cField) ;
        nField := GetFieldNo(cField) ;

        If ValidFieldNumber(nField) then
          Begin
            With TfmBadFieldView.Create(oOwner  ,
                                        xDbfPtr ,
                                        nField   ) do
              Try
                ShowModal ;

              Finally
                Free ;
              End ;
          End
        Else
          ShowErrorMessage(errMsgInvalidFieldInBadRecList) ;
      Except
        On e: exception do
          ShowErrorMessage(errMsgInvalidFieldInBadRecList + ' in ' +
                       'TfrmValidateFile.BadFieldListBoxDblClick'#13#10'Error message ' + e.Message);
      End ;
  End ;  { TfrmValidateFile.BadFieldListBoxDblClick }


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.btnResetClick                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmValidateFile.btnResetClick(Sender : TObject) ;
  Var
    nField : Integer ;

  Begin  { TfrmValidateFile.btnResetClick }
    For nField := 0 to (clbFieldsCheck.Items.Count - 1) do
      clbFieldsCheck.Checked[nField] := False ;

    clbFieldsCheckClickCheck(Sender) ;
  End ;  { TfrmValidateFile.btnResetClick }


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.clbFieldsCheckClickCheck                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmValidateFile.clbFieldsCheckClickCheck(Sender : TObject) ;
  Var
    nField : Integer ;

  Begin  { TfrmValidateFile.clbFieldsCheckClickCheck }
    For nField := 0 to (clbFieldsCheck.Items.Count - 1) do
      xDbfPtr^.SetFieldIgnore(nField , clbFieldsCheck.Checked[nField])
  End ;  { TfrmValidateFile.clbFieldsCheckClickCheck }

Procedure TfrmValidateFile.abfThreadTerminate(Sender: TObject);
  Begin
    NormalCursor ;

    btnScan.Enabled := True ;
    btnScan.Caption := '&Scan'  ;
    btnScan.Repaint ;
  End ;


{***********************************************************************
*                                                                      *
*       TfrmValidateFile.thrdValidateExecute                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmValidateFile.thrdValidateExecute(Sender : TObject ;
                                               Params : Pointer  ) ;
  Const
    cProcName = 'TfrmValidateFile.thrdValidateExecute' ;

  Var
    nI      : Integer ;
    nField  : Integer ;
    nErrors : Integer ;

  Begin  { TfrmValidateFile.thrdValidateExecute }
    With xDbfPtr^ Do
      Begin
        If HasMemoFile then
          If not IsMemoFileOpen then
            ErrorMsg(rsErrorMemoFileDoesNotExist , [cProcName]) ;
      End ;

    Try
      HourGlassCursor ;

      tlbBadRec.Items.Clear    ;

      tlbBadRec.Enabled := False ;

      BlankEdits(Self) ;
      BlankLists(Self) ;
      edtFileName.Text := xDbfPtr^.GetDataFileName ;

      With xDbfPtr^ Do
        Begin
          VerifyFile(SetMeter, neTotalRecords.AsInteger, @Self);

         { Enable the split records button if any }
         { invalid records were found.            }
          SplitRecords.Enabled := ((GetBadFieldCnt  > 0) or
                                   (GetBadRecordCnt > 0)    ) ;

          { Check if any bad records were found, then display them.   }
          { Need user to specify whether there needs to be a good/bad }
          { record split.                                             }
          If ((GetBadFieldCnt    > 0) or
              (GetBadRecordCnt   > 0) or
              (GetBadNameListCnt > 0)   ) then
            Begin
              With GetBadNameListPtr^ do
                Begin
                  If GetBadFieldNo(0) > 0 then
                    Add(Pad(' Delete' , GetNameMaxWidth + 1) +
                        ' C ' +
                        PadL(IntToStr(GetBadFieldNo(0)) , 6));
                End ;

              GetBadNameListPtr^.Sort ;

              With tlbBadFields do
                Try
                  tlbBadFields.Items.Clear ;
                  Items.BeginUpdate ;
                  Items.Assign(GetBadNameListPtr^) ;
                  Items.EndUpdate ;
                Except
                  On e: exception do
                    ShowErrorMessage('Error assigning field name list in ' +
                                 'TfrmValidateFile.vgThread1Execute'#13#10'Error message ' + e.Message);
                End ;

              tlbBadRec.Items.Assign(GetBadListPtr^) ;

              With tlbBadFields , Items do
                Begin
                  Items.BeginUpdate ;

                  For nI := 0 To (Items.Count - 1) Do
                    Begin
                      nField := GetFieldNo(Items[nI]) ;
                      If (nField > 0) and (nField <= GetFieldCount) then
                        Try
                          nErrors := GetBadFieldNo(nField);
                          If nErrors > 0 then
                            Items[nI] := PadR(Trim(Items[nI]) , GetNameMaxWidth + 1) + ' ' +
                                         GetFieldType(nField)                        + ' ' +
                                         PadL(IntToStr(nErrors) , 6) ;
                        Except
                          On e: exception do
                            ShowErrorMessage('Error updating bad field list in ' +
                                         'TfrmValidateFile.vgThread1Execute'#13#10'Error message ' + e.Message);
                        End
                      Else
                        If (nField < 0) or (nField > GetFieldCount) then
                          ShowMessage('Field [' + Trim(Items[nI]) + '] not found in list.') ;
                    End ;

                  Items.EndUpdate ;
                End ;
            End ;
        End ;

    Finally
      tlbBadRec.Enabled := True ;

      NormalCursor ;
    End
  End ;  { TfrmValidateFile.thrdValidateExecute }


Procedure BlankIt(oComp : TComponent) ;
  Begin
    If oComp is TFnpNumericEdit then
      TFnpNumericEdit(oComp).Clear ;
  End ;
End.
