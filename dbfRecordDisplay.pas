{***********************************************************************
*                                                                      *
*       dbfRecordDisplay.pas                                           *
*                                                                      *
*       (C) Copyright 1982-2003 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfRecordDisplay ;

Interface

Uses
  Windows , Messages , SysUtils , Classes  , Graphics , Controls   ,
  Forms   , Dialogs  , StdCtrls , ClipBrd  , Grids    ,
  ComCtrls , ExtCtrls , Menus    , Mask , Buttons    ,

  JvEdit       ,
  JvExGrids    ,
  JvExMask     ,
  JvExStdCtrls ,    
  JvGrids      ,
  JvListBox    ,
  JvMaskEdit   ,
  JvMemo       ,
  JvMenus      ,
  JvSpeedBar   ,
  JvSpin       ,
  JvStringGrid ,

  bcClasses     ,
  ngColourGrid  ,
  bcDialogUtilities ,
  bcHeapView    ,
  bcMathUtilities   ,
  bcMemo        ,
  bcNewList     ,
  bcNumericEdit ,
  ngSortGrid    ,
  ngStringGrid  ,
  bcStringUtilities ,
  bcViewMem     ,

  dbfBMPMemoDisplay   ,
  dbfCommon           ,
  dbfConstant         ,
  dbfFieldHexView     ,
  dbfFixedMemoDisplay ,
  dbfStructure        ,
  dbfViewMemo           ;

Type
  TRec = Array[1..MaxRecordWidth] of Char ;

Type
  TfrmRecordDisplay = Class(TForm)
    mnuDisplay : TPopupMenu      ;
    vwmHexRec  : TViewMem        ;
    lblFields  : TNewTextListBox ;

    pnlTop         : TPanel ;
    pnlMiddle      : TPanel ;
    pnlBottom      : TPanel ;
    pnlSplitter1   : TPanel ;
    pnlFileName    : TPanel ;
    pnlSplitter2   : TPanel ;
    pnlTopBottom   : TPanel ;
    pnlButtonRight : TPanel ;
    pnlSlider      : TPanel ;
    Panel1         : TPanel ;
    Panel3         : TPanel ;
    Panel4         : TPanel ;

    Label2 : TLabel ;
    Label3 : TLabel ;
    Label4 : TLabel ;

    btnShow        : TButton ;
    ValidateButton : TButton ;

    btnPicture   : TBitBtn ;
    btnPostShift : TBitBtn ;
    btnRepair    : TBitBtn ;

    pmnDateClick   : TMenuItem ;
    DateTime1      : TMenuItem ;
    N1             : TMenuItem ;
    Byte1          : TMenuItem ;
    Double1        : TMenuItem ;
    Integer1       : TMenuItem ;
    Memo1          : TMenuItem ;
    ShortInt1      : TMenuItem ;
    Word1          : TMenuItem ;
    HexString1     : TMenuItem ;
    Comp1          : TMenuItem ;
    Currency1      : TMenuItem ;
    String1        : TMenuItem ;
    pmnReal6       : TMenuItem ;
    pmnThreeWords1 : TMenuItem ;
    DBVOffset1     : TMenuItem ;
    pmnDBVMemo     : TMenuItem ;
    pmnDBVPars     : TMenuItem ;

    edtFileName : TJvEdit;
    Label1: TLabel;
    ednRecordNo: TFnpNumericEdit;
    sbrRecord: TScrollBar;

    RadioGroup1 : TRadioGroup ;
    RadioGroup2 : TRadioGroup ;

    rbtAscii   : TRadioButton ;
    rbtGrid    : TRadioButton ;
    rbtFields  : TRadioButton ;
    rbtRecord  : TRadioButton ;
    rbtDecimal : TRadioButton ;
    rbtHex     : TRadioButton ;

    Splitter1 : TSplitter ;
    Splitter2 : TSplitter ;
    Splitter3 : TSplitter ;
    Splitter4 : TSplitter ;

    GroupBox1: TGroupBox;
    speRotate: TJvSpinEdit;
    grdFields: TngStringGrid ;

    Procedure btnShowClick(Sender: TObject);
    Procedure ValidateButtonClick(Sender: TObject);
    Procedure sbrRecordChange(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ednRecordNoExit(Sender: TObject);
    Procedure lblFieldsDblClick(Sender: TObject);
    Procedure btnPictureClick(Sender: TObject);
    Procedure lblFieldsClick(Sender: TObject);
    Procedure ednRecordNoKeyPress(Sender: TObject; Var Key: Char);
    Procedure rbtAsciiClick(Sender: TObject);
    Procedure rbtFieldsClick(Sender: TObject);
    Procedure rbtRecordClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);

    Procedure lblFieldsMouseDown(Sender : TObject      ;
                                 Button : TMouseButton ;
                                 Shift  : TShiftState  ;
                                 X , Y  : Integer       ) ;

    Procedure grdFieldsMouseMove(Sender : TObject     ;
                                 Shift  : TShiftState ;
                                 X , Y  : Integer      ) ;
    Procedure grdFieldsMouseDown(Sender : TObject      ;
                                 Button : TMouseButton ;
                                 Shift  : TShiftState  ;
                                 X , Y  : Integer       ) ;
    Procedure btnPostShiftClick(Sender: TObject);

    Procedure rbtGridClick(Sender: TObject);
    Procedure grdFieldsDblClick(Sender: TObject);
    Procedure btnRepairClick(Sender: TObject);

    Procedure grdFieldsDrawCell( Sender : TObject        ;
                                 aCol,
                                 aRow   : Integer        ;
                                 Rect   : TRect          ;
                                 State  : TGridDrawState  ) ;

    Procedure Double1Click(Sender: TObject);
    Procedure Integer1Click(Sender : TObject) ;
    Procedure HexString1Click(Sender : TObject) ;
    Procedure ShortInt1Click(Sender : TObject) ;
    Procedure Word1Click(Sender : TObject) ;
    Procedure Byte1Click(Sender : TObject) ;
    Procedure Memo1Click(Sender: TObject);
    Procedure Comp1Click(Sender: TObject);
    Procedure Currency1Click(Sender: TObject);
    Procedure String1Click(Sender: TObject);
    Procedure pmnReal6Click(Sender: TObject);
    Procedure pmnThreeWords1Click(Sender: TObject);
    Procedure DBVOffset1Click(Sender: TObject);
    Procedure pmnDBVMemoClick(Sender: TObject);
    Procedure pmnDBVParsClick(Sender: TObject);
    procedure rbtDecimalClick(Sender: TObject);
    procedure pmnDateClickClick(Sender: TObject);

  Protected { Protected declarations }
    oOwner : TComponent ;

    bRightSel      : Boolean ;
    nSelectedField : Integer ;


    Procedure DisplayRecordBuffer(pRec : Pointer) ;
    Procedure Shifter(nShift : Integer) ;
    Function  GetDisplayRecPtr : Pointer ;
    Procedure RepaintCurrentDisplay ;

  Private  { Private declarations }
    xDbfPtr           : pTxBase ;
    nCurrentRec       : Integer ;
    bRecNumberEnabled : Boolean ;

    bAscii  : Boolean ;
    bFields : Boolean ;
    bGrid   : Boolean ;
    bRecord : Boolean ;

    cBuffer : tRec ;
    cRotate : tRec ;

    nSelectedFld : Integer ;

    nMousePosX : Integer ;
    nMousePosY : Integer ;

  Public
    Constructor Create(aOwner : TComponent ;
                       xPtr   : pTxBase  ;
                       nRec   : Integer     ) ; Reintroduce ;
    Destructor Destroy ; override ;

    Procedure SetRecEnable(bEnable : Boolean) ;
  End ;

Implementation
  Uses
    dbfResources ;

  Const
    { Special row numbers }
    nTitleRow  = 0 ;
    nDeleteRow = 1 ;

    { Column numbers }
    nFieldNo  = 0 ;
    nName     = 1 ;
    nV        = 2 ;
    nType     = 3 ;
    nTypeDesc = 4 ;
    nWidth    = 5 ;
    nDec      = 6 ;
    nOffset   = 7 ;
    nData     = 8 ;

    ColumnNames : Array[nFieldNo..nData] of String[9] =
                      ('Fld #'     ,
                       'Name'      ,
                       'V'         ,
                       'Type'      ,
                       'Type Desc' ,
                       'Width'     ,
                       'Dec'       ,
                       'Offset'    ,
                       'Data'       ) ;
{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TRecordDisplayForm.Create                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmRecordDisplay.Create(aOwner : TComponent ;
                                    xPtr   : pTxBase     ;
                                    nRec   : Integer     ) ;
  Begin  { TfmRecordDisplay.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    bAscii  := False ;
    bFields := False ;
    bGrid   := True  ;
    bRecord := False ;

    bRightSel := False ;

    If nRec = 0 Then
      nRec := 1;

    xDbfPtr := xPtr ;
    bRecNumberEnabled := False ;

    sbrRecord.Enabled := (xDbfPtr^.GetTotalRecords > 1) ;
    sbrRecord.Visible := sbrRecord.Enabled ;

    sbrRecord.Min := 1 ;
    sbrRecord.Max := xDbfPtr^.GetTotalRecords ;

    nCurrentRec := nRec ;
    sbrRecord.Position := nCurrentRec ;

    With xDbfPtr^ do
      Begin
        speRotate.MinValue := (-GetRecordSize + 1) ;
        speRotate.MaxValue := (GetRecordSize  - 1) ;
      End ;
  End ;  { TfmRecordDisplay.Create }


{***********************************************************************
*                                                                      *
*       TRecordDisplayForm.Destroy                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmRecordDisplay.Destroy ;
  Begin  { TfmRecordDisplay.Destroy }
    Inherited Destroy ;
  End ;  { TfmRecordDisplay.Destroy }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.btnShowClick                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.btnShowClick(Sender : TObject) ;
  Begin  { TfmRecordDisplay.btnShowClick }
//    UpdateRecordDisplay(nCurrentRec) ;
  End ;  { TfmRecordDisplay.btnShowClick }


{***********************************************************************
*                                                                      *
*       TRecordDisplayForm.ValidateButtonClick                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.ValidateButtonClick(Sender : TObject) ;
  Var
    nField : Integer ;
    cStr,
    cStr1  : String ;

  Begin  { TRecordDisplayForm.ValidateButtonClick }
    With xDbfPtr^ Do
      Begin
        ZapCounts ;

        If not VerifyRecord(GetRecordPtr) then
          Begin
            If GetBadFieldCnt > 0 Then
              Begin
                cStr := '';
                For nField := 1 To GetFieldCount do
                  Begin
                    If GetDataAreaPtr^.dbfBadFieldList[nField] > 0 then
                      cStr := cStr + '    ' + Pad(GetFieldName(nField) , 12) +
                              GetFieldType(nField) + CRLF ;
                  End ;

                cStr := Trim(cStr) ;
                cStr1 := 'Record has ' + IntToStr(GetBadFieldCnt) + ' bad field';
                If GetBadFieldCnt > 1 Then
                  cStr1 := cStr1 + 's';
                ShowErrorMessage(cStr1 + CRLF + '    ' + cStr) ;
              End ;
          End ;
      End ;
  End ;  { TRecordDisplayForm.ValidateButtonClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.sbrRecordChange                               *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.sbrRecordChange(Sender : TObject) ;
  Begin  { TfmRecordDisplay.sbrRecordChange }
    btnPicture.Enabled := False ;
    btnPicture.Visible := False ;

    speRotate.AsInteger := 0 ;

    btnShow.Click;
    nCurrentRec := sbrRecord.Position ;
    ednRecordNo.AsInteger := nCurrentRec ;
    ednRecordNoExit(Sender) ;
  End ;  { TfmRecordDisplay.sbrRecordChange }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.FormShow                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.FormShow(Sender : TObject) ;
  Begin  { TfmRecordDisplay.FormShow }
    WindowState := wsMaximized ;

    edtFileName.Text := xDbfPtr^.GetDataFileName ;
    edtFileName.Repaint ;

    sbrRecord.Min := 1;
    sbrRecord.Max := xDbfPtr^.GetTotalRecords ;

    lblFields.Clear;
    ValidateButton.Enabled := False;
    ednRecordNo.AsInteger := nCurrentRec;
    ednRecordNo.Enabled := bRecNumberEnabled ;
    ednRecordNo.Repaint ;

//    UpdateRecordDisplay(nCurrentRec) ;
  End ;  { TfmRecordDisplay.FormShow }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.ednRecordNoExit                               *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.ednRecordNoExit(Sender : TObject) ;
  Var
    nRec : Integer ;

  Begin  { TfmRecordDisplay.ednRecordNoExit }
    With xDbfPtr^ , GetDataAreaPtr^ do
      Begin
        nRec := ednRecordNo.AsInteger ;
        If not GetRecord(nRec) then
          Begin
            CheckRecordBytesRead ;
          End ;
      End ;

    If bAscii or bFields then
      Begin
        vwmHexRec.Hide ;
        grdFields.Hide ;
        lblFields.Items.Clear ;
        lblFields.Show ;
      End
    Else
      If bRecord then
        Begin
          lblFields.Hide  ;
          grdFields.Hide  ;
          vwmHexRec.Clear ;
          vwmHexRec.Show  ;
        End
      Else
        If bGrid then
          Begin
            lblFields.Hide ;
            vwmHexRec.Hide ;
            
            grdFields.Update ;
            grdFields.Show ;
          End ;

    nCurrentRec := ednRecordNo.AsInteger ;
    ednRecordNo.Repaint ;
    If ednRecordNo.AsInteger <= 0 then
      Exit ;

    With xDbfPtr^ , GetDataAreaPtr^ do
      Begin
        ValidateButton.Enabled := True ;
        If speRotate.AsInteger = 0 then
          DisplayRecordBuffer(GetRecordPtr)
        Else
          Begin
            Shifter(speRotate.AsInteger) ;
            DisplayRecordBuffer(@cRotate) ;
            If bGrid then
              grdFields.Repaint ;
          End ;
      End ;

    ednRecordNo.Repaint ;
  End ;  { TfmRecordDisplay.ednRecordNoExit }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.DisplayRecordBuffer                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.DisplayRecordBuffer(pRec : Pointer) ;
  Var
    nField : Integer ;

    cFld   ,
    cStr   ,
    cItem  ,
    cStar  ,
    cDate    : String ;

  Begin  { TfmRecordDisplay.DisplayRecordBuffer }
    If pRec = nil then
      Raise Exception.Create('Record buffer pointer is nil in routine'#$0D#$0A +
                             'TfmRecordDisplay.DisplayRecordBuffer') ;

    lblFields.Clear ;

    With xDbfPtr^ , GetDataAreaPtr^ do
      Begin
        If (bAscii or bFields) then
          Begin
            lblFields.Items.BeginUpdate ;
            For nField := 0 to GetFieldCount do
              With lblFields do
                Begin
                  cStr := GetFieldDisplayString(pRec   ,
                                                nField ,
                                                rbtFields.Checked) ;

                  cStar := xDbfPtr^.CheckVerifyFieldChar(GetDisplayRecPtr, nField) ;

                  If (GetFieldType(nField) = 'D') then
                    cDate := ' ' + xDateToEnglish(cStr)
                  Else
                    cDate := '' ;

                  cFld := Trim(GetFieldName(nField)) ;
                  cItem := PadR(cFld , GetNameMaxWidth + 2) +
                           PadR(cStar , 2) +
                           PadR(GetFieldType(nField) , 2) +
                           PadLeft(Trim(IntToStr(GetFieldWidth(nField))) , 4) +
                           PadLeft(Trim(IntToStrBlank(GetFieldDecimals(nField))) , 3) +
                           ' [' + cStr + ']' + cDate ;
                  Items.Add(cItem) ;
                End ;

            lblFields.Items.EndUpdate ;
          End
        Else
          If bRecord then
            Begin
              vwmHexRec.BeginUpdate ;
              vwmHexRec.SetMemory(pRec , xDbfPtr^.GetRecordSize , True) ;
              vwmHexRec.EndUpdate ;
            End
          Else
            If bGrid then
              Begin
                grdFields.Repaint ;
              End
            Else
              Raise Exception.Create('Format selection is invalid.') ;
      End ;
  End ;  { TfmRecordDisplay.DisplayRecordBuffer }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.lbFieldsDblClick                              *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.lblFieldsDblClick(Sender : TObject) ;
  Begin  { TfmRecordDisplay.lbFieldsDblClick }
    If xDbfPtr^.CheckFieldRead(nSelectedField) then
      Begin
        If xDbfPtr^.GetFieldType(nSelectedField) in MemoFieldTypes then
          Begin
            With TfrmFixedMemo.Create(Self    ,
                                      xDbfPtr ,
                                      nSelectedField) do
              Try
                ShowMemoFixed ;

              Finally
                Free ;
              End ;
          End
        Else
          Begin
            With TfmFieldHexView.Create(oOwner  ,
                                        xDbfPtr ,
                                        nSelectedField) do
              Try
                ShowModal ;

              Finally
                Free ;
              End ;
          End ;
      End
    Else
      ErrorMsg(ErrMsgUndefinedField) ;
  End ;  { TfmRecordDisplay.lbFieldsDblClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.btnPictureClick                               *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.btnPictureClick(Sender : TObject) ;
  Var
    nMemo : Integer ;

  Begin  { TfmRecordDisplay.btnPictureClick }
    nMemo := xDbfPtr^.GetCurrentMemoNumber(lblFields.Row) ;

    With TfmBMPMemoDisplay.Create(Self    ,
                                  xDbfPtr ,
                                  nMemo    ) do
    Try
      ShowModal ;
    Finally
      Free ;
    End ;
  End ;  { TfmRecordDisplay.btnPictureClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.lbFieldsClick                                 *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.lblFieldsClick(Sender : TObject) ;
  Begin  { TfmRecordDisplay.lbFieldsClick }
    btnPicture.Enabled := (xDbfPtr^.GetFieldType(lblFields.Row) = 'B') ;
    btnPicture.Visible := btnPicture.Enabled ;
  End ;  { TfmRecordDisplay.lbFieldsClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.RecordNumberBoxKeyPress                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.ednRecordNoKeyPress(    Sender : TObject ;
                                                Var Key    : Char     ) ;
  Begin  { TfmRecordDisplay.enRecordNoKeyPress }
    If Key = Chr($00) Then
      Begin
        { Don't try to move outside valid record range. }
        If xDbfPtr^.ValidRecordNumber(ednRecordNo.AsInteger) Then
          nCurrentRec := ednRecordNo.AsInteger
        Else
          ednRecordNo.AsInteger := nCurrentRec ;

        sbrRecord.Position := nCurrentRec ;
        sbrRecord.Update;
      End ;
  End ;  { TfmRecordDisplay.enRecordNoKeyPress }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.SetRecEnable                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.SetRecEnable(bEnable : Boolean) ;
  Begin  { TfmRecordDisplay.SetRecEnable }
    bRecNumberEnabled := bEnable ;
  End ;  { TfmRecordDisplay.SetRecEnable }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.rbtAsciiClick                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.rbtAsciiClick(Sender : TObject) ;
  Begin  { TfmRecordDisplay.rbtAsciiClick }
    bAscii  := True  ;
    bFields := False ;
    bGrid   := False ;
    bRecord := False ;

    ednRecordNoExit(Sender) ;
  End ;  { TfmRecordDisplay.rbtAsciiClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplayForm.rbtFieldsClick                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.rbtFieldsClick(Sender: TObject);
  Begin  { TfmRecordDisplayForm.rbtFieldsClick }
    bAscii  := False ;
    bFields := True  ;
    bGrid   := False ;
    bRecord := False ;

    ednRecordNoExit(Sender) ;
  End ;  { TfmRecordDisplayForm.rbtFieldsClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplayForm.rbtRecordClick                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.rbtRecordClick(Sender : TObject) ;
  Begin  { TfmRecordDisplayForm.rbtRecordClick }
    bAscii  := False ;
    bFields := False ;
    bGrid   := False ;
    bRecord := True  ;

    ednRecordNoExit(Sender) ;
  End ;  { TfmRecordDisplayForm.rbtRecordClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.rbtGridClick                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.rbtGridClick(Sender : TObject) ;
  Begin  { TfmRecordDisplay.rbtGridClick }
    bAscii  := False ;
    bFields := False ;
    bGrid   := True  ;
    bRecord := False ;

    ednRecordNoExit(Sender) ;
  End ;  { TfmRecordDisplay.rbtGridClick }


{***********************************************************************
*                                                                      *
*       TRecordDisplayForm.FormCreate                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.FormActivate(Sender : TObject) ;
  Var
    nRows     : Integer ;
    cStr      : String  ;
    nFldWidth : Integer ;

  Begin  { TfmRecordDisplayForm.FormActivate }
    rbtAscii.Checked  := bAscii  ;
    rbtFields.Checked := bFields ;
    rbtGrid.Checked   := bGrid   ;
    rbtRecord.Checked := bRecord ;

    ValidateButton.Enabled := False ;

    With xDbfPtr^ do
      Begin
        rbtDecimal.Checked := GetOptionsPtr^.dbvShowFieldOffset ;
        rbtHex.Checked     := GetOptionsPtr^.dbvShowFieldHex    ;
        
        edtFileName.Text := GetDataFileName ;

        lblFields.Clear ;

        nRows := GetFieldCount + 2 ;
        grdFields.RowCount := nRows ;

        cStr := GetFieldName(GetNameMaxWidthFld) ;
        { Make sure the DELETE byte name is length tested. }
        If Length(cStr) < Length(GetFieldName(0)) then
          cStr := GetFieldName(0) ;

        With grdFields do
          Begin
            SetColumnWidth(nFieldNo , MaxInteger(GetTextWidthInt(1) ,
                                          GetTextWidth(ColumnNames[nFieldNo])) +
                                          (GridLineWidth + 1) * 3) ;
            SetColumnWidth(nName , MaxInteger(GetTextWidth(cStr) ,
                                          GetTextWidth(ColumnNames[nName])) +
                                          (GridLineWidth + 1) * 2) ;
            SetColumnWidth(nV , MaxInteger(GetTextWidthInt(1) ,
                                          GetTextWidth(ColumnNames[nV])) +
                                          (GridLineWidth + 1) * 3) ;
            SetColumnWidth(nType , MaxInteger(GetTextWidthInt(1) ,
                                          GetTextWidth(ColumnNames[nType])) +
                                          (GridLineWidth + 1) * 3) ;
            SetColumnWidth(nTypeDesc , MaxInteger(GetTextWidthInt(FieldTypeNamesMaxWidth) ,
                                          GetTextWidth(ColumnNames[nTypeDesc])) +
                                          (GridLineWidth + 1) * 2) ;
            SetColumnWidth(nWidth  , GetTextWidth(ColumnNames[nWidth]) + (GridLineWidth + 1) * 2) ;
            SetColumnWidth(nDec    , GetTextWidth(ColumnNames[nDec]) + (GridLineWidth + 1) * 2) ;
            SetColumnWidth(nOffset , GetTextWidth(ColumnNames[nOffset]) + (GridLineWidth + 1) * 2) ;



            nFldWidth := MaxInteger(GetTextWidthInt(xDbfPtr^.GetFieldMaxWidth) ,
                                    GetTextWidth(ColumnNames[nTypeDesc])) +
                                    (GridLineWidth + 1) * 2 ;
            If HasFieldType('T') then
              nFldWidth := MaxInteger(nFldWidth , GetTextWidthInt(40)) ;
            SetColumnWidth(nData , nFldWidth) ;
          End ;
      End ;

    ednRecordNoExit(Sender) ;
    WindowState := wsMaximized ;
  End ;  { TfmRecordDisplayForm.FormActivate }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.lblFieldsMouseDown                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.lblFieldsMouseDown(Sender : TObject      ;
                                               Button : TMouseButton ;
                                               Shift  : TShiftState  ;
                                               X , Y  : Integer       ) ;
  Begin  { TfmRecordDisplay.lblFieldsMouseDown }
    nSelectedField := lblFields.Row ;
    If ((nSelectedField >= 0) and (nSelectedField <= lblFields.Items.Count)) then
      Begin
        bRightSel := True ;
      End
    Else
      bRightSel := False ;
  End ;  { TfmRecordDisplay.lblFieldsMouseDown }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.Shifter                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.Shifter(nShift : Integer) ;
  Var
    nIdx  ,
    nSize   : Integer ;

  Begin  { TfmRecordDisplay.Shifter }
    If (nShift = 0) then
     Exit ;

    With xDbfPtr^ do
      Try
        nSize := GetRecordSize ;
        GetRecord(ednRecordNo.AsInteger) ;
        CheckRecordBytesRead ;
        Move(GetRecordPtr^ , cBuffer , nSize) ;

        If nShift < 0 then
          nShift := nShift + nSize ;
        nIdx := nSize - nShift ;

        Move(cBuffer           , cRotate[nShift + 1] , nIdx  ) ;
        Move(cBuffer[nIdx + 1] , cRotate             , nShift) ;

      Except
        On e: exception do
          ShowErrorMessage('Exception in ' +
                           'TfmRecordDisplay.Shifter'#13#10'Error message ' + e.Message);
      End ;
  End ;  { TfmRecordDisplay.Shifter }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.btnPostShiftClick                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.btnPostShiftClick(Sender : TObject) ;
  Begin  { TfmRecordDisplay.btnPostShiftClick }
    With xDbfPtr^ do
      Begin
        { Write an un-shifted Record.  This can be used }
        { to fix an incomplete Record by appending zero }
        { bytes in place of the missing field(s).       }
        If speRotate.AsInteger = 0 then
          Move(GetRecordPtr^ , cRotate , GetRecordSize) ;

        If PutRecordPtr(@cRotate , ednRecordNo.AsInteger) then
          Begin
            FlushDataFile ;
            FlushMemoFile ;
          End ;

        WriteHeader ;
      End ;
  End ;  { TfmRecordDisplay.btnPostShiftClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.GetDisplayRecPtr                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TfrmRecordDisplay.GetDisplayRecPtr : Pointer ;
  Begin  { TfmRecordDisplay.GetDisplayRecPtr }
    If speRotate.AsInteger = 0 then
      Result := xDbfPtr^.GetRecordPtr
    Else
      Result := @cRotate ;
  End ;  { TfmRecordDisplay.GetDisplayRecPtr }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.grdFieldsDblClick                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.grdFieldsDblClick(Sender : TObject) ;
  Var
    nField : Integer ;

  Begin  { TfmRecordDisplay.grdFieldsDblClick }
    nField := grdFields.Row - 1 ;
    
    With xDbfPtr^ do
      If CheckFieldRead(nField) then
        Begin
          Case GetFieldType(nField) of
            'M' :
              Begin
                With TfrmFixedMemo.Create(Self    ,
                                          xDbfPtr ,
                                          nField   ) do
                  Try
                    ShowMemoFixed ;

                  Finally
                    Free ;
                  End ;
              End ;
          Else
            Begin
              With TfmFieldHexView.Create(oOwner  ,
                                          xDbfPtr ,
                                          nField   ) do
                Try
                  ShowModal ;

                Finally
                  Free ;
                End ;
            End ;
          End ;
        End
      Else
        ShowErrorMessage('Cannot display/edit this field.' + CRLF +
                     '(undefined and not in Record buffer)'   ) ;
  End ;  { TfmRecordDisplay.grdFieldsDblClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.btnRepairClick                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.btnRepairClick(Sender : TObject) ;
  Var
    nField : Integer ;

  Begin  { TfmRecordDisplay.btnRepairClick }
    With xDbfPtr^ do
      Begin
        If not CheckRecordBytesRead then
          Begin
            For nField := 1 To GetFieldCount do
              Begin
                If not CheckFieldRead(nField) then
                  SetFieldDefault(GetRecordPtr , nField) ;
              End ;  { For nField := 1 To GetFieldCount do }

            SetRecordBytesRead(GetRecordSize) ;
          End ;

        For nField := 0 To GetFieldCount do
          If not VerifyField(GetRecordPtr , nField) then
            FixField(GetRecordPtr , nField) ;
      End ;

    RepaintCurrentDisplay ;
  End ;  { TfmRecordDisplay.btnRepairClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.RepaintCurrentDisplay                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.RepaintCurrentDisplay ;
  Begin  { TfmRecordDisplay.RepaintCurrentDisplay }
    If (bAscii or bFields or bRecord) then
      DisplayRecordBuffer(xDbfPtr^.GetRecordPtr)
    Else
      If bGrid then
        grdFields.Repaint ;
  End ;  { TfmRecordDisplay.RepaintCurrentDisplay }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.grdFieldsDrawCell                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.grdFieldsDrawCell( Sender : TObject        ;
                                               aCol,
                                               aRow   : Integer        ;
                                               Rect   : TRect          ;
                                               State  : TGridDrawState  ) ;
  Const
    cProcName = 'TfrmRecordDisplay.grdFieldsDrawCell' ;

  Var
    nField   : Integer ;
    bShowRed : Boolean ;
    cStr     : String  ;

    oAlign : TAlignment  ;

    oColour : TColor ;

  Begin  { TfmRecordDisplay.grdFieldsDrawCell }
    bShowRed := False ;
    oAlign   := taLeftJustify ;

    nField := aRow - 1 ;

    If (xDbfPtr^.ValidFieldNumber(nField) or (aRow = nTitleRow)) then
      With (Sender as TDrawGrid).Canvas do
        Begin
          // set brush
          If gdFixed in State then
            Begin
              If ((aRow = nDeleteRow) and (aCol = nName)) then  { DELETE field name }
                Brush.Color := clGray
              Else
                Brush.Color := clBtnFace ;
            End
          Else
            If (aRow mod 2) <> 0 then
              Brush.Color := clWindow
            Else
              Brush.Color := clInfoBK ;

          // paint the cell
          FillRect(Rect) ;

          If (aRow = nTitleRow) then
            Begin
              cStr := ColumnNames[aCol] ;
              bShowRed := True ;
            End ;

          Case aCol of
            nV , nType :
              Begin
                If (aRow > 0) then
                  Case aCol of
                    nV : Try
                           cStr := xDbfPtr^.CheckVerifyFieldChar(GetDisplayRecPtr, nField) ;
                           bShowRed := (PosStr(cStr , '*U') > 0) ;
                         Except
                           On e: exception do
                             ShowErrorMessage('Error setting nV in drawing record grid in ' +
                                              cProcName + #13#10'Error message ' + e.Message);
                         End  ;

                    nType : Try
                              cStr := xDbfPtr^.GetFieldType(nField) ;
                            Except
                              On e: exception do
                                ShowErrorMessage('Error setting nType in drawing record grid in ' +
                                                 cProcName + #13#10'Error message ' + e.Message);
                            End ;
                  End ;

                oAlign := taCenter ;
              End ;

            nFieldNo ,
            nWidth   ,
            nDec     ,
            nOffset   :
              Begin
                If (aRow >= 0) then // right justified
                  Begin
                    If aRow > 0 then
                      Begin
                        Case aCol of
                          nFieldNo : If nField > 0 then
                                       cStr := IntToStrBlank(nField) + '.' ;
                          nWidth   : cStr := IntToStr(xDbfPtr^.GetFieldWidth(nField)) ;

                          nDec     : Begin
                                       bShowRed := not xDbfPtr^.CheckFieldDecimalType(nField) ;
                                       cStr := IntToStrBlank(xDbfPtr^.GetFieldDecimals(nField)) ;
                                     End ;

                          nOffset  : cStr := IntToStr(xDbfPtr^.GetFieldOffset(nField)) ;
                        End ;
                      End ;

                    oAlign := taRightJustify ;
                  End ;
              End ;

            nName     ,
            nTypeDesc ,
            nData     :
              Begin
                If nField >= 0 then
                  Case aCol of
                    nName : cStr := xDbfPtr^.GetFieldName(nField) ;

                    nTypeDesc : cStr := xDbfPtr^.GetFieldTypeName(nField) ;

                    nData :
                      With xDbfPtr^ do
                        Try
                          If xDbfPtr^.CheckVerifyFieldChar(GetDisplayRecPtr, nField) = 'U' then
                            Begin
                              cStr := '< field undefined on record >' ;
                              bShowRed := True ;
                            End
                          Else
                            cStr := GetFieldDisplayString(GetDisplayRecPtr  ,
                                                          nField            ,
                                                          rbtFields.Checked  ) ;
                        Except
                          On e: exception do
                            ShowErrorMessage('Error retrieving GetFieldDisplayString in grid draw in ' +
                                             'TfmRecordDisplay.grdFieldsDrawCell'#13#10'Error message ' + e.Message);
                        End ;
                  End ;

                oAlign := taLeftJustify ;
              End ;
          End ;  { Case Col of }

          oColour := Font.Color ;
          If bShowRed then
            Font.Color := clRed
          Else
            Font.Color := clBlue ;

          // (Sender as TngStringGrid).DrawStr(Rect , cStr , oAlign) ;
          grdFields.DrawStr(Rect , cStr , oAlign) ;
          Font.Color := oColour ;
        End ;  { With (Sender as TDrawGrid).Canvas do }
  End ;  { TfmRecordDisplay.grdFieldsDrawCell }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.grdFieldsMouseMove                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.grdFieldsMouseMove(Sender : TObject     ;
                                              Shift  : TShiftState ;
                                              X , Y  : Integer      ) ;
  Begin  { TfmRecordDisplay.grdFieldsMouseMove }
    With grdFields do
      Begin
        nMousePosX := X  + (GridlineWidth * 2) ;
        nMousePosY := Y + Top + ((DefaultRowHeight + GridlineWidth) * 2) ;
      End ;
  End ;  { TfmRecordDisplay.grdFieldsMouseMove }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.grdFieldsMouseDown                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.grdFieldsMouseDown(Sender : TObject      ;
                                              Button : TMouseButton ;
                                              Shift  : TShiftState  ;
                                              X , Y  : Integer       ) ;
  Begin  { TfmRecordDisplay.grdFieldsMouseDown }
    If Sender is TngStringGrid then
      With Sender as TngStringGrid do
        Begin
          If Button = mbRight then
            Begin
              nSelectedFld := Y div (DefaultRowHeight + GridLineWidth) ;
              If (nSelectedFld < 0) or (nSelectedFld > (RowCount - 1)) then
                nSelectedFld := -1 ;
            End
          Else
            nSelectedFld := -1 ;
        End ;

    If nSelectedFld <> -1 then
      Begin
        nSelectedFld := grdFields.TopRow + nSelectedFld - 2 ;
      End ;
  End ;  { TfmRecordDisplay.grdFieldsMouseDown }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.Double1Click                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.Double1Click(Sender: TObject);
  Begin  { TfmRecordDisplay.Double1Click }
    If nSelectedFld >= 0 then
      With xDbfPtr^ do
        Begin
          ShowMessage(FloatToStr(AsDouble(GetRecordPtr , nSelectedFld))) ;
        End ;
  End ;  { TfmRecordDisplay.Double1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.Integer1Click                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.Integer1Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.Integer1Click }
    If nSelectedFld >= 0 then
      With xDbfPtr^ do
        Begin
          ShowMessage(IntToStr(AsInteger(GetRecordPtr , nSelectedFld))) ;
        End ;
  End ;  { TfmRecordDisplay.Integer1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.HexString1Click                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.HexString1Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.HexString1Click }
    If nSelectedFld >= 0 then
      With xDbfPtr^ do
        Begin
          ShowMessage(AsHexStr(GetRecordPtr , nSelectedFld)) ;
        End ;
  End ;  { TfmRecordDisplay.HexString1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.ShortInt1Click                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.ShortInt1Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.ShortInt1Click }
    If nSelectedFld >= 0 then
      With xDbfPtr^ do
        Begin
          ShowMessage(IntToStr(AsShortInt(GetRecordPtr , nSelectedFld))) ;
        End ;
  End ;  { TfmRecordDisplay.ShortInt1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.Word1Click                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.Word1Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.Word1Click }
    If nSelectedFld >= 0 then
      With xDbfPtr^ do
        Begin
          ShowMessage(IntToStr(AsWord(GetRecordPtr , nSelectedFld))) ;
        End ;
  End ;  { TfmRecordDisplay.Word1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.Byte1Click                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.Byte1Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.Byte1Click }
    If nSelectedFld >= 0 then
      With xDbfPtr^ do
        Begin
          ShowMessage(IntToStr(AsByte(GetRecordPtr , nSelectedFld))) ;
        End ;
  End ;  { TfmRecordDisplay.Byte1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.Memo1Click                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.Memo1Click(Sender : TObject) ;
  Var
    nMemo : Integer ;

  Begin  { TfmRecordDisplay.Memo1Click }
    If nSelectedFld >= 0 then
      Begin
        nMemo := xDbfPtr^.GetCurrentMemoNumber(nSelectedFld) ;
        If nMemo > 0 then
          With TfrmFixedMemo.Create(oOwner       ,
                                    xDbfPtr      ,
                                    nSelectedFld  ) do
            Try
              ShowMemoFixed ;
            Finally
              Free ;
            End ;
      End ;
  End ;  { TfmRecordDisplay.Memo1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.Comp1Click                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.Comp1Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.Comp1Click }
    If nSelectedFld >= 0 then
      With xDbfPtr^ do
        Begin
          ShowMessage(FloatToStr(AsComp(GetRecordPtr , nSelectedFld))) ;
        End ;
  End ;  { TfmRecordDisplay.Comp1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.Currency1Click                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.Currency1Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.Currency1Click }
    If nSelectedFld >= 0 then
      With xDbfPtr^ do
        ShowMessage(Format('%16.4f' , [AsCurrency(GetRecordPtr , nSelectedFld)])) ;
  End ;  { TfmRecordDisplay.Currency1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.String1Click                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.String1Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.String1Click }
    With xDbfPtr^ do
      ShowMessage(MakeStr(GetFieldPtr(GetRecordPtr , nSelectedFld) , GetFieldWidth(nSelectedFld))) ;
  End ;  { TfmRecordDisplay.String1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.pmnReal6Click                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.pmnReal6Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.pmnReal6Click }
    With xDbfPtr^ do
      ShowMessage(FloatToStr(Real48(GetFieldPtr(GetRecordPtr , nSelectedFld)^))) ;
  End ;  { TfmRecordDisplay.pmnReal6Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.pmnThreeWords1Click                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.pmnThreeWords1Click(Sender : TObject) ;
  Var
    pWord1 : pWord ;
    pWord2 : pWord ;
    pWord3 : pWord ;

  Begin  { TfmRecordDisplay.pmnThreeWords1Click }
    With xDbfPtr^ do
      Begin
        pWord1 := GetFieldPtr(GetRecordPtr , nSelectedFld) ;
        pWord2 := Pointer(Integer(pWord1) + 2) ;
        pWord3 := Pointer(Integer(pWord2) + 2) ;

        ShowMessage(IntToStr(pWord1^) + '  ' +
                    IntToStr(pWord2^) + '  ' +
                    IntToStr(pWord3^)         ) ;
      End ;
  End ;  { TfmRecordDisplay.pmnThreeWords1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.DBVOffset1Click                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.DBVOffset1Click(Sender : TObject) ;
  Begin  { TfmRecordDisplay.DBVOffset1Click }
    With xDbfPtr^ do
      ShowMessage(IntToStr(GetMemoOffsetDBV(GetRecordPtr , nSelectedFld))) ;
  End ;  { TfmRecordDisplay.DBVOffset1Click }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.pmnDBVMemoClick                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.pmnDBVMemoClick(Sender : TObject) ;
  Begin  { TfmRecordDisplay.pmnDBVMemoClick }
    With xDbfPtr^ do
      Begin
        If ReadMemoDBV(nSelectedFld) then
          ShowMessage(MakeStr(GetMemoBufferPtr(nSelectedFld) , GetMemoBytesRead))
        Else
          ShowMessage('No DBV memo read') ;
      End ;
  End ;  { TfmRecordDisplay.pmnDBVMemoClick }


{***********************************************************************
*                                                                      *
*       TfmRecordDisplay.pmnDBVParsClick                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.pmnDBVParsClick(Sender : TObject) ;
  Begin  { TfmRecordDisplay.pmnDBVParsClick }
    With xDbfPtr^ do
      Begin
        If ReadMemoHeaderDBV(nSelectedFld) then
          With GetDataAreaPtr^ , dbfMemoDescDBV do
            ShowMessage('Size = '        + IntToStr(Size)      + '   ' +
                        'Signature = '   + IntToStr(Signature) + '   ' +
                        'Compression = ' + IntToStr(Compression)       +
                        CRLF + CRLF +
                        MakeHexStr(@dbfMemoDescDBV , SizeOf(dbfMemoDescDBV))) 
        Else
          ShowMessage('DBV memo header NOT read') ;
      End ;
  End ;  { TfmRecordDisplay.pmnDBVParsClick }


{***********************************************************************
*                                                                      *
*       TfrmRecordDisplay.rbtDecimalClick                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.rbtDecimalClick(Sender : TObject) ;
  Begin  { TfrmRecordDisplay.rbtDecimalClick }
    With xDbfPtr^ , GetOptionsPtr^ do
      Begin
        If rbtDecimal.Checked then
          dbvShowFieldOffset := True
        Else
          If rbtHex.Checked then
            dbvShowFieldOffset := False
          Else
            dbvShowFieldOffset := True ;

        dbvShowFieldHex := not dbvShowFieldOffset ;
      End ;

    ednRecordNoExit(Sender) ;
  End ;  { TfrmRecordDisplay.rbtDecimalClick }

{***********************************************************************
*                                                                      *
*       TfrmRecordDisplay.pmnDateClickClick                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRecordDisplay.pmnDateClickClick(Sender: TObject);
  Var
    dDate : TDateTime ;

  Begin  { TfrmRecordDisplay.pmnDateClickClick }
    With xDbfPtr^ do
      If GetFieldType(nSelectedFld) = 'D' then
        Begin
          If dbfIsBlankDate(GetRecordPtr , nSelectedFld) then
            ShowMessage('Date field is blank.')
          Else
            Begin
              dDate := GetFldDate(nSelectedFld) ;
              ShowMessage(DateToStr(dDate)) ;
            End ;
        End
      Else
        ShowMessage('Not a date type field.') ;
  End ;  { TfrmRecordDisplay.pmnDateClickClick }

End.
