{***********************************************************************
*                                                                      *
*       dbfRestructure.pas                                             *
*                                                                      *
*       (C) Copyright 1990-2001 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfRestructure ;

Interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls , Buttons , StdCtrls , Mask ,

  JvEdit       ,
  JvExMask     ,
  JvExStdCtrls ,
  JvListBox    ,
  JvMaskEdit   ,
  JvSpin       ,

  bcClasses         ,
  bcDialogUtilities ,
  bcNewList         ,
  bcNumericEdit     ,
  bcPanelSplitters  ,
  bcStringUtilities ,

  dbfRestructureDBF ,
  dbfStructure        ;

type
  TfrmRestructure = class(TForm)
    pnlMain: TPanel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    PanelSplitterTop1: TPanelSplitterTop;
    PanelSplitterBottom1: TPanelSplitterBottom;
    pnlMiddle: TPanel;
    GroupBox1: TGroupBox;
    pnlLeftRight: TPanel;
    GroupBox2: TGroupBox;
    tlbFieldList: TNewTextListBox;
    btnRestructure: TBitBtn;
    pnlFieldTotals: TPanel;
    ednNewFieldCount: TFnpNumericEdit;
    Label1: TLabel;
    ednNewRecordSize: TFnpNumericEdit;
    Label5: TLabel;
    Panel1: TPanel;
    tlbFields: TNewTextListBox;
    pnlLeftRightTop: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblDecimal: TLabel;
    lblNewName: TLabel;
    lblNewFieldType: TLabel;
    lblNewWidth: TLabel;
    lblNewDecimals: TLabel;
    lblDelete: TLabel;
    ednFieldNo: TFnpNumericEdit;
    edtName: TJvEdit;
    edtType: TJvEdit;
    edtTypeDesc: TJvEdit;
    ednWidth: TFnpNumericEdit;
    ednDecimals: TFnpNumericEdit;
    edtNewName: TJvEdit;
    cbxFieldType: TComboBox;
    ckbDelete: TCheckBox;
    btnAddField: TBitBtn;
    btnPostField: TBitBtn;
    pnlMiddleBottom: TPanel;
    PanelSplitterBottom2: TPanelSplitterBottom;
    btnAddRSField: TBitBtn;
    speNewWidth: TJvSpinEdit;
    speNewDecimals: TJvSpinEdit;
//    speNewWidth: TJvSpinEdit;
//    speNewDecimals: TJvSpinEdit;
    procedure tlbFieldsClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnPostFieldClick(Sender: TObject);
    procedure btnRestructureClick(Sender: TObject);
    procedure ckbDeleteClick(Sender: TObject);
    procedure btnAddFieldClick(Sender: TObject);
    procedure edtNewNameExit(Sender: TObject);

  Protected
    oOwner : TComponent ;

  Private
    xDbfPtr : pTxBase  ;

    nDecimalTop : Integer ;

    Function  GetNewRecordSize : Integer ;
    Function  GetNewFieldCount : Integer ;
    Procedure ReNewCalcFieldTotals ;
    Function  CheckNewFieldName(cName : String) : Boolean ;

  Public
    Constructor Create(aOwner : TComponent ;
                       xPtr   : pTxBase     ) ; reintroduce ;
    Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfrmRestructure.Create                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmRestructure.Create(aOwner : TComponent ;
                                   xPtr   : pTxBase   ) ;
  Begin  { TfrmRestructure.Create }
    If xPtr = nil then
      Raise Exception.Create('Database pointer is nil.') ;

    Inherited Create(oOwner) ;

    oOwner  := aOwner ;
    xDbfPtr := xPtr   ;
  End ;  { TfrmRestructure.Create }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.Destroy                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmRestructure.Destroy ;
  Begin  { TfrmRestructure.Destroy }
    Inherited Destroy ;
  End ;  { TfrmRestructure.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.tlbFieldsClick                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructure.tlbFieldsClick(Sender : TObject) ;
  Var
    cStr        : String  ;
    nDecimals ,
    nField      : Integer ;
    bShowIt     : Boolean ;
    bShowNew    : Boolean ;
    bShowItNew  : Boolean ;
    bDisable    : Boolean ;

  Begin  { TfrmRestructure.tlbFieldsClick }
    With tlbFields do
      Begin
        If (ItemIndex < 0) or (ItemIndex > Items.Count) then
          ItemIndex := 0 ;
        nField := ItemIndex ;
      End ;

    bShowNew := (nField <> 0) ;
    ckbDelete.Visible    := bShowNew ;
    ckbDelete.Enabled    := bShowNew ;
    lblDelete.Visible    := bShowNew ;
    lblDelete.Enabled    := bShowNew ;
    btnPostField.Visible := bShowNew ;
    btnPostField.Enabled := bShowNew ;

    If ckbDelete.Enabled then
      Begin
        ckbDelete.Checked :=
          (Copy(tlbFieldList.Items[nField] , 11 , 1) = '*') ;
      End ;

    bDisable := (not ckbDelete.Checked) ;
    cbxFieldType.Enabled   := bDisable ;
    speNewWidth.Enabled    := bDisable ;
    speNewDecimals.Enabled := bDisable ;

    With xDbfPtr^ do
      Begin
        edtName.Text         := GetFieldName(nField)     ;
        ednFieldNo.AsInteger := nField                   ;
        edtType.Text         := GetFieldType(nField)     ;
        edtTypeDesc.Text     := GetFieldTypeName(nField) ;
        ednWidth.AsInteger   := GetFieldWidth(nField)    ;

        nDecimals := GetFieldDecimals(nField) ;
        If nDecimals > 0 Then
          Begin
            ednDecimals.AsInteger := nDecimals ;
            bShowIt := True ;
          End
        Else
          If (GetFieldType(nField) In DecimalFieldTypes) Then
            Begin
              ednDecimals.AsInteger := 0 ;
              bShowIt := True ;
            End
          Else
            bShowIt := false ;

        bShowItNew := (bShowIt and bShowNew) ;

        speNewWidth.AsInteger    := ednWidth.AsInteger    ;
        speNewDecimals.AsInteger := ednDecimals.AsInteger ;
        speNewDecimals.MinValue  := 0 ;
        speNewDecimals.MaxValue  := speNewWidth.AsInteger - 1 ;

        cStr := edtType.Text ;
        cbxFieldType.ItemIndex := PosStr(cStr , LegalFieldTypes) - 1 ;
        cbxFieldType.Repaint ;

        cbxFieldType.Enabled := bShowNew and bDisable ;
        cbxFieldType.Visible := bShowNew ;

        { Don't show the Decimals if the field type }
        { doesn't have it.                          }
        ednDecimals.Visible := bShowIt ;
        lblDecimal.Visible  := bShowIt ;

        { Do not allow Delete field to be modified. }
        speNewWidth.Visible := bShowNew ;
        lblNewWidth.Visible := bShowNew ;

        speNewDecimals.Visible  := bShowItNew ;
        lblNewDecimals.Visible  := bShowItNew ;
        lblNewFieldType.Visible := bShowItNew ;
      End ;

    ReNewCalcFieldTotals ;
  End ;  { TfrmRestructure.tlbFieldsClick }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.ReNewCalcFieldTotals                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructure.ReNewCalcFieldTotals ;
  Begin  { TfrmRestructure.ReNewCalcFieldTotals }
    ednNewRecordSize.AsInteger := GetNewRecordSize ;
    ednNewFieldCount.AsInteger := GetNewFieldCount ;

    ednNewRecordSize.Repaint ;
    ednNewFieldCount.Repaint ;
  End ;  { TfrmRestructure.ReNewCalcFieldTotals }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.FormActivate                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructure.FormActivate(Sender : TObject) ;
  Var
    nField : Integer ;

  Begin  { TfrmRestructure.FormActivate }
    nDecimalTop := ednDecimals.Top ;

    tlbFields.Clear ;
    tlbFieldList.Clear ;

    With xDbfPtr^ , GetDataAreaPtr^ do
      For nField := 0 To GetFieldCount do
        tlbFields.Items.Add(IntToPadStr(nField , 3) + '  ' +
                            GetFieldName(nField)            ) ;

    tlbFieldList.Items.Clear ;
    tlbFieldList.Items.Assign(xDbfPtr^.GetRestructureListPtr^) ;

    { Make the first field current and then emulate the }
    { list box being clicked to display the field info. }
    tlbFields.ItemIndex := 1 ;
    tlbFieldsClick(Sender) ;
  End ;  { TfrmRestructure.FormActivate }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.btnPostFieldClick                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructure.btnPostFieldClick(Sender : TObject) ;
  Var
    cType : char ;

  Begin  { TfrmRestructure.btnPostFieldClick }
    { Name      1 10 }
    { Type     12  1 }
    { Width    14  3 }
    { Decimals 18  2 }
    If ckbDelete.Checked then
      Begin
        tlbFieldsClick(Sender)    ;
        ckbDelete.Checked := True ;

        tlbFieldList.Items[ednFieldNo.AsInteger] :=
           Copy(tlbFieldList.Items[ednFieldNo.AsInteger] , 1 , 10) +
           '*' +
           Copy(tlbFieldList.Items[ednFieldNo.AsInteger] , 12 , 8) ;
      End
    Else
      Begin
        cType := GetFieldTypeChar(GetFieldTypeIdx(cbxFieldType.Text)) ;
        tlbFieldList.Items[ednFieldNo.AsInteger] :=
            Copy(tlbFieldList.Items[ednFieldNo.AsInteger] , 1 , 10) + ' ' +
            cType                                                   + ' ' +
            PadL(IntToStr(speNewWidth.AsInteger)    , 3)            + ' ' +
            PadL(IntToStrBlank(speNewDecimals.AsInteger) , 2)               ;
      End ;

    tlbFieldList.Repaint ;
    ReNewCalcFieldTotals ;
  End ;  { TfrmRestructure.btnPostFieldClick }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.btnRestructureClick                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructure.btnRestructureClick(Sender : TObject) ;
  Begin  { TfrmRestructure.btnRestructureClick }
    With TfrmRestructureDBF.Create(oOwner              ,
                                   xDbfPtr             ,
                                   @tlbFieldList.Items  ) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmRestructure.btnRestructureClick }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.ckbDeleteClick                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructure.ckbDeleteClick(Sender : TObject) ;
  Var
    nField : Integer ;

  Begin  { TfrmRestructure.ckbDeleteClick }
    nField := ednFieldNo.AsInteger ;
    If nField = 0 then
      Begin
        ckbDelete.Checked := False ;
        ckbDelete.Visible := False ;
        ckbDelete.Enabled := False ;
        ShowErrorMessage('Cannot remove DELETE field.') ;
      End
    Else
      Begin
        cbxFieldType.Enabled   := (not ckbDelete.Checked) ;
        speNewWidth.Enabled    := (not ckbDelete.Checked) ;
        speNewDecimals.Enabled := (not ckbDelete.Checked) ;
      End ;
  End ;  { TfrmRestructure.ckbDeleteClick }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.GetNewRecordSize                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TfrmRestructure.GetNewRecordSize : Integer ;
  Var
    nField : Integer ;

  Begin  { TfrmRestructure.GetNewRecordSize }
    Result := 0 ;
    For nField := 0 to (tlbFieldList.Items.Count - 1) do
      If (Copy(tlbFieldList.Items[nField] , 11 , 1) = ' ') then
        Result := Result + StrToInt(Trim(Copy(tlbFieldList.Items[nField] , 14 , 3))) ;
  End ;  { TfrmRestructure.GetNewRecordSize }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.GetNewFieldCount                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TfrmRestructure.GetNewFieldCount : Integer ;
  Var
    nField : Integer ;

  Begin  { TfrmRestructure.GetNewFieldCount }
    Result := 0 ;
    For nField := 0 to (tlbFieldList.Items.Count - 1) do
      If (Copy(tlbFieldList.Items[nField] , 11 , 1) = ' ') then
        Result := Result + 1 ;
  End ;  { TfrmRestructure.GetNewFieldCount }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.btnAddFieldClick                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructure.btnAddFieldClick(Sender : TObject) ;
  Begin  { TfrmRestructure.btnAddFieldClick }
    edtNewName.Enabled := True ;
    ActiveControl := edtNewName ;

    cbxFieldType.Text        := 'Character' ;
    speNewWidth.AsInteger    := 12          ;
    speNewDecimals.AsInteger := 0           ;
  End ;  { TfrmRestructure.btnAddFieldClick }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.edtNewNameExit                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmRestructure.edtNewNameExit(Sender : TObject) ;
  Var
    nFieldNo  : Integer ;
    cName     : String  ;
    cType     : Char    ;
    nWidth    : Integer ;
    nDecimals : Integer ;

  Begin  { TfrmRestructure.edtNewNameExit }
    nFieldNo        := tlbFieldList.Items.Count ;
    cName           := edtNewName.Text          ;
    cName           := AnsiUpperCase(Trim(cName))   ;
    cType           := GetFieldTypeCharFromName(cbxFieldType.Text) ;
    nWidth          := speNewWidth.AsInteger    ;
    nDecimals       := speNewDecimals.AsInteger ;

    edtNewName.Text := cName ;

    If CheckNewFieldName(cName) then
      Begin
        tlbFieldList.Items.Add(PadR(cName , 10)                  + ' ' +
                               cType                             + ' ' +
                               PadL(IntToStr(nWidth) , 3)        + ' ' +
                               PadL(IntToStrBlank(nDecimals) , 2)       ) ;
        ednFieldNo.AsInteger  := nFieldNo  ;
        edtName.Text          := cName     ;
        ednWidth.AsInteger    := nWidth    ;
        ednDecimals.AsInteger := nDecimals ;
        edtType.Text          := cType     ;
      End
    Else
      ShowErrorMessage('Unable to add duplicate/null field.') ;

    ActiveControl := btnPostField ;
    edtNewName.Enabled := False ;
  End ;  { TfrmRestructure.edtNewNameExit }


{***********************************************************************
*                                                                      *
*       TfrmRestructure.CheckNewFieldName                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TfrmRestructure.CheckNewFieldName(cName : String) : Boolean ;
  Var
    nField : Integer ;

  Begin
    If Length(cName) <= 0 then
      Begin
        Result := False ;
        Exit ;
      End ;
    Result := True ;

    For nField := 0 to (tlbFieldList.Items.Count - 1) do
      Begin
        cName := AnsiUpperCase(Copy(tlbFieldList.Items[nField] , 1 , dbfMaxNameLen)) ;
        If AnsiUpperCase(cName) = cName then
          Begin
            Break ;
            Result := False ;
          End ;
      End ;
  End ;

End.


