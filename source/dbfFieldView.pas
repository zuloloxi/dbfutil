{***********************************************************************
*                                                                      *
*       dbfFieldView.pas                                               *
*                                                                      *
*       (C) Copyright 1982-1999 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfFieldView ;

Interface

Uses
  Windows  , Messages , SysUtils , Classes  , Graphics ,
  Controls , Forms    , Dialogs  , StdCtrls ,

  JvEdit       ,
  JvExStdCtrls ,
  JvListBox    ,

  bcClasses     ,
  bcStringUtilities ,
  bcNewList     ,
  bcNumericEdit ,

  dbfCommon    ,
  dbfStructure   ;

Type
  TfmViewField = Class(TForm)
    GroupBox1: TGroupBox;
    lblName: TLabel;
    lblType: TLabel;
    lblSize: TLabel;
    ListBox1: TListBox;
    lblDecimals: TLabel;
    lblOffset: TLabel;

    FldNoBox        : TFnpNumericEdit ;
    NameBox         : TJvEdit         ;
    sleType         : TJvEdit         ;
    edFieldTypeDesc : TJvEdit         ;
    SizeBox         : TFnpNumericEdit ;
    enDecimals      : TFnpNumericEdit ;
    enOffset        : TFnpNumericEdit ;

    sleWorkAreaId    : TJvEdit ;
    sleHexIndexed    : TJvEdit ;
    sleIndexed       : TJvEdit ;
    sleSetFieldsFlag : TJvEdit ;
    sleColumnFlags   : TJvEdit ;
    sleReserved01    : TJvEdit ;
    sleReserved02    : TJvEdit ;
    sleUnknown       : TJvEdit ;

    tlbFieldDesc : TNewTextListBox ;
    grpbxFieldDescriptionEntry: TGroupBox;

    lblWorkAreaID        : TLabel ;
    lblIndexed           : TLabel ;
    lblSetFields         : TLabel ;
    lblColumnFlags       : TLabel ;
    lblReserveMultiUser1 : TLabel ;
    lblReserveMultiUser2 : TLabel ;
    lblReservedUnknown   : TLabel ;

    Procedure FormActivate(Sender : TObject) ;
    Procedure ListBox1Click(Sender : TObject) ;

  Protected
    oOwner : TComponent ;

  Private  { Private declarations }
    xDbfPtr    : pTxBase ;

    nDecimalTop : Integer ;
    nOffsetTop  : Integer ;

  Public   { Public declarations }
    Constructor Create(AOwner : TComponent ;
                       xPtr   : pTxBase     ) ; ReIntroduce ;
    Destructor Destroy ; override ;
  End;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TRecordDisplayForm.Create                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfmViewField.Create(AOwner : TComponent ;
                                xPtr   : pTxBase     ) ;
  Begin  { TfmViewField.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := AOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr ;
  End ;  { TfmViewField.Create }


{***********************************************************************
*                                                                      *
*       TfmViewField.Destroy                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfmViewField.Destroy ;
  Begin  { TfmViewField.Destroy }
    Inherited Destroy ;
  End ;  { TfmViewField.Destroy }


{***********************************************************************
*                                                                      *
*       TfmViewField.FormActivate                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmViewField.FormActivate(Sender : TObject) ;
  Var
    nI : Integer ;

  Begin  { TfmViewField.FormActivate }
    nDecimalTop := enDecimals.Top ;
    nOffsetTop  := enOffset.Top   ;

    ListBox1.Clear ;
    With xDbfPtr^ do
      For nI := 0 To GetFieldCount Do
        ListBox1.Items.Add(GetFieldName(nI)) ;

    { Make the first field current and then emulate the }
    { list box being clicked to display the field info. }
    ListBox1.ItemIndex := 1 ;
    ListBox1Click(Sender) ;
  End ;  { TfmViewField.FormActivate }


{***********************************************************************
*                                                                      *
*       TfmViewField.ListBox1Click                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmViewField.ListBox1Click(Sender: TObject);
  Var
    nI        ,
    nDecimals ,
    nField      : Integer ;
    cChar       : Char    ;
    cStr        : String  ;
    bShowIt     : Boolean ;
    aFldRec     : Array[1..SizeOf(dbfFieldDescType)] of Byte ;

  Begin  { TfmViewField.ListBox1Click }
    nField := 0 ;

    With ListBox1 Do
      If ItemIndex > -1 Then
        Begin
          nField := ItemIndex;
          NameBox.Text := Items[ItemIndex] ;
        End
      Else
        ItemIndex := 0;

    tlbFieldDesc.Clear ;

    With xDbfPtr^ do
      Begin
        SetFieldChoice(nField) ;
        FldNoBox.AsInteger := nField ;
        cChar := GetFieldType(nField) ;
        sleType.Text := cChar ;
        SizeBox.AsInteger := GetFieldWidth(nField) ;
        edFieldTypeDesc.Text := GetFieldTypeName(nField) ;

        nDecimals := GetFieldDecimals(nField) ;
        If nDecimals > 0 Then
          Begin
            enDecimals.AsInteger := nDecimals ;
            bShowIt := True ;
          End
        Else
          If (cChar In DecimalFieldTypes) Then
            Begin
              enDecimals.AsInteger := 0 ;
              bShowIt := True ;
            End
          Else
            bShowIt := False ;

        enOffset.AsInteger := GetFieldOffset(nField) ;

        { Don't show the Decimals if the field type }
        { doesn't have it.                          }
        enDecimals.Visible  := bShowIt ;
        lblDecimals.Visible := bShowIt ;

        If bShowIt then
          Begin
            enOffset.Top  := nOffsetTop ;
            lblOffset.Top := nOffsetTop ;
          End
        Else
          Begin
            enOffset.Top  := nDecimalTop ;
            lblOffset.Top := nDecimalTop ;
          End;

        bShowIt := (nField > 0) ;

        lblWorkAreaID.Visible        := bShowIt ;
        lblIndexed.Visible           := bShowIt ;
        lblSetFields.Visible         := bShowIt ;
        lblColumnFlags.Visible       := bShowIt ;
        lblReserveMultiUser1.Visible := bShowIt ;
        lblReserveMultiUser2.Visible := bShowIt ;
        lblReservedUnknown.Visible   := bShowIt ;

        grpbxFieldDescriptionEntry.Visible := bShowIt ;

        sleWorkAreaId.Visible    := bShowIt ;
        sleHexIndexed.Visible    := bShowIt ;
        sleIndexed.Visible       := bShowIt ;
        sleSetFieldsFlag.Visible := bShowIt ;
        sleColumnFlags.Visible   := bShowIt ;
        sleReserved01.Visible    := bShowIt ;
        sleReserved02.Visible    := bShowIt ;
        sleUnknown.Visible       := bShowIt ;
        tlbFieldDesc.Visible     := bShowIt ;

        If bShowIt then
          Begin
            sleWorkAreaId.Text := '$' + HexByte(GetWorkAreaID(nField)) ;

            sleIndexed.Text       := BooleanYesNo(GetFieldIndexed(nField))      ;
            sleHexIndexed.Text    := '$' + HexByte(GetFieldIndexedByte(nField)) ;
            sleSetFieldsFlag.Text := '$' + HexByte(GetSetFieldsFlag(nField))    ;
            sleColumnFlags.Text   := '$' + HexByte(GetColumnFlags(nField))      ;

            sleReserved01.Text := GetReservedMultiUserStr01(nField) ;
            sleReserved02.Text := GetReservedMultiUserStr02(nField) ;
            sleUnknown.Text    := GetReservedUnknown(nField)        ;

            If (nField > 0) and ValidFieldNumber(nField) then
              Begin
                Move(GetFieldDescPtr(nField)^ , aFldRec , SizeOf(aFldRec)) ;

                cStr := '00: ' ;
                For nI := 1 to 8 do
                  cStr := cStr + HexByte(aFldRec[nI]) + ' ' ;
                tlbFieldDesc.Items.Add(Trim(cStr)) ;

                cStr := '08: ' ;
                For nI := 9 to 16 do
                  cStr := cStr + HexByte(aFldRec[nI]) + ' ' ;
                tlbFieldDesc.Items.Add(Trim(cStr)) ;

                cStr := '10: ' ;
                For nI := 17 to 24 do
                  cStr := cStr + HexByte(aFldRec[nI]) + ' ' ;
                tlbFieldDesc.Items.Add(Trim(cStr)) ;

                cStr := '18: ' ;
                For nI := 25 to SizeOf(dbfFieldDescType) do
                  cStr := cStr + HexByte(aFldRec[nI]) + ' ' ;
                tlbFieldDesc.Items.Add(Trim(cStr)) ;
              End
            Else
              Begin
                tlbFieldDesc.Clear ;
                If nField = 0 then
                  tlbFieldDesc.Items.Add('< DELETE byte field >') ;
              End ;
          End ;
      End ;
  End ;  { TfmViewField.ListBox1Click }

End.
