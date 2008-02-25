{***********************************************************************
*                                                                      *
*       dbfRepairRecords.pas                                           *
*                                                                      *
*       (C) Copyright 1990-2003 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfRepairRecords ;

Interface

Uses
  Windows , SysUtils , Classes    , Graphics , Forms , Controls , StdCtrls ,
  Buttons , ExtCtrls ,

  JvExStdCtrls ,
  JvListBox    ,

  bcDialogUtilities ,
  bcNewList         ,
  bcPanelSplitters  ,
  bcStringUtilities ,

  dbfStructure ;

Type
  TfrmRepairRecords = class(TForm)
    pnlMain: TPanel;
    pnlButtons: TPanel;
    PanelSplitterBottom1: TPanelSplitterBottom;
    pnlTop: TPanel;
    pnlLeft: TPanel;
    pnlSplitterMiddle: TPanel;
    pnlRight: TPanel;
    tlbMessages: TNewTextListBox;
    Panel1: TPanel;
    PanelSplitterTop1: TPanelSplitterTop;
    btnRepair: TBitBtn;
    ckbCharacterNulls: TCheckBox;
    ckbInvalidBoolean: TCheckBox;
    GroupBox1: TGroupBox;
    rbtTrue: TRadioButton;
    rbtFalse: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnRepairClick(Sender: TObject);

  Protected
    oOwner  : TComponent ;
    xDbfPtr : pTxBase  ;

  Private

  Public
    Constructor Create(aOwner : TComponent ;
                       xPtr   : pTxBase   ) ; Reintroduce ;
    Destructor Destroy ; Reintroduce ;
  End ;

implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfrmRepairRecords.Create                                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmRepairRecords.Create(aOwner : TComponent ;
                                     xPtr   : pTxBase   ) ;
  Begin  { TfrmRepairRecords.Create }
    If xPtr = nil then
      Raise Exception.Create('Database pointer is nil.') ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr ;
  End ;  { TfrmRepairRecords.Create }


{***********************************************************************
*                                                                      *
*       TfrmRepairRecords.Destroy                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmRepairRecords.Destroy ;
  Begin  { TfrmRepairRecords.Destroy }
    Inherited Destroy ;
  End ;  { TfrmRepairRecords.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmRepairRecords.FormCreate                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairRecords.FormCreate(Sender: TObject);
  Begin  { TfrmRepairRecords.FormCreate }
    FormResize(Sender) ;
  End ;  { TfrmRepairRecords.FormCreate }


{***********************************************************************
*                                                                      *
*       TfrmRepairRecords.FormCreate                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairRecords.FormResize(Sender: TObject);
  Var
    nWidth : Integer ;

  Begin  { TfrmRepairRecords.FormResize }
    nWidth := (pnlTop.Width - pnlSplitterMiddle.Width) div 2 ;
    pnlLeft.Width := nWidth ;

    tlbMessages.Clear ;
  End ;  { TfrmRepairRecords.FormResize }


{***********************************************************************
*                                                                      *
*       TfrmRepairRecords.btnRepairClick                               *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmRepairRecords.btnRepairClick(Sender : TObject) ;
  Var
    nRecord  : Integer ;
    nField   : Integer ;
    pFld     : pAsciiZ ;
    cType    : Char    ;
    nWidth   : Integer ;
    cLogical : String  ;

    nI : Integer ;

    bFlag    : Boolean ;
    bChanged : Boolean ;

  Begin  { TfrmRepairRecords.btnRepairClick }
    tlbMessages.Items.Clear ;

    With xDbfPtr^ do
      Begin
        For nRecord := 1 to GetTotalRecords do
          Begin
            GetRecord(nRecord) ;
            For nField := 0 to GetFieldCount do
              Begin
                pFld   := GetCurrentFieldPtr(nField) ;
                cType  := GetFieldType(nField)       ;
                nWidth := GetFieldWidth(nField)      ;

                Case cType of
                  'C' : Begin
                          If ckbCharacterNulls.Checked then
                            Begin
                              bFlag    := False ;
                              bChanged := False ;

                              For nI := 0 to (nWidth - 1) do
                                Begin
                                  If not bFlag then
                                    bFlag := (pFld^[nI] = #00) ;

                                  If bFlag then
                                    Begin
                                      pFld^[nI] := ' ' ;
                                      bChanged := True ;
                                    End ;
                                End ;

                              If bChanged then
                                Begin
                                  If PutFieldRecordPtr(pFld    ,
                                                       nRecord ,
                                                       nField   ) then
                                    tlbMessages.Items.Add('Record [' + IntToStr(nRecord) +
                                                          '] Field [' + IntToStr(nField) +
                                                          '] blanked after null.')
                                  Else
                                    tlbMessages.Items.Add('Field [' + IntToStr(nField)  +
                                                          '] Rec [' + IntToStr(nRecord) +
                                                          '] not written.'                ) ;
                                End ;
                            End ;
                        End ;

                  'L' : Begin
                          If ckbInvalidBoolean.Checked then
                            Begin
                              bChanged := False ;

                              bFlag := (not (pFld^[0] in dbfValidLogical)) ;

                              If bFlag then
                                Begin
                                  If rbtTrue.Checked then
                                    Begin
                                      pFld^[0] := 'T'    ;
                                      cLogical  := 'True' ;
                                    End
                                  Else
                                    If rbtFalse.Checked then
                                      Begin
                                        pFld^[0] := 'F'     ;
                                        cLogical  := 'False' ;
                                      End
                                    Else
                                      Raise Exception.Create('Invalid True/False checkbox value.') ;

                                  bChanged := True ;
                                End ;

                              If bChanged then
                                Begin
                                  If PutFieldRecordPtr(pFld    ,
                                                       nRecord ,
                                                       nField   ) then
                                    tlbMessages.Items.Add('Record [' + IntToStr(nRecord) +
                                                          '] Field [' + IntToStr(nField) +
                                                          '] set to ' + cLogical + '.')
                                  Else
                                    tlbMessages.Items.Add('Field [' + IntToStr(nField)  +
                                                          '] Rec [' + IntToStr(nRecord) +
                                                          '] not written.'                ) ;
                                End ;
                            End ;
                        End ;
                End ;
              End ;
          End ;
      End ;
  End ;  { TfrmRepairRecords.btnRepairClick }

End.
