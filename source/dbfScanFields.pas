{***********************************************************************
*                                                                      *
*       dbScanFields.pas                                               *
*                                                                      *
*       (C) Copyright 1982-2006 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfScanFields ;

Interface

uses
  Windows , Messages , SysUtils , Classes  , Graphics , Controls ,
  Forms   , Dialogs  , StdCtrls , ExtCtrls ,

  JvExStdCtrls ,
  JvListBox    ,

  bcClasses        ,
  bcNewList        ,
  bcPanelSplitters ,
  bcNumericEdit    ,

  dbfCommon    ,
  dbfStructure  ;

Type
  TfrmScanFields =
    Class(TForm)
    pnlMain: TPanel;
    pnlButtons: TPanel;
    PanelSplitterBottom1: TPanelSplitterBottom;
    pnlTop: TPanel;
    btnScan: TButton;
    pnlEntry: TPanel;
    PanelSplitterTop1: TPanelSplitterTop;
    pnlCentre: TPanel;
    ednRecord: TFnpNumericEdit;
    tlbStats: TNewTextListBox;
    procedure btnScanClick(Sender: TObject);
      Private
        oOwner : TComponent ;

        xDbfPtr : pTxBase ;

      Public
        Constructor Create(aOwner : TComponent ;
                           xPtr   : pTxBase     ) ; reintroduce ;
        Destructor Destroy ; override ;
      End ;

Implementation
  Uses
    bcStringUtilities ,
    
    dbfScanFieldsMaxMin ;

  Var
    oSelf : TfrmScanFields ;
    oButton : TMsgDlgBtn ;
// (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mnNoToAll, mbYesToAll, mbHelp);


{$R *.DFM}

{***********************************************************************
*                                                                      *
*       RecordScan                                                     *
*                                                                      *
*         Type                                                         *
*           TOnRecordScan = Procedure(const RecNo : Integer  ) ;       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure RecordScan(const RecNo : Integer) ;
  Begin  { RecordScan }
    If oSelf <> nil then
      Begin
        oSelf.ednRecord.SetInteger(RecNo) ;
        oButton := mbOk ;
      End
    Else
      oButton := TMsgDlgBtn(MessageDlg('Invalid display window.', mtError, [mbOK, mbCancel], 0));
  End ;  { RecordScan }

  
{***********************************************************************
*                                                                      *
*       TfrmScanFields.Create                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Constructor TfrmScanFields.Create(aOwner : TComponent ;
                                  xPtr   : pTxBase     ) ;
  Begin  { TfrmScanFields.Create }
    CheckDatabasePtr(xPtr) ;

    Inherited Create(oOwner) ;

    xDbfPtr := xPtr   ;
    oOwner  := aOwner ;
    oSelf   := Self   ;
    oButton := mbOk   ;
  End ;  { TfrmScanFields.Create }


{***********************************************************************
*                                                                      *
*       TfrmScanFields.Destroy                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Destructor TfrmScanFields.Destroy ;
  Begin  { TfrmScanFields.Destroy }
    Inherited Destroy ;
  End ;  { TfrmScanFields.Destroy }

  
{***********************************************************************
*                                                                      *
*       TfrmScanFields.btnScanClick                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmScanFields.btnScanClick(Sender: TObject);
  Var
    nField  : Integer ;
    cStr    : String  ;
    cStrVal : String  ;

  Begin  { TfrmScanFields.btnScanClick }
    ScanFields(xDbfPtr , RecordScan , (oButton = mbOK)) ;

    With xDbfPtr^ do
      For nField := 1 to GetFieldCount do
        Begin
          cStr := '' ;

          Case LoRec[nField].vFldType of
            oBoolean :
              Begin
                If LoRec[nField].bBoolean then
                  cStr := PadL('T' , 12)
                Else
                  cStr := PadL('F' , 12) ;

                If HiRec[nField].bBoolean then
                  cStr := cStr + PadL('T' , 13)
                Else
                  cStr := cStr + PadL('F' , 13) ;
              End ;

            oCurrency :
              Begin
                Str(LoRec[nField].nCurrency : 12 : 2 , cStr) ;
                Str(HiRec[nField].nCurrency : 12 : 2 , cStrVal) ;
                cStr := cStr + '  ' + cStrVal ;
              End ;

            oDateTime : ;
            oDouble   : ;

            oInteger :
              Begin
                cStr := PadL(IntToStr(LoRec[nField].nInteger) , 12) + ' ' +
                        PadL(IntToStr(HiRec[nField].nInteger) , 12) ;
              End ;

            oPicture : ;

            oString :
              Begin
                cStr    := LoRec[nField].cString ;
                cStrVal := HiRec[nField].cString ;
              End ;
          End ;

          cStr := Pad(GetFieldName(nField) , 11) + cStr ;
          tlbStats.Items.Add(cStr) ;
          If LoRec[nField].vFldType = oString then
            Begin
              cStr := Pad('' , 11) + cStrVal ;
              tlbStats.Items.Add(cStr) ;
            End ;
        End ;
  End ;  { TfrmScanFields.btnScanClick }

Initialization
  oSelf := nil ;
End.
