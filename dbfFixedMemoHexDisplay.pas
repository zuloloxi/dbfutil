{***********************************************************************
*                                                                      *
*       dbfFixedMemoDisplay.pas                                        *
*                                                                      *
*       (C) Copyright 1982-2001 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfFixedMemoHexDisplay ;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,

  bcMemo ,
  bcStringUtilities ,
  bcMathUtilities ,

  Menus, ExtCtrls,

  ClipBrd       ,
  Buttons       ,

  JvExStdCtrls ,
  JvMemo       ,

  bcClasses         ,
  bcDialogUtilities ,
  bcHeapView        ,
  bcNumericEdit     ,
  bcScreenUtilities ,

  dbfCommon    ,
  dbfStructure   ;

Type
  TfrmFixedMemoHex = Class(TForm)
    FontDialog1: TFontDialog;
    MainMenu1: TMainMenu;
    View1: TMenuItem;
    HexView1: TMenuItem;
    Font1: TMenuItem;
    TranslateCRLF1: TMenuItem;
    Edit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Clear1: TMenuItem;
    Undo1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Memo: TLabel;
    ednMemoNo: TFnpNumericEdit;
    Label1: TLabel;
    ednMemoSize: TFnpNumericEdit;
    Panel3: TPanel;
    Panel4: TPanel;
    ImageBMP: TImage;
    btnHexView: TBitBtn;
    Label2: TLabel;
    edtFieldType: TEdit;
    pnlRight: TPanel;
    MemoListBox: TListBox;
    pnlLeft: TPanel;
    ExtendedMemo: TExtendedMemo;
    btnTranslate: TBitBtn;

    Procedure FormActivate(Sender: TObject);
    Procedure MemoHexViewClick(Sender: TObject);
//    Procedure TranslateCRLF1Click(Sender : TObject) ;
    Procedure Font1Click(Sender: TObject);

    Procedure ShowMemoFixed ;

    Procedure FormCreate(Sender : TObject) ;
    Procedure Cut1Click(Sender : TObject) ;
    Procedure Copy1Click(Sender : TObject) ;
    Procedure Paste1Click(Sender : TObject) ;
    Procedure Clear1Click(Sender : TObject) ;
    Procedure Undo1Click(Sender : TObject) ;
    procedure MemoListBoxClick(Sender: TObject);

  Protected
    oOwner : TComponent ;

  Private                               { Private declarations }
    xDbfPtr    : pTxBase ;
    bFixedShow : Boolean   ;
    nRec       : Integer   ;
    nField     : Integer   ;
    nMemoNo    : Integer   ;

    Procedure mdShowMemo(bFixed : Boolean) ;

  Public
    Constructor Create(aOwner   : TComponent ;
                       xPtr     : pTxBase  ;
                       nFieldNo : Integer     ) ; reintroduce ;
    Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}

Uses
  bcMemoryUtilities ,

  dbfResources ;

{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.Create                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmFixedMemoHex.Create(aOwner   : TComponent ;
                                  xPtr     : pTxBase    ;
                                  nFieldNo : Integer     ) ;
  Begin  { TfrmFixedMemoHex.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr        ;
    nField  := nFieldNo    ;

    With xDbfPtr^ do
      Begin
        nRec  := GetRecordNo ;
        If GetFieldType(nField) in MemoFieldTypes then
          nMemoNo := GetMemoNumber(GetRecordPtr , nField)
        Else
          nMemoNo := 0 ;
      End ;
  End ;  { TfrmFixedMemoHex.Create }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.Destroy                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmFixedMemoHex.Destroy ;
  Begin  { TfrmFixedMemoHex.Destroy }
    Inherited Destroy ;
  End ;  { TfrmFixedMemoHex.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.FormActivate                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.FormActivate(Sender : TObject) ;
  Begin  { TfrmFixedMemoHex.FormActivate }
    ExtendedMemo.Clear ;

    With xDbfPtr^ do
      Begin
        Try
          ScanMemoFields ;
        Except
          On E : Exception do
            Begin
              ErrorMsg(rsErrScanMemoFieldsInProc ,
                       ['TfrmFixedMemoHex.FormActivate' , E.Message]) ;
              Exit ;
            End ;
        End ;

        With MemoListBox Do
          Begin
            Clear ;
            // SortMemoFields ;
            Items.Assign(GetMemoFieldsPtr^) ;
          End ;

        If bFixedShow then
          mdShowMemo(True) ;
      End ;
  End ;  { TfrmFixedMemoHex.FormActivate }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.mdShowMemo                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.mdShowMemo(bFixed : Boolean) ;
  Var
    pMemoPtr : Pointer ;

    cMemoStr  : String  ;
    nMemoSize : Integer ;
    cMemoType : String  ;
    cType     : Char    ;

  Begin  { TfrmFixedMemoHex.mdShowMemo }
    With xDbfPtr^ Do
      Begin
        If bFixed Then
          Begin
            cMemoType := GetFieldType(nField) ;
            nMemoNo   := ednMemoNo.AsInteger  ;
          End
        Else
          Begin
            ednMemoNo.AsInteger := StrToInt(MemoListBox.Items[MemoListBox.ItemIndex]) ;
            cMemoType := GetMemoFieldTypesPtr^.Strings[MemoListBox.ItemIndex] ;
            nMemoNo   := ednMemoNo.AsInteger ;
          End ;

        edtFieldType.Text := cMemoType ;
        cType := cMemoType[1] ;

        If (nMemoNo > 0) then
          Case cType of
            'M' : Begin
                    btnTranslate.Visible := True  ;
                    ImageBMP.Visible     := False ;
                    ExtendedMemo.Visible := True  ;

                    nMemoSize := ReadMemo(nMemoNo , nField) ;
                    ednMemoSize.AsInteger := nMemoSize ;

                    pMemoPtr := GetMemoBufferPtr(nField) ;
                    cMemoStr := MakeStrEOF(pMemoPtr , nMemoSize) ;

                    ExtendedMemo.Lines.Clear ;
                    ExtendedMemo.Text := cMemoStr ;
                    ExtendedMemo.GoToTop ;
                    Show ;
                  End ;

            'B' : Begin
                    ExtendedMemo.Visible := False ;
                    btnTranslate.Visible := False ;
                    ImageBMP.Visible := True;

                    ReadMemoBMP(nMemoNo) ;
                    ImageBMP.Picture.LoadFromFile(GetBlobFileName);
                  End ;

            'G' : Begin
                    ExtendedMemo.Visible := False ;
                    btnTranslate.Visible := False ;
                    ImageBMP.Visible     := True ;

                    ReadMemoObject(nMemoNo) ;
                    
                    ImageBMP.Picture.LoadFromFile(GetBlobFileName);
                  End ;
          End ;  { Case cType of }
      End ;
  End ;  { TfrmFixedMemoHex.mdShowMemo }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.MemoHexViewClick                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.MemoHexViewClick(Sender: TObject);
  Var
    pMemoPtr  : Pointer ;
    nMemoSize : Integer ;

  Begin  { TfrmFixedMemoHex.MemoHexViewClick }
    With xDbfPtr^ , GetDataAreaPtr^ do
      Begin
        nMemoSize := GetMemoBufferLen(nField) ;
        Inc(nMemoSize , SizeOf(dbfMemoDesc)) ;
        GetMemCheck(pMemoPtr , nMemoSize) ;
      End ;

    nMemoSize := MinInteger(nMemoSize , 2048) ;
    With TfmHeapView.Create(Application ,
                            pMemoPtr    ,
                            nMemoSize   ,
                            True         ) do
      Try
        With xDbfPtr^ , GetDataAreaPtr^ do
          Begin
            Caption := Format(rsCaptionDisplayFMHD , [ednMemoNo.AsInteger]) ;
            ShowModal ;

          End ;
      Finally
        Free ;
      End ;

    FreeMemCheck(pMemoPtr , nMemoSize) ;
  End ;  { TfrmFixedMemoHex.MemoHexViewClick }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.Font1Click                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.Font1Click(Sender : TObject) ;
  Begin  { TfrmFixedMemoHex.Font1Click }
    FontDialog1.Font := ExtendedMemo.Font ;
    FontDialog1.Execute ;
    ExtendedMemo.Font := FontDialog1.Font ;
  End ;  { TfrmFixedMemoHex.Font1Click }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.ShowMemoFixed                                 *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.ShowMemoFixed ;
  Begin  { TfrmFixedMemoHex.ShowMemoFixed }
    If nMemoNo = -1 Then
      bFixedShow := False
    Else
      Begin
        ednMemoNo.AsInteger := nMemoNo ;
        bFixedShow := True ;
      End ;

    ShowModal ;
  End ;  { TfrmFixedMemoHex.ShowMemoFixed }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.FormCreate                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.FormCreate(Sender : TObject) ;
  Begin  { TfrmFixedMemoHex.FormCreate }
    bFixedShow := False ;
  End ;  { TfrmFixedMemoHex.FormCreate }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.Cut1Click                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.Cut1Click(Sender : TObject) ;
  Begin  { TfrmFixedMemoHex.Cut1Click }
    SendMessage(ActiveControl.Handle , WM_Cut , 0 , 0) ;
  End ;  { TfrmFixedMemoHex.Cut1Click }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.Copy1Click                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.Copy1Click(Sender : TObject) ;
  Begin  { TfrmFixedMemoHex.Copy1Click }
    SendMessage(ActiveControl.Handle , WM_Copy , 0 , 0) ;
  End ;  { TfrmFixedMemoHex.Copy1Click }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.Paste1Click                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.Paste1Click(Sender : TObject) ;
  Begin  { TfrmFixedMemoHex.Paste1Click }
    SendMessage(ActiveControl.Handle , WM_Paste , 0 , 0) ;
  End ;  { TfrmFixedMemoHex.Paste1Click }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.Clear1Click                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.Clear1Click(Sender : TObject) ;
  Begin  { TfrmFixedMemoHex.Clear1Click }
    SendMessage(ActiveControl.Handle , WM_Clear , 0 , 0) ;
  End ;  { TfrmFixedMemoHex.Clear1Click }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.Undo1Click                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.Undo1Click(Sender : TObject) ;
  Begin  { TfrmFixedMemoHex.Undo1Click }
    SendMessage(ActiveControl.Handle , WM_Undo , 0 , 0) ;
  End ;  { TfrmFixedMemoHex.Undo1Click }


{***********************************************************************
*                                                                      *
*       TfrmFixedMemoHex.MemoListBoxClick                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemoHex.MemoListBoxClick(Sender : TObject) ;
  Begin  { TfrmFixedMemoHex.MemoListBoxClick }
    mdShowMemo(False) ;
  End ;  { TfrmFixedMemoHex.MemoListBoxClick }


End.
