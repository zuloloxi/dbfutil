{***********************************************************************
*                                                                      *
*       dbfFixedMemoDisplay.pas                                        *
*                                                                      *
*       (C) Copyright 1982-2005  Bruce K. Christensen                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfFixedMemoDisplay ;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, bcStringUtilities , bcMathUtilities ,
  Menus, ExtCtrls, bcScreenUtilities , bcDialogUtilities ,

  JvEdit       ,
  JvExStdCtrls ,
  JvMemo       ,

  bcClasses         ,
  bcMemo            ,
  bcMemoryUtilities ,
  bcViewMem         ,

  ClipBrd       ,
  Buttons       ,
  bcNumericEdit ,
  bcHeapView    ,

  dbfCommon    ,
  dbfStructure   ;

Type
  TfrmFixedMemo = Class(TForm)
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
//    Splitter1: TSplitter;
    btnTranslate: TBitBtn;
    btnHexView: TBitBtn;
    Label2: TLabel;
    edtFieldType: TJvEdit;
    pnlRight: TPanel;
    MemoListBox: TListBox;
    pnlLeft: TPanel;
    ckbHex: TCheckBox;
    sleMemoSizeHex: TJvEdit;
    ImageBMP: TImage;
    ExtendedMemo: TViewMem;

    Procedure FormActivate(Sender: TObject);
    Procedure MemoHexViewClick(Sender: TObject);
    Procedure TranslateCRLF1Click(Sender : TObject) ;
    Procedure Font1Click(Sender: TObject);

    Procedure ShowMemoFixed ;

    Procedure FormCreate(Sender : TObject) ;
    Procedure Cut1Click(Sender : TObject) ;
    Procedure Copy1Click(Sender : TObject) ;
    Procedure Paste1Click(Sender : TObject) ;
    Procedure Clear1Click(Sender : TObject) ;
    Procedure Undo1Click(Sender : TObject) ;
    procedure MemoListBoxClick(Sender: TObject);
    procedure ckbHexClick(Sender: TObject);

  Protected
    oOwner : TComponent ;

  Private                               { Private declarations }
    xDbfPtr    : pTxBase ;
    bFixedShow : Boolean ;
    nRec       : Integer ;
    nField     : Integer ;
    nMemoNo    : Integer ;

    Procedure mdShowMemo(bFixed : Boolean) ;

  Public
    Constructor Create(aOwner   : TComponent ;
                       xPtr     : pTxBase    ;
                       nFieldNo : Integer     ) ; reintroduce ;
    Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}

  Uses
    dbfResources ;
    
{***********************************************************************
*                                                                      *
*       TRecordDisplayForm.Create                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmFixedMemo.Create(aOwner   : TComponent ;
                                 xPtr     : pTxBase    ;
                                 nFieldNo : Integer     ) ;
  Begin  { TFixedMemoForm.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr        ;
    nField  := nFieldNo    ;

    With xDbfPtr^ do
      Begin
        nRec := GetRecordNo ;
        If (GetFieldType(nField) in MemoFieldTypes) then
          nMemoNo := GetMemoNumber(GetRecordPtr , nField)
        Else
          nMemoNo := 0 ;
      End ;
  End ;  { TFixedMemoForm.Create }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.Destroy                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmFixedMemo.Destroy ;
  Begin  { TFixedMemoForm.Destroy }
    Inherited Destroy ;
  End ;  { TFixedMemoForm.Destroy }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.FormActivate                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.FormActivate(Sender : TObject) ;
  Begin  { TFixedMemoForm.FormActivate }
    ExtendedMemo.Clear ;

    With xDbfPtr^ do
      Begin
        Try
          ScanMemoFields ;
        Except
          On E : Exception do
            Begin
              ErrorMsg(rsErrScanMemoFieldsInProc , ['TFixedMemoForm.FormActivate' , E.Message]) ;
              Exit ;
            End ;
        End ;

        With MemoListBox Do
          Begin
            Clear ;
            Items.Assign(GetMemoFieldsPtr^) ;
          End ;

        If bFixedShow then
          mdShowMemo(True) ;
      End ;
  End ;  { TFixedMemoForm.FormActivate }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.mdShowMemo                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.mdShowMemo(bFixed : Boolean) ;
  Var
    pMemoPtr : Pointer ;

    cStr      : String  ;
    cMemoStr  : String  ;
    nMemoSize : Integer ;
    cMemoType : String  ;
    cType     : Char    ;

  Begin  { TFixedMemoForm.mdShowMemo }
    With xDbfPtr^ , GetDataAreaPtr^ do
      Begin
        If bFixed Then
          nMemoNo := ednMemoNo.AsInteger
        Else
          Begin
            nField              := dbfMemoIntFields.Items[MemoListBox.ItemIndex]                ;
            cStr                := Trim(Copy(MemoListBox.Items[MemoListBox.ItemIndex] , 1 , 9)) ;
            ednMemoNo.AsInteger := StrToInt(cStr)      ;
            nMemoNo             := ednMemoNo.AsInteger ;
          End ;

        cMemoType := GetFieldType(nField) ;
        SetFieldChoice(nField) ;

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

                    If nMemoSize > 0 then
                      Begin
                        pMemoPtr := GetMemoBufferPtr(nField) ;

                        // Fix offset for FPT memo types? xxxxxxxxx
                        If GetMemoBlockType = dbfFPTMemo then
                          Begin
                            If GetSignature = $F5 then
                              pMemoPtr := Pointer(LongWord(pMemoPtr) + 2) ;
                          End ;

                        cMemoStr := MakeStrEOF(pMemoPtr , nMemoSize) ;

                        ExtendedMemo.Lines.Clear ;
                        ExtendedMemo.Text := cMemoStr ;
                        ExtendedMemo.GoToTop ;
                        Show ;
                      End
                    Else
                      ErrorMsg(rsErrorMissingMemo , [nMemoNo]) ;
                  End ;

            'B' ,
            'G'   : Begin
                      ExtendedMemo.Visible := False ;
                      btnTranslate.Visible := False ;
                      ImageBMP.Visible := True;

                      ReadMemoBMP(nMemoNo) ;
                      ImageBMP.Picture.LoadFromFile(GetBlobFileName);
                      ImageBMP.Height := ImageBMP.Picture.Height ;
                      ImageBMP.Width  := ImageBMP.Picture.Width  ;

                      With ImageBMP do
                        Begin
                          Top  := (pnlLeft.Height - Height) div 2 ;
                          Left := (pnlLeft.Width  - Width ) div 2 ;
                        End ;
                    End ;
          End  { Case cType of }
        Else
          ErrorMsg(rsErrorInvalidMemoNumber , [nMemoNo]) ;
      End ;
  End ;  { TFixedMemoForm.mdShowMemo }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.MemoHexViewClick                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.MemoHexViewClick(Sender : TObject) ;
  Var
    pMemoPtr  : Pointer ;
    nMemoSize : Integer ;

  Begin  { TFixedMemoForm.MemoHexViewClick }
    With xDbfPtr^ , GetDataAreaPtr^ do
      Begin
        nMemoSize := GetMemoBufferLen(nField) ;
        If GetMemoBlockType = dbfFPTMemo then
          Begin
            Inc(nMemoSize , SizeOf(dbfMemoDesc)) ;
            GetMemCheck(pMemoPtr , nMemoSize) ;
            Move(dbfMemoDesc , pMemoPtr^ , SizeOf(dbfMemoDesc)) ;
            Move(GetMemoBufferPtr(nField)^                       ,
                 AddPtr(pMemoPtr , SizeOf(dbfMemoDesc))^ ,
                 GetMemoBufferLen(nField)                 ) ;
          End
        Else
          If GetFieldType(nField) = 'B' then
            Begin
              nMemoNo := GetCurrentMemoNumber(nField) ;
              ReadMemoBMP(nMemoNo) ;
              pMemoPtr  := GetMemoBMPPtr    ;
              nMemoSize := GetMemoBMPLength ;
            End
          Else
            pMemoPtr := GetMemoBufferPtr(nField) ;
      End ;

    With TfmHeapView.Create(Application ,
                            pMemoPtr    ,
                            nMemoSize   ,
                            True         ) do
      Try
        Caption := Format(rsCaptionDisplayFMHD , [ednMemoNo.AsInteger]) ;
        ShowModal ;

        With xDbfPtr^ , GetDataAreaPtr^ do
          If GetMemoBlockType = dbfFPTMemo then
            FreeMemCheck(pMemoPtr , nMemoSize) ;
      Finally
        Free ;
      End ;
  End ;  { TFixedMemoForm.MemoHexViewClick }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.Font1Click                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.Font1Click(Sender : TObject) ;
  Begin  { TFixedMemoForm.Font1Click }
    FontDialog1.Font := ExtendedMemo.Font ;
    FontDialog1.Execute ;
    ExtendedMemo.Font := FontDialog1.Font ;
  End ;  { TFixedMemoForm.Font1Click }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.TranslateCRLF1Click                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.TranslateCRLF1Click(Sender : TObject) ;
  Var
    cStr   : String  ;
    Buffer : PChar   ;
    Size   : Integer ;

  Begin  { TFixedMemoForm.TranslateCRLF1Click }
    With ExtendedMemo Do
      Begin
        Size := GetTextLen;                    {Get length of string in Edit1}
        Inc(Size);                             {Add room for null character}
        GetMemCheck(Pointer(Buffer) , Size) ;  {Creates Buffer dynamic variable}
        GetTextBuf(Buffer, Size) ;             {Puts Edit1.Text into Buffer}

        cStr := FixLFtoCRLF(Pointer(Buffer)  , Size) ;

        FreeMemCheck(Pointer(Buffer), Size) ;  {Frees memory allocated to Buffer}
        SetTextBuf(PChar(cStr)) ;
      End ;
  End ;  { TFixedMemoForm.TranslateCRLF1Click }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.ShowMemoFixed                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.ShowMemoFixed ;
  Begin  { TFixedMemoForm.ShowMemoFixed }
    If nMemoNo = -1 Then
      bFixedShow := False
    Else
      Begin
        ednMemoNo.AsInteger := nMemoNo ;
        bFixedShow := True ;
      End ;

    ShowModal ;
  End ;  { TFixedMemoForm.ShowMemoFixed }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.FormCreate                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.FormCreate(Sender : TObject) ;
  Begin  { TFixedMemoForm.FormCreate }
    bFixedShow := False ;
  End ;  { TFixedMemoForm.FormCreate }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.Cut1Click                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.Cut1Click(Sender : TObject) ;
  Begin  { TFixedMemoForm.Cut1Click }
    SendMessage(ActiveControl.Handle , WM_Cut , 0 , 0) ;
  End ;  { TFixedMemoForm.Cut1Click }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.Copy1Click                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.Copy1Click(Sender : TObject) ;
  Begin  { TFixedMemoForm.Copy1Click }
    SendMessage(ActiveControl.Handle , WM_Copy , 0 , 0) ;
  End ;  { TFixedMemoForm.Copy1Click }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.Paste1Click                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.Paste1Click(Sender : TObject) ;
  Begin  { TFixedMemoForm.Paste1Click }
    SendMessage(ActiveControl.Handle , WM_Paste , 0 , 0) ;
  End ;  { TFixedMemoForm.Paste1Click }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.Clear1Click                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.Clear1Click(Sender : TObject) ;
  Begin  { TFixedMemoForm.Clear1Click }
    SendMessage(ActiveControl.Handle , WM_Clear , 0 , 0) ;
  End ;  { TFixedMemoForm.Clear1Click }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.Undo1Click                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.Undo1Click(Sender : TObject) ;
  Begin  { TFixedMemoForm.Undo1Click }
    SendMessage(ActiveControl.Handle , WM_Undo , 0 , 0) ;
  End ;  { TFixedMemoForm.Undo1Click }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.MemoListBoxClick                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.MemoListBoxClick(Sender : TObject) ;
  Var
    nSize ,
    nMemoNo ,
    nMemoBinary : Integer ;

    cStr : String ;

  Begin  { TFixedMemoForm.MemoListBoxClick }
    If ckbHex.Checked then
      Begin
        ExtendedMemo.Clear ;

        cStr := Trim(Copy(MemoListBox.Items[MemoListBox.ItemIndex] , 1 , 9)) ;
        nMemoNo := StrToInt(cStr) ;
        ednMemoNo.AsInteger := nMemoNo ;
        nMemoBinary         := ednMemoNo.AsInteger ;
        xDbfPtr^.ReadMemoBinary(nMemoBinary) ;

        nSize                 := xDbfPtr^.GetDataAreaPtr^.dbfBinaryMemoLength ;
        ednMemoSize.AsInteger := nSize ;
        sleMemoSizeHex.Text   := HexInteger(nSize) ;

        With xDbfPtr^ , GetDataAreaPtr^ do
          Try
            ExtendedMemo.SetMemory(dbfMemoBuffer                    ,
                                   MinInteger(dbfMemoLength , 2048) ,
                                   True                              ) ;
          Except
            ErrorMsg(rsErrorMemoHexDisplay , [nMemoNo]) ;
          End ;
      End
    Else
      mdShowMemo(False) ;
  End ;  { TFixedMemoForm.MemoListBoxClick }


{***********************************************************************
*                                                                      *
*       TFixedMemoForm.ckbHexClick                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmFixedMemo.ckbHexClick(Sender: TObject);
  Begin  { TFixedMemoForm.ckbHexClick }
    ExtendedMemo.Clear ;
  End ;  { TFixedMemoForm.ckbHexClick }

End.
