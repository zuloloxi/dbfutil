{***********************************************************************
*                                                                      *
*       dbfFieldHexView.pas                                            *
*                                                                      *
*       (C) Copyright 1990-1999 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfFieldHexView ;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls , Buttons ,

  JvExStdCtrls ,
  JvListBox    ,

  bcClasses         ,
  bcDialogUtilities ,
  bcFileUtilities   ,
  bcMathUtilities   ,
  bcMemoryUtilities ,
  bcScreenUtilities ,
  bcStringUtilities ,

  dbfBMPMemoDisplay ,
  dbfCommon         ,
  dbfConstant       ,
  dbfFieldEdit      ,
  dbfStructure      ,
  bcNewList         ,

  SecureStream ;

type
  TfmFieldHexView = class(TForm)
    Panel1       : TPanel ;
    pnlTop       : TPanel ;
    pnlSplitter1 : TPanel ;
    pnlBottom    : TPanel ;

    edFieldName : TEdit ;

    Label1 : TLabel ;

    btnPicture    : TBitBtn ;
    btnHexPicture : TBitBtn ;
    btnEdit       : TBitBtn ;
    tlbField: TNewTextListBox;
    btnJPG: TBitBtn;

    Procedure FormShow(Sender : TObject) ;
    Procedure btnPictureClick(Sender : TObject) ;
    Procedure FormCreate(Sender : TObject) ;
    Procedure btnHexPictureClick(Sender : TObject) ;
    Procedure btnEditClick(Sender : TObject) ;
    procedure FormActivate(Sender: TObject);
    procedure btnJPGClick(Sender: TObject);

  Protected { Protected declarations }
    oOwner : TComponent ;

  Private  { Private declarations }
    nField   : Integer   ;
    nMemo    : Integer   ;
    cFldType : Char      ;
    nFldLen  : Integer   ;
    xDbfPtr  : pTxBase ;

    Procedure PictureHex ;
    Procedure EnableButtons ;

  Public   { Public declarations }
    Constructor Create(AOwner : TComponent ;
                       xPtr   : pTxBase    ;
                       nFld   : Integer     ) ; Reintroduce ;
    Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}

  Uses
    dbfResources ;

{***********************************************************************
*                                                                      *
*       TfmFieldHexView.Create                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfmFieldHexView.Create(AOwner : TComponent ;
                                   xPtr   : pTxBase    ;
                                   nFld   : Integer     ) ;
  Begin  { TfmFieldHexView.Create }
    CheckDatabasePtr(xPtr) ;
    If not xPtr^.ValidFieldNumber(nFld) then
      Raise Exception.Create(errMsgUndefinedFieldNo) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    nField  := nFld ;
    xDbfPtr := xPtr ;

    With xDbfPtr^ do
      Begin
        cFldType := GetFieldType(nField)         ;
        nFldLen  := GetFieldWidth(nField)        ;
        nMemo    := GetCurrentMemoNumber(nField) ;
      End ;
  End ;  { TfmFieldHexView.Create }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.Destroy                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfmFieldHexView.Destroy ;
  Begin  { TfmFieldHexView.Destroy }
    Inherited Destroy ;
  End ;  { TfmFieldHexView.Destroy }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.FormShow                                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldHexView.FormShow(Sender : TObject) ;
  Var
    nI ,
    nJ   : Integer ;
    pFld : Pointer ;
    cStr : String  ;

  Begin  { TfmFieldHexView.FormShow }
    EnableButtons ;  { Show/enable only the relevant buttons. }

    tlbField.Clear ;

    With xDbfPtr^ do
      Begin
        edFieldName.Text := GetFieldName(nField)       ;
        pFld             := GetCurrentFieldPtr(nField) ;

        nJ := 0 ;
        cStr := '' ;

        For nI := 1 to GetFieldWidth(nField) do
          Begin
            cStr := cStr + HexByte(Byte(Pointer(Integer(pFld) + (nI - 1))^)) + ' ' ;
            Inc(nJ) ;

            If (nJ = 16) or (nI = GetFieldWidth(nField)) then
              Begin
                cStr := Trim(cStr) ;
                If Length(cStr) > 0 then
                  tlbField.Items.Add(cStr) ;
                nJ := 0 ;
                cStr := '' ;
              End ;
          End ;
      End ;
  End ;  { TfmFieldHexView.FormShow }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.FormCreate                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldHexView.FormCreate(Sender : TObject) ;
  Begin  { TfmFieldHexView.FormCreate }
    btnPicture.Enabled := False ;
    btnPicture.Visible := False ;
  End ;  { TfmFieldHexView.FormCreate }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.FormActivate                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldHexView.FormActivate(Sender : TObject) ;
  Var
    bEnabled : Boolean ;

  Begin  { TfmFieldHexView.FormActivate }
    FormCentre(@Self) ;

    bEnabled := (nMemo > 0) ;

    btnPicture.Enabled := bEnabled ;
    btnPicture.Visible := bEnabled ;

    btnHexPicture.Enabled := bEnabled ;
    btnHexPicture.Visible := bEnabled ;
  End ;  { TfmFieldHexView.FormActivate }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.EnableButtons                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldHexView.EnableButtons ;
  Var
    bEnabled : Boolean ;

  Begin  { TfmFieldHexView.EnableButtons }
    bEnabled := (cFldType = 'B') ;

    btnHexPicture.Enabled := bEnabled ;
    btnHexPicture.Visible := bEnabled ;

    btnPicture.Enabled := bEnabled ;
    btnPicture.Visible := bEnabled ;
  End ;  { TfmFieldHexView.EnableButtons }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.btnPictureClick                                *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldHexView.btnPictureClick(Sender : TObject) ;
  Begin  { TfmFieldHexView.btnPictureClick }
    With xDbfPtr^ do
      Begin
        If (nMemo > 0) and (GetFieldType(nField) in MemoFieldTypes) then
          With TfmBMPMemoDisplay.Create(Self    ,
                                        xDbfPtr ,
                                        nMemo    ) do
            Try
              ShowModal ;

            Finally
              Free ;
            End ;
      End ;
  End ;  { TfmFieldHexView.btnPictureClick }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.HexPicture                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldHexView.PictureHex ;
  Var
    nI ,
    nJ    : Integer ;

    nByte       : Byte    ;
    cStr        : String  ;
    cAsciiStr   : String  ;
    pMemoBuffer : Pointer ;
    nMemoLen    : Integer ;
    nOffset     : Integer ;
    nMemoType   : Integer ;

  Begin  { TfmFieldHexView.PictureHex }
    tlbField.Clear ;
    pMemoBuffer := nil ;

    With xDbfPtr^ , GetDataAreaPtr^ do
      Begin
        edFieldName.Text := GetFieldName(nField) ;
        nMemo := GetCurrentMemoNumber(nField) ;

        nMemoType := GetMemoDescriptor(nMemo) ;
        Case nMemoType of
          1 , 2 : nMemoLen := GetMemoLength ;
        Else
          nMemoLen := 65520 ;
        End ;

        With dbfMemoBMPDesc do
          Begin
            GetMemCheck(pMemoBuffer , nMemoLen) ;
            If SeekMemo(nMemo) then
              { nBytesRead := } GetMemoFileVar^.Read(pMemoBuffer^ , nMemoLen) 
            Else
              ErrorMsg(rsErrSeekMemoInProc , ['TfmFieldHexView.PictureHex']) ;
          End ;

        nJ := 0 ;
        cStr      := '' ;
        cAsciiStr := '' ;
        nOffset   := 0  ;

        For nI := 1 to nMemoLen do
          Begin
            nByte   := Byte(Pointer(Integer(pMemoBuffer) + (nI - 1))^) ;
            cStr    := cStr + HexByte(nByte) + ' ' ;
            If Char(nByte) in NonPrintableChars then
              cAsciiStr := cAsciiStr + '.'
            Else
              cAsciiStr := cAsciiStr + Char(nByte) ;

            Inc(nJ) ;

            If (nJ = 16) or (nI = nMemoLen) then
              Begin
                If Length(Trim(cStr)) > 0 then
                  Begin
                    If nJ < 16 then
                      cStr := cStr + Spaces((16 - nJ) * 3) ;

                    cStr := HexIntReadable(nOffset) + ' : ' + cStr ;

                    tlbField.Items.Add(cStr + ' [' + cAsciiStr + ']') ;
                    nOffset := nOffset + nJ ;
                  End ;

                nJ := 0 ;

                cStr      := '' ;
                cAsciiStr := '' ;
              End ;
          End ;
      End ;

    GetMemCheck(pMemoBuffer , nMemoLen) ;
  End ;  { TfmFieldHexView.PictureHex }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.btnHexPictureClick                             *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldHexView.btnHexPictureClick(Sender : TObject) ;
  Begin  { TfmFieldHexView.btnHexPictureClick }
    PictureHex ;
  End ;  { TfmFieldHexView.btnHexPictureClick }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.btnEditClick                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldHexView.btnEditClick(Sender : TObject) ;
  Begin  { TfmFieldHexView.btnEditClick }
    With TfmFieldEdit.Create(Owner   ,
                             xDbfPtr ,
                             nField   ) do
      Try
        ShowModal ;

      Finally
        Free ;
      End ;
  End ;  { TfmFieldHexView.btnEditClick }


{***********************************************************************
*                                                                      *
*       TfmFieldHexView.btnJPGClick                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmFieldHexView.btnJPGClick(Sender: TObject);
  Const
    cFileName = 'dbfutil.jpg' ;

  Var
    pMemoBuffer   : Pointer  ;
    nBytesRead    : Integer  ;
    nMemoLen      : Integer  ;
    pLen          : pInteger ;
    nLen          : Integer  ;
    nBytesToWrite : Integer  ;
    nOfs          : Integer  ;

    oFileVar : TFileStreamX ;

    cFile : String ;

  Begin  { TfmFieldHexView.btnJPGClick }
    tlbField.Clear ;
    pMemoBuffer := nil ;
    nBytesRead := 0 ;

    With xDbfPtr^ , GetDataAreaPtr^ do
      Begin
        edFieldName.Text := GetFieldName(nField) ;
        nMemo := GetCurrentMemoNumber(nField) ;

        nMemoLen := 1000000 ;
        GetMemCheck(pMemoBuffer , nMemoLen) ;
        If SeekMemo(nMemo) then
          nBytesRead := GetMemoFileVar^.Read(pMemoBuffer^ , nMemoLen)
        Else
          ErrorMsg(rsErrSeekMemoInProc , ['TfmFieldHexView.btnJPGClick']) ;

        pLen := Pointer(Integer(pMemoBuffer) + 4) ;
        nLen := pLen^ ;
        nLen := SwapInteger(nLen) ;
        ShowMessage('Memo len = ' + IntToStr(nLen)                     + #$0D#$0A +
                    'Offset     8 = ' + IntToStr(GetMemoOffset(8))     + #$0D#$0A +
                    'Offset     9 = ' + IntToStr(GetMemoOffset(9))     + #$0D#$0A +
                    'Offset 14626 = ' + IntToStr(GetMemoOffset(14626)) + #$0D#$0A +
                    'nBytesRead   = ' + IntToStr(nBytesRead) ) ;
      End ;

    cFile := GetTmpPath + cFileName ;
    If bcFileUtilities.FileExists(cFile) then
      If not EraseFile(cFile) then
        ShowMessage('Did not erase JPG file.') ;

    Try
      oFileVar := TFileStreamX.Create(cFile , fmCreate) ;
    Except
      Begin
        MessageDlg('Cannot create JPG file [' + cFile + ']' , mtError, [mbOK], 0) ;
        oFileVar := nil ;
      End ;
    End ;

    If oFileVar <> nil then
      Begin
        nOfs := 24 ;
        nBytesToWrite := nLen - nOfs ;
        pLen := Pointer(Integer(pMemoBuffer) + nOfs) ;
        oFileVar.Write(pLen^ , nBytesToWrite) ;

        FreeAndNil(oFileVar) ;
      End ;

    FreeMemCheck(pMemoBuffer , nMemoLen) ;
  End ;  { TfmFieldHexView.btnJPGClick }

End.
