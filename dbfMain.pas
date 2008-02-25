{***********************************************************************
*                                                                      *
*       dbfMain.pas                                                    *
*                                                                      *
*       (C) Copyright 1982-2002 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*       2001-05-27  BKC  Delete LastUpdateBox and add sleLastUpdate.   *
*       2001-05-27  BKC  Pretty up the code a bit.                     *
*       2002-01-12  BKC  Pretty up the code a bit more.                *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit
  dbfMain ;

Interface

uses
  Windows  , Messages , SysUtils , Classes  , Graphics , Controls ,
  Forms    , Dialogs  , Menus    , StdCtrls , Buttons  ,
  Registry , Mask     , ExtCtrls ,

  JvEdit        ,
  JvExStdCtrls  ,
  JvMRUList     ,
  JvTextListBox ,
  JvToolEdit    ,

  bcClasses       ,
  bcCmdLinePars   ,
  bcDialogUtilities   ,
  bcDiskScan      ,
  bcFileUtilities ,
  bcMemoryUtilities      ,
  bcNumericEdit   ,
  bcScreenUtilities ,
  bcStringUtilities ,

  DistBar  ,
  MRUFList ,

  dbfBackLinkDisplay ,
  dbfCommon          ,
  dbfConstant        ,
  dbfDataGrid        ,
  dbfExport          ,
  dbfMemoFileDisplay ,
  dbfOptions         ,
  dbfScanInfoDBV     ,
  dbfStatistics      ,
  dbfStructure         ;


Type
  TfrmMainDBF = class(TForm)
    OpenDialog : TOpenDialog ;

    GroupBox1 : TGroupBox ;
    GroupBox2 : TGroupBox ;
    GroupBox3 : TGroupBox ;

    DiskScan: TDiskScan;

    CalcFileSizeBox   : TFnpNumericEdit ;
    CalcHeaderSizeBox : TFnpNumericEdit ;
    CalcRecordSizeBox : TFnpNumericEdit ;
    FieldCountBox     : TFnpNumericEdit ;
    FileSizeBox       : TFnpNumericEdit ;
    HeaderSizeBox     : TFnpNumericEdit ;
    RecordsBox        : TFnpNumericEdit ;
    RecordSizeBox     : TFnpNumericEdit ;

    Label1  : TLabel ;
    Label2  : TLabel ;
    Label3  : TLabel ;
    Label4  : TLabel ;
    Label5  : TLabel ;
    Label6  : TLabel ;
    Label7  : TLabel ;
    Label8  : TLabel ;
    Label9  : TLabel ;
    Label10 : TLabel ;
    Label11 : TLabel ;
    Label12 : TLabel ;
    Label13 : TLabel ;
    Label14 : TLabel ;
    Label15 : TLabel ;
    Label16 : TLabel ;

    lblBackLink : TLabel ;

    edtLanguageDesc      : TEdit ;
    edtLastUpdateEnglish : TEdit ;
    FileTypeBox          : TEdit ;

    edtFilename          : TJvEdit ;
    edtShortFilename     : TJvEdit ;
    sleBackLink          : TJvEdit ;
    sleCodePage          : TJvEdit ;
    sleLanguageID        : TJvEdit ;
    sleLastUpdate        : TJvEdit ;
    sleMDXFlag           : TJvEdit ;
    sleSignature         : TJvEdit ;
    sleTerminator        : TJvEdit ;

    MainMenu : TMainMenu ;

    About1             : TMenuItem ;
    About2             : TMenuItem ;
    ClearMRU1          : TMenuItem ;
    Close1             : TMenuItem ;
    DBVConvertFix      : TMenuItem ;
    Directory1         : TMenuItem ;
    Edit1              : TMenuItem ;
    Exit1              : TMenuItem ;
    Field1             : TMenuItem ;
    File1              : TMenuItem ;
    FixedMemo1         : TMenuItem ;
    FixFieldErrors     : TMenuItem ;
    Global1            : TMenuItem ;
    Header1            : TMenuItem ;
    Header2            : TMenuItem ;
    Header3            : TMenuItem ;
    Help1              : TMenuItem ;
    MemoHeader1        : TMenuItem ;
    mnuBackLink        : TMenuItem ;
    mnuBackLinkFiles   : TMenuItem ;
    mnuConvert         : TMenuItem ;
    mnuDataGrid        : TMenuItem ;
    mnuDBVPars         : TMenuItem ;
    mnuPrintTable      : TMenuItem ;
    mnuReports         : TMenuItem ;
    mnuReStructure     : TMenuItem ;
    mnuScanFields      : TMenuItem ;
    mnuSep1            : TMenuItem ;
    mnuZapIndexMarkers : TMenuItem ;
    MRUList1           : TMenuItem ;
    New1               : TMenuItem ;
    Open1              : TMenuItem ;
    Options1           : TMenuItem ;
    PartialRecord1     : TMenuItem ;
    Print1             : TMenuItem ;
    ReBuild1           : TMenuItem ;
    Record1            : TMenuItem ;
    Record2            : TMenuItem ;
    Records1           : TMenuItem ;
    Repair1            : TMenuItem ;
    Search1            : TMenuItem ;
    Statistics1        : TMenuItem ;
    Structures1        : TMenuItem ;
    Table1             : TMenuItem ;
    Validate1          : TMenuItem ;
    View1              : TMenuItem ;
    ZapFldNames1       : TMenuItem ;
    mnuExport: TMenuItem;
    dfsMRUFileList1: TdfsMRUFileList;

    Procedure About1Click(Sender: TObject);
    procedure About2Click(Sender: TObject);
    procedure ClearMRU1Click(Sender: TObject);
    Procedure Close1Click(Sender: TObject);
    procedure DBVConvertFixClick(Sender: TObject);
    Procedure DiskScanFileFound(Sender: TObject; FileName: String);
    Procedure DiskScanScanFinish(Sender: TObject);
    Procedure DiskScanSpecialEvent(Sender: TObject);
    Procedure Exit1Click(Sender: TObject);
    Procedure Field1Click(Sender: TObject);
    Procedure FixedMemo1Click(Sender: TObject);
    Procedure FixFieldErrorsClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormClose(Sender: TObject; var Action: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Global1Click(Sender: TObject);
    Procedure Header1Click(Sender: TObject);
    Procedure Header2Click(Sender: TObject);
    Procedure Header3Click(Sender: TObject);
    Procedure MemoHeader1Click(Sender: TObject);

    procedure mnuBackLinkClick(Sender: TObject);
    Procedure mnuConvertClick(Sender: TObject);
    procedure mnuDataGridClick(Sender: TObject);
    procedure mnuDBVParsClick(Sender: TObject);
    procedure mnuExportClick(Sender: TObject);
    Procedure mnuPrintTableClick(Sender: TObject);
    procedure mnuReStructureClick(Sender: TObject);
    procedure mnuScanFieldsClick(Sender: TObject);
    procedure mnuZapIndexMarkersClick(Sender: TObject);

    Procedure New1Click(Sender: TObject);
    Procedure Open1Click(Sender: TObject);
    Procedure ReBuild1Click(Sender: TObject);
    Procedure Record2Click(Sender: TObject);
    procedure Records1Click(Sender: TObject);
    Procedure Reset1Click(Sender: TObject);
    procedure Statistics1Click(Sender: TObject);
    Procedure Structures1Click(Sender: TObject);
    Procedure Table1Click(Sender: TObject);
    Procedure ZapFldNames1Click(Sender: TObject);
    procedure dfsMRUFileList1MRUItemClick(Sender: TObject;
      AFilename: String);

  Protected
    OldCursor : TCursor     ;
    RegSave   : TRegIniFile ;

    bCalled  : Boolean ;
    cFileMDI : TFileName ;

    Procedure TestMRU ;

  Private  { Private declarations }
    oFileList : TStringList ;

    xBaseIndex   : Integer ;
    xBaseFilePtr : pTxBase ;

    Procedure SetScreen ;
    Procedure DisplayScreen(bShowDateError : Boolean) ;
    Procedure ClearScreen ;
    Procedure EnableMenuSelections(bOn : Boolean) ;


  Public  { Public declarations }
    Constructor Create(aOwner : TComponent ;
                       cFile  : TFileName = '') ; reintroduce ;

    Destructor Destroy ; override ;

    Procedure SetParameters(cFileMDI : TFileName) ;

  End ;

Var
  frmMainDBF : TfrmMainDBF ;

Implementation
  Uses
    bcFlushDrive ,
    bcHeapView   ,

    dbfAboutBox         ,
    dbfConvert          ,
    dbfConvertFixDBV    ,
    dbfCreate           ,
    dbfCreateFile       ,
    dbfFieldView        ,
    dbfFixedMemoDisplay ,
    dbfFixFields        ,
    dbfHeaderEdit       ,
    dbfRecordDisplay    ,
    dbfRepairHeader     ,
    dbfRepairRecords    ,
    dbfResources        ,
    dbfRestructure      ,
    dbfScanFields       ,
    dbfStructurePrint   ,
    dbfValidateFile       , dbfMsgs;

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfrmMain.Create                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Constructor TfrmMainDBF.Create(aOwner : TComponent ;
                               cFile  : TFileName = ''  ) ;
  Begin  { TfrmMainDBF.Create }
    xBaseIndex   := 0   ;
    xBaseFilePtr := nil ;
    
    cFileMDI := Trim(cFile) ;
    bCalled := (Length(cFileMDI) > 0) ;

    Inherited Create(aOwner) ;
  End ;  { TfrmMainDBF.Create }


{***********************************************************************
*                                                                      *
*       TfrmMainDBF.Destroy                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Destructor TfrmMainDBF.Destroy ;
  Begin  { TfrmMainDBF.Destroy }
    Inherited Destroy ;
  End ;  { TfrmMainDBF.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmMainDBF.Exit1Click                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*       2001-05-27  BKC  Update error message when error closing down. *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Exit1Click(Sender : TObject) ;
  Begin  { TfrmMainDBF.Exit1Click }
    Try
      Close1Click(Sender) ;
    Except
      ShowErrorMessage('Error in closing down.') ;
    End ;

    FlushAllFileHandles ;
    If not FlushAllDrivesViaMsg then
      ShowErrorMessage('FlushAllDrivesViaMsg has failed.') ;

    Close ;
  End ;  { TfrmMainDBF.Exit1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.DisplayScreen                                         *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*       2001-05-27  BKC  Change LastUpdateBox to sleLastUpdate field.  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.DisplayScreen(bShowDateError : Boolean) ;
  Begin  { TfrmMain.DisplayScreen }
    With xBaseFilePtr^ do
      Begin
        edtShortFilename.Text       := GetShortDataFileName ;
        sleSignature.Text           := '$' + HexByte(GetSignature) ;
        sleTerminator.Text          := '$' + HexByte(GetTerminator) ;
        FileTypeBox.Text            := GetFileTypeMsg ;
        sleLastUpdate.Text          := DateToStr(GetLastUpdate(False)) ;
        edtLastUpdateEnglish.Text   := GetLastUpdateEnglish ;
        RecordsBox.AsInteger        := GetTotalRecords ;
        RecordSizeBox.AsInteger     := GetRecordSize   ;
        CalcRecordSizeBox.AsInteger := CalcRecordSize  ;
        HeaderSizeBox.AsInteger     := GetHeaderSize   ;
        CalcHeaderSizeBox.AsInteger := CalcHeaderSize  ;
        FileSizeBox.AsInteger       := GetDataFileSize ;
        CalcFileSizeBox.AsInteger   := CalcFileSize    ;
        FieldCountBox.AsInteger     := GetFieldCount   ;

        sleLanguageID.Text   := '$' + HexByte(GetLanguageDriver) ;
        edtLanguageDesc.Text := GetLanguageDriverDesc ;
        sleCodePage.Text     := IntToStrBlank(GetCodePage)       ;
        sleMDXFlag.Text      := '$' + HexByte(GetProductionMDX)  ;

        sleBackLink.Enabled := HasBackLink ;
        sleBackLink.Visible := HasBackLink ;
        lblBackLink.Enabled := HasBackLink ;
        lblBackLink.Visible := HasBackLink ;
        If HasBackLink then
          sleBackLink.Text := GetBackLinkStr ;

        mnuBackLink.Visible := HasBackLink ;
        mnuBackLink.Enabled := HasBackLink ;
      End ;
  End ;  { TfrmMain.DisplayScreen }


{***********************************************************************
*                                                                      *
*       TfrmMain.SetScreen                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.SetScreen ;
  Begin  { TfrmMain.SetScreen }
    edtFileName.Text := cLastFile ;

    If not dbfMultiUser then
      Begin
        If xBaseIndex > 0 then
          FreeDatabaseIdx(xBaseIndex) ;
        xBaseIndex := 0 ;
      End ;

    If xBaseIndex <= 0 then
      xBaseIndex := CreateDatabaseIdx(Self) ;

    xBaseFilePtr := GetDatabasePtr(xBaseIndex) ;
    If xBaseFilePtr = nil then
      Begin
        ShowErrorMessage('Nil pointer returned from GetDataBasePtr.' +
                         Chr(13) + Chr(10) +
                         'xBaseIndex = ' + IntToStr(xBaseIndex)) ;
      End ;

    xBaseFilePtr^.InitFile(cLastFile , '') ;
    xBaseFilePtr^.SetUserIndex(xBaseIndex) ;

    If bCalled then
      Begin
        Caption := 'xBase Utility  ' + IntToStr(xBaseFilePtr^.GetUserIndex) ;
      End ;

    With xBaseFilePtr^ do
      Begin
        If DataFileIsOpen then
          Begin
            dfsMRUFileList1.AddItem(cLastFile) ;
            dfsMRUFileList1.Save ;
            TestMRU ;

            SetFileName(cLastFile) ;
            DisplayScreen(True) ;
          End
        Else
          Begin
            ClearScreen ;
            ShowErrorMessage('File [' + cLastFile + '] not opened.') ;
          End ;
      End ;

    EnableMenuSelections(True) ;
  End ;  { TfrmMain.SetScreen }


{***********************************************************************
*                                                                      *
*       TfrmMain.EnableMenuSelections                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.EnableMenuSelections(bON : Boolean) ;
  Begin  { TfrmMain.EnableMenuSelections }
    Open1.Enabled         := not bON ;
    Close1.Enabled        := bON ;
    Edit1.Enabled         := bON ;
//    Search1.Enabled       := bON ;
    View1.Enabled         := bON ;
    Table1.Enabled        := bON ;
    Repair1.Enabled       := bON ;
    mnuPrintTable.Enabled := bON ;
//    mnuReports.Enabled    := bON ;
    mnuConvert.Enabled    := bON ;

    mnuReStructure.Enabled := bON ;

    If bON then
      Begin
        MemoHeader1.Enabled := xBaseFilePtr^.HasMemoFile ;
        FixedMemo1.Enabled  := xBaseFilePtr^.HasMemoFile ;
      End
    Else
      Begin
        MemoHeader1.Enabled := False ;
        FixedMemo1.Enabled  := False ;
      End ;
  End ;  { TfrmMain.EnableMenuSelections }


{***********************************************************************
*                                                                      *
*       TfrmMain.ClearScreen                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.ClearScreen ;
  Begin  { TfrmMain.ClearScreen }
    edtLanguageDesc.Clear ;
    edtLastUpdateEnglish.Clear ;
    FileTypeBox.Clear ;

    edtFilename.Clear ;
    edtShortFilename.Clear ;
    sleBackLink.Clear ;
    sleCodePage.Clear ;
    sleLanguageID.Clear ;
    sleLastUpdate.Clear ;
    sleMDXFlag.Clear ;
    sleSignature.Clear ;
    sleTerminator.Clear ;

    CalcFileSizeBox.Clear ;
    CalcHeaderSizeBox.Clear ;
    CalcRecordSizeBox.Clear ;
    FieldCountBox.Clear ;
    FileSizeBox.Clear ;
    HeaderSizeBox.Clear ;
    RecordsBox.Clear ;
    RecordSizeBox.Clear ;
  End ;  { TfrmMain.ClearScreen }


{***********************************************************************
*                                                                      *
*       TfrmMain.Open1Click                                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Open1Click(Sender : TObject) ;
  Begin  { TfrmMain.Open1Click }
    OpenDialog.InitialDir := cLastDir ;
    If not bParFile then
      If Assigned(Sender) then
        Begin
          If OpenDialog.Execute then
            cLastFile := OpenDialog.FileName
          Else
            Exit ;
        End ;

    WriteLastDir(xBaseIndex) ;

    SetScreen ;
  End ;  { TfrmMain.Open1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.Field1Click                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Field1Click(Sender : TObject) ;
  Begin  { TfrmMain.Field1Click }
    With TfmViewField.Create(Self         ,
                             xBaseFilePtr  ) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMain.Field1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.FormClose                                             *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.FormClose(    Sender : TObject      ;
                            Var Action : TCloseAction  ) ;
  Begin  { TfrmMain.FormClose }
    HourGlassCursor ;
    CommonDone ;
    SetCursor(OldCursor) ;
  End ;  { TfrmMain.FormClose }


{***********************************************************************
*                                                                      *
*       TfrmMain.Close1Click                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Close1Click(Sender : TObject) ;
  Begin  { TfrmMain.Close1Click }
    FreeDatabaseIdx(xBaseIndex) ;

    If Pointer(Sender) <> nil then
      dfsMRUFileList1.Save ;

    EnableMenuSelections(False) ;

    ClearScreen ;
  End ;  { TfrmMain.Close1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.FormActivate                                          *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.FormActivate(Sender : TObject) ;
  Begin  { TfrmMain.FormActivate }
    EnableMenuSelections(False) ;
    ClearScreen ;

    With dfsMRUFileList1 do
      Begin
        ClearAllItems ;
        If not Load then
          MessageDlg('Couldn''t load MRU file list.', mtWarning, [mbOK], 0);

        TestMRU ;
      End ;

    If (not FlushAllDrivesViaMsg) then
      ShowErrorMessage('FlushAllDrivesViaMsg has failed.') ;

    FlushAllFileHandles ;

    If bCalled then
      SetParameters(cFileMDI)
    Else
      GetCmdLineParameters ;

    If bParFile then
      Begin
        SetScreen ;
      End ;
  End ;  { TfrmMain.FormActivate }


{***********************************************************************
*                                                                      *
*       TfrmMain.Header2Click                                          *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Header2Click(Sender : TObject) ;
  Begin  { TfrmMain.Header2Click }
    With TfmHeapView.Create(Self , xBaseFilePtr^.GetHeaderPtr , 8192 , True) do
      Try
        Caption := 'Header View for ' + xBaseFilePtr^.GetDataFileName ;
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMain.Header2Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.Record2Click                                          *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Record2Click(Sender : TObject) ;
  Begin  { TfrmMain.Record2Click }
    If xBaseFilePtr^.GetTotalRecords > 0 then
      With TfrmRecordDisplay.Create(Application , xBaseFilePtr , 0) do
        Try
          SetRecEnable(True) ;
          ShowModal ;
        Finally
          Free ;
        End ;
  End ;  { TfrmMain.Record2Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.MemoHeader1Click                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.MemoHeader1Click(Sender : TObject) ;
  Begin  { TfrmMain.MemoHeader1Click }
    If xBaseFilePtr^.HasMemoFile then
      Try
        frmMemoFileDisplay := TfrmMemoFileDisplay.Create(Self , xBaseFilePtr) ;
        With frmMemoFileDisplay do
          Try
            ShowModal
          Finally
            FreeAndNil(frmMemoFileDisplay) ;
          End ;
      Except
        MessageDlg(cUnableToCreateForm , mtError , [mbOK] , 0) ;
      End ;
  End ;  { TfrmMain.MemoHeader1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.FixedMemo1Click                                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.FixedMemo1Click(Sender : TObject) ;
  Begin  { TfrmMain.FixedMemo1Click }
    With TfrmFixedMemo.Create(Self         ,
                              xBaseFilePtr ,
                              0             ) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMain.FixedMemo1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.ZapFldNames1Click                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.ZapFldNames1Click(Sender : TObject) ;
  Var
    nFld : Integer ;

  Begin  { TfrmMain.ZapFldNames1Click }
    With xBaseFilePtr^ do
      Begin
        For nFld := 1 to GetFieldCount do
          SetFieldName(GetFieldName(nFld) , nFld) ;
        WriteHeader ;
      End ;
  End ;  { TfrmMain.ZapFldNames1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.Header3Click                                          *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Header3Click(Sender : TObject) ;
  Begin  { TfrmMain.Header3Click }
    With TfrmRepairHeader.Create(Self , xBaseFilePtr) do
      Try
        ShowModal ;

      Finally
        Free ;
      End ;
      
    DisplayScreen(True) ;
  End ;  { TfrmMain.Header3Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.mnuPrintTableClick                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.mnuPrintTableClick(Sender : TObject) ;
  Begin  { TfrmMain.ClipperVars1Click }
    With TfrmStrucPrint.Create(Self , xBaseFilePtr) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMain.ClipperVars1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.Scan1Click                                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.mnuConvertClick(Sender : TObject) ;
  Begin  { TfrmMain.Scan1Click }
    With TConvertFileForm.Create(Self , xBaseFilePtr) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMain.Scan1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.About1Click                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.About1Click(Sender: TObject);
  Begin  { TfrmMain.About1Click }
    ShowMessage('Beta Version ' + GetCurrentFileVersion + #13#10 +
                'Compiled with ' + DelphiVersion) ;
  End ;  { TfrmMain.About1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.New1Click                                             *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.New1Click(Sender : TObject) ;
  Begin  { TfrmMain.New1Click }
    With TfmCreateFile.Create(Self) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;

    cLastFile := xBaseFilePtr^.GetDataFileName ;
    If Length(Trim(cLastFile)) > 0 then
      Begin
        SetScreen ;
      End ;
  End ;  { TfrmMain.New1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.Reset1Click                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Reset1Click(Sender : TObject) ;
  Begin  { TfrmMain.Reset1Click }
    dfsMRUFileList1.ClearAllItems ;
  End ;  { TfrmMain.Reset1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.FormCreate                                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.FormCreate(Sender : TObject) ;
  Begin  { TfrmMain.FormCreate }
    oFileList := TStringList.Create ;
    oFileList.Clear ;

    Try
      RegSave := TRegIniFile.Create ;

      // Section to look for within the registry
      RegSave.RootKey := HKEY_CURRENT_USER ;

      If not RegSave.OpenKey(cRegKey , False) then
        RegSave.CreateKey(cRegKey) ;
    Except
      On e: exception do
        ErrorMsg(rsErrRegIniFile , ['TfrmMain.FormCreate' , E.Message]) ;
    End ;
  End ;  { TfrmMain.FormCreate }



{***********************************************************************
*                                                                      *
*       TfrmMain.FormDestroy                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.FormDestroy(Sender : TObject) ;
  Begin  { TfrmMain.FormDestroy }
    FreeAndNil(oFileList) ;

    Try
      FreeAndNil(RegSave) ;
      
    Except
      //
    End ;
  End ;  { TfrmMain.FormDestroy }


{***********************************************************************
*                                                                      *
*       TfrmMain.ReBuild1Click                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.ReBuild1Click(Sender : TObject) ;
  Begin  { TfrmMain.ReBuild1Click }
    dfsMRUFileList1.ClearAllItems ;

    With DiskScan do
      Begin
        StartDirectory := ''      ;
        FileSpec       := '*.dbf' ;
        BeginScan ;
      End ;
  End ;  { TfrmMain.ReBuild1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.DiskScanFileFound                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.DiskScanFileFound(Sender   : TObject ;
                                    FileName : String   ) ;
  Begin  { TfrmMain.DiskScanFileFound }
    oFileList.Add(FileName) ;
  End ;  { TfrmMain.DiskScanFileFound }


{***********************************************************************
*                                                                      *
*       TfrmMain.DiskScanScanFinish                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.DiskScanScanFinish(Sender : TObject) ;
  Begin  { TfrmMain.DiskScanScanFinish }
    With dfsMRUFileList1 do
      Begin
        ClearAllItems ;
        AddStringList(oFileList.GetStringList) ;
//        UpdateRecentMenu ;
      End ;
  End ;  { TfrmMain.DiskScanScanFinish }


{***********************************************************************
*                                                                      *
*       TfrmMain.DiskScanSpecialEvent                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.DiskScanSpecialEvent(Sender : TObject) ;
  Begin  { TfrmMain.DiskScanSpecialEvent }
  //
  End ;  { TfrmMain.DiskScanSpecialEvent }


{***********************************************************************
*                                                                      *
*       TfrmMain.FixFieldErrorsClick                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.FixFieldErrorsClick(Sender : TObject) ;
  Begin  { TfrmMain.FixFieldErrorsClick }
    With TfmFixFields.Create(Self , xBaseFilePtr) do
      Try
        ShowModal ;
      Finally
        Release ;
      End ;
  End ;  { TfrmMain.FixFieldErrorsClick }


{***********************************************************************
*                                                                      *
*       TfrmMain.Header1Click                                          *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Header1Click(Sender: TObject);
  Begin  { TfrmMain.Header1Click }
    With TfrmHeaderEdit.Create(Self , xBaseFilePtr) do
      Begin
        ShowModal ;
        Free ;
      End ;
  End;  { TfrmMain.Header1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.Table1Click                                           *
*                                                                      *
*         Validate a single table.                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Table1Click(Sender : TObject) ;
  Begin  { TfrmMain.Table1Click }
    With TfrmValidateFile.Create(Self , xBaseFilePtr) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMain.Table1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.Structures1Click                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Structures1Click(Sender : TObject) ;
  Begin  { TfrmMain.Structures1Click }
    With TfrmStrucPrint.Create(Self , nil) do
      Try
        ShowModal ;

      Finally
        Free ;
      End ;
  End ;  { TfrmMain.Structures1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.Statistics1Click                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Statistics1Click(Sender : TObject) ;
  Begin  { TfrmMain.Statistics1Click }
    With TfmStatistics.Create(Self , xBaseFilePtr) do
      Try
        ShowModal ;

      Finally
        Free ;
      End ;
  End ;  { TfrmMain.Statistics1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.SetParameters                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.SetParameters(cFileMDI : TFileName) ;
  Begin  { TfrmMain.SetParameters }
    cLastDir  := '' ;

    cLastFile := RemoveFileQuotes(cFileMDI) ;
    cLastFile := Trim(cLastFile) ;  // Probably not necessary.

    If bcFileUtilities.FileExists(cLastFile) then
      Begin
        cLastDir := ExtractFilePath(cLastFile) ;
        bParFile := True ;
      End
    Else
      Begin
        ErrorMsg(rsErrDataFileNotExist , [cLastFile]) ;
        cLastDir  := '' ;
        cLastFile := '' ;
        bParFile  := False ;
      End ;
  End ;  { TfrmMain.SetParameters }


{***********************************************************************
*                                                                      *
*       TfrmMain.FormShow                                              *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.FormShow(Sender : TObject) ;
  Begin  { TfrmMain.FormShow }
    OldCursor := NormalCursor ;
    HideCursor ;
  End ;  { TfrmMain.FormShow }


{***********************************************************************
*                                                                      *
*       TfrmMain.Global1Click                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Global1Click(Sender: TObject);
  Begin  { TfrmMain.Global1Click }
    With TfmOptions.Create(Self) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMain.Global1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.ReStructure1Click                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.mnuReStructureClick(Sender : TObject) ;
  Begin  { TfrmMain.ReStructure1Click }
    If Assigned(xBaseFilePtr) then
      Try
        With TfrmRestructure.Create(Self , xBaseFilePtr) do
          Try
            ShowModal ;

          Finally
            Free ;
          End ;
      Except
        On E : Exception do
          ErrorMsg(errMsgUnknRestructure , ['TfrmMain.FormCreate' , e.Message]) ;
      End
    Else
      ErrorMsg(errMsgDatabaseNotOpen) ;
  End ;  { TfrmMain.ReStructure1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.Records1Click                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.Records1Click(Sender : TObject) ;
  Begin  { TfrmMain.Records1Click }
    Try
      With TfrmRepairRecords.Create(Self , xBaseFilePtr) do
        Try
          ShowModal ;

        Finally
          Free ;
        End ;
    Except
      On E : Exception do
        ErrorMsg(rsErrRepairRecFailed , ['TfrmMain.Records1Click' , E.Message]) ;
    End ;
  End ;  { TfrmMain.Records1Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.About2Click                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.About2Click(Sender : TObject) ;
  Begin  { TfrmMain.About2Click }
    With TfrmAboutBox.Create(Owner) do
      Try
        ShowModal ;

      Finally
        Free ;
      End ;
  End ;  { TfrmMain.About2Click }


{***********************************************************************
*                                                                      *
*       TfrmMain.mnuBackLinkClick                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.mnuBackLinkClick(Sender : TObject) ;
  Begin  { TfrmMain.mnuBackLinkClick }
    With TfrmBackLink.Create(Owner , xBaseFilePtr) do
      Try
        ShowModal ;

      Finally
        Free ;
      End ;
  End ;  { TfrmMain.mnuBackLinkClick }


{***********************************************************************
*                                                                      *
*       TfrmMain.TestMRU                                               *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.TestMRU ;
  Var
    bShowMRU  : Boolean     ;

  Begin  { TfrmMain.TestMRU }
    bShowMru := (dfsMRUFileList1.ComponentCount > 0) ;

    mnuSep1.Enabled := bShowMRU ;
    mnuSep1.Visible := bShowMRU ;

    ClearMRU1.Enabled := bShowMRU ;
    ClearMRU1.Visible := bShowMRU ;
  End ;  { TfrmMain.TestMRU }


{***********************************************************************
*                                                                      *
*       TfrmMain.ClearMRU1Click                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.ClearMRU1Click(Sender : TObject) ;
  Begin  { TfrmMain.ClearMRU1Click }
    dfsMRUFileList1.ClearAllItems ;
    dfsMRUFileList1.Save ;
    TestMRU ;
  End ;  { TfrmMain.ClearMRU1Click }


{***********************************************************************
*                                                                      *
*       TfrmMainDBF.DBVConvertFixClick                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.DBVConvertFixClick(Sender : TObject) ;
  Begin  { TfrmMainDBF.DBVConvertFixClick }
    With TfrmConvertFixDBV.Create(Owner , xBaseFilePtr) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMainDBF.DBVConvertFixClick }


{***********************************************************************
*                                                                      *
*       TfrmMainDBF.mnuDBVParsClick                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.mnuDBVParsClick(Sender : TObject) ;
  Begin  { TfrmMainDBF.mnuDBVParsClick }
    With TfrmScanInfoDBV.Create(Owner , xBaseFilePtr) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMainDBF.mnuDBVParsClick }


{***********************************************************************
*                                                                      *
*       TfrmMainDBF.mnuZapIndexMarkersClick                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.mnuZapIndexMarkersClick(Sender : TObject) ;
  Begin  { TfrmMainDBF.mnuZapIndexMarkersClick }
    With xBaseFilePtr^ do
      Begin
        RemoveIndexIndicators ;
        WriteHeader ;
      End ;
  End ;  { TfrmMainDBF.mnuZapIndexMarkersClick }

  
{***********************************************************************
*                                                                      *
*       TfrmMainDBF.mnuScanFieldsClick                                 *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.mnuScanFieldsClick(Sender : TObject) ;
  Begin  { TfrmMainDBF.mnuScanFieldsClick }
    With TfrmScanFields.Create(Owner , xBaseFilePtr) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMainDBF.mnuScanFieldsClick }


{***********************************************************************
*                                                                      *
*       TfrmMainDBF.mnuDataGridClick                                   *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.mnuDataGridClick(Sender : TObject) ;
  Begin  { TfrmMainDBF.mnuDataGridClick }
    With TfrmDataGrid.Create(Owner , xBaseFilePtr) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMainDBF.mnuDataGridClick }


{***********************************************************************
*                                                                      *
*       TfrmMainDBF.mnuExportClick                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.mnuExportClick(Sender: TObject);
  Begin  { TfrmMainDBF.mnuExportClick }
    With TfrmExport.Create(Owner , xBaseFilePtr) do
      Try
        ShowModal ;
      Finally
        Free ;
      End ;
  End ;  { TfrmMainDBF.mnuExportClick }


{***********************************************************************
*                                                                      *
*      TfrmMainDBF.dfsMRUFileList1MRUItemClick                         *
*                                                                      *
*      Modification                                                    *
*      ============                                                    *
*                                                                      *
***********************************************************************}

Procedure TfrmMainDBF.dfsMRUFileList1MRUItemClick(Sender    : TObject ;
                                                  aFilename : String   ) ;
  Begin  { TfrmMainDBF.dfsMRUFileList1MRUItemClick }
    If bcFileUtilities.FileExists(aFileName) then
      Begin
        Close1Click(nil)       ;  { Close routine erases cLastFile }
        cLastFile := aFileName ;  { so must set it here.           }
        Open1Click(nil) ;
      End
    Else
      ErrorMsg(rsErrDataFileNotExist , [cLastFile]) ;
  End ;  { TfrmMainDBF.dfsMRUFileList1MRUItemClick }

End.

