{***********************************************************************
*                                                                      *
*       dbfConvert.pas                                                 *
*                                                                      *
*       (C) Copyright 1982-2002 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Unit
  dbfConvert ;

Interface

Uses
  Windows  , Messages , SysUtils , Classes , Graphics ,
  Controls , Forms    , Dialogs  ,
  StdCtrls , ComCtrls  , W95Meter , ExtCtrls ,

  JvListBox    ,
  JvExStdCtrls ,

  bcClasses         ,
  bcMathUtilities   ,
  bcMemoryUtilities ,
  bcNewList         ,
  bcNumericEdit     ,
  bcStringUtilities ,

  SecureStream ,

  dbfCommon    ,
  dbfStructure ,
  dbfCreate      ;

Type
  TConvertFileForm =
    Class(TForm)
    pnlMain: TPanel;
    Label1: TLabel;
    FileToConvert: TEdit;
    Label2: TLabel;
    OutputFile: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    ProcessButton: TButton;
    enfUnConverted: TFnpNumericEdit;
    enfConverted: TFnpNumericEdit;
    tlbNewFields: TNewTextListBox;

    Procedure ProcessButtonClick(Sender : TObject) ;
    Procedure TransferButtonClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);

    Protected
      oOwner : TComponent ;

    Private
      xDbfPtr : pTxBase ;
      NewFile : NewFile_Type ;

      Procedure SetConvertFields ;
      Procedure TransferList ;
      Procedure ConvertFileData ;
      {$HINTS OFF}
      Function ConvertField(Var N            : NewFile_Type ;
                                InRecordPtr  : Pointer      ;
                                OutRecordPtr : Pointer      ;
                                OutFieldNo   : Integer       ) : Boolean ;
      {$HINTS ON}
      Function ConvertRecord(Var N         : NewFile_Type  ;
                                 InRecord  : TRecordBuffer ;
                                 OutRecord : TRecordBuffer  ) : Boolean ;
      Procedure CreateTypeFile(Var N : NewFile_Type) ;

    Public
      Constructor Create(AOwner : TComponent ;
                         xPtr   : pTxBase     ) ; reintroduce ;
      Destructor Destroy ; override ;
    End ;

Implementation

{$R *.DFM}

Uses
  bcFormTools ,

  bcFileUtilities ;

{***********************************************************************
*                                                                      *
*       TConvertFileForm.Create                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TConvertFileForm.Create(AOwner : TComponent ;
                                    xPtr   : pTxBase     ) ;
  Begin  { TConvertFileForm.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := AOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr ;
  End ;  { TConvertFileForm.Create }


{***********************************************************************
*                                                                      *
*       TfmStatistics.Destroy                                          *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TConvertFileForm.Destroy ;
  Begin  { TConvertFileForm.Destroy }
    Try
      Inherited Destroy ;
    Except
      ShowMessage('Destroy exception.') ;
    End ;
  End ;  { TConvertFileForm.Destroy }

  
{***********************************************************************
*                                                                      *
*       TConvertFileForm.SetConvertFields                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TConvertFileForm.SetConvertFields ;
  Begin  { TConvertFileForm.SetConvertFields }
    BlankEdits(Self) ;
    FileToConvert.Text := xDbfPtr^.GetDataFileName ;
    NewFile.nFileName  := ForceExtension(FileToConvert.Text , 'CNV') ;
    OutputFile.Text    := NewFile.nFileName ;
    RepaintEdits(Self) ;
  End ;  { TConvertFileForm.SetConvertFields }


{***********************************************************************
*                                                                      *
*       TConvertFileForm.TransferList                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TConvertFileForm.TransferList ;
  Var
    nField : Integer ;

  Begin  { TConvertFileForm.TransferList }
    tlbNewFields.Clear ;
    With xDbfPtr^ do
      For nField := 1 to GetFieldCount do
        With tlbNewFields.Items do
          Add(Format('%3d' , [nField])                  + ' ' +
                     TrimPad(GetFieldName(nField) , 11) + ' ' +
                     GetFieldType(nField)               + ' ' +
                     TrimPad(NewFieldTypeDesc[GetNewFieldType(nField)], 7) + ' ' +
                     Format('%3d' , [GetNewFieldWidth(nField)])) ;
  End ;  { TConvertFileForm.TransferList }


{***********************************************************************
*                                                                      *
*       TConvertFileForm.ProcessButtonClick                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TConvertFileForm.ProcessButtonClick(Sender : TObject) ;
  Begin  { TConvertFileForm.ProcessButtonClick }
    dbfCreate.InitializeConversion ;
    ConvertFileData ;
  End ;  { TConvertFileForm.ProcessButtonClick }

  
{***********************************************************************
*                                                                      *
*       TConvertFileForm.ConvertField                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TConvertFileForm.ConvertField(Var N            : NewFile_Type ;
                                           InRecordPtr  : Pointer      ;
                                           OutRecordPtr : Pointer      ;
                                           OutFieldNo   : Integer       ) : Boolean ;
  Var
    OldFieldNumb : Integer ;
    OldFieldName : String  ;
    OutFieldPtr  : Pointer ;
    cString      : String  ;

  Begin  { TConvertFileForm.ConvertField }
    OldFieldNumb := N.nFieldDesc[OutFieldNo].OldFieldNo ;
    OldFieldName := xDbfPtr^.GetFieldName(OldFieldNumb) ;

    With xDbfPtr^ ,  N , nFieldDesc[OutFieldNo] do
      Begin
        OutFieldPtr := Pointer(Integer(OutRecordPtr) + NewOffset) ;

        {  Convert field into a string variable and scan for invalid }
        {  characters.                                               }
        cString := xDbfPtr^.GetBufferFieldStr(InRecordPtr  ,
                                              OldFieldNumb  ) ;
        If not TestField(cString , OldFieldNumb) then
          cString := Spaces(Length(cString)) ;

        { Call the conversion routine. }
        If not ConvertToArray[NewFType](cString               ,
                                        OutFieldPtr           ,
                                        nFieldDesc[OutFieldNo] ) then
          ShowMessage('Field not converted [' + cString +
                      '] Name [' + OldFieldName + '] Type [' +
                      GetFieldType(OutFieldNo) + '] Fld [' +
                      Format('%3d' , [OutFieldNo]) + ']') ;
      End ;  { With F^ , Header , ... do  }

    Result := True ;
  End ;  { TConvertFileForm.ConvertField }
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertRecord                                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TConvertFileForm.ConvertRecord(Var N         : NewFile_Type  ;
                                            InRecord  : TRecordBuffer ;
                                            OutRecord : TRecordBuffer  ) : Boolean ;
  Begin  { TConvertFileForm.ConvertRecord }
    Result := (Char(InRecord[0]) in [' ' , '*']) ;
    If not Result then
      ShowMessage('Invalid delete byte [' + Char(InRecord[0]) + ']') ;
  End ;  { TConvertFileForm.ConvertRecord }


{***********************************************************************
*                                                                      *
*       TConvertFileForm.CreateTypeFile                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TConvertFileForm.CreateTypeFile(Var N : NewFile_Type) ;
  Var
    nField : Integer ;
    cOutLine : String ;

  Begin  { TConvertFileForm.CreateTypeFile }
    TypeFileStr := ForceExtension(xDbfPtr^.GetDataFileName , 'TYP') ;
    AssignFile(TypeFile , TypeFileStr) ;
    ReWrite(TypeFile) ;
    WriteLn(TypeFile , 'Type') ;
    WriteLn(TypeFile , '  Float = Extended ;') ;
    WriteLn(TypeFile) ;
    WriteLn(TypeFile , '  NewRecord_Type = Record') ;

    nField := 1 ;
    While nField <= NewFile.nFieldCount do
      With NewFile , nFieldDesc[nField] do
        Begin
          Write(TypeFile , ' ' : 21) ;
          Write(TypeFile , Pad(NewName , nNameMaxLen) , ' : ') ;
          Case NewFType of
            PascalStr :
              cOutLine := 'String[' + IntToStr(NewWidth - 1) + '] ;' ;

            nChar :
              cOutLine := 'Char ;' ;

            AsciiZStr :
              If NewWidth > 1 then
                cOutLine := 'Array[0..' + IntToStr(NewWidth + 1) +'] of Char ;'
              Else
                cOutLine := 'Char ;'  ;

            NBoolean :
              cOutLine := 'Boolean ;' ;

            NDate :
              cOutLine := 'TDateTime ;' ;

            NByte :
              cOutLine := 'Byte ;' ;

            NWord :
              cOutLine := 'Word ;' ;
            NInteger :
              cOutLine := 'Integer ;' ;

            NFloat :
              cOutLine := 'Float ;' ;
          End ;

          WriteLn(TypeFile , Pad(cOutLine , 20) + '// ' +
                             Format('%3d' , [NewOffset])) ;
          Inc(nField) ;
        End ;

    WriteLn(TypeFile , ' ' : 19 , 'End ;') ;
    WriteLn(TypeFile) ;
    WriteLn(TypeFile , 'Const') ;
    WriteLn(TypeFile ,
            '  NewRecord_Width = SizeOf(NewRecord_Type) ;  { ' ,
            IntToStr(NewFile.nRecordSize) + ' }') ;
    WriteLn(TypeFile ,
            '  NumberOfFields  = ' +
            IntToStr(NewFile.nFieldCount) + ' ;') ;
    CloseFile(TypeFile) ;
  End ;  { TConvertFileForm.CreateTypeFile }
{.PA}
{***********************************************************************
*                                                                      *
*       TConvertFileForm.ConvertFile                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TConvertFileForm.ConvertFileData ;
  Const
    MaxBufferSize = 65520 ;

  Var
    nField ,
    CurrentRecNo : Integer ;

    OutputBuffer : TRecordBuffer ;

    nLen ,
    nConvertedRecs      ,
    nUnConvertedRecords   : Integer ;
//    nMaxOutRecords        : Integer ;

    nBytesWritten : Integer ;  { Bytes written during a stream write call. }

  Begin  { TConvertFileForm.ConvertFileData }
    nUnConvertedRecords := 0 ;
    nConvertedRecs := 0 ;

    SetConvertFields ;

    nField := 1 ;
    NewFile.nNameMaxLen := 0 ;
    Repeat
      With xDbfPtr^ , NewFile do
        Begin
          If (GetFieldType(nField) in dbfStructure.AllFieldTypes) then
            Begin
              Inc(nFieldCount) ;

              With nFieldDesc[nFieldCount] do
                Begin
                  NewFType := GetNewFieldType(nField) ;
                  NewWidth := GetNewFieldWidth(nField) ;
                  OldFieldNo := nField ;
                  NewName := xDbfPtr^.GetFieldName(nField) ;
                  nLen := Length(NewName) ;
                  nNameMaxLen := MaxInteger(nNameMaxLen , nLen) ;
                  NewOffset := nRecordSize ;
                  Inc(nRecordSize , NewWidth) ;
                End ;
            End ;
        End ;
      Inc(nField) ;
    Until nField > xDbfPtr^.GetFieldCount ;


{***********************************************************************
*                                                                      *
*       TConvertFileForm.ConvertFileData                               *
*                                                                      *
*       < continued >                                                  *
*                                                                      *
***********************************************************************}

    With NewFile do
      If nRecordSize <= 0 then
        Begin
          MessageDlg('Invalid length (' + IntToStr(nRecordSize) + ') for new record' ,
                     mtError ,
                     [mbOK]  ,
                     0        ) ;
          Exit ;
        End ;

    CreateTypeFile(NewFile) ;  { Write out the new type structure(s). }

    With NewFile do
      Begin
        nFileVar := TFileStreamX.Create(nFileName , fmCreate) ;

        nTotalRecords := 0 ;
        //nMaxOutRecords := MaxBufferSize div nRecordSize ;
      End ;

    With xDbfPtr^ do
      Begin
        For CurrentRecNo := 1 to GetTotalRecords do
          Begin
            If not GetRecord(CurrentRecNo) then
              Begin
                MessageDlg('Unable to read xBase file record: ' +
                           IntToStr(CurrentRecNo) ,
                           mtError                ,
                           [mbOK]                 ,
                           0                       ) ;
                Exit ;
              End ;

            If ConvertRecord(NewFile                      ,
                             TRecordBuffer(GetRecordPtr^) ,
                             OutPutBuffer                  ) then
              Begin
                nBytesWritten := NewFile.nFileVar.Write(OutputBuffer , NewFile.nRecordSize) ;
                If nBytesWritten <> NewFile.nRecordSize then
                  ShowMessage('BlockWrite failed: Record Size [' +
                              IntToStr(NewFile.nRecordSize) +
                              '] Written [' +
                              IntToStr(nBytesWritten) + ']') ;
                Inc(nConvertedRecs) ;

//                With W95Meter1 do
//                  Percent := (CurrentRecNo * 100) div GetTotalRecords ;

                With enfConverted do
                  Begin
                    Text := IntToStr(nConvertedRecs) ;
                    Repaint ;
                  End ;
              End
            Else
              Begin
                Inc(nUnConvertedRecords) ;
                enfUnconverted.Text := IntToStr(nUnConvertedRecords) ;
                RepaintEdits(Self) ;
              End ;
          End ;  { For CurrentRecNo := 1 to GetTotalRecords }
      End ;

    FreeAndNil(NewFile.nFileVar) ;  { Close the data file. }
  End ;  { TConvertFileForm.ConvertFileData }


{***********************************************************************
*                                                                      *
*       TConvertFileForm.TransferButtonClick                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TConvertFileForm.TransferButtonClick(Sender: TObject);
  Begin  { TConvertFileForm.TransferButtonClick }
    TransferList ;
  End ;  { TConvertFileForm.TransferButtonClick }


{***********************************************************************
*                                                                      *
*       TConvertFileForm.FormShow                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TConvertFileForm.FormShow(Sender : TObject) ;
  Begin  { TConvertFileForm.FormShow }
    SetConvertFields ;
  End ;  { TConvertFileForm.FormShow }

End.
