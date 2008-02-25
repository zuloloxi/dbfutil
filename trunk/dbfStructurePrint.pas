{***********************************************************************
*                                                                      *
*       dbfStructurePrint.pas                                          *
*                                                                      *
*       (C) Copyright 1982-2003 Bruce K. Christensen                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfStructurePrint ;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Mask, JvToolEdit,  ExtCtrls,

  bcClasses         ,
  bcDirectoryScan   ,
  bcDirScan         ,
  bcFileUtilities   ,
  bcMathUtilities   ,
  bcMemo            ,
  bcStringUtilities ,

  dbfCommon , dbfStructure, JvExStdCtrls, JvMemo ;

Type
{$HINTS OFF}
  TfrmStrucPrint = Class(TForm)
    pnlMain   : TPanel       ;
    pnlBottom : TPanel       ;
    pnlTop    : TPanel       ;
    btnScan   : TBitBtn      ;
//    DirScan1  : TDirectoryScanner ;
    GroupBox1 : TGroupBox    ;
    rbPAS     : TRadioButton ;
    rbPRG     : TRadioButton ;
    StructureMemo: TExtendedMemo;
    Panel1: TPanel;

    Procedure btnScanClick(Sender : TObject) ;
    Procedure FormCreate(Sender : TObject) ;

  Private  { Private declarations }
    bCreate      : Boolean   ;
    xDataBasePtr : pTxBase ;

    Procedure PrintStrucPRG(cFile : String) ;
    Procedure PrintStrucPAS(cFile : String) ;

  Public  { Public declarations }
    Constructor Create(AOwner : TComponent ;
                       xPtr   : pTxBase   ) ; reintroduce;
    Destructor Destroy ; override ;
  End ;
{$HINTS ON}

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

Constructor TfrmStrucPrint.Create(aOwner : TComponent ;
                                  xPtr   : pTxBase   ) ;
  Begin  { TfmStrucPrint.Create }
    CheckDatabasePtr(xPtr) ;
    
    Inherited Create(aOwner) ;

    xDataBasePtr := xPtr ;
  End ;  { TfmStrucPrint.Create }


{***********************************************************************
*                                                                      *
*       TfmStrucPrint.Destroy                                          *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmStrucPrint.Destroy ;
  Begin  { TfmStrucPrint.Destroy }
    Inherited Destroy ;
  End ;  { TfmStrucPrint.Destroy }


{***********************************************************************
*                                                                      *
*       TfmStrucPrint.PrintStrucPAS                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmStrucPrint.PrintStrucPAS(cFile : String) ;
  Const
    cSQ = '''' ;

  Var
    nField   : Integer     ;

    nFldTypeMaxWidth : Integer ;

  Begin  { TfmStrucPrint.PrintStrucPAS }
    With xDataBasePtr^ do
      Try
        If bCreate then
          InitFile(cFile , '') ;

        With StructureMemo , Lines do
          Begin
            Add('Var') ;
            Add('  Table1 : TTable ;') ;
            Add('') ;

            Add('With Table1 do') ;
            Add('  Begin') ;
            Add('    { The Table component must not be active }') ;
            Add('    Active := False ;') ;

            Add('') ;
            Add('    DatabaseName := ''DBNEW''        ;') ;
            Add('    TableType    := ttDBase        ;') ;
            Add('    TableName    := ''NewTable.DBF'' ;') ;

            Add('') ;
            Add('    { Field definitions }') ;
            Add('    With FieldDefs do') ;
            Add('      Begin') ;
            Add('        Clear ;') ;

            nFldTypeMaxWidth := 0 ;
            For nField := 1 to (GetFieldCount - 1) do
              nFldTypeMaxWidth := MaxInteger(nFldTypeMaxWidth          ,
                                             Length(InternalFieldDesc(nField))) ;
            For nField := 1 to (GetFieldCount - 1) do
              Begin
                Add('        Add(' +
                    Pad(cSQ                  +
                    GetFieldName(nField) +
                    cSQ                    ,
                    GetNameMaxWidth + 2     ) +
                    ' , ' +
                    Pad(InternalFieldDesc(nField) , nFldTypeMaxWidth) +
                    ' { ' + GetFieldType(nField) + ' } , ' +
                    PadLeft(IntToStr(GetFieldWidth(nField)) ,
                    IntWidth(GetFieldMaxWidth)       ) +
                    ' , ' +
                    'False) ;') ;
              End ;
            Add('      End ;') ;

            Add('') ;
            Add('    { Index descriptions }') ;
            Add('') ;
            Add('    CreateTable ;') ;
            Add('  End ;') ;
          End ;
      Finally
        If bCreate then
          CloseFiles ;
      End ;
  End ;  { TfmStrucPrint.PrintStrucPAS }


{***********************************************************************
*                                                                      *
*       TfmStrucPrint.PrintStrucPRG                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmStrucPrint.PrintStrucPRG(cFile : String) ;
  Var
    nDec   ,
    nMax   ,
    nField   : Integer ;

  Begin  { TfmStrucPrint.PrintStrucPRG }
    With xDataBasePtr^ do
      Try
        If bCreate then
          InitFile(cFile , '') ;

        With StructureMemo , Lines do
          Begin
            Add(' ') ;
            Add('* ' + JustFileName(GetDataFileName)) ;
            Add('aDBF := {}') ;

            nMax := GetNameMaxWidth ;
            nDec := Length(IntToStr(GetFieldMaxWidth)) ;

            For nField := 1 to GetFieldCount do
              Begin
                Add('AADD(aDBF , "' +
                    Pad(GetFieldName(nField) + '"' , nMax + 1) + ' , "' +
                    GetFieldType(nField) + '" , ' +
                    PadLeft(IntToStr(GetFieldWidth(nField)) , nDec) + ' , ' +
                    IntToStr(GetFieldDecimals(nField)) + '})') ;
              End ;

            Add('DBCREATE("' + JustName(GetDataFileName) + '" , aDBF)') ;
          End ;

      Finally
        If bCreate then
          CloseFiles ;
      End ;
  End ;  { TfmStrucPrint.PrintStrucPRG }


{***********************************************************************
*                                                                      *
*       TfmStrucPrint.btnScanClick                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmStrucPrint.btnScanClick(Sender : TObject) ;
  {
  Var
    nI : Integer ;

    cFileList : TStringList ;
  }
  Begin  { TfmStrucPrint.btnScanClick }
    {
    If bCreate then
      Begin
        With DirectoryScanner1 do
          Begin
            cFileList := TStringList.Create ;
            With StructureMemo do
              Begin
                Clear ;
                cFileList.Sort ;
                For nI := 0 to (DirectoryScanner1.Count - 1) do
                  cFileList.Add(Item[nI].Name) ;
              End ;

            StructureMemo.Clear ;
            For nI := 0 to (cFileList.Count - 1) do
              If rbPAS.Checked then
                PrintStrucPAS(cFileList[nI])
              Else
                If rbPRG.Checked then
                  PrintStrucPRG(cFileList[nI])
                Else
                  ShowMessage('Something is wrong') ;

            FreeAndNil(cFileList) ;
          End ;
      End
    Else
      Begin
        If rbPAS.Checked then
          PrintStrucPAS('')
        Else
          If rbPRG.Checked then
            PrintStrucPRG('')
          Else
            ShowMessage('Something is wrong') ;
      End ;
    }
  End ;  { TfmStrucPrint.btnScanClick }


{***********************************************************************
*                                                                      *
*       TfmStrucPrint.FormCreate                                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmStrucPrint.FormCreate(Sender : TObject) ;
  Begin  { TfmStrucPrint.FormCreate }
//    DirectoryEdit1.Visible := bCreate ;
//    If bCreate then
//      DirectoryEdit1.Text := cLastDir ;
  End ;  { TfmStrucPrint.FormCreate }

End.
