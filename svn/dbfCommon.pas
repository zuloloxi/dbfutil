{***********************************************************************
*                                                                      *
*       dbfCommon.pas                                                  *
*                                                                      *
*       (C) Copyright 1982-2003  Bruce K. Christensen                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfCommon ;

Interface
  Uses
    SysUtils , Classes , Dialogs , Windows ,

    bcStringUtilities ,

    dbfStructure ;

  Const
    xBaseMaxOpen = 64 ;

  Type
    TxBaseList    = Array[1..xBaseMaxOpen] of TxBase  ;
    pTxBaseList = Array[1..xBaseMaxOpen] of pTxBase ;

  Var
    xBaseCount   : Integer     ;
    xBaseList    : TxBaseList  ;
    xBasePtrList : pTxBaseList ;

    GoodFileList    : TxBaseList  ;
    GoodFilePtrList : pTxBaseList ;

    BadFileList    : TxBaseList  ;
    BadFilePtrList : pTxBaseList ;

    FixedFileList    : TxBaseList ;
    FixedFilePtrList : pTxBaseList ;

    GoodFile     : TxBase  ;
    GoodFilePtr  : pTxBase ;
    BadFile      : TxBase  ;
    BadFilePtr   : pTxBase ;
    FixedFile    : TxBase  ;
    FixedFilePtr : pTxBase ;

    dbDirFileName : String  ;  { Text file for saving the data directory. }
    cLastDir      : String  ;
    cLastFile     : String  ;
    bParFile      : Boolean ;

    dbfMinDate : TDateTime ;
    dbfMaxDate : TDateTime ;

  Procedure ReadLastDir  ;
  Procedure WriteLastDir(nIdx : Integer) ;

  Procedure CommonDone  ;

  Function  CreateDatabaseIdx(aOwner : TComponent) : Integer ;
  Procedure FreeDatabaseIdx(nIdx : Integer) ;
  Function  GetDatabasePtr(nIdx : Integer) : pTxBase ;

  Procedure CheckDatabasePtr(xPtr : pTxBase) ;

Implementation
  Uses
    bcFileUtilities ;

{***********************************************************************
*                                                                      *
*       CreateDatabaseIdx                                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function CreateDatabaseIdx(aOwner : TComponent) : Integer ;
  Var
    nIdx : Integer ;

  Begin  { CreateDatabaseIdx }
    nIdx  := 1 ;

    While (nIdx <= xBaseMaxOpen) do
      Begin
        If xBasePtrList[nIdx] = nil then
          Break
        Else
          Begin
            If nIdx > xBaseMaxOpen then
              Begin
                Raise Exception.Create('Maximum number of files open') ;
                Result := 0 ;
                Exit ;
              End ;
          End ;

        nIdx := nIdx + 1 ;
      End ;

    If nIdx <= xBaseMaxOpen then
      Begin
        xBaseCount := xBaseCount + 1 ;
        xBaseList[nIdx] := TxBase.Create(aOwner) ;

        xBaseList[nIdx] := TxBase.Create(aOwner) ;
        xBasePtrList[nIdx] := @xBaseList[nIdx] ;
        xBaseList[nIdx].ZapStructures ;

        Result := nIdx ;
      End
    Else
      Result := 0 ;
  End ;  { CreateDatabaseIdx }


{***********************************************************************
*                                                                      *
*       FreeDatabaseIdx                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure FreeDatabaseIdx(nIdx : Integer) ;
  Begin  { FreeDatabaseIdx }
    If ((nIdx >= 1) and (nIdx <= xBaseMaxOpen)) then
      If (xBasePtrList[nIdx] <> nil) then
        If xBasePtrList[nIdx]^.DataFileIsOpen then
          Try
            FreeAndNil(xBaseList[nIdx]) ;
            xBasePtrList[nIdx] := nil ;

            xBaseCount := xBaseCount - 1 ;
          Except
            // ?????
          End ;
  End ;  { FreeDatabaseIdx }


{***********************************************************************
*                                                                      *
*       GetDatabasePtr                                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function GetDatabasePtr(nIdx : Integer) : pTxBase ;
  Begin  { GetDatabasePtr }
    Result := xBasePtrList[nIdx] ;
  End ;  { GetDatabasePtr }


{***********************************************************************
*                                                                      *
*       CheckDatabasePtr                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure CheckDatabasePtr(xPtr : pTxBase) ;
  Begin  { CheckDatabasePtr }
    If xPtr = nil then
      Raise Exception.Create('Database pointer is nil.'#13#10) ;
  End ;  { CheckDatabasePtr }


{***********************************************************************
*                                                                      *
*       ReadLastDir                                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure ReadLastDir ;
  Var
    fDirectory : TextFile ;

  Begin  { ReadLastDir }
    cLastDir := '' ;
    dbDirFileName := ExtractFilePath(ParamStr(0)) + 'DB3.TXT' ;

    If bcFileUtilities.FileExists(dbDirFileName) Then
      Begin
        AssignFile(fDirectory, dbDirFileName);
        Reset(fDirectory);
        ReadLn(fDirectory, cLastDir);
        CloseFile(fDirectory);
      End;
  End ;  { ReadLastDir }


{***********************************************************************
*                                                                      *
*       WriteLastDir                                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure WriteLastDir(nIdx : Integer) ;
  Var
    fDirectory : TextFile ;

  Begin  { WriteLastDir }
    If (nIdx > 0) and (xBaseCount > 0) then
      With GetDatabasePtr(nIdx)^ Do
        Begin
          cLastDir := ExtractFilePath(cLastFile) ;

          AssignFile(fDirectory , dbDirFileName) ;
          Rewrite(fDirectory) ;
          Writeln(fDirectory , cLastDir)  ;
          CloseFile(fDirectory) ;
        End ;
  End ;  { WriteLastDir }


{***********************************************************************
*                                                                      *
*       CommonInitialize                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure CommonInitialize ;
  Var
    nI : Integer ;

  Begin  { CommonInitialize }
    LongDateFormat  := 'yyyy/mm/dd' ;
    ShortDateFormat := 'yyyy/mm/dd' ;

    dbfMinDate := bcStringUtilities.MinDate ;
    dbfMaxDate := bcStringUtilities.MaxDate ;

    ReadLastDir;

    xBaseCount := 0 ;

    FillChar(xBaseList , SizeOf(xBaseList) , 0) ;

    For nI := 1 to xBaseMaxOpen do
      xBasePtrList[nI] := nil ;

    GoodFilePtr  := @GoodFile  ;
    BadFilePtr   := @BadFile   ;
    FixedFilePtr := @FixedFile ;

    cLastDir  := '' ;
    cLastFile := '' ;
    bParFile  := False ;
  End ;  { CommonInitialize }


{***********************************************************************
*                                                                      *
*       xBaseFreeAll                                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure xBaseFreeAll ;
  Var
    nIdx : Integer ;
    
  Begin  { xBaseFreeAll }
    If xBaseCount > 0 then
      For nIdx := 1 to xBaseCount do
        If xBasePtrList[nIdx] <> nil then
          Begin
            FreeAndNil(xBaseList[nIdx]) ;
            xBasePtrList[nIdx] := nil ;
          End ;
  End ;  { xBaseFreeAll }


{***********************************************************************
*                                                                      *
*       CommonDone                                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure CommonDone;
  Begin  { CommonDone }
    Try
      xBaseFreeAll ;
    Except
      ShowMessage('Error in CommonDone.') ;
    End ;

    GoodFilePtr  := nil ;
    BadFilePtr   := nil ;
    FixedFilePtr := nil ;

    { Get rid of the tempory BMP (Blob) file. }
    Try
      If bcFileUtilities.FileExists(BlobFileName) Then
        DeleteFile(BlobFileName) ;
    Except
      ShowMessage('Error deleting ' + BlobFileName) ;
    End ;
  End ;  { CommonDone }

Initialization
  CommonInitialize ;
End.
