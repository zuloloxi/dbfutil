{***********************************************************************
*                                                                      *
*      dbfResources.pas                                                *
*                                                                      *
*       (C) Copyright 1982-2005  Bruce K. Christensen                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Unit dbfResources ;

interface

ResourceString
  //  dbfMain
  rsErrDataFileNotExist = 'File [%s] does not exist.' ;
  rsErrRepairRecFailed  = 'Repair records failed in %s'#13#10'Error message %s' ;
  rsErrRegIniFile       = 'Unable to create TRegIniFile object in %s'#13#10'Error message %s' ;

  errMsgUndefinedField     = 'Cannot display/edit this field.'#13#10'(undefined and not in Record buffer).' ;
  errMsgUndefinedFieldType = 'Field type is undefined.'                   ;
  errMsgUndefinedFieldNo   = 'Field number is undefined.'                 ;
  errMsgPartialRecordRead  = 'Record only partially read.'                ;
  errMsgUnknownRecordError = 'Unknown record error.'                      ;
  errMsgInsufficientMemory = 'Unable to allocate memory'                  ;
  errMsgRestructAllocated  = 'Re-structure array already allocated'       ;
  errMsgInvalidDataAreaPtr = 'Invalid data area pointer'                  ;
  ErrMsgBadDecimals        = 'Invalid decimal places in SetFieldDecimals' ;

  errMsgInvalidFieldWidth   = 'Invalid field width for field type.' ;

  errMsgInvalidLogicalField = 'Invalid Logical field contents.' ;
  errMsgInvalidGetRecord    = 'Invalid return from GetRecord.'  ;

  errMsgInvalidFieldInBadRecList = 'Invalid entry in bad field list.' ;

  errMsgDatabaseNotOpen = 'Database file not open.'            ;
  errMsgUnknRestructure = 'Error calling restructure routine in %s'#13#10'Error message %s' ;

{ dbfValidateFile }
ResourceString
  //  dbfFieldHexView
  rsErrSeekMemoInProc = 'SeekMemo error in %s' ;

  //  dbfFixedMemoDisplay
  rsErrScanMemoFieldsInProc   = 'ScanMemoFields error in %s'#13#10'Error message %s' ;
  rsErrorMissingMemo          = 'Memo [%d] not found.' ;
  rsErrorInvalidMemoNumber    = 'Invalid memo number [%d]' ;
  rsErrorMemoHexDisplay       = 'Error in hex display of memo [%d]' ;
  rsCaptionDisplayFMHD        = 'Hex View for Memo [%d]' ;
  rsErrorMemoFileDoesNotExist = 'Warning: Memo file not open or does not exist in:'#$0D#$0A#$0D#$0A'%s' ;

Const
  hintErrCnt_B = 'Binary field errors'#0    ;
  hintErrCnt_C = 'Character field errors'#0 ;
  hintErrCnt_D = 'Date field errors'#0      ;
  hintErrCnt_F = 'Float field errors'#0     ;
  hintErrCnt_G = 'General field errors'#0   ;
  hintErrCnt_I = 'Integer field errors'#0   ;
  hintErrCnt_L = 'Logical field errors'#0   ;
  hintErrCnt_M = 'Memo field errors'#0      ;
  hintErrCnt_N = 'Numeric field errors'#0   ;
  hintErrCnt_P = 'Picture field errors'#0   ;
  hintErrCnt_T = 'DateTime field errors'#0  ;
  hintErrCnt_Y = 'Currency field errors'#0  ;

Const
  nHintErrorCount = 12 ;

Type
  THintMsg = packed
    Record
      cHintMsg : String[24] ;
    End ;

  THints = Array[1..nHintErrorCount] of THintMsg ;

Const
  rsHintErrMsgs : THints = ((cHintMsg : hintErrCnt_B) ,
                            (cHintMsg : hintErrCnt_C) ,
                            (cHintMsg : hintErrCnt_D) ,
                            (cHintMsg : hintErrCnt_F) ,
                            (cHintMsg : hintErrCnt_G) ,
                            (cHintMsg : hintErrCnt_I) ,
                            (cHintMsg : hintErrCnt_L) ,
                            (cHintMsg : hintErrCnt_M) ,
                            (cHintMsg : hintErrCnt_N) ,
                            (cHintMsg : hintErrCnt_P) ,
                            (cHintMsg : hintErrCnt_T) ,
                            (cHintMsg : hintErrCnt_Y)  ) ;

Implementation

End.
