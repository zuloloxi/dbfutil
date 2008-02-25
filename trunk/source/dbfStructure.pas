{***********************************************************************
*                                                                      *
*       dbfStructure.pas                                               *
*                                                                      *
*       (C) Copyright 1982-2003  Bruce K. Christensen                  *
*                                                                      *
*       To do:                                                         *
*         1) check memo number is valid - GetMemoPtr                   *
*         2) check for duplicate field names SetFieldName              *
*         3) TxBase.dbRepairFieldName needs work.                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*       2000-09-29  BKC  VerifyField                                   *
*       2006-07-12  BKC  Change name from dbfStruc to dbfStructure.pas *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

{$A-}

Unit dbfStructure ;

Interface

Uses
  Windows  , Forms    , Dialogs  , SysUtils ,
  StdCtrls , Classes  , Graphics , ExtCtrls , Controls ,

  bcBmpStructure    ,
  bcClasses         ,
  bcDialogUtilities ,
  bcFileUtilities   ,
  bcMathUtilities   ,
  bcMemoryUtilities ,
  bcStringUtilities ,

  SecureStream ,

  dbfConstant ;

Const
  dbfMultiUser   = False ;

  CRLF = #$0D#$0A ;
  
Type
  TDBVStru = packed
    Record
      Size        : Integer ;
      Signature   : Byte    ;
      Compression : Byte    ;
    End ;

  pCharSet  = ^CharSet    ;
  CharSet   = set of Char ;
  BitMapPtr = ^TBitMap    ;

  ProcType = Procedure(nPerc    : Integer ;
                       nRec     : Integer ;
                       xFormPtr : Pointer  ) ;
Const
  MaxRecordWidth = 4096 ;
  MaxFieldWidth  = 4096 ;

Type
  TRecordBuffer  = Array[0..(MaxRecordWidth - 1)] of Byte ;
  pTRecordBuffer = ^TRecordBuffer  ;

  TFieldBuffer  = Array[0..(MaxFieldWidth - 1)] of Char ;
  pTFieldBuffer = ^TFieldBuffer  ;

Const
  dbfMaxFields   = 255   ;  { Maximum number of fields in TxBase file. }
  MaxSegmentSize = 65520 ;
  dbfMaxNameLen  = 10    ;  { This doesn't include the '00' AsciiZ terminator. }

  BlobFileName = 'db3blob.bmp' ;

  dbfMaxMemoSize      = 65520 ;
  dbfMaxMemoBlockSize = 4096  ;
  dbfTerminator       = #$0D  ;
  dbfNull             = #00   ;
  dbfDefaultMemoWidth = 68    ;  { 68 is the memo width for PC File. }
  dbfDataFileExt      = 'DBF' ;  { Default data file extenstion. }
  dbIndexFileExt      = 'NDX' ;  { Default index file extension. }

  dbfGoodDataFileExt  = 'GDF' ;  { Good data filename extension.  }
  dbfGoodMemoFileExt  = 'GDT' ;  { Good memo filename extension.  }
  dbfBadDataFileExt   = 'BDF' ;  { Bad data filename extension.   }
  dbfBadMemoFileExt   = 'BDT' ;  { Bad memo filename extension.   }
  dbfFixedDataFileExt = 'FDF' ;  { Fixed data filename extension. }
  dbfFixedMemoFileExt = 'FDT' ;  { Fixed memo filename extension. }

  dbfDateFieldWidth = 8 ;

  dbfMemoFieldWidth  = 10     ;  { Width of a memo field.         }
  MaxMemoSize        = 10000  ;  { Maximum memo size.             }

  dbfBackLinkSize = 263 ;

  OneEof : Array[1..2] of Char = (#$1A , #$00) ;
  TwoEOF : Array[1..3] of Char = (#$1A , #$1A , #$00)  ;

Type
  aFldCntType = Array[0..dbfMaxFields] of Integer ;

Const
  ZeroChar = Char(0) ;

  { Characters that are legal for the first letter of a field name. }
  LegalFieldFirstChars = ['A'..'Z' , '_'] ;
  { Characters that are legal for any letter of a field name. }
  LegalFieldNameChars  = ['A'..'Z' , '_' , '0'..'9'] ;

Type
  ArrayCharSet    = Array[1..NoOfFieldTypes] of CharSet ;
  FieldErrorsType = Array[0..NoOfFieldTypes] of Integer ;

Const
  AllFields = 'BCDFGILMNPTY' ;
  LegalFieldTypes : String[NoOfFieldTypes] = AllFields ;

  DB3_FldTypes : CharSet = ['C' , 'D' , 'L' , 'M' , 'N'] ;
  DB4_FldTypes : CharSet = ['C' , 'D' , 'F' , 'L' , 'M' , 'N'] ;
  DB5_FldTypes : CharSet = ['B' , 'C' , 'D' , 'F' , 'G' , 'L' , 'M' , 'N'] ;
  FXP_FldTypes : CharSet = ['B' , 'C' , 'D' , 'F' , 'G' , 'L' , 'M' , 'N' , 'P'] ;
  VDB_FldTypes : CharSet = ['B' , 'C' , 'D' , 'F' , 'G' , 'L' , 'M' , 'N' , 'P'] ;
  FLG_FldTypes : CharSet = ['2' , '4' , '8' , 'C' , 'D' , 'F' , 'L' , 'M' , 'N' , 'V'] ;

  AllFieldTypes  : CharSet = ['B' , 'C' , 'D' , 'F' ,
                              'G' , 'I' , 'L' , 'M' ,
                              'N' , 'P' , 'T' , 'Y'  ] ;

  DecimalFieldTypes : CharSet = ['F' , 'N' , 'Y'] ;

  NoDecimalFieldTypes : CharSet = ['B' , 'C' , 'D' , 'G' ,
                                   'I' , 'L' , 'M' , 'P' ,
                                   'T'                    ] ;

  FieldFixedWidth : Array[1..NoOfFieldTypes] of Boolean =
                     (True  ,  { B }
                      False ,  { C }
                      True  ,  { D }
                      False ,  { F }
                      False ,  { G }
                      False ,  { I }
                      False ,  { L }
                      False ,  { M }
                      False ,  { N }
                      True  ,  { P }
                      True  ,  { T }
                      True     { Y }  ) ;

  MinMaxField : Array[1..NoOfFieldTypes , 1..2] of Integer =
                  (( 1 ,  10) ,     { B }
                   ( 1 , 255) ,     { C }
                   ( 8 ,   8) ,     { D }
                   ( 1 ,  16) ,     { F }
                   ( 1 ,  10) ,     { G }
                   ( 4 ,   4) ,     { I }
                   ( 1 ,   1) ,     { L }
                   (10 ,  10) ,     { M }
                   ( 1 ,  16) ,     { N }
                   ( 1 ,  16) ,     { P }
                   ( 1 ,   8) ,     { T }
                   ( 1 ,  16)  ) ;  { Y }


  MemoFieldTypes : CharSet = ['G' , 'M' , 'B'] ;

  ValidDelete  = [' ' , '*'] ;

  {
  Binary    B
  Character C
  Date      D
  Float     F
  General   G
  Integer   I
  Logical   L
  Memo      M
  Numeric   N
  Picture   P
  DateTime  T
  Currency  Y

  Binary
  Character
  Date
  Float
  General
  Integer
  Logical
  Memo
  Numeric
  Picture
  DateTime
  Currency
  }
  
  FieldTypeNamesMaxWidth = 9 ;
  FieldTypeNames : Array[1..NoOfFieldTypes] of String[FieldTypeNamesMaxWidth] =
                    ('Binary'    ,     { B }
                     'Character' ,     { C }
                     'Date'      ,     { D }
                     'Float'     ,     { F }
                     'General'   ,     { G }
                     'Integer'   ,     { I }
                     'Logical'   ,     { L }
                     'Memo'      ,     { M }
                     'Numeric'   ,     { N }
                     'Picture'   ,     { P }
                     'DateTime'  ,     { T }
                     'Currency'   ) ;  { Y }

  { Some of the ranges here are probably not valid, need to }
  { check:                                                  }
  {                                                         }
  {    ValidDateTime - FoxPro                               }
  {    ValidCurrency - FoxPro                               }
  {                                                         }
  dbfValidLogicalTrue   = ['y','Y','t','T']     ;  { L - True  }
  dbfValidLogicalFalse  = [' ','n','N','f','F'] ;  { L - False }

  dbfValidBinary   = [' ','0'..'9'] ;                              { B }
  dbfValidChar     = [' '..'~'] ;                                  { C }
  dbfValidDate     = [' ','0'..'9'] ;                              { D }
  dbfValidFloat    = [' ','0'..'9','-','.'] ;                      { F }
  dbfValidGeneral  = [' ','0'..'9'] ;                              { G }
  dbfValidInteger  = [#00..#255] ;                                 { I }
  dbfValidLogical  = dbfValidLogicalTrue + dbfValidLogicalFalse ;  { L }
  dbfValidMemo     = [' ','0'..'9'] ;                              { M }
  dbfValidNumeric  = [' ','0'..'9','-','.'] ;                      { N }
  dbfValidPicture  = [' ','0'..'9'] ;                              { P }
  dbfValidDateTime = [' '..'~'] ;                                  { T }
  dbfValidCurrency = [' '..'~'] ;                                  { Y }

  ValidSetArray : ArrayCharSet = ( dbfValidBinary   ,     { B }
                                   dbfValidChar     ,     { C }
                                   dbfValidDate     ,     { D }
                                   dbfValidFloat    ,     { F }
                                   dbfValidGeneral  ,     { G }
                                   dbfValidInteger  ,     { I }
                                   dbfValidLogical  ,     { L }
                                   dbfValidMemo     ,     { M }
                                   dbfValidNumeric  ,     { N }
                                   dbfValidPicture  ,     { P }
                                   dbfValidDateTime ,     { T }
                                   dbfValidCurrency  ) ;  { Y }
Type
  NewFieldType = (PascalStr , AsciiZStr , NBoolean ,
                  NDate     , NByte     , NWord    ,
                  NInteger  , NFloat    , NChar     ) ;

  NewFieldDesc_Type = Packed
    Record
      NewName    : String10     ;
      NewFType   : NewFieldType ;
      NewWidth   : Integer      ;
      NewOffset  : Integer      ;
      OldFieldNo : Integer      ;
    End ;

  NewFile_Type = Packed
    Record
      nFileVar      : TFileStreamX ;
      nFileName     : String  ;
      nRecordSize   : Word    ;
      nTotalRecords : Integer ;
      nFieldCount   : Integer ;
      nNameMaxLen   : Integer ;
      nFieldDesc    : Array[1..dbfMaxFields] of NewFieldDesc_Type ;
    End ;

  dbfStrucDescType = Packed
    Record
      cName     : String[dbfMaxNameLen] ;
      cType     : Char                  ;
      nWidth    : Byte                  ;
      nDecimals : Byte                  ;
    End ;

  dbfStrucArrayType = Array[0..dbfMaxFields] of dbfStrucDescType ;

  FieldNamePtr     = ^FieldName_Type ;
  FieldName_Type   = Array[0..dbfMaxFields] of String10 ;

  BooleanFieldArrayPtr = ^BooleanFieldArray ;
  BooleanFieldArray = Array[1..dbfMaxFields] of Boolean ;

  dbfFieldByteArrayPtr = ^dbfFieldByteArrayType ;
  dbfFieldByteArrayType = Array[1..dbfMaxFields] of Byte ;

  dbfFieldBooleanArrayPtr = ^dbfFieldBooleanArrayType ;
  dbfFieldBooleanArrayType = Array[1..dbfMaxFields] of Boolean ;

  { Field descriptor layout in header (must have at least 1). }
  dbfFieldDescPtr  = ^dbfFieldDescType ;
  dbfFieldDescType = Packed
    Record
      Name     : Array[0..dbfMaxNameLen] of AnsiChar ;  { AsciiZ string. }
      FType    : Char    ;
      FOffset  : Integer ;
      FWidth   : Byte    ;  { Field width.  }
      Decimals : Byte    ;  { No. decimal places. }

      FColumnFlags : Byte ;  { FoxPro file definitions:                   }
                             { $01 System Column (not visible to user)    }
                             { $02 Column can store null values           }
                             { $04 Binary column (for CHAR and MEMO only) }

      ReservedMultiUser01 : Byte ;
      WorkAreaId          : Byte ;
      ReservedMultiUser02 : Array[1..2] of Byte ;
      SetFieldsFlag       : Byte ;
      Reserved            : Array[1..7] of Byte ;
      Indexed             : Byte                ;  { $01 if field indexed }
                                                   { in MDX file.         }
    End ;

  { Structure of the fixed length portion of the header. }
  xBaseFixedHeaderPtr = ^xBaseFixedHeaderType ;
  xBaseFixedHeaderType = Packed
    Record
      Signature : Byte ;
      { Determines type of data/memo file. }
      { See: Function TxBase.GetFileTypeMsg : String ;       }
      {      Function TxBase.GetMemoFileExtension : String ; }

      { Date the data file was last updated. }
      LastUpdate : Packed
        Record
          Year  : Byte ;
          Month : Byte ;
          Day   : Byte ;
        End ;

      dbTotalRecords : Integer ;  { No. records in data file. }
      dbfHeaderSize  : Word    ;  { Header size in bytes.     }
      dbfRecordSize  : Word    ;  { Size of record in bytes.  }

      Reserved1      : Array[0..1] of Byte ;
      Transaction    : Byte                ;  { dBase IV trans in progress. }
      Encryption     : Byte                ;  { Is data file encrypted?     }
      MultUseFlg     : Longint             ;
      UserIDLast     : Longint             ;
      ReservedLAN    : Array[0..3] of Byte ;  { ???? }

      ProductionMDX  : Byte ;  { Table Flags           }
                               {  1 = production index }
                               {  2 = has memos (VFP)  }
                               {  4 = is a DBC (VFP)   }
      LanguageDriver : Byte ;
      Reserved2      : Array[1..2] of Byte ;
    End ;

  { Language driver codes                }
  { 01h  DOS USA          code page 437  }
  { 02h  DOS Multilingual code page 850  }
  { 03h  Windows ANSI     code page 1251 }
  { C8h  Windows EE       code page 1250 }
  { 64h  EE MS-DOS        code page 852  }
  { 66h  Russian MS-DOS   code page 866  }
  { 65h  Nordic MS-DOS    code page 865  }

  BinaryMemoHeaderType = Packed
    Record
      MemoType : Integer ;

      Case Integer of
        1 : (MemoLengthBMP : Word    ;
             xUnknown2     : Array[1..10] of Byte ;
             Marker        : Array[1..2]  of Char ;
             BMPLength     : Word                  ) ;

        2 : (MemoLengthBinary : Integer ;
             xUnknown3        : Array[1..12] of Byte ) ;

        3 : (OleLengthBinary : Integer ;
             xUnknown4       : Array[1..10] of Byte ;
             xUnknown5       : Array[1..10] of Byte ;

             OleFoxPro       : Array[1..98] of Byte ;
             OleMarker       : Array[1..2]  of Char ;    //  dynamic
             OleLength       : Word                  ) ; //  dynamic
    End ;


  { Different types of memos, each with a slightly }
  { default structure.                             }
  dbfMemoType = (dbfDBTMemo , dbfDB4Memo , dbfFPTMemo , dbfDBVMemo) ;

  pdbfMemoHeader = ^dbfMemoHeaderType ;
  dbfMemoHeaderType = Packed
    Record
      Case Integer of
        0 : (dbfMemoHeaderBuffer : Array[0..511] of Byte) ;

        1 :  { DBT memo file }
         (dbfBlockCountDBT  : Integer   ;  { Number of 512 byte blocks }
                                           { in memo file.             }
          xxReserved00      : Array[0..11] of Byte ;
          dbfMemoVersionDBT : Byte ;
          dbfFillerDBT      : Array[0..494] of Byte) ;

        2 :  { DB4 memo file }
          (dbfBlockCountDB4   : Integer ;
           dbfBlockSizeResDB4 : Integer ;
           dbfFileNameDB4     : Array[0..7] of Char ;
           xxReserved1        : Byte ;
           xxReserved2        : Array[0..2] of Byte ;
           dbfBlockLenDB4     : Word ;
           dbReserved3        : Array[0..489] of Byte) ;

        3 :  { FPT memo file }
          (dbfBlockCountFPT : Integer ;
           xxUnused0405     : Array[0..1] of Byte ;
           dbfBlockLenFPT   : Word ;
           dbfFillerFPT     : Array[0..503] of Byte) ;
    End ;

Const
  dbfMemoHeaderSize : Integer = SizeOf(dbfMemoHeaderType) ;

  dbfDefaultBlockLenDB3 = 512 ;
  dbfDefaultBlockLenDB4 = 512 ;
  dbfDefaultBlockLenFPT =  64 ;

  dbfDefaultMemoHeaderSizeDB3 = 512 ;
  dbfDefaultMemoHeaderSizeDB4 = 512 ;
  dbfDefaultMemoHeaderSizeFPT =  64 ;

  dbMinDataFileSize = SizeOf(xBaseFixedHeaderType) +
                      SizeOf(dbfFieldDescType)     +
                      1                              ;


Type
  dbfFieldOffsetType = Array[0..dbfMaxFields] of Integer ;
  dbIgnoreFieldType = Array[0..dbfMaxFields] of Boolean ;

  { Record descriptor structure. }
  dbfRecordDescPtr  = ^dbfRecordDescType ;
  dbfRecordDescType = Array[1..dbfMaxFields] of dbfFieldDescType ;

  { Warning: Do NOT change the structure of the xBaseHeaderType structure. }
  pxBaseHeader  = ^xBaseHeaderType ;
  xBaseHeaderType  = Packed
    Record
      dbfFixedHeader   : xBaseFixedHeaderType ;

      dbfRecordDesc    : dbfRecordDescType                  ;
      FieldTerminator  : Byte                              ;  { Set to Null $0D }
      dbfBackLink      : Array[1..dbfBackLinkSize] of Char ;
    End ;

  pdbfMultiType = ^dbfMultiType ;
  dbfMultiType = Packed
    Record
      dbfMultiRecordPtr   : Pointer ;
      dbfMultiStartRecord : Integer ;
      dbfMultiRecordCnt   : Integer ;
      dbfMultiMaxRecords  : Integer ;
      dbfMultiBufferSize  : Integer ;
      dbfMultiFirstRec    : Integer ;
      dbfMultiLastRec     : Integer ;
      dbfExcessBytesRead  : Integer ;
    End ;

  pdbfDataArea   = ^dbfDataAreaType ;
  dbfDataAreaPtr = ^dbfDataAreaType ;
  dbfDataAreaType = Packed
    Record
      dbfUserIndex : Integer ;

      dbfFileName     : TFileName ;
      dbfMemoFileName : TFileName ;

      dbfShortDataFileName : TFileName ;

      dbfFileVar      : TFileStreamX ;
      dbfMemoFileVar  : TFileStreamX ;

      dbfMemoFormat   : dbfMemoType ;
      dbfMemoFileExt  : String[3]   ;

      dbfMemoIntFields  : TIntegerList    ;

      dbfFileSize     : Integer ;
      dbfMemoFileSize : Integer ;

      dbfGoodRecList : TStringList ;
      dbfBadRecList  : TStringList ;
      dbfBadNameList : TStringList ;

      dbfLastUpdate : TDateTime ;

      bBadFieldRec  : Array[0..dbfMaxFields] of Boolean      ;
      dbfFieldLists : Array[0..dbfMaxFields] of TIntegerList ;

      dbfRestructureList : TStringList ;

      dbfFile_EOF    : Boolean ;  { True if at end of file. }
      dbfIO          : Integer ;

      dbfVersion    : Byte    ;
      dbfSQLTable ,
      dbfMemoFile   : Boolean ;

      dbfHeader          : xBaseHeaderType   ;
      dbfMemoHeader      : dbfMemoHeaderType ;
      dbfMemoHeaderBytes : Integer           ;

      dbfOpened          : Boolean ;
      dbfMemoOpened      : Boolean ;

      dbfFieldCount      : Integer ;
      dbfFieldMaxWidth   : Integer ;
      dbfFieldOffset     : dbfFieldOffsetType ;
      dbfIgnoreField     : dbIgnoreFieldType ;
      dbfMemoDateTimeSet : Boolean ;
      dbfNameMaxWidth    : Integer ;
      dbfNameMaxWidthFld : Integer ;

      dbfBadDeletes      : Integer         ;
      dbfBadFieldList    : aFldCntType     ;
      dbfBadFields       : Integer         ;
      dbfBadRecords      : Integer         ;
      dbfFieldErrors     : FieldErrorsType ;

      dbfHeaderDirty  : Boolean ;
      dbfHeaderErrors : Array[0..500] of Integer ;

      dbfAllFieldTypes    : String  ;
      dbfCurrentRecord    : Integer ;  { Record number of record to display. }
      dbfCurrRecordBuffer : TRecordBuffer ;
      dbfCurrentRecordPtr : Pointer ;
      dbfDataRecordOffset : Integer ;
      dbfDelFlag          : Boolean ;
      dbfFieldChoice      : Integer ;
      dbfFieldToDisplay   : String  ;
      dbfVerifyFieldChar  : Char    ;

      dbfRecordBytesRead : Integer ;
{.PA}
      dbfMemoFldCount   : Integer ;
      dbfMemoFldList    : Array[1..dbfMaxFields] of Byte ;
      dbfMemoFldScanned : Boolean ;

      dbfMemoWidth        : Byte    ;
      dbfMemoFieldNo      : Integer ;
      dbfMemoBlockCount   : Integer ;
      dbfMemoBuffer       : Pointer ;
      dbfMemoLength       : Integer ;
      dbfBinaryMemoLength : Integer ;

      dbfMemoDesc : Packed
        Record
          nType  : Integer ;
          nWidth : Integer ;
        End ;

      dbfMemoDescDBV : TDBVStru ;

      dbfMemoBMPDesc : BinaryMemoHeaderType ;
      dbfMemoOleDesc : BinaryMemoHeaderType ;

      dbfBMP    : TBitMap ;
      dbfBMPPtr : Pointer ;
      dbfBMPLen : Integer ;

      dbfBMPHeight : Integer ;
      dbfBMPWidth  : Integer ;

      dbfOlePtr : Pointer ;
      dbfOleLen : Integer ;

      dbfBlobFile : String  ;

      dbfReadMulti  : dbfMultiType ;
      dbfWriteMulti : dbfMultiType ;

      dbfFieldStack : Array[0..dbfMaxFields] of Byte ;

      dbfNewFieldType  : Array[1..dbfMaxFields] of NewFieldType ;
      dbfNewFieldWidth : Array[1..dbfMaxFields] of Integer      ;
    End ;

  pTdbfOptions = ^TdbfOptions ;
  TdbfOptions = Packed
    Record
      NullsToBlanks      : Boolean ;
      TruncateAtNull     : Boolean ;
      UnprintableToBlank : Boolean ;

      dbvShowFieldHex    : Boolean ;
      dbvShowFieldOffset : Boolean ;
    End ;

  pTxBase = ^TxBase ;
  TxBase = Class(TComponent)
          Private
            dbfDataArea       : dbfDataAreaType ;
            dbfMemoFields     : TStringList     ;
            dbfMemoFieldTypes : TStringList     ;

            dbfOptions : TdbfOptions ;

            Procedure dbfInternalCalculations ;

            Function GetOleOffset : Integer ;
            Function GetOleLength : Integer ;

          Public
            Constructor Create(aOwner : TComponent) ; reintroduce ;
            Destructor Destroy ; override ;

            Procedure SetUserIndex(nIdx : Integer  ) ;
            Function  GetUserIndex : Integer ;

            Procedure Init ;
            Procedure InitFile(ifData  : String ;
                               ifMemo  : String  ) ;
            Procedure OpenCurrentFile ;
            Procedure CreateCurrentFile ;
            Procedure CloseFiles ;

            Function GetOptionsPtr : pTdbfOptions ;

            Procedure dbfOpenDataFileVar ;
            Procedure dbfCloseDataFileVar ;
            Function  dbfOpenMemoFile : Boolean ;
            Procedure dbfCloseMemoFileVar ;

            Function FieldCreateStr(nField : Integer) : String      ;
            Procedure FieldListCreate(pFldList : pTStringList) ;

            Procedure CreateDatabase(    cFileName  : String      ;
                                         nSignature : Integer     ;
                                     Var oFlds      : TStringList  ) ;

            Function  GetDataFileName : TFileName ;
            Function  GetShortDataFileName : TFileName ;
            Function  ResetShortFileName : TFileName ;
            Procedure SetFileName(Const cFileName : TFileName) ;
            Function  FlushDataFile : Boolean ;
            Function  FlushMemoFile : Boolean ;

            Function  GetBackLinkPtr : Pointer ;
            Function  GetBackLinkStr : String ;

            Function  IsDataFileOpen : Boolean ;
            Function  IsMemoFileOpen : Boolean ;

            Function  GetDataFileVar : pTFileStreamX ;
            Function  GetMemoFileVar : pTFileStreamX ;

            Function  GetDataAreaPtr : dbfDataAreaPtr ;
            Procedure dbfCheckDataAreaPtr ;

            Function  WriteHeader : Boolean ;
            Function  ReadHeader : Boolean ;
            Procedure dbfCheckHeader ;

            Function  GetHeaderSize : Integer ;
            Procedure SetHeaderSize(nHeaderSize : Word) ;
            Function  GetHeaderPtr : pxBaseHeader ;
            Function  GetFixedHeaderPtr : xBaseFixedHeaderPtr ;
            Function  GetMemoHeaderPtr : pdbfMemoHeader ;

            Function IsDBase3   : Boolean ;
            Function IsDBase4   : Boolean ;
            Function IsDBase5   : Boolean ;
            Function IsFlagShip : Boolean ;
            Function IsFlexFile : Boolean ;
            Function IsFoxPro   : Boolean ;

            Function IsFlexField(nField : Integer) : Boolean ;

            Function  GetFieldBytesAfter(cFldName : String) : Integer ;
            Function  GetFieldCount : Integer ;
            Function  GetFieldNo(cFldName : String) : Integer ;
            Function  GetFileTypeMsg : String ;
            Function  GetLastFileByte : Byte ;
            Function  GetLastUpdate(bShowError : Boolean) : TDateTime ;
            Function  GetLastUpdateEnglish : String ;
            Function  GetLastUpdateStr : String ;
            Function  GetSignature : Byte ;
            Function  GetTerminator : Byte ;
            Function  GetTerminatorPtr : Pointer ;
            Function  HasBackLink : Boolean ;
            Function  HasMemoFile : Boolean ;
            Procedure SetFileTerminator ;
            Procedure SetLastUpdate(sluDate : TDateTime) ;
            Procedure SetLastUpdateToday ;
            Procedure SetSignature(nSignature : Byte) ;
            Procedure SetTerminator ;

            Procedure SetLanguageDriver(nLanguage : Byte) ;
            Function  GetLanguageDriver : Byte ;
            Function  GetLanguageDriverDesc : String ;
            Function  GetCodePage : Integer ;
            Function  GetProductionMDX : Byte ;
            Procedure SetProductionMDX(nByte : Byte) ;

            Function GetRecordDescPtr : dbfRecordDescPtr ;
            Function GetFieldDescPtr(nField : Integer) : dbfFieldDescPtr ;

            Function PutFieldRecordPtr(pFld   : Pointer ;
                                       nRecNo : Integer ;
                                       nField : Integer  ) : Boolean ;

            Function  GetFieldIgnore(nField : Integer) : Boolean ;
            Procedure SetFieldIgnore(nField : Integer ;
                                     bIgnore : Boolean ) ;

            Function GetDateTimeField(pRec   : Pointer ;
                                      nField : Integer  ) : TDateTime ;
            Function GetDateTimeFieldEnglish(pRec   : Pointer ;
                                             nField : Integer  ) : String ;

            Function GetFieldOffset(nField : Integer) : Integer ;
            Function GetFieldLastByteOffset(nFieldNo : Integer) : Integer ;
            Function WithinFieldByOffset(nOffset : Integer) : Integer ;
            Function CheckFieldRead(nField : Integer) : Boolean ;

            Function GetWorkAreaId(nFieldNo : Integer) : Byte ;

            Function SetFieldCount(nFlds : Integer ) : Integer ;

            Function CalcHeaderSize  : Integer ;
            Function CalcRecordSize  : Integer ;
            Function CalcFileSize    : Integer ;
            Function GetDataFileSize : Integer ;

            Function  GetTotalRecords : Integer ;
            Function  GetRecordCount  : Integer ;  // alias for GetTotalRecords

            Procedure SetTotalRecords(nRecs : Integer) ;
            Procedure IncTotalRecords(nRecs : Integer) ;
            Function  CalcTotalRecords : Integer ;

            Function  GetNameMaxWidth    : Integer ;
            Function  GetNameMaxWidthFld : Integer ;
            Function  GetFieldMaxWidth   : Integer ;
            Procedure SetNameMaxWidth(nLen : Integer) ;

            Procedure dbfCalcAllFieldTypes ;
            Function  GetFieldName(nField : Integer) : String ;
            Function  GetFieldType(nField : Integer) : Char ;
            Function  IsFieldDecimalType(nField : Integer) : Boolean ;
            Function  CheckValidFieldType(nField : Integer) : Boolean ;
            Function  CheckFieldDecimalType(nField : Integer) : Boolean ;
            Function  GetFieldIdxType(nField : Integer) : Integer ;
            Function  GetAllFieldTypes : String ;
            Function  GetNoFieldTypes : Integer ;
            Function  GetFieldTypeName(nField : Integer) : String ;
            Function  GetFieldWidth(nField : Integer) : Integer ;
            Function  GetFieldNameWidth(cFldName : String) : Integer ;
            Function  GetFieldDecimals(nField : Integer) : Integer ;
            Procedure SetFieldDecimals(nField    : Integer ;
                                       nDecimals : Integer  ) ;
            Function  GetFieldIndexed(nField : Integer) : Boolean ;
            Procedure SetFieldIndexedByte(nField : Integer ;
                                          nByte  : Byte     ) ;

            Function FieldSpaces(Const nField : Word) : String ;

            Procedure SetAllFieldIndexBytes(nByte : Byte) ;
            Procedure RemoveIndexIndicators ;
            Function  GetFieldIndexedByte(nField : Integer) : Byte ;
            Function  GetFieldOfs(cFldName : String) : Integer ;
            Function  GetFieldPos(cFldName : String) : Integer ;

            Function  GetFieldValidSet(nField : Integer) : pCharSet ;

            Function  HasFieldType(cType : Char) : Boolean ;

            Function GetReservedMultiUserStr01(nField : Integer) : String ;
            Function GetReservedMultiUserStr02(nField : Integer) : String ;
            Function GetReservedUnknown(nField : Integer) : String ;

            Function  GetSetFieldsFlag(nField : Integer) : Byte ;

            Function GetFieldRecordOffset(nRecord : Integer ;
                                          nField  : Integer  ) : Integer ;

            Function FloatToFieldStr(nField : Word  ;
                                     nFloat : Float  ) : String ;
            Function FixFloatField(nField : Integer) : String ;

            { Set a field name with trailing 00's. }
            Procedure SetFieldName(cName  : String  ;
                                   nField : Integer  ) ;
            Function RenameField(cFieldName : String ;
                                 nFieldNo   : Integer ) : Boolean ;

            Function AddField(cName     : String  ;
                              cType     : Char    ;
                              nWidth    : Integer ;
                              nDecimals : Integer  ) : Integer ;

            Function  GetFieldErrors(cChar : Char) : Integer ;
            Function  GetFieldErrorsIdx(nIdx : Integer ) : Integer ;
            Function  GetTotalFieldErrors : Integer ;

            Function  IsFloatField(nField : Integer) : Boolean ;
            Function  IsIntegerField(nField : Integer) : Boolean ;

            Function  GetRecordNo : Integer ;
            Procedure SetRecordNo(nRecNo : Integer) ;
            Function  GetRecordSize : Integer ;
            Function  SetRecordSize(S : Word) : Word ;
            Function  GetRecordPtr : Pointer ;
            Procedure SetRecordPtr(pRecPtr : Pointer) ;
            Function  SetCurrentRecord(nRecNo : Integer) : Boolean ;

            Procedure BlankRecord(pRec : Pointer) ;
            Procedure ZapCurrentRecord ;
            Procedure BlankCurrentRecord ;

            Function  GetRecord(nRecNo : Integer) : Boolean ;
            Function  GetRecordBytesRead : Integer ;
            Procedure SetRecordBytesRead(nSize : Integer) ;
            Function  CheckRecordBytesRead : Boolean ;

            Function DeleteRecord(nRecNo : Integer) : Boolean ;
            Function UnDeleteRecord(nRecNo : Integer) : Boolean ;

            Function PutRecordPtr(pRec   : Pointer ;
                                  nRecNo : Integer  ) : Boolean ;
            Function PutRecord(nRecNo : Integer) : Boolean ;
            Function PutCurrentRecord : Boolean ;

            Function AddRecord(    pRec   : Pointer ;
                               Var nRecNo : Integer  ) : Boolean ;
            Function AddBlankRecord(Var RecNum : Integer) : Boolean ;

            Procedure InitMulti(Var rMulti : dbfMultiType) ;
            Function  GetMultiRecord(nFirstRec : Integer) : Boolean ;
            Function  PutMultiRecord(nFirstRec : Integer) : Boolean ;
            Function  AddMultiRecord(    pRec   : Pointer ;
                                     Var nRecNo : Integer  ) : Boolean ;
            Function  IsMultiWriteBufferFull : Boolean ;
            Function  AddMultiRecordBuffer : Boolean ;

            Function GetFldStr(nField : Integer) : String ;
            Function GetFieldHexStr(pRec   : Pointer ;
                                    nField : Integer  ) : String ;
            Function GetCurrFieldHexStr(nField : Integer) : String ;
            Function GetCleanFieldStr(pRec   : Pointer ;
                                      nField : Integer  ) : String ;
            Function GetFieldInteger(pRec   : Pointer ;
                                     nField : Integer  ) : Integer ;
            Function GetFldInteger(nField : Integer) : Integer ;
            Function GetFieldSmallInt(pRec   : Pointer ;
                                      nField : Integer  ) : Integer ;
            Function GetFieldStr(pRec   : Pointer ;
                                 nField : Integer ;
                                 bClean : Boolean  ) : String ;
            Function GetDeleteField(pRec : Pointer) : Char ;

            Function GetFieldDisplayString(pRec   : Pointer ;
                                           nField : Integer ;
                                           bHex   : Boolean  ) : String ;
            Function GetFieldPtr(pRec   : Pointer ;
                                 nField : Integer  ) : Pointer ;
            Function GetCurrentFieldPtr(nField : Integer) : Pointer ;
            Function GetBufferFieldStr(pRec   : Pointer ;
                                       nField : Integer  ) : String ;

            Function GetFieldDate(nField : Integer) : TDateTime ;
            Function GetFldDate(nField : Integer) : TDateTime ;

            Procedure SetFieldStr(pRec   : Pointer ;
                                  cField : String  ;
                                  nField : Integer  ) ;
            Procedure SetFldStr(cField : String  ;
                                nField : Integer  ) ;
            Procedure ZapField(pRec   : Pointer ;
                               nField : Integer  ) ;
            Procedure SetFieldDefault(pRec   : Pointer ;
                                      nField : Integer  ) ;
            Procedure BlankField(pRec   : Pointer ;
                                 nField : Integer  ) ;
            Procedure SetFieldInteger(pRec     : Pointer ;
                                      nInteger : Integer ;
                                      nField   : Integer  ) ;
            Procedure SetFieldSmallInt(pRec      : Pointer  ;
                                       nSmallInt : SmallInt ;
                                       nField    : Integer   ) ;
            Procedure SetFieldByte(pRec   : Pointer ;
                                   nByte  : Integer ;
                                   nField : Integer  ) ;
            Procedure SetFieldLogical(pRec     : Pointer ;
                                      bBoolean : Boolean ;
                                      nField   : Integer  ) ;
            Procedure SetFieldReal48(pRec    : Pointer ;
                                     nReal48 : Real48  ;
                                     nField  : Integer  ) ;
            Procedure SetFieldFloat(pRec    : Pointer ;
                                    nFloat  : Float   ;
                                    nField  : Integer  ) ;
            Function GetFieldFloat(    pRec   : Pointer ;
                                       nField : Integer ;
                                   Var bOkay  : Boolean  ) : Float ;

            Function GetNumericFieldMask(nField : Integer) : String ;

            Function GetColumnFlags(nField : Integer) : Byte ;
            Procedure SetColumnFlags(nField : Integer ;
                                     nFlags : Byte     ) ;

            Function DisplayFldStr : String ;
            Function GetFieldChoice : Integer ;
            Procedure SetFieldChoice(nField : Integer) ;

            Function SetMemoField(pRec  : Pointer ;
                                  nMemo  : Integer ;
                                  nField : Integer  ) : Boolean ;
            Function SetCurrentMemoField(nMemo  : Integer ;
                                         nField : Integer  ) : Boolean ;

            Function  IsHeaderDirty : Boolean ;
            Procedure SetHeaderDirty(bDirty : Boolean) ;

            procedure ZapDataArea ;
            Procedure ZapStructures ;
            Procedure dbfZapBadFieldStats ;
            Function  CreateFiles(bCreateHeader : Boolean ;
                                  nSignature    : Byte     ) : Boolean ;

            Procedure dbfAllocateInternalRecord ;
            Function  dbfCreateMemoFile(MName : String) : Boolean ;
            Procedure dbfCalcFieldCount ;
            Procedure dbfCalcFieldOffsets ;
            Procedure dbfResetFieldOffsets ;
            Function  dbDuplicateField(cFieldName : String ;
                                       nFieldNo   : Integer ) : Boolean ;

            Function CalcRecordOffset(nRecNo : Integer) : Integer ;

            Function  dbfCorrectedHeaderSize : Integer ;
            Procedure dbfCalcRecordOffset ;
            Function  dbGetRecordOffset : Integer ;
            Procedure dbfConvertHeader ;
            Procedure dbfBuildMemoFldList ;
            Procedure dbfSetMemoFile ;
            Function  dbfCalcMemoOffset(nMemoNumber : Integer) : Integer ;

            Procedure dbfSetUpMultiRecord(Var rMulti : dbfMultiType) ;

            Procedure dbfReadFixedMemo(   pRec        : Pointer ;
                                          nField      : Integer ;
                                          pMemoBuffer : Pointer ;
                                       Var nMemoBytes  : Integer  ) ;

            Function  GetMemoNumber(pRec   : Pointer ;
                                    nField : Integer  ) : Integer ;
            Function  GetCurrentMemoNumber(nField : Integer) : Integer ;
            Function  SeekMemo(nMemo : Integer) : Boolean ;
            Function  GetMemoDescriptor(nMemo : Integer) : Integer ;
            Function  GetMemoLength : Integer ;
            Function  IsMemoFieldValid(pRec   : Pointer ;
                                       nField : Integer  ) : Boolean ;

            Function  GetMemoBytesRead : Integer ;
            Procedure SetMemoBytesRead(nBytesRead : Integer) ;

            Function GetOleMemoDescriptor(nMemo : Integer) : Integer ;

            Procedure dbfReadFixedMemoNo(    rfmnMemoNo : Integer ;
                                            MemoBuffer : Pointer ;
                                        Var MemoBytes  : Integer  ) ;

            Function GetMemoBufferPtr(nField : Integer) : Pointer ;
            Function GetMemoBufferLen(nField : Integer) : Integer ;
            Procedure FillMemoBuffer(nField : Integer ;
                                     nFill  : Byte     ) ;
            Function GetMemoFieldsPtr : pTStringList ;
            Function GetMemoFieldTypesPtr : pTStringList ;

            Function IsValidDate(cDate : String) : Boolean ;
            Function dbfIsBlankDate(pRec   : Pointer ;
                                    nField : Integer  ) : Boolean ;

            Function TestField(cField : String ;
                               nField : Integer ) : Boolean ;

            Function TestFieldNo(pRec   : Pointer ;
                                 nField : Integer  ) : Boolean ;

            Function VerifyField(pRec   : Pointer ;
                                 nField : Integer  ) : Boolean ; 
            Function FixField(pRec   : Pointer ;
                              nField : Integer  ) : Boolean ;
            Function CheckVerifyField(pRec   : Pointer ;
                                      nField : Integer  ) : Boolean ; 
            Function CheckVerifyFieldChar(pRec   : Pointer ;
                                          nField : Integer  ) : Char ;
            Function GetVerifyFieldChar : Char ;

            Function VerifyRecord(pRec : Pointer) : Boolean ;

            Procedure VerifyFile(pProc    : ProcType ;
                                 nMaxRec  : Integer  ;
                                 xFormPtr : Pointer   ) ;

            Function VerifyHeaderField(nField : Integer) : Boolean ;

            Procedure Duplicate(DB    : pTxBase ;
                                FName : String    ;
                                MName : String     ) ;

            Procedure CloneFileHeader(cFile : String) ;

            Function DataFileIsOpen : Boolean ;
            Function MemoFileIsOpen : Boolean ;
            Function GetDbfPtr : pTxBase ;

            Procedure SetMemoFileName(F : String) ;
            Function  GetMemoFieldCount : Word ;
            Function  GetMemoFileName : String ;
            Function  ReadMemoHeader : Boolean ;
            Procedure dbfSetMemoVars ;
            Function  WriteMemoHeader : Boolean ;
            Function  GetMemoHeaderSize : Integer ;
            Function  GetMemoSize(pPtr : Pointer) : Integer ;
            Function  GetMemoOffset(nMemo : Integer) : Integer ;
            Function  GetMemoOffsetDBV(pRec   : Pointer ;
                                       nField : Integer  ) : Integer ;

            Function  GetMemoBlockType : dbfMemoType ;
            Procedure SetMemoBlockType(mType : dbfMemoType) ;
            Function  GetMemoBlockLen : Integer ;
            Procedure SetMemoBlockLen(nSize : Word) ;

            Function  ReadMemo(nMemoNo  : Integer ;
                               nFieldNo : Integer  ) : Integer ;
            Procedure ReadMemoBMP(nMemoNo : Integer) ;
            Function  ReadMemoObject(nMemoNo : Integer) : String ;

            Function  ReadMemoFPT(nMemoNo  : Integer ;
                                  nFieldNo : Integer  ) : Integer ;

            Function  ReadMemoDB4(nMemoNo  : Integer ;
                                  nFieldNo : Integer  ) : Integer ;

            Function  ReadMemoDBV(nFieldNo : Integer) : Boolean ;
            Function  ReadMemoHeaderDBV(nFieldNo : Integer) : Boolean ;
            Function  ReadMemoBinary(nMemo : Integer) : String ;

            Function  GetMemoBMPPtr : BitMapPtr ;
            Function  GetMemoBMPLength : Integer ;
            Function  GetBlobFileName : String ;

            Function  GetMemoFileExtension : String ;
            Procedure SetMemoFileExtension ;
            Procedure SetMemoBlockCount(nBlocks : Integer) ; 
            Procedure UpdateMemoBlockCount ;
            Function  CalcMemoFileBlocks : Integer ;
            Function  GetMemoFileSize : Integer ;

            Function GetDataFilePtr : pTFileStreamX ;
            Function GetMemoFilePtr : pTFileStreamX ;

            Function GetDataFileHandle : Integer ;
            Function GetMemoFileHandle : Integer ;

            Function  GetMemoFileBlocks : Integer ;
            Function  GetMemoBlockCount : Integer ;
            Function  GetMemoFldNo(gmnI : Byte) : Integer ;
            Function  GetMemoFldIdx(mfiM : Byte) : Integer ;
            Function  GetNextMemoFld(gnmfM : Byte) : Integer ;

            Function AddMemo(    pBuffer    : Pointer ;
                                 nLen       : Integer ;
                             Var nMemoBlock : Integer  ) : Boolean ;
            Function AppendMemo(pMemoBuffer : Pointer ;
                                nMemoSize   : Integer  ) : Integer ;

            Procedure WriteFixedMemoNo(wfmnMemoNo     : Integer ;
                                       wfmnMemoBuffer : Pointer ;
                                       wfmnMemoBytes  : Integer  ) ;

            Procedure ScanMemoFields ;
            Procedure ZapCounts ;

            Function dbValidateFieldName(nFieldNo : Integer) : Boolean ;
            Function dbRepairFieldName(cFldStr : String) : Boolean ;

            Procedure SetMemoDateTime(smdtB : Boolean) ;

            Function ValidateHeader : Boolean ;
            Function RepairHeader   : Boolean ;
            Function ValidFieldNumber(nFieldNo : Integer) : Boolean ; 
            Function ValidRecordNumber(nRecNo : Integer) : Boolean ;

            Function dbfCalcNewFieldType(    nField   : Integer      ;
                                         Var NewFType : NewFieldType ;
                                         Var NewWidth : Integer       ) : Boolean ;
            Function GetNewFieldType(nField : Integer) : NewFieldType ;
            Function GetNewFieldWidth(nField : Integer) : Integer ;

            Procedure CleanFixedHeader ;

            Function InternalFieldDesc(nField : Integer) : String ;

            Function GetBadFieldCnt  : Integer        ;
            Function GetBadFieldsPtr : pTArrayInteger ;

            Function GetBadNameListPtr : pTStringList  ;
            Function GetBadNameListCnt : Integer         ;
            
            Function GetBadListPtr     : pTStringList  ;
            Function GetGoodListPtr    : pTStringList  ;
            Function GetBadRecordCnt   : Integer         ;

            Procedure ResetFieldRecNoLists ;
            Procedure AllocateFieldList(nField : Integer) ;
            Procedure AddRecNoToFieldList(nField : Integer ;
                                          nRec   : Integer  ) ;

            Function GetBadFieldNo(nIdx : Integer) : Integer ; 

            Procedure dbfSetupRestructure ;
            Function  GetRestructureListPtr : pTStringList ;

            Function AsComp(pRec   : Pointer ;
                            nField : Integer  ) : Comp ;
            Function AsCurrency(pRec   : Pointer ;
                                nField : Integer  ) : Currency ;
            Function AsDouble(pRec   : Pointer ;
                              nField : Integer  ) : Double ;
            Function AsInteger(pRec   : Pointer ;
                               nField : Integer  ) : Integer ;
            Function AsWord(pRec   : Pointer ;
                            nField : Integer  ) : Word ;
            Function AsByte(pRec   : Pointer ;
                            nField : Integer  ) : Byte ;
            Function AsShortInt(pRec   : Pointer ;
                                nField : Integer  ) : ShortInt ;
            Function AsHexStr(pRec   : Pointer ;
                              nField : Integer  ) : String ;

            Function FldAsInteger(nField : Integer) : Integer ;
            Function FldAsBoolean(nField : Integer) : Boolean ;
            Function FldAsDouble(nField : Integer) : Double ;

            Procedure FixCharacterField(    pRec   : Pointer ;
                                            nField : Integer ;
                                        Var bFixed : Boolean  ) ;

            Function IsAtBOF(nRec : Integer) : Boolean ;
            Function IsAtEOF(nRec : Integer) : Boolean ;
          End ;

  { External functions/procedures pgms may need. }
  Function GetMinDataFileSize : Integer ;
  Function GetFieldTypeIdx(cName : String) : Integer ;
  Function GetFieldTypeChar(nI : Integer) : Char ;
  Function GetFieldTypeCharFromName(cName : String) : Char ;
  Function GetTypeNameFromType(cType : Char) : String ;

  Function GetVersionStr : String ;

Implementation
  Uses
    dbfResources ;
    
{***********************************************************************
*                                                                      *
*       CheckField                                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function CheckField(cType     : Char    ;
                    nWidth    : Integer ;
                    nDecimals : Integer  ) : Boolean ;
  Var
    cMsg : String  ;
    nIdx : Integer ;

  Begin  { CheckField }
    Result := True ;
    cMsg := '' ;

    If not (cType in AllFieldTypes) then
      Begin
        cMsg := cMsg + 'Invalid field type [' + cType + ']' ;
        Result := False ;
      End ;

    nIdx := PosStr(cType , AllFields) ;
    If (nWidth < MinMaxField[nIdx , 1]) or
       (nWidth > MinMaxField[nIdx , 2])    then
      Begin
        cMsg := cMsg + 'Invalid field width [' + IntToStr(nWidth) + ']' ;
        Result := False ;
      End ;
  End ;  { CheckField }



{***********************************************************************
*                                                                      *
*       TxBase.CheckFieldDecimalType                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CheckFieldDecimalType(nField : Integer) : Boolean ;
  Var
    nDecimals : Integer ;

  Begin  { TxBase.CheckFieldDecimalType }
    nDecimals := GetFieldDecimals(nField) ;

    If IsFieldDecimalType(nField) then
      Result := ((nDecimals >= 0) and
                 (nDecimals < GetFieldWidth(nField)))
    Else
      Result := (nDecimals = 0) ;
  End ;  { TxBase.CheckFieldDecimalType }



{***********************************************************************
*                                                                      *
*       TxBase.CheckValidFieldType                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CheckValidFieldType(nField : Integer) : Boolean ;
  Var
    scheck : CharSet ;

  {
  DB3_FldTypes : CharSet = ['C' , 'D' , 'L' , 'M' , 'N'] ;
  DB4_FldTypes : CharSet = ['C' , 'D' , 'F' , 'L' , 'M' , 'N'] ;
  DB5_FldTypes : CharSet = ['B' , 'C' , 'D' , 'F' , 'G' , 'L' , 'M' , 'N'] ;
  FXP_FldTypes : CharSet = ['B' , 'C' , 'D' , 'F' , 'G' , 'L' , 'M' , 'N' , 'P'] ;
  VDB_FldTypes : CharSet = ['B' , 'C' , 'D' , 'F' , 'G' , 'L' , 'M' , 'N' , 'P'] ;
  FLG_FldTypes : CharSet = ['2' , '4' , '8' , 'C' , 'D' , 'F' , 'L' , 'M' , 'N' , 'V'] ;
 }

  Begin  { TxBase.CheckValidFieldType }
    If IsDBase3 then
      sCheck := DB3_FldTypes
    Else

    If IsDBase4 then
      sCheck := DB4_FldTypes
    Else

    If IsDBase5 then
      sCheck := DB5_FldTypes
    Else

    If IsFoxPro then
      sCheck := FXP_FldTypes
    Else

    If IsFlagShip then
      sCheck := FXP_FldTypes
    Else
      Begin
        ShowErrorMessage('Invalid file type in TxBase.CheckValidFieldType') ;
        sCheck := EmptySet ;
      End ;

    If sCheck = EmptySet then
      Result := False
    Else
      Result := (GetFieldType(nField) in sCheck) ;
  End ;  { TxBase.CheckValidFieldType }


{***********************************************************************
*                                                                      *
*       TxBase.IsFieldDecimalType                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsFieldDecimalType(nField : Integer) : Boolean ;
  Begin  { TxBase.IsFieldDecimalType }
    Result := (GetFieldType(nField) in DecimalFieldTypes) ;
  End ;  { TxBase.IsFieldDecimalType }


{***********************************************************************
*                                                                      *
*       TxBase.VerifyHeaderField                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.VerifyHeaderField(nField : Integer) : Boolean ;
  Var
    nIdx      : Integer ;
    cName     : String  ;
    cType     : Char    ;
    nWidth    : Integer ;
    nDecimals : Integer ;

  Begin  { TxBase.VerifyHeaderField }
    Result := True ;

    cName     := GetFieldName(nField)     ;
    cType     := GetFieldType(nField)     ;
    nWidth    := GetFieldWidth(nField)    ;
    nDecimals := GetFieldDecimals(nField) ;

    If not (cType in AllFieldTypes) then
      Begin
        Result := False ;
        Exit ;
      End ;

    If (nDecimals < 0) then
      Begin
        Result := False ;
        Exit ;
      End ;

    If (not CheckFieldDecimalType(nField)) then
      Begin
        Result := False ;
        Exit ;
      End ;

    nIdx := PosStr(cType , AllFields) ;
    If (nWidth < MinMaxField[nIdx , 1]) or
       (nWidth > MinMaxField[nIdx , 2])    then
      Begin
        Result := False ;
      End ;
  End ;  { TxBase.VerifyHeaderField }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFileExtension                                    *
*                                                                      *
*         Return a 3 character memo file extension based on the data   *
*       file signature.                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFileExtension : String ;
  Begin  { TxBase.GetMemoFileExtension }
    Case GetSignature of
      $02 , { FoxBASE , dBase II }
      $83 , { FoxBASE+/dBASE III PLUS, with memo }

      $04 , { dBASE IV or IV w/o memo file }
      $43 , { dBASE IV SQL table files, no memo }
      $63 , { dBASE IV SQL system files, no memo }
      $7B , { dBASE IV with memo           }
      $8B , { dBASE IV w. memo             }
      $8E , { dBASE IV w. SQL table        }
      $CB , { dBASE IV SQL table files, with memo }

      $05   { dBASE V w/o memo file        }
          : Result := 'DBT' ;

      $03 : If IsFlexFile then
              Result := 'DBV'
            Else
              Result := 'DBT' ;  { FoxBASE+/dBASE III PLUS, no memo }

      $30 , { Visual FoxPro w. DBC         }
      $F5 , { FoxPro 2.x (or earlier) with memo }
      $FB   { FoxBASE }
          : Result := 'FPT' ;
    Else
      Result := 'DBT' ;
    End ;
  End ;  { TxBase.GetMemoFileExtension }


{***********************************************************************
*                                                                      *
*       TxBase.SetMemoFileExtension                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetMemoFileExtension ;
  Begin  { TxBase.SetMemoFileExtension }
    With GetDataAreaPtr^ do
     dbfMemoFileExt := GetMemoFileExtension ;
  End ;  { TxBase.SetMemoFileExtension }


{***********************************************************************
*                                                                      *
*       TxBase.GetFileTypeMsg                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFileTypeMsg : String ;
  Begin  { TxBase.GetFileTypeMsg }
    Case GetSignature of
      $02 : Result := 'FoxBASE , dBase II'                  ;
      $03 : Result := 'FoxBASE+/dBASE III PLUS, no memo'    ;
      $04 : Result := 'dBASE IV or IV w/o memo file'        ;
      $05 : Result := 'dBASE V w/o memo file'               ;
      $30 : Result := 'Visual FoxPro w. DBC'                ;
      $43 : Result := 'dBASE IV SQL table files, no memo'   ;
      $63 : Result := 'dBASE IV SQL system files, no memo'  ;
      $7B : Result := 'dBASE IV with memo'                  ;
      $83 : Result := 'FoxBASE+/dBASE III PLUS, with memo'  ;
      $8B : Result := 'dBASE IV w. memo'                    ;
      $8E : Result := 'dBASE IV w. SQL table'               ;
      $CB : Result := 'dBASE IV SQL table files, with memo' ;
      $F5 : Result := 'FoxPro 2.x (or earlier) with memo'   ;
      $FB : Result := 'FoxBASE'                             ;
    Else
      Result := 'Unknown xBase file type' ;
    End ;
  End ;  { TxBase.GetFileTypeMsg }


{***********************************************************************
*                                                                      *
*       TxBase.IsFlexFile                                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsFlexFile : Boolean ;
  Begin  { TxBase.IsFlexFile }
    Case GetSignature of
      $03 : Result := bcFileUtilities.FileExists(ForceExtension(GetDataFileName , 'DBV')) ;
    Else
      Result := False ;
    End ;
  End ;  { TxBase.IsFlexFile }


{***********************************************************************
*                                                                      *
*       TxBase.IsFlexField                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsFlexField(nField : Integer) : Boolean ;
  Begin  { TxBase.IsFlexField }
    Result := ((GetFieldType(nField)  = 'C') and
               (GetFieldWidth(nField) = 6  ) and
               IsFlexFile                        ) ;
  End ;  { TxBase.IsFlexField }


{***********************************************************************
*                                                                      *
*       TxBase.IsFoxPro                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsFoxPro : Boolean ;
  Begin  { TxBase.IsFoxPro }
    Case GetSignature of
      $30 ,
      $F5 ,
      $FB   : Result := True ;
    Else
      Result := HasFieldType('P') ;
    End ;
  End ;  { TxBase.IsFoxPro }


{***********************************************************************
*                                                                      *
*       TxBase.IsDBase5                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsDBase5: Boolean ;
  Begin  { TxBase.IsDBase5 }
    Case GetSignature of
      $04 ,
      $7B ,
      $8B ,
      $8E ,
      $05   : Result := True ;
    Else
      Result := HasFieldType('B') or
                HasFieldType('G')    ;
    End ;
  End ;  { TxBase.IsDBase5 }


{***********************************************************************
*                                                                      *
*       TxBase.IsDBase4                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsDBase4 : Boolean ;
  Begin  { TxBase.IsDBase4 }
    Case GetSignature of
      $04 ,
      $7B ,
      $8B ,
      $8E ,
      $05   : Result := True ;
    Else
      Result := False ;
    End ;
  End ;  { TxBase.IsDBase4 }


{***********************************************************************
*                                                                      *
*       TxBase.IsDBase3                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsDBase3 : Boolean ;
  Begin  { TxBase.IsDBase3 }
    Case GetSignature of
      $03 ,
      $83   : Result := True ;
    Else
      Result := False ;
    End ;
  End ;  { TxBase.IsDBase3 }


{***********************************************************************
*                                                                      *
*       TxBase.IsFlagShip                                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsFlagShip : Boolean ;
  Begin  { TxBase.IsFlagShip }
    Case GetSignature of
      $B3 : Result := True ;
    Else
      { Only flagship has the 2 , 4 , 8 and V field types }
      Result := HasFieldType('2') or
                HasFieldType('4') or
                HasFieldType('8') or
                HasFieldType('V')    ;
    End ;
  End ;  { TxBase.IsFlagShip }


{***********************************************************************
*                                                                      *
*       FixMemoSize                                                    *
*                                                                      *
*         Return a True if all characters in field name are valid.     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure FixMemoSize(    fmsMemoPtr : Pointer ;
                      Var fmsSize    : Integer  ) ;
  Var
    nI : Integer ;

    fmsPtr : pAsciiZ absolute fmsMemoPtr ;

  Begin  { FixMemoSize }
    nI := 0 ;
    
    Repeat
      Inc(nI) ;
    Until (fmsPtr^[nI] = #$00) or (fmsPtr^[nI] = #$1A) ;
    
    fmsPtr^[nI    ] := #$1A ;
    fmsPtr^[nI + 1] := #$1A ;

    fmsSize := nI + 2 ;
  End ;  { FixMemoSize }


{***********************************************************************
*                                                                      *
*       CreateMemoFile                                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function CreateMemoFile(    MName          : String       ;
                        Var cmfMemoFileVar : TFileStreamX  ) : Boolean ;
  Var
    cmfMemoHeader : dbfMemoHeaderType ;
    nBytesWritten : Integer ;

   Begin  { CreateMemoFile }
     MName := DefaultExtension(MName , 'DBT') ;

     Try
       cmfMemoFileVar := TFileStreamX.Create(MName , fmCreate) ;
     Except
       Begin
         MessageDlg('Cannot create memo file [' + MName + ']' , mtError, [mbOK], 0) ;
         cmfMemoFileVar := nil ;
         Result := False ;
         Exit ;
       End ;
     End ;

     If cmfMemoFileVar = nil then
       Result := False 
     Else
       Begin
         ZeroMemory(@cmfMemoHeader , SizeOf(cmfMemoHeader)) ;

         { Write the first (unused) memo block. }
         nBytesWritten := cmfMemoFileVar.Write(cmfMemoHeader , SizeOf(cmfMemoHeader)) ;

         Result := (nBytesWritten = SizeOf(cmfMemoHeader)) ;
       End ;
   End ;  { CreateMemoFile }


{***********************************************************************
*                                                                      *
*       ValidFieldName                                                 *
*                                                                      *
*         Return a True if all characters in field name are valid.     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ValidFieldName(vfnS : String) : Boolean ;
  Begin  { ValidFieldName }
    If Length(vfnS) > 0 then
      Result := CheckString(vfnS , LegalFieldNameChars)
    Else
      Result := False ;
  End ;  { ValidFieldName }


{***********************************************************************
*                                                                      *
*       TxBase.InitMulti                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.InitMulti(Var rMulti : dbfMultiType) ;
  Begin  { TxBase.InitMulti }
    With rMulti do
      Begin
        FillChar(rMulti , SizeOf(rMulti) , 0) ;

        dbfMultiRecordPtr   := Nil ;
        dbfMultiRecordCnt   := 0   ;
        dbfMultiStartRecord := 0   ;
        dbfMultiMaxRecords  := 0   ;
      End ;
  End ;  { TxBase.InitMulti }


{***********************************************************************
*                                                                      *
*       TxBase.Init                                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.Init ;
  Begin  { TxBase.Init }
    Inherited Create(nil) ;

    DateParInitialize ;

    ZapStructures ;

    With GetDataAreaPtr^ do
      Begin
        dbfOpened := False ;

        dbfMemoOpened := False ;
        dbfMemoWidth  := dbfDefaultMemoWidth ;

        dbfCurrentRecordPtr := nil ;

        dbfHeaderDirty := False ;

        dbfGoodRecList := TStringList.Create ;
        dbfBadRecList  := TStringList.Create ;
        dbfBadNameList := TStringList.Create ;

        dbfRestructureList := TStringList.Create ;

        dbfGoodRecList.Clear     ;
        dbfBadRecList.Clear      ;
        dbfBadNameList.Clear     ;
        dbfRestructureList.Clear ;

        dbfBMP := TBitMap.Create ;
        dbfBlobFile := BlobFileName ;

        With dbfOptions do
          Begin
            dbvShowFieldOffset := True  ;
            dbvShowFieldHex    := False ;
          End ;

        InitMulti(dbfReadMulti) ;
      End ;
  End ;  { TxBase.Init }


{***********************************************************************
*                                                                      *
*       TxBase.InitFile                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.InitFile(ifData  : String  ;
                          ifMemo  : String   ) ;
  Begin  { TxBase.InitFile }
    Init ;
    SetFileName(ifData) ;
    SetMemoFileName(ifMemo) ;
    OpenCurrentFile ;

    dbfInternalCalculations ;
  End ;  { TxBase.InitFile }


{***********************************************************************
*                                                                      *
*       TxBase.Create                                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Constructor TxBase.Create(aOwner : TComponent) ;
  Begin  { TxBase.Create }
    Inherited Create(aOwner) ;

    DateParInitialize ;
  End ;  { TxBase.Create }


{***********************************************************************
*                                                                      *
*       TxBase.Destroy                                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Destructor TxBase.Destroy ;
  Begin  { TxBase.Destroy }
    CloseFiles ;

    With GetDataAreaPtr^ do
      Begin
        Try
          FreeAndNil(dbfGoodRecList)     ;
          FreeAndNil(dbfBadRecList)      ;
          FreeAndNil(dbfBadNameList)     ;
          FreeAndNil(dbfRestructureList) ;
        Except
          On e: exception do
            ShowErrorMessage('Cannot destroy Good/Bad lists in ' +
                         'TxBase.Destroy'#13#10'Error message ' + e.Message);
        End ;

        If ((dbfMemoFldCount > 0) or IsFlexFile  or (dbfMemoBuffer <> nil)) then
          FreeMemCheck(dbfMemoBuffer , dbfMaxMemoSize) ;

        If dbfMemoFldScanned then
          Begin
            Try
              FreeAndNil(dbfMemoFields)     ;
              FreeAndNil(dbfMemoIntFields)  ;
              FreeAndNil(dbfMemoFieldTypes) ;
            Except
              On e: exception do
                ShowErrorMessage('Cannot destroy dbfMemoFields/dbfMemoFieldTypes in ' +
                                 'TxBase.Destroy'#13#10'Error message ' + e.Message);
            End ;
          End ;

        If dbfBMPPtr <> nil then
          FreeMemCheck(dbfBMPPtr , dbfBMPLen) ;
        FreeAndNil(dbfBMP) ;
      End ;

    ResetFieldRecNoLists ;

    Inherited Destroy ;
  End ;  { TxBase.Destroy }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.GetDataAreaPtr                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDataAreaPtr : dbfDataAreaPtr ;
  Begin  { TxBase.GetDataAreaPtr }
    Result := @dbfDataArea ;
  End ;  { TxBase.GetDataAreaPtr }


{***********************************************************************
*                                                                      *
*       TxBase.dbfCheckDataAreaPtr                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfCheckDataAreaPtr ;
  Begin  { TxBase.dbfCheckDataAreaPtr }
    If GetDataAreaPtr = nil then
      Raise Exception.Create(errMsgInvalidDataAreaPtr) ;
  End ;  { TxBase.dbfCheckDataAreaPtr }


{***********************************************************************
*                                                                      *
*       TxBase.ValidFieldNumber                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ValidFieldNumber(nFieldNo : Integer) : Boolean ;
  Begin  { TxBase.ValidFieldNumber }
    Result := ((nFieldNo >= 0) and (nFieldNo <= GetFieldCount)) ;
  End ;  { TxBase.ValidFieldNumber }


{***********************************************************************
*                                                                      *
*       TxBase.ValidRecordNumber                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ValidRecordNumber(nRecNo : Integer) : Boolean ;
  Begin  { TxBase.ValidRecordNumber }
    Result := (nRecNo >= 0) and (nRecNo <= GetTotalRecords) ;
  End ;  { TxBase.ValidRecordNumber }


{***********************************************************************
*                                                                      *
*       TxBase.TestField                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.TestField(cField : String ;
                          nField : Integer ) : Boolean ;
  Var
    nI        : Integer     ;
    oValidSet : Set of Char ;

  Begin  { TxBase.TestField }
    Result := False ;

    oValidSet := ValidSetArray[GetFieldIdxType(nField)] ;

    For nI := 1 to GetFieldWidth(nField) do
      If not (cField[nI] in oValidSet) then
        Exit ;

    Result := True ;
  End ;  { TxBase.TestField }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.TestFieldNo                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.TestFieldNo(pRec   : Pointer ;
                            nField : Integer  ) : Boolean ;
  Var
    nI        : Integer ;
    cField    : String  ;
    oValidSet : Set of Char ;

  Begin  { TxBase.TestFieldNo }
    Result := False ;

    oValidSet := ValidSetArray[GetFieldIdxType(nField)] ;

    cField := GetFieldStr(pRec , nField , False) ;
    For nI := 1 to GetFieldWidth(nField) do
      If not (cField[nI] in oValidSet) then
        Exit ;

    If (GetFieldType(nField) in ['N' , 'F']) then
      Begin
        If Trim(cField) = '.' then
          Result := False
        Else
          Try
            SysUtils.StrToFloat(cField) ;
            Result := True ;
          Except
            Result := False ;
          End ;
      End
    Else
      Result := True ;
  End ;  { TxBase.TestFieldNo }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldValidSet                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldValidSet(nField : Integer) : pCharSet ;
  Begin  { TxBase.GetFieldValidSet }
    Result := @(ValidSetArray[GetFieldIdxType(nField)]) ;
  End ;  { TxBase.GetFieldValidSet }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.VerifyField                                             *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*       2000-09-29  BKC  Blank numeric fields are valid.               *
*                                                                      *
***********************************************************************}

Function TxBase.VerifyField(pRec   : Pointer ;
                            nField : Integer  ) : Boolean ;
  Var
    cStr ,
    vfTrimString ,
    vfTString      : String  ;

    cType       : Char    ;
    vfFldIndex  : Integer ;
    nDecimals   : Integer ;
    nWidth      : Integer ;
    bBlankField : Boolean ;

    nMemoNo   : Integer ;
    nMemoSize : Integer ;

  Begin  { TxBase.VerifyField }
    {$IFDEF Debug}
    dbfCheckDataAreaPtr ;
    {$ENDIF}

    If pRec = nil then
      Begin
        ShowErrorMessage('Invalid record pointer in TxBase.VerifyField') ;
        Result := False ;
        Exit ;
      End ;

    Result := False ;

    With GetDataAreaPtr^ , GetHeaderPtr^ do
      Try
        cType        := GetFieldType(nField) ;
        vfFldIndex   := GetFieldIdxType(nField) ;
        vfTString    := GetFieldStr(pRec , nField , True) ;
        nDecimals    := GetFieldDecimals(nField) ;
        nWidth       := GetFieldWidth(nField) ;
        vfTrimString := Trim(vfTString) ;

        bBlankField := (Length(vfTrimString) = 0) ;

        If (not CheckFieldDecimalType(nField)) then
          Begin
            Inc(dbfFieldErrors[vfFldIndex]) ;
            Exit ;
          End ;
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.VerifyField                                             *
*                                                                      *
*       < continued >                                                  *
*                                                                      *
***********************************************************************}

        Case cType of
          'B' ,
          'G' ,
          'M'   : Case nWidth of
                     4 ,
                     8   : Begin
                           End ;

                    10 : Begin
                           If Length(Trim(vfTrimString)) > 0 then
                             Begin
                               If (not ValidNumStr(vfTrimString)) then
                                 Begin
                                   Inc(dbfFieldErrors[vfFldIndex]) ;
                                   Exit ;
                                 End ;

                               nMemoNo   := StrToInt(vfTrimString) ;
                               nMemoSize := ReadMemo(nMemoNo , nField) ;
                               If nMemoSize < 1 then
                                 Begin
                                   Inc(dbfFieldErrors[vfFldIndex]) ;
                                   Exit ;
                                 End ;
                             End ;
                         End ;
                  End ;

          'C' : Begin
                  If (nField = 0) then
                    Begin
                      If not (GetDeleteField(pRec) in ValidDelete) then
                        Begin
                          Exit ;
                        End ;
                    End ;
                End ;

          'D' : Begin
                  Case nWidth of
                    8 : Begin
                          cStr := Trim(GetFieldStr(pRec , nField , False)) ;
                          If (not IsValidDate(cStr)) then
                            Begin
                              Inc(dbfFieldErrors[vfFldIndex]) ;
                              Exit ;
                            End ;
                        End ;
                  Else
                    Inc(dbfFieldErrors[vfFldIndex]) ;
                    Exit ;
                  End ;
                End ;


          'F' : Begin
                  If (not bBlankField) then
                    If (not (vfTString[Length(vfTString)] in ['0'..'9'])) or
                       (not ValidDecStr(vfTrimString))                 or
                       ((nDecimals > 0) and (vfTString[nWidth - nDecimals] <> '.')) then
                      Begin
                        Inc(dbfFieldErrors[vfFldIndex]) ;
                        Exit ;
                      End ;
                End ;

          'I' : Begin
//                      nTemp := GetFieldInteger(pRec , nField) ;
                End ;

          'L' : Begin
                  If not (vfTString[1] in dbfValidLogical) then
                    Begin
                      Inc(dbfFieldErrors[vfFldIndex]) ;
                      Exit ;
                    End ;
                End ;

          'N' : Begin
                  If (not bBlankField) then
                    If (not (vfTString[Length(vfTString)] in ['0'..'9'])) or
                       (not ValidDecStr(vfTrimString))                 or
                       ((nDecimals > 0) and (vfTString[nWidth - nDecimals] <> '.')) then
                      Begin
                        Inc(dbfFieldErrors[vfFldIndex]) ;
                        Exit ;
                      End ;
                End ;

          'P' : Begin
                End ;

          'T' : Begin
                  Result := bcStringUtilities.ValidDate(GetDateTimeField(pRec , nField)) ;
                  Exit ;
                End ;

          'Y' : Begin
                End ;
        Else
          Begin
            Inc(dbfFieldErrors[0]) ;
            Exit ;
          End ;
        End ;  { Case FType of }

        Result := True ;

      Except
        On e: exception do
          Begin
            ShowErrorMessage('Exception in ' +
                         'TxBase.VerifyField'#13#10'Error message ' + e.Message);
            Result := False ;
          End ;
      End ;  { With GetDataAreaPtr^ , dbfHeader do }
  End ;  { TxBase.VerifyField }


{***********************************************************************
*                                                                      *
*       TxBase.CheckVerifyField                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CheckVerifyField(pRec   : Pointer ;
                                 nField : Integer  ) : Boolean ;
  Begin  { TxBase.CheckVerifyField }
    With GetDataAreaPtr^ do
      Try
        If CheckFieldRead(nField) then
          Begin
            If VerifyField(pRec , nField) then
              Begin
                dbfVerifyFieldChar := ' ' ;
                Result := True
              End
            Else
              Begin
                dbfVerifyFieldChar := '*' ;
                Result := False
              End ;
          End
        Else
          Begin
            dbfVerifyFieldChar := 'U' ;
            Result := False ;
          End ;
      Except
        Begin
          dbfVerifyFieldChar := 'U' ;
          Result := False ;
        End ;
      End ;
  End ;  { TxBase.CheckVerifyField }


{***********************************************************************
*                                                                      *
*       TxBase.CheckVerifyFieldChar                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CheckVerifyFieldChar(pRec   : Pointer ;
                                     nField : Integer  ) : Char ;
  Begin  { TxBase.CheckVerifyFieldChar }
    CheckVerifyField(pRec , nField) ;
    Result := GetVerifyFieldChar ;
  End ;  { TxBase.CheckVerifyFieldChar }


{***********************************************************************
*                                                                      *
*       TxBase.GetVerifyFieldChar                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetVerifyFieldChar : Char ;
  Begin  { TxBase.GetVerifyFieldChar }
    Result := GetDataAreaPtr^.dbfVerifyFieldChar ;
  End ;  { TxBase.GetVerifyFieldChar }


{***********************************************************************
*                                                                      *
*       TxBase.VerifyRecord                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*       1995-07-13  BKC  Use dbIgnoreFIeld for field testing.          *
*       1998-04-01  BKC  ValidDelete set used for validation.          *
*       1999-06-16  BKC  Use CheckVerifyField instead of VerifyField.  *
*                                                                      *
***********************************************************************}

Function TxBase.VerifyRecord(pRec : Pointer) : Boolean ;
  Var
    nField : Word ;
    cFldName : String ;

  Begin  { TxBase.VerifyRecord }
    Result := True ;

    With GetDataAreaPtr^ , GetHeaderPtr^ do
      Begin
        FillChar(bBadFieldRec , SizeOf(bBadFieldRec) , Byte(False)) ;

        { Check the delete byte. }
        If not GetFieldIgnore(0) then
          If not (GetDeleteField(pRec) in ValidDelete) then
            Begin
              Inc(dbfBadDeletes) ;
              Inc(dbfBadFieldList[0]) ;
              bBadFieldRec[0] := True ;
              Result := False ;
            End ;

        For nField := 1 to GetFieldCount do
          If not GetFieldIgnore(nField) then
            Begin
              cFldName := GetFieldName(nField) ;

              If not CheckVerifyField(pRec , nField) then
                Begin
                  dbfBadFields := dbfBadFields + 1 ;

                  dbfBadFieldList[nField] := dbfBadFieldList[nField] + 1 ;

                  If dbfBadNameList.IndexOf(cFldName) < 0 then
                    Begin
                      cFldName := Trim(CleanString(cFldName , ' ')) ;
                      If Length(cFldName) > 0 then
                        dbfBadNameList.Add(cFldName)
                      Else
                        ShowErrorMessage('Invalid field name [' + cFldName +
                                         ' in TxBase.VerifyRecord'          ) ;
                    End ;

                  Result := False ;

                  bBadFieldRec[nField] := True ;
                End ;
            End ;
      End ;  { With dbfDataArea.dbfHeader do }
  End ;  { TxBase.VerifyRecord }


{***********************************************************************
*                                                                      *
*      TxBase.VerifyFile                                               *
*                                                                      *
*      Modifications                                                   *
*      =============                                                   *
*                                                                      *
***********************************************************************}

Procedure TxBase.VerifyFile(pProc    : ProcType ;
                            nMaxRec  : Integer  ;
                            xFormPtr : Pointer   ) ;
  Var
    nRec        ,
    nMaxScanRec ,
    nPerc         : Integer ;

    nField         : Integer ;
    nFirstBadField : Integer ;
    nCheckField    : Integer ;
    cFldName       : String  ;

    cStr    : String ;

    Save_Cursor : TCursor ;

  Begin  { TxBase.VerifyFile }
    Save_Cursor   := Screen.Cursor ;
    Screen.Cursor := crHourglass   ;  { Show hourglass cursor }

    Try
      Try
        ZapCounts ;

        With GetDataAreaPtr^ do
          Begin
            dbfGoodRecList.Clear ;
            dbfBadRecList.Clear  ;
            dbfBadNameList.Clear ;

            If (nMaxRec <= 0) or (nMaxRec > GetTotalRecords) then
              nMaxScanRec := GetTotalRecords
            Else
              nMaxScanRec := nMaxRec ;

            For nRec := 1 to nMaxScanRec do
              Begin
                Application.ProcessMessages ;

                If nRec = GetTotalRecords then
                  nPerc := 100
                Else
                  nPerc := (100 * nRec) div GetTotalRecords ;

                If GetRecord(nRec) then
                  Begin
                    cStr := PadLeft(IntToStr(nRec) , 8) ;
                    If VerifyRecord(GetRecordPtr) then
                      dbfGoodRecList.Add(cStr)
                    Else
                      Begin
                        dbfBadRecList.Add(cStr) ;
                        dbfBadRecords := dbfBadRecords + 1 ;
                      End ;
                  End
                Else
                  Begin
                    If CheckRecordBytesRead then
                      ShowErrorMessage(ErrMsgUnknownRecordError)
                    Else
                      Begin
                        cStr := PadLeft(IntToStr(nRec) , 8) ;
                        If dbfBadRecList.IndexOf(cStr) <= 0 then
                          Begin
                            dbfBadRecList.Add(cStr) ;
                            Inc(dbfBadRecords) ;

                            nFirstBadField := WithinFieldByOffset(GetRecordBytesRead) ;
                            FillChar(bBadFieldRec , SizeOf(bBadFieldRec) , Byte(False)) ;
                            For nCheckField := nFirstBadField to GetFieldCount do
                              Begin
                                bBadFieldRec[nCheckField] := True ;
                                dbfBadFields := dbfBadFields + 1 ;
                                Inc(dbfBadFieldList[nCheckField]) ;
                                cFldName := GetFieldName(nCheckField) ;
                                If dbfBadNameList.IndexOf(cFldName) < 0 then
                                  dbfBadNameList.Add(cFldName) ;

                                Inc(dbfFieldErrors[GetFieldIdxType(nCheckField)]) ;
                              End ;
                          End ;
                      End ;
                  End ;

                { Add the record number to the bad field type lists. }
                For nField := 0 to GetFieldCount do
                  If bBadFieldRec[nField] then
                    AddRecNoToFieldList(nField , nRec) ;

                Application.ProcessMessages ;

                If (@pProc <> Nil) and (xFormPtr <> nil) then
                  pProc(nPerc , nRec , xFormPtr) ;
              End ;
          End ;
      Except
        MessageDlg('Exception during VerifyFile.', mtError, [mbOK], 0);
      End ;
    Finally
      Screen.Cursor := Save_Cursor ;  { Restore cursor to normal. }
    End ;
  End ;  { TxBase.VerifyFile }


{***********************************************************************
*                                                                      *
*       TxBase.dbfCalcFieldCount                                       *
*                                                                      *
*         Calculate the number of fields in the header.                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfCalcFieldCount ;
  Var
    cfcI ,
    cfcFieldCount : Integer ;

  Begin  { TxBase.dbfCalcFieldCount }
    With GetDataAreaPtr^ , GetHeaderPtr^ do
      Begin
        { Need check to stop loop if no terminator byte (#13) exists. }
        cfcFieldCount := ((GetHeaderSize - 1) div SizeOf(dbfFieldDescType)) - 1 ;

        dbfFieldCount := 0 ;
        cfcI := 1 ;
        Repeat
          If dbfRecordDesc[cfcI].Name[0] <> dbfTerminator then
            Inc(dbfFieldCount) ;
          Inc(cfcI) ;

          If cfcI >= dbfMaxFields then
            Begin
              ShowMessage('Maxfields exceeded in dbfCalcFieldCount.') ;
              Break ;
            End ;
        Until (dbfRecordDesc[cfcI].Name[0] = dbfTerminator) or
              (dbfFieldCount = cfcFieldCount) ;

        If dbfFieldCount < 1 then
          dbfFieldCount := cfcFieldCount ;
      End ;
  End ;  { TxBase.dbfCalcFieldCount }


{***********************************************************************
*                                                                      *
*       TxBase.dbfCalcFieldOffsets                                     *
*                                                                      *
*         Calculate the byte offsets for each field on a data record.  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfCalcFieldOffsets ;
  Var
    nI ,
    nOffset: Integer  ;

  Begin  { TxBase.dbCalcFieldOffsets }
    nOffset := 0 ;

    With GetDataAreaPtr^ , GetHeaderPtr^ do
      Begin
        FillChar(dbfFieldOffset , SizeOf(dbfFieldOffset) , 0) ;

        { DELETE field has an offset of zero. }
        dbfFieldOffset[0] := 0 ;

        For nI := 0 to GetFieldCount do
          Begin
            dbfFieldOffset[nI] := nOffset ;
            nOffset := nOffset + GetFieldWidth(nI) ;
          End ;
      End ;
  End ;  { TxBase.dbfCalcFieldOffsets }


{***********************************************************************
*                                                                      *
*       TxBase.dbCalcRecordOffset                                      *
*                                                                      *
*       Calculate a record offset of the current record in the         *
*       data file.                                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfCalcRecordOffset ;
  Begin  { TxBase.dbfCalcRecordOffset }
    GetDataAreaPtr^.dbfDataRecordOffset := CalcRecordOffset(GetRecordNo) ;
  End ;  { TxBase.dbfCalcRecordOffset }


{***********************************************************************
*                                                                      *
*       TxBase.CalcRecordOffset                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CalcRecordOffset(nRecNo : Integer) : Integer ;
  Begin  { TxBase.CalcRecordOffset }
    Result := GetHeaderSize + ((nRecNo - 1) * GetRecordSize) ;
  End ;  { TxBase.CalcRecordOffset }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldRecordOffset                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldRecordOffset(nRecord : Integer ;
                                     nField  : Integer  ) : Integer ;
  Begin  { TxBase.GetFieldRecordOffset }
    Result := CalcRecordOffset(nRecord) + GetFieldOffset(nField) ;
  End ;  { TxBase.GetFieldRecordOffset }

  
{***********************************************************************
*                                                                      *
*       TxBase.dbGetRecordOffset                                       *
*                                                                      *
*         Retrieve the current record's offset in the data file.       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.dbGetRecordOffset : Integer ;
  Begin  { TxBase.dbGetRecordOffset }
    Result := GetDataAreaPtr^.dbfDataRecordOffset ;
  End ;  { TxBase.dbGetRecordOffset }

  
{***********************************************************************
*                                                                      *
*       TxBase.GetFieldNo                                              *
*                                                                      *
*         Given the field name, return the field number.               *
*                                                                      *
*       Note:  the DELETE field has a field number of 0.               *                                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldNo(cFldName : String) : Integer ;
  Var
    nField : Integer ;

  Begin  { TxBase.GetFieldNo }
    Result := 0 ;

    For nField := 1 to GetFieldCount do
      If cFldName = GetFieldName(nField) then
        Begin
          Result := nField ;
          Exit ;
        End ;
  End ;  { TxBase.GetFieldNo }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldName                                            *
*                                                                      *
*         Return a field name given a field number.                    *
*                                                                      *
*       Note:  the DELETE field has a field number of 0.               *                                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldName(nField : Integer) : String ;
  Begin  { TxBase.GetFieldName }
    If nField = 0 then
      Result := 'DELETE'
    Else
      Result := StrPas(GetFieldDescPtr(nField)^.Name) ;
  End ;  { TxBase.GetFieldName }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldName                                            *
*                                                                      *
*         Set a field name with trailing 00's.                         *
*                                                                      *
*       Note:  the field name is an AsciiZ string with a total length  *
*              of 11 bytes.  The last byte must be a hex 00.           *
*              This routine is also intended to clean up the field     *
*              name by terminating with all hex 00's.  eg:             *
*                                                                      *
*          *** Need to check for duplicate field names.                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldName(cName  : String  ;
                              nField : Integer  ) ;
  Var
    pFldName : Pointer ;

  Begin  { TxBase.SetFieldName }
    pFldName := @(GetFieldDescPtr(nField)^.Name) ;
    cName := Trim(cName) ;

    { Truncate the name if more than 10 chars. }
    If Length(cName) > dbfMaxNameLen then
      cName := Copy(cName , 1 , dbfMaxNameLen) ;

    FillChar(pFldName^ , (dbfMaxNameLen + 1) , ZeroChar) ;
    Move(Pointer(cName)^ , pFldName^ , Length(cName)) ;
  End ;  { TxBase.SetFieldName }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldIgnore                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldIgnore(nField : Integer) : Boolean ;
  Begin  { TxBase.GetFieldIgnore }
    dbfCheckDataAreaPtr ;

    With GetDataAreaPtr^ do
      Try
        Result := dbfIgnoreField[nField] ;
      Except
        Raise Exception.Create(errMsgUndefinedFieldNo) ;
      End ;
  End ;  { TxBase.GetFieldIgnore }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldIgnore                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldIgnore(nField : Integer ;
                                bIgnore : Boolean ) ;
  Begin  { TxBase.SetFieldIgnore }
    dbfCheckDataAreaPtr ;

    With GetDataAreaPtr^ do
      Try
        dbfIgnoreField[nField] := bIgnore ;
      Except
        Raise Exception.Create(errMsgUndefinedFieldNo) ;
      End ;
  End ;  { TxBase.SetFieldIgnore }


{***********************************************************************
*                                                                      *
*       TxBase.AddField                                                *
*                                                                      *
*         Add a new field to the file definition.                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.AddField(cName     : String  ;
                         cType     : Char    ;
                         nWidth    : Integer ;
                         nDecimals : Integer  ) : Integer ;
  Var
    nFields : Integer ;

  Begin  { TxBase.AddField }
    dbfCheckDataAreaPtr ;

    nFields := GetFieldCount ;
    If nFields = -1 then
      nFields := 0 ;
    nFields := SetFieldCount(nFields + 1) ;
    Result := nFields ;

    cName := Copy(AnsiUpperCase(cName) , 1 , 10) ;
    If nFields > 0 then
      Begin
        With GetFieldDescPtr(nFields)^ do
          Begin
            SetFieldName(cName , nFields) ;

            FType      := cType     ;
            FWidth     := nWidth    ;
            Decimals   := nDecimals ;

            If IsDBase3 then
              WorkAreaId := $00
            Else
              If IsDBase4 then
                WorkAreaId := $01
              Else
                If IsFoxPro then
                  WorkAreaId := $00
                Else
                  WorkAreaId := $00 ;
          End ;
      End ;

    SetRecordSize(nWidth + GetRecordSize) ;
    SetHeaderSize((GetFieldCount * SizeOf(dbfFieldDescType)) +
                  SizeOf(xBaseFixedHeaderType) + 1) ;  { BKC }
    SetTerminator ;

    dbfCalcFieldOffsets ;
    dbfResetFieldOffsets ;
  End ;  { TxBase.AddField }


{***********************************************************************
*                                                                      *
*       TxBase.dbfResetFieldOffsets                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfResetFieldOffsets ;
  Var
    nField : Integer ;

  Begin  { TxBase.dbfResetFieldOffsets }
    For nField := 1 to GetFieldCount do
      With GetFieldDescPtr(nField)^ do
        FOffset := GetFieldOffset(nField) ;
  End ;  { TxBase.dbfResetFieldOffsets }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.FieldCreateStr                                          *
*                                                                      *
*           Name      1 10                                             *
*           Type     12  1                                             *
*           Width    14  3                                             *
*           Decimals 18  2                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.FieldCreateStr(nField : Integer) : String      ;
  Begin  { TxBase.FieldCreateStr }
    Result := Pad(GetFieldName(nField) , 10)                 + ' ' +
              GetFieldType(nField)                           + ' ' +
              IntToPadStr(GetFieldWidth(nField) , 3)         + ' ' +
              IntToStrBlankPad(GetFieldDecimals(nField) , 2)         ;
  End ;  { TxBase.FieldCreateStr }


{***********************************************************************
*                                                                      *
*       TxBase.FieldListCreate                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TxBase.FieldListCreate(pFldList : pTStringList) ;
  Var
    nField : Integer ;

  Begin  { TxBase.FieldListCreate }
    If pFldList = nil then
      Begin
        ShowErrorMessage('Invalid field list pointer in TxBase.FieldListCreate') ;
        Exit ;
      End ;

    pFldList^.Clear ;

    For nField := 1 to GetFieldCount do
      Begin
        pFldList^.Add(FieldCreateStr(nField)) ;
      End ;
  End ;  { TxBase.FieldListCreate }


{***********************************************************************
*                                                                      *
*       TxBase.CreateDatabase                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.CreateDatabase(    cFileName  : String      ;
                                    nSignature : Integer     ;
                                Var oFlds      : TStringList  ) ;
  Var
    nI   : Integer ;
    cStr : String  ;

    cName     : String  ;
    cType     : Char    ;
    nWidth    : Integer ;
    nDecimals : Integer ;
    cDecimal  : String  ;

  Begin  { TxBase.CreateDatabase }
    Try
      bcFileUtilities.DeleteFile(cFileName) ;
      bcFileUtilities.DeleteFile(ForceExtension(cFileName , 'FPT')) ;
      bcFileUtilities.DeleteFile(ForceExtension(cFileName , 'DBT')) ;
    Except
      On e: exception do
        Begin
          ShowErrorMessage('Error deleting old files in ' +
                       'TxBase.CreateDatabase'#13#10'Error message ' + e.Message);
          Exit ;
        End ;
    End ;

    Try
      Init ;
      SetFileName(cFileName) ;
      SetRecordSize(0)       ;
      SetLanguageDriver($00) ; { Use the default }
    Except
      On e: exception do
        Begin
          ShowErrorMessage('Error during initialization in ' +
                       'TxBase.CreateDatabase'#13#10'Error message ' + e.Message);
          Exit ;
        End ;
    End ;

    SetSignature(nSignature) ;
    Case nSignature of
      $03 ,
      $04  : SetMemoFileName('') ;

      $83 , $7B , $8B , $CB :
        Begin
          dbfSetMemoFile ;
          If not dbfCreateMemoFile('') then
            ShowErrorMessage('Could not create memo file [' + GetMemoFileName + ']') ;
        End ;
    End ;

    If not CreateFiles(True , nSignature) then
      Begin
        ShowErrorMessage('Error in TxBase.CreateFiles') ;
      End ;

    For nI := 0 to (oFlds.Count - 1) do
      Begin
        cStr := oFlds.Strings[nI] ;

//        If (not bFieldDeleted) then
          Try
            cName     := Copy(cStr ,  1 , 10) ;  { Name      1 10 }
            cType     := cStr[12] ;              { Type     12  1 }
            nWidth    := 0 ;                     { Width    14  3 }
            nDecimals := 0 ;                     { Decimals 18  2 }

            Try
              nWidth := StrToInt(Trim(Copy(cStr , 14 , 3))) ;
            Except
              On EConvertError do
                Begin
                  ShowMessage('Invalid field width in conversion in TxBase.CreateDatabase.') ;
                  nWidth := 0 ;
                End ;
            Else
              ShowMessage('Error converting field width.') ;
            End ;

            If cType in ['F' , 'N'] then
              Begin
                Try
                  cDecimal  := Trim(Copy(cStr , 18 , 2)) ;
                  If Length(cDecimal) = 0 then
                    nDecimals := 0
                  Else
                    nDecimals := StrToInt(cDecimal) ;
                Except
                  On EConvertError do
                    Begin
                      ShowMessage('Invalid decimal count in conversion.') ;
                      nDecimals := 0 ;
                    End ;
                Else
                  ShowMessage('Error converting decimal count.') ;
                End ;
              End
            Else
              nDecimals := 0 ;

            AddField(cName     ,
                     cType     ,
                     nWidth    ,
                     nDecimals  ) ;
          Except
            On e: exception do
                ShowErrorMessage('Error adding field definition in ' +
                             'TxBase.CreateDatabase'#13#10'Error message ' + e.Message);
          End ;  { If bFieldDeleted = False then }
      End ;

    dbfAllocateInternalRecord ;
    dbfBuildMemoFldList ;
    If not WriteHeader then
      ShowErrorMessage('Error writing header in TxBase.CreateDatabase') ;

    CloseFiles ;
    InitFile(cFileName , '') ;
  End ;  { TxBase.CreateDatabase }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldType                                            *
*                                                                      *
*         Return the type of a field given a field number.             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldType(nField : Integer) : Char ;
  Begin  { TxBase.GetFieldType }
    { The DELETE field is a type 'C', }
    If (nField = 0) then
      Result := 'C'
    Else
      If ValidFieldNumber(nField) then
        Result := GetFieldDescPtr(nField)^.FType
      Else
        Raise Exception.Create('Invalid field number [' + IntToStr(nField) +
                               '] in GetFieldType'#$0D#$0A +
                               'GetFieldCount = ' + IntToStr(GetFieldCount)) ;
  End ;  { TxBase.GetFieldType }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldIdxType                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldIdxType(nField : Integer) : Integer ;
  Begin  { TxBase.GetFieldIdxType }
    Result := PosStr(GetFieldType(nField) , LegalFieldTypes) ;
  End ;  { TxBase.GetFieldIdxType }


{***********************************************************************
*                                                                      *
*       TxBase.dbfCalcAllFieldTypes                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfCalcAllFieldTypes ;
  Var
    nIdx ,
    nField : Integer ;
    cFldTypes    : String ;
    cFldTypeChar : Char   ;

  Begin  { TxBase.dbfCalcAllFieldTypes }
    cFldTypes := '' ;

    With GetDataAreaPtr^ do
      Begin
        dbfAllFieldTypes := Spaces(Length(AllFields)) ;

        For nField := 1 to GetFieldCount do
          Begin
            cFldTypeChar := GetFieldType(nField) ;
            nIdx := PosStr(cFldTypeChar , AllFields) ;
            If nIdx > 0 then
              dbfAllFieldTypes[nIdx] := cFldTypeChar
            Else
              ShowErrorMessage('Unknown field type [' + cFldTypeChar + ']') ;
          End ;

        dbfAllFieldTypes := KillChar(dbfAllFieldTypes , ' ') ;
      End ;
  End ;  { TxBase.dbfCalcAllFieldTypes }


{***********************************************************************
*                                                                      *
*       TxBase.GetAllFieldTypes                                        *
*                                                                      *
*         Return a string of all field types.                          *
*                                                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetAllFieldTypes : String ;
  Begin  { TxBase.GetAllFieldTypes }
    Result := GetDataAreaPtr^.dbfAllFieldTypes ;
  End ;  { TxBase.GetAllFieldTypes }


{***********************************************************************
*                                                                      *
*       TxBase.HasFieldType                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.HasFieldType(cType : Char) : Boolean ;
  Begin  { TxBase.HasFieldType }
    Result := (PosStr(cType , GetAllFieldTypes) > 0) ;
  End ;  { TxBase.HasFieldType }


{***********************************************************************
*                                                                      *
*       TxBase.GetNoFieldTypes                                         *
*                                                                      *
*         Return a string of all field types.                          *
*                                                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetNoFieldTypes : Integer ;
  Begin  { TxBase.GetNoFieldTypes }
    Result := Length(GetDataAreaPtr^.dbfAllFieldTypes) ;
  End ;  { TxBase.GetNoFieldTypes }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldTypeName                                        *
*                                                                      *
*         Return the type of a field given a field number.             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldTypeName(nField : Integer) : String ;
  Var
    cFType : Char    ;
    nIdx   : Integer ;

  Begin  { TxBase.GetFieldTypeName }
    If ValidFieldNumber(nField) then
      Begin
        If nField = 0 then
          Result := FieldTypeNames[2]
        Else
          Begin
            cFType := GetFieldType(nField) ;
            If Byte(cFType) = 0 then
              Result := ''
            Else
              Try
                nIdx := PosStr(cFType , AllFields) ;
                If nIdx > 0 then
                  Result := FieldTypeNames[nIdx]
                Else
                  Result := '' ;
              Except
                Result := '' ;
              End ;
          End ;
      End
    Else
      Result := '' ;
  End ;  { TxBase.GetFieldTypeName }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldWidth                                           *
*                                                                      *
*         Return the width of a field given a field number.            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldWidth(nField : Integer) : Integer ;
  Begin  { TxBase.GetFieldWidth }
    If nField = 0 then
      Result := 1  { DELETE byte }
    Else
      Result := Integer(GetFieldDescPtr(nField)^.FWidth * 1) ;
  End ;  { TxBase.GetFieldWidth }


{***********************************************************************
*                                                                      *
*       TxBase.GetNameMaxWidth                                         *
*                                                                      *
*         Return the length of the longest field name, used            *
*       for screen/text formatting purposes.                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetNameMaxWidth : Integer ;
  Begin  { TxBase.GetNameMaxWidth }
    Result := GetDataAreaPtr^.dbfNameMaxWidth ;
  End ;  { TxBase.GetNameMaxWidth }


{***********************************************************************
*                                                                      *
*       TxBase.SetNameMaxWidth                                         *
*                                                                      *
*         Set the length of the longest field name, used for           *
*       screen/text formatting purposes.                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetNameMaxWidth(nLen : Integer) ;
  Begin  { TxBase.SetNameMaxWidth }
    GetDataAreaPtr^.dbfNameMaxWidth := nLen ;
  End ;  { TxBase.SetNameMaxWidth }


{***********************************************************************
*                                                                      *
*       TxBase.GetNameMaxWidthFld                                      *
*                                                                      *
*         Return the field number of the longest field name, used      *
*       for screen/text formatting purposes.                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetNameMaxWidthFld : Integer ;
  Begin  { TxBase.GetNameMaxWidth }
    Result := GetDataAreaPtr^.dbfNameMaxWidthFld ;
  End ;  { TxBase.GetNameMaxWidth }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldMaxWidth                                        *
*                                                                      *
*         Return the length of the longest field, used                 *
*       for screen/text formatting purposes.                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldMaxWidth : Integer ;
  Begin  { TxBase.GetFieldMaxWidth }
    Result := GetDataAreaPtr^.dbfFieldMaxWidth ;
  End ;  { TxBase.GetFieldMaxWidth }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldNameWidth                                       *
*                                                                      *
*         Return the width of a field given a field name.              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldNameWidth(cFldName : String) : Integer ;
  Begin  { TxBase.GetFieldNameWidth }
    Result := GetFieldWidth(GetFieldNo(cFldName)) ;
  End ;  { TxBase.GetFieldNameWidth }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldDecimals                                        *
*                                                                      *
*         Return the number of decimal places of a field given         *
*       given a field number.                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldDecimals(nField : Integer) : Integer ;
  Begin  { TxBase.GetFieldDecimals }
    { DELETE byte has no decimal places. }
    If nField = 0 then
      Result := 0
    Else
      Result := GetFieldDescPtr(nField)^.Decimals ;
  End ;  { TxBase.GetFieldDecimals }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldDecimals                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldDecimals(nField    : Integer ;
                                  nDecimals : Integer  ) ;
  Const
    cErrMsg : String[42] = 'Invalid decimal places in SetFieldDecimals' ;

  Var
    nDec : Integer ;

  Begin  { TxBase.SetFieldDecimals }
    If (nDecimals >= 0) then
      Begin
        nDec := GetFieldDecimals(nField) ;

        { DELETE byte has no decimal places. }
        If ((nField = 0) or (GetFieldType(nField) in NoDecimalFieldTypes)) then
          Begin
            If nDecimals = 0 then
              GetFieldDescPtr(nField)^.Decimals := 0
            Else
              Raise Exception.Create(cErrMsg) ;
          End
        Else
          Begin
            GetFieldDescPtr(nField)^.Decimals := nDecimals ;
          End ;

        SetHeaderDirty((nDec <> GetFieldDescPtr(nField)^.Decimals)) ;
      End
    Else
      Raise Exception.Create(cErrMsg) ;
  End ;  { TxBase.SetFieldDecimals }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldIndexedByte                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldIndexedByte(nField : Integer) : Byte ;
  Begin  { TxBase.GetFieldIndexedByte }
    If nField = 0 then  { DELETE byte is not indexed. }
      Result := 0
    Else
      Result := GetFieldDescPtr(nField)^.Indexed ;
  End ;  { TxBase.GetFieldIndexedByte }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldIndexedByte                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldIndexedByte(nField : Integer ;
                                     nByte  : Byte     ) ;
  Begin  { TxBase.SetFieldIndexedByte }
    If nField > 0 then  { DELETE byte is not indexed. }
      GetFieldDescPtr(nField)^.Indexed := nByte ;
  End ;  { TxBase.SetFieldIndexedByte }


{***********************************************************************
*                                                                      *
*       TxBase.SetAllFieldIndexBytes                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetAllFieldIndexBytes(nByte : Byte) ;
  Var
    nField : Integer ;
    
  Begin  { TxBase.SetAllFieldIndexBytes }
    For nField := 1 to GetFieldCount do
      GetFieldDescPtr(nField)^.Indexed := nByte ;
  End ;  { TxBase.SetAllFieldIndexBytes }


{***********************************************************************
*                                                                      *
*       TxBase.RemoveIndexIndicators                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.RemoveIndexIndicators ;
  Var
    nField : Integer ;

  Begin  { TxBase.RemoveIndexIndicators }
    SetProductionMDX(0) ;
    For nField := 1 to GetFieldCount do
      GetFieldDescPtr(nField)^.Indexed := 0 ;
  End ;  { TxBase.RemoveIndexIndicators }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldIndexed                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldIndexed(nField : Integer) : Boolean ;
  Begin  { TxBase.GetFieldIndexed }
    If nField = 0 then  { DELETE byte is not indexed. }
      Result := False
    Else
      Result := (GetFieldIndexedByte(nField) = $01) ;
  End ;  { TxBase.GetFieldIndexed }


{***********************************************************************
*                                                                      *
*       TxBase.GetSetFieldsFlag                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetSetFieldsFlag(nField : Integer) : Byte ;
  Begin  { TxBase.GetSetFieldsFlag }
    If nField = 0 then  { DELETE byte is not indexed. }
      Result := 0
    Else
      Result := GetFieldDescPtr(nField)^.SetFieldsFlag ;
  End ;  { TxBase.GetSetFieldsFlag }


{***********************************************************************
*                                                                      *
*       TxBase.GetColumnFlags                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetColumnFlags(nField : Integer) : Byte ;
  Begin  { TxBase.GetColumnFlags }
    Result := GetFieldDescPtr(nField)^.FColumnFlags ;
  End ;  { TxBase.GetColumnFlags }


{***********************************************************************
*                                                                      *
*       TxBase.SetColumnFlags                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetColumnFlags(nField : Integer ;
                                nFlags : Byte     ) ;
  Begin  { TxBase.SetColumnFlags }
    GetFieldDescPtr(nField)^.FColumnFlags := nFlags ;
  End ;  { TxBase.SetColumnFlags }


{***********************************************************************
*                                                                      *
*       TxBase.GetReservedMultiUserStr01                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetReservedMultiUserStr01(nField : Integer) : String ;
  Begin  { TxBase.GetReservedMultiUserStr01 }
    If nField = 0 then  { DELETE byte is not indexed. }
      Result := ''
    Else
      With GetFieldDescPtr(nField)^ do
        Result := '$ ' + HexByte(ReservedMultiUser01) ;
  End ;  { TxBase.GetReservedMultiUserStr01 }


{***********************************************************************
*                                                                      *
*       TxBase.GetReservedMultiUserStr02                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetReservedMultiUserStr02(nField : Integer) : String ;
  Begin  { TxBase.GetReservedMultiUserStr02 }
    If nField = 0 then  { DELETE byte is not indexed. }
      Result := ''
    Else
      With GetFieldDescPtr(nField)^ do
        Begin
          Result := '$ ' +
                    HexByte(ReservedMultiUser02[1]) + ' ' +
                    HexByte(ReservedMultiUser02[2]) ;
        End ;
  End ;  { TxBase.GetReservedMultiUserStr02 }


{***********************************************************************
*                                                                      *
*       TxBase.GetReservedUnknown                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetReservedUnknown(nField : Integer) : String ;
  Begin  { TxBase.GetReservedUnknown }
    If nField = 0 then  { DELETE byte is not indexed. }
      Result := ''
    Else
      With GetFieldDescPtr(nField)^ do
        Begin
          Result := '$ ' +
                    HexByte(Reserved[1]) + ' ' +
                    HexByte(Reserved[2]) + ' ' +
                    HexByte(Reserved[3]) + ' ' +
                    HexByte(Reserved[4]) + ' ' +
                    HexByte(Reserved[5]) + ' ' +
                    HexByte(Reserved[6]) + ' ' +
                    HexByte(Reserved[7]) ;
        End ;
  End ;  { TxBase.GetReservedUnknown }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.IsFloatField                                            *
*                                                                      *
*         This Function returns true if the field is a numeric type    *
*       with at least one decimal place specified.  Mostly used for    *
*       field conversion to the pascal FLOAT (Extended or Real) type.  *
*         Field Type 'F' is always interpreted as a floating point and *
*       'N' field type is a floating point type only if decimal places *
*       are specified.                                                 *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*       1999-04-09  BKC  Mods to correctly interpret field type and    *
*                        number of decimals properly.                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsFloatField(nField : Integer) : Boolean ;
  Begin  { TxBase.IsFloatField }
    Case GetFieldType(nField) of
      'F' : Result := True ;
      'N' : Result := (GetFieldDecimals(nField) > 0) ;
    Else
      Result := False ;
    End ;
  End ;  { TxBase.IsFloatField }


{***********************************************************************
*                                                                      *
*       TxBase.IsIntegerField                                          *
*                                                                      *
*         This function returns true if the field is a numeric type    *
*       with no decimal places.  Mostly used for field conversion to   *
*       the pascal integer types (Byte, Integer, Word, or Integer).    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsIntegerField(nField : Integer) : Boolean ;
  Begin  { TxBase.IsNumericField }
    Case GetFieldType(nField) of
      'F' ,
      'N'   : Result := (GetFieldDecimals(nField) = 0) ;
    Else
      Result := False ;
    End ;
  End ;  { TxBase.IsIntegerField }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldOffset                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldOffset(nField : Integer) : Integer ;
  Begin  { TxBase.GetFieldOffset }
    If nField = 0 then
      Result := 0
    Else
      Result := GetDataAreaPtr^.dbfFieldOffset[nField] ;
  End ;  { TxBase.GetFieldOffset }


{***********************************************************************
*                                                                      *
*       TxBase.GetWorkAreaId                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetWorkAreaId(nFieldNo : Integer) : Byte ;
  Begin  { TxBase.GetWorkAreaId }
    If nFieldNo = 0 then
      Result := 0
    Else
      If ValidFieldNumber(nFieldNo) then
        Result := GetRecordDescPtr^[nFieldNo].WorkAreaID
      Else
        Result := 0 ;
  End ;  { TxBase.GetWorkAreaId }

{***********************************************************************
*                                                                      *
*       TxBase.GetFieldLastByteOffset                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldLastByteOffset(nFieldNo : Integer) : Integer ;
  Begin  { TxBase.GetFieldLastByteOffset }
    Result := GetFieldOffset(nFieldNo) + GetFieldWidth(nFieldNo) ;
  End ;  { TxBase.GetFieldLastByteOffset }


{***********************************************************************
*                                                                      *
*       TxBase.WithinFieldByOffset                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.WithinFieldByOffset(nOffset : Integer) : Integer ;
  Var
    nField : Integer ;

  Begin  { TxBase.WithinFieldByOffset }
    nField := 0 ;
    Result := -1 ;

    Repeat
      If ((nOffset >= GetFieldOffset(nField)) and
          (nOffset <= GetFieldOffset(nField))    ) then
        Begin
          Result := nField ;
          Exit ;
        End ;

      Inc(nField) ;
    Until nField > GetFieldCount ;
  End ;  { TxBase.WithinFieldByOffset }


{***********************************************************************
*                                                                      *
*       TxBase.CheckFieldRead                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CheckFieldRead(nField : Integer) : Boolean ;
  Begin  { TxBase.CheckFieldRead }
    If (GetRecordSize = GetRecordBytesRead) then
      Result := True
    Else
      Begin
        If nField = 0 then
          Result := (GetRecordBytesRead >= 1)
        Else
          Begin
            If ((GetFieldOffset(nField) > GetRecordBytesRead) or
                (WithinFieldByOffset(GetRecordBytesRead) = nField)) then
              Result := False
            Else
              Result := True ;
          End ;
      End ;
  End ;  { TxBase.CheckFieldRead }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldOfs                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldOfs(cFldName : String) : Integer ;
  Begin  { TxBase.GetFieldOfs }
    Result := GetDataAreaPtr^.dbfFieldOffset[GetFieldNo(cFldName)] ;
  End ;  { TxBase.GetFieldOfs }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldPos                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldPos(cFldName : String) : Integer ;
  Var
    nResult : Integer ;

  Begin  { TxBase.GetFieldPos }
    nResult := GetFieldOfs(cFldName) ;
    If nResult > 0 then
      Inc(nResult) ;  { account for the DELETE byte }
    Result := nResult ;
  End ;  { TxBase.GetFieldPos }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldBytesAfter                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldBytesAfter(cFldName : String) : Integer ;
  Var
    nI : Integer ;

  Begin  { TxBase.GetFieldBytesAfter }
    nI := GetFieldNo(cFldName) ;
    Result := GetRecordSize      -
              GetFieldOffset(nI) -
              GetFieldWidth(nI)    ;
  End ;  { TxBase.GetFieldBytesAfter }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.GetBufferFieldStr                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBufferFieldStr(pRec   : Pointer ;
                                  nField : Integer  ) : String ;
  Var
    nI : Integer ;
    aPtr : CharPtr ;

  Begin  { TxBase.GetBufferFieldStr }
    If ValidFieldNumber(nField) then
      Begin
        aPtr := Pointer(Integer(pRec) + GetFieldOffset(nField)) ;

        Result := '' ;
        For nI := 1 to GetFieldWidth(nField) do
          Begin
            Result := Result + aPtr^ ;
            IncPointer(Pointer(aPtr) , 1) ;
          End ;
      End ;
  End ;  { TxBase.GetBufferFieldStr }


{***********************************************************************
*                                                                      *
*       TxBase.GetRecordNo                                             *
*                                                                      *
*         Return the current record number.                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetRecordNo : Integer ;
  Begin  { TxBase.GetRecordNo }
    Result := GetDataAreaPtr^.dbfCurrentRecord ;
  End ;  { TxBase.GetRecordNo }


{***********************************************************************
*                                                                      *
*       TxBase.SetRecordNo                                             *
*                                                                      *
*         Set the current record number.                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetRecordNo(nRecNo : Integer) ;
  Begin  { TxBase.SetRecordNo }
    GetDataAreaPtr^.dbfCurrentRecord := nRecNo ;
  End ;  { TxBase.SetRecordNo }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.GetRecordSize                                           *
*                                                                      *
*         Return the record size as a Integer.                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetRecordSize : Integer ;
  Begin  { TxBase.GetRecordSize }
    Result := GetFixedHeaderPtr^.dbfRecordSize ;
  End ;  { TxBase.GetRecordSize }


{***********************************************************************
*                                                                      *
*       TxBase.SetRecordSize                                           *
*                                                                      *
*         Set the record size as a Word.                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.SetRecordSize(S : Word) : Word ;
  Begin  { TxBase.SetRecordSize }
    With GetFixedHeaderPtr^ do
      dbfRecordSize := S ;
    Result := S ;
  End ;  { TxBase.SetRecordSize }


{***********************************************************************
*                                                                      *
*       TxBase.GetRecordPtr                                            *
*                                                                      *
*         Get the pointer to the current record buffer.                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetRecordPtr : Pointer ;
  Begin  { TxBase.GetRecordPtr }
    Result := GetDataAreaPtr^.dbfCurrentRecordPtr ;
  End ;  { TxBase.GetRecordPtr }


{***********************************************************************
*                                                                      *
*       TxBase.SetRecordPtr                                            *
*                                                                      *
*         Set the pointer to the current record buffer.                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetRecordPtr(pRecPtr : Pointer) ;
  Begin  { TxBase.SetRecordPtr }
    GetDataAreaPtr^.dbfCurrentRecordPtr := pRecPtr ;
  End ;  { TxBase.SetRecordPtr }


{***********************************************************************
*                                                                      *
*       TxBase.SetCurrentRecord                                        *
*                                                                      *
*         Set the current record buffer based on a record number.      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.SetCurrentRecord(nRecNo : Integer) : Boolean ;
  Begin  { TxBase.SetCurrentRecord }
    Result := False ;

    With GetDataAreaPtr^ do
      Begin
        { No negative record numbers or a number greater than }
        { the total number of records.                        }
        If not ValidRecordNumber(nRecNo) then
          Begin
            SetRecordNo(0) ;
            Exit ;
          End
        Else
          SetRecordNo(nRecNo) ;

        dbfCalcRecordOffset ;
        Result := True ;
      End ;
  End ;  { TxBase.SetCurrentRecord }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.BlankRecord                                             *
*                                                                      *
*         Fill a record buffer with blanks.                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.BlankRecord(pRec : Pointer) ;
  Var
    nField : Integer ;

  Begin  { TxBase.BlankRecord }
    For nField := 0 to GetFieldCount do
      SetFieldDefault(pRec , nField) ;
  End ;  { TxBase.BlankRecord }


{***********************************************************************
*                                                                      *
*       TxBase.ZapCurrentRecord                                        *
*                                                                      *
*         Fill the current record buffer with hex '00's.               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.ZapCurrentRecord ;
  Begin  { TxBase.ZapCurrentRecord }
    FillChar(GetRecordPtr^  , GetRecordSize , 0) ;
  End ;  { TxBase.ZapCurrentRecord }


{***********************************************************************
*                                                                      *
*       TxBase.BlankCurrentRecord                                      *
*                                                                      *
*         Fill the current record buffer with blanks.                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.BlankCurrentRecord ;
  Begin  { TxBase.BlankCurrentRecord }
    BlankRecord(GetRecordPtr) ;
  End ;  { TxBase.BlankCurrentRecord }


{***********************************************************************
*                                                                      *
*       TxBase.GetRecord                                               *
*                                                                      *
*         Read the specified record.                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetRecord(nRecNo : Integer) : Boolean ;
  Begin  { TxBase.GetRecord }
    Result := True ;

    { If a physical record number is out of range, exit with error. }
    With GetDataAreaPtr^ , GetHeaderPtr^ , dbfReadMulti do
      Begin
        SetRecordNo(nRecNo) ;

        If dbfMultiRecordPtr <> nil then
          Begin
            If (nRecNo < dbfMultiFirstRec) or (nRecNo > dbfMultiLastRec) then
              GetMultiRecord(nRecNo)
            Else
              dbfCurrentRecordPtr := Pointer((nRecNo - dbfMultiFirstRec) * GetRecordSize +
                                     Integer(dbfMultiRecordPtr)) ;
          End
        Else
          Begin
            If SetCurrentRecord(nRecNo) then
              Begin
                Try
                  dbfFileVar.SeekToPos(dbfDataRecordOffset) ;
                Except
                  On e: exception do
                    Begin
                      ShowErrorMessage('Seek failed in ' +
                                   'TxBase.GetRecord'#13#10'Error message ' + e.Message);
                      Result := False ;
                    End ;
                End ;

                FillChar(dbfCurrentRecordPtr^ , GetRecordSize , 0) ;
                Try
                  dbfRecordBytesRead := dbfFileVar.Read(dbfCurrentRecordPtr^ , GetRecordSize) ;
                Except
                  On e: exception do
                    Begin
                      ShowErrorMessage('Stream read failed in ' +
                                   'TxBase.GetRecord'#13#10'Error message ' + e.Message);
                      Result := False ;
                    End ;
                End ;

                If Result then
                  dbfDelFlag := (Char(dbfCurrentRecordPtr^) = '*')
                Else
                  dbfDelFlag := True ;
              End
            Else
              Begin
                Result := False ;
              End ;
          End ;
      End ;
  End ;  { TxBase.GetRecord }


{***********************************************************************
*                                                                      *
*       TxBase.GetRecordBytesRead                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetRecordBytesRead : Integer ;
  Begin  { TxBase.GetRecordBytesRead }
    Result := GetDataAreaPtr^.dbfRecordBytesRead ;
  End ;  { TxBase.GetRecordBytesRead }


{***********************************************************************
*                                                                      *
*       TxBase.SetRecordBytesRead                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetRecordBytesRead(nSize : Integer) ;
  Begin  { TxBase.SetRecordBytesRead }
    GetDataAreaPtr^.dbfRecordBytesRead := nSize ;
  End ;  { TxBase.SetRecordBytesRead }


{***********************************************************************
*                                                                      *
*       TxBase.CheckRecordBytesRead                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CheckRecordBytesRead : Boolean ;
  Begin  { TxBase.GetRecordBytesRead }
    Result := GetRecordSize = GetRecordBytesRead ;
{
    If not Result then
      ShowErrorMessage('Complete record not read: [ ' +
                       IntToStr(GetRecordBytesRead) + '] of [' +
                       IntToStr(GetRecordSize) + '] bytes read' + CRLF +
                       ' Record No. [' + PadLeft(IntToStr(GetRecordNo) , 8) + ']') ;
}
  End ;  { TxBase.CheckRecordBytesRead }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.GetMultiRecord                                          *
*                                                                      *
*         Read a bunch of records.                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMultiRecord(nFirstRec : Integer) : Boolean ;
  Var
    nBytesRead   : Integer ;

  Begin  { TxBase.GetMultiRecord }
    Result := False ;

    { If a physical record number is out of range, exit with error. }
    With GetDataAreaPtr^ , GetHeaderPtr^ do
      Begin
        { Set up the mult-record read buffer if necessary. }
        If dbfReadMulti.dbfMultiRecordPtr = nil then
          dbfSetUpMultiRecord(dbfReadMulti) ;

        If not SetCurrentRecord(nFirstRec) then
          Begin
            ShowMessage('GetMultiRecord [' + Format('%d' , [nFirstRec]) + '] is out of range.') ;
            Exit ;
          End ;

        Try
          dbfFileVar.SeekToPos(dbfDataRecordOffset) ;
        Except
          On e: exception do
            Begin
              ShowErrorMessage('Seek failed in ' +
                           'TxBase.GetMultiRecord'#13#10'Error message ' + e.Message);
              Result := False ;
            End ;
        End ;

        If Result then
          With dbfDataArea , dbfReadMulti do
            Begin
              Try
                nBytesRead := dbfFileVar.Read(dbfMultiRecordPtr^ , dbfMultiBufferSize) ;
              Except
                On e: exception do
                  Begin
                    ShowErrorMessage('Stream read failed in ' +
                                 'TxBase.GetMultiRecord'#13#10'Error message ' + e.Message);
                    Result := False ;
                    Exit ;
                  End ;
              End ;

              dbfCurrentRecordPtr := dbfMultiRecordPtr ;
              dbfMultiRecordCnt   := nBytesRead div GetRecordSize ;
              dbfExcessBytesRead  := nBytesRead - (dbfMultiRecordCnt * GetRecordSize) ;

              If dbfMultiRecordCnt = 0 then
                Begin
                  Result := False ;
                  Exit ;
                End ;

              dbfMultiFirstRec := nFirstRec ;
              dbfMultiLastRec  := dbfMultiFirstRec + dbfMultiRecordCnt - 1 ;

              dbfDelFlag := (Char(dbfMultiRecordPtr^) = '*') ;
            End ;
      End ;
  End ;  { TxBase.GetMultiRecord }


{***********************************************************************
*                                                                      *
*       TxBase.dbSetupMultiRecord                                      *
*                                                                      *
*         Set up the mult-record buffer.                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfSetUpMultiRecord(Var rMulti : dbfMultiType) ;
  Begin  { TxBase.dbfSetUpMultiRecord }
    With GetDataAreaPtr^ , GetHeaderPtr^ , rMulti do
      Begin
        If dbfMultiRecordPtr = nil then
          Begin
            dbfMultiMaxRecords := MaxSegmentSize div GetRecordSize ;
            dbfMultiBufferSize := dbfMultiMaxRecords * GetRecordSize ;

            If GetMemCheck(dbfMultiRecordPtr , dbfMultiBufferSize) then
              Begin
              End
            Else
              Begin
                ShowMessage('Unable to allocate memory for SetUpMultiRecord.') ;
                dbfMultiRecordPtr := nil ;
              End ;
          End ;
      End ;
  End ;  { TxBase.dbfSetUpMultiRecord }

  
{***********************************************************************
*                                                                      *
*       TxBase.PutMultiRecord                                          *
*                                                                      *
*         Read a bunch of records.                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.PutMultiRecord(nFirstRec : Integer) : Boolean ;
  Var
    nBytesWritten : Integer ;

  Begin  { TxBase.PutMultiRecord }
    Result := False ;

    { If a physical record number is out of range, exit with error. }
    With GetDataAreaPtr^ , GetHeaderPtr^ do
      Begin
        { Set up the mult-record write buffer if necessary. }
        If dbfWriteMulti.dbfMultiRecordPtr = nil then
          dbfSetUpMultiRecord(dbfWriteMulti) ;

        If not SetCurrentRecord(nFirstRec) then
          Begin
            ShowMessage('PutMultiRecord - Record [' + Format('%d' , [nFirstRec]) + '] is out of range.') ;
            Exit ;
          End ;

        dbfFileVar.SeekToPos(dbfDataRecordOffset) ;

        With GetDataAreaPtr^ , dbfWriteMulti do
          Begin
            nBytesWritten := dbfFileVar.Write(dbfMultiRecordPtr^ , dbfMultiBufferSize) ;

            If (nBytesWritten <> dbfMultiBufferSize) then
              Exit ;

            dbfCurrentRecordPtr := dbfMultiRecordPtr ;
            dbfMultiRecordCnt   := nBytesWritten div GetRecordSize ;
            dbfExcessBytesRead  := nBytesWritten - (dbfMultiRecordCnt * GetRecordSize) ;

            If dbfMultiRecordCnt = 0 then
              Begin
                Result := False ;
                Exit ;
              End ;

            dbfMultiFirstRec := nFirstRec ;
            dbfMultiLastRec  := dbfMultiFirstRec + dbfMultiRecordCnt - 1 ;
          End ;
      End ;

    Result := True ;
  End ;  { TxBase.PutMultiRecord }


{***********************************************************************
*                                                                      *
*       TxBase.AddMultiRecord                                          *
*                                                                      *
*         Add a new record to the multi-record write buffer.           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.AddMultiRecord(    pRec   : Pointer ;
                               Var nRecNo : Integer  ) : Boolean ;
  Var
    nOffset : Integer ;
    nOffPtr : Pointer ;

  Begin  { TxBase.AddMultiRecord }
    With GetDataAreaPtr^ , GetHeaderPtr^ , dbfWriteMulti do
      Begin
        If dbfWriteMulti.dbfMultiRecordPtr = nil then
          dbfSetUpMultiRecord(dbfWriteMulti) ;

        { Maximum records in buffer already. }
        If dbfMultiRecordCnt < dbfMultiMaxRecords then
          Begin
            Inc(dbfMultiRecordCnt) ;
            nRecNo := dbfMultiRecordCnt + GetTotalRecords ;

            nOffset :=  (dbfMultiRecordCnt - 1) * GetRecordSize ;
            nOffPtr := Pointer(Integer(dbfMultiRecordPtr) + nOffset) ;
            Move(pRec^ , nOffPtr^ , GetRecordSize) ;

            Result := True ;
          End
        Else
          Begin
            nRecNo := 0 ;
            Result := False ;
          End ;
      End ;
  End ;  { TxBase.AddMultiRecord }


{***********************************************************************
*                                                                      *
*       TxBase.AddMultiRecordBuffer                                    *
*                                                                      *
*         Read a bunch of records.                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.AddMultiRecordBuffer : Boolean ;
  Var
    nBufferSize   : Integer ;
    nBytesWritten : Integer ;

  Begin  { TxBase.AddMultiRecordBuffer }
    Result := False ;

    { If a physical record number is out of range, exit with error. }
    With GetDataAreaPtr^ , GetHeaderPtr^ , dbfWriteMulti do
      If dbfMultiRecordCnt > 0 then
        Begin
          GetDataFileVar^.SeekToPos(CalcFileSize) ;

          nBufferSize := dbfMultiRecordCnt * GetRecordSize ;

          With GetDataAreaPtr^ , dbfWriteMulti do
            Begin
              nBytesWritten := GetDataFileVar^.Write(dbfMultiRecordPtr^ , nBufferSize) ;

              If (nBytesWritten <> nBufferSize) then
                Exit ;

              IncTotalRecords(dbfMultiRecordCnt) ;

              dbfMultiRecordCnt := 0 ;
              If WriteHeader then
               Result := FlushDataFile
              Else
                Begin
                  Result := False ;
                  ShowMessage('Header not written in TxBase.AddMultiRecordBuffer') ;
                End ;
            End ;
        End
    Else
      Result := True ;
  End ;  { TxBase.AddMultiRecordBuffer }


{***********************************************************************
*                                                                      *
*       TxBase.IsMultiWriteBufferFull                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsMultiWriteBufferFull : Boolean ;
  Begin  { TxBase.IsMultiWriteBufferFull }
    With GetDataAreaPtr^ , dbfWriteMulti do
      Result := (dbfMultiRecordCnt >= dbfMultiMaxRecords) ;
  End ;  { TxBase.IsMultiWriteBufferFull }


{***********************************************************************
*                                                                      *
*       TxBase.DeleteRecord                                            *
*                                                                      *
*         Delete the specified record by setting the delete record     *
*       byte to a star "*".                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.DeleteRecord(nRecNo : Integer) : Boolean ;
  Begin  { TxBase.DeleteRecord }
    With GetDataAreaPtr^ do
      If GetRecord(nRecNo) then
        Begin
          Char(dbfCurrentRecordPtr^) := '*' ;
          dbfDelFlag := True ;
          Result     := True ;
        End
      Else
        Result := False ;
  End ;  { TxBase.DeleteRecord }


{***********************************************************************
*                                                                      *
*       TxBase.UnDeleteRecord                                          *
*                                                                      *
*         Un-Delete the specified record by setting the delete record  *
*       byte to a star " ".                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.UnDeleteRecord(nRecNo : Integer) : Boolean ;
  Begin  { TxBase.UnDeleteRecord }
    SetCurrentRecord(nRecNo) ;

    If GetRecord(nRecNo) then
      Begin
        Char(GetRecordPtr^) := ' ' ;
        Result := True ;
      End
    Else
      Result := False ;
  End ;  { TxBase.UnDeleteRecord }


{***********************************************************************
*                                                                      *
*       TxBase.PutRecordPtr                                            *
*                                                                      *
*         Write the specified record.                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.PutRecordPtr(pRec   : Pointer ;
                             nRecNo : Integer  ) : Boolean ;
  Var
    prBytesWritten : Integer ;

  Begin  { TxBase.PutRecordPtr }
    Result := False ;

    With GetDataAreaPtr^ , GetHeaderPtr^ do
      Begin
        dbfIO := 0 ;

        If (nRecNo < 1) then
          Exit ;

        SetRecordNo(nRecNo) ;

        dbfCalcRecordOffset ;
        GetDataFileVar^.SeekToPos(dbfDataRecordOffset) ;

        prBytesWritten := GetDataFileVar^.Write(pRec^ , GetRecordSize) ;

        If (prBytesWritten <> GetRecordSize) then
          Exit ;

        dbfDelFlag := (Char(pRec^) = '*') ;

        Result := True ;
      End ;  { With dbDataArea do }
  End ;  { TxBase.PutRecordPtr }


{***********************************************************************
*                                                                      *
*       TxBase.PutFieldRecordPtr                                       *
*                                                                      *
*         Write the specified record.                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.PutFieldRecordPtr(pFld   : Pointer ;
                                  nRecNo : Integer ;
                                  nField : Integer  ) : Boolean ;
  Var
    bResult : Boolean ;
    
    nWidth ,
    nBytesWritten : Integer ;

  Begin  { TxBase.PutFieldRecordPtr }
    nWidth := GetFieldWidth(nField) ;

    Try
      GetDataFileVar^.SeekToPos(GetFieldRecordOffset(nRecNo , nField)) ;

      nBytesWritten := GetDataFileVar^.Write(pFld^ , nWidth) ;
      bResult := True ;
    Except
      Begin
        nBytesWritten := 0 ;
        bResult := False ;
      End ;
    End ;

    If bResult then
      bResult := (nWidth = nBytesWritten) ;

    If bResult then
      FlushDataFile ;

    Result := bResult ;
  End ;  { TxBase.PutFieldRecordPtr }


{***********************************************************************
*                                                                      *
*       TxBase.PutRecord                                               *
*                                                                      *
*         Write the specified record.                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.PutRecord(nRecNo : Integer) : Boolean ;
  Begin  { TxBase.PutRecord }
    Result := PutRecordPtr(GetRecordPtr , nRecNo) ;
  End ;  { TxBase.PutRecord }


{***********************************************************************
*                                                                      *
*       TxBase.PutCurrentRecord                                        *
*                                                                      *
*         Write the specified record.                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.PutCurrentRecord : Boolean ;
  Begin  { TxBase.PutCurrentRecord }
    Result := PutRecordPtr(GetRecordPtr , GetRecordNo) ;
  End ;  { TxBase.PutCurrentRecord }


{***********************************************************************
*                                                                      *
*       TxBase.AddRecord                                               *
*                                                                      *
*         Add a new record to the end of the file.                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.AddRecord(    pRec   : Pointer ;
                          Var nRecNo : Integer  ) : Boolean ;
  Var
    nBytesWritten : Integer ;

  Begin  { TxBase.AddRecord }
    Result := False ;

    With GetDataAreaPtr^ do
      Begin
        GetDataFileVar^.SeekToPos(CalcFileSize) ;

        Char(pRec^) := ' ' ;  { Delete byte must be blank. }

        nBytesWritten := GetDataFileVar^.Write(pRec^ , GetRecordSize) ;
        If (nBytesWritten <> GetRecordSize) then
          Exit ;

        IncTotalRecords(1) ;
        nRecNo := GetTotalRecords ;
        Result := WriteHeader ;
      End ;
  End ;  { TxBase.AddRecord }


{***********************************************************************
*                                                                      *
*       TxBase.AddBlankRecord                                          *
*                                                                      *
*         Add a blank new record to the end of the file.               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.AddBlankRecord(Var RecNum : Integer) : Boolean ;
  Var
    oRec : TRecordBuffer ;

  Begin  { TxBase.AddBlankRecord }
    FillChar(oRec , GetRecordSize , ' ') ;

    Result := AddRecord(@oRec , RecNum) ;
  End ;  { TxBase.AddBlankRecord }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldInteger                                         *
*                                                                      *
*         Get the integer represention of a field.                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldInteger(pRec   : Pointer ;
                                nField : Integer  ) : Integer ;
  Var
    cMemo : String  ;
    nMemo : Integer ;

  Begin  { TxBase.GetFieldInteger }
    Case GetFieldType(nField) of
      'I' :
        Case GetFieldWidth(nField) of
          2 : Result := SmallInt(GetFieldPtr(pRec , nField)^) ;
          4 : Result := Integer(GetFieldPtr(pRec , nField)^)  ;
        Else
          Raise Exception.Create('Invalid I type integer in GetFieldInteger.') ;
        End ;

      'G' ,
      'M' : If GetFieldWidth(nField) = SizeOf(Integer) then
              Result := Integer(GetFieldPtr(pRec , nField)^)
            Else
              If GetFieldWidth(nField) = SizeOf(SmallInt) then
                Result := SmallInt(GetFieldPtr(pRec , nField)^)
              Else
                Begin
                  cMemo := GetFieldStr(pRec , nField , False) ;
                  cMemo := Trim(cMemo) ;
                  If Length(cMemo) = 0 then
                    cMemo := '0' ;
                  If Str2Int(cMemo , nMemo) then
                    Result := nMemo
                  Else
                    Raise Exception.Create('Not an Integer field [' + IntToStr(nField) + '] [' +
                                           GetFieldName(nField) + ']') ;
                End;
                
      'F' ,
      'N'  : Result := StrToInt(GetFieldStr(pRec , nField , False)) ;
    Else
      Raise Exception.Create('Not an Integer field [' + IntToStr(nField) + '] [' +
                             GetFieldName(nField) + ']') ;
    End ;
  End ;  { TxBase.GetFieldInteger }


{***********************************************************************
*                                                                      *
*       TxBase.GetFldInteger                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.GetFldInteger(nField : Integer) : Integer ;
  Begin  { TxBase.GetFldInteger }
    Result := GetFieldInteger(GetRecordPtr , nField) ;
  End ;  { TxBase.GetFldInteger }


{***********************************************************************
*                                                                      *
*       TxBase.GetSmallInt                                             *
*                                                                      *
*         Get the small integer represention of a field.               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldSmallInt(pRec   : Pointer ;
                                 nField : Integer  ) : Integer ;
  Begin  { TxBase.GetFieldSmallInt }
    If GetFieldWidth(nField) = SizeOf(SmallInt) then
      Result := SmallInt(GetFieldPtr(pRec , nField)^)
    Else
      Raise Exception.Create('Wrong field Length For SmallInt.') ;
  End ;  { TxBase.GetFieldSmallInt }


{***********************************************************************
*                                                                      *
*       TxBase.GetFldStr                                               *
*                                                                      *
*         Get the string represention of a field.                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*       93-12-07  BKC  Return null string if field number is invalid.  *
*       98-05-21  BKC  Add bClean parameter.                           *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldStr(pRec   : Pointer ;
                            nField : Integer ;
                            bClean : Boolean  ) : String ;
  Var
    nWidth : Integer ;
    cType  : Char    ;
    cStr   : String  ;

    pFldRec : Pointer ;

  Begin  { TxBase.GetFieldStr }
    If ValidFieldNumber(nField) then
      Begin
        SetRecordPtr(pRec) ;

        cStr   := '' ;
        Result := '' ;
        cType  := GetFieldType(nField) ;
        nWidth := GetFieldWidth(nField) ;

        pFldRec := Pointer(Integer(pRec) + GetFieldOffset(nField))  ;
        If bClean then
          MakeCleanStr(pFldRec ,
                       nWidth  ,
                       cStr     )
        Else
          cStr := MakeStr(pFldRec , nWidth) ;

        With GetDataAreaPtr^ do
          Begin
            Case cType of
              'L' : If cStr[1] in dbfValidLogicalTrue then
                      Result := 'True'
                    Else
                      If cStr[1] in dbfValidLogicalFalse then
                        Result := 'False'
                      Else
                        Result := '?????  [' + GetCurrFieldHexStr(nField) + ']' ;

              'I' :
                Case GetFieldWidth(nField) of
                  2 : Result := IntToStr(GetFieldSmallInt(pRec , nField)) ;
                  4 : Result := IntToStr(GetFieldInteger(pRec , nField)) ;
                End ;

              'T' : Result := GetDateTimeFieldEnglish(pRec , nField) ;

              'G' ,
              'M' : Try
                      Case nWidth of
                           4 ,
                           8 ,
                          10   : Begin
                                   If Length(Trim(cStr)) = 0 then
                                     Result := ''
                                   Else
                                     Result := IntToStr(GetMemoNumber(pRec , nField)) ;
                                 End ;
                        Else
                          Raise Exception.Create('Invalid memo field Width?') ;
                        End ;
                    Except
                      ShowMessage('Harry Aardvark') ;
                    End ;
            Else
              Result := cStr ;
            End ;  { Case cType of }
          End ;
      End
    Else
      Result := '' ;
  End ;  { TxBase.GetFieldStr }


{***********************************************************************
*                                                                      *
*       TxBase.GetCleanFieldStr                                        *
*                                                                      *
*         Get the printable string represention of a field.            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetCleanFieldStr(pRec   : Pointer ;
                                 nField : Integer  ) : String ;
  Begin  { TxBase.GetCleanFieldStr }
    Result := GetFieldStr(pRec   ,
                          nField ,
                          True    ) ;
  End ;  { TxBase.GetCleanFieldStr }


{***********************************************************************
*                                                                      *
*       TxBase.GetDeleteField                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDeleteField(pRec : Pointer) : Char ;
  Begin  { TxBase.GetDeleteField }
    Result := Char(pRec^) ;
  End ;  { TxBase.GetDeleteField }


{***********************************************************************
*                                                                      *
*       TxBase.GetFldStr                                               *
*                                                                      *
*         Get the string represention of a field.                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFldStr(nField : Integer) : String ;
  Begin  { TxBase.GetFldStr }
    Result := GetFieldStr(GetRecordPtr , nField , False) ;
  End ;  { TxBase.GetFldStr }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldDate                                            *
*                                                                      *
*         Get the date represention of a field.                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldDate(nField : Integer) : TDateTime ;
  Var
    cDate : String ;
    dDate : TDateTime ;

  Begin  { TxBase.GetFieldDate }
    cDate := Trim(GetFieldStr(GetRecordPtr , nField , False)) ;
    If (GetFieldType(nField) = 'D') and (Length(cDate) = dbfDateFieldWidth) then
      Begin
        If xDateToDate(cDate , dDate) then
          Result := dDate
        Else
          Result := 0 ;
      End
    Else
      Result := 0 ;
  End ;  { TxBase.GetFieldDate }


{***********************************************************************
*                                                                      *
*       TxBase.GetFldDate                                              *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.GetFldDate(nField : Integer) : TDateTime ;
  Begin  { TxBase.GetFldDate }
    Case GetFieldType(nField) of
      'D' : Result := GetFieldDate(nField) ;

      'T' : Result := GetDateTimeField(GetRecordPtr , nField) ;
    Else
      Raise Exception.Create('Invalid field type for GetFldDate.') ;
    End ;
  End ;  { TxBase.GetFldDate }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.GetFieldPtr                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldPtr(pRec   : Pointer ;
                            nField : Integer  ) : Pointer ;
  Begin  { TxBase.GetFieldPtr }
    If ValidFieldNumber(nField) then
      Begin
        If nField = 0 then
          Result := pRec
        Else
          Result := Pointer(Integer(pRec) + GetFieldOffset(nField)) ;
      End
    Else
      Result := nil ;
  End ;  { TxBase.GetFieldPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetCurrentFieldPtr                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetCurrentFieldPtr(nField : Integer) : Pointer ;
  Begin  { TxBase.GetCurrentFieldPtr }
    Result := GetFieldPtr(GetRecordPtr , nField) ;
  End ;  { TxBase.GetCurrentFieldPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldHexStr                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldHexStr(pRec   : Pointer ;
                               nField : Integer  ) : String ;
  Begin  { TxBase.GetFieldHexStr }
    Result := MakeHexStr(GetFieldPtr(pRec , nField) , GetFieldWidth(nField)) ;
  End ;  { TxBase.GetFieldHexStr }


{***********************************************************************
*                                                                      *
*       TxBase.GetCurrFieldHexStr                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetCurrFieldHexStr(nField : Integer) : String ;
  Begin  { TxBase.GetCurrFieldHexStr }
    Result := MakeHexStr(GetFieldPtr(GetRecordPtr , nField) , GetFieldWidth(nField)) ;
  End ;  { TxBase.GetCurrFieldHexStr }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldStr                                             *
*                                                                      *
*         Set the string represention of a field.                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldStr(pRec   : Pointer ;
                             cField : String  ;
                             nField : Integer  ) ;
  Var
    nLen : Integer ;
    pField : Pointer ;

  Begin  { TxBase.SetFieldStr }
    If pRec = nil then
      ShowMessage('Invalid record pointer in SetFieldStr')
    Else
      Begin
        pField := Pointer(Integer(pRec) + GetFieldOffset(nField)) ;

        nLen   := GetFieldWidth(nField) ;
        cField := Pad(Copy(cField , 1 , nLen) , nLen) ;
        Move(cField[1] , pField^ , GetFieldWidth(nField)) ;
      End ;
  End ;  { TxBase.SetFieldStr }


{***********************************************************************
*                                                                      *
*       TxBase.SetFldStr                                               *
*                                                                      *
*         Set the string represention of a field on the current record.*
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFldStr(cField : String  ;
                           nField : Integer  ) ;
  Begin  { TxBase.SetFldStr }
    SetFieldStr(GetRecordPtr ,
                cField       ,
                nField        ) ;
  End ;  { TxBase.SetFldStr }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldDefault                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldDefault(pRec   : Pointer ;
                                 nField : Integer  ) ;
  Begin  { TxBase.SetFieldDefault }
    Case GetFieldType(nField) of
      'D' : If GetFieldWidth(nField) = 8 then
              BlankField(pRec , nField)
            Else
              ZapField(pRec , nField) ;

      'M' : If GetFieldWidth(nField) = 10 then
              BlankField(pRec , nField)
            Else
              ZapField(pRec , nField) ;

      'B' ,
      'C' ,
      'F' ,
      'L' ,
      'N'   : BlankField(pRec , nField) ;

      'G' ,
      'I' ,
      'P' ,
      'T' ,
      'Y'   : ZapField(pRec , nField) ;
    Else
      Raise Exception.Create('Invalid field type ' + GetFieldType(nField)) ;
    End
  End ;  { TxBase.SetFieldDefault }


{***********************************************************************
*                                                                      *
*       TxBase.ZapField                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.ZapField(pRec   : Pointer ;
                          nField : Integer  ) ;
  Begin  { TxBase.ZapField }
    If pRec = nil then
      Raise Exception.Create('Invalid record pointer in ZapField')
    Else
      FillChar(Pointer(Integer(pRec) + GetFieldOffset(nField))^ ,
               GetFieldWidth(nField) ,
               0) ;
  End ;  { TxBase.ZapField }


{***********************************************************************
*                                                                      *
*       TxBase.BlankField                                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.BlankField(pRec   : Pointer ;
                            nField : Integer  ) ;
  Begin  { TxBase.BlankField }
    If pRec = nil then
      Raise Exception.Create('Invalid record pointer in BlankField.')
    Else
      FillChar(Pointer(Integer(pRec) + GetFieldOffset(nField))^ ,
               GetFieldWidth(nField) ,
               ' ') ;
  End ;  { TxBase.BlankField }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldInteger                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldInteger(pRec     : Pointer ;
                                 nInteger : Integer ;
                                 nField   : Integer  ) ;
  Begin  { TxBase.SetFieldInteger }
    SetFieldStr(pRec ,
                PadLeft(IntToStr(nInteger) , GetFieldWidth(nField)) ,
                nField                                               ) ;
  End ;  { TxBase.SetFieldInteger }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldSmallInt                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldSmallInt(pRec      : Pointer  ;
                                  nSmallInt : SmallInt ;
                                  nField    : Integer   ) ;
  Begin  { TxBase.SetFieldSmallInt }
    SetFieldInteger(pRec , nSmallInt , nField) ;
  End ;  { TxBase.SetFieldSmallInt }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldByte                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldByte(pRec   : Pointer ;
                              nByte  : Integer ;
                              nField : Integer  ) ;
  Begin  { TxBase.SetFieldInteger }
    SetFieldInteger(pRec , nByte , nField) ;
  End ;  { TxBase.SetFieldInteger }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldLogical                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldLogical(pRec     : Pointer ;
                                 bBoolean : Boolean ;
                                 nField   : Integer  ) ;
  Begin  { TxBase.SetFieldLogical }
    If bBoolean then
      SetFieldStr(pRec , 'T' , nField)
    Else
      SetFieldStr(pRec , 'F' , nField) ;
  End ;  { TxBase.SetFieldLogical }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldReal48                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldReal48(pRec    : Pointer ;
                                nReal48 : Real48  ;
                                nField  : Integer  ) ;
  Var
    cStr : String ;

  Begin  { TxBase.SetFieldReal48 }
    Str(nReal48 : GetFieldWidth(nField) : GetFieldDecimals(nField), cStr) ;
    SetFieldStr(pRec   ,
                cStr   ,
                nField  ) ;
  End ;  { TxBase.SetFieldReal48 }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldFloat                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldFloat(pRec    : Pointer ;
                               nFloat  : Float   ;
                               nField  : Integer  ) ;
  Var
    cStr : String ;

  Begin  { TxBase.SetFieldFloat }
    If nFloat = 0 then
      Begin
        If GetFieldDecimals(nField) = 0 then
          cStr := '0'
        Else
          cStr := '0.' + StrMake('0' , GetFieldDecimals(nField)) ;
      End
    Else
      cStr := FloatToStrF(nFloat , ffFixed , 18 , GetFieldDecimals(nField)) ;
//      Str(nFloat : GetFieldWidth(nField) : GetFieldDecimals(nField), cStr) ;
    cStr := PadLeft(Trim(cStr) , GetFieldWidth(nField)) ;

    SetFieldStr(pRec   ,
                cStr   ,
                nField  ) ;
  End ;  { TxBase.SetFieldFloat }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldFloat                                           *
*                                                                      *
*         Get the float represention of a field.                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldFloat(    pRec   : Pointer ;
                                  nField : Integer ;
                              Var bOkay  : Boolean  ) : Float ;
  Begin  { TxBase.GetFieldFloat }
    If (GetFieldType(nField) in ['N' , 'F']) and
       (GetFieldDecimals(nField) > 0)            then
      Try
        Result := StrToFloat(Trim(GetFieldStr(pRec , nField , False))) ;
        bOkay := True ;
      Except
        Begin
          Result := 0.00 ;
          bOkay  := False ;
        End ;
      End 
    Else
      Raise Exception.Create('Wrong field type for GetFieldFloat.') ;
  End ;  { TxBase.GetFieldFloat }


{***********************************************************************
*                                                                      *
*       TxBase.DisplayFldStr                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.DisplayFldStr : String ;
  Begin  { TxBase.DisplayFldStr }
    Result := GetDataAreaPtr^.dbfFieldToDisplay ;
  End ;  { TxBase.DisplayFldStr }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldChoice                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldChoice : Integer ;
  Begin  { TxBase.GetFieldChoice }
    Result := GetDataAreaPtr^.dbfFieldChoice ;
  End ;  { TxBase.GetFieldChoice }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldChoice                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFieldChoice(nField : Integer) ;
  Begin  { TxBase.SetFieldChoice }
    GetDataAreaPtr^.dbfFieldChoice := nField ;
  End ;  { TxBase.SetFieldChoice }


{***********************************************************************
*                                                                      *
*       TxBase.Duplicate                                               *
*                                                                      *
*         Duplicate the current xBase object.                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.Duplicate(DB    : pTxBase ;
                           FName : String    ;
                           MName : String     ) ;
  Var
    nSize : Word ;

  Begin  { TxBase.Duplicate }
    FName := UpperTrim(Fname) ;
    MName := UpperTrim(MName) ;

    If (Length(FName) = 0)       or
       (FName = GetDataFileName) or
       (FName = MName) then
      Begin
        ShowMessage('Invalid file name [' + FName + '] to duplicate.') ;
        Exit ;
      End ;

    If DB = nil then
      Begin
        ShowMessage('Destination xBasePtr is nil.') ;
        Exit ;
      End ;

    DB^.Init ;
    { Move only the needed file header information. }
    nSize := dbfCorrectedHeaderSize ;
    With GetDataAreaPtr^ do
      Move(GetHeaderPtr^ , DB^.GetHeaderPtr^ , nSize) ;

    With DB^ , GetDataAreaPtr^ , GetHeaderPtr^ do
      Begin
        dbfConvertHeader ;  { Convert the header information. }

        { Set the new data file name. }
        SetFileName(FName) ;

        If CreateFiles(False , GetSignature) then
          Begin
            SetTotalRecords(0) ;

            If not WriteHeader then
              Begin
                ShowMessage(FName + ' Header not written out.') ;
                Application.Terminate ;
              End ;
          End
        Else
          Begin
            ShowMessage(FName + ' not created.') ;
            Application.Terminate ;
          End ;
      End ;

    With DB^ , GetDataAreaPtr^ do
      Begin
        dbfAllocateInternalRecord ;

        If (Length(MName) > 0) then
          Begin
            { Create the memo file if any memo fields exist. }
            SetMemoFileName(MName) ;
            If HasMemoFile then
              Begin
                If not dbfCreateMemoFile(MName) then
                  Begin
                    ShowMessage('Memo file [' + GetMemoFileName + '] not created.') ;
                    Application.Terminate ;
                  End ;
              End ;
          End ;
      End ;
  End ;  { TxBase.Duplicate }


{***********************************************************************
*                                                                      *
*       TxBase.GetDataFileName                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDataFileName : TFileName ;
  Begin  { TxBase.GetDataFileName }
    Result := GetDataAreaPtr^.dbfFileName ;
  End ;  { TxBase.GetDataFileName }


{***********************************************************************
*                                                                      *
*       TxBase.GetBackLinkStr                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBackLinkPtr : Pointer ;
  Begin  { TxBase.GetBackLinkPtr }
    Result := Pointer(Integer(GetTerminatorPtr) + 1) ;
  End ;  { TxBase.GetBackLinkPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetBackLinkStr                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBackLinkStr : String ;
  Var
    cResult : String ;

  Begin  { TxBase.GetBackLinkStr }
    If HasBackLink then
      cResult := MakeStr(GetBackLinkPtr , 263)
    Else
      cResult := '' ;

    Result := cResult ;
  End ;  { TxBase.GetBackLinkStr }


{***********************************************************************
*                                                                      *
*       TxBase.GetShortDataFileName                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetShortDataFileName : TFileName ;
  Begin  { TxBase.GetShortDataFileName }
    Result := GetDataAreaPtr^.dbfShortDataFileName ;
  End ;  { TxBase.GetShortDataFileName }


{***********************************************************************
*                                                                      *
*       TxBase.SetFileName                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFileName(Const cFileName : TFileName) ;
  Var
    cFile : TFileName ;

  Begin  { TxBase.SetFileName }
    cFile := cFileName ;
// ShowMessage(cFile) ;

    GetDataAreaPtr^.dbfFileName := cFileName ;
    Try
      ResetShortFileName ;
    Except
      // Ignore error
    End ;
  End ;  { TxBase.SetFileName }


{***********************************************************************
*                                                                      *
*       TxBase.GetDataFilePtr                                          *
*                                                                      *
*         Flush the data file.                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDataFilePtr : pTFileStreamX ;
  Begin  { TxBase.GetDataFilePtr }
    With GetDataAreaPtr^ do
      Result := @dbfFileVar ;
  End ;  { TxBase.GetDataFilePtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFilePtr                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFilePtr : pTFileStreamX ;
  Begin  { TxBase.GetMemoFilePtr }
    With GetDataAreaPtr^ do
      Result := @dbfMemoFileVar ;
  End ;  { TxBase.GetMemoFilePtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetDataFileHandle                                       *
*                                                                      *
*         Flush the data file.                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDataFileHandle : Integer ;
  Begin  { TxBase.GetDataFileHandle }
    Result := GetDataFilePtr^.Handle ;
  End ;  { TxBase.GetDataFileHandle }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFileHandle                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFileHandle : Integer ;
  Begin  { TxBase.GetMemoFileHandle }
    Result := GetMemoFilePtr^.Handle ;
  End ;  { TxBase.GetMemoFileHandle }

  
{***********************************************************************
*                                                                      *
*       TxBase.FlushDataFile                                           *
*                                                                      *
*         Flush the data file.                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.FlushDataFile : Boolean ;
  Begin  { TxBase.FlushDataFile }
//    Result := FlushFile(GetDataFilePtr) ;
    Result := True ;
  End ;  { TxBase.FlushDataFile }


{***********************************************************************
*                                                                      *
*       TxBase.FlushMemoFile                                           *
*                                                                      *
*         Flush the memo file.                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.FlushMemoFile : Boolean ;
  Begin  { TxBase.FlushMemoFile }
//    Result := FlushFile(GetMemoFilePtr) ;
    Result := True ;
  End ;  { TxBase.FlushMemoFile }

  
{***********************************************************************
*                                                                      *
*       TxBase.CreateFiles                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
***********************************************************************}

Function TxBase.CreateFiles(bCreateHeader : Boolean ;
                            nSignature    : Byte     ) : Boolean ;
   Begin  { TxBase.CreateFiles }
     Try
       dbfDataArea.dbfFileVar := TFileStreamX.Create(GetDataFileName , fmCreate) ;

       If bCreateHeader then
         Begin
           SetSignature(nSignature) ;
           SetHeaderSize(SizeOf(xBaseFixedHeaderType)) ;  // BKC
           SetRecordSize(1) ;  { One byte for the Delete field. }
           SetFieldCount(0) ;
         End ;

       Result := True ;
     Except
       Result := False ;
     End ;
   End ;  { TxBase.CreateFiles }


{***********************************************************************
*                                                                      *
*       TxBase.dbfSetMemoVars                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
***********************************************************************}

Procedure TxBase.dbfSetMemoVars ;
  Begin  { TxBase.dbfSetMemoVars }
    If IsDbase3 then
      SetMemoBlockType(dbfDBTMemo)
    Else
      If IsDbase4 then
        SetMemoBlockType(dbfDB4Memo)
      Else
        If IsFoxPro then
          SetMemoBlockType(dbfFPTMemo)
        Else
          Begin
            SetMemoBlockType(dbfDBTMemo) ;
            ShowMessage('Default memo type set.') ;
          End ;
  End ;  { TxBase.dbfSetMemoVars }


{***********************************************************************
*                                                                      *
*       TxBase.dbfCreateMemoFile                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
***********************************************************************}

Function TxBase.dbfCreateMemoFile(MName : String) : Boolean ;
  Var
    bMemoHeaderWritten : Boolean ;
    nBytesWritten      : Integer ;

   Begin  { TxBase.dbfCreateMemoFile }
     SetMemoFileExtension ;

     FillChar(GetMemoHeaderPtr^ , dbfMemoHeaderSize , $00) ;
     dbfSetMemoVars ;

     With GetMemoHeaderPtr^ do
       Case GetMemoBlockType of
         dbfDBTMemo : With GetMemoHeaderPtr^ do
                        Begin
                          dbfMemoVersionDBT := $03 ;

                          SetMemoBlockLen(dbfDefaultBlockLenDB3) ;
                        End ;

         dbfDB4Memo : SetMemoBlockLen(dbfDefaultBlockLenDB4) ;
         dbfFPTMemo : SetMemoBlockLen(dbfDefaultBlockLenFPT) ;
       Else
         Raise Exception.Create('Invalid memo block type TxBase.dbfCreateMemoFile') ;
       End ;

     With GetDataAreaPtr^ , GetHeaderPtr^ do
       Begin
         dbfMemoOpened := False ;

         If dbfMemoFile then
           Begin
             If Length(MName) = 0 then
               dbfMemoFileName := dbfFileName
             Else
               dbfMemoFileName := MName ;
             dbfMemoFileName := ForceExtension(dbfMemoFileName , dbfMemoFileExt) ;

             If dbfMemoOpened then
               Begin
                 FreeAndNil(dbfMemoFileVar) ;
                 dbfMemoOpened := False ;
               End ;

             Try
               dbfMemoFileVar := TFileStreamX.Create(dbfMemoFileName, fmCreate);
               dbfMemoOpened := True ;
             Except
               Begin
                 MessageDlg('Memo file create failed in dbfMemoFileCreate' ,
                            mtError ,
                            [mbOK]  ,
                            0        ) ;
                 dbfMemoOpened := False ;
               End ;
             End ;

             If dbfMemoOpened then
               Begin
                 Try
                   dbfMemoFileVar.SeekToStart ;

                   { Write the first (unused) memo block. }
                   nBytesWritten := dbfMemoFileVar.Write(dbfMemoHeader , SizeOf(dbfMemoHeader)) ;
                   bMemoHeaderWritten := True  ;
                 Except
                   Begin
                     nBytesWritten := 0 ;
                     bMemoHeaderWritten := False ;
                   End ;
                 End ;

                 If bMemoHeaderWritten then
                   dbfMemoOpened := (nBytesWritten = SizeOf(dbfMemoHeader))
                 Else
                   Begin
                     ShowMessage('Memo header stream write failed in dbfMemoFileCreate') ;
                     dbfMemoOpened := False ;
                   End ;
               End ;

             Result := dbfMemoOpened ;
             SetMemoBlockCount(0) ;
           End
         Else
           Result := False ;
       End ;
   End ;  { TxBase.dbfCreateMemoFile }

   
{***********************************************************************
*                                                                      *
*       TxBase.WriteHeader                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.WriteHeader : Boolean ;
  Var
    nBytesWritten : Integer ;

  Begin  { TxBase.WriteHeader }
    SetHeaderSize(GetHeaderSize) ;
    
    With GetDataAreaPtr^ do
      Begin
        SetLastUpdateToday ;

        If dbfFileVar.Size > 0 then
          dbfFileVar.SeekToStart ;

        Try
          nBytesWritten := dbfFileVar.Write(GetFixedHeaderPtr^ , GetHeaderSize) ;
          Result := (GetHeaderSize = nBytesWritten) ;
        Except
          Result := False ;
        End ;

        If Result then
          Result := FlushDataFile ;

        SetHeaderDirty(not Result) ;

        dbfCalcFieldOffsets ;
        CalcFileSize ;
      End ;
  End ;  { TxBase.WriteHeader }

  
{***********************************************************************
*                                                                      *
*       TxBase.ReadHeader                                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ReadHeader : Boolean ;
//  Var
//    nBytesRead : Integer ;

  Begin  { TxBase.ReadHeader }
    With GetDataAreaPtr^ do
      Begin
        Try
          dbfFileVar.SeekToStart ;
        Except
          MessageDlg('Cannot seek to start of header in TxBase.ReadHeader' ,
                     mtError ,
                     [mbOK]  ,
                     0        ) ;
        End ;

        Try
          { nBytesRead := } dbfFileVar.Read(GetHeaderPtr^ , SizeOf(dbfHeader)) ;
          Result := True ;
        Except
          Begin
            MessageDlg('Cannot read header in TxBase.ReadHeader', mtError, [mbOK], 0);
            Result := False ;
          End ;
        End ;
      End ;
  End ;  { TxBase.ReadHeader }


{***********************************************************************
*                                                                      *
*       TxBase.GetTerminatorPtr                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetTerminatorPtr : Pointer ;
  Begin  { TxBase.GetTerminatorPtr }
    Result := Pointer(Integer(GetFixedHeaderPtr)   +
                           SizeOf(xBaseFixedHeaderType) +
                           (GetFieldCount * SizeOf(dbfFieldDescType))) ;
  End ;  { TxBase.GetTerminatorPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetTerminator                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetTerminator : Byte ;
  Begin  { TxBase.GetTerminator }
    Result := Byte(GetTerminatorPtr^) ;
  End ;  { TxBase.GetTerminator }


{***********************************************************************
*                                                                      *
*       TxBase.SetTerminator                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetTerminator ;
  Begin  { TxBase.SetTerminator }
    Char(GetTerminatorPtr^) := dbfTerminator ;
  End ;  { TxBase.SetTerminator }


{***********************************************************************
*                                                                      *
*       TxBase.SetFileTerminator                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetFileTerminator ;
  Const
    cEOF : Char = #$1A ;

  Var
    nOffset       : Integer ;
    nBytesWritten : Integer ;

  Begin  { TxBase.SetFileTerminator }
    nOffset := GetHeaderSize + (GetTotalRecords * GetRecordSize) ;
    GetDataFileVar^.SeekToPos(nOffset) ;
    nBytesWritten := GetDataFileVar^.Write(cEOF , SizeOf(cEOF)) ;
    If SizeOf(cEOF) <> nBytesWritten then
      Raise Exception.Create('Unable to write file terminator byte in TxBase.SetFileTerminator') ;
  End ;  { TxBase.SetFileTerminator }


{***********************************************************************
*                                                                      *
*       TxBase.GetSignature                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetSignature : Byte ;
  Begin  { TxBase.GetSignature }
    Result := GetFixedHeaderPtr^.Signature ;
  End ;  { TxBase.GetSignature }


{***********************************************************************
*                                                                      *
*       TxBase.HasBackLink                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.HasBackLink : Boolean ;
  Begin  { TxBase.HasBackLink }
    Result := (GetSignature in [$30]) ;
  End ;  { TxBase.HasBackLink }


{***********************************************************************
*                                                                      *
*       TxBase.SetSignature                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetSignature(nSignature : Byte) ;
  Begin  { TxBase.SetSignature }
    GetFixedHeaderPtr^.Signature := nSignature ;
  End ;  { TxBase.SetSignature }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.GetLastFileByte                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetLastFileByte : Byte ;
  Begin  { TxBase.GetLastFileByte }
    // need work here
    Result := $00 ;
  End ;  { TxBase.GetLastFileByte }


{***********************************************************************
*                                                                      *
*       TxBase.GetLastUpdate                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetLastUpdate(bShowError : Boolean) : TDateTime ;
  Var
    nYear : Word ;

  Begin  { TxBase.GetLastUpdate }
    With GetDataAreaPtr^ , GetFixedHeaderPtr^ do
      Begin
        With LastUpdate do
          Begin
            If Year < 80 then
              nYear := Year + 2000
            Else
              nYear := Year + 1900 ;

            Try
              dbfLastUpdate := EncodeDate(nYear , Month , LastUpdate.Day) ;
            Except
              Begin
                dbfLastUpdate := Now ;
                If bShowError then
                  MessageDlg('Invalid Last Update, default (today) used.' ,
                             mtError ,
                             [mbOK]  ,
                             0        ) ;
              End ;
            End ;
          End ;

      Result := dbfLastUpdate ;
    End ;
  End ;  { TxBase.GetLastUpdate }


{***********************************************************************
*                                                                      *
*       TxBase.SetLastUpdate                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetLastUpdate(sluDate : TDateTime) ;
  Var
    sluDay , sluMonth , sluYear : Word ;

  Begin  { SetLastUpdate }
    With GetDataAreaPtr^ , GetFixedHeaderPtr^ do
      Begin
        dbfLastUpdate := sluDate ;
        DecodeDate(dbfLastUpdate , sluYear , sluMonth , sluDay) ;
        With LastUpdate do
          Begin
            Day   := sluDay         ;
            Month := sluMonth       ;
            Year  := sluYear - 1900 ;
          End ;
      End ;
  End ;  { TxBase.SetLastUpdate }


{***********************************************************************
*                                                                      *
*       TxBase.GetLastUpdateEnglish                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetLastUpdateEnglish : String ;
  Var
    nYear  ,
    nMonth ,
    nDay     : Word ;

    xDate : TDateTime ;

  Begin  { TxBase.GetLastUpdateEnglish }
    xDate := GetLastUpdate(False) ;
    DecodeDate(xDate , nYear , nMonth , nDay) ;
    Result := LongDayNames[DayOfWeek(xDate)] + ' '  +
              LongMonthNames[nMonth]         + ' '  +
              IntToStr(nDay)                 + ', ' +
              IntToStr(nYear) ;
  End ;  { TxBase.GetLastUpdateEnglish }


{***********************************************************************
*                                                                      *
*       TxBase.GetDateTimeField                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDateTimeField(pRec   : Pointer ;
                                 nField : Integer  ) : TDateTime ;
  Var
    nDateTime : TDateTime ;

  Begin  { TxBase.GetDateTimeField }
    nDateTime := TDateTime(GetFieldPtr(pRec , nField)^) ;

    If (GetFieldType(nField) = 'T') and (nDateTime <> 0) then
      Result := ConvertDateTime(nDateTime)
    Else
      Result := 0 ;
  End ;  { TxBase.GetDateTimeField }


{***********************************************************************
*                                                                      *
*       TxBase.GetDateTimeFieldEnglish                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDateTimeFieldEnglish(pRec   : Pointer ;
                                        nField : Integer  ) : String ;
  Var
    nYear  ,
    nMonth ,
    nDay   ,

    nHour ,
    nMin  ,
    nSec  ,
    nMSec   : Word ;

    xDate : TDateTime ;

  Begin  { TxBase.GetDateTimeFieldEnglish }
    Result := '' ;
    If GetFieldType(nField) = 'T' then
      Begin
        xDate := GetDateTimeField(pRec , nField) ;
        If xDate = 0 then
          Result := msgBlankDate  + '  [$' + GetFieldHexStr(pRec , nField) + ']'
        Else
          Begin
            Try
              DecodeDate(xDate , nYear , nMonth , nDay) ;
              DecodeTime(xDate ,
                         nHour ,
                         nMin  ,
                         nSec  ,
                         nMSec  ) ;
              If nMSec >= 500 then
                nSec := nSec + 1 ;
            Except
              ShowMessage(msgInvalidDate + '  [$' + GetFieldHexStr(pRec , nField) + ']') ;
            End ;

            If IsDateValid(nYear  ,
                           nMonth ,
                           nDay    ) then
              Try
                Result := LongDayNames[DayOfWeek(xDate)] + ' '  +
                          LongMonthNames[nMonth]         + ' '  +
                          IntToStr(nDay)                 + ', ' +
                          IntToStr(nYear)                + '  ' +

                          TimeStr24(nHour , nMin , nSec) ;
              Except
                Result := msgInvalidDate ;
              End
            Else
              Result := msgInvalidDate ;
          End ;
      End
    Else
      Raise Exception.Create('Only T field value can be decoded.'#13#10) ;
  End ;  { TxBase.GetDateTimeFieldEnglish }


{***********************************************************************
*                                                                      *
*       TxBase.LastUpdateToday                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetLastUpdateToday ;
  Begin
    SetLastUpdate(SysUtils.Date) ;
  End ;


{***********************************************************************
*                                                                      *
*       TxBase.GetLastUpdateStr                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetLastUpdateStr : String ;
  Begin  { TxBase.GetLastUpdateStr }
    Result := DateToStr(GetLastUpdate(False)) ;
  End ;  { TxBase.GetLastUpdateStr }


{***********************************************************************
*                                                                      *
*       TxBase.CalcHeaderSize                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CalcHeaderSize : Integer ;
  Var
    nFields     : Integer ;
    nHeaderSize : Integer ;

  Begin  { TxBase.CalcHeaderSize }
    nFields := 1 ;
    nHeaderSize := SizeOf(xBaseFixedHeaderType) + 1 ;

    Repeat
      With GetFieldDescPtr(nFields)^ do
        If Name[0] <> dbfTerminator then  { Terminator is usually #13 }
          nHeaderSize := nHeaderSize + SizeOf(dbfFieldDescType) ;
      Inc(nFields) ;
    Until (nFields >= (dbfMaxFields + 1)) or (GetFieldDescPtr(nFields)^.Name[0] = dbfTerminator) ;

    If GetSignature in [$30] then
      nHeaderSize := nHeaderSize + dbfBackLinkSize ;

    Result := nHeaderSize ;
  End ;  { TxBase.CalcHeaderSize }


{***********************************************************************
*                                                                      *
*       TxBase.CalcRecordSize                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CalcRecordSize : Integer ;
  Var
    nField  : Integer ;
    nResult : Integer ;

  Begin  { TxBase.CalcRecordSize }
    nResult := 0 ;

    For nField := 0 to GetFieldCount do
      Inc(nResult , GetFieldWidth(nField)) ;
      
    Result := nResult ;
  End ;  { TxBase.CalcRecordSize }


{***********************************************************************
*                                                                      *
*       TxBase.GetHeaderSize                                           *
*                                                                      *
*         Return the header size as a Integer.                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetHeaderSize : Integer ;
  Begin  { TxBase.GetHeaderSize }
    Result := GetFixedHeaderPtr^.dbfHeaderSize ;
  End ;  { TxBase.GetHeaderSize }


{***********************************************************************
*                                                                      *
*       TxBase.dbCorrectedHeaderSize                                   *
*                                                                      *
*         Return the header size as a word excluding the NULL byte     *
*       that some xBase pgms put at the end of the header.  Probably   *
*       an AsciiZ terminator, which we don't need.                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.dbfCorrectedHeaderSize : Integer ;
  Begin  { TxBase.dbfCorrectedHeaderSize }
    If GetTerminator = $00 then
      Result := GetHeaderSize - 1
    Else
      Result := GetHeaderSize ;
  End ;  { TxBase.dbfCorrectedHeaderSize }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoHeaderSize                                       *
*                                                                      *
*         Return the memo header size as a Integer.                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoHeaderSize : Integer ;
  Begin  { TxBase.GetMemoHeaderSize }
    Result := dbfMemoHeaderSize ;
  End ;  { TxBase.GetMemoHeaderSize }


{***********************************************************************
*                                                                      *
*       TxBase.SetHeaderSize                                           *
*                                                                      *
*         Set the header size.                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetHeaderSize(nHeaderSize : Word) ;
  Begin  { TxBase.SetHeaderSize }
    GetFixedHeaderPtr^.dbfHeaderSize := nHeaderSize ;
  End ;  { TxBase.SetHeaderSize }


{***********************************************************************
*                                                                      *
*       TxBase.GetNumericFieldMask                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetNumericFieldMask(nField : Integer) : String ;
  Var
    nWidth  ,
    nDecimals : Byte ;

  Begin  { TxBase.GetNumericFieldMask }
    nWidth    := GetFieldWidth(nField)    ;
    nDecimals := GetFieldDecimals(nField) ;

    If nDecimals = 0 then
      Result := IntToStr(nWidth)
    Else
      Result := IntToStr(nWidth {- nDecimals - 1}) + '.' + IntToStr(nDecimals) ;
    Result := '%' + Result + 'f' ;
  End ;  { TxBase.GetNumericFieldMask }


{***********************************************************************
*                                                                      *
*       TxBase.GetTotalRecords                                         *
*                                                                      *
*       Return the total number of records in the data file.           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetTotalRecords : Integer ;
  Begin  { TxBase.GetTotalRecords }
    Result := GetFixedHeaderPtr^.dbTotalRecords ;
  End ;  { TxBase.GetTotalRecords }


{***********************************************************************
*                                                                      *
*       TxBase.GetRecordCount                                          *
*                                                                      *
*         Alias function for TxBase.GetTotalRecords.                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetRecordCount : Integer ;
  Begin  { TxBase.GetRecordCount }
    Result := GetTotalRecords ;
  End ;  { TxBase.GetRecordCount }


{***********************************************************************
*                                                                      *
*       TxBase.CalcTotalRecords                                        *
*                                                                      *
*         Calculate the total number of records in the data file.      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CalcTotalRecords : Integer ;
  Begin  { TxBase.CalcTotalRecords }
    Try
      Result := (GetDataFileSize - GetHeaderSize) div GetRecordSize ;
    Except
      Result := 0 ;
    End ;
  End ;  { TxBase.CalcTotalRecords }


{***********************************************************************
*                                                                      *
*       TxBase.SetTotalRecords                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetTotalRecords(nRecs : Integer) ;
  Begin  { TxBase.SetTotalRecords }
    GetFixedHeaderPtr^.dbTotalRecords := nRecs ;
  End ;  { TxBase.SetTotalRecords }


{***********************************************************************
*                                                                      *
*       TxBase.IncTotalRecords                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.IncTotalRecords(nRecs : Integer) ;
  Begin  { TxBase.IncTotalRecords }
    SetTotalRecords(GetTotalRecords + nRecs) ;
  End ;  { TxBase.IncTotalRecords }


{***********************************************************************
*                                                                      *
*       TxBase.GetDataFileVar                                          *
*                                                                      *
*         Return the data file variable.                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDataFileVar : pTFileStreamX ;
  Begin  { TxBase.GetDataFileVar }
    With GetDataAreaPtr^ do
      Result := @dbfFileVar ;
  End ;  { TxBase.GetDataFileVar }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFileVar                                          *
*                                                                      *
*         Return the memo file variable.                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFileVar : pTFileStreamX ;
  Begin  { TxBase.GetMemoFileVar }
    With GetDataAreaPtr^ do
      Result := @dbfMemoFileVar ;
  End ;  { TxBase.GetMemoFileVar }


{***********************************************************************
*                                                                      *
*       TxBase.dbfInternalCalculations                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfInternalCalculations ;
  Var
    nField : Integer ;

  Begin  { TxBase.dbfInternalCalculations }
    With GetDataAreaPtr^ do
      Begin
        FillChar(dbfNewFieldType  , SizeOf(dbfNewFieldType)  , 0) ;
        FillChar(dbfNewFieldWidth , SizeOf(dbfNewFieldWidth) , 0) ;

        For nField := 1 to GetFieldCount do
          dbfCalcNewFieldType(nField , dbfNewFieldType[nField] ,
                                       dbfNewFieldWidth[nField] ) ;
      End ;
  End ;  { TxBase.dbfInternalCalculations }


{***********************************************************************
*                                                                      *
*       TxBase.ResetShortFileName                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ResetShortFileName : TFileName ;
  Var
    bWasOpen : Boolean ;

  Begin  { TxBase.ResetShortFileName }
    bWasOpen := DataFileIsOpen ;
    If bWasOpen then
      dbfCloseDataFileVar ;

    With GetDataAreaPtr^ do
      Begin
        dbfShortDataFileName := bcFileUtilities.ShortFileName(GetDataFileName) ;
        Result := dbfShortDataFileName ;
      End ;

    If bWasOpen then
      dbfOpenDataFileVar ;
  End ;  { TxBase.ResetShortFileName }


{***********************************************************************
*                                                                      *
*       TxBase.dbfOpenDataFileVar                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfOpenDataFileVar ;
  Begin  { TxBase.dbfOpenDataFileVar }
    With GetDataAreaPtr^ do
      Begin
        Try
          dbfFileVar := TFileStreamX.Create(dbfFileName , fmOpenReadWrite) ;
          dbfOpened := True ;
        Except
          Begin
            dbfOpened := False ;
            MessageDlg('Error opening ' + dbfFileName + ' in TxBase.dbfOpenFileVar',
                       mtError ,
                       [mbOK]  ,
                       0        ) ;
          End ;
        End ;
      End ;
  End ;  { TxBase.dbfOpenDataFileVar }


{***********************************************************************
*                                                                      *
*       TxBase.dbfCloseDataFileVar                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfCloseDataFileVar ;
  Begin  { TxBase.dbfCloseDataFileVar }
Exit ;  
    With dbfDataArea do
      Try
        dbfFileVar.Free ;
//        dbfFileVar := nil ;

//        dbfOpened := False ;
      Except
        // ShowErrorMessage('Error closing data file.') ;
      End ;
  End ;  { TxBase.dbfCloseDataFileVar }


{***********************************************************************
*                                                                      *
*       TxBase.dbfCloseMemoFileVar                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfCloseMemoFileVar ;
  Begin  { TxBase.dbfCloseMemoFileVar }
    If HasMemoFile then
      With dbfDataArea do
        Try
          If MemoFileIsOpen then
            GetMemoFilePtr^.Free ;
          dbfMemoOpened := False ;
        Except
          ShowErrorMessage('Error closing memo file.') ;
        End ;
  End ;  { TxBase.dbfCloseMemoFileVar }


{***********************************************************************
*                                                                      *
*       TxBase.OpenCurrentFile                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.OpenCurrentFile ;
  Begin  { TxBase.OpenCurrentFile }
    With GetDataAreaPtr^ do
      If not dbfOpened then
        Begin
          If not bcFileUtilities.FileExists(dbfFileName) then
            Begin
              dbfOpened := False ;
              Exit ;
            End ;

          Try
            dbfFileVar := TFileStreamX.Create(dbfFileName , fmOpenReadWrite) ;
            dbfOpened := True ;
          Except
            Begin
              dbfOpened := False ;
              MessageDlg('Error opening ' + dbfFileName + ' in TxBase.OpenCurrentFile' ,
                         mtError ,
                         [mbOK]  ,
                         0        ) ;
            End ;
          End ;

          If dbfOpened then
            Begin
              If GetDataFileSize < GetMinDataFileSize then
                Begin
                  dbfOpened := False ;
                  CloseFiles ;
                  ShowMessage(GetDatafileName + ' under minimum length.') ;
                  Exit ;
                End ;
            End ;

          If not ReadHeader then
            Begin
              ShowMessage('Header could not be read') ;
              dbfOpened := False ;
              Exit ;
            End ;

          If dbfOpened then
            dbfConvertHeader { Convert the header information. }
          Else
            Begin
              dbfOpened := False ;
              Exit ;
            End ;

          dbfSetMemoFile ;

          dbfMemoOpened := False ;
          dbfOpenMemoFile ;  // Open the memo file if there is one.

          dbfMemoFldScanned := False ;

          dbfAllocateInternalRecord ;
        End ;  { With dbDataArea do }
  End ;  { TxBase.OpenCurrentFile }


{***********************************************************************
*                                                                      *
*       TxBase.dbfOpenMemoFile                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.dbfOpenMemoFile : Boolean ;
  Begin  { TxBase.dbfOpenMemoFile }
    With GetDataAreaPtr^ do
      Begin
        If dbfMemoFile then
          Begin
            If Length(dbfMemoFileName) = 0 then
              dbfMemoFileName := ForceExtension(dbfFileName , dbfMemoFileExt) ;

            dbfMemoOpened := False ;
            If bcFileUtilities.FileExists(dbfMemoFileName) then
              Try
                dbfMemoFileVar := TFileStreamX.Create(dbfMemoFileName , fmOpenRead) ;
                dbfMemoOpened := True ;
              Except
                dbfMemoOpened := False ;
              End ;

            If dbfMemoOpened then
              Begin
                dbfMemoFileSize := dbfMemoFileVar.Size ;
                dbfMemoOpened := ReadMemoHeader ;
              End ;
          End ;

        Result := dbfMemoOpened ;
      End ;
  End ;  { TxBase.dbfOpenMemoFile }
  

{***********************************************************************
*                                                                      *
*       TxBase.dbfAllocateInternalRecord                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfAllocateInternalRecord ;
  Begin  { TxBase.dbfAllocateInternalRecord }
    With GetDataAreaPtr^ do
      dbfCurrentRecordPtr := @dbfCurrRecordBuffer ;
  End ;  { TxBase.dbfAllocateInternalRecord }


{***********************************************************************
*                                                                      *
*       TxBase.CreateCurrentFile                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.CreateCurrentFile ;
  Begin  { TxBase.CreateCurrentFile }
    With GetDataAreaPtr^ do
      Try
        dbfFileVar := TFileStreamX.Create(dbfFileName , fmCreate) ;
        dbfOpened := True ;
      Except
        dbfOpened := False ;
      End ;
  End ;  { TxBase.CreateCurrentFile }

  
{***********************************************************************
*                                                                      *
*       TxBase.CloseFiles                                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.CloseFiles ;
  Begin  { TxBase.CloseFiles }
    With GetDataAreaPtr^ do
      Begin
        Try
          dbfCloseDataFileVar ;
          dbfCloseMemoFileVar ;
        Except ;
          //
        End ;

        With dbfReadMulti do
          If dbfMultiRecordPtr = nil then
            FreeMemCheck(dbfMultiRecordPtr , dbfMultiBufferSize) ;

        With dbfWriteMulti do
          If dbfMultiRecordPtr = nil then
            FreeMemCheck(dbfMultiRecordPtr , dbfMultiBufferSize) ;

        dbfMemoOpened := False ;
        dbfOpened     := False ;
      End ;
  End ;  { TxBase.CloseFiles }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldCount                                           *
*                                                                      *
*         Return the number of fields.                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldCount : Integer ;
  Begin  { TxBase.GetFieldCount }
    Result := GetDataAreaPtr^.dbfFieldCount ;
  End ;  { TxBase.GetFieldCount }


{***********************************************************************
*                                                                      *
*       TxBase.SetFieldCount                                           *
*                                                                      *
*         Set the number of fields.                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.SetFieldCount(nFlds : Integer ) : Integer ;
  Begin  { TxBase.SetFieldCount }
    GetDataAreaPtr^.dbfFieldCount := nFlds ;
    Result := nFlds ;
  End ;  { TxBase.SetFieldCount }


{***********************************************************************
*                                                                      *
*       TxBase.CalcFileSize                                            *
*                                                                      *
*         Return the file size as a Integer.                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CalcFileSize : Integer ;
  Begin  { TxBase.CalcFileSize }
    Result := GetHeaderSize + (GetRecordSize * GetTotalRecords)
  End ;  { TxBase.CalcFileSize }


{***********************************************************************
*                                                                      *
*       TxBase.GetDataFileSize                                         *
*                                                                      *
*         Return the data file size in bytes.                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDataFileSize : Integer ;
  Begin  { TxBase.GetDataFileSize }
    Try
      Result := dbfDataArea.dbfFileVar.Size ;
    Except
      Result := 0 ;
    End ;
  End ;  { TxBase.GetDataFileSize }


{***********************************************************************
*                                                                      *
*       TxBase.DataFileIsOpen                                          *
*                                                                      *
*         Return true if the data file was successfully opened.        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.DataFileIsOpen : Boolean ;
  Begin  { TxBase.DataFileIsOpen }
    Result := dbfDataArea.dbfOpened ;
  End ;  { TxBase.DataFileIsOpen }


{***********************************************************************
*                                                                      *
*       TxBase.MemoFileIsOpen                                          *
*                                                                      *
*         Return true if the data file was successfully opened.        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.MemoFileIsOpen : Boolean ;
  Begin  { MemoFileIsOpen }
    Result := dbfDataArea.dbfMemoOpened ;
  End ;  { MemoFileIsOpen }


{***********************************************************************
*                                                                      *
*       TxBase.GetDbfPtr                                               *
*                                                                      *
*         Return pointer to current xBase object.                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetDbfPtr : pTxBase ;
  Begin  { TxBase.GetDbfPtr }
    Result := @Self ;
  End ;  { TxBase.GetDbfPtr }


{***********************************************************************
*                                                                      *
*       TxBase.dbfConvertHeader                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfConvertHeader ;
  Begin  { TxBase.dbfConvertHeader }
    DateParInitialize ;

    With GetDataAreaPtr^ , GetFixedHeaderPtr^ do
      Begin
        dbfVersion  := Signature and $07 ;
        dbfSQLTable := ((Signature and $70) shr 4) > 0 ;

        dbfMemoFileExt := GetMemoFileExtension ;

        dbfSetMemoVars ;

        GetLastUpdate(True) ;

        dbfMemoFile       := False ;  { Assume memo file does not exist. }
        dbfMemoFldScanned := False ;  { Memo fields have been scanned.   }

        dbfCalcFieldCount ;  { Calculate number of fields. }

        dbfFieldMaxWidth    := 0 ;
        dbfMemoFldCount     := 0 ;

        dbfCalcFieldOffsets ;
        CalcFileSize ;

        dbfCalcAllFieldTypes ;
        dbfBuildMemoFldList ;

        If (dbfMemoFldCount > 0) or IsFlexFile then
          GetMemCheck(dbfMemoBuffer , dbfMaxMemoSize) ;
      End ;  { With dbfHeader do }

    dbfSetupRestructure ;
  End ;  { TxBase.dbfConvertHeader }


{***********************************************************************
*                                                                      *
*       TxBase.dbfBuildMemoFldList                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfBuildMemoFldList ;
  Var
    nI : Integer ;
    
  Begin  { TxBase.dbfBuildMemoFldList }
    With GetDataAreaPtr^ , GetHeaderPtr^ do
      Begin
        dbfMemoFldCount := 0 ;
        FillChar(dbfMemoFldList , SizeOf(dbfMemoFldList) , 0) ;

        For nI := 1 to dbfFieldCount do
          Begin
            With dbfRecordDesc[nI] do
              Begin
                If (GetFieldType(nI) in MemoFieldTypes) then
                  Begin
                    Inc(dbfMemoFldCount) ;
                    dbfMemoFldList[dbfMemoFldCount] := nI ;
                  End ;

                dbfMemoFile := dbfMemoFile or (FType in MemoFieldTypes) ;

                dbfFieldMaxWidth := MaxInteger(GetFieldWidth(nI) , dbfFieldMaxWidth) ;
                If Length(GetFieldName(nI)) > GetNameMaxWidth then
                  Begin
                    SetNameMaxWidth(Length(GetFieldName(nI))) ;
                    dbfNameMaxWidthFld := nI ;
                  End ;
              End ;
          End ;

        dbfMemoFile := (dbfMemoFile or IsFlexFile) ;
      End ;
  End ;  { TxBase.dbfBuildMemoFldList }


{***********************************************************************
*                                                                      *
*       TxBase.dbfCheckHeader                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfCheckHeader ;
  Var
    nField : Integer ;

  Begin  { TxBase.dbfCheckHeader }
    For nField := 1 to GetFieldCount do
      If GetFieldType(nField) in NoDecimalFieldTypes then
        If GetFieldDecimals(nField) <> 0 then
          Begin
          End ;
  End ;  { TxBase.dbfCheckHeader }


{***********************************************************************
*                                                                      *
*       TxBase.ScanMemoFields                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.ScanMemoFields ;
  Var
    nI ,
    nJ ,
    nMemo : Integer ;
    cType : Char    ;
    cStr  : String  ;

  Begin  { TxBase.ScanMemoFields }
    With GetDataAreaPtr^ do
      Begin
        If dbfMemoFldScanned then
          Exit
        Else
          Begin
            dbfMemoFields := TStringList.Create ;
            dbfMemoFields.Clear ;
            dbfMemoFields.Sorted := False ;

            dbfMemoIntFields := TIntegerList.Create ;
            dbfMemoIntFields.Clear ;

            dbfMemoFieldTypes := TStringList.Create ;
            dbfMemoFieldTypes.Clear ;

            For nI := 1 to GetTotalRecords do
              Begin
                SetCurrentRecord(nI) ;
                If GetRecord(nI) then
                  Begin
                    For nJ := 1 to dbfMemoFldCount do
                      Begin
                        nMemo := GetCurrentMemoNumber(dbfMemoFldList[nJ]) ;
                        If nMemo > 0 then
                          Begin
                            cStr := IntToStr(nMemo) ;
                            cType := GetFieldType(dbfMemoFldList[nJ]) ;
                            cStr := PadLeft(cStr , 9) + ' ' + cType ;

                            dbfMemoFields.Add(cStr) ;
                            dbfMemoIntFields.Add(dbfMemoFldList[nJ]) ;
                          End ;
                      End ;
                  End ;
              End ;

            dbfMemoFldScanned := True ;
          End ;

        dbfMemoFields.Sort ;

        With dbfMemoFields do
          Begin
            For nJ := 0 to (Count - 1) do
              Begin
                cStr := dbfMemoFields.Strings[nJ] ;
                If Length(cStr) >= 11 then
                  Begin
                    cType := cStr[11] ;
                    dbfMemoFieldTypes.Add(cType) ;
                    Strings[nJ] := Copy(Strings[nJ] , 1 , 11) ;
                  End
                Else
                  ShowErrorMessage('TxBase.ScanMemoFields : Length problem with [' + cStr + ']  ' +
                                   IntToStr(Length(cStr))) ;
              End ;
          End ;
      End ;
  End ;  { TxBase.ScanMemoFields }


{***********************************************************************
*                                                                      *
*       TxBase.SetMemoFileName                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetMemoFileName(F : String) ;
  Begin  { TxBase.SetMemoFileName }
    With GetDataAreaPtr^ do
      Begin
        dbfMemoFileName := UpperTrim(F) ;
        dbfSetMemoFile ;
        If dbfMemoFile then
          If Length(dbfMemoFileName) = 0 then
            dbfMemoFile := False ;
      End ;
  End ;  { TxBase.SetMemoFileName }

{***********************************************************************
*                                                                      *
*       TxBase.SetCurrentMemoField                                     *
*                                                                      *
*         Set the string represention of a memo field.                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.SetCurrentMemoField(nMemo  : Integer ;
                                    nField : Integer  ) : Boolean ;
  Begin  { TxBase.SetCurrentMemoField }
    Result := SetMemoField(GetRecordPtr ,
                             nMemo      ,
                             nField      ) ;
  End ;  { TxBase.SetCurrentMemoField }


{***********************************************************************
*                                                                      *
*       TxBase.SetMemoField                                            *
*                                                                      *
*         Set the string represention of a memo field.                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.SetMemoField(pRec   : Pointer ;
                             nMemo  : Integer ;
                             nField : Integer  ) : Boolean ;
  Var
    cType : Char ;
    cStr : String[dbfMemoFieldWidth] ;

  Begin  { TxBase.SetMemoField }
    Result := False ;

    If nMemo >= 0 then
      Begin
        If ValidFieldNumber(nField) then
          Begin
            cType := GetFieldType(nField) ;

            If (cType in MemoFieldTypes) then
              Begin
                Case GetFieldWidth(nField) of
                   SizeOf(SmallInt) ,
                   SizeOf(Integer)    :
                       Begin
                         If GetFieldWidth(nField) = SizeOf(Integer) then
                           Integer(GetFieldPtr(pRec , nField)^) := nMemo
                         Else
                           If GetFieldWidth(nField) = SizeOf(SmallInt) then
                             SmallInt(GetFieldPtr(pRec , nField)^) := nMemo ;
                         Result := True ;
                       End ;

                   dbfMemoFieldWidth :  { 10 }
                       Begin
                         If (nMemo = 0) then
                           cStr := kBlanks10
                         Else
                           cStr := PadLeftZero(nMemo , dbfMemoFieldWidth) ;
                         SetFieldStr(pRec , cStr , nField) ;
                         Result := True ;
                       End ;
                Else
                  Result := False ;
                End ;  { Case GetFieldWidth(nField) of }
              End ;
          End ;
      End ;
  End ;  { TxBase.SetMemoField }


{***********************************************************************
*                                                                      *
*       TxBase.WriteMemoHeader                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.WriteMemoHeader : Boolean ;
  Var
    nBytesToWrite ,
    nBytesWritten   : Integer ;

    cFile : String ;
    cShort : String ;

  Begin  { TxBase.WriteMemoHeader }
    cFile := GetShortDataFileName ;
    cShort := JustName(cFile) ;
    cShort := JustName(GetShortDataFileName) ;

    With GetDataAreaPtr^ do
      Begin
        With GetMemoHeaderPtr^ do
          Begin
            If IsDbase4 then
              Begin
                FillChar(dbfFileNameDB4 , SizeOf(dbfFileNameDB4) , 0) ;
                Move(cShort[1] , dbfFileNameDB4 , Length(cShort)) ;
              End ;
          End ;

        If dbfMemoOpened then
          Begin
            nBytesToWrite := MinInteger(SizeOf(dbfMemoHeader) ,
                                        dbfMemoFileSize       ) ;

            GetMemoFileVar^.SeekToStart ;
            nBytesWritten := GetMemoFileVar^.Write(dbfMemoHeader , nBytesToWrite) ;

            Result := (nBytesToWrite = nBytesWritten) ;
          End
        Else
          Result := False ;
      End ;
  End ;  { TxBase.WriteMemoHeader }


{***********************************************************************
*                                                                      *
*       TxBase.ReadMemoHeader                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ReadMemoHeader : Boolean ;
  Var
    bResult : Boolean ;

    nBytesRead   ,
    nBytesToRead   : Integer ;

  Begin  { TxBase.ReadMemoHeader }
    With GetDataAreaPtr^ do
      Begin
        { Make sure not to read more header bytes than the }
        { actual memo file size.                           }
        nBytesToRead := MinInteger(SizeOf(dbfMemoHeader) ,
                                   dbfMemoFileSize        ) ;

        Try
          nBytesRead := dbfMemoFileVar.Read(dbfMemoHeader , nBytesToRead) ;
          bResult := True ;
        Except
          Begin
            nBytesRead := 0 ;
            bResult := False ;
            MessageDlg('Cannot read memo header in TxBase.ReadMemoHeader' ,
                       mtError ,
                       [mbOK]  ,
                       0        ) ;
          End ;
        End ;

        If bResult then
          bResult := (nBytesToRead >= nBytesRead) ;

        If bResult then
          Begin
//            dbfSetMemoVars ;

            dbfMemoHeaderBytes := nBytesRead ;
          End ;
      End ;

    Result := bResult ;
  End ;  { TxBase.ReadMemoHeader }

  
{***********************************************************************
*                                                                      *
*       TxBase.ReadMemo                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ReadMemo(nMemoNo  : Integer ;
                         nFieldNo : Integer  ) : Integer ;
  Var
    nBlocks ,
    nPosEOF ,
    nBytesRead : Integer ;
    bFinished  : Boolean ;
    nOffset    : Integer ;
    nMemoLen   : Integer ;

    pCurr : Pointer ;

  Begin  { TxBase.ReadMemo }
    If IsMemoFileOpen then
      Case GetMemoBlockType of
        dbfDB4Memo : Result := ReadMemoDB4(nMemoNo , nFieldNo) ;
        dbfFPTMemo : Result := ReadMemoFPT(nMemoNo , nFieldNo) ;
        dbfDBTMemo :
          Begin
            With GetDataAreaPtr^ do
              Begin
                bFinished := False ;
                nBlocks := 0 ;

                nOffset := nMemoNo * GetMemoBlockLen ;
                dbfMemoFileVar.SeekToPos(nOffset) ;

                Repeat
                  pCurr := Pointer(Integer(dbfMemoBuffer)  + (nBlocks * GetMemoBlockLen)) ;
                  Try
                    nBytesRead := dbfMemoFileVar.Read(pCurr^ , GetMemoBlockLen) ;
                  Except
                    Begin
                      MessageDlg('Memo stream read failed in TxBase.ReadMemo.' ,
                                 mtError ,
                                 [mbOK]  ,
                                 0        ) ;
                      nBytesRead := 0 ;
                    End ;
                  End ;

                  If nBytesRead > 0 then
                    Inc(nBlocks) ;

                  nPosEOF := ScanBufferChar(pCurr           ,
                                            #$1A            ,
                                            GetMemoBlockLen  ) ;
                  If (nPosEOF > 0) or (nBytesRead = 0) then
                    bFinished := True ;
                Until bFinished ;

                If nPosEOF > 0 then
                  nMemoLen := ((nBlocks - 1) * GetMemoBlockLen) + nPosEOF + 1
                Else
                  nMemoLen := 0 ;

                FillChar(dbfMemoDesc , SizeOf(dbfMemoDesc) , 0) ;
                dbfMemoDesc.nWidth := nMemoLen ;
              End ;

            Result := nMemoLen ;
          End ;
      Else
        Result := 0 ;
      End  { Case GetMemoBlockType of }
    Else
      Result := 0 ;
  End ;  { TxBase.ReadMemo }


{***********************************************************************
*                                                                      *
*       TxBase.ReadMemoFPT                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ReadMemoFPT(nMemoNo  : Integer ;
                            nFieldNo : Integer  ) : Integer ;
  Var
    nBytesRead ,
    nOffset      : Integer ;

  Begin  { TxBase.ReadMemoFPT }
    With GetDataAreaPtr^ do
      Begin
        nOffset := nMemoNo * GetMemoBlockLen ;
        dbfMemoFileVar.SeekToPos(nOffset) ;
        Try
          { nBytesRead := } dbfMemoFileVar.Read(dbfMemoDesc , SizeOf(dbfMemoDesc)) ;
        Except
          Begin
            MessageDlg('Stream read(1) failed in ReadMemoFPT' ,
                       mtError ,
                       [mbOK]  ,
                       0        ) ;
            Result := -1 ;
            Exit ;
          End ;
        End ;

        With dbfMemoDesc do
          Begin
            nType  := SwapInteger(nType)  ;
            nWidth := SwapInteger(nWidth) ;
          End ;

        FillChar(dbfMemoBuffer^ , dbfMaxMemoSize , $1A) ;
        Try
          nBytesRead := dbfMemoFileVar.Read(GetMemoBufferPtr(nFieldNo)^ ,
                                            GetMemoBufferLen(nFieldNo)   ) ;
        Except
          nBytesRead := 0 ;
        End ;

        Result := nBytesRead ;
      End ;
  End ;  { TxBase.ReadMemoFPT }


{***********************************************************************
*                                                                      *
*       TxBase.ReadMemoDB4                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ReadMemoDB4(nMemoNo  : Integer ;
                            nFieldNo : Integer  ) : Integer ;
  Var
    nBytesRead ,
    nOffset      : Integer ;
    nMemoLen     : Integer ;

  Begin  { TxBase.ReadMemoDB4 }
    With GetDataAreaPtr^ do
      Begin
        nOffset := nMemoNo * GetMemoBlockLen ;
        dbfMemoFileVar.SeekToPos(nOffset) ;
        Try
          nBytesRead := dbfMemoFileVar.Read(dbfMemoDesc , SizeOf(dbfMemoDesc)) ;
        Except
          nBytesRead := 0 ;
        End ;

        If nBytesRead = SizeOf(dbfMemoDesc) then
          Begin
            nMemoLen := dbfMemoDesc.nWidth ;

            FillChar(dbfMemoBuffer^ , dbfMaxMemoSize , $1A) ;
            Try
              nBytesRead := dbfMemoFileVar.Read(GetMemoBufferPtr(nFieldNo)^ ,
                                                nMemoLen                     ) ;
            Except
              nBytesRead := 0 ;
            End ;

            Result := nBytesRead ;
          End
        Else
          Result := 0 ;
      End ;
  End ;  { TxBase.ReadMemoDB4 }


{***********************************************************************
*                                                                      *
*       TxBase.ReadMemoHeaderDBV                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ReadMemoHeaderDBV(nFieldNo : Integer) : Boolean ;
  Var
    nBytesRead ,
    nOffset      : Integer ;

  Begin  { TxBase.ReadMemoHeaderDBV }
    With GetDataAreaPtr^ do
      Begin
        FillChar(dbfMemoDescDBV , SizeOf(dbfMemoDescDBV) , 0) ;

        nOffset := GetMemoOffsetDBV(GetRecordPtr , nFieldNo) ;

        { Check for memo existence.  An offset of 538976288 }
        { is equivalent to 'bbbbbb' (6 blanks).             }
        If (nOffset = 538976288) or (nOffset < 1) then
          Begin
            Result := False ;
            Exit ;
          End
        Else
          Begin
            Try
              dbfMemoFileVar.SeekToPos(nOffset) ;

              nBytesRead := dbfMemoFileVar.Read(dbfMemoDescDBV         ,
                                                SizeOf(dbfMemoDescDBV)  ) ;
              Result := (nBytesRead = SizeOf(dbfMemoDescDBV))
            Except
              Begin
                FillChar(dbfMemoDescDBV , SizeOf(dbfMemoDescDBV) , 0) ;
                Result := False ;
              End ;
            End ;
          End ;
      End ;
  End ;  { TxBase.ReadMemoHeaderDBV }

  
{***********************************************************************
*                                                                      *
*       TxBase.ReadMemoDBV                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ReadMemoDBV(nFieldNo : Integer) : Boolean ;
  Var
    nSize      ,
    nBytesRead ,
    nOffset      : Integer ;

  Begin  { TxBase.ReadMemoDBV }
    With GetDataAreaPtr^ do
      Begin
        nOffset := GetMemoOffsetDBV(GetRecordPtr , nFieldNo) ;

        If ReadMemoHeaderDBV(nFieldNo) then
          Begin
            nSize := dbfMemoDescDBV.Size ;
            If (nSize < 1) or (nSize >= GetMemoFileSize) then
              Begin
                SetMemoBytesRead(0) ;
                Result := False ;
              End
            Else
              Begin
                FillMemoBuffer(nFieldNo ,  $00) ;
                dbfMemoFileVar.SeekToPos(nOffset + SizeOf(dbfMemoDescDBV)) ;
                Try
                  nBytesRead := dbfMemoFileVar.Read(GetMemoBufferPtr(nFieldNo)^ ,
                                                    nSize                        ) ;
                Except
                  Begin
                    MessageDlg('Stream read(2) failed in ReadMemoDBV' ,
                               mtError ,
                               [mbOK]  ,
                               0        ) ;
                    Result := False ;
                    Exit ;
                  End ;
                End ;

                SetMemoBytesRead(nBytesRead) ;

                Result := (nBytesRead = nSize) ;
              End ;
          End
        Else
          Begin
            SetMemoBytesRead(0) ;
            Result := False ;
          End ;
      End ;
  End ;  { TxBase.ReadMemoDBV }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoBytesRead                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoBytesRead : Integer ;
  Begin  { TxBase.GetMemoBytesRead }
    Result := GetDataAreaPtr^.dbfMemoLength ;
  End ;  { TxBase.GetMemoBytesRead }


{***********************************************************************
*                                                                      *
*       TxBase.SetMemoBytesRead                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetMemoBytesRead(nBytesRead : Integer) ;
  Begin  { TxBase.SetMemoBytesRead }
    GetDataAreaPtr^.dbfMemoLength := nBytesRead ;
  End ;  { TxBase.SetMemoBytesRead }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoDescriptor                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoDescriptor(nMemo : Integer) : Integer ;
//  Var
//    nBytesRead : Integer ;

  Begin  { TxBase.GetMemoDescriptor }
    With GetDataAreaPtr^ do
      Begin
        SeekMemo(nMemo) ;

        Try
          { nBytesRead := } dbfMemoFileVar.Read(dbfMemoBMPDesc         ,
                                                SizeOf(dbfMemoBMPDesc)  ) ;
          Result := SwapInteger(dbfMemoBMPDesc.MemoType) ;

        Except
          Begin
            ShowMessage('Stream read failure in TxBase.GetMemoDescriptor') ;
            Result := -1 ;
          End ;
        End ;
      End ;
  End ;  { TxBase.GetMemoDescriptor }


{***********************************************************************
*                                                                      *
*       TxBase.GetOleMemoDescriptor                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetOleMemoDescriptor(nMemo : Integer) : Integer ;
//  Var
//    nBytesRead : Integer ;

  Begin  { TxBase.GetOleMemoDescriptor }
    With GetDataAreaPtr^ do
      Begin
        SeekMemo(nMemo) ;

        Try
          {nBytesRead := } dbfMemoFileVar.Read(dbfMemoOleDesc         ,
                                               SizeOf(dbfMemoOleDesc)  ) ;
          Result := SwapInteger(dbfMemoOleDesc.MemoType) ;
        Except
          Begin
            ShowMessage('Stream read failure in TxBase.GetOleMemoDescriptor') ;
            Result := -1 ;
          End ;
        End ;
      End ;
  End ;  { TxBase.GetOleMemoDescriptor }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoLength                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoLength : Integer ;
  Var
    nType : Integer ;

  Begin  { TxBase.GetMemoLength }
    With GetDataAreaPtr^ , dbfMemoBMPDesc do
      Begin
        nType := SwapInteger(dbfMemoBMPDesc.MemoType) ;

        Case nType of
          1 : Result := SwapInteger(MemoLengthBMP)    ;
          2 : Result := SwapInteger(MemoLengthBinary) ;
        Else
          Result := 0 ;
          ShowMessage('Unknown memo type header.') ;
        End ;
      End ;
  End ;  { TxBase.GetMemoLength }


{***********************************************************************
*                                                                      *
*       TxBase.SeekMemo                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.SeekMemo(nMemo : Integer) : Boolean ;
  Var
    nPos : Int64 ;

  Begin  { TxBase.SeekMemo }
    With GetDataAreaPtr^ do
      Try
        nPos := dbfMemoFileVar.SeekToPos(GetMemoOffset(nMemo)) ;

        Result := (nPos = GetMemoOffset(nMemo)) ;
      Except
        Result := False ;
      End ;
  End ;  { TxBase.SeekMemo }


{***********************************************************************
*                                                                      *
*       TxBase.GetOleOffset                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetOleOffset : Integer ;
  Type
    TPBrush = Record
                cSig      : Array[1..6] of Char ;
                xUnknown1 : Array[1..5] of Byte ;
                nPBLen    : Byte                ;
                xUnknown2 : Array[1..7] of Byte ;
              End ;

  Const
    nBrushSize = SizeOf(TPBrush) ;

  Var
    pRec : Pointer ;
    pPtr : Pointer ;
    nLen : Integer ;
    cStr : String  ;


  Begin  { TxBase.GetOleOffset }
    With GetDataareaPtr^ , dbfMemoOleDesc do
      pPtr := @OleFoxPro ;

    pRec := pPtr ;

    cStr := MakeStr(pPtr , 4096) ;
    pPtr := Pointer(Integer(pPtr) + Length(cStr) + 1) ;

    pPtr := Pointer(Integer(pPtr) + SizeOf(TPBrush)) ;
    nLen := TPBrush(pPtr^).nPBLen ;
    nLen := nLen + nBrushSize ;
    pPtr := Pointer(Integer(pPtr) + nLen) ;

    Result := Integer(pPtr) - Integer(pRec) + 28 ;
  End ;  { TxBase.GetOleOffset }


{***********************************************************************
*                                                                      *
*       TxBase.GetOleLength                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetOleLength : Integer ;
  Type
    TPBrush = Record
                cSig      : Array[1..6] of Char ;
                xUnknown1 : Array[1..5] of Byte ;
                nPBLen    : Byte                ;
                xUnknown2 : Array[1..7] of Byte ;
              End ;

  Const
    nBrushSize = SizeOf(TPBrush) ;

  Var
    pPtr : Pointer ;
    nLen : Integer ;
    cStr : String  ;


  Begin  { TxBase.GetOleLength }
    With GetDataareaPtr^ , dbfMemoOleDesc do
      pPtr := @OleFoxPro ;

    cStr := MakeStr(pPtr , 4096) ;
    pPtr := Pointer(Integer(pPtr) + Length(cStr) + 1) ;

    pPtr := Pointer(Integer(pPtr) + SizeOf(TPBrush)) ;
    nLen := TPBrush(pPtr^).nPBLen ;
    nLen := nLen + nBrushSize ;
    pPtr := Pointer(Integer(pPtr) + nLen + 2) ;

    Result := Word(pPtr^) ;
  End ;  { TxBase.GetOleLength }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.ReadMemoBMP                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.ReadMemoBMP(nMemoNo : Integer) ;
  Var
    nBlocks        ,
    nBytesRead     ,
    nBytesToRead   ,
    nBytesWritten  ,
    nMemoLen       ,
    nOffset          : Integer ;
    cType            : Char    ;

    fVar : TFileStreamX ;

  Begin  { TxBase.ReadMemoBMP }
    If nMemoNo < 1 then
      Exit ;

    nBytesRead := 0 ;

    cType := GetFieldType(GetFieldChoice) ;

    With GetDataAreaPtr^ do
      Begin
        If cType = 'G' then
          Begin
            GetOleMemoDescriptor(nMemoNo) ;
            dbfMemoOleDesc.OleLength := GetOleLength ;
            nMemoLen := dbfMemoOleDesc.OleLength ;
          End
        Else
          Begin
            GetMemoDescriptor(nMemoNo) ;
            nMemoLen := dbfMemoBMPDesc.MemoLengthBMP ;
          End ;

        nBlocks  := (nMemoLen div GetMemoBlockLen) ;
        If nMemoLen > (nBlocks * GetMemoBlockLen) then
          Inc(nBlocks) ;

        nOffset   := nMemoNo * GetMemoBlockLen ;
        dbfBMPLen := nMemoLen ;

        GetMemCheck(dbfBmpPtr , dbfBmpLen) ;
        If cType = 'G' then
          Begin
            dbfMemoFileVar.SeekToPos(nOffset + GetOleOffset) ;
            nBytesToRead := (nBlocks * GetMemoBlockLen) ;
          End
        Else
          Begin
            dbfMemoFileVar.SeekToPos(nOffset + 16) ;
            nBytesToRead := (nBlocks * GetMemoBlockLen) - 16  ;
          End ;

        If nBytesToRead < 1 then
          Begin
            nBytesRead := 0 ;
            ShowErrorMessage('nBytesToRead = ' + IntToStr(nBytesToRead)) ;
          End
        Else
          Try
            nBytesRead := dbfMemoFileVar.Read(dbfBmpPtr^ , nBytesToRead) ;
          Except
            Begin
              ShowMessage('Stream read(2) failed in ReadMemoBMP '  + Chr(13) + Chr(10) +
                          'Offset        = ' + IntToStr(nOffset)          + Chr(13) + Chr(10) +
                          'Bytes To read = ' + IntToStr(nBytesToRead)     + Chr(13) + Chr(10) +
                          'Bytes read    = ' + IntToStr(nBytesRead)       + Chr(13) + Chr(10) +
                          'nBlocks       = ' + IntToStr(nBlocks)          + Chr(13) + Chr(10) +
                          'MemoBlockSize = ' + IntToStr(GetMemoBlockLen) + Chr(13) + Chr(10)  ) ;
              nBytesRead := 0 ;
            End ;
          End ;

        If nBytesRead > 0 then
          Begin
            dbfBMPHeight := pBmpFileHeader(dbfBmpPtr)^.bfhHeight ;
            dbfBMPWidth  := pBmpFileHeader(dbfBmpPtr)^.bfhWidth  ;

            fVar := TFileStreamX.Create(dbfBlobFile , fmCreate) ;
            nBytesWritten := fVar.Write(dbfBmpPtr^ , dbfBmpLen) ;
            FreeAndNil(fVar) ;
            If nBytesWritten <> dbfBmpLen then
              MessageDlg('Invalid number of bytes written for [' +
                         dbfBlobFile + ']' + CRLF +
                         'Bytes = ' + IntToStr(dbfBmpLen) +
                         '  Actual = ' + IntToStr(nBytesWritten) ,
                         mtError ,
                         [mbOK]  ,
                         0        ) ;
          End
        Else
          Begin
            dbfBMPHeight := 0 ;
            dbfBMPWidth  := 0 ;
          End ;

        FreeMemCheck(dbfBmpPtr , dbfBmpLen) ;

        If nBytesRead > 0 then
          dbfBmp.LoadFromFile(dbfBlobFile) ;
      End ;
  End ;  { TxBase.ReadMemoBMP }


{***********************************************************************
*                                                                      *
*       TxBase.ReadMemoObject                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ReadMemoObject(nMemoNo : Integer) : String ;
  Var
    nBlocks        ,
    nBytesRead     ,
    nBytesToRead   ,
    nBytesWritten  ,
    nMemoLen       ,
    nOffset          : Integer ;

    fVar : TFileStreamX ;

  Begin  { TxBase.ReadMemoObject }
    If nMemoNo < 1 then
      Begin
        Result := '' ;
        Exit ;
      End ;

    nBytesRead := 0 ;

    With GetDataAreaPtr^ do
      Begin
        GetOleMemoDescriptor(nMemoNo) ;

        nMemoLen := dbfMemoOleDesc.OleLength ;
        nBlocks  := (nMemoLen div GetMemoBlockLen) ;
        If nMemoLen > (nBlocks * GetMemoBlockLen) then
          Inc(nBlocks) ;

        nOffset   := nMemoNo * GetMemoBlockLen ;
        dbfBMPLen := dbfMemoOleDesc.OleLength ;
        GetMemCheck(dbfBmpPtr , dbfBmpLen) ;
        dbfMemoFileVar.SeekToPos(nOffset + 65) ;
        nBytesToRead := (nBlocks * GetMemoBlockLen) - 16  ;

        If nBytesToRead < 1 then
          nBytesRead := 0
        Else
          Try
            nBytesRead := dbfMemoFileVar.Read(dbfBmpPtr^ , nBytesToRead) ;
          Except
            Begin
              ShowMessage('Stream read(2) failed in ReadMemoObject '  + Chr(13) + Chr(10) +
                          'Offset        = ' + IntToStr(nOffset)          + Chr(13) + Chr(10) +
                          'Bytes To read = ' + IntToStr(nBytesToRead)     + Chr(13) + Chr(10) +
                          'Bytes read    = ' + IntToStr(nBytesRead)       + Chr(13) + Chr(10) +
                          'nBlocks       = ' + IntToStr(nBlocks)          + Chr(13) + Chr(10) +
                          'MemoBlockSize = ' + IntToStr(GetMemoBlockLen) + Chr(13) + Chr(10)  ) ;
              nBytesRead := 0 ;
            End ;
          End ;

        If nBytesRead > 0 then
          Begin
            fVar := TFileStreamX.Create(dbfBlobFile , fmCreate) ;
            nBytesWritten := fVar.Write(dbfBmpPtr^ , dbfBmpLen) ;
            FreeAndNil(fVar) ;
            If nBytesWritten <> dbfBmpLen then
              MessageDlg('Invalid number of bytes written for [' +
                         dbfBlobFile + ']' + CRLF +
                         'Bytes = ' + IntToStr(dbfBmpLen) +
                         '  Actual = ' + IntToStr(nBytesWritten) ,
                         mtError ,
                         [mbOK]  ,
                         0        ) ;
          End ;

        FreeMemCheck(dbfBmpPtr , dbfBmpLen) ;

        If nBytesRead > 0 then
          dbfBmp.LoadFromFile(dbfBlobFile) ;
      End ;
  End ;  { TxBase.ReadMemoObject }


{***********************************************************************
*                                                                      *
*       TxBase.ReadMemoBinary                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ReadMemoBinary(nMemo : Integer) : String ;
  Var
    nBytesToRead ,
    nMemoLen       : Integer ;

  Begin  { TxBase.ReadMemoBinary }
    If nMemo < 1 then
      Exit ;

    With GetDataAreaPtr^ do
      Begin
        GetOleMemoDescriptor(nMemo) ;

        nMemoLen            := SwapInteger(dbfMemoOleDesc.OleLengthBinary) ;
        dbfBinaryMemoLength := nMemoLen ;
        nBytesToRead        := MinInteger(nMemoLen , dbfMaxMemoSize) ;
        dbfMemoLength       := nBytesToRead ;

        SeekMemo(nMemo) ;

        If nBytesToRead < 1 then
          Begin
            MessageDlg('Invalid number of bytes [' + IntToStr(nBytesToRead) +
                       '] to read in TxBase.ReadMemoBinary.' ,
                       mtError , [mbOK] , 0) ;
            Exit ;
          End ;

        Try
          { nBytesRead := } dbfMemoFileVar.Read(dbfMemoBuffer^ , nBytesToRead) ;
        Except
          ShowMessage('Stream read(2) failed in ReadMemoBinary'  + Chr(13) + Chr(10) +
                      'Bytes To read = ' + IntToStr(nBytesToRead)     + Chr(13) + Chr(10) +
                      'MemoBlockSize = ' + IntToStr(GetMemoBlockLen) + Chr(13) + Chr(10)  ) ;
        End ;
      End ;
  End ;  { TxBase.ReadMemoBinary }

  
{***********************************************************************
*                                                                      *
*       TxBase.GetMemoBMPPtr                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoBMPPtr : BitMapPtr ;
  Begin  { TxBase.GetMemoBMPPtr }
    With GetDataAreaPtr^ do
      Result := @dbfBmp ;
  End ;  { TxBase.GetMemoBMPPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoBMPLength                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoBMPLength : Integer ;
  Begin  { TxBase.GetMemoBMPLength }
    Result := GetDataAreaPtr^.dbfBmpLen ;
  End ;  { TxBase.GetMemoBMPLength }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoBufferPtr                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoBufferPtr(nField : Integer) : Pointer ;
  Begin  { TxBase.GetMemoBufferPtr }
    Case GetFieldType(nField) of
      'B' : Result := GetDataAreaPtr^.dbfBmpPtr     ;
      'G' : Result := GetDataAreaPtr^.dbfMemoBuffer ;
      'M' : Result := GetDataAreaPtr^.dbfMemoBuffer ;
    Else
      If IsFlexFile then
        Result := GetDataAreaPtr^.dbfMemoBuffer
      Else
        Result := GetDataAreaPtr^.dbfMemoBuffer ;
    End ;
  End ;  { TxBase.GetMemoBufferPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoBufferLen                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoBufferLen(nField : Integer) : Integer ;
  Begin  { TxBase.GetMemoBufferLen }
    Case GetFieldType(nField) of
      'B' : Result := GetDataAreaPtr^.dbfMemoBMPDesc.BMPLength ;
      'G' : Result := GetDataAreaPtr^.dbfMemoDesc.nWidth       ;
      'M' : Result := GetDataAreaPtr^.dbfMemoDesc.nWidth       ;
    Else
      If IsFlexFile then
        Result := GetDataAreaPtr^.dbfMemoDesc.nWidth
      Else
        Result := GetDataAreaPtr^.dbfMemoDesc.nWidth ;
    End ;
  End ;  { TxBase.GetMemoBufferLen }


{***********************************************************************
*                                                                      *
*       TxBase.FillMemoBuffer                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.FillMemoBuffer(nField : Integer ;
                                nFill  : Byte     ) ;
  Begin  { TxBase.FillMemoBuffer }
    FillChar(GetMemoBufferPtr(nField)^ , GetMemoBufferLen(nField) , nFill) ;
  End ;  { TxBase.FillMemoBuffer }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoOffset                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoOffset(nMemo : Integer) : Integer ;
  Begin  { TxBase.GetMemoOffset }
    Result := nMemo * GetMemoBlockLen ;
  End ;  { TxBase.GetMemoOffset }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoSize                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoSize(pPtr : Pointer) : Integer ;
  Var
    rMemoRec : Record
                 nMemoSignature : Integer ;
                 nMemoSize      : Integer ;
               End ;

  Begin  { TxBase.GetMemoSize }
    If (GetMemoBlockType = dbfDB4Memo) then
      Begin
        Move(pPtr^ , rMemoRec , SizeOf(rMemoRec)) ;
        Result := rMemoRec.nMemoSize ;
      End
    Else
      If (GetMemoBlockType = dbfFPTMemo) then
        Begin
          Move(pPtr^ , rMemoRec , SizeOf(rMemoRec)) ;
          Result := SwapInteger(rMemoRec.nMemoSize) ;
        End
      Else
        Begin
          Result := 0 ;

          While (Byte(pPtr^) <> $1A) do
            Begin
              Inc(Result) ;
              IncPointer(pPtr , 1) ;
            End ;
        End ;
  End ;   { TxBase.GetMemoSize }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFileSize                                         *
*                                                                      *
*         Return the memo file size in bytes.                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFileSize : Integer ;
  Begin  { TxBase.GetMemoFileSize }
    Result := GetDataAreaPtr^.dbfMemoFileSize ;
  End ;  { TxBase.GetMemoFileSize }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFileBlocks                                       *
*                                                                      *
*         Return the memo file size in blocks.                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFileBlocks : Integer ;
  Begin  { TxBase.GetMemoFileBlocks }
    Result := GetMemoFileSize div GetMemoBlockLen ;
  End ;  { TxBase.GetMemoFileBlocks }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoBlockType                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoBlockType : dbfMemoType ;
  Begin  { TxBase.GetMemoBlockType }
    Result := GetDataAreaPtr^.dbfMemoFormat ;
  End ;  { TxBase.GetMemoBlockType }


{***********************************************************************
*                                                                      *
*       TxBase.SetMemoBlockType                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetMemoBlockType(mType : dbfMemoType) ;
  Begin
    GetDataAreaPtr^.dbfMemoFormat := mType ;
  End ;


{***********************************************************************
*                                                                      *
*       TxBase.CalcMemoFileBlocks                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.CalcMemoFileBlocks : Integer ;
  Begin  { TxBase.CalcMemoFileBlocks }
    Result := RoundUpDiv(GetMemoFileSize , GetMemoBlockLen) ;
  End ;  { TxBase.CalcMemoFileBlocks }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoBlockCount                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoBlockCount : Integer ;
  Begin  { TxBase.GetMemoBlockCount }
    With GetDataAreaPtr^ , dbfMemoHeader do
      Begin
        Case dbfMemoFormat of
          dbfDBTMemo : dbfMemoBlockCount := dbfBlockCountDBT ;
          dbfFPTMemo : dbfMemoBlockCount := dbfBlockCountFPT ;
          dbfDB4Memo : dbfMemoBlockCount := dbfBlockCountDB4 ;
        End ;

        Result := dbfMemoBlockCount ;
      End ;
  End ;  { TxBase.GetMemoBlockCount }


{***********************************************************************
*                                                                      *
*       TxBase.SetMemoBlockCount                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetMemoBlockCount(nBlocks : Integer) ;
  Begin  { TxBase.SetMemoBlockCount }
    GetDataAreaPtr^.dbfMemoBlockCount := nBlocks ;
  End ;  { TxBase.SetMemoBlockCount }


{***********************************************************************
*                                                                      *
*       TxBase.UpdateMemoBlockCount                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.UpdateMemoBlockCount ;
  Var
    nBlocks : Integer ;

  Begin  { TxBase.UpdateMemoBlockCount }
    With GetDataAreaPtr^ , dbfMemoHeader do
      Begin
        dbfMemoFileSize := GetMemoFileVar^.Size ;
        nBlocks := (dbfMemoFileSize div GetMemoBlockLen) - 1 ;
        
        Case GetMemoBlockType of
          dbfDBTMemo : dbfBlockCountDBT := nBlocks ;
          dbfFPTMemo : dbfBlockCountFPT := nBlocks ;
          dbfDB4Memo : dbfBlockCountDB4 := nBlocks ;
        End ;
      End ;

    If not WriteMemoHeader then
      Begin
        ShowErrorMessage('Memo header not written.') ;
      End ;
  End ;  { TxBase.UpdateMemoBlockCount }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.GetMemoBlockLen                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoBlockLen : Integer ;
  Var
    nBlkLen : Integer ;

  Begin  { TxBase.GetMemoBlockLen }
    With GetMemoHeaderPtr^ do
      Case GetMemoBlockType of
        dbfDBTMemo : nBlkLen := dbfDefaultBlockLenDB3 ;
        dbfDB4Memo : nBlkLen := dbfBlockLenDB4        ;
        dbfFPTMemo : nBlkLen := SwapInteger(dbfBlockLenFPT)  ;
      Else
        Raise Exception.Create('Invalid memo block type TxBase.GetMemoBlockLen') ;
      End ;

    If nBlkLen <= 0 then
      Begin
        Raise Exception.Create('Invalid memo block length = [' + IntToStr(nBlkLen) +
                               '] - TxBase.GetMemoBlockLen') ;
      End ;

    Result := nBlkLen ;
  End ;  { TxBase.GetMemoBlockLen }


{***********************************************************************
*                                                                      *
*       TxBase.SetMemoBlockLen                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetMemoBlockLen(nSize : Word) ;
  Begin  { TxBase.SetMemoBlockLen }
    With GetMemoHeaderPtr^ do
      Case GetMemoBlockType of
        dbfDBTMemo :
          If nSize <> dbfDefaultBlockLenDB3 then
            Begin
              Raise Exception.Create('Invalid memo block size = [' +
                                     IntToStr(nSize) + '] in TxBase.SetMemoBlockSize') ;
            End ;

        dbfDB4Memo :
          Begin
            dbfBlockLenDB4     := nSize ;
            dbfBlockSizeResDB4 := nSize ;
          End ;

        dbfFPTMemo : dbfBlockLenFPT := SwapInteger(nSize) ;
      Else
        Raise Exception.Create('Invalid memo block type TxBase.SetMemoBlockSize') ;
      End ;
  End ;  { TxBase.SetMemoBlockLen }


{***********************************************************************
*                                                                      *
*       TxBase.dbfReadFixedMemo                                        *
*                                                                      *
*       Note: dbfFieldChoice must contain the field number of the memo.*
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*       1994-08-07  BKC  Add debugging code bad chars in first 2 chars *
*                        of a memo.                                    *
*       1994-11-29  BKC  Used dbfMemoFieldNo as current memo number.   *
*       2002-02-16  BKC  Return MemoBytes = 0 if no memo read.         *
*       2002-02-16  BKC  Additional error checking on buffers-not nil. *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfReadFixedMemo(   pRec        : Pointer ;
                                     nField      : Integer ;
                                     pMemoBuffer : Pointer ;
                                 Var nMemoBytes  : Integer  ) ;
  Begin  { TxBase.dbfReadFixedMemo }
    With GetDataAreaPtr^ do
      Begin
        If not (GetFieldType(nField) in MemoFieldTypes) then
          Begin
            ShowMessage('Invalid memo number field [' +
                        Format('%d' , [nField]) +
                        '] Type [' + GetFieldType(nField) + ']') ;
            nMemoBytes := 0 ;
            Exit ;
          End ;

        If pRec = nil then
          Begin
            ShowMessage('Invalid record buffer in dbfReadFixedMemo.') ;
            nMemoBytes := 0 ;
            Exit ;
          End ;

        If pMemoBuffer = nil then
          Begin
            ShowMessage('Invalid memo buffer in dbfReadFixedMemo.') ;
            nMemoBytes := 0 ;
            Exit ;
          End ;

        dbfMemoFieldNo := GetMemoNumber(pRec , nField) ;
        If dbfMemoFieldNo > 0 then
          dbfReadFixedMemoNo(dbfMemoFieldNo ,
                             pMemoBuffer    ,
                             nMemoBytes      )
        Else
          nMemoBytes := 0 ;
      End ;  { With dbDataArea do }
  End ;  { TxBase.dbfReadFixedMemo }


{***********************************************************************
*                                                                      *
*       TxBase.dbfReadFixedMemoNo                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfReadFixedMemoNo(   rfmnMemoNo : Integer ;
                                       MemoBuffer : Pointer ;
                                   Var MemoBytes  : Integer  ) ;
  Type
    rmBlockBufferType = Array[1..dbfMaxMemoBlockSize] of Char ;
    rmBlockBufferPtrType = ^rmBlockBufferType ;

  Var
    rmQuitLoop : Boolean ;
    rmBlockBuffer : rmBlockBufferPtrType ;
    rmPtr : PChar ;

    rmI , rmEofOffset , rmBytesRead , rmBytesReadL  : Integer ;
    rmMemoOffset , rmMaxBlock : Integer ;

  Begin  { TxBase.dbfReadFixedMemoNo }
    With GetDataAreaPtr^ do
      Begin
        MemoBytes := 0 ;
        rmBlockBuffer := Pointer(MemoBuffer) ;

        If rfmnMemoNo = 0 then
          Exit ;

        rmMemoOffset := dbfCalcMemoOffset(rfmnMemoNo) ;
        rmMaxBlock := MaxSegmentSize div GetMemoBlockLen ;
        Try
          dbfMemoFileVar.SeekToPos(rmMemoOffset) ;
        Except
          Begin
            MessageDlg('Seek failed in TxBase.dbfReadFixedMemoNo' ,
                       mtError ,
                       [mbOK]  ,
                       0        ) ;
            Exit ;
          End ;
        End ;

        rmI := 0 ;
        rmQuitLoop := False ;
        Repeat
          Try
            rmBytesRead := dbfMemoFileVar.Read(rmBlockBuffer^ , GetMemoBlockLen) ;
          Except
            Begin
              rmBytesRead := 0 ;
              MessageDlg('Stream read failed on memo file in TxBase.dbfReadFixedMemoNo' ,
                         mtError ,
                         [mbOK]  ,
                         0        ) ;
            End ;
          End ;

          rmBytesReadL := rmBytesRead ;
          Inc(rmI) ;

          rmPtr := SysUtils.StrPos(PChar(rmBlockBuffer) ,
                                   PChar(@OneEOF)        ) ;
          If rmPtr = nil then
            rmEofOffset := 0
          Else
            rmEofOffset := PtrDiff(rmPtr , rmBlockBuffer) ;

          If (rmPtr <> nil) or (rmBytesRead < GetMemoBlockLen) then
            Begin
              rmQuitLoop   := True ;
              rmBytesRead  := rmEofOffset ;
              rmBytesReadL := rmBytesRead ;
            End ;
          Inc(MemoBytes , rmBytesReadL) ;
          rmBlockBuffer := AddPtr(rmBlockBuffer , rmBytesReadL) ;
        Until (rmI > rmMaxBlock) or rmQuitLoop ;
      End ;  { With dbDataArea do }
  End ;  { TxBase.dbfReadFixedMemoNo }


{***********************************************************************
*                                                                      *
*       TxBase.AppendMemo                                              *
*                                                                      *
*       Note:  The memo supplied to this routine does NOT have the 2   *
*              Ctrl-Z 's attached to it.                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*       94-08-07  BKC  Add debugging code bad chars in first 2 chars   *
*                      of a memo.                                      *
*       94-08-07  BKC  Added error checking code for bad memo numbers. *
*                                                                      *
***********************************************************************}

Function TxBase.AppendMemo(pMemoBuffer : Pointer ;
                           nMemoSize   : Integer  ) : Integer ;
  Var
    amZeroBuffer : Array[1..dbfMaxMemoBlockSize] of Byte ;

    amFixWord : Word ;
    amFileSize     : Integer ;
    amBytesWritten ,
    amBytesToWrite  : Integer ;
//    amNewMemoNo : Integer ;

  {$IFDEF Debug }
  Type
    amTestArrayPtr = ^amTestArray ;
    amTestArray = Array[1..MaxMemoSize] of Char ;

  Var
    amI          : Integer        ;
    amTwoChar    : String[2]      ;
    amMemoOffset : Integer        ;
    amTestBuffer : amTestArrayPtr ;
    amBytesRead  : Word           ;
  {$ENDIF}

  Begin  { TxBase.AppendMemo }
    {$IFDEF Debug}
    amTwoChar[0] := #2 ;
    Move(pMemoBuffer^ , amTwoChar[1] , 2) ;
    If not ((amTwoChar[1] in ReadableChars) or
            (amTwoChar[1] in ReadableChars)   ) then
      Begin
        ErrorMsg('[xBase.AppendMemo] Bad chars in first 2 memo bytes.') ;
      End ;
    {$ENDIF}

    If nMemoSize = 0 then
      Begin
        Result := 0 ;
        Exit ;
      End ;

    Result := 0 ;
    If not FlushMemoFile then
      Exit ;

    ZeroMemory(@amZeroBuffer , SizeOf(amZeroBuffer)) ;

    amFileSize := GetMemoFileVar^.Size ;
    Try
      Result := RoundUpDiv(amFileSize , GetMemoBlockLen) ;
    Except
      ShowErrorMessage('Error calculation new block offset [' +
                       IntToStr(amFileSize) + ']  [' + IntToStr(GetMemoBlockLen) + ']') ;
    End ;

    GetMemoFileVar^.SeekToPos(amFileSize) ;

    { See if the end of the memo file needs to be aligned }
    { with the standard memo block size.                  }
    If (amFileSize mod GetMemoBlockLen) > 0 then
      Begin
        { Align the last block. }
        amFixWord := GetMemoBlockLen - (amFileSize mod GetMemoBlockLen)  ;
        amBytesWritten := GetMemoFileVar^.Write(amZeroBuffer , amFixWord) ;
        If (amFixWord <> amBytesWritten) or (not FlushMemoFile) then
          Exit ;
      End ;

    {$IFDEF Debug}
    {                                                     }
    { This debugging code will re-read a memo and compare }
    { to what was written.                                }
    {                                                     }
    If not FlushFileBuffers(GetMemoFileVar^.Handle) then
      ErrorMsg('Flushing memo file failed.') ;

    { Reset file point to end of file. }
    amMemoOffset := GetMemoFileVar^.Size ;
    GetMemoFileVar^.SeekToPos(amMemoOffset) ;
    {$ENDIF}

    FixMemoSize(Pointer(pMemoBuffer) , nMemoSize) ;
    amBytesWritten := GetMemoFileVar^.Write(pMemoBuffer^ , nMemoSize) ;
    If (nMemoSize <> amBytesWritten) or (not FlushMemoFile) then
      Exit ;

    { Align the memo if it did not end on a memo block size boundary. }
    If (nMemoSize mod GetMemoBlockLen) > 0 then
      Begin
        amBytesToWrite := (GetMemoBlockLen - (nMemoSize mod GetMemoBlockLen)) ;
        amBytesWritten := GetMemoFileVar^.Write(amZeroBuffer , amBytesToWrite) ;
        If (amBytesToWrite <> amBytesWritten) or (not FlushMemoFile) then
          Exit ;
      End ;

    {$IFDEF Debug}
    {                                                     }
    { This debugging code will re-read a memo and compare }
    { to what was written.                                }
    {                                                     }
    If not GetMemCheck(amTestBuffer , nMemoSize) then
      Begin
        ErrorMsg('GetMemCheck failed TxBase.AppendMemo.') ;
        Application.Terminate ;
      End ;

    Try
      GetMemoFileVar^.SeekToPos(amMemoOffset) ;  // xxxx
      Try
        amBytesRead := GetMemoFileVar^.Read(amTestBuffer^ , (nMemoSize - 2)) ;
      Except
        amBytesRead  := 0 ;
      End ;

      If (amBytesRead <> (nMemoSize - 2)) or (not FlushMemoFile) then
        Begin
          ErrorMsg('Byte count mismatch on memo re-read.') ;
          Application.Terminate ;
        End ;

      For amI := 1 to amBytesRead do
        If amTestBuffer^[amI] <> amTestArrayPtr(pMemoBuffer)^[amI] then
          Begin
            ErrorMsg('Char mismatch at [%d] in new memo [%d]' , [amI , Result]) ;
          End ;
    Finally
      FreeMemCheck(amTestBuffer , nMemoSize) ;
    End ;
    {$ENDIF}

    If Result <= 0 then
      Begin
        ShowMessage('Invalid memo number in TxBase.Append [' +
                     Format('%d' , [Result]) + ']') ;
        Application.Terminate ;
      End ;

    If not WriteMemoHeader then
      Begin
        ShowMessage('Invalid return from WriteMemoHeader.') ;
        Application.Terminate ;
      End ;
  End ;  { TxBase.AppendMemo }


{***********************************************************************
*                                                                      *
*       TxBase.AddMemo                                                 *
*                                                                      *
*         Add a new memo to the end of the memo file.                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.AddMemo(    pBuffer    : Pointer ;
                            nLen       : Integer ;
                        Var nMemoBlock : Integer  ) : Boolean ;
  Begin  { TxBase.AddMemo }
    nMemoBlock := AppendMemo(pBuffer , nLen) ;
    Result := (nMemoBlock <> 0) ;
  End ;  { TxBase.TxBase.AddMemo }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.dbfCalcMemoOffset                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.dbfCalcMemoOffset(nMemoNumber : Integer) : Integer ;
  Begin  { TxBase.dbfCalcMemoOffset }
    Result := nMemoNumber * GetMemoBlockLen ;
  End ;  { TxBase.dbfCalcMemoOffset }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoNumber                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoNumber(pRec   : Pointer ;
                              nField : Integer  ) : Integer ;
  Var
    cType  : Char    ;
    nWidth : Integer ;
    pField : Pointer ;

  Begin  { TxBase.GetMemoNumber }
    Result := 0 ;

    If (pRec = nil) then
      Begin
        ShowMessage('Invalid record pointer in TxBase.GetMemoNumber') ;
        Exit ;
      End ;

    pField := GetFieldPtr(pRec , nField) ;
    nWidth := GetFieldWidth(nField)      ;
    cType  := GetFieldType(nField)       ;

    If (cType in MemoFieldTypes) then
      Case nWidth of
         4 : Result := Integer(pField^) ;

         8 : Result := Trunc(Double(pField^)) ;

        10 : Result := StrToIntZero(MakeStr(pField , nWidth)) ;
      Else
        Raise Exception.Create('Invalid memo field width [' + IntToStr(nWidth) + ']') ;
      End
    Else
      Raise Exception.Create('Invalid memo field Type [' + cType + ']') ;
  End ;  { TxBase.GetMemoNumber }


{***********************************************************************
*                                                                      *
*       TxBase.GetCurrentMemoNumber                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetCurrentMemoNumber(nField : Integer) : Integer ;
  Begin  { TxBase.GetCurrentMemoNumber }
    If GetFieldType(nField) in MemoFieldTypes then
      Result := GetMemoNumber(GetRecordPtr , nField)
    Else
      Result := 0 ;
  End ;  { TxBase.GetCurrentMemoNumber }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFieldCount                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFieldCount : Word ;
  Begin  { TxBase.GetMemoFieldCount }
    With GetDataAreaPtr^ do
      Result := dbfMemoFldCount ;
  End ;  { TxBase.GetMemoFieldCount }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFldNo                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFldNo(gmnI : Byte) : Integer ;
  Begin  { TxBase.GetMemoFldNo }
    With GetDataAreaPtr^ do
      Result := dbfMemoFldList[gmnI] ;
  End ;  { TxBase.GetMemoFldNo }


{***********************************************************************
*                                                                      *
*       TxBase.MemoFldIdx                                              *
*                                                                      *
*       Look for a memo field number in the internal list and return   *
*       the index into the list.  Return 0 if the field number is not  *
*       a memo field or not in the list.                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFldIdx(mfiM : Byte) : Integer ;
  Var
    mfiI : Word ;

  Begin  { TxBase.GetMemoFldIdx }
    With GetDataAreaPtr^ do
      If (GetFieldType(mfiM) in MemoFieldTypes) then
        For mfiI := 1 to GetMemoFieldCount do
          If dbfMemoFldList[mfiI] = mfiM then
            Begin
              Result := mfiI ;
              Exit ;
            End ;
    Result := 0 ;
  End ;  { TxBase.GetMemoFldIdx }


{***********************************************************************
*                                                                      *
*       TxBase.GetNextMemoFld                                          *
*                                                                      *
*       Look for the next memo field number in the internal list and   *
*       return the the index into the list.  Return 0 if their no memo *
*       fields on file.                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetNextMemoFld(gnmfM : Byte) : Integer ;
  Var
    gnmfI : Word ;

  Begin  { TxBase.GetNextMemoFld }
    With GetDataAreaPtr^ do
      Case GetMemoFieldCount of
        0 : GetNextMemoFld := 0 ;
        1 : GetNextMemoFld := dbfMemoFldList[1] ;
      Else
        Begin
          gnmfI := 0 ;

          Repeat
            Inc(gnmfI) ;
            If (gnmfI > GetMemoFieldCount) then
              Break ;
          Until (dbfMemoFldList[gnmfI] > gnmfM) ;

          If gnmfI > GetMemoFieldCount then
            Result := dbfMemoFldList[1]
          Else
            Result := dbfMemoFldList[gnmfI] ;
        End ;
      End ;
  End ;  { TxBase.GetNextMemoFld }


{***********************************************************************
*                                                                      *
*       TxBase.WriteFixedMemoNo                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.WriteFixedMemoNo(wfmnMemoNo     : Integer ;
                                  wfmnMemoBuffer : Pointer ;
                                  wfmnMemoBytes  : Integer  ) ;
  Var
    wfmnBytesWritten : Integer ;

  Begin  { TxBase.WriteFixedMemoNo }
    If wfmnMemoNo = 0 then
      Exit ;

    With GetDataAreaPtr^ do
      Begin
        Try
          dbfMemoFileVar.SeekToPos(dbfCalcMemoOffset(wfmnMemoNo)) ;
        Except
          Begin
            MessageDlg('Invalid seek on memo file in TxBase.WriteFixedMemoNo' ,
                       mtError ,
                       [mbOK]  ,
                       0        ) ;
            Exit ;
          End ;
        End ;

        FixMemoSize(Pointer(wfmnMemoBuffer) ,
                    wfmnMemoBytes            ) ;
        wfmnBytesWritten := dbfMemoFileVar.Write(wfmnMemoBuffer^ , wfmnMemoBytes) ;
        If (wfmnMemoBytes <> wfmnBytesWritten) then
          Begin
            ShowMessage('Invalid stream write memo file in WriteFixedMemoNo') ;
            Exit ;
          End ;
      End ;  { With GetDataAreaPtr^ do }
  End ;  { TxBase.WriteFixedMemoNo }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFileName                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFileName : String ;
  Begin
    With GetDataAreaPtr^ do
      Result := dbfMemoFileName ;
  End ;


{***********************************************************************
*                                                                      *
*       TxBase.dbfSetMemoFile                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfSetMemoFile ;
  Begin  { TxBase.dbfSetMemoFile }
    With GetDataAreaPtr^ do
      Begin
        Case GetSignature of
          $83 , { dBASE III+ with memo file    }

          $7B , { dBASE IV with memo           }
          $8B , { dBASE IV w. memo             }

          $30 ,
          $F5   { FoxPro w. memo file          }
              : dbfMemoFile := True ;
        Else
          dbfMemoFile := False ;
        End ;

      dbfMemoFile := (dbfMemoFile or IsFlexFile) ;
      
      If dbfMemoFile then
        dbfSetMemoVars ;
    End ;
  End ;  { TxBase.dbfSetMemoFile }


{***********************************************************************
*                                                                      *
*       TxBase.HasMemoFile                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.HasMemoFile : Boolean ;
  Begin  { TxBase.HasMemoFile }
    With GetDataAreaPtr^ do
      Result := dbfMemoFile ;
  End ;  { TxBase.HasMemoFile }


{***********************************************************************
*                                                                      *
*       TxBase.ZapDataArea                                             *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TxBase.ZapDataArea ;
  Begin  { TxBase.ZapDataArea }
    FillChar(GetDataAreaPtr^ , SizeOf(dbfDataAreaType) , 0) ;
  End ;  { TxBase.ZapDataArea }


{***********************************************************************
*                                                                      *
*       TxBase.ZapStructures                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.ZapStructures ;
  Var
    nI : Integer ;
    
  Begin  { TxBase.ZapStructures }
    ZapDataArea ;

    { Probably don't need to do this, }
    { but make sure anyway.           }
    With GetDataAreaPtr^ do
      For nI := 0 to NoOfFieldTypes do
        dbfFieldLists[nI] := nil ;
  End ;  { TxBase.ZapStructures }


{***********************************************************************
*                                                                      *
*       TxBase.ResetFieldRecNoLists                                    *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TxBase.ResetFieldRecNoLists ;
  Var
    nI : Integer ;

  Begin  { TxBase.ResetFieldRecNoLists }
    For nI := 0 to GetFieldCount do
      With GetDataAreaPtr^ do
        FreeAndNil(dbfFieldLists[nI]) ;
  End ;  { TxBase.ResetFieldRecNoLists }


{***********************************************************************
*                                                                      *
*       TxBase.AllocateFieldList                                       *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TxBase.AllocateFieldList(nField : Integer) ;
  Begin  { TxBase.AllocateFieldList }
    With GetDataAreaPtr^ do
      If dbfFieldLists[nField] = nil then
        Try
          dbfFieldLists[nField] := TIntegerList.Create ;
        Except
          dbfFieldLists[nField] := nil ;
        End ;
  End ;  { TxBase.AllocateFieldList }


{***********************************************************************
*                                                                      *
*       TxBase.AddRecNoToFieldList                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TxBase.AddRecNoToFieldList(nField : Integer ;
                                     nRec   : Integer  ) ;
  Begin  { TxBase.AddRecNoToFieldList }
    AllocateFieldList(nField) ;

    With GetDataAreaPtr^ do
      dbfFieldLists[nField].Add(nRec) ;
  End ;  { TxBase.AddRecNoToFieldList }


{***********************************************************************
*                                                                      *
*       TxBase.ZapCounts                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.ZapCounts ;
  Begin  { TxBase.ZapCounts }
    With GetDataAreaPtr^ do
      Begin
        FillChar(dbfFieldErrors  , SizeOf(dbfFieldErrors)  , 0) ;
        FillChar(dbfHeaderErrors , SizeOf(dbfHeaderErrors) , 0) ;

        dbfBadDeletes := 0 ;
        dbfBadRecords := 0 ;
        dbfZapBadFieldStats ;
      End ;
  End ;  { TxBase.ZapCounts }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldErrors                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldErrors(cChar : Char) : Integer ;
  Begin  { TxBase.GetFieldErrors }
    With GetDataAreaPtr^ do
      Begin
        If cChar = '*' then          { Unknown field type. }
          Result := dbfFieldErrors[0]
        Else
          Result := dbfFieldErrors[PosStr(cChar , LegalFieldTypes)] ;
      End ;
  End ;  { TxBase.GetFieldErrors }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldErrorsIdx                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldErrorsIdx(nIdx : Integer ) : Integer ;
  Begin  { TxBase.GetFieldErrorsIdx }
    With GetDataAreaPtr^ do
      Result := dbfFieldErrors[nIdx] ;
  End ;  { TxBase.GetFieldErrorsIdx }


{***********************************************************************
*                                                                      *
*       TxBase.GetTotalFieldErrors                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetTotalFieldErrors : Integer ;
  Var
    nI      : Integer ;
    nResult : Integer ;

  Begin  { TxBase.GetTotalFieldErrors }
    nResult := 0 ;

    With GetDataAreaPtr^ do
      For nI := 1 to Length(LegalFieldTypes) do
        nResult := nResult + dbfFieldErrors[nI] ;

    Result := nResult ;
  End ;  { TxBase.GetTotalFieldErrors }


{***********************************************************************
*                                                                      *
*       TxBase.dbValidateFieldName                                     *
*                                                                      *
*         All were are validating here is the field name characters.   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.dbValidateFieldName(nFieldNo : Integer) : Boolean ;
  Var
    nI : Integer ;
    cFldStr : String ;

  Begin  { TxBase.dbValidateFieldName }
    Result := True ;

    cFldStr := GetFieldName(nFieldNo) ;
    If Length(cFldStr) < 1 then
      Result := False
    Else
      Begin
        If (cFldStr[1] in LegalFieldFirstChars) then
            Result := False ;

        For nI := 2 to Length(cFldStr)do
          If not (cFldStr[nI] in LegalFieldNameChars) then
            Result := False ;
      End ;
  End ;  { TxBase.dbValidateFieldName }


{***********************************************************************
*                                                                      *
*       TxBase.dbRepairFieldName                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.dbRepairFieldName(cFldStr : String) : Boolean ;
  Begin  { TxBase.dbRepairFieldName }
    If Length(cFldStr) < 1 then
      Result := False
    Else
      Begin
        Result := True ;
      End ;
  End ;  { TxBase.dbRepairFieldName }


{***********************************************************************
*                                                                      *
*       TxBase.ValidateHeader                                          *
*                                                                      *
*         10001 - invalid header size field.                           *
*         10002 - invalid total records field.                         *
*         10003 - invalid field count.                                 *
*         10004 - invalid field name.                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.ValidateHeader : Boolean ;
  Var
    nI  : Integer ;

  Begin  { TxBase.ValidateHeader }
    With GetDataAreaPtr^ do
      Begin
        ZeroMemory(@dbfHeaderErrors , SizeOf(dbfHeaderErrors)) ;

        { Check for minimum size of header. }
        If GetHeaderSize < (SizeOf(dbfFieldDescType)     +
                            SizeOf(xBaseFixedHeaderType) +
                            1                             ) then
          Begin
            Inc(dbfHeaderErrors[0]) ;
            dbfHeaderErrors[dbfHeaderErrors[0]] := 10001 ;
          End ;

        If GetTotalRecords < 0 then
          Begin
            Inc(dbfHeaderErrors[0]) ;
            dbfHeaderErrors[dbfHeaderErrors[0]] := 10002 ;
          End ;

        If GetFieldCount < 1 then
          Begin
            Inc(dbfHeaderErrors[0]) ;
            dbfHeaderErrors[dbfHeaderErrors[0]] := 10003 ;
          End
        Else
          For nI := 1 to GetFieldCount do
            If not dbValidateFieldName(nI) then
              Begin
                Inc(dbfHeaderErrors[0]) ;
                dbfHeaderErrors[dbfHeaderErrors[0]] := 10004 ;
              End ;

        Result := (dbfHeaderErrors[0] = 0) ;
      End ;
  End ;  { TxBase.ValidateHeader }


{***********************************************************************
*                                                                      *
*       TxBase.RepairHeader                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.RepairHeader : Boolean ;
  Var
    nI , nFields : Integer ;

  Begin  { TxBase.RepairHeader }
    Result := True ;

    With GetDataAreaPtr^ do
      Begin
        If dbfHeaderErrors[0] > 0 then
          For nI := 1 to dbfHeaderErrors[0] do
            Case dbfHeaderErrors[nI] of
              { Header size field is wrong, suggest correction. }
              10001 : Begin
                      End ;

              { Reord count field is wrong, suggest correction. }
              10002 : Begin
                      End ;

              { Adjust the field count by doing an actual count }
              10003 : Begin
                        nFields := GetFieldCount ;
                        dbfCalcFieldCount ;
                        If nFields <> GetFieldCount then
                          ShowMessage('Field count adjust from [' +
                                      Format('%d' , [nFields]) + '] to ' +
                                      Format('%d' , [GetFieldCount]) + ']') ;
                      End ;

              { Validate the field names then suggest a correction. }
//              10004 : Begin
//                        For nI := 1 to GetFieldCount do
//                          If not dbValidateFieldName(nI) then
//                            Begin
//                            End ;
//                      End ;
            End ;
      End ;  { With dbDataArea do }
  End ;  { TxBase.RepairHeader }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.dbDuplicateField                                        *
*                                                                      *
*         Given the field number and name , return true if their is a  *
*       duplicate field name.                                          *
*         This routine is mostly used when renaming a field.           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.dbDuplicateField(cFieldName : String ;
                                 nFieldNo   : Integer ) : Boolean ;
  Var
    nI : Integer ;

  Begin  { TxBase.dbDuplicateField }
    Result := False ;

    With GetDataAreaPtr^ do
      For nI := 1 to dbfFieldCount do
        If nI <> nFieldNo then   { Skip the specified field. }
          Begin
            If cFieldName = GetFieldName(nI) then
              Begin
                Result := True ;
                Exit ;
              End ;
          End ;
  End ;  { TxBase.dbDuplicateField }


{***********************************************************************
*                                                                      *
*       TxBase.RenameField                                             *
*                                                                      *
*         Rename the given field, checking for possible duplicates.    *
*       Return True if the renaming succeeded.                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.RenameField(cFieldName : String ;
                            nFieldNo   : Integer ) : Boolean ;
  Begin  { TxBase.RenameField }
    Result := not dbDuplicateField(cFieldName , nFieldNo) ;
    If Result then
      SetFieldName(cFieldName , nFieldNo) ;
  End ;  { TxBase.RenameField }


{***********************************************************************
*                                                                      *
*       TxBase.IsValidDate                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsValidDate(cDate : String) : Boolean ;
  Var
    nYear  ,
    nMonth ,
    nDay     : Word ;

  Begin  { TxBase.IsValidDate }
    Result := True;

    { A blank date field is valid.  }
    If Length(Trim(cDate)) = 0 then
      Exit ;

    If ValidNumStr(cDate) and (Length(cDate) = 8) then
      Begin
        Try
          nYear  := StrToInt(Copy(cDate , 1 , 4)) ;
          nMonth := StrToInt(Copy(cDate , 5 , 2)) ;
          nDay   := StrToInt(Copy(cDate , 7 , 2)) ;

          Result := IsDateValid(nYear , nMonth , nDay) ;
        Except
          Result := False ;
        End ;
      End
    Else
      Result := False ;
  End ;  { TxBase.IsValidDate }


{***********************************************************************
*                                                                      *
*       TxBase.dbfIsBlankDate                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.dbfIsBlankDate(pRec   : Pointer ;
                               nField : Integer  ) : Boolean ;
  Begin  { TxBase.IsBlankDate }
    If GetFieldType(nField) = 'D' then
      Result := (Length(Trim(GetFieldStr(pRec , nField, False))) = 0)
    Else
      Raise Exception.Create(GetFieldName(nField) + ' is not a D type field.') ;
  End ;  { TxBase.dbfIsBlankDate }


{***********************************************************************
*                                                                      *
*       TxBase.SetMemoDateTime                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetMemoDateTime(smdtB : Boolean) ;
  Begin  { TxBase.SetMemoDateTime }
    With GetDataAreaPtr^ , dbfMemoHeader do
      dbfMemoDateTimeSet := smdtB ;
  End ;  { TxBase.SetMemoDateTime }


{***********************************************************************
*                                                                      *
*       TxBase.dbZapBadFieldStats                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfZapBadFieldStats ;
  Begin  { TxBase.dbfZapBadFieldStats }
    With GetDataAreaPtr^ , dbfMemoHeader do
      Begin
        dbfBadFields := 0 ;
        FillChar(dbfBadFieldList , SizeOf(dbfBadFieldList) , 0) ;
      End ;
  End ;  { TxBase.dbZapBadFieldStats }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.dbfCalcNewFieldType                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.dbfCalcNewFieldType(    nField   : Integer      ;
                                    Var NewFType : NewFieldType ;
                                    Var NewWidth : Integer       ) : Boolean ;
  Begin  { TxBase.dbCalcNewFieldType }
    Result := False ;

    Case GetFieldType(nField) of
      'B' : Begin
              NewFType := NInteger ;
              NewWidth := SizeOf(Integer) ;
            End ;

      'M' ,
      'C' : Begin  { Character string }
              If GetFieldWidth(nField) = 1 then
                Begin
                  NewFType := NChar ;
                  NewWidth := 1 ;
                End
              Else
                Begin
                  NewFType := PascalStr ;
                  NewWidth := GetFieldWidth(nField) + 1 ;
                End ;
            End ;

      'L' : Begin  { Logical }
              NewFType := NBoolean ;
              NewWidth := SizeOf(Boolean) ;
            End ;

      'D' : Begin  { Date }
              NewFType := NDate ;
              NewWidth := SizeOf(TDateTime) ;
            End ;

      'N' : Begin
              If (GetFieldDecimals(nField) = 0) and (GetFieldWidth(nField) <= 9) then
                Begin
                  NewFType := NInteger ;
                  NewWidth := SizeOf(Integer) ;
                End
              Else
                Begin
                  NewFType := NFloat        ;  { 6 or 8 byte reals }
                  NewWidth := SizeOf(Float) ;
                End ;
            End ;

      'F' : Begin
              NewFType := NFloat        ;  { 6 or 8 byte reals }
              NewWidth := SizeOf(Float) ;
            End ;
    Else
      Exit ;
    End ;  { Case FType of }

    Result := True ;
  End ;  { TxBase.dbCalcNewFieldType }


{***********************************************************************
*                                                                      *
*       TxBase.GetNewFieldType                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetNewFieldType(nField : Integer) : NewFieldType ;
  Begin  { TxBase.GetNewFieldType }
    With GetDataAreaPtr^ do
      Result := dbfNewFieldType[nField] ;
  End ;  { TxBase.GetNewFieldType }


{***********************************************************************
*                                                                      *
*       TxBase.GetNewFieldWidth                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetNewFieldWidth(nField : Integer) : Integer ;
  Begin  { TxBase.GetNewFieldWidth }
    With GetDataAreaPtr^ do
      Result := dbfNewFieldWidth[nField] ;
  End ;  { TxBase.GetNewFieldWidth }


{***********************************************************************
*                                                                      *
*       TxBase.CleanFixedHeader                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.CleanFixedHeader ;
  Var
    nFld : Integer ;
    nLen : Integer ;
    nI   : Integer ;

  Begin  { TxBase.CleanFixedHeader }
    For nFld := 1 to GetFieldCount do
      With GetFieldDescPtr(nFld)^ do  
        Begin
          Name[dbfMaxNameLen] := ZeroChar ;
          nLen := 0 ;

          For nI := 0 to dbfMaxNameLen do
            If Name[nI] <> ZeroChar then
              Inc(nLen)
            Else
              Break ;

          For nI := nLen to dbfMaxNameLen do
            Name[nI] := ZeroChar ;
        End ;
  End ;  { TxBase.CleanFixedHeader }


{***********************************************************************
*                                                                      *
*       TxBase.GetHeaderPtr                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetHeaderPtr : pxBaseHeader ;
  Begin  { TxBase.GetHeaderPtr }
    With GetDataAreaPtr^ do
      Result := @dbfHeader ;
  End ;  { TxBase.GetHeaderPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoHeaderPtr                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoHeaderPtr : pdbfMemoHeader;
  Begin  { TxBase.GetMemoHeaderPtr }
    With GetDataAreaPtr^ do
      Result := @dbfMemoHeader ;
  End ;  { TxBase.GetMemoHeaderPtr }


{***********************************************************************
*                                                                      *
*       TxBase.InternalFieldDesc                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.InternalFieldDesc(nField : Integer) : String ;
  Begin  { TxBase.InternalFieldDesc }
    Case GetFieldType(nField) of
      'C' : Result := 'ftString'      ;  // Character or string field
      'L' : Result := 'ftBoolean'     ;  // Boolean field

      'F' : Result := 'ftFloat'      ;  // Floating-point numeric field

      'N' : Begin
              If GetFieldDecimals(nField) = 0 then
                Result := 'ftInteger'    // Integer field 32-bit
              Else
                Result := 'ftFloat'   ;  // Floating-point numeric field
            End ;

      'D' : Result := 'ftDate'        ;  // Date field

      { This could also be an ftGraphic field. }
      'B' : Result := 'ftBlob'        ;  // Binary Large OBject field

      'M' : Result := 'ftMemo'        ;  // Text memo field
    Else
      Result := 'ftUnknown'           ;  // Unknown or undetermined
    End ;
  End ;  { TxBase.InternalFieldDesc }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFieldsPtr                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFieldsPtr : pTStringList ;
  Begin  { TxBase.GetMemoFieldsPtr }
    Result := @dbfMemoFields ;
  End ;  { TxBase.GetMemoFieldsPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoFieldTypesPtr                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoFieldTypesPtr : pTStringList ;
  Begin  { TxBase.GetMemoFieldTypesPtr }
    Result := @dbfMemoFieldTypes ;
  End ;  { TxBase.GetMemoFieldTypesPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetBadFieldCnt                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBadFieldCnt : Integer ;
  Begin  { TxBase.GetBadFieldCnt }
    With GetDataAreaPtr^ do
      Begin
        Result := dbfBadFields ;
      End ;
  End ;  { TxBase.GetBadFieldCnt }


{***********************************************************************
*                                                                      *
*       TxBase.GetBadFieldsPtr                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBadFieldsPtr : pTArrayInteger ;
  Begin  { TxBase.GetBadFieldsPtr }
    With GetDataAreaPtr^ do
      Result := @dbfBadFieldList ;
  End ;  { TxBase.GetBadFieldsPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetGoodListPtr                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetGoodListPtr : pTStringList ;
  Begin  { TxBase.GetGoodListPtr }
    With GetDataAreaPtr^ do
      Result := @dbfGoodRecList ;
  End ;  { TxBase.GetGoodListPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetBadListPtr                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBadListPtr : pTStringList ;
  Begin  { TxBase.GetBadListPtr }
    With GetDataAreaPtr^ do
      Result := @dbfBadRecList ;
  End ;  { TxBase.GetBadListPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetBadNameListPtr                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBadNameListPtr : pTStringList ;
  Begin  { TxBase.GetBadNameListPtr }
    With GetDataAreaPtr^ do
      Result := @dbfBadNameList ;
  End ;  { TxBase.GetBadNameListPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetBadNameListCnt                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBadNameListCnt : Integer ;
  Begin  { TxBase.GetBadNameListCnt }
    Result := GetBadNameListPtr^.Count ;
  End ;  { TxBase.GetBadNameListCnt }


{***********************************************************************
*                                                                      *
*       TxBase.GetBadFieldNo                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBadFieldNo(nIdx : Integer) : Integer ;
  Begin  { TxBase.GetBadFieldNo }
    If ValidFieldNumber(nIdx) then
      Begin
        With GetDataAreaPtr^ do
          Result := dbfBadFieldList[nIdx] ;
      End
    Else
      Result := 0 ;
  End ;  { TxBase.GetBadFieldNo }


{***********************************************************************
*                                                                      *
*       TxBase.GetBadRecordCnt                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetBadRecordCnt : Integer ;
  Begin  { TxBase.GetBadRecordCnt }
    Result := GetDataAreaPtr^.dbfBadRecords ;
  End ;  { TxBase.GetBadRecordC`P8


{***********************************************************************
*                                                                      *
*       TxBase.GetBlobFileName                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.GetBlobFileName : String ;
  Begin  { TxBase.GetBlobFileName }
    Result := GetDataAreaPtr^.dbfBlobFile ;
  End ;  { TxBase.GetBlobFileName }


{***********************************************************************
*                                                                      *
*       TxBase.FloatToField                                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.FloatToFieldStr(nField : Word  ;
                                nFloat : Float  ) : String ;
  Var
    cMask : String ;

  Begin  { TxBase.FloatToFieldStr }
    If nFloat = 0 then
      Result := FieldSpaces(nField)
    Else
      Begin
        cMask := GetNumericFieldMask(nField) ;
        Result := Format(GetNumericFieldMask(nField) , [nFloat]) ;
      End ;
  End ;  { TxBase.FloatToFieldStr }


{***********************************************************************
*                                                                      *
*       TxBase.FixFloatField                                           *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.FixFloatField(nField : Integer) : String ;
  Type
    pCharArray = ^CharArray ;
    CharArray  = Array[1..65520] of Char ;

  Var
    nI       : Integer    ;
    cStr     : String     ;
    pFld     : pCharArray ;
    bInvalid : Boolean    ;

  Begin  { TxBase.FixFloatField }
    Try
      pFld := GetFieldPtr(GetRecordPtr , nField) ;

      bInvalid := False ;
      cStr := '' ;
      For nI := 1 to GetFieldWidth(nField) do
        If ((pFld^[nI] in dbfValidNumeric) and (pFld^[nI] <> #$00)) then
          cStr := cStr + pFld^[nI]
        Else
          Begin
            bInvalid := True ;
            Break ;
          End ;

      If bInvalid then
        Result := FieldSpaces(nField)
      Else
        Result := FixFloatStr(cStr) ;
    Except
      Begin
        Result := FieldSpaces(nField) ;
        MessageDlg('Error in TxBase.FixFloatField - Field [' + IntToStr(nField) + ']',
                   mtError ,
                   [mbOK]  ,
                   0        ) ;
      End ;
    End ;
  End ;  { TxBase.FixFloatField }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldDisplayString                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldDisplayString(pRec   : Pointer ;
                                      nField : Integer ;
                                      bHex   : Boolean  ) : String ;
  Var
    nOffset : Integer ;

  Begin  { TxBase.GetFieldDisplayString }
    If bHex then
      Result := GetFieldHexStr(pRec , nField)
    Else
      Case GetFieldType(nField) of
        'B' : Result := IntToStrBlankPad(GetMemoNumber(pRec , nField) , 10) ;

        'G' ,
        'M'   : Result := IntToStrBlank(GetMemoNumber(pRec , nField)) ;

        'T'   : Result := GetDateTimeFieldEnglish(pRec , nField) ;

        'Y'   : Result := Format('%16.4f' , [AsComp(pRec , nField) / 10000]) ;
      Else
        If IsFlexField(nField) then
          Begin
            { Check for memo existence.  An offset of 538976288 }
            { is equivalent to 'bbbbbb' (6 blanks).             }
            nOffset := GetMemoOffsetDBV(pRec , nField) ;

            If nOffset = 538976288 then
              Result := ''
            Else
              If nOffset > GetMemoFileSize then
                Result := '< Invalid memo offset >'
              Else
                Begin
                  With GetOptionsPtr^ do
                    If dbvShowFieldOffset then
                      Result := PadLeft(IntToStr(nOffset) , 9)
                    Else
                      If dbvShowFieldHex then
                        Result := ' [' + GetFieldHexStr(pRec , nField) + ']'
                      Else
                        Result := PadLeft(IntToStr(nOffset) , 9) ;
                End ;
          End
        Else
          Result := GetCleanFieldStr(pRec , nField) ;
      End ;
  End ;  { TxBase.GetFieldDisplayString }


{***********************************************************************
*                                                                      *
*       TxBase.FixField                                                *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.FixField(pRec   : Pointer ;
                         nField : Integer  ) : Boolean ;
  Var
    nI     : Integer ;
    bOkay  : Boolean ;

    cType : Char   ;
    cStr  : String ;

    pField    : pTFieldBuffer ;
    aValidChr : CharSet       ;

  Begin  { TxBase.FixField }
    Result := False ;
    pField := GetFieldPtr(pRec , nField) ;
    aValidChr := ValidSetArray[PosStr(GetFieldType(nField) , LegalFieldTypes)] ;
    cType := GetFieldType(nField) ;

    If (cType in NoDecimalFieldTypes) and (nField > 0) then
      SetFieldDecimals(nField , 0) ;

    Case cType of
      'C' : Begin
              If nField = 0 then
                Begin
                  If ((not (pField^[0] in ValidDelete)) or (pField^[0] = #$00)) then
                    Begin
                      pField^[0] := ' '  ;
                      Result     := True ;
                    End ;
                End
              Else
                Begin
                  For nI := 0 to (GetFieldWidth(nField) - 1) do
                    If not (pField^[nI] in aValidChr) then
                      Begin
                        pField^[nI] := ' ' ;
                        Result := True ;
                      End ;
                End ;
            End ;

      'F' ,
      'N'   : If GetFieldDecimals(nField) > 0 then
                Try
                  SetFieldFloat(pRec                    ,
                                GetFieldFloat(pRec   ,
                                              nField ,
                                              bOkay   ) ,
                                nField                   )
                Except
                  Begin
                    SetFieldFloat(pRec , 0.00 , nField) ;
                    Result := True ;
                  End ;
                End
              Else
                Try
                  SetFieldInteger(pRec                    ,
                                  GetFieldInteger(pRec ,
                                                  nField) ,
                                  nField                   ) ;
                Except
                  Begin
                    SetFieldInteger(pRec , 0 , nField) ;
                    Result := True ;
                  End ;
                End ;

      'B' ,
      'D' ,
      'G' ,
      'I' ,
      'L' ,
      'M' ,
      'P' ,
      'T' ,
      'Y'   : Begin
                For nI := 0 to (GetFieldWidth(nField) - 1) do
                  If not (pField^[nI] in aValidChr) then
                    Begin
                      SetFieldDefault(pRec , nField) ;
                      Result := True ;
                      Break ;
                    End ;

                If not Result then
                  Case cType of
                    'D' : Begin
                            cStr := GetFieldStr(pRec , nField , False) ;
                            If IsValidDate(cStr) then
                              Result := False
                            Else
                              Begin
                                SetFieldDefault(pRec , nField) ;
                                Result := True ;
                              End ;
                          End ;

                    'M' : If not IsMemoFieldValid(pRec , nField) then
                            Begin
                              SetFieldDefault(pRec , nField) ;
                              Result := True ;
                            End ;
                  End ;  { Case cType of }
              End ;
    Else
      Raise Exception.Create('Invalid field type ' + GetFieldType(nField)) ;
    End ;
  End ;  { TxBase.FixField }


{***********************************************************************
*                                                                      *
*       TxBase.IsMemoFieldValid                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsMemoFieldValid(pRec   : Pointer ;
                                 nField : Integer  ) : Boolean ;
  Var
    cMemo : String  ;
    nMemo : Integer ;

    bResult  : Boolean ;
    nMemoLen : Integer ;
    cBuffer  : Array[0..(dbfMaxMemoSize - 1)] of Byte ;

  Begin  { TxBase.IsMemoFieldValid }
    cMemo := Trim(GetFieldStr(pRec , nField , False)) ;
    nMemo := GetMemoNumber(pRec , nField) ;
    If nMemo = 0 then
      bResult := (Length(cMemo) = 0)
    Else
      If nMemo < 0 then
        bResult := False
      Else
        Begin
          Try
            dbfReadFixedMemoNo(nMemo    ,
                               @cBuffer ,
                               nMemoLen  ) ;
            bResult := (nMemoLen > 0) ;
          Except
            bResult := False ;
          End ;
        End ;

    Result := bResult ;
  End ;  { TxBase.IsMemoFieldValid }


{***********************************************************************
*                                                                      *
*       TxBase.dbfSetupRestructure                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.dbfSetupRestructure ;
  Var
    nField : Integer ;

    cStr      : String ;

    cName     : String[dbfMaxNameLen] ;
    cType     : Char                  ;
    nWidth    : Integer               ;
    nDecimals : Integer               ;

  Begin  { TxBase.dbfSetupRestructure }
    With GetDataAreaPtr^ do
      Begin
        dbfRestructureList.Clear ;

        For nField := 0 to GetFieldCount do
          Begin
            cName     := GetFieldName(nField)     ;
            cType     := GetFieldType(nField)     ;
            nWidth    := GetFieldWidth(nField)    ;
            nDecimals := GetFieldDecimals(nField) ;
            If nDecimals = 0 then
              cStr := '  '
            Else
              cStr := PadL(IntToStr(nDecimals) , 2) ;

            With dbfRestructureList do
              Add(PadR(cName , dbfMaxNameLen) + ' ' +     { Name      1 10 }
                  cType                       + ' ' +     { Type     12  1 }
                  PadL(IntToStr(nWidth) , 3)  + ' ' +     { Width    14  3 }
                  cStr                               ) ;  { Decimals 18  2 }
          End ;
      End ;  { With dbDataArea do }
  End ;  { TxBase.dbfSetupRestructure }


{***********************************************************************
*                                                                      *
*       TxBase.GetRestructureListPtr                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetRestructureListPtr : pTStringList ;
  Begin  { TxBase.GetRestructureListPtr }
    With GetDataAreaPtr^ do
      Result := @dbfRestructureList ;
  End ;  { TxBase.GetRestructureListPtr }


{***********************************************************************
*                                                                      *
*       TxBase.AsDouble                                                *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.AsDouble(pRec   : Pointer ;
                         nField : Integer  ) : Double ;
  Begin  { TxBase.AsDouble }
    Result := Double(GetFieldPtr(pRec , nField)^) ;
  End ;  { TxBase.AsDouble }


{***********************************************************************
*                                                                      *
*       TxBase.FldAsDouble                                             *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.FldAsDouble(nField : Integer) : Double ;
  Begin  { TxBase.FldAsDouble }
    Result := Double(GetCurrentFieldPtr(nField)^) ;
  End ;  { TxBase.FldAsDouble }


{***********************************************************************
*                                                                      *
*       TxBase.FldAsBoolean                                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.FldAsBoolean(nField : Integer) : Boolean ;
  Begin  { TxBase.FldAsBoolean }
    Result := (Char(GetCurrentFieldPtr(nField)^) in dbfValidLogicalTrue) ;
  End ;  { TxBase.FldAsBoolean }


{***********************************************************************
*                                                                      *
*       TxBase.AsComp                                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.AsComp(pRec   : Pointer ;
                       nField : Integer  ) : Comp ;
  Begin  { TxBase.AsComp }
    Result := Comp(GetFieldPtr(pRec , nField)^) ;
  End ;  { TxBase.AsComp }


{***********************************************************************
*                                                                      *
*       TxBase.AsCurrency                                              *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.AsCurrency(pRec   : Pointer ;
                           nField : Integer  ) : Currency ;
  Begin  { TxBase.AsCurrency }
    Result := Currency(GetFieldPtr(pRec , nField)^) ;
  End ;  { TxBase.AsCurrency }


{***********************************************************************
*                                                                      *
*       TxBase.AsInteger                                               *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.AsInteger(pRec   : Pointer ;
                          nField : Integer  ) : Integer ;
  Begin  { TxBase.AsInteger }
    Result := Integer(GetFieldPtr(pRec , nField)^) ;
  End ;  { TxBase.AsInteger }


{***********************************************************************
*                                                                      *
*       TxBase.FldAsInteger                                            *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.FldAsInteger(nField : Integer) : Integer ;
  Begin  { TxBase.FldAsInteger }
    Result := Integer(GetCurrentFieldPtr(nField)^) ;
  End ;  { TxBase.FldAsInteger }


{***********************************************************************
*                                                                      *
*       TxBase.AsWord                                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.AsWord(pRec   : Pointer ;
                       nField : Integer  ) : Word ;
  Begin  { TxBase.AsWord }
    Result := Word(GetFieldPtr(pRec , nField)^) ;
  End ;  { TxBase.AsWord }


{***********************************************************************
*                                                                      *
*       TxBase.AsByte                                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.AsByte(pRec   : Pointer ;
                       nField : Integer  ) : Byte ;
  Begin  { TxBase.AsByte }
    Result := Byte(GetFieldPtr(pRec , nField)^) ;
  End ;  { TxBase.AsByte }


{***********************************************************************
*                                                                      *
*       TxBase.AsShortInt                                              *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.AsShortInt(pRec   : Pointer ;
                           nField : Integer  ) : ShortInt ;
  Begin  { TxBase.AsShortInt }
    Result := ShortInt(GetFieldPtr(pRec , nField)^) ;
  End ;  { TxBase.AsShortInt }


{***********************************************************************
*                                                                      *
*       TxBase.AsHexStr                                                *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.AsHexStr(pRec   : Pointer ;
                         nField : Integer  ) : String ;
  Begin  { TxBase.AsInteger }
    Result := MakeHexStr(GetFieldPtr(pRec , nField) , GetFieldWidth(nField)) ;
  End ;  { TxBase.AsInteger }
{.PA}
{***********************************************************************
*                                                                      *
*       TxBase.IsHeaderDirty                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsHeaderDirty : Boolean ;
  Begin  { TxBase.IsHeaderDirty }
    With GetDataAreaPtr^ do
      Begin
        Result := dbfHeaderDirty ;
      End ;
  End ;  { TxBase.IsHeaderDirty }


{***********************************************************************
*                                                                      *
*       TxBase.SetHeaderDirty                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetHeaderDirty(bDirty : Boolean) ;
  Begin  { TxBase.SetHeaderDirty }
    With GetDataAreaPtr^ do
      Begin
        dbfHeaderDirty := bDirty ;
      End ;
  End ;  { TxBase.SetHeaderDirty }


{***********************************************************************
*                                                                      *
*       TxBase.GetFixedHeaderPtr                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetFixedHeaderPtr : xBaseFixedHeaderPtr ;
  Begin  { TxBase.GetFixedHeaderPtr }
    With GetHeaderPtr^ do
      Result := @dbfFixedHeader ;
  End ;  { TxBase.GetFixedHeaderPtr }


{***********************************************************************
*                                                                      *
*       TxBase.SetLanguageDriver                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetLanguageDriver(nLanguage : Byte) ;
  Begin  { TxBase.SetLanguageDriver }
    With GetFixedHeaderPtr^ do
      LanguageDriver := nLanguage ;
  End ;  { TxBase.SetLanguageDriver }


{***********************************************************************
*                                                                      *
*       TxBase.GetLanguageDriver                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetLanguageDriver : Byte ;
  Begin  { TxBase.GetLanguageDriver }
    Result := GetFixedHeaderPtr^.LanguageDriver ;
  End ;  { TxBase.GetLanguageDriver }


{***********************************************************************
*                                                                      *
*       TxBase.GetLanguageDriverDesc                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetLanguageDriverDesc : String ;
  Begin  { TxBase.GetLanguageDriverDesc }
    Case GetLanguageDriver of
      $01 : Result := 'DOS USA'          ;
      $02 : Result := 'DOS Multilingual' ;
      $03 : Result := 'Windows ANSI'     ;
      $C8 : Result := 'Windows EE'       ;
      $64 : Result := 'EE MS-DOS'        ;
      $65 : Result := 'Nordic MS-DOS'    ;
      $66 : Result := 'Russian MS-DOS'   ;
    Else
      Result := 'Default language' ;
    End ;
  End ;  { TxBase.GetLanguageDriverDesc }


{***********************************************************************
*                                                                      *
*       TxBase.GetCodePage                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetCodePage : Integer ;
  Begin  { TxBase.GetCodePage }
    Case GetLanguageDriver of
      $01 : Result :=  437 ;
      $02 : Result :=  850 ;
      $03 : Result := 1251 ;
      $C8 : Result := 1250 ;
      $64 : Result :=  852 ;
      $66 : Result :=  866 ;
      $65 : Result :=  865 ;
    Else
      Result := 0 ;
    End ;
  End ;  { TxBase.GetCodePage }


{***********************************************************************
*                                                                      *
*       TxBase.GetProductionMDX                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetProductionMDX : Byte ;
  Begin  { TxBase.GetProductionMDX }
    Result := GetFixedHeaderPtr^.ProductionMDX ;
  End ;  { TxBase.GetProductionMDX }


{***********************************************************************
*                                                                      *
*       TxBase.GetProductionMDX                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetProductionMDX(nByte : Byte) ;
  Begin  { TxBase.GetProductionMDX }
    GetFixedHeaderPtr^.ProductionMDX := nByte ;
  End ;  { TxBase.GetProductionMDX }


{***********************************************************************
*                                                                      *
*       TxBase.FixCharacterField                                       *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.FixCharacterField(    pRec   : Pointer ;
                                       nField : Integer ;
                                   Var bFixed : Boolean  ) ;
  Var
    pFld    : pAsciiZ ;
    pNewFld : pAsciiZ ;
    nI , nJ ,
    nWidth  : Integer ;

  Begin  { TxBase.FixCharacterField }
    bFixed := False ;
    nWidth := GetFieldWidth(nField) ;
    GetMemCheck(Pointer(pNewFld) , nWidth) ;

    pFld := GetFieldPtr(pRec , nField) ;

    If dbfOptions.NullsToBlanks then
      Begin
        For nI := 0 to (nWidth - 1) do
          If pFld^[nI] = #$00 then
            Begin
              pFld^[nI] := ' ' ;
              bFixed := True ;
            End ;
      End
    Else
      If dbfOptions.TruncateAtNull then
        Begin
          nI := 0 ;
          nJ := 0 ;
          FillChar(pNewFld^ , nWidth , ' ') ;

          Repeat
            If pFld^[nI] = #$00 then
              Begin
                bFixed := True ;
                Break
              End ;

            pNewFld^[nJ] := pFld^[nI] ;
            Inc(nI) ;
            Inc(nJ) ;
          Until nI >= nWidth ;

          If bFixed then
            Move(pNewFld^ , pFld , nWidth) ;
        End ;

    If dbfOptions.UnprintableToBlank then
      Begin
        For nI := 0 to (nWidth - 1) do
          If pFld^[nI] in NonPrintableChars then
            Begin
              pFld^[nI] := ' ' ;
              bFixed := True ;
            End ;
      End ;

    FreeMemCheck(Pointer(pNewFld) , nWidth) ;
  End ;  { TxBase.FixCharacterField }


{***********************************************************************
*                                                                      *
*       TxBase.GetRecordDescPtr                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.GetRecordDescPtr : dbfRecordDescPtr ;
  Begin  { TxBase.GetRecordDescPtr }
    With GetHeaderPtr^ do
      Result := @dbfRecordDesc ;
  End ;  { TxBase.GetRecordDescPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetFieldDescPtr                                         *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.GetFieldDescPtr(nField : Integer) : dbfFieldDescPtr ;
  Begin  { TxBase.GetFieldDescPtr }
    Result := Pointer(Integer(GetRecordDescPtr) + ((nField - 1) * SizeOf(dbfFieldDescType))) ;
  End ;  { TxBase.GetFieldDescPtr }


{***********************************************************************
*                                                                      *
*       TxBase.GetMemoOffsetDBV                                        *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function TxBase.GetMemoOffsetDBV(pRec   : Pointer ;
                                 nField : Integer  ) : Integer ;
  Var
    nI    : Integer             ;
    oWork : Array[0..5] of Byte ;

  Begin  { TxBase.GetMemoOffsetDBV }
    Move(GetFieldPtr(pRec , nField)^ , oWork , 6) ;

    For nI := 0 to 5 do
      oWork[nI] := (oWork[nI] and 127) or (((oWork[5] shr nI) and 1) shl 7) ;

    Result := TDBVStru(oWork).Size ;
  End ;  { TxBase.GetMemoOffsetDBV }


{***********************************************************************
*                                                                      *
*       TxBase.SetUserIndex                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.SetUserIndex(nIdx : Integer  ) ;
  Begin  { TxBase.SetUserIndex }
    GetDataAreaPtr^.dbfUserIndex := nIdx ;
  End ;  { TxBase.SetUserIndex }


{***********************************************************************
*                                                                      *
*       TxBase.GetUserIndex                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetUserIndex : Integer ;
  Begin  { TxBase.GetUserIndex }
    Result := GetDataAreaPtr^.dbfUserIndex ;
  End ;  { TxBase.GetUserIndex }


{***********************************************************************
*                                                                      *
*       TxBase.GetOptionsPtr                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.GetOptionsPtr : pTdbfOptions ;
  Begin  { TxBase.GetOptionsPtr }
    Result := @dbfOptions ;
  End ;  { TxBase.GetOptionsPtr }


{***********************************************************************
*                                                                      *
*       TxBase.CloneFileHeader                                         *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TxBase.CloneFileHeader(cFile : String) ;
  Var
    oFile : TFileStreamX ;

    nBytesWritten : Integer ;

  Begin  { TxBase.CloneFileHeader }
    Try
      oFile := TFileStreamX.Create(cFile , fmCreate) ;
    Except
      Begin
        MessageDlg('Error cloning file header in TxBase.CloneFileHeader for [' + cFile + ']' ,
                   mtError ,
                   [mbOK]  ,
                   0        ) ;
        oFile := nil ;
      End ;
    End ;

    If oFile <> nil then
      Try
        nBytesWritten := oFile.Write(GetHeaderPtr^ , GetHeaderSize) ;
        If nBytesWritten <> GetHeaderSize then
          Begin
            MessageDlg('Header bytes not written properly in TxBase.CloneFileHeader' ,
                       mtError ,
                       [mbOK]  ,
                       0        ) ;
          End ;
      Finally
        FreeAndNil(oFile) ;
      End ;
  End ;  { TxBase.CloneFileHeader }


{***********************************************************************
*                                                                      *
*       TxBase.IsDataFileOpen                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsDataFileOpen : Boolean ;
  Begin  { TxBase.IsDataFileOpen }
    Result := GetDataAreaPtr^.dbfOpened ;
  End ;  { TxBase.IsDataFileOpen }


{***********************************************************************
*                                                                      *
*       TxBase.IsMemoFileOpen                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function TxBase.IsMemoFileOpen : Boolean ;
  Begin  { TxBase.IsMemoFileOpen }
    Result := GetDataAreaPtr^.dbfMemoOpened ;
  End ;  { TxBase.IsMemoFileOpen }


{***********************************************************************
*                                                                      *
*       TxBase.IsAtBOF                                                 *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

Function TxBase.IsAtBOF(nRec : Integer) : Boolean ;
  Begin  { TxBase.IsAtBOF }
    If GetRecordCount > 0 then
      Result := (nRec = 1)
    Else
      Result := True ;
  End ;  { TxBase.IsAtBOF }


{***********************************************************************
*                                                                      *
*       TxBase.IsAtEOF                                                 *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

Function TxBase.IsAtEOF(nRec : Integer) : Boolean ;
  Begin  { TxBase.IsAtEOF }
    If GetRecordCount > 0 then
      Result := (nRec = GetRecordCount)
    Else
      Result := True ;
  End ;  { TxBase.IsAtEOF }
{.PA}
{***********************************************************************
*                                                                      *
*       GetVersionStr                                                  *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function GetVersionStr : String ;
  Begin  { GetVersionStr }
    Result := '1.04' ;
  End ;  { GetVersionStr }


{***********************************************************************
*                                                                      *
*       GetMinDataFileSize                                             *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Function GetMinDataFileSize : Integer ;
  Begin  { GetMinDataFileSize }
    Result := dbMinDataFileSize ;
  End ;  { GetMinDataFileSize }


{***********************************************************************
*                                                                      *
*       GetFieldTypeIdx                                                *
*                                                                      *
*         Return the index of field type from a field name.            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function GetFieldTypeIdx(cName : String) : Integer ;
  Var
    nI : Integer ;

  Begin  { GetFieldTypeIdx }
    nI := 0 ;
    Repeat
      nI := nI + 1 ;
      If AnsiUpperCase(cName) = AnsiUpperCase(FieldTypeNames[nI]) then
        Begin
          Result := nI ;
          Exit ;
        End ;
    Until (nI = NoOfFieldTypes) ;
    Result := 0 ;
  End ;  { GetFieldTypeIdx }


{***********************************************************************
*                                                                      *
*       GetFieldTypeChar                                               *
*                                                                      *
*         Return the field type char from an index.                    *
*                                                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function GetFieldTypeChar(nI : Integer) : Char ;
  Begin  { GetFieldTypeChar }
    If (nI >= 1) and (nI <= NoOfFieldTypes) then
      Result := AllFields[nI]
    Else
      Result := #0 ;
  End ;  { GetFieldTypeChar }


{***********************************************************************
*                                                                      *
*       GetFieldTypeCharFromName                                       *
*                                                                      *
*         Return the field type char from an field type name.          *
*                                                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function GetFieldTypeCharFromName(cName : String) : Char ;
  Begin  { GetFieldTypeCharFromName }
    Result := GetFieldTypeChar(GetFieldTypeIdx(cName)) ;
  End ;  { GetFieldTypeCharFromName }


{***********************************************************************
*                                                                      *
*       GetTypeNameFromType                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function GetTypeNameFromType(cType : Char) : String ;
  Var
    nIdx : Integer ;

  Begin  { GetTypeNameFromType }
    Try
      nIdx := PosStr(cType , AllFields) ;
      If nIdx > 0 then
        Result := FieldTypeNames[nIdx]
      Else
        Result := 'Unknown' ;
    Except
      Result := 'Unknown' ;
    End ;
  End ;  { GetTypeNameFromType }


{***********************************************************************
*                                                                      *
*      TxBase.FieldSpaces                                              *
*                                                                      *
*      Modification                                                    *
*      ============                                                    *
*                                                                      *
***********************************************************************}

Function TxBase.FieldSpaces(Const nField : Word) : String ;
  Begin  { TxBase.FieldSpaces }
    Result := Spaces(GetFieldWidth(nField))
  End ;  { TxBase.FieldSpaces }

End.

