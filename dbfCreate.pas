{***********************************************************************
*                                                                      *
*       dbfCreate.PAS                                                  *
*                                                                      *
*       (C) Copyright 1990-1999 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfCreate ;
  Interface
    Uses
      Dialogs , SysUtils , Windows ,

      bcClasses      ,
      bcStringUtilities ,

      dbfStructure ;

  Var
    MaxBufferSize : Word ;  { Maximum buffer size that can be allocated }
                            { via GetMem.                               }
    TypeFileStr : String ;
    TypeFile    : Text   ;

    NewFile : NewFile_Type ;  { New file data structure. }

  Type
    aString6 = Array[NewFieldType] Of String[7] ;

  Const
    NewFieldTypeDesc: aString6 = ('String'  ,
                                  'AsciiZ'  ,
                                  'Boolean' ,
                                  'Date'    ,
                                  'Byte'    ,
                                  'Word'    ,
                                  'Integer' ,
                                  'Float'   ,
                                  'Char'     ) ;

  Type
    Function_Type = Function(Var S: String;
      Var D: Pointer;
      Var sNewField: NewFieldDesc_Type): Boolean;

    FuncArray_Type = Array[NewFieldType] Of Function_Type;

    FromFunc_Type = Function(Var S      : Pointer ;
                                 FldNo  : Byte    ;
                                 InLen  : Byte    ;
                             Var RecPtr           ;
                                 dbPtr  : pTxBase  ) : Boolean ;
    FromFuncArray_Type = Array[NewFieldType] Of FromFunc_Type ;

  Var
    ConvertToArray: FuncArray_Type ;  { Array of conversion to functions. }
{.PA}
{***********************************************************************
*                                                                      *
*       Interfaced conversion routines.                                *
*                                                                      *
***********************************************************************}

  Function ConvertToPascalStr(Var S         : String            ;
                              Var D         : Pointer           ;
                              Var sNewField : NewFieldDesc_Type  ) : Boolean ;

  Function ConvertToChar(Var S         : String            ;
                         Var D         : Pointer           ;
                         Var sNewField : NewFieldDesc_Type  ) : Boolean ;

  Function ConvertToAsciizStr(Var S         : String            ;
                              Var D         : Pointer           ;
                              Var sNewField : NewFieldDesc_Type  ) : boolean ;

  Function ConvertToBoolean(Var S         : String            ;
                            Var D         : Pointer           ;
                            Var sNewField : NewFieldDesc_Type  ) : Boolean ;

  Function ConvertToDate(Var S         : String            ;
                         Var D         : Pointer           ;
                         Var sNewField : NewFieldDesc_Type  ) : Boolean ;

  Function ConvertToByte(Var S         : String            ;
                         Var D         : Pointer           ;
                         Var sNewField : NewFieldDesc_Type  ) : Boolean ;

  Function ConvertToWord(Var S         : String            ;
                         Var D         : Pointer           ;
                         Var sNewField : NewFieldDesc_Type  ) : Boolean ;

  Function ConvertToInteger(Var S         : String            ;
                            Var D         : Pointer           ;
                            Var sNewField : NewFieldDesc_Type  ) : Boolean ;

  Function ConvertToLongInt(Var S         : String            ;
                            Var D         : Pointer           ;
                            Var sNewField : NewFieldDesc_Type  ) : Boolean ;

  Function ConvertToFloat(Var S         : String            ;
                          Var D         : Pointer           ;
                          Var sNewField : NewFieldDesc_Type  ) : Boolean ;

  Procedure InitializeConversion ;

Implementation
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertToPascalStr                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToPascalStr(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Var
  cStr: ShortString;

Begin                                   { ConvertToPascalStr }
    { Trim excess bytes in needed. }
  If (Length(S) > (sNewField.NewWidth - 1)) Or
    (Length(S) > (SizeOf(ShortString) - 1)) Then
    cStr := Copy(S, 1, sNewField.NewWidth - 1)
  Else
    cStr := S;
  cStr := Trim(cStr);

  FillMemory(D, sNewField.NewWidth, Byte(' '));
  Move(cStr, D^, Length(cStr));
  Result := True;
End;                                    { ConvertToPascalStr }


{***********************************************************************
*                                                                      *
*       ConvertToChar                                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToChar(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Var
  cStr: String;

Begin                                   { ConvertToChar }
  cStr := Pad(S, sNewField.NewWidth);

  MoveMemory(@cStr, D, sNewField.NewWidth);
  Result := True;
End;                                    { ConvertToChar }
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertToAsciizStr                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToAsciizStr(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Var
  pChars: Array[1..MaxFieldWidth] Of Char;

Begin                                   { ConvertToAsciizStr }
  MoveMemory(@S, @pChars, sNewField.NewWidth);
  pChars[sNewField.NewWidth] := Char(0); // Set the terminator byte.
  MoveMemory(@pChars, D, sNewField.NewWidth);
  Result := True;
End;                                    { ConvertToAsciizStr }
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertToBoolean                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToBoolean(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Begin                                   { ConvertToBoolean }
  Result := True;
  If S[1] In ['y', 't', 'Y', 'T'] Then
    BooleanPtr(D)^ := True
  Else
    If S[1] In [' ', 'n', 'f', 'N', 'F'] Then
      BooleanPtr(D)^ := False
    Else
      Result := False;
End;                                    { ConvertToBoolean }
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertToDate                                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToDate(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Begin                                   { ConvertToDate }
  If Length(Trim(S)) = 0 Then
    Begin
      TDateTimePtr(D)^ := MinDate;
      Result := True;
    End
  Else
    Result := xDateToDate(S, TDateTimePtr(D)^);
End;                                    { ConvertToDate }
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertToByte                                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToByte(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Var
  TS: String;
  ErrorConvertByte: Integer;

Begin                                   { ConvertToByte }
  Result := True;
  TS := Trim(S);
  If Length(TS) <= 0 Then
    BytePtr(D)^ := 0
  Else
    Begin
      Val(TS, BytePtr(D)^, ErrorConvertByte);
      Result := (ErrorConvertByte = 0);
    End;
End;                                    { ConvertToByte }
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertToWord                                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToWord(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Var
  TS: String;
  ErrorConvertWord: Integer;

Begin                                   { ConvertToWord }
  ConvertToWord := True;
  TS := Trim(S);
  If Length(TS) <= 0 Then
    WordPtr(D)^ := 0
  Else
    Begin
      Val(TS, WordPtr(D)^, ErrorConvertWord);
      Result := (ErrorConvertWord = 0);
    End;
End;                                    { ConvertToWord }
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertToInteger                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToInteger(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Var
  TS: String;
  NewInteger: ^Integer Absolute D;
  ErrorConvertInteger: Integer;

Begin                                   { ConvertInteger }
  ConvertToInteger := True;
  TS := Trim(S);
  If Length(TS) <= 0 Then
    IntegerPtr(D)^ := 0
  Else
    Begin
      Val(TS, IntegerPtr(D)^, ErrorConvertInteger);
      Result := (ErrorConvertInteger = 0);
    End;
End;                                    { ConvertToInteger }
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertToLongInt                                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToLongInt(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Var
  TS: String;
  ErrorConvertLongInt: Integer;

Begin                                   { ConvertToLongInt }
  ConvertToLongInt := True;
  TS := Trim(S);
  If Length(TS) <= 0 Then
    LongIntPtr(D)^ := 0
  Else
    Begin
      Val(TS, LongIntPtr(D)^, ErrorConvertLongInt);
      Result := (ErrorConvertLongInt = 0);
    End;
End;                                    { ConvertToLongInt }
{.PA}
{***********************************************************************
*                                                                      *
*       ConvertToFloat                                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Function ConvertToFloat(Var S: String;
  Var D: Pointer;
  Var sNewField: NewFieldDesc_Type): Boolean;
Var
  TS: String;

Begin                                   { ConvertToFloat }
  Result := True;

  TS := Trim(S);
  If Length(TS) <= 0 Then
    FloatPtr(D)^ := 0.0
  Else
    Begin
      Try
        FloatPtr(D)^ := SysUtils.StrToFloat(TS);
        Result := True;
      Except
        Result := False;
      End;
    End;
End;                                    { ConvertToFloat }
{.PA}
{***********************************************************************
*                                                                      *
*       InitializeConversion                                           *
*                                                                      *
*         Initialize the conversion function array.                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure InitializeConversion;
Begin
  ConvertToArray[PascalStr] := ConvertToPascalStr;
  ConvertToArray[NChar] := ConvertToChar;
  ConvertToArray[AsciizStr] := ConvertToAsciiZStr;
  ConvertToArray[NBoolean] := ConvertToBoolean;
  ConvertToArray[NDate] := ConvertToDate;
  ConvertToArray[NByte] := ConvertToByte;
  ConvertToArray[NWord] := ConvertToWord;
  ConvertToArray[NInteger] := ConvertToInteger;
  ConvertToArray[NFloat] := ConvertToFloat;

    {                                            }
    { Initialize the Pascal data file structure. }
    {                                            }
  ZeroMemory(@NewFile, SizeOf(NewFile));
End;
{.PA}
{***********************************************************************
*                                                                      *
*       Initialization                                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Initialization { dbfCreate }
  InitializeConversion;
End.
