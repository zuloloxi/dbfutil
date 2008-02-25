{***********************************************************************
*                                                                      *
*       dbfConvertFixDBV.pas                                           *
*                                                                      *
*       (C) Copyright 1982-1999 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfConvertFixDBV ;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, JvSplit,

  JvListBox    ,
  JvExStdCtrls ,

  bcClasses        ,
  bcStringUtilities       ,
  bcNewList        ,
  bcPanelSplitters ,

  dbfCommon ,
  dbfStructure    ;

type
  TfrmConvertFixDBV = class(TForm)
    pnlMain: TPanel;
    pnlButtons: TPanel;
    PanelSplitterBottom1: TPanelSplitterBottom;
    pnlTop: TPanel;
    btnConvert: TBitBtn;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    Panel1: TPanel;
    PanelSplitterTop1: TPanelSplitterTop;
    Panel2: TPanel;
    PanelSplitterTop2: TPanelSplitterTop;
    tlbFields    : TNewTextListBox ;
    tlbNewFields : TNewTextListBox ;
    btnRepair   : TBitBtn;
    procedure FormActivate(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure btnRepairClick(Sender: TObject);

  Protected
    oOwner : TComponent ;

  Private
    xDbfPtr  : pTxBase   ;
    oFlds    : TStringList ;
    xNewFile : TxBase      ;

  Public
    Constructor Create(aOwner : TComponent ;
                       xPtr   : pTxBase     ) ; reintroduce ;
    Destructor Destroy ; override ;
  End ;

Implementation

{$R *.DFM}

Uses
  bcFileUtilities ;

{***********************************************************************
*                                                                      *
*       TRecordDisplayForm.Create                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Constructor TfrmConvertFixDBV.Create(aOwner : TComponent ;
                                     xPtr   : pTxBase     ) ;
  Begin  { TfrmConvertFixDBV.Create }
    CheckDatabasePtr(xPtr) ;

    Inherited Create(oOwner) ;

    xDbfPtr := xPtr   ;
    oOwner  := aOwner ;

    oFlds := TStringList.Create ;
  End ;  { TfrmConvertFixDBV.Create }


{***********************************************************************
*                                                                      *
*       TfrmConvertFixDBV.Destroy                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Destructor TfrmConvertFixDBV.Destroy ;
  Begin  { TfrmConvertFixDBV.Destroy }
    FreeAndNil(oFlds) ;

    Inherited Destroy ;
  End ;  { TfrmConvertFixDBV.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmConvertFixDBV.FormActivate                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmConvertFixDBV.FormActivate(Sender : TObject) ;
  Var
    nField : Integer ;
    cField : String  ;

  Begin  { TfrmConvertFixDBV.FormActivate }
    SetBounds((Screen.Width  - Width ) div 2 ,
              (Screen.Height - Height) div 2 ,
              Width                          ,
              Height                          ) ;

    oFlds.Clear ;

    tlbFields.Clear    ;
    tlbNewFields.Clear ;

    With xDbfPtr^ do
      For nField := 1 to GetFieldCount do
        Begin
          cField := FieldCreateStr(nField) ;
          tlbFields.Items.Add(cField) ;

          If IsFlexField(nField) then
            Begin
              cField[12] := 'M' ;

              cField[14] := ' ' ;
              cField[15] := '1' ;
              cField[16] := '0' ;

              cField[18] := ' ' ;
            End ;

          oFlds.Add(cField) ;
        End ;

    tlbNewFields.AddList(oFlds) ;
  End ;  { TfrmConvertFixDBV.FormActivate }


{***********************************************************************
*                                                                      *
*       TfrmConvertFixDBV.btnConvertClick                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmConvertFixDBV.btnConvertClick(Sender : TObject) ;
  Var
    cFileName : String  ;

    nField     : Integer ;
    nRecord    : Integer ;
    nNewRecord : Integer ;
    nMemoNo    : Integer ;
    cMemo      : String  ;
    nMemoLen   : Integer ;

//    nPercent    : LongWord     ;

  Begin  { TfrmConvertFixDBV.btnConvertClick }
    xNewFile := TxBase.Create(Self) ;
    With xNewFile do
      Begin
        cFileName := ForceExtension(xDbfPtr^.GetDataFileName , dbfGoodDataFileExt) ;

        CreateDatabase(cFileName , $83 , oFlds) ;

        For nRecord := 1 to xDbfPtr^.GetTotalRecords do
          Begin
            BlankCurrentRecord ;

            If xDbfPtr^.GetRecord(nRecord) then
              Begin
                For nField := 1 to GetFieldCount do
                  Begin
                    If xDbfPtr^.IsFlexField(nField) then
                      Begin
                        If xDbfPtr^.ReadMemoDBV(nField) then
                          Begin
                            Try
                              cMemo    := MakeStr(xDbfPtr^.GetMemoBufferPtr(nField) ,
                                                  xDbfPtr^.GetMemoBytesRead          ) ;
                              cMemo    := Trim(cMemo)   ;
                              nMemoLen := Length(cMemo) ;
                              nMemoNo  := AppendMemo(@cMemo , nMemoLen) ;
                            Except
                              cMemo   := '' ;
                              nMemoNo := 0 ;
                            End ;

                            SetCurrentMemoField(nMemoNo , nField) ;
                          End
                        Else
                          Begin
//                            ShowMessage('No DBV memo read') ;
                          End ;
                      End
                    Else
                      Move(xDbfPtr^.GetCurrentFieldPtr(nField)^ ,
                           GetCurrentFieldPtr(nField)^          ,
                           GetFieldWidth(nField)                 ) ;
                  End ;

                If not AddRecord(GetRecordPtr , nNewRecord) then
                  Begin
                    ShowMessage('AddBlankRecord failed in TfrmConvertFixDBV.btnConvertClick') ;
                  End ;
              End
            Else
              ShowMessage('GetRecord failed in TfrmConvertFixDBV.btnConvertClick') ;

//            nPercent := (nRecord * 100) div xDbfPtr^.GetTotalRecords ;
          End ;
      End ;
      
    FreeAndNil(xNewFile) ;
  End ;  { TfrmConvertFixDBV.btnConvertClick }


{***********************************************************************
*                                                                      *
*       TfrmConvertFixDBV.btnRepairClick                               *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmConvertFixDBV.btnRepairClick(Sender : TObject) ;
  Var
    cFileName : String  ;

    nField     : Integer ;
    nRecord    : Integer ;
    nNewRecord : Integer ;
    nMemoNo    : Integer ;
    cMemo      : String  ;

  Begin  { TfrmConvertFixDBV.btnRepairClick }
    xNewFile := TxBase.Create(Self) ;
    With xNewFile do
      Begin
        cFileName := ForceExtension(xDbfPtr^.GetDataFileName , dbfGoodDataFileExt) ;

        CreateDatabase(cFileName , $83 , oFlds) ;

        For nRecord := 1 to xDbfPtr^.GetTotalRecords do
          Begin
            BlankCurrentRecord ;

            If xDbfPtr^.GetRecord(nRecord) then
              Begin
                For nField := 1 to GetFieldCount do
                  Begin
                    If xDbfPtr^.IsFlexField(nField) then
                      Begin
                        If xDbfPtr^.ReadMemoDBV(nField) then
                          Begin
                            cMemo   := MakeStr(xDbfPtr^.GetMemoBufferPtr(nField) ,
                                               xDbfPtr^.GetMemoBytesRead          ) ;
                            nMemoNo := AppendMemo(@cMemo[1] , Length(cMemo)) ;

                            SetCurrentMemoField(nMemoNo , nField) ;
                          End
                        Else
                          Begin
//                            ShowMessage('No DBV memo read') ;
                          End ;
                      End
                    Else
                      Move(xDbfPtr^.GetCurrentFieldPtr(nField)^ ,
                           GetCurrentFieldPtr(nField)^          ,
                           GetFieldWidth(nField)                 ) ;
                  End ;

                If not AddRecord(GetRecordPtr , nNewRecord) then
                  Begin
                    ShowMessage('AddBlankRecord failed in TfrmConvertFixDBV.btnConvertClick') ;
                  End ;
              End
            Else
              ShowMessage('GetRecord failed in TfrmConvertFixDBV.btnConvertClick') ;
          End ;
      End ;

    FreeAndNil(xNewFile) ;
  End ;  { TfrmConvertFixDBV.btnRepairClick }

End.
