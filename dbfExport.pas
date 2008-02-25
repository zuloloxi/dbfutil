{***********************************************************************
*                                                                      *
*       dbfExport.pas                                                  *
*                                                                      *
*       (C) Copyright 1982-2002 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfExport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs ,
  ExtCtrls, StdCtrls, Mask, Buttons,

  JvEdit       ,
  JvExStdCtrls ,
  JvToolEdit   ,

  bcClasses        ,
  bcNumericEdit    ,
  bcPanelSplitters ,

  dbfCommon    ,
  dbfStructure   ;

type
  TfrmExport =
    class(TForm)
    pnlMain: TPanel;
    pnlButtons: TPanel;
    PanelSplitterBottom1: TPanelSplitterBottom;
    pnlTop: TPanel;
    Label1: TLabel;
    btnExport: TBitBtn;
    FnpNumericEdit1: TFnpNumericEdit;
    Label2: TLabel;
    sleOutFileName: TJvEdit;
    procedure FormShow(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
      private
        oOwner : TComponent ;

        xDbfPtr : pTxBase ;

        cOutFileName : String ;

        Procedure ExportData(cFileName : String) ;

      Public
        Constructor Create(aOwner : TComponent ;
                           xPtr   : pTxBase     ) ; reintroduce ;
        Destructor Destroy ; override ;
      End ;

var
  frmExport: TfrmExport ;

implementation
  Uses
    bcStringUtilities ;

  Const
    cExportExtension = 'dlm' ;

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfrmExport.Create                                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Constructor TfrmExport.Create(aOwner : TComponent ;
                              xPtr   : pTxBase     ) ;
  Begin  { TfrmExport.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr ;
  End ;  { TfrmExport.Create }


{***********************************************************************
*                                                                      *
*       TfrmExport.Destroy                                             *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Destructor TfrmExport.Destroy ;
  Begin  { TfrmExport.Destroy }
    Try
      Inherited Destroy ;
    Except
      ShowMessage('Destroy exception.') ;
    End ;
  End ;  { TfrmExport.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmExport.ExportData                                          *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmExport.ExportData(cFileName : String) ;
  Var
    nRecord : Integer ;
    nField  : Integer ;
    cFld    : String  ;
    cLine   : String  ;
    cQuote  : String  ;

    oFile         : File    ;
    nBytesWritten : Integer ;

  Begin  { TfrmExport.ExportData }
    AssignFile(oFile , cFileName) ;
    ReWrite(oFile , 1) ;

    With xDbfPtr^ do
      For nRecord := 1 to GetRecordCount do
        Begin
          cLine := '' ;

          If GetRecord(nRecord) then
            Begin
              For nField := 0 to GetFieldCount do
                Begin
                  cFld := GetFldStr(nField) ;
                  If (GetFieldWidth(nField) > 1) and (nField > 0) then
                    cFld := Trim(cFld) ;
                  If Length(cFld) = 0 then
                    cFld := ' ' ;

                  If cFld = ' ' then
                    cFld := '" "'
                  Else
                    If PosStr(',' , cFld) > 0 then
                      cQuote := '"'
                    Else
                      cQuote := '' ;

                  cLine := cLine + ',' + cQuote + cFld + cQuote ;
                End ;  { With xDbfPtr^ do }

              Delete(cLine , 1 , 1) ;
              cLine := cLine + Chr(13) + Chr(10) ;

              BlockWrite(oFile , cLine[1] , Length(cLine) , nBytesWritten) ;
              cLine := '' ;
            End ;
        End ;

      CloseFile(oFile) ;
  End ;  { TfrmExport.ExportData }


{***********************************************************************
*                                                                      *
*       TfrmExport.FormShow                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmExport.FormShow(Sender: TObject);
  Begin  { TfrmExport.FormShow }
    cOutFileName := ChangeFileExt(xDbfPtr^.GetDataFileName , '.' + cExportExtension) ;
    sleOutFileName.Text := cOutFileName ;
  End ;  { TfrmExport.FormShow }


{***********************************************************************
*                                                                      *
*       TfrmExport.btnExportClick                                      *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmExport.btnExportClick(Sender: TObject);
  Begin  { TfrmExport.btnExportClick }
    ExportData(cOutFileName) ;
  End ;  { TfrmExport.btnExportClick }

End.
