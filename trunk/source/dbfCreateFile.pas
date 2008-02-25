{***********************************************************************
*                                                                      *
*       dbfCreateFile.pas                                              *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

unit dbfCreateFile ;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Mask,

  bcClasses     ,
  bcNumericEdit ,
  bcScreenUtilities ,
  bcStringUtilities    ,

  dbfStructure ;

type
  TfmCreateFile = class(TForm)
    pnlName        : TPanel;
    pnlLeft        : TPanel;
    pnlRight       : TPanel;
    btnAddField    : TBitBtn;
    lbFieldList    : TListBox;
    pnlBottomRight : TPanel;
    btnCreate      : TBitBtn;
    pnlSummary     : TPanel;
    edFldCount     : TEdit;
    edRecLen       : TEdit;
    Label4         : TLabel;
    Label5         : TLabel;
    btnFinish      : TBitBtn;
    Label6         : TLabel;
    edFileName     : TEdit;
    Label1         : TLabel;
    edFldName      : TEdit;
    Label2         : TLabel;
    edFldType      : TComboBox;
    Label3         : TLabel;
    edFldWidth     : TFnpNumericEdit;
    lblDecimals    : TLabel;
    edFldDecimals  : TFnpNumericEdit;
    
    Procedure btnAddFieldClick(Sender: TObject);
    Procedure edFldTypeExit(Sender: TObject);
    Procedure edFldWidthChange(Sender: TObject);
    Procedure edFldDecimalsChange(Sender: TObject);
    Procedure btnCreateClick(Sender: TObject);
    procedure btnFinishClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  Private
    xBaseFile : TxBase ;

  Public
    cFileName : String ;
    nRecLen   : Integer ;

    Constructor Create(AOwner: TComponent) ; override ;
  End ;

Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfmCreateFile.Create                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Constructor TfmCreateFile.Create(AOwner: TComponent) ;
  Begin  { TfmCreateFile.Create }
    Inherited Create(AOwner) ;

    cFileName := '' ;
    lbFieldList.Items.Clear ;
    lbFieldList.Items.Add(Pad('DELETE' , 10)         + ' ' +
                          'C'+ ' ' +
                          '  1') ;
    nRecLen := 1 ;
    edRecLen.Text := '   1' ;
    edFldCount.Text := PadLeft(IntToStr(lbFieldList.Items.Count) , 3) ;
  End ;  { TfmCreateFile.Create }


{***********************************************************************
*                                                                      *
*       TfmCreateFile.btnAddFieldClick                                 *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfmCreateFile.btnAddFieldClick(Sender : TObject) ;
  Var
    cFld : Char ;
    cStr : String ;

  Begin  { TfmCreateFile.btnAddFieldClick }
    If ((Length(Trim(edFldName.Text )) > 0) and
        (Length(Trim(edFldType.Text )) > 0) and
        (Length(Trim(edFldWidth.Text)) > 0)    ) then
      Begin
        cFld := edFldType.Text[1] ;
        cStr := Pad(edFldName.Text , 10) + ' ' +
                cFld                     + ' ' +
                PadLeft(Trim(edFldWidth.Text),3) ;
        If cFld in ['F', 'N'] then
          cStr := cStr + ' ' + PadLeft(IntToStr(edFldDecimals.AsInteger) , 2) ;
        lbFieldList.Items.Add(cStr) ;
        edFldCount.Text := PadLeft(IntToStr(lbFieldList.Items.Count) , 3) ;

        nRecLen := nRecLen + edFldWidth.AsInteger ;
        edRecLen.Text := PadLeft(IntToStr(nRecLen) , 4) ;

        edFldName.Text      := '' ;
        edFldType.Text      := '' ;
        edFldWidth.Value    := 0 ;
        edFldDecimals.Value := 0 ;
      End ;

    edFldDecimals.Enabled := False ;
    edFldDecimals.Visible := False ;
    lblDecimals.Visible   := False ;
    edFldName.SetFocus ;
  End ;  { TfmCreateFile.btnAddFieldClick }


{***********************************************************************
*                                                                      *
*       TfmCreateFile.edFldTypeExit                                    *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfmCreateFile.edFldTypeExit(Sender: TObject);
  Var
    cFld : Char ;

  Begin  { TfmCreateFile.edFldTypeExit }
    cFld := edFldType.Text[1] ;
    edFldDecimals.Enabled := (cFld in ['F', 'N']) ;
    edFldDecimals.Visible := edFldDecimals.Enabled ;
    lblDecimals.Visible   := edFldDecimals.Enabled ;
  End ;  { TfmCreateFile.edFldTypeExit }


{***********************************************************************
*                                                                      *
*       TfmCreateFile.edFldWidthChange                                 *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmCreateFile.edFldWidthChange(Sender : TObject) ;
  Begin  { TfmCreateFile.edFldWidthChange }
    If edFldWidth.Value = 0 then
      Text := '' ;
  End ;  { TfmCreateFile.edFldWidthChange }


{***********************************************************************
*                                                                      *
*       TfmCreateFile.edFldDecimalsChange                              *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmCreateFile.edFldDecimalsChange(Sender : TObject) ;
  Begin  { TfmCreateFile.edFldDecimalsChange }
    If edFldDecimals.Value = 0 then
      Text := '' ;
  End ;  { TfmCreateFile.edFldDecimalsChange }


{***********************************************************************
*                                                                      *
*       TfmCreateFile.btnCreateClick                                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfmCreateFile.btnCreateClick(Sender : TObject);
  Var
    nRec : Integer ;
    oFlds : TStringList ;

  Begin  { TfmCreateFile.btnCreateClick }
    cFileName := Trim(edFileName.Text) ;
    oFlds := TStringList.Create ;
    oFlds.Clear ;
    oFlds.AddList(lbFieldList.Items) ;

    With xBaseFile do
      Begin
        { Need to changed signature !!!}
        CreateDatabase(cFileName , $03 , oFlds) ;
        If not AddBlankRecord(nRec) then
          Begin
            ShowMessage('AddBlankRecord failed.') ;
          End ;
      End ;

    FreeAndNil(oFlds) ;
  End ;  { TfmCreateFile.btnCreateClick }


{***********************************************************************
*                                                                      *
*       TfmCreateFile.btnFinishClick                                   *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmCreateFile.btnFinishClick(Sender : TObject) ;
  Begin
    FreeAndNil(xBaseFile) ;
  End ;


{***********************************************************************
*                                                                      *
*       TfmCreateFile.FormActivate                                     *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfmCreateFile.FormActivate(Sender: TObject);
 Begin  { TfmCreateFile.FormActivate }
   FormCentre(@Self) ;
 End ;  { TfmCreateFile.FormActivate }

End.
