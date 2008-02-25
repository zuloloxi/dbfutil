{***********************************************************************
*                                                                      *
*       dbfOptions.pas                                                 *
*                                                                      *
*       (C) Copyright 1990-2001 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

unit dbfOptions ;

Interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons ,

  bcPopCalendar , bcStringUtilities , bcScreenUtilities ,

  dbfCommon, bcPanelSplitters ;

type
  TfmOptions = class(TForm)
    pnlMain: TPanel;
    PanelSplitterTop1: TPanelSplitterTop;
    pnlTop: TPanel;
    PanelSplitterTop2: TPanelSplitterTop;
    pnlBottom: TPanel;
    Label1: TLabel;
    btnMinDate: TBitBtn;
    Label2: TLabel;

    edtMinDate: TEdit;
    edtMaxDate: TEdit;

    btnMaxDate: TBitBtn;
    procedure btnMinDateClick(Sender: TObject);
    procedure btnMaxDateClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

  Private
    Procedure PopupCalendar(edt : TEdit) ;

  Public

  End;

implementation

{$R *.DFM}


{***********************************************************************
*                                                                      *
*       TfmOptions.PopupCalendar                                       *
*                                                                      *
*         Pop up calendar for specified date edit.                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfmOptions.PopupCalendar(edt : TEdit) ;
  Var
    dDate : TDateTime ;

  Begin  { TfmOptions.PopupCalendar }
    dDate := ShowPopCal(StrToDateChk(edt.Text)) ;
    If dDate > 0 then
      edt.Text := DateToStr(dDate)  // a valid date entered
    Else
      edt.Text := DateToStr(SysUtils.Date) ;                            
  End ;  { TfmOptions.PopupCalendar }


{***********************************************************************
*                                                                      *
*       TfmOptions.btnMinDateClick                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfmOptions.btnMinDateClick(Sender : TObject) ;
  Begin  { TfmOptions.btnMinDateClick }
    PopupCalendar(edtMinDate) ;
  End ;  { TfmOptions.btnMinDateClick }


{***********************************************************************
*                                                                      *
*       TfmOptions.btnMaxDateClick                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfmOptions.btnMaxDateClick(Sender : TObject) ;
  Begin  { TfmOptions.btnMaxDateClick }
    PopupCalendar(edtMaxDate) ;
  End ;  { TfmOptions.btnMaxDateClick }


{***********************************************************************
*                                                                      *
*       TfmOptions.FormActivate                                        *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfmOptions.FormActivate(Sender : TObject) ;
  Begin  { TfmOptions.FormActivate }
    FormCentre(@Self) ;

    edtMinDate.Text := DateToStr(dbfMinDate) ;
    edtMaxDate.Text := DateToStr(dbfMaxDate) ;
  End ;  { TfmOptions.FormActivate }

End.
