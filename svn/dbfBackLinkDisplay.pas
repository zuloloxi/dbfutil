{***********************************************************************
*                                                                      *
*       dbfBackLinkDisplay.pas                                         *
*                                                                      *
*       (C) Copyright 1982-2008 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfBackLinkDisplay ;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls,

  JvListBox    ,
  JvExStdCtrls ,
  JvEdit       ,

  bcClasses         ,
  bcDialogUtilities ,
  bcNewList         ,
  bcNumericEdit     ,
  bcPanelSplitters  ,

  dbfCommon    ,
  dbfStructure   ;

Type
  TfrmBackLink = Class(TForm)
    Panel1: TPanel;
    pnlFileName: TPanel;
    pnlMiddle: TPanel;
    sbrStatus: TStatusBar;
    sleFileName: TJvEdit;
    Label1: TLabel;
    tlbBackLink: TNewTextListBox;

    procedure FormActivate(Sender: TObject);

  Protected
    oOwner : TComponent ;

  Private
    xDbfPtr : pTxBase ;

    Procedure SetStatusText(cMsg : String) ;

  Public
    Constructor Create(aOwner : TComponent ;
                       xPtr   : pTxBase     ) ; Reintroduce ;
    Destructor Destroy ; override ;

  End;


Implementation

{$R *.DFM}

{***********************************************************************
*                                                                      *
*       TfrmBackLink.Create                                            *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Constructor TfrmBackLink.Create(aOwner : TComponent ;
                                xPtr   : pTxBase     ) ;
  Begin  { TfrmBackLink.Create }
    CheckDatabasePtr(xPtr) ;

    oOwner := aOwner ;
    Inherited Create(oOwner) ;

    xDbfPtr := xPtr ;
  End ;  { TfrmBackLink.Create }


{***********************************************************************
*                                                                      *
*       TfrmBackLink.Destroy                                           *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Destructor TfrmBackLink.Destroy ;
  Begin  { TfrmBackLink.Destroy }
    Inherited Destroy ;
  End ;  { TfrmBackLink.Destroy }


{***********************************************************************
*                                                                      *
*       TfrmBackLink.SetStatusText                                     *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmBackLink.SetStatusText(cMsg : String) ;
  Begin  { TfrmBackLink.SetStatusText }
    sbrStatus.SimpleText := '' ;
    Sleep(250) ;
    sbrStatus.SimpleText := cMsg ;
  End ;  { TfrmBackLink.SetStatusText }


{***********************************************************************
*                                                                      *
*       TfrmRepairHeader.FormActivate                                  *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Procedure TfrmBackLink.FormActivate(Sender : TObject) ;
  Begin  { TfrmRepairHeader.FormActivate }
    SetStatusText('Initializing..') ;
    sleFileName.Text := xDbfPtr^.GetDataFileName ;
    tlbBackLink.Clear ;
    tlbBackLink.Items.Add(xDbfPtr^.GetBackLinkStr) ;
    SetStatusText('Done') ;
  End ;  { TfrmRepairHeader.FormActivate }

End.
