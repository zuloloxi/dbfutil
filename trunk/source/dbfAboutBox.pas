{***********************************************************************
*                                                                      *
*       dbfAboutBox.pas                                                *
*                                                                      *
*       (C) Copyright 1982-2001 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*       2001-05-11  BKC  Display Windows version.                      *
*       2001-05-27  BKC  Correct caption information.                  *
*                                                                      *
***********************************************************************}

{$I dbfInclude.inc}

Unit dbfAboutBox ;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons ,

  bcClasses     ,
  bcStringUtilities    ,
  bcScreenUtilities ,

  dbfStructure ;

type
  TfrmAboutBox = class(TForm)
    Panel1: TPanel;
    imgProgram: TImage;
    ProductName: TLabel;
    lblVersion: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    btnOK: TBitBtn;
    lblWindozeVer: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


Implementation

{$R *.DFM}
  Uses
    JclSysInfo ;
    
{***********************************************************************
*                                                                      *
*       TfrmAboutBox.FormActivate                                      *
*                                                                      *
*       Modification                                                   *
*       ============                                                   *
*                                                                      *
***********************************************************************}

Procedure TfrmAboutBox.FormActivate(Sender: TObject);
  Begin  { TfrmAboutBox.FormActivate }
    FormCentre(@Self) ;

    lblVersion.Caption := 'Version ' + dbfStructure.GetVersionStr ;
    lblWindozeVer.Caption := GetOSVersionString ;

    DateParInitialize ;
  End ;  { TfrmAboutBox.FormActivate }


{***********************************************************************
*                                                                      *
*       TfrmAboutBox.btnOKClick                                        *
*                                                                      *
*       Modifications                                                  *
*       ===============                                                *
*                                                                      *
***********************************************************************}

Procedure TfrmAboutBox.btnOKClick(Sender : TObject) ;
  Begin  { TfrmAboutBox.btnOKClick }
    Close ;
  End ;  { TfrmAboutBox.btnOKClick }

End.
