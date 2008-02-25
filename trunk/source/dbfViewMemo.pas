{***********************************************************************
*                                                                      *
*       dbfViewMemo.pas                                                *
*                                                                      *
*       (C) Copyright 1990-2002 Bruce K. Christensen                   *
*                                                                      *
*       Modifications                                                  *
*       =============                                                  *
*                                                                      *
***********************************************************************}

Unit dbfViewMemo ;

interface

Uses
  Windows , Messages , SysUtils , Classes , Graphics , Controls , Menus ,
  Forms   , Dialogs  , StdCtrls , ClipBrd , bcMemo ;

Type
  TMemoDisplayForm = Class(TForm)
                       MemoWindow     : TExtendedMemo ;
                       MemoFontDialog : TFontDialog   ;
                       SetFont        : TButton       ;
                       Translate      : TButton       ;

                       MainMenu1 : TMainMenu ;
                       Edit1     : TMenuItem ;
                       Cut1      : TMenuItem ;
                       Copy1     : TMenuItem ;
                       Paste1    : TMenuItem ;
                       Delete1   : TMenuItem ;

                     Procedure SetFontClick(Sender : TObject) ;
                     procedure Cut1Click(Sender: TObject);
                     procedure Copy1Click(Sender: TObject);
                     procedure Paste1Click(Sender: TObject);
                     procedure Delete1Click(Sender: TObject);

                     Private  { Private declarations }

                     Public   { Public declarations }

                     End ;

Var
  MemoDisplayForm : TMemoDisplayForm ;

implementation

{$R *.DFM}

procedure TMemoDisplayForm.SetFontClick(Sender: TObject);
begin
  MemoFontDialog.Execute ;
  MemoWindow.Font := MemoFontDialog.Font ;
end;

procedure TMemoDisplayForm.Cut1Click(Sender: TObject);
begin
  SendMessage(ActiveControl.Handle, WM_Cut, 0, 0);
end;

procedure TMemoDisplayForm.Copy1Click(Sender: TObject);
begin
  SendMessage(ActiveControl.Handle, WM_Copy, 0, 0);
end;

procedure TMemoDisplayForm.Paste1Click(Sender: TObject);
begin
  SendMessage(ActiveControl.Handle, WM_Paste, 0, 0);
end;

procedure TMemoDisplayForm.Delete1Click(Sender: TObject);
begin
  SendMessage(ActiveControl.Handle, WM_Clear, 0, 0);
end;

end.
