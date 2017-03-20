unit confirmation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmConfirm }

  TfrmConfirm = class(TForm)
    btnCancel: TButton;
    btnConfirm: TButton;
    Label1: TLabel;
    lblActionText: TLabel;

    procedure btnCancelClick(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
  private
    FTakeAction: boolean;
    { private declarations }
  public
    property takeAction: boolean read FTakeAction;
    class function shouldTakeAction(commandAction: string):boolean;
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TfrmConfirm }

class function TfrmConfirm.shouldTakeAction(commandAction: string):boolean;
var form : TfrmConfirm;
begin
  form := TfrmConfirm.create(nil);
  form.lblActionText.Caption:=commandAction;
  form.ShowModal;
  result := form.FTakeAction;
  form.free;
end;

procedure TfrmConfirm.btnCancelClick(Sender: TObject);
begin
  FTakeAction := false;
  close;
end;

procedure TfrmConfirm.btnConfirmClick(Sender: TObject);
begin
  FTakeAction := true;
  close;
end;

end.

