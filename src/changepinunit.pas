unit ChangePINUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, SysUtils, Forms, Controls, Graphics, Dialogs, MaskEdit, StdCtrls,
  ExtCtrls, SynCommons, modelunit, globalsunit;

type

  { TChangePINDialog }

  TChangePINDialog = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    NewPinEdit: TMaskEdit;
    OldPinEdit: TMaskEdit;
    WrongLabel: TLabel;
    OldPINLabel: TLabel;
    NewPINLabel: TLabel;
    Panel1: TPanel;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewPinEditClick(Sender: TObject);
    procedure NewPinEditEditingDone(Sender: TObject);
    procedure NewPinEditExit(Sender: TObject);
    procedure NewPinEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure OldPinEditEditingDone(Sender: TObject);
    procedure OldPinEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private

  public

  end;

var
  ChangePINDialog: TChangePINDialog;

implementation

{$R *.lfm}

{ TChangePINDialog }

procedure TChangePINDialog.ApplyButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
  Close;
end;

procedure TChangePINDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TChangePINDialog.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  who: TSomeone;
  MinSomeoneID: RawUTF8;
begin
  if (NewPinEdit.Text <> OldPinEdit.Text) then begin
    MinSomeoneID := Database.OneFieldValue(TSomeOne, 'MIN(ID)', '');
    who := TSomeone.Create(Database, MinSomeoneID);
    try
      who.SetPassWord(NewPinEdit.Text);
      if Database.TableRowCount(TSomeone) > 0 then
        Database.Update(who)
      else
        Database.Add(who, True);
    finally
      who.Free;
    end;
  end;
  CanClose := True;
end;

procedure TChangePINDialog.FormCreate(Sender: TObject);
begin
  OldPinEdit.PasswordChar := '*';
  NewPinEdit.PasswordChar := '*';
  NewPinEdit.Enabled := False;
end;

procedure TChangePINDialog.FormShow(Sender: TObject);
begin
  OldPinEdit.SetFocus;
  OldPinEdit.Enabled := True;
end;

procedure TChangePINDialog.NewPinEditClick(Sender: TObject);
begin

end;

procedure TChangePINDialog.NewPinEditEditingDone(Sender: TObject);
begin
  ApplyButton.SetFocus;
end;

procedure TChangePINDialog.NewPinEditExit(Sender: TObject);
begin
  if (NewPinEdit.Text <> OldPinEdit.Text) then
    ApplyButton.SetFocus
  else
    CancelButton.SetFocus;
end;

procedure TChangePINDialog.NewPinEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_RETURN then begin
    ApplyButton.Click; key := 0;
  end else if key = VK_ESCAPE then begin
    CancelButton.Click; key := 0;
  end;
end;

procedure TChangePINDialog.OldPinEditEditingDone(Sender: TObject);
var
  anon: TSomeone;
  pw: RawUTF8;
  MinSomeoneID: RawUTF8;
begin
  MinSomeoneID := Database.OneFieldValue(TSomeOne, 'MIN(ID)', '');
  anon := TSomeone.Create(Database, MinSomeoneID);
  if (anon <> Nil) then begin
    pw := OldPinEdit.Text;
    if (anon.CheckPassword(pw)) then begin
      WrongLabel.Caption := '';
      NewPinEdit.Enabled := True;
      NewPinEdit.SetFocus;
    end else begin
      WrongLabel.Caption := 'INCORRECT, Try Again';
      NewPinEdit.Enabled := False;
      OldPinEdit.SetFocus;
    end;
  end;
end;

procedure TChangePINDialog.OldPinEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_RETURN then begin
    TEdit(Sender).OnEditingDone(Self); // Enter as well as tab completes edit
    Key := 0;
  end;
end;

end.

