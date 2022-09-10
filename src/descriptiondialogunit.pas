unit DescriptionDialogUnit;

{$mode objfpc}{$H+}

{ Optionally attach a description to a video }

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, SynCommons, mORMot, globalsunit, modelunit;

type

  { TDescriptionDialog }

  TDescriptionDialog = class(TForm)
    DescriptionMemo: TMemo;
    CloseButton: TButton;
    CancelButton: TButton;
    BottomPanel: TPanel;
    ButtonPanel: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure DescriptionMemoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    fID: TID;
    fDescriptionChanged: Boolean;
    procedure SetID(Value: TID);
  public
    property ID: TID read fID write SetID;
  end;

var
  DescriptionDialog: TDescriptionDialog;

implementation

{$R *.lfm}

{ TDescriptionDialog }

procedure TDescriptionDialog.CloseButtonClick(Sender: TObject);
var
  clp: TVideoClip;
begin
  if fDescriptionChanged and
    (MessageDlg('Save', 'Save Changes (Y/N)', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
    clp := TVideoClip.Create(Database, fID);
    try
      clp.Description := DescriptionMemo.Text;
      Database.Update(clp);
    finally
      clp.Free;
    end;
  end;
end;

procedure TDescriptionDialog.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TDescriptionDialog.DescriptionMemoChange(Sender: TObject);
begin
  fDescriptionChanged := True;
end;

procedure TDescriptionDialog.FormCreate(Sender: TObject);
begin
  fDescriptionChanged := False;
end;

procedure TDescriptionDialog.SetID(Value: TID);
var
  clp: TVideoClip;
begin
  if fID <> Value then begin
    fID := Value;
    clp := TVideoClip.Create(Database, fID);
    try
      DescriptionMemo.Text := clp.Description;
    finally
      clp.Free;
    end;
  end;
end;

end.

