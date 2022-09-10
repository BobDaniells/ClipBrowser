unit AttributionDialogUnit;

{$mode objfpc}{$H+}

{ Optionally attach a description to a video }

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, SynCommons, mORMot, globalsunit, modelunit;

type

  { TAttributionDialog }

  TAttributionDialog = class(TForm)
    AttributionMemo: TMemo;
    CloseButton: TButton;
    CancelButton: TButton;
    BottomPanel: TPanel;
    ButtonPanel: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure AttributionMemoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    fID: TID;
    fAttributionChanged: Boolean;
    procedure SetID(Value: TID);
  public
    property ID: TID read fID write SetID;
  end;

var
  AttributionDialog: TAttributionDialog;

implementation

{$R *.lfm}

{ TAttributionDialog }

procedure TAttributionDialog.CloseButtonClick(Sender: TObject);
var
  clp: TVideoClip;
begin
  if fAttributionChanged and
    (MessageDlg('Save', 'Save Changes (Y/N)', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
    clp := TVideoClip.Create(Database, fID);
    try
      clp.Attribution := AttributionMemo.Text;
      Database.Update(clp);
    finally
      clp.Free;
    end;
  end;
end;

procedure TAttributionDialog.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAttributionDialog.AttributionMemoChange(Sender: TObject);
begin
  fAttributionChanged := True;
end;

procedure TAttributionDialog.FormCreate(Sender: TObject);
begin
  fAttributionChanged := False;
end;

procedure TAttributionDialog.SetID(Value: TID);
var
  clp: TVideoClip;
begin
  if fID <> Value then begin
    fID := Value;
    clp := TVideoClip.Create(Database, fID);
    try
      AttributionMemo.Text := clp.Attribution;
    finally
      clp.Free;
    end;
  end;
end;

end.

