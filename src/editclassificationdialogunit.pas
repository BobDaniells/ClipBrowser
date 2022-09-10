unit editclassificationdialogunit;

{$mode objfpc}{$H+}

{ Edit classification codes as G, PG, R, or X. }

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mORMot, globalsunit, modelunit, LCLType;

type

  { TEditClassificationDialog }

  TEditClassificationDialog = class(TForm)
    ClassificationRadioGroup: TRadioGroup;
    SaveButton: TButton;
    CancelButton: TButton;
    Panel1: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure ClassificationRadioGroupClick(Sender: TObject);
    procedure ClassificationRadioGroupKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ClassificationRadioGroupSelectionChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    fID: TID;
    fRestrictionChanged: Boolean;
    procedure SetID(Value: TID);
  public
    property ID: TID read fID write SetID;
  end;

var
  EditClassificationDialog: TEditClassificationDialog;

implementation

{$R *.lfm}

{ TEditClassificationDialog }

procedure TEditClassificationDialog.SaveButtonClick(Sender: TObject);
var
  aClip: TVideoClip;
begin
  if fRestrictionChanged then begin
    aClip := TVideoClip.Create(Database, fID);
    try
      aClip.Restriction := ClassificationRadioGroup.Items[ClassificationRadioGroup.ItemIndex];
      Database.Update(aClip);
    finally
      aClip.Free;
    end;
  end;
  Close;
end;

procedure TEditClassificationDialog.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TEditClassificationDialog.ClassificationRadioGroupClick(Sender: TObject);
begin

end;

procedure TEditClassificationDialog.ClassificationRadioGroupKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: SaveButton.OnClick(Self);
    VK_ESCAPE: CancelButton.OnClick(Self);
  end;
  case UpperCase(Chr(Key)) of
    'G': TRadioGroup(Sender).ItemIndex := TRadioGroup(Sender).Items.Indexof('G');
    'P' : TRadioGroup(Sender).ItemIndex := TRadioGroup(Sender).Items.Indexof('PG');
    'R' : TRadioGroup(Sender).ItemIndex := TRadioGroup(Sender).Items.Indexof('R');
    'X' : TRadioGroup(Sender).ItemIndex := TRadioGroup(Sender).Items.Indexof('X');
    '#10': SaveButton.OnClick(Self);
  end;
end;

procedure TEditClassificationDialog.ClassificationRadioGroupSelectionChanged(
  Sender: TObject);
begin
  fRestrictionChanged := True;
end;

procedure TEditClassificationDialog.FormCreate(Sender: TObject);
begin
  fRestrictionChanged := False;
end;

procedure TEditClassificationDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TEditClassificationDialog.FormShow(Sender: TObject);
begin
  ClassificationRadioGroup.SetFocus;
end;

procedure TEditClassificationDialog.SetID(Value: TID);
var
  aClip: TVideoClip;
begin
  if fID <> Value then begin
    fID := Value;
    aClip := TVideoClip.Create(Database, fID);
    try
      ClassificationRadioGroup.ItemIndex := ClassificationRadioGroup.Items.IndexOf(aClip.Restriction);
    finally
      aClip.Free;
    end;
  end;
end;

end.

