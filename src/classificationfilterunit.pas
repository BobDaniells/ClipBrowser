unit ClassificationFilterUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  UITypes, LCLType;

type

  { TClassificationsFilterDialog }

  TClassificationsFilterDialog = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    CheckGroup1: TCheckGroup;
    Panel1: TPanel;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure CheckGroup1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    fAllowedClassifications: string;
  public
    property AllowedClassifications: string read fAllowedClassifications write fAllowedClassifications;
  end;

var
  ClassificationsFilterDialog: TClassificationsFilterDialog;

implementation

{$R *.lfm}

{ TClassificationsFilterDialog }

procedure TClassificationsFilterDialog.CheckGroup1ItemClick(Sender: TObject;
  Index: integer);
var
  i: Integer;
  SL: TStringList;
begin
  fAllowedClassifications := '';
  SL := TStringList.Create;
  try
    if Index = CheckGroup1.Items.IndexOf('All') then
      for i := 0 to CheckGroup1.Items.Count-1 do
        if CheckGroup1.Checked[Index] then
           CheckGroup1.Checked[i] := True
        else
           CheckGroup1.Checked[i] := False;
    for i := 0 to CheckGroup1.Items.Count-1 do
      if CheckGroup1.Checked[i] then SL.Add(CheckGroup1.Items[i]);
    if SL.IndexOf('All') > -1 then
      SL.Delete(SL.IndexOf('All'));
    fAllowedClassifications := SL.CommaText;
  finally
    SL.Free;
  end;
end;

procedure TClassificationsFilterDialog.CheckGroup1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: ApplyButton.OnClick(Self);
    VK_ESCAPE: CancelButton.OnClick(Self);
  end;
  case UpperCase(Chr(Key)) of
    'G': TCheckGroup(Sender).Checked[TCheckGroup(Sender).Items.IndexOf('G')] := True;
    'P': TCheckGroup(Sender).Checked[TCheckGroup(Sender).Items.IndexOf('PG')] := True;
    'R': TCheckGroup(Sender).Checked[TCheckGroup(Sender).Items.IndexOf('R')] := True;
    'X': TCheckGroup(Sender).Checked[TCheckGroup(Sender).Items.IndexOf('X')] := True;
    'A': TCheckGroup(Sender).Checked[TCheckGroup(Sender).Items.IndexOf('All')] := True;
  end;
  Key := 0;
end;

procedure TClassificationsFilterDialog.ApplyButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
  Close;
end;

procedure TClassificationsFilterDialog.CancelButtonClick(Sender: TObject);
begin
  modalResult := mrCancel;
  Close;
end;

procedure TClassificationsFilterDialog.FormShow(Sender: TObject);
begin
  CheckGroup1.SetFocus;
end;

end.

