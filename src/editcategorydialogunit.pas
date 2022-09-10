unit EditCategoryDialogUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  contnrs, mORMot, modelunit, globalsunit;

type

  { TEditCategoryDialog }

  TEditCategoryDialog = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    ButtonPanel: TPanel;
    CategoryRadioGroup: TRadioGroup;
    ScrollBox1: TScrollBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fID: TID;
  public
    property ID: TID read fID write fID;
  end;

var
  EditCategoryDialog: TEditCategoryDialog;

implementation

{$R *.lfm}

{ TEditCategoryDialog }

procedure TEditCategoryDialog.FormCreate(Sender: TObject);
var
  List: TObjectList;
  i: Integer;
begin
  List := Database.RetrieveList(TCategory, '', []);
  for i := 0 to List.Count-1 do
    CategoryRadioGroup.Items.Add(TCategory(List[i]).Category);
end;

procedure TEditCategoryDialog.FormShow(Sender: TObject);
var
  aClip: TVideoClip;
begin
  aClip := TVideoClip.Create(Database, fID);
  try
    CategoryRadioGroup.ItemIndex := CategoryRadioGroup.Items.IndexOf(aClip.Category);
  finally
    aClip.Free;
  end;
end;

procedure TEditCategoryDialog.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  aClip: TVideoClip;
begin
  aClip := TVideoClip.Create(Database, fID);
  try
    aClip.Category := CategoryRadioGroup.Items[CategoryRadioGroup.ItemIndex];
  finally
    Database.Update(aClip);
    aClip.Free;
    CanClose := True;
  end;
end;

end.

