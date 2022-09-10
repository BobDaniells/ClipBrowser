unit FilterDialogUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  contnrs, app_globals, app_model;

type
  TFilterType = (ftTitle, ftKeywords);

  { TFilterDialog }

  TFilterDialog = class(TForm)
    BottomPanel: TPanel;
    Button1: TButton;
    Button2: TButton;
    ClearButton: TButton;
    GoButton: TButton;
    CancelButton: TButton;
    ButtonPanel: TPanel;
    KeywordsPanel: TPanel;
    FilterListBox: TListBox;
    ArrowPanel: TPanel;
    AvailableWordsListBox: TListBox;
    Panel1: TPanel;
    WordListMemo: TMemo;
    Splitter1: TSplitter;
    WordListMemo1: TMemo;
    WordListPanel: TPanel;
    procedure AvailableWordsListBoxDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure AvailableWordsListBoxDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure FilterListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FilterListBoxDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  FilterDialog: TFilterDialog;

implementation

procedure TFilterDialog.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to AvailableWordsListBox.Items.Count-1 do
    if AvailableWordsListBox.Selected[i] then
      if FilterListBox.Items.IndexOf(AvailableWordsListBox.Items[i]) < 0 then
        FilterListBox.Items.Add(AvailableWordsListBox.Items[i]);
end;

procedure TFilterDialog.AvailableWordsListBoxDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TFilterDialog.AvailableWordsListBoxDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  TListBox(Source).DeleteSelected;
end;

procedure TFilterDialog.Button2Click(Sender: TObject);
begin
  FilterListBox.DeleteSelected;
end;

procedure TFilterDialog.ClearButtonClick(Sender: TObject);
begin
  FilterListBox.Clear;
end;

procedure TFilterDialog.FilterListBoxDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  i: Integer;
  Value: string;
begin
  for i := 0 to TListBox(Source).Count-1 do begin
    Value := TListBox(Source).Items[i];
    if TListBox(Source).Selected[i] then
      if (TListBox(Sender).Items.IndexOf(Value) < 0) then // No Duplicates
        FilterListBox.Items.Add(TListBox(Source).Items[i]);
  end;
end;

procedure TFilterDialog.FilterListBoxDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

constructor TFilterDialog.Create(AOwner: TComponent);
var
  Keywords: TObjectList;
  i: Integer;
begin
  inherited Create(AOwner);
  Keywords := Database.RetrieveList(TKeyword, '', []);
  AvailableWordsListBox.MultiSelect := True;
  AvailableWordsListBox.Sorted := True;
  AvailableWordsListBox.DragMode := dmAutomatic;
  FilterListBox.MultiSelect := True;
  FilterListBox.Sorted := True;
  FilterListBox.DragMode := dmAutomatic;
  for i := 0 to Keywords.Count-1 do
    AvailableWordsListBox.Items.Add(TKeyword(Keywords[i]).Keyword);
end;

{$R *.lfm}

end.

