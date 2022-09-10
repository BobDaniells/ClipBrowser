unit AdditionalFilterDialogUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, CheckLst, Menus, contnrs, SynCommons, mORMot, modelunit, globalsunit, thumbunit;

type
  TCloseMode = (cmApply, cmCancel, cmClear);

  { TAdditionalFilterDialog }

  TAdditionalFilterDialog = class(TForm)
    ApplyButton: TButton;
    Button2: TButton;
    CategoriesCheckListBox: TCheckListBox;
    KeywordSearchEdit: TEdit;
    Label2: TLabel;
    SuggestedKeywordsCheckGroup: TCheckGroup;
    ClearButton: TButton;
    CategoriesGroupBox: TGroupBox;
    KeywordsGroupBox: TGroupBox;
    BottomPanel: TPanel;
    ButtonPanel: TPanel;
    Label1: TLabel;
    KeywordPopupMenu: TPopupMenu;
    AddKeywordMenuItem: TMenuItem;
    ActiveKeywordsListBox: TListBox;
    SuggestedKeywordsListBox: TListBox;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    AddKeywordsSpeedButton: TSpeedButton;
    RemoveKeywordsSpeedButton: TSpeedButton;
    procedure AddKeywordMenuItemClick(Sender: TObject);
    procedure AddKeywordsSpeedButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure KeywordSearchEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RemoveKeywordsSpeedButtonClick(Sender: TObject);
  private
    fSelectedThumb: TThumb;
    fKeywords: string;
    fCategories: string;
    fCloseMode: TCloseMode;
    procedure RefreshSuggestedKeywords;
  public
    property SelectedThumb: TThumb read fSelectedThumb write fSelectedThumb;
    property Keywords: string read fKeywords write fKeywords;
    property Categories: string read fCategories write fCategories;
    property CloseMode: TCloseMode read fCloseMode;
  end;

var
  AdditionalFilterDialog: TAdditionalFilterDialog;

implementation

{$R *.lfm}

{ TAdditionalFilterDialog }

procedure TAdditionalFilterDialog.FormCreate(Sender: TObject);
var
  List: TObjectList;
  i: Integer;
begin
  List := Database.RetrieveList(TCategory, '', []);
  try
    for i := 0 to List.Count-1 do
      CategoriesCheckListBox.Items.Add(TCategory(List[i]).Category);
  finally
    List.Free;
  end;
end;

procedure TAdditionalFilterDialog.ClearButtonClick(Sender: TObject);
begin
  ActiveKeywordsListBox.Clear;
  fKeywords := '';
  fCloseMode := cmClear;
  Close;
end;

procedure TAdditionalFilterDialog.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  i: Integer;
  SL: TStringList;
begin
  //if no categories selected then select all categories...
  if CategoriesCheckListBox.SelCount = 0 then
    for i := 0 to CategoriesCheckListBox.Items.Count-1 do
      CategoriesCheckListBox.Selected[i] := True;

  fCategories := '';
  SL := TStringList.Create;
  try
    for i := 0 to CategoriesCheckListBox.Count-1 do
      if CategoriesCheckListBox.Checked[i] then
        SL.Add(CategoriesCheckListBox.Items[i]);
  finally
    fCategories := SL.CommaText;
    SL.Free;
  end;
  SL := TStringList.Create;
  try
    for i := 0 to ActiveKeywordsListBox.Items.Count-1 do
      SL.Add(ActiveKeywordsListBox.Items[i]);
    fKeywords := SL.CommaText;
  finally
    SL.Free;
  end;
  CanClose := True;
end;

procedure TAdditionalFilterDialog.ApplyButtonClick(Sender: TObject);
begin
  fCloseMode := cmApply;
  Close;
end;

procedure TAdditionalFilterDialog.Button2Click(Sender: TObject);
begin
  fCloseMode := cmCancel;
  Close;
end;

procedure TAdditionalFilterDialog.AddKeywordMenuItemClick(Sender: TObject);
var
  k: string;
  aKeyword: TSuggestedKeyword;
begin
  k := InputBox('New Keyword', 'Keyword', '');
  if k <> '' then begin
    SuggestedKeywordsCheckGroup.Items.Add(k);
    aKeyword := TSuggestedKeyword.Create;
    aKeyword.SuggestedKeyword := k;
    Database.Add(aKeyword, True);
  end;
end;

procedure TAdditionalFilterDialog.AddKeywordsSpeedButtonClick(Sender: TObject);
var
  i: Integer;
  kw: string;
begin
  for i := 0 to SuggestedKeywordsListBox.Count-1 do
    if SuggestedKeywordsListBox.Selected[i] then begin
      kw := SuggestedKeywordsListBox.Items[i];
      if ActiveKeywordsListBox.Items.IndexOf(kw) < 0 then
        ActiveKeywordsListBox.Items.Add(kw);
    end;
end;

procedure TAdditionalFilterDialog.FormShow(Sender: TObject);
var
  StoredKeywords: RawUTF8;
  i: Integer;
  SL: TStringList;
  SuggestedKeywords: TObjectList;
begin
  SuggestedKeywordsListBox.Sorted := True;
  RefreshSuggestedKeywords;
  KeywordSearchEdit.Clear;
  KeywordSearchEdit.SetFocus;
end;

procedure TAdditionalFilterDialog.RefreshSuggestedKeywords;
var
  SuggestedKeywords: TObjectList;
  i: Integer;
begin
  SuggestedKeywordsListBox.Clear;
  SuggestedKeywords := Database.RetrieveList(TSuggestedKeyword, '', []);
  if Assigned(SuggestedKeywords) then begin
    SuggestedKeywordsListBox.Clear;
    if SuggestedKeywords.Count > 0 then begin
      for i := 0 to SuggestedKeywords.Count-1 do
        SuggestedKeywordsListBox.Items.Add(TSuggestedKeyword(SuggestedKeywords[i]).SuggestedKeyword);
    end;
  end;
end;

procedure TAdditionalFilterDialog.KeywordSearchEditKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
  NewKeyword: TSuggestedKeyword;
begin
  if Key <> VK_RETURN then begin
    for i := 0 to SuggestedKeywordsListBox.Count-1 do begin
      if SuggestedKeywordsListBox.Items[i].StartsWith(KeywordSearchEdit.Text, True) then begin
        SuggestedKeywordsListBox.ItemIndex := i;
        break;
      end else begin
        SuggestedKeywordsListBox.ItemIndex := -1;
      end;
    end;
  end else if SuggestedKeywordsListBox.ItemIndex < 0 then
    if MessageDlg('Not Found', 'Keyword Not Found. Add it?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      NewKeyword := TSuggestedKeyword.Create;
      NewKeyword.SuggestedKeyword := KeywordSearchEdit.Text;
      Database.Add(NewKeyword, True);
      RefreshSuggestedKeywords;
      SuggestedKeywordsListBox.ItemIndex := SuggestedKeywordsListBox.Items.IndexOf(KeywordSearchEdit.Text);
      KeywordSearchEdit.Clear;
    end;
  Key := 0;
end;

procedure TAdditionalFilterDialog.RemoveKeywordsSpeedButtonClick(Sender: TObject
  );
var
  i: Integer;
begin
  for i := 0 to ActiveKeywordsListBox.Items.Count-1 do
    if ActiveKeywordsListBox.Selected[i] then
      ActiveKeywordsListBox.Items.Delete(i);
end;

end.

