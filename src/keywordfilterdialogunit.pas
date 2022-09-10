unit KeywordFilterDialogUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, contnrs,
  app_globals, app_model;

type

  { TKeywordFilterDialog }

  TKeywordFilterDialog = class(TForm)
    SelectButton1: TButton;
    ClearButton: TButton;
    FilterListBox: TListBox;
    procedure ClearButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SelectButton1Click(Sender: TObject);
  private
  public
  end;

var
  KeywordFilterDialog: TKeywordFilterDialog;

implementation

{$R *.lfm}

{ TKeywordFilterDialog }

function CompareKeyWords(Item1, Item2: Pointer): Integer;
var
  Keyword1, Keyword2: TKeyword;
begin
  Keyword1 := TKeyword(Item1);
  Keyword2 := TKeyword(Item2);
  if Keyword1.Keyword > Keyword2.Keyword
  then Result := 1
  else if Keyword1.Keyword = Keyword2.Keyword
  then Result := 0
  else Result := -1;
end;

procedure TKeywordFilterDialog.FormCreate(Sender: TObject);
var
  List: TObjectList;
  i: Integer;
  s: string;
begin
  List := TObjectList.Create(False);
  try
    List := Database.RetrieveList(TKeyword, '', []);
    List.Sort(@CompareKeyWords);
    for i := 0 to List.Count-1 do begin
      s :=  TKeyword(List[i]).Keyword;
      FilterListBox.Items.Add(s);
    end;
  finally
    List.Free;
  end;
end;

procedure TKeywordFilterDialog.SelectButton1Click(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

procedure TKeywordFilterDialog.ClearButtonClick(Sender: TObject);
begin
  FilterListBox.Clear;
  Close
end;

end.

