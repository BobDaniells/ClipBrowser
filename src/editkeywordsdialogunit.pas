unit EditKeywordsDialogunit;

{$mode objfpc}{$H+}

{ Associate various keywords to a video (separate from title) to facilitate
  category-type searches, etc. }

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, StdCtrls,
  StrUtils, contnrs, fphttpclient, opensslsockets, dom, sax_html, dom_html,
  fpJSON, JSONParser, Menus, ExtCtrls, SynCommons, mORMot, globalsunit, modelunit;

type
  { TEditKeywordsDialog }

  TEditKeywordsDialog = class(TForm)
    AddButton: TButton;
    AddYTKeywordsButton: TButton;
    CloseButton: TButton;
    KeywordSearchEdit: TEdit;
    KeywordsListBox: TListBox;
    Panel1: TPanel;
    procedure AddButtonClick(Sender: TObject);
    procedure AddMenuItemClick(Sender: TObject);
    procedure AddYTKeywordsButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure KeywordSearchEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure KeywordsListBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fID: TID;
    fClip: TVideoClip;
    fAllKeywords: TObjectList;
    procedure SetID(Value: TID);
    procedure UpdateClip;
  public
    constructor Create(AOwner: TComponent); override;
    property ID: TID read fID write SetID;
  end;

var
  EditKeywordsDialog: TEditKeywordsDialog;

implementation

uses
  thumbunit;

{$R *.lfm}

procedure TEditKeywordsDialog.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  fClip.Keywords := KeywordsListBox.Items.CommaText;
  Database.Update(fClip);
end;

procedure TEditKeywordsDialog.KeywordSearchEditKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  if Key <> 0 then begin
    for i := 0 to KeywordsListBox.Count-1 do begin
      if KeywordsListBox.Items[i].StartsWith(KeywordSearchEdit.Text, True) then begin
        KeywordsListBox.ItemIndex := i;
        break;
      end else begin
        KeywordsListBox.ItemIndex := -1;
      end;
    end;
  end;
end;

procedure TEditKeywordsDialog.KeywordsListBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  s: string;
begin
  if Key = VK_DELETE then begin
    Key := 0;
    s := TListBox(Sender).Items[TListBox(Sender).ItemIndex];
    if MessageDlg('Delete Keyword', 'Delete ' + s +' (Y/N) ?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        TListBox(Sender).DeleteSelected;
  end;
end;

procedure TEditKeywordsDialog.AddMenuItemClick(Sender: TObject);
var
  s: string;
begin
  s := InputBox('Keyword', 'Enter New Keyword', '');
  if (s <> '') and (not AnsiContainsStr(s, KeywordsListBox.Items.Text)) then
    KeywordsListBox.Items.Add(s);
end;

procedure TEditKeywordsDialog.AddYTKeywordsButtonClick(Sender: TObject);
var
  Client: TFPHttpClient;
  Doc: THTMLDocument;
  att: TDomNode;
  MetaNodes: TDomNodeList;
  attrs: TDOMNamedNodeMap;
  s: string;
  stream: TStringStream;
  i, n, a, s1: Integer;
  SL: TStringList;
  SuggestedList: TObjectList;
  Exists: Boolean;
begin
  // Add YouTube keywords for this clip from its html meta tags to the listbox
  Client := TFPHttpClient.Create(Nil);
  Doc := THTMLDocument.Create;
  try
    s := Client.Get(fClip.ClipURI);
    stream := TStringStream.Create(s);
    SL := TStringList.Create;
    try
      ReadHTMLFile(Doc, stream);
      MetaNodes := Doc.GetElementsByTagName('meta');
      for n := 0 to MetaNodes.Count-1 do begin
        attrs := MetaNodes[n].Attributes;
        for a := 0 to attrs.Length-1 do begin
          att := attrs[a];
          if (att.NodeName = 'name') and (att.NodeValue = 'keywords') then begin
            SL.CommaText := AnsiString(attrs.GetNamedItem('content').TextContent);
            break;
          end;
        end;
      end;
      for i := 0 to SL.Count-1 do begin
        if KeyWordsListBox.Items.IndexOf(SL[i]) < 0 then
          KeyWordsListBox.Items.Add(SL[i]);
      end;
    finally
      SL.Free;
      stream.Free;
    end;
  finally
    Doc.Free;
    Client.Free;
  end;
end;

procedure TEditKeywordsDialog.AddButtonClick(Sender: TObject);
var
  s: string;
  Suggested : TSuggestedKeyword;
begin
  s := InputBox('Keyword', 'Enter New Keyword', '');
  if (s <> '') and (not AnsiContainsStr(s, KeywordsListBox.Items.Text)) then begin
    KeywordsListBox.Items.Add(s);
    // Add to keyword to suggested keywords for later use it does not already exist...
    Suggested := TSuggestedKeyword.CreateAndFillPrepare(Database, 'SuggestedKeyword = ?', [s]);
    try
      if not Suggested.FillOne then begin
        Suggested := TSuggestedKeyword.Create;
        Suggested.SuggestedKeyword := s;
        Database.Add(Suggested, True);
        KeywordsListBox.ItemIndex := KeywordsListBox.Items.IndexOf(s);
      end;
    finally
      if Assigned(Suggested) then
        Suggested.Free;
    end;
  end;
end;

procedure TEditKeywordsDialog.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TEditKeywordsDialog.UpdateClip;
begin
  fClip.Keywords := KeywordsListBox.Items.CommaText;
  Database.Update(fClip);
end;

constructor TEditKeywordsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  KeywordsListBox.Clear;
  AddButton.Enabled := True;
  AddYTKeywordsButton.Enabled := False;
end;

procedure TEditKeywordsDialog.SetID(Value: TID);
begin
  KeywordsListBox.Clear;
  if fID <> Value then
    fID := Value;
  fClip := TVideoClip.Create(Database, fID);
  KeywordsListBox.Items.CommaText := fClip.Keywords;
  Caption := Caption + ' - ' + fClip.Title;
  AddYTKeywordsButton.Enabled := IsFileURI(fClip.ClipURI);
  KeywordSearchEdit.Clear;
end;

end.

