unit clipparametersunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, LCLtype, StrUtils, RegExpr, dom, sax_html, dom_html, fphttpclient,
  fpJSON, JSONParser, BCLabel, mORMot, SynCommons, contnrs, modelunit,
  globalsunit;

type

  { TParametersDialog }

  TParametersDialog = class(TForm)
    AddButton: TButton;
    AddYTKeywordsButton: TButton;
    AttributionMemo: TMemo;
    BCLabelFive: TBCLabel;
    BCLabelFour: TBCLabel;
    BCLabelOne: TBCLabel;
    BCLabelThree: TBCLabel;
    BCLabelTwo: TBCLabel;
    BCLabelZero: TBCLabel;
    CancelButton: TButton;
    CategoryRadioGroup: TRadioGroup;
    ClassificationRadioGroup: TRadioGroup;
    CloseButton: TButton;
    DescriptionMemo: TMemo;
    KeywordSearchEdit: TEdit;
    KeywordsListBox: TListBox;
    Label1: TLabel;
    RatingLabel: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    CategoryTabSheet: TTabSheet;
    ParentalControlTabSheet: TTabSheet;
    DescriptionTabSheet: TTabSheet;
    AttributionTabSheet: TTabSheet;
    RatingTabSheet: TTabSheet;
    KeywordsTabSheet: TTabSheet;
    MetricsTabSheet: TTabSheet;
    RatingTrackBar: TTrackBar;
    VideoDetailsMemo: TMemo;
    procedure AddButtonClick(Sender: TObject);
    procedure AddYTKeywordsButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CategoryTabSheetShow(Sender: TObject);
    procedure ClassificationRadioGroupKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CloseButtonClick(Sender: TObject);
    procedure DescriptionTabSheetShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure KeywordSearchEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure KeywordsListBoxKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ParentalControlTabSheetShow(Sender: TObject);
    procedure RatingTrackBarChange(Sender: TObject);
  private
    fID: TID;
    fClip: TVideoClip;
    fSaveFlag: Boolean;
    fCategory: RawUTF8;
    fRestriction: RawUTF8;
    fDescription: RawUTF8;
    fAttribution: RawUTF8;
    fRating: RawUTF8;
    fKeywords: RawUTF8;
    procedure SetID(Value: TID);
    function ParseYTDuration(YTDuration: string): string;
    function Secs2HMS(Seconds: string): string;
  public
    property ID: TID read fID write SetID;
  end;

var
  ParametersDialog: TParametersDialog;

implementation

{$R *.lfm}

{ TParametersDialog }

procedure TParametersDialog.FormCreate(Sender: TObject);
var
  CategoryList: TObjectList;
  i: Integer;
begin
  fSaveFlag := False;
  CategoryList := Database.RetrieveList(TCategory, '', []);
  for i := 0 to CategoryList.Count-1 do
    CategoryRadioGroup.Items.Add(TCategory(CategoryList[i]).Category);
  PageControl1.ActivePage := CategoryTabSheet;
end;

procedure TParametersDialog.KeywordSearchEditKeyUp(Sender: TObject;
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

procedure TParametersDialog.KeywordsListBoxKeyUp(Sender: TObject;
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

procedure TParametersDialog.SetID(Value: TID);
var
  Props: TVideoProps;
begin
  if fID <> Value then begin
    fID := Value;
    fClip := TVideoClip.Create(Database, fID);
    fCategory := fClip.Category;
    fRestriction := fClip.Restriction;
    fDescription := fClip.Description;
    fAttribution := fClip.Attribution;
    fRating := fClip.Rating;
    RatingTrackBar.Position := Round(StrToFloat(fRating) * 100);
    RatingLabel.Caption := fClip.Rating;
    fKeywords := fClip.Keywords;
    KeywordsListBox.Items.CommaText := fKeywords;
    fClip.GetVideoParameters(fClip.ClipURI, Props);
    fClip.Width := Props.Width;
    fClip.Height := Props.Height;
    fClip.Duration := Props.Duration;
    fClip.PublishedDate := Props.PublishedDate;
    Database.Update(fClip);

    with VideoDetailsMemo do begin
      Clear;
      if fClip.Description <> '' then Lines.Add('Description: ' + fClip.Description);
      Lines.Add('');
      if fClip.Width <> '' then Lines.Add('Width: ' + fClip.Width);
      if fClip.Height <> '' then Lines.Add('Height: ' + fClip.Height);
      Lines.Add('');
      if fClip.Duration <> '' then
        if AnsiContainsStr(fClip.Duration, 'PT') then // YouTube format
          Lines.Add('Playing Time: ' + ParseYTDuration(fClip.Duration))
        else
          Lines.Add('Playing Time (Seconds) = ' + Secs2HMS(fClip.Duration));
      Lines.Add('');
      if fClip.PublishedDate <> '' then Lines.Add('Published Date: ' + fClip.PublishedDate);
      Lines.Add('');
      if fClip.Youtube_Keywords <> '' then Lines.Add('Youtube_Keywords: ' + fClip.Youtube_Keywords);
    end;
  end;
end;

function TParametersDialog.ParseYTDuration(YTDuration: string): string;
var
  re: TRegExpr;
  SL: TStringList;
  i: Integer;
begin
  re := TRegExpr.Create;
  SL := TStringList.Create;
  try
    re.Expression:='\D+';
    re.Split(YTDuration, SL);
    for i := 0 to SL.Count-1 do
      if SL[i] = '' then
        SL[i] := '0';
    Result := SL[0] + ' Hours ' + SL[1] + ' Minutes ' + SL[2] + ' Seconds.';
  finally
    SL.Free;
    re.Free;
  end;
end;

function TParametersDialog.Secs2HMS(Seconds: string): string;
var
  h, m, s, si, rem: Integer;
begin
  Result := '';
  if Seconds <> '' then
    si := Round(StrToFloat(Seconds));
    h := si div 3600;
    rem := si mod 3600;
    m := rem div 60;
    s := rem mod 60;
  Result := IntToStr(h) + ' Hours ' + IntToStr(m) + ' Minutes ' + IntToStr(s) + ' Seconds.';
end;

procedure TParametersDialog.ParentalControlTabSheetShow(Sender: TObject);
begin
  ClassificationRadioGroup.SetFocus;
  ClassificationRadioGroup.ItemIndex := ClassificationRadioGroup.Items.IndexOf(fRestriction);
end;

procedure TParametersDialog.RatingTrackBarChange(Sender: TObject);
begin
  fRating := FloatToStr(RatingTrackBar.Position / 100);
  RatingLabel.Caption := fRating;
end;

procedure TParametersDialog.CategoryTabSheetShow(Sender: TObject);
begin
  CategoryRadioGroup.ItemIndex := CategoryRadioGroup.Items.IndexOf(fCategory);
end;

procedure TParametersDialog.ClassificationRadioGroupKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: CloseButton.OnClick(Self);
    VK_ESCAPE: CancelButton.OnClick(Self);
  end;
  case UpperCase(Chr(Key)) of
    'G': TRadioGroup(Sender).ItemIndex := TRadioGroup(Sender).Items.Indexof('G');
    'P' : TRadioGroup(Sender).ItemIndex := TRadioGroup(Sender).Items.Indexof('PG');
    'R' : TRadioGroup(Sender).ItemIndex := TRadioGroup(Sender).Items.Indexof('R');
    'X' : TRadioGroup(Sender).ItemIndex := TRadioGroup(Sender).Items.Indexof('X');
    '#10': CloseButton.OnClick(Self);
  end;
end;

procedure TParametersDialog.CancelButtonClick(Sender: TObject);
begin
  fSaveFlag := False;
  Close;
end;

procedure TParametersDialog.AddButtonClick(Sender: TObject);
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

procedure TParametersDialog.AddYTKeywordsButtonClick(Sender: TObject);
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

procedure TParametersDialog.CloseButtonClick(Sender: TObject);
begin
  fSaveFlag := True;
  Close;
end;

procedure TParametersDialog.DescriptionTabSheetShow(Sender: TObject);
begin
  DescriptionMemo.SetFocus;
  Descriptionmemo.Text := fClip.Description;
end;

procedure TParametersDialog.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if fSaveFlag then begin
    fClip.Category := fCategory;
    fClip.Restriction := fRestriction;
    fDescription := DescriptionMemo.Text;
    fClip.Description := fDescription;
    fAttribution := AttributionMemo.Text;
    fClip.Attribution := fAttribution;
    fRating := FloatToStr(RatingTrackBar.Position / 100);
    fClip.Rating := fRating;
    fClip.Keywords := KeywordsListBox.Items.CommaText;
    Database.Update(fClip);
    CanClose := True;
  end;
  if Assigned(fClip) then
    fClip.Free;
end;

end.

