unit ThumbViewUnit;

{$mode objfpc}{$H+}

{ TThumbView is a component that displays the video thumbnail matrix.
  It represents the Controller element of the Model-View-Controller structure.}

interface

uses
  Classes, SysUtils, Forms, Graphics, Controls, ComCtrls, Menus, StdCtrls, Dialogs, contnrs,
  StrUtils, ExtCtrls, LCLType, LCLIntf, LMessages, SynCommons, mORMot, BGRABitmap,
  BGRABitmapTypes, globalsunit, thumbunit, ModelUnit, ThumbFailDialogUnit;

type
  TScrollDirection = (sdDown, sdUp);

  TScrollExEvent = procedure(Sender: TObject; var ScrollPos: Integer) of object;

  TThumbView = class(TFlowPanel)
  private
    fThumbs: TThumbs;
    fThumbWidth: Integer;
    fThumbHeight: Integer;
    fColumnCount: Integer;
    fRowCount: Integer;
    fVisibleThumbCount: Integer;
    fPreviousVisibleThumbCount: Integer;
    fVisibleColumns: Integer;
    fVisibleRows: Integer;
    fShowHowToMessage: Boolean;
    fSelectedColumn: Integer;
    fSelectedRow: Integer;
    fSelectedThumb: TThumb;
    fRowNum: Integer;
    fClipList: TObjectList;
    fMinClipID: RawUTF8;
    fMaxClipID: RawUTF8;
    fDBRowID: Integer;
    fTitleFilter: string;
    fClassificationFilter: string;
    fCategoryFilter: string;
    fKeywordFilter: string;
    fThumbFailList: TStringList;
    fPathErrors: TStringList;
    fRestriction: RawUTF8;
    FOnVScroll: TScrollExEvent;
    FOnHScroll: TScrollExEvent;
    fOnSelectThumb: TNotifyEvent;
    fPlayerApp: string;
    procedure SetThumbWidth(Value: Integer);
    procedure SetThumbHeight(Value: Integer);
    procedure SetShowHowToMessage(Value: Boolean);
    function CreateThumbForClip(aClip: TVideoClip): TThumb;
    procedure GetClassificationsFilter(var where: RawUTF8);
    procedure GetCategoryFilter(var where: RawUTF8);
    procedure GetTitleFilter(var where: RawUTF8);
    procedure GetKeywordFilter(var where: RawUTF8);
    procedure SetTitleFilter(Value: string);
    procedure SetClassificationFilter(Value: string);
    procedure SetCategoryFilter(Value: string);
    procedure SetKeywordFilter(Value: string);
    procedure ClipClicked(Sender: TObject);
    procedure ClipDblClicked(Sender: TObject);
    procedure SetPreferredPlayerApp;
    procedure PlayVideo(Thumb: TThumb);
  protected
    procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    procedure DoHScroll(var aScrollPos: integer);
    procedure DoVScroll(var aScrollPos: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetClipTableIDLimits;
    procedure FlowResize(Sender: TObject);
    function LoadPageOfThumbs: Integer;// Return count
    procedure ClearThumbNails;
    procedure Purge;
    function NextThumb: Integer;
    function PreviousThumb: Integer;
    function NextRow: Integer;
    function PreviousRow: Integer;
    procedure ScrollPage(Direction: TScrollDirection);
    function AddThumb(Path: string; var Thumb: TThumb): string;
    procedure RemoveThumb(Thumb: TThumb);
    function RemoveMarkedThumbs: Boolean;
    property Thumbs: TThumbs read fThumbs write fThumbs;
    property ThumbWidth: Integer read fThumbWidth write SetThumbWidth;
    property ThumbHeight: Integer read fThumbHeight write SetThumbHeight;
    property ColumnCount: Integer read fColumnCount write fColumnCount;
    property RowCount: Integer read fRowCount write fRowCount;
    property MinClipID: RawUTF8 read fMinClipID write fMinClipID;
    property MaxClipID: RawUTF8 read fMaxClipID write fMaxClipID;
    property VisibleThumbCount: Integer read fVisibleThumbCount write fVisibleThumbCount;
    property VisibleColumns: Integer read fVisibleColumns write fVisibleColumns;
    property VisibleRows: Integer read fVisibleRows write fVisibleRows;
    property SelectedThumb: TThumb read fSelectedThumb write fSelectedThumb;
    property SelectedRow: Integer read fSelectedRow write fSelectedRow;
    property SelectedColumn: Integer read fSelectedColumn;
    property ShowHowToMessage: Boolean read fShowHowToMessage write SetShowHowToMessage;
    property RowNum: Integer read fRowNum write fRowNum;
    property TitleFilter: string read fTitleFilter write SetTitleFilter;
    property ClassificationFilter: string read fClassificationFilter write SetClassificationFilter;
    property CategoryFilter: string read fCategoryFilter write SetCategoryFilter;
    property KeywordFilter: string read fKeywordFilter write SetKeywordFilter;
    property OnKeyDown;
    property OnKeyUp;
    property OnClick;
    property OnVScroll: TScrollExEvent read FOnVScroll write FOnVScroll;
    property OnHScroll: TScrollExEvent read FOnHScroll write FOnHScroll;
    property OnSelectThumb: TNotifyEvent read fOnSelectThumb write fOnSelectThumb;
  end;

implementation

constructor TThumbView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fClipList := TObjectList.Create(true);
  fRestriction := DefaultRestriction;
  OnResize := @FlowResize;
  fPathErrors := TStringList.Create;
  fThumbFailList := TStringList.Create;
  fPreviousVisibleThumbCount := 0;
end;

destructor TThumbView.Destroy;
begin
  if Assigned(fClipList) then
    fClipList.Free;
  if Assigned(fThumbFailList) then
    fThumbFailList.Free;
  if Assigned(fPathErrors) then
    fPathErrors.Free;
  inherited Destroy;
end;

procedure TThumbView.SetClipTableIDLimits;
begin
  fMinClipID := Database.OneFieldValue(TVideoClip, 'MIN(ID)', '');
  fMaxClipID := Database.OneFieldValue(TVideoClip, 'MAX(ID)', '');
end;

procedure TThumbView.DoHScroll(var aScrollPos: integer);
begin
  if Assigned(FOnHScroll) then
    FOnHScroll(Self, aScrollPos);
end;

procedure TThumbView.DoVScroll(var aScrollPos: integer);
begin
  if Assigned(FOnVScroll) then
    FOnVScroll(Self, aScrollPos);
end;

procedure TThumbView.WMHScroll(var Message: TLMHScroll);
begin
  DoHScroll(Message.Pos);
end;

procedure TThumbView.WMVScroll(var Message: TLMVScroll);
begin
  DoVScroll(Message.Pos);
end;

procedure TThumbView.FlowResize(Sender: TObject);
begin
  if fThumbWidth > 0 then
    fColumnCount := Width div fThumbWidth;
  if fThumbHeight > 0 then
    fRowCount := Self.Height div fThumbHeight;
  fVisibleThumbCount := fColumnCount * (fRowCount*1);
  if (fVisibleThumbCount <> fPreviousVisibleThumbCount) and
    (fPreviousVisibleThumbCount > 0) then
    LoadPageOfThumbs;
  fPreviousVisibleThumbCount := fVisibleThumbCount;
end;

procedure TThumbView.SetThumbWidth(Value: Integer);
begin
  if fThumbWidth <> Value then begin
    fThumbWidth := Value;
    fVisibleColumns := Width div fThumbWidth;
  end;
end;

procedure TThumbView.SetThumbHeight(Value: Integer);
begin
  if fThumbHeight <> Value then begin
    fThumbHeight := Value;
    fVisibleRows := Height div fThumbHeight;
  end;
end;

procedure TThumbView.SetShowHowToMessage(Value: Boolean);
begin
  if fShowHowToMessage <> Value then begin
    fShowHowToMessage := Value;
    if fShowHowToMessage then
      Caption := 'Drag and Drop Video File(s) here to add them to the library...'
    else
      Caption := '';
  end;
end;

procedure TThumbView.ScrollPage(Direction: TScrollDirection);
begin
  case Direction of
    sdDown: begin
      Inc(fRowNum, 1);
      LoadPageOfThumbs;
    end;
    sdUp: begin
      if fRowNum > 0 then begin
        Dec(fRowNum, 1);
        LoadPageOfThumbs;
      end;
    end;
  end;
  if FThumbs.Count > 0 then begin;
    fThumbs.SelectThumb(fThumbs[0]); //Update selected title
    fThumbs.SelectedThumb := fThumbs[0];
  end;
end;

function TThumbView.NextThumb: Integer;
var
  i: Integer;
begin
  i := Thumbs.SelectedIndex;
  if i >= 0 then begin
    Result := i + 1;
    if Result >= fThumbs.Count then begin
      ScrollPage(sdDown);
    end;
    if Result < fThumbs.Count then begin
      Thumbs.SelectedIndex := Result;
      with Thumbs do begin
        Items[i].IsSelected := False;
        Items[i].RedrawBitmap;
        Items[Result].IsSelected := True;
        Items[Result].RedrawBitmap;
      end;
    end;
  end;
end;

function TThumbView.PreviousThumb: Integer;
var
  i, Rows, LastPageBase: Integer;
begin
  Result := -1;
  i := Thumbs.SelectedIndex;
  if (i > 0) then begin
    Rows := Thumbs.Count div ColumnCount;
    LastPageBase := Rows * ColumnCount - (ColumnCount * RowCount);
    if (i = LastPageBase) or ((i < LastPageBase) and (i mod ColumnCount = 0)) then
      ScrollPage(sdUp);
    Result := i - 1;
    Thumbs.SelectedIndex := Result;
    with Thumbs do begin
      Items[i].IsSelected := False;
      Items[i].RedrawBitmap;
      Items[Result].IsSelected := True;
      Items[Result].RedrawBitmap;
    end;
  end;
end;

function TThumbView.NextRow: Integer;
var
  i, Rows, LastPageBase: Integer;
begin   i := Thumbs.SelectedIndex;
  Result := i + ColumnCount;
  Rows := Thumbs.Count div ColumnCount;
  LastPageBase := Rows * ColumnCount - (ColumnCount * RowCount);
  if (Result >= fThumbs.Count) then begin
    ScrollPage(sdDown);
  end else if (Result < LastPageBase) then
    ScrollPage(sdDown);
  if Result < Thumbs.Count-1 then begin
    with Thumbs do begin
      Thumbs.SelectedIndex := Result;
      Items[i].IsSelected := False;
      Items[i].RedrawBitmap;
      Items[Result].IsSelected := True;
      Items[Result].RedrawBitmap;
    end;
  end;
end;

function TThumbView.PreviousRow: Integer;
var
  i, Rows, LastPageBase: Integer;
begin
  i := Thumbs.SelectedIndex;
  if i >= 0 then begin
    Rows := Thumbs.Count div ColumnCount;
    LastPageBase := Rows * ColumnCount - (ColumnCount * RowCount);
    Result := i - ColumnCount;
    if (Result < LastPageBase) then
      ScrollPage(sdUp);
    if Result >= 0 then begin
      Thumbs.SelectedIndex := Result;
      with Thumbs do begin
        Thumbs.SelectedIndex := Result;
        Items[i].IsSelected := False;
        Items[i].RedrawBitmap;
        Items[Result].IsSelected := True;
        Items[Result].RedrawBitmap;
      end;
    end;
  end;
end;

procedure TThumbView.SetTitleFilter(Value: string);
begin
  if fTitleFilter <> Value then begin
    fTitleFilter := Value;
    fRowNum := 0;
    LoadPageOfThumbs;
  end;
end;

procedure TThumbView.SetClassificationFilter(Value: string);
begin
  if fClassificationFilter <> Value then begin
    fClassificationFilter := Value;
    fRowNum := 0;
    LoadPageOfThumbs;
  end;
end;

procedure TThumbView.SetCategoryFilter(Value: string);
begin
  if fCategoryFilter <> Value then begin
    fCategoryFilter := Value;
    fRowNum := 0;
    LoadPageOfThumbs;
  end;
end;

procedure TThumbView.SetKeywordFilter(Value: string);
begin
  if fKeywordFilter <> Value then begin
    fKeywordFilter := Value;
    fRowNum := 0;
    LoadPageOfThumbs;
  end;
end;

procedure TThumbView.GetClassificationsFilter(var where: RawUTF8);
var
  i: Integer;
  s: TStringArray;
  AdditionalWhere: RawUTF8;
begin
  AdditionalWhere := '';
  if fClassificationFilter <> '' then begin
    s := fClassificationFilter.Split([','], Length(fClassificationFilter));
    if Length(s) = 1 then
      AdditionalWhere := FormatUTF8('Restriction = ?', [where], [s[0]])
    else begin
      for i := 0 to Length(s)-1 do begin
        if i = 0 then
          AdditionalWhere := FormatUTF8('(Restriction = ?', [where], [s[i]])
        else if i = Length(s)-1 then
          AdditionalWhere := AdditionalWhere + FormatUTF8('% OR Restriction = ?)', [where], [s[i]])
        else
          AdditionalWhere := AdditionalWhere + FormatUTF8('% OR Restriction = ?', [where], [s[i]]);
      end;
    end;
  end;
  if AdditionalWhere <> '' then
    where := AdditionalWhere;
end;

procedure TThumbView.GetCategoryFilter(var where: RawUTF8);
var
  i: Integer;
  s: TStringArray;
  AdditionalWhere: RawUTF8;
begin
  AdditionalWhere := '';
  if fCategoryFilter <> '' then begin
    s := fCategoryFilter.Split([','], Length(fCategoryFilter));
    if Length(s) = 1 then
      AdditionalWhere := FormatUTF8('Category = ?', [where], [s[0]])
    else begin
      for i := 0 to Length(s)-1 do begin
        if i = 0 then
          AdditionalWhere := FormatUTF8('(Category = ?', [where], [s[i]])
        else if i = Length(s)-1 then
          AdditionalWhere := FormatUTF8('% OR Category = ?)', [where], [s[i]])
        else
          AdditionalWhere := FormatUTF8('% OR Category = ?', [where], [s[i]]);
      end;
    end;
  end;
  if AdditionalWhere <> '' then
    where := where + ' AND ' + AdditionalWhere;
end;

procedure TThumbView.GetTitleFilter(var where: RawUTF8);
var
  SL: TStringList;
  i: Integer;
  AdditionalWhere: RawUTF8;
begin
  SL := TStringList.Create;
  try
    SL.CommaText := fTitleFilter;
    for i := 0 to SL.Count-1 do
      AdditionalWhere := FormatUTF8('% AND Title LIKE ?', [where], ['%'+SL[i]+'%']);
  finally
    if AdditionalWhere <> '' then
      where := AdditionalWhere;
    SL.Free;
  end;
end;

procedure TThumbView.GetKeywordFilter(var where: RawUTF8);
var
  i: Integer;
  SL: TStringList;
  AdditionalWhere: RawUTF8;
begin
  SL := TStringList.Create;
  try
    SL.CommaText := fKeywordFilter;
    for i := 0 to SL.Count-1 do
      AdditionalWhere := FormatUTF8('% AND Keywords LIKE ?', [where], ['%'+SL[i]+'%']);
  finally
    if AdditionalWhere <> '' then
      where := AdditionalWhere;
    SL.Free;
  end;
end;

function TThumbView.LoadPageOfThumbs: Integer; // Return count
var
  Thumb: TThumb;
  aClip: TVideoClip;
  Dlg: TThumbFailDialog;
  Where: RawUTF8;
  MinRowID: Integer;
begin
  fPathErrors.Clear;
  fThumbs.Clear;
  if fVisibleThumbCount > 0 then begin
    if fMinClipID = '' then
      fMinClipID := '0';
    MinRowID := StrToInt(fMinClipID) * (fRowNum * fColumnCount);
    where := '';
    // Filter records on parental restrictions...
    GetClassificationsFilter(where);
    // Filter records on category...
    GetCategoryFilter(where);
    // Filter records on title keywords...
    GetTitleFilter(where);
    // Filter records on user-assigned keywords...
    GetKeywordFilter(where);
    // Retrieve only one page of thumbs at a time...
    where := where + ' LIMIT ' + IntToStr(fVisibleThumbCount) + ' OFFSET ' + IntToStr(MinRowID * fRowCount);

    // Fetch filtered data
    aClip := TVideoClip.CreateAndFillPrepare(Database, where);
    try
      while aClip.FillOne do begin // Populate the clip
        Thumb := CreateThumbForClip(aClip);
        fThumbs.Add(Thumb);
        fDBRowID := aClip.ID;
      end;
    finally
      if Assigned(aClip) then
        aClip.Free;
    end;
    // Keep track of any video files that cannot be retrieved...
    if fPathErrors.Count > 0 then begin
      Dlg := TThumbFailDialog.Create(Nil);
      try
        Dlg.ErrorList := fPathErrors;
        Dlg.ShowModal;
      finally
        Dlg.Free;
      end;
    end;
  end;
  if fThumbs.Count > 0 then begin
    fThumbs[0].IsSelected := True;
    fThumbs.SelectedThumb := fThumbs[0];
  end;
  Result := Thumbs.Count;
end;

procedure TThumbView.ClearThumbnails;
begin
  fThumbs.Clear;
end;

procedure TThumbView.Purge;
begin
  try
    ClearThumbNails; // Clear displayed thumbnails
    Database.Execute('DELETE FROM "VideoDoc"'); // Delete any linked documents
    Database.Execute('DELETE FROM "VideoClip"'); // Delete all video clips
    Database.Execute('DELETE FROM SQLITE_SEQUENCE WHERE NAME = "VideoDoc"'); // Reset auto-increment of IDs
    Database.Execute('DELETE FROM SQLITE_SEQUENCE WHERE NAME = "VideoClip"'); // Reset auto-increment of IDs
  finally
    SetShowHowtoMessage(True);
  end;
end;

function TThumbView.CreateThumbForClip(aClip: TVideoClip): TThumb;
begin
  Result := TThumb.Create(Nil);
  with Result do begin
    Clip := aClip;
    ThumbSize := ts256;
    RecID := Clip.ID;
    ClipURI := Clip.ClipURI; // Load a thumbnail if it already exists else create one
 //   ClipURI := Clip.ClipURI;
    Category := Clip.Category;
    OnClick := @ClipClicked;
    OnDblClick := @ClipDblClicked;
    Description := Clip.Description;
    IsSelected := False;
    MarkedForRemoval := False;
    Parent := Self;
    if PathError <> '' then
      fPathErrors.add(PathError);
  end;
end;

procedure TThumbView.ClipClicked(Sender: TObject);
begin
  fThumbs.SelectThumb(TThumb(Sender));
  fThumbs.SelectedThumb := TThumb(Sender);
end;

procedure TThumbView.ClipDblClicked(Sender: TObject);
begin
  fThumbs.SelectThumb(TThumb(Sender));
  fThumbs.SelectedThumb := TThumb(Sender);
  SetPreferredPlayerApp;
  PlayVideo(TThumb(fThumbs.SelectedThumb));
end;

procedure TThumbView.SetPreferredPlayerApp;
var
  List: TObjectList;
begin
  List := Database.RetrieveList(TVideoPlayer, 'DefaultPlayer', [True]);
  if List.Count > 0 then
    fPlayerApp := TVideoPlayer(List[0]).AppName;
end;

procedure TThumbView.PlayVideo(Thumb: TThumb);
begin
  OnClick := Nil; // Avoid spurious clicks while playing
  Thumb.Enabled := False; // Avoid spurious clicks while playing
  try
  if (Thumb.ClipURI <> '') and URIReachable(Thumb.ClipURI) then begin
    if Copy(Thumb.ClipURI, 1, 4) = 'file' then
      ProcessExecute(fPlayerApp, AnsiQuotedStr(Thumb.ClipURI, '"'), nil, nil, nil)
    else
      OpenURL(Thumb.ClipURI); //Play YouTube videos with YouTube player in default browser.
                              // Attempting to play YouTube URL with a local player gives jerky results.
  end else
    raise Exception.Create('FATAL ERROR: Unable to access video file!');
  finally
    OnClick := @ClipClicked;
    Thumb.Enabled := True;
  end;
end;

function TThumbView.AddThumb(Path: string; var Thumb: TThumb): string; // Returns FailURI on fail
var
  Exists: TObjectList;
  NewID: TID;
begin
  Exists := Database.RetrieveList(TVideoClip, 'Title=?', [ExtractFileName(Path)]);
  if Exists.Count < 1 then begin // Avoid duplicates
    try
    Thumb := TThumb.Create(Self);
    with Thumb do begin
      ThumbSize := ts256;
      ClipURI := Path;
      OnClick := @ClipClicked;
      IsSelected := False;
      Result := FailURI;
      if Result = '' then begin
        Parent := Self;
        NewId := Database.Add(Clip, True);
        RecID := NewID;
        fThumbs.Add(Thumb);
        Result := '';
      end else
        Free; // Discard failed thumb
    end;
    except
      if MessageDlg('LOAD FAIL', 'Failed to load: ' + #13#10#13#10 + Path + #13#10#13#10 +
                 'Continue to load remainder (Y/N)?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then begin
        raise;
        Abort;
      end;
    end;
  end;
end;

procedure TThumbView.RemoveThumb(Thumb: TThumb);
begin
  if Thumb <> Nil then begin
    RemoveControl(Thumb);
    fThumbs.Delete(fThumbs.IndexOf(Thumb));
    Database.Delete(TVideoClip, Thumb.RecID); // Delete associated clip record
  end;
end;

function TThumbView.RemoveMarkedThumbs: Boolean;
var
  aThumb: TThumb;
  i: Integer;
begin
  Result := False;
  for i := fThumbs.Count-1 downto 0 do begin
    aThumb := fThumbs[i];
    if aThumb.MarkedForRemoval then begin
      Result := True;
      RemoveThumb(aThumb);
    end;
  end;
  if Result then begin
    LoadPageOfThumbs;
    if fThumbs.Count > 0 then begin
      fThumbs.SelectThumb(fThumbs[0]);
      fThumbs.SelectedThumb := fThumbs[0];
    end
    else
      ShowHowToMessage:= True;
  end;
end;

end.

