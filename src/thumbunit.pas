unit thumbunit;

{$mode objfpc}{$H+}

{ A TThumb component is an object containing a video thumbnail and its
  associated video title. It responds to select/play clicks, and also has a
  right-click popup menu to perform per-thumb actions.
  It saves/retrieves TVideoClip app_model object data from an SQLite3 database. }

interface
uses
  Classes, SysUtils, Controls, Graphics, StrUtils, Menus, URIParser, Process,
  MD5, Dialogs, fgl, Forms, LCLType, SynCommons, mORMot, fphttpclient,
  opensslsockets, fpjson, jsonparser, RegExpr,
  BGRABitmap, BGRAGraphicControl, BGRABitmapTypes, globalsunit,
  ModelUnit, VideoDocumentsDialogUnit, clipparametersunit;

type
  TEventProc = procedure(aSender:Tobject);
  TThumbSize = (tsNone, ts128, ts256, ts512, ts1024);

  TAfterSelectNextEvent = procedure(Sender: TObject; var ThumbIndex: Integer) of object;
  TAfterSelectPrevEvent = procedure(Sender: TObject; var ThumbIndex: Integer) of object;
  TThumbChangeEvent = procedure(Sender: TObject; var ThumbIndex: Integer) of object;
  TLoadRowEvent = procedure(Sender: TObject; var ThumbIndex: Integer) of object;

  TThumb = class(TBGRAGraphicControl)
  private
    fClip: TVideoClip; // persisted in database
    fRecID: TID;
    fSequenceNo: Integer;
    fClipURI: RawUTF8;
    fPathError: RawUTF8;
    fTitle: RawUTF8;
    fThumbWidth: Integer;
    fThumbHeight: Integer;
    fCaptionHeight: Integer;
    fPicture: TPicture;
    fThumbSize: TThumbSize;
    fThumbScale: String;
    fIsSelected: Boolean;
    fMarkedForRemoval: Boolean;
    fPopup: TPopupMenu;
    fFailURI: string;
    fDescription: RawUTF8;
    fCategory: RawUTF8;
    procedure SetClipURI(Value: RawUTF8);
    procedure SetThumbSize(Value: TThumbSize);
    procedure GetThumbPic;
    procedure CreateOrLoadPic(Folder: string);
    function CreateThumb(Path: string): string;
    procedure SetisSelected(Value: Boolean);
    procedure SetMarkedForRemoval(Value: Boolean);
    procedure PopulateClip;
    procedure ThumbRedraw(Sender: TObject; aBitmap: TBGRABitmap);
    function Path2URI(Path: RawUTF8): RawUTF8;
    procedure DoEditParameters(Sender: TObject);
    procedure MarkToRemove(Sender: TObject);
    procedure UnMarkToRemove(Sender: TObject);
    procedure ShowVideoPath(Sender: TObject);
    procedure AssociateDocuments(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Width;
    property OnStartDrag;
    property OnDragOver;
    property OnEndDrag;
    property Parent;
    property ClipURI: RawUTF8 read fClipURI write SetClipURI;
    property PathError: RawUTF8 read fPathError write fPathError;
    property Title: RawUTF8 read fTitle write fTitle;
    property Clip: TVideoClip read fClip write fClip;
    property RecID: TID read fRecID write fRecID;
    property SequenceNo: Integer read fSequenceno write fSequenceNo;
    property ThumbSize: TThumbSize read fThumbSize write SetThumbSize;
    property IsSelected: Boolean read fIsSelected write SetIsSelected;
    property MarkedForRemoval: Boolean read fMarkedForRemoval write SetMarkedForRemoval;
    property FailURI: string read fFailURI write fFailURI;
    property Description: RawUTF8 read fDescription write fDescription;
    property Category: RawUTF8 read fCategory write fCategory;
  published
    property OnClick;
    property OnDblClick;
  end;

  TThumbList = specialize TFPGObjectList<TThumb>;

  TThumbs = class(TThumbList)
  private
    fSelectedThumb: TThumb;
    fSelectedIndex: Integer;
    fPathErrors: TStringList;
    fTitleFilter: string;
    fOnThumbChange: TThumbChangeEvent;
    fOnLoadRow: TLoadRowEvent;
    fOnAfterSelectNext: TAfterSelectNextEvent;
    fOnAfterSelectPrev: TAfterSelectPrevEvent;
    procedure SetSelectedIndex(Value: Integer);
  public
    constructor Create(FreeObj: Boolean);
    destructor Destroy; override;
    function GetSelectedIndex: Integer;
    function SelectThumb(Thumb: TThumb): Integer;
    function GetSelected(var Thumb: TThumb): Integer;
    function MarkForRemoval(Thumb: TThumb): Integer;
    property TitleFilter: string read fTitleFilter write fTitleFilter;
    property SelectedThumb: TThumb read fSelectedThumb write fSelectedThumb;
    property SelectedIndex: Integer read fSelectedIndex write SetSelectedIndex;
    property OnAfterSelectNext: TAfterSelectNextEvent read fOnAfterSelectNext write fOnAfterSelectNext;
    property OnAfterSelectPrev: TAfterSelectPrevEvent read fOnAfterSelectPrev write fOnAfterSelectPrev;
    property OnThumbChange: TThumbChangeEvent read fOnThumbChange write fOnThumbChange;
    property OnLoadRow: TLoadRowEvent read fOnLoadRow write fOnLoadRow;
  end;

implementation

constructor TThumb.Create(AOwner: TComponent);
var
  EditCategory, EditClassification, EditDescription, EditAttribution, EditRating,
  EditKeywords, VideoDetails, EditParameters, MarkForRemoval,
  UnMarkForRemoval, ShowPath, AddOrRemoveDocuments: TMenuItem;
 begin
  inherited Create(AOwner);
  BevelWidth := 2;
  BevelInner := bvRaised;
  BevelOuter := bvRaised;
  fFailURI := '';
  fPicture := TPicture.Create;
  OnRedraw := @ThumbRedraw;
  Width := THUMB_WIDTH;
  fThumbSize := tsNone;
  fThumbWidth := Width;
  fThumbHeight := THUMB_HEIGHT;
  fCaptionHeight := CAPTION_HEIGHT;
  fClip := TVideoClip.Create;
  Height := fThumbHeight + fCaptionHeight;
  fPopup := TPopupMenu.Create(Self);
  with fPopup do begin
    EditParameters := TMenuItem.Create(Self);
    Parent := Self;
    EditParameters.Caption := 'Show/Edit Parameters';
    Items.Add(EditParameters);
    EditParameters.OnClick := @doEditParameters;

    MarkForRemoval := TMenuItem.Create(Self);
    Parent := Self;
    MarkForRemoval.Caption := 'Mark For Removal';
    Items.Add(MarkForRemoval);
    MarkForRemoval.OnClick := @MarkToRemove;

    UnMarkForRemoval := TMenuItem.Create(Self);
    Parent := Self;
    UnMarkForRemoval.Caption := 'UnMark';
    Items.Add(UnMarkForRemoval);
    UnMarkForRemoval.OnClick := @UnMarkToRemove;

    ShowPath := TMenuItem.Create(Self);
    Parent := Self;
    ShowPath.Caption := 'Show Video File Path';
    Items.Add(ShowPath);
    ShowPath.OnClick := @ShowVideoPath;

    AddOrRemoveDocuments := TMenuItem.Create(Self);
    Parent := Self;
    AddOrRemoveDocuments.Caption := 'Add/Remove Documents';
    Items.Add(AddOrRemoveDocuments);
    AddOrRemoveDocuments.OnClick := @AssociateDocuments;
  end;
end;

destructor TThumb.Destroy;
begin
  if Assigned(fPicture) then
    fPicture.Free;
  if Assigned(fPopup) then
    fPopup.Free;
  inherited Destroy;
end;

procedure TThumb.SetClipURI(Value: RawUTF8);
begin
  if fClipURI <> Value then begin
    fClipURI := Value;
    if (fClipURI <> '') then begin
      PopulateClip;
      if Assigned(fCLip) and (fClipURI <> '') and (fThumbSize <> tsNone) then begin
        fPathError := fClip.PathError;
        GetThumbPic;
      end;
    end;
  end;
end;

function TThumb.Path2URI(Path: RawUTF8): RawUTF8;
var
  aURI: TURI;
begin
  aURI.Document := Path;
  aURI.Protocol := 'file';
  aURI.Port := 0;
  Result := QuotedStr(EncodeURI(aURI));
end;

procedure TThumb.SetThumbSize(Value: TThumbSize);
begin
  if fThumbSize <> Value then begin
    fThumbSize := Value;
    //The following are resultant thumbnail widths. Their height is autocalculated
    //by ffmpeg according to the associated video file aspect ratio.
    //Note that the default Linux thumbnails are too small to be of practical use
    //So we create new ones with a default width of 128 pixels.
    //Thumbnails are saved at /home/<user>/.cache/thumbnails/large. (This is a hidden
    //directory so in file explorer you need to type Ctrl-h in the user directory to see it).
    case fThumbSize of
      ts128: fThumbScale := '128';
      ts256: fThumbScale := '256';
      ts512: fThumbScale := '512';
      ts1024: fThumbScale := '1024';
    else
      fThumbScale := '128';
    end;
    if Assigned(fCLip) and (fClipURI <> '')  and (fThumbSize <> tsNone) then
      GetThumbPic;
  end;
end;

procedure TThumb.GetThumbPic;
var
  Folder, d: string;
begin
 Folder := '';
 d := DirectorySeparator;
  case fThumbSize of
    ts128: Folder := ThumbnailFolder + d +'normal' + d;
    ts256: Folder := ThumbnailFolder + d + 'large' + d;
    ts512: Folder := ThumbnailFolder + d + 'x-large' + d;
    ts1024: Folder := ThumbnailFolder + d + 'xx-large' + d;
  end;
  if not DirectoryExists(ThumbnailFolder) then
    CreateDir(ThumbnailFolder);
  CreateOrLoadPic(Folder);
end;

procedure TThumb.CreateOrLoadPic(Folder: string);
var
  ThumbnailFilePath: string;
  ErrorString: string;
begin
  if not DirectoryExists(Folder) then
    CreateDir(Folder);
  // Store the thumbnail in a path that is a hash of the URI
  ThumbnailFilePath := Folder + MD5Print(MD5String(fClipURI)) + '.PNG';
  if FileExists(ThumbnailFilePath) then begin
    if fClip.ThumbnailFilePath <> ThumbnailFilePath then begin
      fClip.ThumbnailFilePath := ThumbnailFilePath;
      Database.Update(fClip);
    end
  end else
    ErrorString := CreateThumb(ThumbnailFilePath);
  fPicture.LoadFromFile(ThumbnailFilePath);
end;

procedure TThumb.SetIsSelected(Value: Boolean);
begin
  if fIsSelected <> Value then begin
    fIsSelected := Value;
    RedrawBitmap;
  end;
end;

procedure TThumb.SetMarkedForRemoval(Value: Boolean);
begin
  if fMarkedForRemoval <> Value then begin
    fMarkedForRemoval := Value;
    RedrawBitmap;
  end;
end;

function TThumb.CreateThumb(Path: string): string;
var
  OutputStream, ErrorStream: TMemoryStream;
  Params: string;
  SL, ParamList: TStringList;
  wid, hgt: string;
  YoutubeThumbPath, VideoID: string;
begin
  wid := fthumbScale;
  hgt := IntToStr(StrToInt(fThumbScale) * 3 div 4);
  OutputStream := TMemoryStream.Create;
  ErrorStream := TMemoryStream.Create;
  ParamList := TStringList.Create;
  if Copy(fClipURI, 1, 4) = 'file' then begin
    //Our thumbnail is a snapshot taken 3 seconds into the video...
    with ParamList do begin
      Clear;
      Add('-ss'); // position
      Add('3'); // 3 seconds in
      Add('-i'); // input
      Add(fClipURI); // input file
      // Scale thumbnail to fThumbScale width, keep aspect ratio
      Add('-vf'); // output filter graph
      Add('scale='+wid+':'+hgt+':force_original_aspect_ratio=decrease,pad='+wid+':'+hgt
         +':x=('+wid+'-iw)/2:y=('+hgt+'-ih)/2');
      Add('-vframes'); // video frames
      Add('1'); // number of frames
      Add(Path); // output file
    end;
  end else if IsYoutubeURI(fClipURI) then begin // Assume YouTube
    VideoID := Copy(fClipURI, Pos('watch?v=', fClipURI) + 8, Length(ClipURI));
    YoutubeThumbPath := 'https://img.youtube.com/vi/' + VideoID + '/1.jpg'; ///maxresdefault.jpg';
    with ParamList do begin
      Clear;
      Add('-i');
      Add(YoutubeThumbPath); // input file
      // Scale thumbnail to fThumbScale width, keep aspect ratio
      Add('-vf'); // output filter graph
      Add('scale='+wid+':'+hgt+':force_original_aspect_ratio=decrease,pad='+wid+':'+hgt
         +':x=('+wid+'-iw)/2:y=('+hgt+'-ih)/2');
      Add(Path); // output file
    end;
  end;
  Params := ParamList.CommaText;
  if ProcessExecute('ffmpeg', Params, nil, OutputStream, ErrorStream) then begin
    try
      ErrorStream.Position := 0;
      SL := TStringList.Create;
      SL.LoadFromStream(ErrorStream);
      Result := SL.Text;
    finally
      if Assigned(SL) then
        SL.Free;
      FreeAndNil(OutputStream);
      FreeAndNil(ErrorStream);
      ParamList.Free;
    end;
  end else begin
    ShowMessage('Failed to run ffmpeg process.' + #13#10 +
                'Check that ffmpeg is in the ClipBrowser install folder.');
    Abort;
  end;
end;

procedure TThumb.PopulateClip;
var
  Client: TFPHttpClient;
  aTitle, s: string;
  jData: TJSONData;
begin
  if Assigned(fClip) then with fClip do begin
    Rating := DEFAULT_RATING;
    if Restriction = '' then
      Restriction := DefaultRestriction;
    if Category = '' then
      Category := DefaultCategory;
    ClipURI := Self.ClipURI;
    if Copy(ClipURI, 1, 4) = 'file' then
      aTitle := ExtractFileName(URI2Path(ClipURI))
    else begin
      Client := TFPHttpClient.Create(Nil);
      try
      // Get YouTube metadata for this VideoID
      s := Client.Get(YoutubeMetadataURL + '?url=' + ClipURI + '&format=json');
      jData := GetJSON(s);
      aTitle := TJSONObject(jData).Get('title');
      finally
        Client.Free;
      end;
    end;
    fTitle := aTitle;
    fClip.Title := aTitle;
    Database.Update(fClip);
  end;
end;

procedure TThumb.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    fPopup.Popup;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TThumb.ThumbRedraw(Sender: TObject; aBitmap: TBGRABitmap);
var
  PicRect, TitleRect: TRect;
  c: TBGRAPixel;
begin
  if fPicture <> Nil then begin
    PicRect.Left := 2 * Self.BevelWidth;
    PicRect.Top := 2 * Self.BevelWidth;
    PicRect.Right := aBitmap.Width - 2 * Self.BevelWidth;
    PicRect.Height := Round(fThumbWidth * fPicture.Height/fPicture.Width);
    aBitmap.Canvas.StretchDraw(PicRect, fPicture.Graphic);
    if fIsSelected then begin
      // Draw focus rectangle
      aBitmap.Canvas.Pen.Width := 3;
      aBitmap.Canvas.Pen.Color := clRed;
      aBitmap.Canvas.Brush.Style := bsClear;
      aBitmap.Canvas.Rectangle(PicRect);
    end;
    if fMarkedForRemoval then begin
      // Draw an X
      aBitmap.Canvas.Font.Size := 20;
      aBitmap.Canvas.Font.Color := clRed;
      aBitmap.Canvas.TextOut(10,10, 'X');
    end;
  end;
  if fTitle <> '' then begin
    TitleRect.Top := PicRect.Bottom;
    TitleRect.Left := 2 * Self.BevelWidth;
    TitleRect.Right := aBitmap.Width - 2 * Self.BevelWidth;
    TitleRect.Height := fCaptionHeight;
    aBitmap.FillRect(TitleRect, clMoneyGreen);
    aBitmap.FontFullHeight := 12;
    aBitmap.FontStyle := [fsBold];
    c := ColorToBGRA(ColorToRGB(clBtnText));
    aBitmap.TextRect(TitleRect, fTitle, taCenter, tlTop, c);
  end;
end;

procedure TThumb.DoEditParameters(Sender: TObject);
var
  Dlg: TParametersDialog;
begin
 Dlg := TParametersDialog.Create(Nil);
 Dlg.ID := fRecID;
 try
   Dlg.ShowModal;
 finally
   Dlg.Free;
 end;
end;

procedure TThumb.MarkToRemove(Sender: TObject);
begin
  fMarkedForRemoval := True;
  RedrawBitmap;
end;

procedure TThumb.UnMarkToRemove(Sender: TObject);
begin
  fMarkedForRemoval := False;
  RedrawBitmap;
end;

procedure TThumb.ShowVideoPath(Sender: TObject);
begin
  Application.MessageBox(pChar(fClipURI), 'File Path', IDOK);
end;

procedure TThumb.AssociateDocuments(Sender: TObject);
var
  Dlg: TAssociatedDocumentsDialog;
begin
  Dlg := TAssociatedDocumentsDialog.Create(Self);
  try
    Dlg.ID := fRecID;
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{ TThumbs }

constructor TThumbs.Create(FreeObj: Boolean);
begin
  inherited Create(FreeObj);
  fpathErrors := TStringList.Create;
end;

destructor TThumbs.Destroy;
begin
  if Assigned(fPathErrors) then
    fPathErrors.Free;
  inherited Destroy;
end;

function TThumbs.SelectThumb(Thumb: TThumb): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Count-1 do begin
    if Items[Index] = Thumb then begin
      Items[Index].fIsSelected := True;
      Items[Index].RedrawBitmap;
      Result := Index;
    end else begin
      Items[Index].fIsSelected := False;
      Items[Index].RedrawBitmap;
    end;
  end;
  if Assigned(fOnThumbChange) then
    fOnThumbChange(Self, Result);
end;

function TThumbs.GetSelected(var Thumb: TThumb): Integer;
var
  Index: Integer;
begin
  Thumb := Nil;
  Result := -1;
  for Index := 0 to Count-1 do begin
    if Items[Index].fIsSelected then begin
      Thumb := Items[Index];
      Result := Index;
      Exit;
    end;
  end;
end;

function TThumbs.MarkForRemoval(Thumb: TThumb): Integer;
begin
  Result := IndexOf(Thumb);
  if Result > -1 then
    Items[Result].MarkedForRemoval := True;
end;

function TThumbs.GetSelectedIndex: Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Count -1 do
    if Items[Index].IsSelected then begin
      Result := Index;
      Exit;
    end;
end;

procedure TThumbs.SetSelectedIndex(Value: Integer);
var
  i: Integer;
begin
  if (fSelectedIndex < Count) then begin
    fSelectedIndex := Value;
    for i := 0 to Count-1 do
      if i = Value then begin
        Items[i].fIsSelected := True;
        Items[i].RedrawBitmap;
      end else begin
        Items[i].fIsSelected := False;
        Items[i].RedrawBitmap;
      end;
  end;
end;

end.

