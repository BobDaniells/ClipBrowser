unit ClipBrowserMain;
{******************************************************************************
 "ClipBrowser" is a Linux or Windows desktop application that gathers all of 
 your videos into one library to facilitates rapid search and play operations.
 As such it has similar functionality to the Linux "gmusicbrowser" browser 
 that targets music CDs. It can handle local video files and/or YouTube links.

 Features:

 1. Video files are stored in a database as file paths (or URL's in the case of
    YouTube clips. Local files can be added to the database individually, or as
    groups, or as dragging and dropping onto the program's main window. There
    is also a menu option to find and load all video files under a specified 
    root path. You add YoutTube links by copying and pasting URLs from a web
    browser.

 2. Library content is displayed as a matrix of thumbnails together with
    titles. For local files these thumbnails are auto-generated, or downloaded
    in the case of YouTube videos. Thumbnails are stored in the default
    thumbnail path for the relevant OS. 

 3. Any video can be instantly selected simply by clicking its thumbnail and/or
    played by double-clicking its thumbnail. ClipBrowser provides a choice of
    (separately=installed) players like VLC for local video files. The default
    web browser (with an active Internet connection) is used for YouTube videos.

 4. The displayed thumbs may be filtered by:

        a. A text string that represents any substring of the title.
           E.g. Entering 'Bach' would show only videos with 'Bach' in the title.

        b. Up to 32 keywords that get stored in the library along with the
           (This is far more convenient than the alternative of manually finding
           and playing videos that may be stored in multiple OS directories).

        c. A category like 'Defaut', 'Music', 'How To', 'Pets', etc.
           videos are visible).

  5. The thumb matrix uses a 'lazy loading' strategy to optimize load times and
    responsiveness.  I.e., when the app is opened only one page of thumbs
    (typically 15) is loaded. Scrolling the display down by one row then loads
    the next row of thumbs (typically 5), and so on. This thumb matrix is fully
    responsive in terms of displaying more or fewer thumbs on resizing the
    window.

 7. Each thumb can also any number of have document files attached. For example,
    if a particular thumb contains a music video then it is possible to add
    associated sheet music as a PDF file. Double clicking any item in the document
    list for the thumb opens the associated file with the default application
    for that file type.

 8. On first run the database and thumb matrix will be empty, but a menu option
    loads a range of test data from YouTube. Note that this action is very fast
    as it does not download and store any files, but rather just the URI's of
    those files. This also avoids potential copyright implications.

 9. ClipBrowser also contains parental control options.

 10. ClipBrowser showcases the capabilities of the Synopse mORMot object-
    oriented database framework with Lazarus. The libary database is
    automatically created at program startup. Data access is extremely fast.

 Copyright Bob Daniells, 10/09/2022.
 License: GPL

 Compile with Lazarus 2.1.12 or later and Synopse mORMot 1.18.
 Other dependencies include ffmpeg, BGRABitmap, BGRAControls,
 ffmpeg, plus your choice of video players.
}

{$mode objfpc}{$H+}

interface

uses
  {$I SynDprUses.inc}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType, ExtCtrls,
  Types, LCLintf, Process, StdCtrls, Menus, ActnList, UITypes, Clipbrd,
  ComCtrls, Buttons, Grids, fileutil, strutils, contnrs, userunit, ModelUnit, globalsunit,
  mORMot, SynCommons, SynCrypto, mORMotSQLite3, SynSQLite3Static, thumbunit,
  ThumbViewUnit, ThumbFailDialogUnit,
  LoadProgressDialogUnit, ClassificationFilterUnit, ProcessAnimationDialogUnit,
  ChangePINUnit, AdditionalFilterDialogUnit, AboutUnit, TestDataUnit;

type
  { TMainForm }

  { This form represents the VIEW component of a MVC structure. It should
    contain only display-specific items and certainly no business logic! }

  TMainForm = class(TForm)
    AboutAction: TAction;
    AddTestDataAction: TAction;
    AddYouTubeURLAction: TAction;
    ClearFilterAction: TAction;
    Label3: TLabel;
    ClearFilterSpeedButton: TSpeedButton;
    AddYTMenuItem: TMenuItem;
    AddTestDataMenuItem: TMenuItem;
    PINResetLabel: TLabel;
    PreviousRowAction: TAction;
    NextRowAction: TAction;
    PreviousThumbAction: TAction;
    NextThumbAction: TAction;
    PageDownAction: TAction;
    PageUpAction: TAction;
    PopupMenuSpeedButton: TSpeedButton;
    SearchPanel1: TPanel;
    AdditionalFiltersSpeedButton: TSpeedButton;
    TitleFilterClearPanel: TPanel;
    PurgeMenuItem: TMenuItem;
    PurgeAction: TAction;
    AdditionalFilterAction: TAction;
    DeleteClipMenuItem: TMenuItem;
    AdditionalFiltersButton: TButton;
    ChangeParentalPinAction: TAction;
    FileDropAction: TAction;
    ChangeParentalPinMenuItem: TMenuItem;
    ReloadAction: TAction;
    ParentalControlMenuItem: TMenuItem;
    ParentalControlAction: TAction;
    LoadAllFromVideosFolderAction: TAction;
    HelpAction: TAction;
    MenuItem1: TMenuItem;
    HelpMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    FindAndLoadMenuItem: TMenuItem;
    QueueClearAction: TAction;
    QueuePlayAction: TAction;
    MainMenuAction: TAction;
    DownAction: TAction;
    TotalClipsLabel: TLabel;
    UpAction: TAction;
    TopSpeedButton: TSpeedButton;
    UpButton: TSpeedButton;
    ScrollButtonPanel: TPanel;
    PlayerMenuItemsAction: TAction;
    PlayerPopupMenu: TPopupMenu;
    SetPlayerAction: TAction;
    SetPlayerMenuItem: TMenuItem;
    PopulateAction: TAction;
    PopulateMenuItem: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    ButtonPanel: TPanel;
    AddClipMenuItem: TMenuItem;
    SelectVideoDirectoryDialog: TSelectDirectoryDialog;
    DownButton: TSpeedButton;
    Splitter1: TSplitter;
    TitleMemo: TMemo;
    SearchEdit: TEdit;
    RemoveMarkedThumbsAction: TAction;
    AddClipAction: TAction;
    MainActionList: TActionList;
    ClipFileOpenDialog: TOpenDialog;
    Panel1: TPanel;
    FlowBasePanel: TPanel;
    SearchPanel: TPanel;
    MenuPopupMenu: TPopupMenu;
    procedure AboutActionExecute(Sender: TObject);
    procedure AddClipActionExecute(Sender: TObject);
    procedure AdditionalFilterActionExecute(Sender: TObject);
    procedure AddTestDataActionExecute(Sender: TObject);
    procedure AddYouTubeURLActionExecute(Sender: TObject);
    procedure AddYTMenuItemClick(Sender: TObject);
    procedure ClearFilterActionExecute(Sender: TObject);
    procedure DownActionExecute(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PasteURIMenuItemClick(Sender: TObject);
    procedure PINResetLabelClick(Sender: TObject);
    procedure MenuButtonClick(Sender: TObject);
    procedure UpActionExecute(Sender: TObject);
    procedure FlowBasePanelMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure NextRowActionExecute(Sender: TObject);
    procedure NextThumbActionExecute(Sender: TObject);
    procedure PageDownActionExecute(Sender: TObject);
    procedure PageUpActionExecute(Sender: TObject);
    procedure PreviousRowActionExecute(Sender: TObject);
    procedure PreviousThumbActionExecute(Sender: TObject);
    procedure PurgeActionExecute(Sender: TObject);
    procedure RemoveMarkedThumbsActionExecute(Sender: TObject);
    procedure LoadAllFromVideosFolderActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PopupMenuActionExecute(Sender: TObject);
    procedure ParentalControlActionExecute(Sender: TObject);
    procedure PopulateActionExecute(Sender: TObject);
    procedure ReloadActionExecute(Sender: TObject);
    procedure SearchEditExit(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SetPlayerActionExecute(Sender: TObject);
    procedure ChangeParentalPinActionExecute(Sender: TObject);
    procedure AskAboutTestData;
    procedure SelectedThumbChanged(Sender: TObject; var ThumbIndex: Integer);
  private
    fUser: TUser;
    fThumbView: TThumbView;
    fThumbs: TThumbs;
    fInstalledVideoPlayers: TStringList;
    fThumbFailList: TStringList;
    fPathErrors: TStringList;
    fProcessAnimationDialog: TProcessAnimationDialog;
    procedure TheKeyHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CreateThumbsList;
    procedure CreateThumbView;
    procedure CreateModel;
    procedure CreateDatabase;
    procedure SetThumbnailAccess;
    procedure GetAvailableVideoPlayers;
    procedure AddVideoFile(FileName: string; Index: Integer; ProgressDlg: TLoadProgressDialog);
    procedure AddMultipleVideoFiles(const FileNames: array of string);
    function IsVideoFile(FileName: string): Boolean;
    procedure ThumbDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    function GetCellHeight(aGrid: TStringGrid; ACol, ARow: Integer): Integer;
    procedure PopulatePlayersMenu;
    procedure PlayerMenuItemsClicked(Sender: TObject);
    procedure ThumbsNewRow(Sender: TObject; var Index: Integer);
    procedure ThumbViewOnSelectThumb(Sender: TObject);
  public
    procedure UpdateClipsTotal;
    procedure CloseAnimationDialog;
    property ProcessAnimationDialog: TProcessAnimationDialog read fProcessAnimationDialog
             write fProcessAnimationDialog;
    property ThumbView: TThumbView read fThumbView write fThumbView;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  ApplicationPath := ExeVersion.ProgramFileName;
  Application.AddOnKeyDownBeforeHandler(@TheKeyHandler);
  Self.Width := 864;
  Self.Height := 655;
  Self.Position := poScreenCenter;
  fUser := TUser.Create;
  SetThumbnailAccess;
  fThumbFailList := TStringList.Create;
  fPathErrors := TStringList.Create;
  ClipFileOpenDialog.InitialDir := ClipFileOpenDialogFolder;
  ClipFileOpenDialog.Filter := 'Video files|' + VideoFileTypes + 'All files|*.*';
  SearchEdit.Width := SearchPanel.Width - ButtonPanel.Width - 110;
  TitleFilterClearPanel.Left := SearchEdit.Left + SearchEdit.Width;
  CreateModel;
  CreateDatabase;
  CreateThumbView;
  SetInitialPassword;
  CreateThumbsList;
  fThumbView.ClassificationFilter := 'G'; // Always apply censorship for initial opening
  UpdateClipsTotal;
  AskAboutTestData;
  GetAvailableVideoPlayers;
  TCategory.AddDefaultCategories;
end;

procedure TMainForm.SetThumbnailAccess;
begin
  ThumbnailFolder := fUser.ThumbPath;
  if not DirectoryExists(ThumbnailFolder) then
    CreateDir(ThumbnailFolder);
//    raise Exception.Create('FATAL ERROR - Cannot access the Linux thumbnails cache');
  fThumbFailList := TStringList.Create;
end;

procedure TMainForm.CreateThumbsList;
begin
  fThumbs := TThumbs.Create(True); // List that owns all thumb instances
  fThumbView.Thumbs := fThumbs;
  fThumbView.Thumbs.OnThumbChange := @SelectedThumbChanged;
  fThumbs.OnLoadRow := @ThumbsNewRow;
  fThumbs.SelectedIndex := 0;
  if fThumbs.Count > 0 then
    TitleMemo.Text := fThumbs.Items[0].Title;
end;

procedure TMainForm.CreateThumbView;
begin
  fThumbView := TThumbView.Create(Self);
  with fThumbView do begin
    Parent := FlowBasePanel;
    Align := alClient;
    Color := clMoneyGreen;
    Font.Style := [fsBold];
    ThumbWidth := THUMB_WIDTH;
    ThumbHeight := THUMB_HEIGHT + CAPTION_HEIGHT;
    Width := Parent.ClientWidth;
    Height := Parent.ClientHeight;
    ThumbRowCapacity := Width div ThumbWidth;
    ThumbMaxRows := Height div ThumbHeight;
    ShowHowToMessage := True;
    OnSelectThumb := @ThumbViewOnSelectThumb;
    SetClipTableIDLimits;
  end;
end;

procedure TMainForm.CreateModel;
begin
  aModel := CreateDataModel;
end;

procedure TMainForm.CreateDatabase;
begin
  Database := Nil;
  // Use an embedded SQLite3 database...
  DatabasePath := ChangeFileExt(ApplicationPath,'.db3');
  Database := TSQLRestServerDB.Create(aModel, DatabasePath);
  TSQLRestServerDB(Database).CreateMissingTables;
end;

procedure TMainForm.GetAvailableVideoPlayers;
begin
  GetInstalledVideoPlayers;
  PopulatePlayersMenu;
end;

procedure TMainForm.UpdateClipsTotal;
begin
  TotalClipsLabel.Caption := 'Total Clips: ' + IntToStr(Database.TableRowCount(TVideoClip));
  Application.ProcessMessages;
  if Database.TableRowCount(TVideoClip) > 0 then
    fThumbView.ShowHowToMessage := False;
end;

procedure TMainForm.AskAboutTestData;
begin
  if Database.TableRowCount(TVideoClip) < 1 then
    if MessageDlg('ClipBrowser: Empty Database', 'ClipBrowser Database is currently empty' + #13#10 +
                                 'Automatically add some test data from YouTube (Y/N)?',
                                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
                                 AddTestDataActionExecute(Self);
end;

procedure TMainForm.TheKeyHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RIGHT: NextThumbAction.Execute;
    VK_LEFT: PreviousThumbAction.Execute;
    VK_DOWN: NextRowAction.Execute;
    VK_UP: PreviousRowAction.Execute;
    VK_PRIOR: PageUpAction.Execute;
    VK_NEXT: PageDownAction.Execute;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Assigned(fThumbFailList) then
    fThumbFailList.Free;
  if Assigned(fPathErrors) then
    fPathErrors.Free;
  if Assigned(fThumbs) then
    fThumbs.Free;
  if Assigned(fInstalledVideoPlayers) then
    fInstalledVideoPlayers.Free;
  if Assigned(fThumbView) then
    fThumbView.Free;
  if Assigned(fUser) then
    fUser.Free;
  CanClose := True;
end;

procedure TMainForm.RemoveMarkedThumbsActionExecute(Sender: TObject);
begin
  if MessageDlg('Remove the selected thumb(s) from the library?' +#13#10 +
                  'Are You Sure (Y/N)', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    if not fThumbView.RemoveMarkedThumbs then
      TitleMemo.Clear;
end;

procedure TMainForm.LoadAllFromVideosFolderActionExecute(Sender: TObject);
var
  aThread: TFindAndLoadVideosThread;
  var
    RootPath: string;
begin
  {$IFDEF LINUX}
    RootPath := InputBox('Search Path', 'Root path to search from', fUser.VideosPath);
  {$ENDIF}
  {$IFDEF WINDOWS}
    RootPath := InputBox('Search Path', 'Root path to search from', 'C:\');
  {$ENDIF}
  Application.ProcessMessages;
  aThread := TFindAndLoadVideosThread.Create(True);
  aThread.SearchPath := RootPath;
  aThread.Start;
  Application.ProcessMessages;
end;

procedure TMainForm.AddClipActionExecute(Sender: TObject);
var
  FileURI, FailURI: string;
  aThumb: TThumb;
begin
  aThumb := Nil;
  if ClipFileOpenDialog.Execute then begin
    FileURI := Path2URI(ClipFileOpenDialog.FileName);
    FailURI := fThumbView.AddThumb(FileURI, aThumb);
  end;
end;

procedure TMainForm.AboutActionExecute(Sender: TObject);
var
  AboutDialog: TAboutDialog;
begin
  AboutDialog := TAboutDialog.Create(Nil);
  try
    AboutDialog.ShowModal;
  finally
    AboutDialog.Free;
  end;
end;

procedure TMainForm.AdditionalFilterActionExecute(Sender: TObject);
var
  Dlg: TAdditionalFilterDialog;
begin
  Dlg := TAdditionalFilterDialog.Create(Nil);
  try
    Dlg.SelectedThumb := fThumbs.SelectedThumb;
    Dlg.ShowModal;
    if Dlg.CloseMode <> cmCancel then begin
      fThumbView.CategoryFilter := Dlg.Categories;
      fThumbView.KeywordFilter := Dlg.Keywords;
      fThumbView.LoadPageOfThumbs;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TMainForm.AddTestDataActionExecute(Sender: TObject);
var
  TestDataPopulator: TTestDataPopulator;
begin
  TestDataPopulator := TTestDataPopulator.Create;
  try
    TestDataPopulator.Execute;
  finally
    TestDataPopulator.Free;
  end;
end;

procedure TMainForm.AddYouTubeURLActionExecute(Sender: TObject);
var
  URL, FailURI: string;
  aThumb: TThumb;
begin
  aThumb := Nil;
  URL := InputBox('YouTube Item', 'Paste a YouTube URL here...', '');
  FailURI:= fThumbView.AddThumb(URL, aThumb);
  if FailURI <> '' then
    ShowMessage('That URL was not reachable.' + #1#10 +
    'Please check that the address is valid' + #13#10 +
    'and that you have a working Internet connection.');
end;

procedure TMainForm.AddYTMenuItemClick(Sender: TObject);
begin
  AddYouTubeURLAction.Execute;
end;

procedure TMainForm.ClearFilterActionExecute(Sender: TObject);
begin
  SearchEdit.Clear;
  SearchEditExit(Self);
end;

procedure TMainForm.DownActionExecute(Sender: TObject);
begin
  fThumbView.ScrollPage(sdDown);
end;

procedure TMainForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  fThumbView.ScrollPage(sdDown);
end;

procedure TMainForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  fThumbView.ScrollPage(sdUp);
end;

procedure TMainForm.PasteURIMenuItemClick(Sender: TObject);
begin
end;

// Secret access to reset parental pin...
procedure TMainForm.PINResetLabelClick(Sender: TObject);
begin
  ResetPassword;
end;

procedure TMainForm.MenuButtonClick(Sender: TObject);
begin
  MenuPopupMenu.Popup(0, 0);
end;

procedure TMainForm.UpActionExecute(Sender: TObject);
begin
  fThumbView.ScrollPage(sdUp);
end;

procedure TMainForm.FlowBasePanelMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
end;

procedure TMainForm.NextRowActionExecute(Sender: TObject);
begin
  fThumbView.NextRow;
end;

procedure TMainForm.NextThumbActionExecute(Sender: TObject);
begin
  fThumbView.NextThumb;
end;

procedure TMainForm.PageDownActionExecute(Sender: TObject);
begin
  DownButton.Click;
end;

procedure TMainForm.PageUpActionExecute(Sender: TObject);
begin
  UpButton.Click;
end;

procedure TMainForm.PreviousRowActionExecute(Sender: TObject);
begin
  fThumbView.PreviousRow;
end;

procedure TMainForm.PreviousThumbActionExecute(Sender: TObject);
begin
  fThumbView.PreviousThumb;
end;

procedure TMainForm.PurgeActionExecute(Sender: TObject);
begin
  if MessageDlg('Purge ALL thumbs from the library?' +#13#10 +
                'Are You Sure (Y/N)', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    FThumbView.Purge;
    UpdateClipsTotal;
  end;
end;

procedure TMainForm.ThumbDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Sender is TThumb then
    Accept:=True;
end;

procedure TMainForm.PopulatePlayersMenu;
var
  Players: TObjectList;
  i: Integer;
  Item: TMenuItem;
  Items: array of TMenuItem = Nil;
begin
  Players := Database.RetrieveList(TVideoPlayer, '', []);
  try
    for i := 0 to Players.Count-1 do begin
      Item := TMenuItem.Create(PlayerPopupMenu);
      Item.Tag := i;
      Item.Onclick := @PlayerMenuItemsClicked;
      Item.Caption := TVideoPlayer(Players[i]).DisplayName;
      if TVideoPlayer(Players[i]).DefaultPlayer then
        Item.Checked := True;
      SetLength(Items, Length(Items)+1);
      Items[i] := Item;
    end;
    PlayerPopupMenu.Items.Add(Items);
  finally
    Players.Free;
  end;
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  MousePoint: TPoint;
  Control: TControl;
begin
  MousePoint := ScreenToClient(Mouse.CursorPos);
  if PtInRect(FlowBasePanel.BoundsRect, MousePoint) then
    AddMultipleVideoFiles(FileNames);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  TitleMemo.Width := Self.Width - SearchPanel.Width - 12;
  fThumbView.FlowResize(Self);
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  ThumbIndex: Integer;
begin
  ThumbView.FlowResize(Self); //Reloads first page according to page size
  SearchEdit.SetFocus;
  ThumbIndex := 0;
  SelectedThumbChanged(Self, ThumbIndex);
end;

procedure TMainForm.PopupMenuActionExecute(Sender: TObject);
begin
  MenuPopupMenu.PopUp;
end;

procedure TMainForm.ParentalControlActionExecute(Sender: TObject);
var
  PIN: string;
  anon: TSomeone;
  ctr: Integer;
  Dlg: TClassificationsFilterDialog;
begin
  ctr := 0;
  anon := TSomeone.Create(Database, 1);
  while (ctr < 3) do begin
    PIN := PasswordBox('Parental Control PIN', 'Enter PIN');
    if anon.Checkpassword(PIN) then begin
      Dlg := TClassificationsFilterDialog.Create(self);
      try
        Dlg.ShowModal;
        fThumbView.ClassificationFilter := Dlg.AllowedClassifications;
        fThumbView.LoadPageOfThumbs;
      finally
        Dlg.Free;
      end;
      break;
    end;
    Inc(ctr);
  end;
  anon.Free;
end;

procedure TMainForm.PlayerMenuItemsClicked(Sender: TObject);
var
  List: TObjectList;
  i: Integer;
begin
  List := Database.RetrieveList(TVideoPlayer, '', []);
  try
    for i := 0 to List.Count-1 do begin
      TVideoPlayer(List[i]).DefaultPlayer := False; // Set All false
      if TVideoPlayer(List[i]).Idx = (Sender as TMenuItem).Tag then begin
        PlayerPopupMenu.Items[i].Checked := True;
        TVideoPlayer(List[i]).DefaultPlayer := True;
      end else begin
        PlayerPopupMenu.Items[i].Checked := False;
        TVideoPlayer(List[i]).DefaultPlayer := False;
      end;
      Database.Update(TVideoPlayer(List[i]));
    end;
  finally
    List.Free;
  end;
end;

procedure TMainForm.PopulateActionExecute(Sender: TObject);
var
  i: Integer;
  VideoFiles: TStringList;
  FilesArray: array of string = ('');
begin
  if SelectVideoDirectoryDialog.Execute then begin
    VideoFiles := FindAllFiles(SelectVideoDirectoryDialog.FileName, VideoFileTypes, True, faDirectory);
    try
      SetLength(FilesArray, VideoFiles.Count);
      for i := 0 To VideoFiles.Count-1 do
        FilesArray[i] := VideoFiles[i];
      AddMultipleVideoFiles(FilesArray);
    finally
      VideoFiles.Free;
    end;
  end;
end;

procedure TMainForm.AddMultipleVideoFiles(const FileNames: array of string);
var
  i, f: Integer;
  FailDlg: TThumbFailDialog;
  ProgressDlg: TLoadProgressDialog;
  FilesList: TStringList;
begin
  ProgressDlg := TLoadProgressDialog.Create(Nil);
  ProgressDlg.NumClips := Length(FileNames);
  ProgressDlg.Show;
  Application.ProcessMessages;
  for i := 0 to Length(FileNames)-1 do begin
    if DirectoryExists(FileNames[i]) then begin
      FilesList := TStringList.Create;
      try
        FindAllFiles(FilesList, FileNames[i], VideoFileTypes, True);
        ProgressDlg.NumClips := FilesList.Count;
        for f := 0 to FilesList.Count -1 do
          AddVideoFile(FilesList[f], f, ProgressDlg);
        UpdateClipsTotal;
      finally
        FilesList.Free;
      end;
    end else
      AddVideoFile(FileNames[i], i, ProgressDlg);
  end;
  if fThumbFailList.Count > 0 then begin
    FailDlg := TThumbFailDialog.Create(Nil);
    try
      FailDlg.ErrorList := fThumbFailList;
      FailDlg.ShowModal;
    finally
      FailDlg.Free;
    end;
  end;
  UpdateClipsTotal;
  fThumbView.SetClipTableIDLimits;
  fThumbView.LoadPageOfThumbs;
end;

procedure TMainForm.AddVideoFile(FileName: string; Index: Integer; ProgressDlg: TLoadProgressDialog);
var
  aThumb: TThumb;
  FailURI: string;
begin
  aThumb := Nil;
  // Returns empty string on success, else failed filename
  FailURI:= fThumbView.AddThumb(Path2URI(FileName), aThumb);
  if FailURI <> '' then
    fThumbFailList.Add(FailURI)
  else
    if MessageDlg('Video add fail', 'Failed to add ' + FileName + #13#10 +
      'Continue (Y/N)?', mtError, [mbYes, mbNo], 0) = mrNo then
       Exit;
  ProgressDlg.Progress := Index + 1;
  Application.ProcessMessages;
end;

procedure TMainForm.ReloadActionExecute(Sender: TObject);
begin
  fThumbView.RowNum := 0;
  fThumbView.LoadPageOfThumbs;
end;

// Use a temporary hidden TLabel to get height of wrapped text...
function TMainForm.GetCellHeight(aGrid: TStringGrid; ACol, ARow: Integer): Integer;
var
  aLabel: TLabel;
begin
  aLabel := TLabel.Create(Self);
  try
    with aLabel do begin
      Width := aGrid.ColWidths[1];
      Constraints.SetInterfaceConstraints(aGrid.ColWidths[1], 0, aGrid.ColWidths[1], 1000);
      Font := aGrid.Font;
      WordWrap := True;
      AutoSize := True;
      Caption := aGrid.Cells[ACol, ARow];
    end;
    Result := aLabel.Height
              + (aGrid.Font.Height * 3); //Extra whitespace
  finally
    aLabel.Free;
  end;
end;

procedure TMainForm.SearchEditExit(Sender: TObject);
begin
  fThumbView.TitleFilter := SearchEdit.Text;
  fThumbView.LoadPageOfThumbs;
  TEdit(Sender).SetFocus;
end;

function CompareKeyWords(Item1, Item2: Pointer): Integer;
var
  Keyword1, Keyword2: TSuggestedKeyword;
begin
  Keyword1 := TSuggestedKeyword(Item1);
  Keyword2 := TSuggestedKeyword(Item2);
  if Keyword1.SuggestedKeyword > Keyword2.SuggestedKeyword
  then Result := 1
  else if Keyword1.SuggestedKeyword = Keyword2.SuggestedKeyword
  then Result := 0
  else Result := -1;
end;

procedure TMainForm.SearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    SelectNext(Sender as TWinControl, True, True);
    Key := 0;
  end;
end;

procedure TMainForm.SetPlayerActionExecute(Sender: TObject);
begin
  PlayerPopupMenu.Popup;
end;

procedure TMainForm.ChangeParentalPinActionExecute(Sender: TObject);
var
  Dlg: TChangePINDialog;
begin
  Dlg := TChangePINDialog.Create(Nil);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function TMainForm.IsVideoFile(FileName: string): Boolean;
begin
  Result := False;
  Result := AnsiContainsStr(VideoFileTypes, ExtractFileExt(FileName));
end;

procedure TMainForm.SelectedThumbChanged(Sender: TObject; var ThumbIndex: Integer);
begin
  if ThumbIndex > 0 then begin
    TitleMemo.Text := fThumbView.Thumbs[ThumbIndex].Title;
    Application.ProcessMessages;
  end;
end;

procedure TMainForm.ThumbsNewRow(Sender: TObject; var Index: Integer);
begin
  fThumbView.ScrollPage(sdDown);
end;

procedure TMainForm.ThumbViewOnSelectThumb(Sender: TObject);
begin
  TitleMemo.Text := TThumb(Sender).Title;
end;

procedure TMainForm.CloseAnimationDialog;
begin
  fProcessAnimationDialog.CanClose := True;
  fProcessAnimationDialog.Close;
  Application.ProcessMessages;
end;

end.

