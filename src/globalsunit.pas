unit globalsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Mormot, Graphics, StrUtils, fileutil, Process,
  contnrs, dom, sax_html, dom_html, fphttpclient, LMessages, SynCommons,
  URIParser, FastHTMLParser,LoadProgressDialogUnit;

const
  ApplicationName = 'ClipBrowser';
  THUMB_WIDTH = 150;
  THUMB_HEIGHT = 102;
  CAPTION_HEIGHT = 95;
  DEFAULT_RATING = '2.5';

  FRAME_OFFSET_SECS = '10';
  BUF_SIZE = 2048;
  VideoFileTypes = '*.avi;*.asf;*.wmv;*.wma;*.mp4;*.mov;*.3gp;*.mkv;*.real;*.flv';
  FrameString = '00:00:10';
  ClipType = '.jpg';
  DefaultRating = '**   ';
  ClipFileOpenDialogFolder = './Youtube Downloads';
  DefaultRestriction = 'G';
  DefaultCategory = 'Default';
  YoutubeMetadataURL = 'https://noembed.com/embed';

type
  TFindAndLoadVideosThread = class(TThread)
  private
    fSearchPath: string;
    fAllVideos: TStringList;
    procedure FoundAllVideos;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
    property AllVideos: TStringList read fAllVideos;
    property SearchPath: string read fSearchPath write fSearchPath;
  end;

 var
   aModel: TSQLModel;
   Database: TSQLRest;
   ApplicationPath: string;
   DatabasePath: string;
   SelectedClipID: TID;
   SelectedThumbID: TID;
   ThumbnailFolder: string;
   DefaultPlayerIndex: Integer;
   ScrollPosition: Integer;
   ThumbRowCapacity: Integer;
   ThumbMaxRows: Integer;
   PlayListFileName: string;

function ProcessExecute(const AExecutable, AParameters: String; const AInput, AOutput, AStderr: TStream): Boolean;
procedure SetInitialPassword;
procedure ResetPassword;
function Path2URI(Path: RawUTF8): RawUTF8;
function URI2Path(URI: RawUTF8): RawUTF8;
function URIReachable(URI: string): Boolean;
procedure GetInstalledVideoPlayers;
function IsInstalled(Appname: string): Boolean;
procedure AddVideoApp(Index: Integer; AppName, DisplayName: string);
function Path2URI(Path: string): string;
function URI2Path(URI: string): string;
function IsFileURI(URI: string): Boolean;
function IsYoutubeURI(aURI: string): Boolean;

implementation

uses
  ModelUnit, thumbunit, ClipBrowserMain;

function ProcessExecute(const AExecutable, AParameters: String; const AInput, AOutput, AStderr: TStream): Boolean;
var
  LBuffer: array[word] of byte;
  LCount: longint;
begin
  Result := True;
  LBuffer[0] := 0;
  with TProcess.Create(nil) do begin
    try
      Options := [poUsePipes];
      ShowWindow := swoHIDE;
      Executable := AExecutable;
      Parameters.CommaText := AParameters;
      try
        Execute;
      except
        On E: Exception do begin
    //      ShowMessage('fail');
          Result := False;
        end;
      end;
      if Assigned(AInput) then begin
        while Running and (Input.Write(LBuffer, AInput.Read(LBuffer, SizeOf(LBuffer))) > 0) do begin
          Sleep(10);
        end;
      end;
      CloseInput;
      while Running or (Output.NumBytesAvailable > 0) or (Stderr.NumBytesAvailable > 0) do begin
        if Stderr.NumBytesAvailable > 0 then begin
          LCount := Stderr.Read(LBuffer, SizeOf(LBuffer));
          if Assigned(AStderr) then begin
            AStderr.Write(LBuffer, LCount);
          end;
        end;
        if Output.NumBytesAvailable > 0 then begin
          LCount := Output.Read(LBuffer, SizeOf(LBuffer));
          if Assigned(AOutput) then begin
            AOutput.Write(LBuffer, LCount);
          end;
        end;
        Sleep(10);
      end;
    finally
      Free;
    end;
  end;
end;

procedure SetInitialPassword;
var
  sw: TSomeone;
begin
  if Database.TableRowCount(TSomeOne) < 1 then begin
    sw := TSomeone.Create;
    try
      sw.SetPassword('0000');
      Database.Add(sw, True)
    finally
      sw.Free;
    end;
  end;
end;

procedure ResetPassword;
var
  sw: TSomeone;
  MinID: RawUTF8;
begin
  MinID := Database.OneFieldValue(TSomeOne, 'ID', '');
  sw := TSomeOne.Create(Database, MinID);
  try
    if Assigned(sw) then begin
      sw.SetPassword('0000');
      Database.Update(sw);
    end;
  finally
    sw.Free;
  end;
end;

function Path2URI(Path: RawUTF8): RawUTF8;
var
  aURI: TURI;
begin
  aURI.Document := Path;
  aURI.Protocol := 'file';
  aURI.Port := 0;
  Result := EncodeURI(aURI); //Correct result, works in browser.
end;

function URI2Path(URI: RawUTF8): RawUTF8;
var
  aURI: TURI;
begin
  aURI := ParseURI(URI, false);
  Result := AnsiQuotedStr(StringReplace(aURI.Path + aURI.Document, '%20', ' ', [rfReplaceAll]), '"');
end;

function URIReachable(URI: string): Boolean;
var
  Client: TFPHttpClient;
  FileName: string;
begin
  if Copy(URI, 1, 4) = 'file' then begin
    FileName := StringReplace(URI, '%20', ' ', [rfReplaceAll]);
    FileName := Copy(FileName, 8, Length(URI));
    Result := FileExists(FileName)
  end
  else begin
    Client := TFPHttpClient.Create(Nil);
    try
      try
        Result := Client.SimpleGet(URI) <> '';
      except
        on E:EHttpClient do
        ShowMessage(E.Message) else raise;
      end;
    finally
      Client.Free;
    end;
  end;
end;

function IsInstalled(Appname: string): Boolean; //TODO LINUX SPECIFIC (use where.bin for windows)
var
  Stream: TMemoryStream;
  SL: TStringList;
begin
  Stream := TMemoryStream.Create;
  SL := TStringList.Create;
  try
    Stream.Clear;
    {$IFDEF LINUX}
      ProcessExecute('which', Appname, nil, Stream, nil);
    {$ENDIF}
    {$IFDEF WINDOWS}
      ProcessExecute('where', Appname, nil, Stream, nil);
    {$ENDIF}
    SL.LoadFromStream(Stream);
    Result := SL.Text = '';
  finally
    SL.Free;
    Stream.Free;
  end;
end;

procedure GetInstalledVideoPlayers;
begin
  Database.Delete(TVideoPlayer, '', []);
  if IsInstalled('celluloid') then
    AddVideoApp(0, 'celluloid', 'Celluloid media player');
  if IsInstalled('vlc') then
    AddVideoApp(1, 'vlc', 'VLC media player');
  if IsInstalled('mpv') then
    AddVideoApp(2, 'mpv', 'MPV Media Player');
  if IsInstalled('xplayer') then
    AddVideoApp(3, 'xplayer', 'Media Player');
  if IsInstalled('smplayer') then
    AddVideoApp(4, 'smplayer', 'SM Player');
  if IsInstalled('firefox') then
    AddVideoApp(5, 'firefox', 'Firefox Web Browser');
  if IsInstalled('chromium-browser') then
    AddVideoApp(6, 'chromium-browser', 'Chrome Web Browser');
end;

procedure AddVideoApp(Index: Integer; AppName, DisplayName: string);
var
  plyr: TVideoPlayer;
  Data: TObjectList;
begin
  Data := Database.RetrieveList(TVideoPlayer, 'AppName=?', [AppName]); // Avoid duplicates
  if Data.Count < 1 then begin
    plyr := TVideoPlayer.Create;
    try
      plyr.Idx := Index;
      plyr.AppName := AppName;
      plyr.DisplayName := DisplayName;
      plyr.DefaultPlayer := False;
      if Index = 0 then
        plyr.DefaultPlayer := True; //Default to VLC
      Database.Add(plyr, True);
    finally
      plyr.Free;
    end;
  end;
end;

constructor TFindAndLoadVideosThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  Self.FreeOnTerminate := True;
  fAllVideos := TStringList.Create;
end;

destructor TFindAndLoadVideosThread.Destroy;
begin
  if Assigned(fAllVideos) then
    fAllVideos.Free;
end;

procedure TFindAndLoadVideosThread.Execute;
begin
  while not Terminated and (fSearchPath <> '') do begin
    try
      fAllVideos.Clear;
      FindAllFiles(fAllVideos, fSearchPath, VideoFileTypes, True);
    except
      ShowMessage('Video search failed at ' + SearchPath);
    end;
    Synchronize(@FoundAllVideos);
  end;
end;

procedure TFindAndLoadVideosThread.FoundAllVideos;
var
  i: Integer;
  fThumb: TThumb;
  ProgressDlg: TLoadProgressDialog;
begin
  ProgressDlg := TLoadProgressDialog.Create(Nil);
  ProgressDlg.NumClips := fAllVideos.Count;
  ProgressDlg.Show;
  Application.ProcessMessages;
  fThumb := Nil;
  for i := 0 to fAllVideos.Count-1 do begin
    MainForm.ThumbView.AddThumb(Path2URI(fAllVideos[i]), fThumb);
    ProgressDlg.StatusBar.SimpleText := 'Loading ' + fAllVideos[i];
    ProgressDlg.Progress := i + 1;
    Application.ProcessMessages;
  end;
  Self.Terminate;
  MainForm.UpdateClipsTotal;
end;

function Path2URI(Path: string): string;
begin
  Result := '';
  if Path <> '' then
    Result := 'file://' + Path;
end;

function URI2Path(URI: string): string;
begin
  Result := '';
  if URI <> '' then
    Result := Copy(URI, Length('file:///'), Length(URI));
end;

function IsFileURI(URI: string): Boolean;
begin
  Result := AnsiContainsStr(URI, 'file://');
end;

function IsYoutubeURI(aURI: string): Boolean;
begin
  Result := AnsiContainsStr(aURI, 'youtube');
end;

end.

