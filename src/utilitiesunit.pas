unit utilitiesunit;

{$mode objfpc}{$H+}

{ Various Linux-specific utilities. }

interface

uses
  Classes, SysUtils, Dialogs, Process, forms, contnrs, SynCommons, mORMot, fileutil,
  globalsunit, modelunit, LoadProgressDialogUnit;

type
  TKnownPlayer = record
    AppName: RawUTF8;
    DisplayName: RawUTF8;
  end;

type
  TLinuxUtilities = class(TObject)
  private
    fStream: TMemoryStream;
    fUser: string;
  public
 //   function GetUserHomePath: string;
 //   function GetUser: string;
 //   function GetThumbCachePath: string;
 //   function IsInstalled(Appname: string): Boolean;
 //   procedure AddVideoApp(Index: Integer; AppName, DisplayName: string);
 //   procedure GetInstalledVideoPlayers;
 //   function FindAndLoadVideos: TStringList;
//    function URIReachable(URI: string): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

{  TFindAndLoadVideosThread = class(TThread)
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
  end; }

implementation

uses
  ClipBrowserMain, thumbunit;

constructor TLinuxUtilities.Create;
begin
  inherited Create;
  fStream := TMemoryStream.Create;
end;

destructor TLinuxUtilities.Destroy;
begin
  if Assigned(fStream)
    then fStream.Free;
  inherited Destroy;
end;

{function TLinuxUtilities.GetUser: string;
var
  SL: TStringList;
  aStream: TMemoryStream;
begin
  Result := '';
  aStream := TMemoryStream.Create;
  ProcessExecute('whoami', ',1,pipe:1', Nil, aStream, Nil);
  aStream.Position := 0;
  SL := TStringList.Create;
  try
    SL.LoadFromStream(aStream);
    Result := Copy(SL.Text, 1, Length(SL.Text)-1);
    fUser := Result;
  finally
    aStream.Free;
    SL.Free;
  end;
end;

function TLinuxUtilities.GetUserHomePath: string;
begin
  Result := '/home/' + GetUser;
end;

function TLinuxUtilities.GetThumbCachePath: string;
begin
  fStream.Clear;
  Result := '/home/' + GetUser + '/.cache/thumbnails';
end;}

{procedure TLinuxUtilities.GetInstalledVideoPlayers;
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

function TLinuxUtilities.IsInstalled(Appname: string): Boolean;
var
  Stream: TMemoryStream;
  SL: TStringList;
begin
  Stream := TMemoryStream.Create;
  SL := TStringList.Create;
  try
    Stream.Clear;
    ProcessExecute('which', Appname, nil, Stream, nil);
    SL.LoadFromStream(Stream);
    Result := SL.Text = '';
  finally
    SL.Free;
    Stream.Free;
  end;
end;

procedure TLinuxUtilities.AddVideoApp(Index: Integer; AppName, DisplayName: string);
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

function TLinuxUtilities.FindAndLoadVideos: TStringList;
var
  params: string;
  SL: TStringList;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  SL := TStringList.Create;
  try
    params := '., -type f, -exec file, -N, -i, --, {} + | grep video';
    ProcessExecute('find', params, Nil, Stream, Nil);
    Stream.Position := 0;
    SL.LoadFromStream(Stream);
    Result := SL;
  finally
    Stream.Free;
  end;
end; }

{function TLinuxUtilities.URIReachable(URI: string): Boolean;
var
  params: string;
  SL: TStringList;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  SL := TStringList.Create;
  try
    params := '--head ' + URI;
    ProcessExecute('curl', params, Nil, Stream, Nil);
    Stream.Position := 0;
    SL.LoadFromStream(Stream);
    Result := SL.Text <> '';
  finally
    SL.Free;
    Stream.Free;
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
    MainForm.ThumbView.AddThumb(fAllVideos[i], fThumb);
    ProgressDlg.Progress := i + 1;
    Application.ProcessMessages;
  end;
  Self.Terminate;
  MainForm.TotalClipsLabel.Caption := 'Total Clips: ' + IntToStr(fAllVideos.Count);
end; }

end.

