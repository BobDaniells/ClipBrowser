unit ModelUnit;

{$mode objfpc}{$H+}

{ This unit contains all objects that are persisted in the database.
  It uses a Synopse mORMot interface to a SQLite3 database. }

interface

uses
  Classes, Dialogs, Graphics, Math, SynCommons, SysUtils, StrUtils, URIParser, MD5, BGRABitmap, BGRAGraphicControl,
  BGRABitmapTypes, mORMot, SynCrypto, Process, dom, sax_html,
  dom_html, fphttpclient, globalsunit, debugdialogunit;

const
  SALT = 'gvideobrowser';

type
  TVideoProps = record
    Description, Keywords, Width, Height, Duration, PublishedDate: RawUTF8;
  end;

  TVideoClip = class(TSQLRecordTimed)
  private
    fClipURI: RawUTF8;
    fThumbnailFilePath: RawUTF8;
    fTitle: RawUTF8;
    fDescription: RawUTF8;
    fAttribution: RawUTF8;
    fDuration: RawUTF8;
    fCategory: RawUTF8;
    fRestriction: RawUTF8;
    fRating: RawUTF8;
    fKeywords: RawUTF8;
    fYoutube_Keywords: RawUTF8;
    fWidth: RawUTF8;
    fHeight: RawUTF8;
    fPublishedDate: RawUTF8;
    fPathError: RawUTF8;
    procedure SetClipURI(Value: RawUTF8);
    function Path2URI(Path: RawUTF8): RawUTF8;
  public
    constructor Create; override;
    procedure GetVideoParameters(Path: String; out Props: TVideoProps);
    procedure GetFileVideoParameters(Path: String; out Props: TVideoProps);
    procedure GetYoutubeVideoParameters(Path: String; out Props: TVideoProps);
    property ID: TID read fID;
    property PathError: RawUTF8 read fPathError write fPathError;
  published
    property ClipURI: RawUTF8 read fClipURI write SetClipURI;
    property ThumbnailFilePath: RawUTF8 read fThumbnailFilePath write fThumbnailFilePath;
    property Title: RawUTF8 read fTitle write fTitle;
    property Description: RawUTF8 read fDescription write fDescription;
    property Attribution: RawUTF8 read fAttribution write fAttribution;
    property Duration: RawUTF8 read fDuration write fDuration;
    property PublishedDate: RawUTF8 read fPublishedDate write fPublishedDate;
    property Category: RawUTF8 read fCategory write fCategory;
    property Restriction: RawUTF8 read fRestriction write fRestriction;
    property Rating: RawUTF8 read fRating write fRating;
    property Keywords: RawUTF8 read fKeywords write fKeywords;
    property Youtube_Keywords: RawUTF8 read fYoutube_Keywords write fYoutube_Keywords;
    property Width: RawUTF8 read fWidth write fWidth;
    property Height: RawUTF8 read fHeight write fHeight;
  end;

  TSuggestedKeyword = class(TSQLRecordTimed)
  private
   fSuggestedKeyword: RawUTF8;
  published
    property SuggestedKeyword: RawUTF8 read fSuggestedKeyword write fSuggestedKeyword;
  end;

  TVideoPlayer = class(TSQLRecordTimed)
  private
    fIdx: Integer;
    fAppName: RawUTF8;
    fDisplayName: RawUTF8;
    fDefaultPlayer: Boolean;
  published
    property Idx: Integer read fIdx write fIdx;
    property AppName: RawUTF8 read fAppName write fAppName;
    property DisplayName: RawUTF8 read fDisplayName write fDisplayName;
    property DefaultPlayer: Boolean read fDefaultPlayer write fDefaultPlayer;
  end;

  TSomeOne = class(TSQLRecordTimed)
  private
    fPassword: RawUTF8;
    fHashedPassword: RawUTF8;
  public
   procedure SetPassword(Password: RawUTF8);
   function CheckPassword(aPassword: RawUTF8): Boolean;
  published
    property HashedPassword: RawUTF8 read fHashedPassword write fHashedPassword;
  end;

  TCategory = class(TSQLRecordTimed)
  private
    fCategory: RawUTF8;
  public
    class procedure AddDefaultCategories;
  published
    property Category: RawUTF8 read fCategory write fCategory;
  end;

  // 1 to many relationship from TVideoClip
  TVideoDoc = class(TSQLRecordTimed)
  private
    fClipID: TID;
    fTitle: RawUTF8;
    fDocType: RawUTF8;
    fURI: RawUTF8;
  published
    property ClipID: TID read fClipID write fClipID;
    property Title: RawUTF8 read fTitle write FTitle;
    property DocType: RawUTF8 read fDocType write fDocType;
    property URI: RawUTF8 read fURI write fURI;
  end;

  function CreateDataModel: TSQLModel;

implementation

constructor TVideoClip.Create;
begin
  inherited Create;
  fPathError := '';
end;

procedure TVideoClip.SetClipURI(Value: RawUTF8);
begin
  if fClipURI <> Value then
    fClipURI := Value;
end;

function TVideoClip.Path2URI(Path: RawUTF8): RawUTF8;
var
  aURI: TURI;
begin
  aURI.Document := Path;
  aURI.Protocol := 'file';
  aURI.Port := 0;
  Result := EncodeURI(aURI);
end;

procedure TVideoClip.GetVideoParameters(Path: String; out Props: TVideoProps);
begin
  if Copy(Path, 1, 4) = 'file' then
    GetFileVideoParameters(Path, Props)
  else if IsYoutubeURI(Path) then
    GetYoutubeVideoParameters(Path, Props);
end;

procedure TVideoClip.GetFileVideoParameters(Path: String; out Props: TVideoProps);
var
  Buffer: array[1..BUF_SIZE] of byte;
  aProcess: TProcess;
  OutputStream: TStream;
  BytesRead: Integer;
  SL: TStringList;
  i: Integer;
begin
  aProcess := TProcess.Create(Nil);
  aProcess.Options := [poUsePipes];
  aProcess.Executable := 'ffprobe';
  try
    aProcess.Parameters.Add('-v');
    aProcess.Parameters.Add('error');
    aProcess.Parameters.Add('-show_format');
    aProcess.Parameters.Add('-show_streams');
    aProcess.Parameters.Add('-i');
    aProcess.Parameters.Add(Path);
    aProcess.Execute;
    OutputStream := TMemoryStream.Create;
    repeat
      BytesRead := aProcess.Output.Read(Buffer, BUF_SIZE);
      OutputStream.Write(Buffer, BytesRead);
    until BytesRead = 0;
    OutputStream.Position := 0;
    SL := TStringList.Create;
    try
      SL.LoadFromStream(OutputStream);
      for i := 0 to SL.Count -1 do begin
        case SL.Names[i] of
          'width': Props.Width := SL.Values['width'];
          'height': Props.Height := SL.Values['height'];
          'duration': Props.Duration := SL.Values['duration'];
        end;
      end;
    finally
      SL.Free;
    end;
  finally
    OutputStream.Free;
    aProcess.Free;
  end;
end;

procedure TVideoClip.GetYoutubeVideoParameters(Path: String; out Props: TVideoProps);
var
  Client: TFPHttpClient;
  Doc: THTMLDocument;
  stream: TStringStream;
  MetaNodes: TDomNodeList;
  n: Integer;
  s: string;
begin
  Client := TFPHttpClient.Create(Nil);
  Doc := THTMLDocument.Create;
  try
    s := Client.Get(Path);
    stream := TStringStream.Create(s);
    try
      ReadHTMLFile(Doc, stream);
      MetaNodes := Doc.GetElementsByTagName('meta');
      for n := 0 to MetaNodes.Length-1 do begin //70
        case TDomElement(MetaNodes[n]).GetAttribute('property') of
          'og:video:width': Props.Width := TDomElement(MetaNodes[n]).GetAttribute('content');
          'og:video:height': Props.Height := TDomElement(MetaNodes[n]).GetAttribute('content');
        end;
        case TDomElement(MetaNodes[n]).GetAttribute('itemprop') of
          'duration': Props.Duration := TDomElement(MetaNodes[n]).GetAttribute('content');
          'datePublished': Props.PublishedDate := TDomElement(MetaNodes[n]).GetAttribute('content');
        end;
        case TDomElement(MetaNodes[n]).GetAttribute('name') of
          'description': Props.Description := TDomElement(MetaNodes[n]).GetAttribute('content');
          'keywords': Props.Keywords := TDomElement(MetaNodes[n]).GetAttribute('content');
        end;
      end;
    finally
      stream.Free;
    end;
  finally
    Doc.Free;
    Client.Free;
  end;
end;

procedure TSomeOne.SetPassword(Password: RawUTF8);
begin
  if Password <> '' then begin
    fPassword := Password;
    fHashedPassword := SHA256(SALT + fPassword);
  end;
end;

function TSomeOne.CheckPassword(aPassword: RawUTF8): Boolean;
var
  sw: TSomeOne;
  MinSomeoneID: RawUTF8;
begin
  MinSomeoneID := Database.OneFieldValue(TSomeOne, 'MIN(ID)', '');
  sw := TSomeOne.Create(Database, MinSomeoneID);
  try
    Result := sw.HashedPassword = SHA256(SALT + aPassword);
  finally
    sw.Free;
  end;
end;

class procedure TCategory.AddDefaultCategories;
var
  fCat: TCategory;
begin
  if Database.TableRowCount(TCategory) < 1 then begin;
    fCat := TCategory.Create;
    try
      fCat.Category := 'Default';
      Database.Add(fCat, True);
      fCat.Category := 'Music';
      Database.Add(fCat, True);
      fCat.Category := 'How To';
      Database.Add(fCat, True);
      fCat.Category := 'Pets';
      Database.Add(fCat, True);
    finally
      fCat.Free;
    end;
  end;
end;

function CreateDataModel: TSQLModel;
begin
  Result := TSQLModel.Create([TVideoClip, TVideoDoc, TSuggestedKeyword,
            TVideoPlayer, TSomeone, TCategory]);
  TVideoClip.AddFilterNotVoidText(['Title']);
end;

end.

