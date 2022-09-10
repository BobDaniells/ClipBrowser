unit TestDataUnit;

{ This object optionally populates an empty database with test URI's from
  Internet sites including YouTube.}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Dialogs, StrUtils, ModelUnit, globalsunit, thumbunit;

type
  TTestDataPopulator = class
  private
  public
    procedure Execute;
  end;

implementation

uses
  ClipBrowserMain;

procedure TTestDataPopulator.Execute;
var
  fYTTestClipData: array of string = ('https://www.youtube.com/watch?v=QSWnsPdhX8M',
                               'https://www.youtube.com/watch?v=rOjHhS5MtvA',
                               'https://www.youtube.com/watch?v=4591dCHe_sE',
                               'https://www.youtube.com/watch?v=NuqnpCzhEdM',
                               'https://www.youtube.com/watch?v=G92E3FWnHm0',
                               'https://www.youtube.com/watch?v=tT9Eh8wNMkw',
                               'https://www.youtube.com/watch?v=SSGH3tulpYc',
                               'https://www.youtube.com/watch?v=VWvNCC_cEZU',
                               'https://www.youtube.com/watch?v=23q3zoKiuGs',
                               'https://www.youtube.com/watch?v=eYVNZgnQ8gE',
                               'https://www.youtube.com/watch?v=lnWrbz4WjU0',
                               'https://www.youtube.com/watch?v=dSlDsup-aLI',
                               'https://www.youtube.com/watch?v=7E6HQkiocmY',
                               'https://www.youtube.com/watch?v=bvNZeh6f8vE',
                               'https://www.youtube.com/watch?v=apQRPEn-SbI',
                               'https://www.youtube.com/watch?v=po83lP0Sw1A',
                               'https://www.youtube.com/watch?v=Gz4IkzM217U',
                               'https://www.youtube.com/watch?v=-uXo7wtGW7M',
                               'https://www.youtube.com/watch?v=jbVfeGD5SnA',
                               'https://www.youtube.com/watch?v=WG8y2KBH0xY',
                               'https://www.youtube.com/watch?v=J_ALQl2MEkg',
                               'https://www.youtube.com/watch?v=taqMkdXL3Xg',
                               'https://www.youtube.com/watch?v=uqGvbVDp5Jk',
                               'https://www.youtube.com/watch?v=_TZ3EjSYWfY');
  FailURI: string;
  i, LinkID: Integer;
  aThumb: TThumb;
  aClip: TVideoClip;
  aDoc: TVideoDoc;
  aTitle: string;
begin
  // Load URIs of representative test videos from YouTube...
  for i := 0 to Length(fYTTestClipData) -1 do begin
    FailURI:= MainForm.ThumbView.AddThumb(fYTTestClipData[i], aThumb);
    if FailURI <> '' then
      if MessageDlg('Test Data Error', 'Failed to add YouTube URL' + #13#10 +
        fYTTestClipData[i] + #13#10 +
        'Continue (Y/N)', mtError, [mbYes, mbNo], 0) = mrNo then
        Exit;
  end;
  MainForm.UpdateClipsTotal;
  // Add some related documents like music scores & lyrics...
  for i := 0 to MainForm.ThumbView.Thumbs.Count - 1 do begin
    LinkID := TThumb(MainForm.ThumbView.Thumbs[i]).Clip.ID;
    aTitle := TThumb(MainForm.ThumbView.Thumbs[i]).Title;
    if AnsiContainsStr(aTitle, 'Moonlight') then begin
      aDoc := TVideoDoc.Create;
      try
        aDoc.ClipID := LinkID;
        aDoc.Title := 'Beethoven, Moonlight Sonata';
        aDoc.DocType := 'IMSLP Record';
        aDoc.URI := 'https://imslp.org/wiki/Piano_Sonata_No.13%2C_Op.27_No.1_(Beethoven%2C_Ludwig_van)';
        Database.Add(aDoc, True);
      finally
        aDoc.Free;
      end;
      aClip := TVideoClip.Create(Database, LinkID);
      try
        aClip.Description := 'Moonlight Sonata (piano)';
        aClip.Category := 'Music';
        aClip.Restriction := 'G';
        aClip.Attribution := 'YouTube';
        aClip.Rating := '4';
        aClip.Keywords := 'beethoven, moonlight, sonata, piano';
        Database.Update(aClip);
      finally
        aClip.Free;
      end;
    end else if AnsiContainsStr(aTitle, 'Song to the Moon') then begin
      aDoc := TVideoDoc.Create;
      try
        aDoc.ClipID := LinkID;
        aDoc.Title := 'Dvorak, Song to the Moon, from opera Rusalka';
        aDoc.DocType := 'Description with lyrics';
        aDoc.URI := 'https://www.liveabout.com/song-to-the-moon-lyrics-and-text-translation-724031';
        Database.Add(aDoc, True);
      finally
        aDoc.Free;
      end;
    end else if AnsiContainsStr(aTitle, 'Take Five') then begin
      aDoc := TVideoDoc.Create;
      try
        aDoc.ClipID := LinkID;
        aDoc.Title := 'Dave Brubeck, Take Five';
        aDoc.DocType := 'Piano Sheet Music';
        aDoc.URI := 'https://musescore.com/static/musescore/scoredata/gen/1/1/5/171511/a6a76f22ca9b902b635ff37e2ee3bf6b66d138be/score_0.png@850x1100?no-cachee=1561379722';
        Database.Add(aDoc, True);
      finally
        aDoc.Free;
      end;
    end;
  end;
end;

end.

