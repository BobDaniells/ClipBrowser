unit MediaPlayerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DBCtrls, Buttons, ComCtrls, PasLibVlcPlayerUnit;

type

  { TVideoPlayer }

  TVideoPlayer = class(TForm)
    ControlsPanel: TPanel;
    PlaySpeedButton: TSpeedButton;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    VlcPlayer: TPasLibVlcPlayer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fVideoFilePath: string;
    procedure SetVideoFilePath(Value: string);
  public
    property VideoFilePath: string read fVideoFilePath write SetVideoFilePath;
  end;

var
  VideoPlayer: TVideoPlayer;

implementation

{$R *.lfm}

{ TVideoPlayer }

procedure TVideoPlayer.FormCreate(Sender: TObject);
begin
end;

procedure TVideoPlayer.FormShow(Sender: TObject);
begin
  vlcPlayer.Play(WideString(Path2URI('/media/bob/DATA/Youtube%20Downloads/%5B7.1ch%5D%20Woody%20-%20Skyfall%20(James%20Bond%20007%20pipe%20organ%20transcription).mp4')));
end;

procedure TVideoPlayer.SetVideoFilePath(Value: string);
begin
  fVideoFilePath := Value;
end;

end.

