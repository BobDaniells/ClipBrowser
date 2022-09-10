unit ratingdialogunit;

{$mode objfpc}{$H+}

{ Rate your videos, zero to 5 stars. }

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  BCLabel, mORMot, ModelUnit, globalsunit;

type

  { TRatingDialogForm }

  TRatingDialogForm = class(TForm)
    BCLabelFour: TBCLabel;
    BCLabelFive: TBCLabel;
    BCLabelTwo: TBCLabel;
    BCLabelThree: TBCLabel;
    BCLabelZero: TBCLabel;
    BCLabelOne: TBCLabel;
    CancelButton: TButton;
    RatingLabel: TLabel;
    SaveButton: TButton;
    TrackBar1: TTrackBar;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    fID: TID;
    procedure SetID(Value: TID);
  public
    property ID: TID read fID write SetID;
  end;

var
  RatingDialogForm: TRatingDialogForm;

implementation

{$R *.lfm}

{ TRatingDialogForm }

procedure TRatingDialogForm.SaveButtonClick(Sender: TObject);
var
  aVideoClip: TVideoClip;
begin
  aVideoClip := TVideoClip.Create(Database, fID);
  try
    aVideoClip.Rating := FloatToStr(TrackBar1.Position / 100);
    Database.Update(aVideoClip);
  finally
    aVideoClip.Free;
  end;
  Close;
end;

procedure TRatingDialogForm.TrackBar1Change(Sender: TObject);
begin
  RatingLabel.Caption := FloatToStr(TrackBar1.Position / 100);
end;

procedure TRatingDialogForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TRatingDialogForm.FormCreate(Sender: TObject);
begin
end;

procedure TRatingDialogForm.SetID(Value: TID);
var
  aVideoClip: TVideoClip;
begin
  if fID <> Value then begin
    fID := Value;
    aVideoClip := TVideoClip.Create(Database, fID);
    try
      if aVideoClip.Rating <> '' then
        Trackbar1.Position := Round(StrToFloat(aVideoClip.Rating) * 100);
    finally
      aVideoClip.Free;
    end;
  end;
end;

end.

