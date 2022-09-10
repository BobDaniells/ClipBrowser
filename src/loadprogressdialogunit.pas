unit LoadProgressDialogUnit;

{$mode objfpc}{$H+}

{ Dialog to show clip load progress }

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  BGRAFlashProgressBar;

type

  { TLoadProgressDialog }

  TLoadProgressDialog = class(TForm)
    BGRAFlashProgressBar1: TBGRAFlashProgressBar;
    CloseButton: TButton;
    CountLabel: TLabel;
    MaxLabel: TLabel;
    StatusBar: TStatusBar;
    procedure CloseButtonClick(Sender: TObject);
  private
    fNumClips: Integer;
    fProgress: Integer;
    procedure SetNumClips(Value: Integer);
    procedure SetProgress(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    property NumClips: Integer read fNumClips write SetNumClips;
    property Progress: Integer read fProgress write SetProgress;
  end;

var
  LoadProgressDialog: TLoadProgressDialog;

implementation

{$R *.lfm}

constructor TLoadProgressDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BGRAFlashProgressBar1.Value := 0;
end;

procedure TLoadProgressDialog.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TLoadProgressDialog.SetNumClips(Value: Integer);
begin
  if fNumClips <> Value then begin
    fNumClips := Value;
    BGRAFlashProgressBar1.MinValue := 0;
    BGRAFlashProgressBar1.MaxValue := Value;
    MaxLabel.Caption := IntToStr(Value);
  end;
end;

procedure TLoadProgressDialog.SetProgress(Value: Integer);
begin
  if fProgress <> Value then begin
    fProgress := Value;
    BGRAFlashProgressBar1.Value := Value;
    BGRAFlashProgressBar1.Invalidate;
    CountLabel.Caption := 'Loaded Count = ' + IntToStr(Value);
  end;
end;

end.

