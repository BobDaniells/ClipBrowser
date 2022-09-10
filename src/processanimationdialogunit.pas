unit ProcessAnimationDialogUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRASpriteAnimation;

type

  { TProcessAnimationDialog }

  TProcessAnimationDialog = class(TForm)
    BGRASpriteAnimation1: TBGRASpriteAnimation;
    ActionLabel: TLabel;
    procedure BGRASpriteAnimation1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    fCanClose: Boolean;
  public
    property CanClose: Boolean read fCanClose write fCanClose;
  end;

var
  ProcessAnimationDialog: TProcessAnimationDialog;

implementation

{$R *.lfm}

{ TProcessAnimationDialog }

procedure TProcessAnimationDialog.FormShow(Sender: TObject);
begin
  BGRASpriteAnimation1.AnimRepeat := 0;
  BGRASpriteAnimation1.AnimStatic := False;
end;

procedure TProcessAnimationDialog.BGRASpriteAnimation1Click(Sender: TObject);
begin

end;

procedure TProcessAnimationDialog.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := fCanClose;
end;

end.

