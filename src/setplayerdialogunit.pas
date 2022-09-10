unit SetPlayerDialogUnit;

{$mode objfpc}{$H+}

{ Select a default player for your videos. }

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, CheckLst,
  StdCtrls;

type

  { TSetPlayerDialog }

  TSetPlayerDialog = class(TForm)
    CancelButton: TButton;
    SetButton: TButton;
    CheckListBox1: TCheckListBox;
    ScrollBox1: TScrollBox;
  private
    fPlayers: TStringList;
    procedure SetPlayers(Value: TStringList);
  public
    property Players: TStringList read fPlayers write SetPlayers;
  end;

var
  SetPlayerDialog: TSetPlayerDialog;

implementation

{$R *.lfm}

procedure TSetPlayerDialog.SetPlayers(Value: TStringList);
var
  i: Integer;
begin
  if fPlayers <> Value then begin
    fPlayers := Value;
    for i := 0 to fPlayers.Count-1 do
      CheckListBox1.AddItem(fPlayers[i], nil);
  end;
end;

end.

