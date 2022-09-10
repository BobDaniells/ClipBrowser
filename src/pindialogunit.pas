unit PINDialogUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  MaskEdit;

type

  { TPINDialog }

  TPINDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    MaskEdit1: TMaskEdit;
    Panel1: TPanel;
  private

  public

  end;

var
  PINDialog: TPINDialog;

implementation

{$R *.lfm}

end.

