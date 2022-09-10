unit debugdialogunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TDebugDialog }

  TDebugDialog = class(TForm)
    CloseButton: TButton;
    DebugMemo: TMemo;
    Panel1: TPanel;
  private

  public

  end;

var
  DebugDialog: TDebugDialog;

implementation

{$R *.lfm}

end.

