unit KeywordFilterUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TAdvancedFilterDialog }

  TAdvancedFilterDialog = class(TForm)
    ImageList1: TImageList;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    rgFilterWhat: TRadioGroup;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
  private

  public

  end;

var
  AdvancedFilterDialog: TAdvancedFilterDialog;

implementation

{$R *.lfm}

end.

