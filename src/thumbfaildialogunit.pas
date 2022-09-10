unit ThumbFailDialogUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TThumbFailDialog }

  TThumbFailDialog = class(TForm)
    ErrorMemo: TMemo;
    OKButton: TButton;
    HeadingMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    fErrorList: TStringList;
    procedure SetErrorList(Value: TStringList);
  public
    property ErrorList: TStringList read fErrorList write SetErrorList;
  end;

var
  ThumbFailDialog: TThumbFailDialog;

implementation

{$R *.lfm}

{ TThumbFailDialog }

procedure TThumbFailDialog.FormCreate(Sender: TObject);
begin
  HeadingMemo.Clear;
  ErrorMemo.Clear;
  HeadingMemo.Lines.Add('Video files at the following locations were NOT LOADED as they are not accessible...');
  HeadingMemo.Lines.Add('');
  HeadingMemo.Lines.Add('Possible causes include:');
  HeadingMemo.Lines.Add('  1. The storage devices containing these video files have not been "mounted" in Linux.');
  HeadingMemo.Lines.Add('  2. The relevant video files are corrupted or truncated.');
end;

procedure TThumbFailDialog.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TThumbFailDialog.SetErrorList(Value: TStringList);
var
  i: Integer;
begin
  if fErrorList <> Value then begin
    fErrorList := Value;
    for i := 0 to fErrorList.Count-1 do begin
      ErrorMemo.Lines.Add(fErrorList[i]);
      ErrorMemo.Lines.Add('');
    end;
  end;
end;

end.

