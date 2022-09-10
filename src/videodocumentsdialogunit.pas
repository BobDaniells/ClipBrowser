unit videodocumentsdialogunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Contnrs, Menus, DBCtrls, ComCtrls, Buttons, SynCommons,
  mORMot, globalsunit, modelunit;

type
  TDBMode = (dmBrowse, dmEdit, dmInsert, dmDelete, dmSearch);

  { TAssociatedDocumentsDialog }

  TAssociatedDocumentsDialog = class(TForm)
    CloseButton: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    sbBrowse: TSpeedButton;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    BrowseListBox: TListBox;
    PageControl1: TPageControl;
    sbInsert1: TSpeedButton;
    sbURI1: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    BrowseTabSheet: TTabSheet;
    DetailsTabSheet: TTabSheet;
    sbSearch: TSpeedButton;
    TitleEdit: TEdit;
    TypeEdit: TEdit;
    BottomPanel: TPanel;
    ButtonPanel: TPanel;
    AddMenuItem: TMenuItem;
    DocumentOpenDialog: TOpenDialog;
    RemoveMenuItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    URIEdit: TEdit;
    procedure AddMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BrowseListBoxDblClick(Sender: TObject);
    procedure BrowseListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure BrowseListBoxShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure RemoveMenuItemClick(Sender: TObject);
    procedure sbBrowseClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
    procedure sbSearchClick(Sender: TObject);
    procedure sbURIClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbEditClick(Sender: TObject);
    procedure sbPostClick(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fID: TID;
    fClip: TVideoClip;
    fDBMode: TDBMode;
    function SetDBMode(Value: TDBMode): TDBMode;
    procedure SetDetailsControls(DBMode: TDBMode);
    procedure Refresh;
    procedure PopulateEdits;
  public
    property ID: TID read fID write fID;
  end;

var
  AssociatedDocumentsDialog: TAssociatedDocumentsDialog;

implementation

{$R *.lfm}

{ TAssociatedDocumentsDialog }

function TAssociatedDocumentsDialog.SetDBMode(Value: TDBMode): TDBMode;
begin
  case Value of
    dmBrowse: Result := dmBrowse;
    dmInsert: Result := dmInsert;
    dmEdit: Result := dmEdit;
    dmDelete: Result := dmDelete;
    dmSearch: Result := dmSearch;
  end;
  SetDetailsControls(Value);
end;

procedure TAssociatedDocumentsDialog.SetDetailsControls(DBMode: TDBMode);
var
  i: Integer;
  aControl: TControl;
begin
  for i := 0 to DetailsTabSheet.ControlCount-1 do begin
    aControl :=  DetailsTabSheet.Controls[i];
    if aControl is TEdit then begin
      case DBMode of
        dmBrowse: begin
          TEdit(aControl).ReadOnly := True;
          aControl.Color := clDefault;
        end;
        dmInsert: begin
          TEdit(aControl).Clear;
          TEdit(aControl).ReadOnly := False;
          aControl.Color := clLime;
        end;
        dmEdit: begin
          TEdit(aControl).ReadOnly := False;
          aControl.Color := clYellow;
        end;
        dmDelete: begin
          TEdit(aControl).ReadOnly := False;
          aControl.Color := clRed;
        end;
        dmSearch: begin
          TEdit(aControl).ReadOnly := False;
          aControl.Color := clAqua;
        end;
      end;
    end;
  end;
end;

procedure TAssociatedDocumentsDialog.AddMenuItemClick(Sender: TObject);
var
  fName: String;
  fURI: RawUTF8;
  aVideoDoc: TVideoDoc;
begin
  if DocumentOpenDialog.Execute then begin
    fName := DocumentOpenDialog.FileName;
    if fName <> '' then begin
      fURI := Path2URI(fName);
      if BrowseListBox.Items.IndexOf(fURI) <> 0 then begin//Avoid duplicates
        aVideoDoc := TVideoDoc.Create;
        aVideoDoc.URI := fURI;
        Database.Add(aVideoDoc, True);
        BrowseListBox.Items.Add(fURI);
      end;
    end;
  end;
end;

procedure TAssociatedDocumentsDialog.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if Assigned(fClip) then
    fClip.Free;
  CanClose := True;
end;

procedure TAssociatedDocumentsDialog.FormCreate(Sender: TObject);
begin
  fDBMode := dmBrowse;
  PageControl1.ActivePage := BrowseTabSheet;
end;

procedure TAssociatedDocumentsDialog.FormShow(Sender: TObject);
begin
  Refresh;
  if BrowseListBox.Count > 0 then
    BrowseListBox.ItemIndex := 0;
end;

procedure TAssociatedDocumentsDialog.Refresh;
var
  aDoc: TVideoDoc;
  i: Integer;
  List: TObjectList;
begin
  BrowseListBox.Clear;
  List := Database.RetrieveList(TVideoDoc, 'ClipID=?', [fID]);
  for i := 0 to List.Count-1 do
    BrowseListBox.Items.AddObject(TVideoDoc(List[i]).URI, TVideoDoc(List[i]));
end;

procedure TAssociatedDocumentsDialog.BrowseListBoxDblClick(Sender: TObject);
var
  Doc: TVideoDoc;
  URI: string;
begin
  Doc := TVideoDoc(TListBox(Sender).Items.Objects[TListBox(Sender).ItemIndex]);
  URI := Doc.URI;
  ProcessExecute('xdg-open', URI, Nil, Nil, Nil);
end;

procedure TAssociatedDocumentsDialog.BrowseListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  PopulateEdits;
end;

procedure TAssociatedDocumentsDialog.PopulateEdits;
var
  Doc: TVideoDoc;
begin
  Doc := TVideoDoc(BrowseListBox.Items.Objects[BrowseListBox.ItemIndex]);
  TitleEdit.Text := Doc.Title;
  TypeEdit.Text := Doc.DocType;
  URIEdit.Text := Doc.URI;
end;

procedure TAssociatedDocumentsDialog.BrowseListBoxShowHint(Sender: TObject;
  HintInfo: PHintInfo);
begin
  if (HintInfo^.HintControl = BrowseListBox) and (BrowseListBox.ItemAtPos(HintInfo^.CursorPos, True) > -1) then
  begin
      HintInfo^.HintStr := BrowseListBox.Items[BrowseListBox.ItemAtPos(HintInfo^.CursorPos, True)];
      HintInfo^.CursorRect := BrowseListBox.ItemRect(BrowseListBox.ItemAtPos(HintInfo^.CursorPos, True));
  end;
end;

procedure TAssociatedDocumentsDialog.RemoveMenuItemClick(Sender: TObject);
begin

end;

procedure TAssociatedDocumentsDialog.sbBrowseClick(Sender: TObject);
begin
  fDBMode := SetDBMode(dmBrowse);
end;

procedure TAssociatedDocumentsDialog.sbInsertClick(Sender: TObject);
begin
  fDBMode := SetDBMode(dmInsert);
end;

procedure TAssociatedDocumentsDialog.sbSearchClick(Sender: TObject);
begin
  fDBMode := SetDBMode(dmSearch);
end;

procedure TAssociatedDocumentsDialog.sbURIClick(Sender: TObject);
var
  fname: string;
  fURI: RawUTF8;
begin
  fDBMode := SetDBMode(dmInsert);
  if DocumentOpenDialog.Execute then begin
    fName := DocumentOpenDialog.FileName;
    if fName <> '' then begin
      fURI := Path2URI(fName);
      URIEdit.Text := fURI;
    end;
  end;
end;

procedure TAssociatedDocumentsDialog.sbDeleteClick(Sender: TObject);
begin
  fDBMode := SetDBMode(dmDelete);
end;

procedure TAssociatedDocumentsDialog.sbEditClick(Sender: TObject);
begin
  fDBMode := SetDBMode(dmEdit);
end;

procedure TAssociatedDocumentsDialog.sbPostClick(Sender: TObject);
var
  aVideoDoc: TVideoDoc;
  DestID: TID;
begin
  if (fDBMode = dmInsert) or (fDBMode = dmEdit) then begin
    if fDBMode = dmInsert then begin
      aVideoDoc := TVideoDoc.Create;
      aVideoDoc.ClipID := fID;
      aVideoDoc.Title := TitleEdit.Text;
      aVideoDoc.DocType := TypeEdit.Text;
      aVideoDoc.URI := URIEdit.Text;
      DestID := Database.Add(aVideoDoc, True)
    end else begin
      aVideoDoc := TVideoDoc(BrowseListBox.Items.Objects[BrowseListBox.ItemIndex]);
      aVideoDoc.Title := TitleEdit.Text;
      aVideoDoc.DocType := TypeEdit.Text;
      aVideoDoc.URI := URIEdit.Text;
      Database.Update(aVideoDoc);
    end;
  end;
  fDBMode := SetDBMode(dmBrowse);
  Refresh;
end;

procedure TAssociatedDocumentsDialog.EditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if fDBMode <> dmInsert then
    fDBMode := SetDBMode(dmEdit);
end;

end.

