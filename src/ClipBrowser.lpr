program ClipBrowser;

{$mode objfpc}{$H+}

uses
  {$IFDEF LINUX}
    cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ClipBrowserMain, globalsunit, PasLibVlcPlayer,
  editclassificationdialogunit, ClassificationFilterUnit, ChangePINUnit,
  TestDataUnit, userunit, clipparametersunit;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEditClassificationDialog, EditClassificationDialog);
  Application.CreateForm(TClassificationsFilterDialog,
    ClassificationsFilterDialog);
  Application.CreateForm(TChangePINDialog, ChangePINDialog);
  Application.CreateForm(TParametersDialog, ParametersDialog);
  Application.Run;
end.

