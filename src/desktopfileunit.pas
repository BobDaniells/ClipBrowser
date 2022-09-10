unit DesktopFileUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Process, globalsunit;

type
  TDesktopFile = class
  private
    fContent: TStringList;
    fTempFile: string;
    fTargetPath: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Content: TStringList read fContent;
  end;

implementation

constructor TDesktopFile.Create;
var
  aProcess: TProcess;
begin
  fTempFile := ApplicationPath + '.desktop';
  fTargetpath := 'admin:///usr/share/applications/' + ApplicationName + '.desktop';
  fContent := TStringList.Create;
  with fContent do begin
    Add('[Desktop Entry]');
    Add('Encoding=UTF-8');
    Add('Version=0.9.1.1');
    Add('Type=Application');
    Add('Terminal=false');
    Add('Exec=' + ApplicationPath);
    Add('Name=' + ApplicationName);
    Add('Icon=' + ApplicationPath + '.ico');
    SaveToFile(fTempFile);
  end;
  aProcess := TProcess.Create(nil);
  try
    aProcess.Executable := '/usr/bin/mv';
    aProcess.Parameters.Add(fTempFile);
    aProcess.Parameters.Add(fTargetPath);
    aProcess.Execute;
    ShowMessage(IntToStr(aProcess.ExitCode));
  finally
    AProcess.Free;
  end;
end;

destructor TDesktopFile.Destroy;
begin
  if Assigned(fContent) then
  fContent.Free;


end;

end.

