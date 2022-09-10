unit userunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, globalsunit;

type
  TUser = class
  private
    fName: string;
    fHomePath: string;
    fThumbPath: string;
    fVideospath: string;
    function GetName: string;
    function GetHomePath: string;
    function GetThumbPath: string;
    function GetVideosPath: string;
  public
    constructor Create;
    property Name: string read fName write fName;
    property HomePath: string read fHomePath write fHomePath;
    property ThumbPath: string read fThumbPath write fThumbPath;
    property VideosPath: string read fVideosPath write fVideosPath;
  end;

implementation

constructor TUser.Create;
begin
  fName := GetName;
  fHomePath := GetHomePath;
  fThumbPath := GetThumbPath;
  fVideosPath := GetVideosPath;
end;

function TUser.GetName: string;
var
  SL: TStringList;
  aStream: TMemoryStream;
begin
  {$IFDEF WINDOWS}
    Result := GetEnvironmentVariable('USERNAME');
  {$ENDIF}
  {$IFDEF LINUX}
    Result := '';
    aStream := TMemoryStream.Create;
    ProcessExecute('whoami', ',1,pipe:1', Nil, aStream, Nil);
    aStream.Position := 0;
    SL := TStringList.Create;
    try
      SL.LoadFromStream(aStream);
      Result := Copy(SL.Text, 1, Length(SL.Text)-1);
      if Result = '' then
        raise Exception.Create('FATAL ERROR: Empty User Name!');
    finally
      aStream.Free;
      SL.Free;
    end;
  {$ENDIF}
end;

function TUser.GetHomePath: string;
begin
  {$IFDEF LINUX}
    Result := '/home/' + GetName;
  {$ENDIF}
end;

function TUser.GetThumbPath: string;
begin
  {$IFDEF LINUX}
    Result := GetHomePath + '/.cache/thumbnails';
  {$ENDIF}
  {$IFDEF WINDOWS}
    Result := 'c:\users\' + fName + '\appdata\Local\Microsoft\Windows\Explorer\ClipBrowser';
  {$ENDIF}
end;

function TUser.GetVideosPath: string;
begin
  {$IFDEF LINUX}
    Result := GetHomePath + '/Videos';
  {$ENDIF}
end;

end.

