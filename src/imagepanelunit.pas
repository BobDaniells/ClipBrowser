unit ImagePanelUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, extctrls, contnrs;

type
  TCoordPicture = class(TPicture)
  private
    fCoord: TPoint;
  public
    property Coord: TPoint read fCoord write fCoord;
  end;

  TPictureList = class(TobjectList);

  TImagePanel = class(TPanel)
  private
    fPictures: TPictureList;
  public
//    procedure Add(pic: TCoordPicture);
    property Pictures: TPictureList read fPictures write fPictures;
  end;

implementation

end.

