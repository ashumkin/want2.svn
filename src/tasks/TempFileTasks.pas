(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

{
  Contributors:
    Radim Novotny <radimnov@seznam.cz>
}

unit TempFileTasks;

interface

uses
  SysUtils,
  Classes,
  WantClasses;

type
  TTempFileTask = class(TTask)
  private
    FDestDir     :string;
    FPrefix      :string;
    FProperty    :string;
    FSuffix      :string;
  public
    procedure Init;    override;
    procedure Execute; override;
  published
    property destdir   :string read FDestDir  write FDestDir;
    property prefix    :string read FPrefix   write FPrefix;
    property _property :string read FProperty write FProperty;
    property suffix    :string read FSuffix   write FSuffix;
  end;

implementation

{ TTempFileTask }

procedure TTempFileTask.Execute;
var
  bDirName : string;
  bTempFileName : string;
begin
  inherited;
  if FDestDir <> ''
    then
    begin
      bDirName := IncludeTrailingPathDelimiter(FDestDir);
      if not DirectoryExists(bDirName) then TaskError('Destination directory does not exists: '+FDestDir);
    end
    else bDirName := '';
  bDirName := ToWantPath(bDirName);
  Randomize;
  repeat
    // temp file name is in Want path format
    bTempFileName := bDirName + '/' + FPrefix + Format('%.4x', [Random(65535)]);
    if FSuffix <> ''
       then bTempFileName := bTempFileName + FSuffix
       else bTempFileName := bTempFileName + '.tmp';
  until not FileExists(ToSystemPath(bTempFileName));
  if Assigned(Owner) then
  begin
    Owner.SetProperty(FProperty, bTempFileName);
    Owner.Configure;
  end;
end;

procedure TTempFileTask.Init;
begin
  inherited;
  RequireAttribute('property');
end;

initialization
  RegisterTask(TTempFileTask);
end.
