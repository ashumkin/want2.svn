(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo A�ez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit FileTasks;

interface

uses
  SysUtils,
  Classes,

  WantClasses,
  WildPaths,
  PatternSets;


type
  TFileTask = class(TTask)
  end;

  TFileSetTask = class(TFileTask)
  protected
    FFileSets: array of TFileSet;
    FDefaultExcludes: boolean;

    procedure AddDefaultPatterns; virtual;

    procedure AddCommaSeparatedIncludes(Value: string);
    procedure AddCommaSeparatedExcludes(Value: string);

    procedure DoFileset(Fileset: TFileSet); virtual;
  public
    constructor Create(Owner: TScriptElement); override;

    procedure Execute; override;
  published
    function CreateFileSet: TFileSet;
    function CreateInclude: TIncludeElement;
    function CreateExclude: TExcludeElement;

    property DefaultExcludes: boolean
      read FDefaultExcludes write FDefaultExcludes default True;
  end;

  TMkDirTask = class(TFileTask)
  protected
    FDir: string;
  public
    procedure Init; override;
    procedure Execute;  override;
  published
    property dir:  string read FDir write FDir;
  end;

  TTouchTask = class(TFileTask)
  protected
    FFile: string;
  public
    procedure Init; override;
    procedure Execute; override;
  published
    property _File:  string read FFile write FFile;
  end;

  TDeleteTask = class(TFileSetTask)
  protected
    FDeleteReadOnly: boolean;
    FDir : string;
    FFile: string;

    procedure AddDefaultPatterns; override;

    procedure SetFile(Value: string);

    procedure DoFileset(Fileset: TFileSet); override;
  public
    procedure Init; override;
  published
    property _File: string  read FFile write SetFile stored True;
    property Dir  : string  read FDir  write FDir;

    property DeleteReadOnly: boolean read FDeleteReadOnly write FDeleteReadOnly;
  end;

  TMoveCopyTask = class(TFileSetTask)
  protected
    FToDir : string;

    procedure DoFileset(Fileset: TFileSet); override;

    procedure DoPaths(Fileset: TFileSet; FromPaths, ToPaths: TPaths); virtual;
    procedure DoFiles(Fileset: TFileSet; FromPath, ToPath: TPath);    virtual; abstract;
  public
    procedure Init; override;
  published
    property todir : string read FToDir  write FToDir;
  end;

  TCopyTask = class(TMoveCopyTask)
  protected
    procedure DoPaths(Fileset: TFileSet; FromPaths, ToPaths: TPaths); override;
    procedure DoFiles(Fileset: TFileSet; FromPath, ToPath: TPath);    override;
  end;

  TMoveTask = class(TMoveCopyTask)
  protected
    procedure DoPaths(Fileset: TFileSet; FromPaths, ToPaths: TPaths); override;
    procedure DoFiles(Fileset: TFileSet; FromPath, ToPath: TPath);    override;
  end;

implementation

{ TFileSetTask }

constructor TFileSetTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  DefaultExcludes := True;
  CreateFileSet;
end;

function TFileSetTask.CreateInclude: TIncludeElement;
begin
  Result := FFileSets[0].CreateInclude;
end;

function TFileSetTask.CreateExclude: TExcludeElement;
begin
  Result := FFileSets[0].CreateExclude;
end;

procedure TFileSetTask.AddDefaultPatterns;
var
  i: Integer;
begin
  if DefaultExcludes then
  begin
    for i := Low(FFileSets) to High(FFileSets) do
      FFileSets[i].AddDefaultPatterns;
  end;
end;

procedure TFileSetTask.AddCommaSeparatedIncludes(Value: string);
var
  Paths: TStringArray;
  p    : Integer;
begin
  Paths := TextToArray(Value);
  for p := Low(Paths) to High(Paths) do
    FFileSets[0].Include(Paths[p]);
end;

procedure TFileSetTask.AddCommaSeparatedExcludes(Value: string);
var
  Paths: TStringArray;
  p    : Integer;
begin
  Paths := TextToArray(Value);
  for p := Low(Paths) to High(Paths) do
    FFileSets[0].Exclude(Paths[p]);
end;



function TFileSetTask.CreateFileSet: TFileSet;
begin
  Result := TFileSet.Create(Self);

  SetLength(FFileSets, 1 + Length(FFileSets));
  FFileSets[High(FFileSets)] := Result;
end;

procedure TFileSetTask.DoFileset(Fileset: TFileSet);
begin

end;

procedure TFileSetTask.Execute;
var
  f: Integer;
begin
  for f := Low(FFileSets) to High(FFileSets) do
    Self.DoFileset(FFileSets[f]);
end;

{ TMkDirTask }

procedure TMkDirTask.Init;
begin
  inherited Init;
  RequireAttribute('dir');
end;


procedure TMkDirTask.Execute;
begin
  Log(vlVerbose, Format('creating dir "%s"', [ToSystempath(ToAbsolutePath(dir))]) );
  if dir = '' then
    TaskError('<dir> attribute not set');
  if not PathIsDir(dir) then
  begin
    if PathExists(dir) then
      TaskFailure(Format('cannot create dir "%s". A file is in the way.', [dir]));
    Log(ToRelativePath(dir));
    WildPaths.MakeDir(ToAbsolutePath(dir));
    if not PathIsDir(dir) then
      TaskFailure(Format('cannot create dir "%s".', [dir]));
  end;
end;


{ TTouchTask }

procedure TTouchTask.Execute;
begin
  if _File = '' then
    TaskError('<file> attribute not set');
  Log(ToRelativePath(_File));
  WildPaths.TouchFile(_File);
end;


procedure TTouchTask.Init;
begin
  inherited Init;
  RequireAttribute('file');
end;

{ TDeleteTask }

procedure TDeleteTask.AddDefaultPatterns;
var
  RelDir: TPath;
begin
  //inherited AddDefaultPatterns;

  if dir <> '' then
  begin
    RelDir := ToRelativePath(dir);
    with FFileSets[0] do
    begin
      Include(RelDir);
      Include(PathConcat(RelDir, '**'));
    end;
  end;
end;

procedure TDeleteTask.DoFileset(Fileset: TFileSet);
var
  Paths : TPaths;
  p     : Integer;
begin
  if dir <> '' then
    AddDefaultPatterns;


  Paths := Fileset.Paths;

  if Paths = nil then
    Log(vlVerbose, 'nothing to delete')
  else begin
    if _file <> '' then
      Log(Format('delete %d files (%s)',    [Length(Paths), ToRelativePath(_file)]))
    else if dir <> '' then
      Log(Format('delete %d files from "%s"', [Length(Paths), ToRelativePath(dir)]))
    else
      Log(Format('delete %d files from "%s"', [Length(Paths), ToRelativePath(BasePath)]));

    for p := High(Paths) downto Low(Paths) do
    begin
      Log(vlVerbose, 'del ' + ToSystemPath(Paths[p]));
      AboutToScratchPath(Paths[p]);
      WildPaths.DeleteFile(Paths[p], FDeleteReadOnly);
      if PathExists(Paths[p]) then
        TaskFailure(Format('Could not delete "%s"', [  Paths[p] ]) );
    end;
  end;
end;

procedure TDeleteTask.SetFile(Value: string);
begin
  FFile := Value;
  FFileSets[0].Include(Value);
end;

procedure TDeleteTask.Init;
begin
  inherited Init;
(*  if (_file = '') and (dir = '') then
    TaskError('either the "file" or the "dir" attribute must be set'); *)
end;

{ TMoveCopyTask }


procedure TMoveCopyTask.DoPaths(Fileset: TFileSet; FromPaths, ToPaths: TPaths);
var
  p      : Integer;
begin
  Assert(Length(FromPaths) = Length(ToPaths));
  for p := Low(FromPaths) to High(FromPaths) do
    DoFiles(Fileset, FromPaths[p], ToPaths[p]);
end;

procedure TMoveCopyTask.DoFileset(Fileset: TFileSet);
var
  FromPaths,
  ToPaths  : TPaths;
begin
  AddDefaultPatterns;

  FromPaths := Fileset.Paths;
  ToPaths   := Fileset.MovePaths(todir);

  DoPaths(Fileset, FromPaths, ToPaths);
end;

procedure TMoveCopyTask.Init;
begin
  inherited Init;
  RequireAttribute('todir');
end;

{ TCopyTask }

procedure TCopyTask.DoFiles(Fileset: TFileSet; FromPath, ToPath: TPath);
begin
  Log(vlVerbose, Format('copy %s -> %s', [ToSystemPath(FromPath), ToSystemPath(ToPath)]));
  AboutToScratchPath(ToPath);
  if not PathIsDir(FromPath) then
  begin
    MakeDir(SuperPath(ToPath));
    WildPaths.CopyFile(FromPath, ToPath);
    if not PathExists(ToPath) then
      TaskFailure(Format('Could not copy"%s" to "%s', [ToRelativepath(FromPath), ToRelativepath(ToPath)]));
  end;
end;

procedure TCopyTask.DoPaths(Fileset: TFileSet; FromPaths, ToPaths: TPaths);
begin
  Log(Format('copy %d files from "%s" to "%s"', [
         Length(FromPaths),
         ToRelativePath(Fileset.BasePath),
         ToRelativePath(todir)]));
  inherited DoPaths(Fileset, FromPaths, ToPaths);
end;

{ TMoveTask }

procedure TMoveTask.DoFiles(Fileset: TFileSet; FromPath, ToPath: TPath);
begin
  Log(vlVerbose, Format('move %s -> %s', [ToSystemPath(FromPath), ToSystemPath(ToPath)]));
  AboutToScratchPath(ToPath);
  if not PathIsDir(FromPath) then
  begin
    MakeDir(SuperPath(ToPath));
    WildPaths.MoveFile(FromPath, ToPath);
    if not PathExists(ToPath) then
      TaskFailure(Format('Could not move "%s" to "%s', [ToRelativepath(FromPath), ToRelativepath(ToPath)]));
  end;
end;

procedure TMoveTask.DoPaths(Fileset: TFileSet; FromPaths, ToPaths: TPaths);
var
  i: Integer;
begin
  Assert(Length(FromPaths) = Length(ToPaths));
  Log(Format('moving %d files from %s to %s', [Length(FromPaths), FileSet.dir, todir]));
  inherited DoPaths(Fileset, FromPaths, ToPaths);

  for i := High(FromPaths) downto 0 do
  begin
    if PathIsDir(FromPaths[i]) then
      WildPaths.DeleteFile(FromPaths[i]);
  end;
end;



initialization
  RegisterTasks([ TMkDirTask,
                  TTouchTask,
                  TDeleteTask,
                  TCopyTask,
                  TMoveTask]
  );
end.

