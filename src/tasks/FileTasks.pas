{ $Id$ }
{
--------------------------------------------------------------------------------
Copyright (c) 2001, Dante Authors -- See authors.txt for complete list
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The name Dante, the names of the authors in authors.txt and the names of
other contributors to this software may not be used to endorse or promote
products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------
}
unit FileTasks;

interface

uses
  DanteClasses,
  WildPaths,
  FileSets,
  FileOps,

  SysUtils,
  Classes;

type
  TFileTask = class(TTask)
  end;

  TFileSetTask = class(TFileTask)
  protected
    FFileSet:         TFileSet;
    FDefaultExcludes: boolean;

    procedure AddDefaultExcludes;

    procedure AddCommaSeparatedIncludes(Value :string);
    procedure AddCommaSeparatedExcludes(Value :string);
  public
    constructor Create(Owner :TDanteElement); override;

  published
    function CreateInclude :TIncludeComponent;
    function CreateExclude :TExcludeComponent;

    property DefaultExcludes :boolean
      read FDefaultExcludes write FDefaultExcludes default True;
  end;

  TMkDirTask = class(TFileTask)
  protected
    FDir :string;
  public
    procedure Validate; override;
    procedure Execute;  override;
  published
    property dir : string read FDir write FDir;
  end;

  TTouchTask = class(TFileTask)
  protected
    FFile :string;
  public
    procedure Validate; override;
    procedure Execute; override;
  published
    property _File : string read FFile write FFile;
  end;

  TDeleteTask = class(TFileSetTask)
  protected
    FDir  :string;
    FFile :string;

    procedure SetDir(Value :string);
    procedure SetFile(Value :string);
  public
    procedure Validate; override;
    procedure Execute; override;
  published
    property _File :string  read FFile write SetFile stored True;
    property Dir   :string  read FDir  write SetDir;
  end;

  TMoveCopyTask = class(TFileSetTask)
  protected
    FToDir  :string;

    procedure DoPaths(FromPaths, ToPaths :TPaths); virtual;
    procedure DoFiles(FromPath, ToPath :TPath);    virtual; abstract;
  public
    procedure Validate; override;
    procedure Execute; override;
  published
    property todir  :string read FToDir  write FToDir;
  end;

  TCopyTask = class(TMoveCopyTask)
  protected
    procedure DoPaths(FromPaths, ToPaths :TPaths); override;
    procedure DoFiles(FromPath, ToPath :TPath);    override;
  end;

  TMoveTask = class(TMoveCopyTask)
  protected
    procedure DoPaths(FromPaths, ToPaths :TPaths); override;
    procedure DoFiles(FromPath, ToPath :TPath);    override;
  end;

implementation

{ TFileSetTask }

constructor TFileSetTask.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  DefaultExcludes := True;
  FFileSet := TFileSet.Create(Self);
  FFileSet.Dir := BasePath;
end;

function TFileSetTask.CreateInclude: TIncludeComponent;
begin
  Result := FFileSet.CreateInclude;
end;

function TFileSetTask.CreateExclude: TExcludeComponent;
begin
  Result := FFileSet.CreateExclude;
end;

procedure TFileSetTask.AddDefaultExcludes;
begin
  if DefaultExcludes then
    with FFileSet do
    begin
      // add the default Ant excludes
      Exclude('**/*~');
      Exclude('**/#*#');
      Exclude('**/%*%');
      Exclude('**/CVS');
      Exclude('**/CVS/*');
      Exclude('**/.cvsignore');

      // Some additional excludes
      Exclude('**/*.*~*');
      Exclude('**/*.bak');
      Exclude('**/dunit.ini');
    end;
end;

procedure TFileSetTask.AddCommaSeparatedIncludes(Value: string);
var
  Paths :TStringArray;
  p     :Integer;
begin
  Paths := CommaTextToArray(Value);
  for p := Low(Paths) to High(Paths) do
    FFileSet.Include(Paths[p]);
end;

procedure TFileSetTask.AddCommaSeparatedExcludes(Value: string);
var
  Paths :TStringArray;
  p     :Integer;
begin
  Paths := CommaTextToArray(Value);
  for p := Low(Paths) to High(Paths) do
    FFileSet.Exclude(Paths[p]);
end;



{ TMkDirTask }

procedure TMkDirTask.Validate;
begin
  inherited Validate;
  RequireAttribute('dir', dir);
end;


procedure TMkDirTask.Execute;
begin
  if dir = '' then
    TaskError('<dir> attribute not set');
  if not IsDir(dir) then
  begin
    if PathExists(dir) then
      TaskFailure(Format('cannot create dir "%s". A file is in the way.', [dir]));
    Log(ToRelativePath(dir));
    Log(vlVerbose, 'mkdir ' + ToSystemPath(dir));
    FileOps.MakeDir(ToAbsolutePath(dir));
    if not IsDir(dir) then
      TaskFailure(Format('cannot create dir "%s".', [dir]));
  end;
end;


{ TTouchTask }

procedure TTouchTask.Execute;
begin
  if _File = '' then
    TaskError('<file> attribute not set');
  Log(ToRelativePath(_File));
  FileOps.TouchFile(_File);
end;


procedure TTouchTask.Validate;
begin
  inherited Validate;
  RequireAttribute('file', _file);
end;

{ TDeleteTask }

procedure TDeleteTask.Execute;
var
  Paths :TPaths;
  p     :Integer;
begin
  if dir <> '' then
    AddDefaultExcludes;

  if FFileSet <> nil then
    Paths := FFileSet.Paths
  else
    Paths := nil;

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
      FileOps.DeleteFile(Paths[p]);
      if PathExists(Paths[p]) then
        TaskFailure(Format('Could not delete "%s"', [  Paths[p] ]) );
    end;
  end;
end;

procedure TDeleteTask.SetDir(Value: string);
begin
  Value := ToRelativePath(Value);
  FDir := Value;
  with FFileSet do
  begin
    Include(Value);
    Include(PathConcat(Value, '**'));
  end;
end;

procedure TDeleteTask.SetFile(Value: string);
begin
  FFile := Value;
  FFileSet.Include(Value);
end;

procedure TDeleteTask.Validate;
begin
  inherited Validate;
  if (_file = '') and (dir = '') then
    TaskError('either the "file" or the "dir" attribute must be set');
end;

{ TMoveCopyTask }


procedure TMoveCopyTask.DoPaths(FromPaths, ToPaths: TPaths);
var
  p       :Integer;
begin
  for p := Low(FromPaths) to High(FromPaths) do
    DoFiles(FromPaths[p], ToPaths[p]);
end;

procedure TMoveCopyTask.Execute;
var
  FromPaths,
  ToPaths   :TPaths;
begin
  if todir = '' then
    TaskError('todir not set');
  AddDefaultExcludes;

  FromPaths := FFileSet.Paths;
  ToPaths   := FFileSet.MovePaths(todir);

  DoPaths(FromPaths, ToPaths);
end;

procedure TMoveCopyTask.Validate;
begin
  inherited Validate;
  RequireAttribute('todir', todir);
end;

{ TCopyTask }

procedure TCopyTask.DoFiles(FromPath, ToPath: TPath);
begin
  Log(vlVerbose, Format('copy %s -> %s', [ToSystemPath(FromPath), ToSystemPath(ToPath)]));
  AboutToScratchPath(ToPath);
  FileOps.CopyFile(FromPath, ToPath);
  if not FileExists(ToSystemPath(ToPath)) then
    TaskFailure(ToPath);
end;

procedure TCopyTask.DoPaths(FromPaths, ToPaths: TPaths);
begin
  Log(Format('copy %d files from "%s" to "%s"', [
         Length(FromPaths),
         ToRelativePath(BasePath),
         ToRelativePath(todir)]));
  inherited DoPaths(FromPaths, ToPaths);
end;

{ TMoveTask }

procedure TMoveTask.DoFiles(FromPath, ToPath: TPath);
begin
  Log(vlVerbose, Format('move %s -> %s', [ToSystemPath(FromPath), ToSystemPath(ToPath)]));
  AboutToScratchPath(ToPath);
  FileOps.MoveFile(FromPath, ToPath);
  if not FileExists(ToSystemPath(ToPath)) then
    TaskFailure(ToPath);
end;

procedure TMoveTask.DoPaths(FromPaths, ToPaths: TPaths);
begin
  Log(Format('moving %d files from %s to %s', [Length(FromPaths), FFileSet.dir, todir]));
  inherited DoPaths(FromPaths, ToPaths);
end;



initialization
  RegisterTasks([ TMkDirTask,
                  TTouchTask,
                  TDeleteTask,
                  TCopyTask,
                  TMoveTask]
  );
end.

