{ $Id$ }
{
--------------------------------------------------------------------------
Copyright (c) 2001, Juancarlo Añez, Caracas, Venezuela.
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The names Chris Morris, Dante and the names of contributors to this software
may not be used to endorse or promote products derived from this software
without specific prior written permission.

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
(based on BSD Open Source License)
}
unit FileSets;

interface
uses
  WildPaths,
  FileOps,

  SysUtils,
  Classes;

type
  EFileSetException = class(Exception);
  EFileSetError     = class(EFileSetException);

  TFileSet = class(TPersistent)
  protected
    FRootPath: TPath;
    FBasePath: TPath;
    FIncludes: TStrings;
    FExcludes: TStrings;
    FPaths:    TPaths;

    procedure SetBasePath(Value :TPath);

    procedure SetChanged;

    procedure SetIncludes(Value :TStrings);
    procedure SetExcludes(Value :TStrings);

    procedure DoInclude(Files :TStrings; Spec :TPath);
    procedure DoExclude(Files :TStrings; Spec :TPath);
  public
    constructor Create(RootPath :TPath; DefaultExcludes :boolean = true);
    destructor  Destroy; override;

    procedure Include(Spec :TPath);
    procedure Exclude(Spec :TPath);

    function Paths   :TPaths;
    function SystemPaths :TPaths;
    function RelativePaths :TPaths;

    procedure DeleteFiles;
    procedure CopyFiles(ToBase :TPath);
    procedure MoveFiles(ToBase :TPath);

    function  FullBasePath :TPath;
    property  RootPath :TPath read FRootPath;
  published
    property BasePath: TPath read FBasePath write SetBasePath;
    property Includes: TStrings read FIncludes;
    property Excludes: TStrings read FExcludes;
  end;


implementation

{ TFileSet }

constructor TFileSet.Create(RootPath: TPath; DefaultExcludes: boolean);
begin
  inherited Create;
  if not PathIsAbsolute(RootPath) then
    raise EFileSetError.Create(Format('Danger, Will Robinson! Root path %s is relative!', [RootPath]));
  FRootPath := RootPath;

  FIncludes := TStringList.Create;
  FExcludes := TStringList.Create;

  if DefaultExcludes then
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
  end;
end;

destructor TFileSet.Destroy;
begin
  FIncludes.Free;
  FExcludes.Free;
  inherited Destroy;
end;

procedure TFileSet.SetIncludes(Value: TStrings);
begin
  FIncludes.Assign(Value);
end;

procedure TFileSet.SetExcludes(Value: TStrings);
begin
  FExcludes.Assign(Value);
  SetChanged;
end;

procedure TFileSet.SetChanged;
begin
  FPaths := nil;
end;

procedure TFileSet.Include(Spec: TPath);
begin
  FIncludes.Add(Spec);
  SetChanged;
end;

procedure TFileSet.Exclude(Spec: TPath);
begin
  FExcludes.Add(Spec);
  SetChanged;
end;

procedure TFileSet.DoInclude(Files: TStrings; Spec: TPath);
begin
  Wild(Files, Spec, FullBasePath);
end;

procedure TFileSet.DoExclude(Files :TStrings; Spec: TPath);
var
  Excluded :TPaths;
  f        :Integer;
begin
  Excluded := SplitPath(PathConcat(FullBasePath, Spec));
  for f := Files.Count-1 downto 0 do
    if IsMatch(SplitPath(Files[f]), Excluded) then
      Files.Delete(f);
end;

function TFileSet.SystemPaths: TPaths;
begin
   Result := ToSystemPaths(Paths);
end;

function TFileSet.RelativePaths: TPaths;
begin
  Result := AbsoluteToRelativePaths(Paths, FullBasePath);
end;

function TFileSet.Paths: TPaths;
var
  Files    :TStringList;
  i        :Integer;
begin
  if FPaths = nil then
  begin
    Files := TStringList.Create;
    try
      Files.Sorted := True;
      for i := 0 to FIncludes.Count-1 do
        DoInclude(Files, FIncludes[i]);
      for i := 0 to FExcludes.Count-1 do
        DoExclude(Files, FExcludes[i]);
      FPaths := StringsToPaths(Files);
    finally
      Files.Free;
    end;
  end;
  Result := FPaths;
end;

procedure TFileSet.DeleteFiles;
begin
  FileOps.DeleteFiles(Paths);
end;

procedure TFileSet.CopyFiles(ToBase: TPath);
begin
  FileOps.CopyFiles(Paths, FullBasePath, ToBase);
end;

procedure TFileSet.MoveFiles(ToBase: TPath);
begin
  FileOps.MoveFiles(Paths, FullBasePath, ToBase);
end;

function TFileSet.FullBasePath: TPath;
begin
  Result := PathConcat(FRootPath, FBasePath);
end;

procedure TFileSet.SetBasePath(Value: TPath);
begin
  if not PathIsAbsolute(Value) then
    FBasePath := Value
  else
  begin
    raise EFileSetError.Create(Format('Danger, Will Robinson! Base path %s is relative!', [RootPath]));
    (*
    FRootPath := Value;
    FBasePath := '';
    *)
  end;
  SetChanged;
end;

end.
