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
  DanteClasses,
  WildPaths,
  FileOps,

  SysUtils,
  Classes;

type
  EFileSetException = class(EDanteException);
  EFileSetError     = class(EFileSetException);

  TFileSetPart = class(TDanteElement)
  protected
    procedure SetValue(Value :string); virtual; abstract;
  published
    property name :string write SetValue;
  end;

  TIncludeComponent = class(TFileSetPart)
    procedure SetValue(Value :string); override;
  end;

  TExcludeComponent = class(TFileSetPart)
    procedure SetValue(Value :string); override;
  end;

  TFileSet = class(TDanteElement)
  protected
    FDir:      TPath;    // prefix for all wildcards
    FIncludes: TStrings;
    FExcludes: TStrings;
    FPaths:    TPaths;

    FIncluder: TIncludeComponent;
    FExcluder: TExcludeComponent;

    procedure SetDir(Value :TPath);

    procedure SetChanged;

    procedure SetIncludes(Value :TStrings);
    procedure SetExcludes(Value :TStrings);

    procedure DoInclude(Files :TStrings; Pattern :TPath);
    procedure DoExclude(Files :TStrings; Pattern :TPath);
  public
    constructor Create(Owner :TDanteElement); override; 
    destructor  Destroy; override;

    procedure Include(Pattern :TPath);
    procedure Exclude(Pattern :TPath);

    function Paths   :TPaths;
    function SystemPaths :TPaths;
    function RelativePaths :TPaths;
    function MovePaths(ToBase :TPath) :TPaths;

    function  AbsoluteDir :TPath;

    property Includes: TStrings read FIncludes;
    property Excludes: TStrings read FExcludes;
  published
    function createInclude :TIncludeComponent;
    function createExclude :TExcludeComponent;

    property dir:      TPath read FDir write SetDir;
  end;


implementation

{ TFileSet }

constructor TFileSet.Create(Owner :TDanteElement);
begin
  inherited Create(Owner);
  FIncludes := TStringList.Create;
  FExcludes := TStringList.Create;
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

procedure TFileSet.Include(Pattern: TPath);
begin
  FIncludes.Add(WildPaths.ToRelativePath(Pattern, Dir));
  SetChanged;
end;

procedure TFileSet.Exclude(Pattern: TPath);
begin
  FExcludes.Add(WildPaths.ToRelativePath(Pattern, Dir));
  SetChanged;
end;

procedure TFileSet.DoInclude(Files: TStrings; Pattern: TPath);
begin
  Wild(Files, Pattern, AbsoluteDir);
end;

procedure TFileSet.DoExclude(Files :TStrings; Pattern: TPath);
var
  Excluded :TPaths;
  f        :Integer;
begin
  Excluded := SplitPath(PathConcat(AbsoluteDir, Pattern));
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
  Result := ToRelativePaths(Paths, AbsoluteDir);
end;

function TFileSet.MovePaths(ToBase: TPath): TPaths;
begin
  Result := WildPaths.MovePaths(Paths, AbsoluteDir, ToBase);
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

function TFileSet.AbsoluteDir: TPath;
begin
  Result := PathConcat(BasePath, dir);
end;

procedure TFileSet.SetDir(Value: TPath);
begin
  FDir := ToRelativePath(Value);
  SetChanged;
end;

function TFileSet.createInclude: TIncludeComponent;
begin
  if FIncluder = nil then
    FIncluder := TIncludeComponent.Create(Self);
  Result := FIncluder;
end;

function TFileSet.createExclude: TExcludeComponent;
begin
  if FExcluder = nil then
    FExcluder := TExcludeComponent.Create(Self);
  Result := FExcluder;
end;

{ TIncludeComponent }

procedure TIncludeComponent.SetValue(Value: string);
begin
 (Owner as TFileSet).Include(Value);
end;

{ TExcludeComponent }

procedure TExcludeComponent.SetValue(Value: string);
begin
 (Owner as TFileSet).Exclude(Value);
end;

end.
