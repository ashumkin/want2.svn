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
Original Author: Juancarlo Añez
Contributors   : 
}
unit PatternSets;

interface
uses
  Classes,
  MiniDOM,
  WildPaths,
  DanteClasses;

type
  TPatternPart = class(TDanteElement)
  protected
    procedure SetValue(Value: string); virtual; abstract;
  published
    property name: string write SetValue;
  end;

  TIncludeElement = class(TPatternPart)
    procedure SetValue(Value: string); override;
  end;

  TExcludeElement = class(TPatternPart)
    procedure SetValue(Value: string); override;
  end;

  TPatternSet = class(TDanteElement)
  protected
    FIncludes: TStrings;
    FExcludes: TStrings;

    FPatternSets: array of TPatternSet;

    procedure AddPatternSet(APatternSet: TPatternSet);

    procedure SetIncludes(Value: TStrings);
    procedure SetExcludes(Value: TStrings);

    procedure DoInclude(Files: TStrings; Pattern: TPath; Base: string); virtual;
    procedure DoExclude(Files: TStrings; Pattern: TPath; Base: string); virtual;

    procedure DoIncludes(Files: TStrings; Base: string); virtual;
    procedure DoExcludes(Files: TStrings; Base: string); virtual;

  public
    constructor Create(Owner: TDanteElement); override;
    destructor  Destroy; override;

    function ParseXMLChild(Child: MiniDom.IElement):boolean; override;

    procedure Include(Pattern: TPath);  overload;
    procedure Exclude(Pattern: TPath);  overload;


    function  Paths  : TPaths;
    procedure GetPaths(Files: TStrings);
    procedure AddPaths(Paths: TPaths);

    function SystemPaths: TSystemPaths;
    function RelativePaths: TPaths;
    function MovePaths(ToBase: TPath): TPaths;

    property Includes: TStrings read FIncludes;
    property Excludes: TStrings read FExcludes;
  published
    function createInclude: TIncludeElement;
    function createExclude: TExcludeElement;
    function createPatternSet: TPatternSet;

    property id;
  end;


  TCustomFileSet  = class(TPatternSet)
  public
    function  SetAttribute(Name, Value: string): boolean; override;
    procedure AddDefaultPatterns; virtual;
    property dir: TPath read GetBaseDir write SetBaseDir;
  end;

  TCustomDirSet  = class(TCustomFileSet)
  protected
    procedure DoExcludes(Files: TStrings; Base: string); override;
  end;

  TFileSet  = class(TCustomFileSet)
  published
    property dir;
  end;

  TDirSet = class(TCustomDirSet)
  published
    property dir;
  end;

implementation

{ TIncludeElement }

procedure TIncludeElement.SetValue(Value: string);
begin
 (Owner as TPatternSet).Include(Value);
end;

{ TExcludeElement }

procedure TExcludeElement.SetValue(Value: string);
begin
 (Owner as TPatternSet).Exclude(Value);
end;

{ TPatternSet }

constructor TPatternSet.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  FIncludes := TStringList.Create;
  FExcludes := TStringList.Create;
end;

destructor TPatternSet.Destroy;
begin
  FIncludes.Free;
  FExcludes.Free;
  inherited Destroy;
end;

procedure TPatternSet.SetIncludes(Value: TStrings);
begin
  FIncludes.Assign(Value);
end;

procedure TPatternSet.SetExcludes(Value: TStrings);
begin
  FExcludes.Assign(Value);
end;

procedure TPatternSet.Include(Pattern: TPath);
begin
  FIncludes.Add(Pattern);
end;

procedure TPatternSet.Exclude(Pattern: TPath);
begin
  FExcludes.Add(Pattern);
end;

procedure TPatternSet.DoInclude(Files: TStrings; Pattern: TPath; Base: string);
begin
  Wild(Files, Pattern, Base);
end;

procedure TPatternSet.DoExclude(Files: TStrings; Pattern: TPath; Base: string);
var
  Excluded: TPaths;
  f       : Integer;
begin
  Excluded := SplitPath(PathConcat(Base, Pattern));
  for f := Files.Count-1 downto 0 do
    if IsMatch(SplitPath(Files[f]), Excluded) then
      Files.Delete(f);
end;

function TPatternSet.createInclude: TIncludeElement;
begin
  Result := TIncludeElement.Create(Self);
end;

function TPatternSet.createExclude: TExcludeElement;
begin
  Result := TExcludeElement.Create(Self);
end;

procedure TPatternSet.DoIncludes(Files: TStrings; Base: string);
var
  i: Integer;
begin
  for i := 0 to FIncludes.Count-1 do
    DoInclude(Files, FIncludes[i], Base);

  for i := Low(FPatternSets) to High(FPatternSets) do
    FPatternSets[i].DoIncludes(Files, Base);
end;


procedure TPatternSet.DoExcludes(Files: TStrings; Base: string);
var
  i: Integer;
begin
  for i := 0 to FExcludes.Count-1 do
    DoExclude(Files, FExcludes[i], Base);

  for i := Low(FPatternSets) to High(FPatternSets) do
    FPatternSets[i].DoExcludes(Files, Base);
end;

procedure TPatternSet.AddPatternSet(APatternSet: TPatternSet);
begin
  SetLength(FPatternSets, 1+Length(FPatternSets));
  FPatternSets[High(FPatternSets)] := APatternSet;
end;

function TPatternSet.ParseXMLChild(Child: IElement): boolean;
begin
  if  (Child.Name = 'patternset')
  and (Child.attribute('refid') <> nil) then
  begin
    AddPatternSet(Project.FindChild(Child.attributeValue('refid'), TPatternSet) as TPatternSet);
    Result := true;
  end
  else
    Result := inherited ParseXMLChild(Child);
end;

function TPatternSet.Paths: TPaths;
var
  Files   : TStringList;
begin
  Files := TStringList.Create;
  try
    Files.Sorted := True;

    Log(vlDebug, 'fileset basedir="%s"', [basedir]);
    GetPaths(Files);

    Result := StringsToPaths(Files);
  finally
    Files.Free;
  end;
end;


procedure TPatternSet.AddPaths(Paths: TPaths);
var
  Files   : TStringList;
  i, n    : Integer;
begin
  Files := TStringList.Create;
  try
    Files.Sorted := True;

    GetPaths(Files);

    n := Length(Paths);
    SetLength(Paths, n + Files.Count);
    for i := 0 to Files.Count-1 do
      Paths[i+n] := Files[i];
  finally
    Files.Free;
  end;
end;


procedure TPatternSet.GetPaths(Files: TStrings);
begin
  DoIncludes(Files, BasePath);
  DoExcludes(Files, BasePath);
end;

function TPatternSet.MovePaths(ToBase: TPath): TPaths;
begin
  Result := WildPaths.MovePaths(Paths, BasePath, ToBase);
end;

function TPatternSet.RelativePaths: TPaths;
begin
  Result := ToRelativePaths(Paths, BasePath);
end;

function TPatternSet.SystemPaths: TSystemPaths;
begin
   Result := ToSystemPaths(Paths);
end;


function TPatternSet.createPatternSet: TPatternSet;
begin
  Result := TPatternSet.Create(Self);
  AddPatternSet(Result);
end;

{ TCustomFileSet }

procedure TCustomFileSet.AddDefaultPatterns;
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

function TCustomFileSet.SetAttribute(Name, Value: string): boolean;
begin
  if Name <> 'refid' then
    Result := inherited SetAttribute(name, Value)
  else
  begin
    AddPatternSet(Project.FindChild(Value, TPatternSet) as TPatternSet);
    Result := true;
  end;
end;

{ TCustomDirSet }

procedure TCustomDirSet.DoExcludes(Files: TStrings; Base: string);
var
  f       : Integer;
begin
  inherited DoExcludes(Files, Base);
  for f := Files.Count-1 downto 0 do
    if not PathIsdir(PathConcat(Base, Files[f])) then
      Files.Delete(f);
end;


initialization
  RegisterElement(TPatternSet);
end.
