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
unit DelphiTasks;

interface
uses
  DanteClasses,
  ExecTasks,
  WildPaths,
  FileOps,

  Collections,
  MiniDom,

  JclBase,
  JclMiscel,
  JclSysInfo,
  JclRegistry,

  Windows,
  SysUtils,
  Classes;


type
  EDelphiTaskError       = class(ETaskError);
  EDelphiNotFoundError   = class(EDelphiTaskError);
  ECompilerNotFoundError = class(EDelphiTaskError);

  TPathElement = class(TDanteElement)
  protected
    FPath :string;

    procedure SetPath(Value :string); virtual; abstract;
  published
    property Path :string write SetPath;
  protected
  end;

  TUnitElement     = class(TPathElement)
    procedure SetPath(Value :string); override;
  end;

  TResourceElement  = class(TPathElement)
    procedure SetPath(Value :string); override;
  end;

  TIncludeElement  = class(TPathElement)
    procedure SetPath(Value :string); override;
  end;

  TDelphiCompileTask = class(TCustomExecTask)
  protected
    FExesPath :string;
    FDCUPath  :string;
    FSource   :string;

    FQuiet           :boolean;
    FMake            :boolean;
    FBuild           :boolean;
    FOptimize        :boolean;
    FDebug           :boolean;

    FUnitPaths       :TStrings;
    FResourcePaths   :TStrings;
    FIncludePaths    :TStrings;

    function BuildArguments: string; override;
    function FindDelphiDir :string;
    function FindCompiler :string;

    procedure SetExes(Value :string);

  public
    constructor Create(owner :TDanteElement); override;
    destructor  Destroy; override;

    class function XMLTag :string; override;

    procedure Validate; override;
    procedure Execute;  override;
  published

    property basedir; // from TTask

    // published methods for creating sub Elements
    // these methods are mapped to XML elements
    function CreateUnit     :TUnitElement;
    function CreateResource :TResourceElement;
    function CreateInclude  :TIncludeElement;

    // these properties are mapped to XML attributes
    property Arguments;
    property ArgumentList stored False;
    property SkipLines :Integer     read FSkipLines   write FSkipLines;

    property exes :string    read FExesPath   write SetExes;
    property dcus :string    read FDCUPath    write FDCUPath;

    property quiet    :boolean read FQuiet    write FQuiet default true;
    property make     :boolean read FMake     write FMake;
    property build    :boolean read FBuild    write FBuild;

    property optimize :boolean read FOptimize write FOptimize;
    property debug    :boolean read FDebug    write FDebug;

    property source  :string read FSource     write FSource;
  end;

implementation

{ TDelphiCompileTask }

constructor TDelphiCompileTask.Create(owner: TDanteElement);
begin
  inherited Create(owner);
  SkipLines  := 1;
  quiet      := true;

  FUnitPaths      := TStringList.Create;
  FResourcePaths  := TStringList.Create;
  FIncludePaths   := TStringList.Create;

  self.Executable := FindCompiler;
end;

destructor TDelphiCompileTask.Destroy;
begin
  FUnitPaths.Free;
  FResourcePaths.Free;
  FIncludePaths.Free;
  inherited Destroy;
end;

procedure TDelphiCompileTask.Validate;
begin
  inherited Validate;
  RequireAttribute('source', source);
end;



procedure TDelphiCompileTask.Execute;
begin
  Log(ToRelativePath(Source));
  Log(vlVerbose, BuildCmdLine);
  inherited Execute;
end;



function TDelphiCompileTask.FindCompiler: string;
begin
  Result := FindDelphiDir + '\bin\dcc32.exe';
  if not FileExists(Result) then
     raise ECompilerNotFoundError.Create(Result);
end;

function TDelphiCompileTask.FindDelphiDir: string;
const
  DelphiRegRoot  = 'SOFTWARE\Borland\Delphi';
  DelphiRootKey  = 'RootDir';

  function RootFor(version :Integer) :string;
  begin
    Result := Format('%s\%d.0', [DelphiRegRoot, version]);
  end;

  function ReadRootFor(version :Integer):string;
  begin
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, RootFor(version), DelphiRootKey, '');
  end;

var
  ver :Integer;
begin
  for ver := 6 downto 4 do
  begin
    Result := ReadRootFor(ver);
    if Result <> '' then EXIT;
  end;
  raise EDelphiNotFoundError.Create('');
end;

class function TDelphiCompileTask.XMLTag: string;
begin
  Result := 'dcc';
end;

function TDelphiCompileTask.BuildArguments: string;
begin
  Result := inherited BuildArguments;

  Result := Result + ' ' + ToSystemPath(source);

  if exes <> '' then
    Result := Result + ' -E' + ToSystemPath(exes);
  if dcus <> '' then
    Result := Result + ' -N' + ToSystemPath(dcus);
  if quiet then
    Result := Result + ' -Q';

  if build then
    Result := Result + ' -B'
  else if make then
    Result := Result + ' -M';

  if optimize then
    Result := Result + ' -$O+'
  else
    Result := Result + ' -$O-';

  if debug then
    Result := Result + ' -V -$D+ -$L+ -GD'
  else
    Result := Result + ' -V- -$D-';


  if FUnitPaths.Count > 0 then
    Result := Result + ' -U' + StringsToSystemPathList(FUnitPaths);

  if FResourcePaths.Count > 0 then
    Result := Result + ' -R' + StringsToSystemPathList(FResourcePaths);

  if FIncludePaths.Count > 0 then
    Result := Result + ' -R' + StringsToSystemPathList(FIncludePaths);
end;

function TDelphiCompileTask.createUnit: TUnitElement;
begin
  Result := TUnitElement.Create(Self);
end;

function TDelphiCompileTask.CreateResource: TResourceElement;
begin
  Result := TResourceElement.Create(Self);
end;

function TDelphiCompileTask.CreateInclude: TIncludeElement;
begin
  Result := TIncludeElement.Create(Self);
end;

procedure TDelphiCompileTask.SetExes(Value: string);
begin
  FExesPath := Value;
end;

{ TUnitElement }

procedure TUnitElement.SetPath(Value: string);
begin
  (Owner as TDelphiCompileTask).FUnitPaths.Add(Value);
end;

{ TResourceElement }

procedure TResourceElement.SetPath(Value: string);
begin
  (Owner as TDelphiCompileTask).FResourcePaths.Add(Value);
end;

{ TIncludeElement }

procedure TIncludeElement.SetPath(Value: string);
begin
  (Owner as TDelphiCompileTask).FIncludePaths.Add(Value);
end;

initialization
  RegisterTasks([TDelphiCompileTask]);
end.
