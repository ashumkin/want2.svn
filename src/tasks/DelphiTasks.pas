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
unit DelphiTasks;

interface
uses
  Windows,
  SysUtils,
  Classes,

  JclBase,
  JclSysUtils,
  JclMiscel,
  JclSysInfo,
  JclRegistry,
  JclStrings,

  Collections,
  MiniDom,
  DanteBase,
  DanteClasses,
  ExecTasks,
  WildPaths,
  PatternSets,
  Attributes;




const
  DelphiRegRoot  = 'SOFTWARE\Borland\Delphi';
  DelphiRootKey  = 'RootDir';

type
  EDelphiTaskError       = class(ETaskError);
  EDelphiNotFoundError   = class(EDelphiTaskError);
  ECompilerNotFoundError = class(EDelphiTaskError);

  TPathSet = class(TCustomDirSet)
  protected
    procedure SetPath(Value: string); virtual;
  public
    constructor Create(Owner :TDanteElement); override;
  published
    property Path: string write SetPath;
  end;

  TUnitPathElement     = class(TPathSet);
  TResourcePathElement = class(TPathSet);
  TIncludePathElement  = class(TPathSet);

  TCustomDelphiTask = class(TCustomExecTask)
  protected
    FVersions : string;

    FVersionFound :string;

    function RootForVersion(version: string): string;
    function FindDelphiVersion(ver :string) :string;
    function FindDelphiDir: string;

    function ReadUserOption(Key, Name :string):string;
    function ReadMachineOption(Key, Name :string):string;
  published
    property versions: string read FVersions  write FVersions;
  end;

  TDelphiCompileTask = class(TCustomDelphiTask)
  protected
    FExesPath: string;
    FDCUPath : string;
    FSource  : string;

    FQuiet          : boolean;
    FMake           : boolean;
    FBuild          : boolean;
    FOptimize       : boolean;
    FDebug          : boolean;
    FConsole        : boolean;
    FUseLibraryPath : boolean;

    FUnitPaths      : TUnitPathElement;
    FResourcePaths  : TResourcePathElement;
    FIncludePaths   : TIncludePathElement;

    FDefines        : TStrings;

    function BuildExecutable: string; override;
    function BuildArguments: string; override;
    function FindCompiler: string;

    procedure SetExes(Value: string);

    function ReadLibraryPaths :string;

  public
    constructor Create(Owner: TDanteElement); override;
    destructor  Destroy; override;

    class function TagName: string; override;

    procedure Init; override;
    procedure Execute;  override;

    procedure AddUnitPath(Path: TPath);
    procedure AddResourcePath(Path: TPath);
    procedure AddIncludePath(Path: TPath);
    procedure AddDefine(Name, Value :string);

  published
    property basedir; // from TTask

    function CreateUnitPath     :TUnitPathElement;
    function CreateResourcePath :TResourcePathElement;
    function CreateIncludePath  :TIncludePathElement;

    // these properties are mapped to XML attributes
    property Arguments;
    property ArgumentList stored False;
    property SkipLines;

    property exeoutput: string read FExesPath write SetExes;
    property dcuoutput: string read FDCUPath  write FDCUPath;

    property quiet: boolean read FQuiet write FQuiet default true;
    property make:  boolean read FMake  write FMake;
    property build: boolean read FBuild write FBuild;

    property optimize: boolean read FOptimize write FOptimize;
    property debug:    boolean read FDebug    write FDebug;
    property console:  boolean read FConsole  write FConsole;

    property uselibrarypath : boolean read FUseLibraryPath write FUseLibraryPath;

    property source : string read FSource     write FSource;
  end;

  TResourceCompileTask = class(TCustomDelphiTask)
  protected
    FFile:   string;
    FOutput: string;

    function BuildExecutable: string; override;
    function BuildArguments: string; override;

    function FindBRCC :string;
  public
    class function TagName: string; override;

    constructor Create(Owner: TDanteElement); override;

    procedure Init;    override;
    procedure Execute; override;
  published
    property _file:  string read FFile   write FFile;
    property output: string read FOutput write FOutput;
  end;

  TQuietElement          = class(TBooleanAttributeElement);
  TMakeElement           = class(TBooleanAttributeElement);
  TBuildElement          = class(TBooleanAttributeElement);
  TOptimizeElement       = class(TBooleanAttributeElement);
  TDebugElement          = class(TBooleanAttributeElement);
  TConsoleElement        = class(TBooleanAttributeElement);
  TUseLibraryPathElement = class(TBooleanAttributeElement);

  TDCUOutputElement = class(TPathAttributeElement);
  TEXEOutputElement = class(TPathAttributeElement);

  TOptionElement = class(TDanteElement)
  protected
    function dcc: TDelphiCompileTask;
  end;

  TDefineElement = class(TOptionElement)
  protected
    FValue :string;

  public
    procedure Init; override;

  published
    property Name;
    property Value :string read FValue write FValue;
  end;


implementation

{ TCustomDelphiTask }

function TCustomDelphiTask.FindDelphiDir: string;
var
  ver:  Integer;
  vers: TStringArray;
  i     :Integer;
begin
  vers := nil;
  if versions <> '' then
  begin
    vers := TextToArray(versions);
    for i := 0 to High(vers) do
    begin
       Result := FindDelphiVersion(vers[i]);
       if Result <> '' then
       begin
         FVersionFound := vers[i];
         BREAK;
       end;
    end;
  end
  else
  begin
    for ver := 6 downto 4 do
    begin
      Result := FindDelphiVersion(IntToStr(ver));
      if Result <> '' then
      begin
        FVersionFound := IntToStr(ver);
        BREAK;
      end;
    end;
  end;
  if Result = '' then
    raise EDelphiNotFoundError.Create('Could not find delphi');
end;


function TCustomDelphiTask.FindDelphiVersion(ver: string): string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, RootForVersion(ver), DelphiRootKey, '');
end;

function TCustomDelphiTask.ReadMachineOption(Key, Name: string): string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, RootForVersion(FVersionFound)+'\'+Key, Name, '');
end;

function TCustomDelphiTask.ReadUserOption(Key, Name: string): string;
begin
  Result := RegReadStringDef(HKEY_CURRENT_USER, RootForVersion(FVersionFound)+'\'+Key, Name, '');
end;

function TCustomDelphiTask.RootForVersion(version: string): string;
begin
  if Pos('.', version) = 0 then
    version := version + '.0';
  Result := Format('%s\%s', [DelphiRegRoot, version]);
end;

{ TDelphiCompileTask }

constructor TDelphiCompileTask.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  SkipLines  := 1;
  quiet      := true;

  FUnitPaths      := TUnitPathElement.Create(Self);
  FResourcePaths  := TResourcePathElement.Create(Self);
  FIncludePaths   := TIncludePathElement.Create(Self);
  FDefines        := TStringList.Create;
end;

destructor TDelphiCompileTask.Destroy;
begin
  FreeAndNil(FDefines);
  inherited Destroy;
end;

procedure TDelphiCompileTask.Init;
begin
  inherited Init;
  RequireAttribute('basedir');
  RequireAttribute('source');
end;



procedure TDelphiCompileTask.Execute;
begin
  Log(ToRelativePath(Source));
  inherited Execute;
end;

function TDelphiCompileTask.FindCompiler: string;
begin
  Result := FindDelphiDir + '\bin\dcc32.exe';
  if not FileExists(Result) then
     raise ECompilerNotFoundError.Create(Result);
end;

class function TDelphiCompileTask.TagName: string;
begin
  Result := 'dcc';
end;

function TDelphiCompileTask.BuildExecutable: string;
begin
  Executable := ToDantePath(FindCompiler);
  Result := inherited BuildExecutable;
end;

function TDelphiCompileTask.BuildArguments: string;
var
  Sources: TPaths;
  d      : Integer;
  s      : Integer;
  p      : Integer;
  PS     : TStrings;
  Paths  : TPaths;
begin
  Result := inherited BuildArguments;

  Log(vlVerbose, 'sources %s', [ToRelativePath(source)]);
  Sources := WildPaths.Wild(Source, BasePath);
  if Length(Sources) = 0 then
    TaskFailure(Format('could not find %s to compile', [PathConcat(BasePath, source)]));

  for s := Low(Sources) to High(Sources) do
  begin
    Log(vlVerbose, 'source %s', [ToRelativePath(Sources[s])]);
    Result := Result + ' ' + ToSystemPath(Sources[s]);
  end;

  if exeoutput <> '' then
  begin
    Log(vlVerbose, 'exeoutput=' + ToRelativePath(exeoutput));
    Result := Result + ' -E' + ToSystemPath(exeoutput);
  end;

  if dcuoutput <> '' then
  begin
    Log(vlVerbose, 'dcuoutput=' + ToRelativePath(dcuoutput));
    Result := Result + ' -N' + ToSystemPath(dcuoutput);
  end;

  if console then
  begin
    Log(vlVerbose, 'console');
    Result := Result + ' -CC'
  end
  else
    Result := Result + ' -CG';

  if quiet then
    Result := Result + ' -Q'
  else
    Log(vlVerbose, 'verbose');

  if build then
  begin
    Log(vlVerbose, 'build');
    Result := Result + ' -B'
  end
  else if make then
  begin
    Log(vlVerbose, 'make');
    Result := Result + ' -M';
  end;

  if optimize then
  begin
    Log(vlVerbose, 'optimize');
    Result := Result + ' -$O+'
  end
  else
    Result := Result + ' -$O-';

  if debug then
  begin
    Log(vlVerbose, 'debug');
    Result := Result + ' -$D+ -$L+ -$R+ -$Q+ -$C+ -GD'
  end
  else if optimize then
    Result := Result + ' -$D- -$L- -$R- -$Q- -$C-';

  for d := 0 to FDefines.Count-1 do
  begin
    Log(vlVerbose, 'define %s', [FDefines.Names[d]]);
    Result := Result + ' -D' + FDefines.Names[d];
  end;


  if useLibraryPath then
  begin
    Log(vlVerbose, 'uselibrarypath');
    PS := TStringList.Create;
    try
      StrToStrings(ReadLibraryPaths, ';', PS);
      for p := 0 to PS.Count-1 do
        Result := Result + ' -U' + PS[p];
    finally
      FreeAndNil(PS);
    end;
  end;

  Paths := FUnitPaths.RelativePaths;
  for p := Low(paths) to High(Paths) do
  begin
    Log(vlVerbose, 'unitpath %s', [ToRelativePath(Paths[p])]);
    Result := Result + ' -U' + ToSystemPath(Paths[p]);
  end;

  Paths := FResourcePaths.RelativePaths;
  for p := Low(paths) to High(Paths) do
  begin
    Log(vlVerbose, 'resourcepath %s', [ToRelativePath(Paths[p])]);
    Result := Result + ' -R' + ToSystemPath(Paths[p]);
  end;

  Paths := FIncludePaths.RelativePaths;
  for p := Low(paths) to High(Paths) do
  begin
    Log(vlVerbose, 'includepath %s', [ToRelativePath(Paths[p])]);
    Result := Result + ' -I' + ToSystemPath(Paths[p]);
  end;
end;

procedure TDelphiCompileTask.SetExes(Value: string);
begin
  FExesPath := Value;
end;

procedure TDelphiCompileTask.AddUnitPath(Path: TPath);
begin
  FUnitPaths.Include(Path);
end;

procedure TDelphiCompileTask.AddIncludePath(Path: TPath);
begin
  FIncludePaths.Include(Path);
end;

procedure TDelphiCompileTask.AddResourcePath(Path: TPath);
begin
  FResourcePaths.Include(Path);
end;

function TDelphiCompileTask.ReadLibraryPaths: string;
begin
  Result := ReadUserOption('Library', 'Search Path') + ';' +
            ReadUserOption('Library', 'SearchPath')
end;


procedure TDelphiCompileTask.AddDefine(Name, Value: string);
begin
  if Trim(Value) <> '' then
    FDefines.Values[Name] := Value
  else
    FDefines.Add(Name + '=');
end;

function TDelphiCompileTask.CreateUnitPath: TUnitPathElement;
begin
  Result := FUnitPaths;
end;

function TDelphiCompileTask.CreateIncludePath: TIncludePathElement;
begin
  Result := FIncludePaths;
end;

function TDelphiCompileTask.CreateResourcePath: TResourcePathElement;
begin
  Result := FResourcePaths;
end;

{ TResourceCompileTask }

function TResourceCompileTask.BuildArguments: string;
begin
  Result := inherited BuildArguments;

  Result := Result + ToSystemPath(_file);

  if output <> '' then
    Result := Result + ' -fo' + ToSystemPath(output);
end;

function TResourceCompileTask.BuildExecutable: string;
begin
  Executable := ToDantePath(FindBRCC);
  Result := inherited BuildExecutable;
end;

constructor TResourceCompileTask.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  SkipLines := 2;
end;

procedure TResourceCompileTask.Execute;
begin
  Log(ToRelativePath(_file));
  inherited Execute;
end;

function TResourceCompileTask.FindBRCC: string;
begin
  Result := FindDelphiDir + '\bin\brcc32.exe';
  if not FileExists(Result) then
     raise ECompilerNotFoundError.Create(Result);
end;

procedure TResourceCompileTask.Init;
begin
  inherited Init;
  RequireAttribute('file');
end;

class function TResourceCompileTask.TagName: string;
begin
  Result := 'brcc';
end;

{ TDefineElement }

procedure TDefineElement.Init;
begin
  inherited Init;
  Log(vlDebug, '%s %s=%s', [TagName, Name, Value]);
  RequireAttribute('name');
  dcc.AddDefine(Name, Value);
end;

{ TPathElement }

procedure TPathSet.SetPath(Value: string);
var
  Pat :TPath;
begin
  Pat := StrToken(Value, ',');
  while Pat <> '' do
  begin
    Include(Pat);
    Pat := StrToken(Value, ',');
  end;
end;

{ TPathSet }

constructor TPathSet.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  AddDefaultPatterns;
end;

{ TOptionElement }

function TOptionElement.dcc: TDelphiCompileTask;
begin
  Result := Owner as TDelphiCompileTask;
end;

initialization
  RegisterTasks( [TDelphiCompileTask, TResourceCompileTask]);
  RegisterElements(TDelphiCompileTask, [
                         TDefineElement,

                         TQuietElement,
                         TMakeElement,
                         TBuildElement,
                         TOptimizeElement,
                         TDebugElement,
                         TConsoleElement,
                         TUseLibraryPathElement,

                         TDCUOutputElement,
                         TEXEOutputElement
                         ]);
end.
