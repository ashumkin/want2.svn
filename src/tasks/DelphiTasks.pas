{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca (R)                               }
{    ~                                         }
{     Copyright (C) 1995,2001 Juancarlo Añez   }
{     All rights reserved.                     }
{            http://www.suigeneris.org/juanca  }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{#(@)$Id$}

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

  JalStrings,

  XPerlRe,

  WantClasses,
  ExecTasks,
  WildPaths,
  PatternSets,
  Attributes;




const
  DelphiRegRoot  = 'SOFTWARE\Borland\Delphi';
  DelphiRootKey  = 'RootDir';

  __RENAMED_CFG_EXT = '.want.cfg';
  
type
  EDelphiTaskError       = class(ETaskError);
  EDelphiNotFoundError   = class(EDelphiTaskError);
  ECompilerNotFoundError = class(EDelphiTaskError);

  TMapType = (none, segments, publics, detailed); 

  TPathSet = class(TCustomDirSet)
  protected
    procedure SetPath(Value: string); virtual;
  public
    constructor Create(Owner :TScriptElement); override;
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

    procedure HandleOutputLine(Line :string); override;

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
    FWarnings       : boolean;
    FUseLibraryPath : boolean;
    FUseCFG         : boolean;

    FMap            : TMapType;

    FUnitPaths      : TUnitPathElement;
    FResourcePaths  : TResourcePathElement;
    FIncludePaths   : TIncludePathElement;

    FDefines        : TStrings;
    FPackages       : TStrings;

    FRenamedCFGs    :boolean;

    function BuildExecutable: string; override;
    function BuildArguments: string; override;
    function FindCompiler: string;

    procedure SetExes(Value: string);

    function ReadLibraryPaths :string;

  public
    constructor Create(Owner: TScriptElement); override;
    destructor  Destroy; override;

    class function TagName: string; override;

    procedure Init; override;
    procedure Execute;  override;

    procedure AddUnitPath(Path: TPath);
    procedure AddResourcePath(Path: TPath);
    procedure AddIncludePath(Path: TPath);
    procedure AddDefine(Name, Value :string);
    procedure AddPackage(Path :string);

    procedure RestoreCFGs;

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
    property warnings: boolean read FWarnings write FWarnings default true;
    property usecfg:   boolean read FUseCFG   write FUseCFG;

    property map       :TMapType read FMap write FMap;

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

    constructor Create(Owner: TScriptElement); override;

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
  TWarningsElement       = class(TBooleanAttributeElement);
  TUseLibraryPathElement = class(TBooleanAttributeElement);
  TUseCFGElement         = class(TBooleanAttributeElement);

  TDCUOutputElement  = class(TPathAttributeElement);
  TEXEOutputElement  = class(TPathAttributeElement);

  TOptionElement = class(TScriptElement)
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

  TUsePackageElement = class(TOptionElement)
  protected
    FPath :TPath;
  public
    procedure Init; override;
  published
    property Path :TPath read FPath write FPath;
  end;

  TMapElement = class(TCustomAttributeElement)
  protected
    FValue :TMapType;
  public
  published
    property value :TMapType read FValue write FValue;
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
    vers := StringToArray(versions);
    for i := 0 to High(vers) do
    begin
       if StrLeft(vers[i], 2) <> '.0' then
         vers[i] := vers[i] + '.0';
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


procedure TCustomDelphiTask.HandleOutputLine(Line: string);
begin
 //if not XPerlre.regex.Match('^(.*\([0-9]+\)) *([A-Z][a-z]+:.*$)', Msg) then
 if (Pos(':', Line) <> 0)
 and XPerlre.regex.Match('^(.*)(\([0-9]+\)) *([HWEF][a-z]+:.*)$', Line) then
 begin
   with regex do
     Line := ToRelativePath(ToPath(SubExp[1].Text)) + ' ' + SubExp[2].Text + #10 + SubExp[3].Text;
   if (Pos('Fatal', Line) <> 0) or  (Pos('Error', Line) <> 0) then
     TaskFailure(Line)
   else
     Log(vlWarnings, Line);

    (*!!!
   with regex do
   begin
     inherited Log(Level, ToRelativePath(ToPath(SubExp[1].Text)) + ' ' + SubExp[2].Text);
     inherited Log(Level, regex.SubExp[3].Text);
   end;
   *)
 end
 else if (Pos('Fatal', Line) <> 0) or  (Pos('Error', Line) <> 0) then
   TaskFailure(Line)
 else if (Pos('Wargint', Line) <> 0) then
     Log(vlWarnings, Line)
 else
   inherited HandleOutputLine(Line);
end;

{ TDelphiCompileTask }

constructor TDelphiCompileTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  SkipLines  := 1;
  quiet      := true;

  FUnitPaths      := TUnitPathElement.Create(Self);
  FResourcePaths  := TResourcePathElement.Create(Self);
  FIncludePaths   := TIncludePathElement.Create(Self);
  FDefines        := TStringList.Create;
  FPackages       := TStringList.Create;
  FWarnings       := True;
end;

destructor TDelphiCompileTask.Destroy;
begin
  FreeAndNil(FDefines);
  FreeAndNil(FPackages);
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
  try
    inherited Execute;
  finally
    RestoreCFGs;
  end;
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
  Executable := ToWantPath(FindCompiler);
  Result := inherited BuildExecutable;
end;

function TDelphiCompileTask.BuildArguments: string;
  function PathOpt(Opt :string; Path :TPath) :string;
  begin
    Result := Format(' -%s"%s"', [Opt, ToSystemPath(Path)] );
  end;

var
  Sources: TPaths;
  d      : Integer;
  s      : Integer;
  p      : Integer;
  PS     : TStringArray;
  Paths  : TPaths;
  cfg    : TPath;
  Delphi : string;
begin
  Result := inherited BuildArguments;

  Delphi := FindDelphiDir;

  Log(vlVerbose, 'sources %s', [ToRelativePath(source)]);
  Sources := WildPaths.Wild(Source, BasePath);

  for s := Low(Sources) to High(Sources) do
  begin
    Log(vlVerbose, 'source %s', [ToRelativePath(Sources[s])]);
    Result := Result + ' "' + ToSystemPath(Sources[s]) +  '"';
  end;

  if not usecfg then
  begin
    try
      for s := Low(Sources) to High(Sources) do
      begin
        if LowerCase(StrRight(Sources[s], 4)) = '.dpr' then
        begin
          cfg := Sources[s];
          cfg := StrLeft(cfg, Length(cfg)-4);
          if PathIsFile(cfg + '.cfg') then
          begin
            Log(vlVerbose, 'Renaming configuration file for %s', [ Sources[s] ]);
            WildPaths.MoveFile(cfg + '.cfg', cfg + __RENAMED_CFG_EXT);
          end;
        end;
      end;
    except
      Log(vlWarnings, 'Could rename configuration file: %s', [cfg]);
    end;
  end
  else
    Log(vlVerbose, 'usecfg=true');

  if exeoutput <> '' then
  begin
    Log(vlVerbose, 'exeoutput=' + ToRelativePath(exeoutput));
    Result := Result + PathOpt('E', exeoutput);
  end;

  if dcuoutput <> '' then
  begin
    Log(vlVerbose, 'dcuoutput=' + ToRelativePath(dcuoutput));
    Result := Result + PathOpt('N', dcuoutput);
  end;

  if console then
  begin
    Log(vlVerbose, 'console=true');
    Result := Result + ' -CC'
  end
  else
    Result := Result + ' -CG';

  if warnings then
    Result := Result + ' -W'
  else
  begin
    Log(vlVerbose, 'warnings=false');
    Result := Result + ' -W-';
  end;


  if quiet then
    Result := Result + ' -Q'
  else
    Log(vlVerbose, 'verbose=true');

  if build then
  begin
    Log(vlVerbose, 'build=true');
    Result := Result + ' -B'
  end
  else if make then
  begin
    Log(vlVerbose, 'make=true');
    Result := Result + ' -M';
  end;

  if optimize then
  begin
    Log(vlVerbose, 'optimize=true');
    Result := Result + ' -$O+'
  end
  else
    Result := Result + ' -$O-';

  if debug then
  begin
    Log(vlVerbose, 'debug=true');
    Result := Result + ' -$D+ -$L+ -$R+ -$Q+ -$C+ -GD'
  end
  else if optimize then
    Result := Result + ' -$D- -$L- -$R- -$Q- -$C-';

  case map of
    segments : Result := Result + ' -GS';
    publics  : Result := Result + ' -GP';
    detailed : Result := Result + ' -GD';
  end;

  for d := 0 to FDefines.Count-1 do
  begin
    Log(vlVerbose, 'define %s', [FDefines.Names[d]]);
    Result := Result + ' -D' + FDefines.Names[d];
  end;


  for p := 0 to FPackages.Count-1 do
  begin
    Log(vlVerbose, 'package %s', [FPackages[p]]);
    Result := Result + PathOpt('LU', FPackages[p]);
  end;

  PS := nil;
  if not useLibraryPath then
  begin
    Result := Result + ' -U' + Delphi + '\Lib';
    Result := Result + ' -R' + Delphi + '\Lib';
  end
  else
  begin
    Log(vlVerbose, 'uselibrarypath=true');
    PS := StringToArray(ReadLibraryPaths, ';');
    try
      for p := 0 to High(PS) do
      begin
        PS[p] := Trim(PS[p]);
        if PS[p] <> '' then
        begin
          PS[p] := StringReplace(PS[p], '$(DELPHI)', Delphi, [rfReplaceAll, rfIgnoreCase]);
          Result := Result + ' -U' + PS[p];
          Result := Result + ' -R' + PS[p];
        end;
      end;
    finally
      PS := nil;
    end;
  end;

  Paths := FUnitPaths.Paths;
  for p := Low(paths) to High(Paths) do
  begin
    Log(vlVerbose, 'unitpath %s', [ToRelativePath(Paths[p])]);
    Result := Result + PathOpt('U', Paths[p]);
  end;

  Paths := FResourcePaths.Paths;
  for p := Low(paths) to High(Paths) do
  begin
    Log(vlVerbose, 'resourcepath %s', [ToRelativePath(Paths[p])]);
    Result := Result + PathOpt('R', Paths[p]);
  end;

  Paths := FIncludePaths.Paths;
  for p := Low(paths) to High(Paths) do
  begin
    Log(vlVerbose, 'includepath %s', [ToRelativePath(Paths[p])]);
    Result := Result + PathOpt('I', Paths[p]);
  end;

  if Length(Sources) = 0 then
    TaskFailure(Format('could not find %s to compile', [PathConcat(BasePath, source)]));
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

procedure TDelphiCompileTask.AddPackage(Path: string);
begin
  FPackages.Add(Path);
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

procedure TDelphiCompileTask.RestoreCFGs;
var
  cfgs :TPaths;
  cfg  :TPath;
  c    :Integer;
begin
  cfgs := nil;
  if not usecfg then
  begin
    cfgs := Wild('*' +__RENAMED_CFG_EXT);
    try
      for c := Low(cfgs) to High(cfgs) do
      begin
        cfg := cfgs[c];
        Delete(cfg, 1+Length(cfg) - Length(__RENAMED_CFG_EXT), Length(__RENAMED_CFG_EXT));
        cfg := cfg + '.cfg';
          WildPaths.MoveFile(cfgs[c], cfg);
      end;
    except
      Log(vlWarnings, 'Could not restore configuration file: %s', [cfg]);
    end;
  end;
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
  Executable := ToWantPath(FindBRCC);
  Result := inherited BuildExecutable;
end;

constructor TResourceCompileTask.Create(Owner: TScriptElement);
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
  versions := '2,3,4,5,6';
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

constructor TPathSet.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  AddDefaultPatterns;
end;

{ TOptionElement }

function TOptionElement.dcc: TDelphiCompileTask;
begin
  Result := Owner as TDelphiCompileTask;
end;

{ TUsePackageElement }

procedure TUsePackageElement.Init;
begin
  inherited Init;
  RequireAttribute('path');
  dcc.AddPackage(Path);
end;

{ TMapElement }

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
                         TWarningsElement,
                         TUseLibraryPathElement,
                         TUseCFGElement,

                         TDCUOutputElement,
                         TEXEOutputElement,

                         TUsePackageElement,

                         TMapElement
                         ]);
end.
