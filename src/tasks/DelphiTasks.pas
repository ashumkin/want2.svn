{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca (R)                               }
{    ~                                         }
{     Copyright (C) 1995,2001 Juancarlo A�ez   }
{     All rights reserved.                     }
{            http://www.suigeneris.org/juanca  }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{#(@)$Id$}

{
  Contributors:
    Dan Hughes <dan@multiedit.com>
}

unit DelphiTasks;

interface
uses
  Windows,
  SysUtils,
  Classes,
  TypInfo,

  JclBase,
  JclSysUtils,
  JclMiscel,
  JclSysInfo,
  JclRegistry,
  JclStrings,

  JalStrings,

  XPerlRe,

  WantUtils,
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

  TDelphiVersion = record
    Version    :string;
    Directory  :string;
    ToolPath   :string;
  end;

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
  TObjectPathElement   = class(TPathSet);

  TCustomDelphiTask = class(TCustomExecTask)
  protected
    FVersions : string;

    FVersionFound :string;
    FDelphiDir    :string;
    FToolPath     :string;
    FVersionNumber:double;
    procedure HandleOutputLine(Line :string); override;

    class function RootForVersion(version: string): string;
    class function ReadDelphiDir(ver :string = '') :string;
    class function ReadUserOption(Key, Name, Ver :string):string;
    class function ReadMachineOption(Key, Name, Ver :string):string;

    class function FindDelphi(V: string): TDelphiVersion;
    class function ToolName :string; virtual;  abstract;

    procedure FindTool;
  public
    function BuildExecutable: string; override;
    procedure Execute;  override;

    property DelphiDir :string read FDelphiDir;
    property ToolPath  :string read FToolPath;
  published
    property versions: string read FVersions  write FVersions;
  end;

type
  TWarning = (
    UNSAFE_CODE,
    SYMBOL_PLATFORM,
    SYMBOL_DEPRECATED,
    UNIT_PLATFORM,
    UNIT_DEPRECATED
  );
  TWarnings = set of TWarning;


  TDelphiCompileTask = class(TCustomDelphiTask)
  protected
    FExesPath: TPath;
    FDCUPath : TPath;
    FBPLPath : TPath;
    FDCPPath : TPath;
    FSource  : TPath;

    FQuiet          : boolean;
    FMake           : boolean;
    FBuild          : boolean;
    FOptimize       : boolean;
    FDebug          : boolean;
    FConsole        : boolean;
    FEnableWarnings : boolean;
    FUseLibraryPath : boolean;
    FUseCFG         : boolean;
    FLongStrings    : boolean;

    FMap            : TMapType;

    FUnitPaths      : TUnitPathElement;
    FResourcePaths  : TResourcePathElement;
    FIncludePaths   : TIncludePathElement;
    FObjectPaths    : TObjectPathElement;

    FDefines        : TStrings;
    FPackages       : TStrings;
    FWarnings       : TWarnings;

    FRenamedCFGs    :boolean;

    function BuildArguments: string; override;

    class function ToolName :string; override;

    function ReadLibraryPaths :string;

    function OutputPathElements(const optionDescription,
          optionFlag : string;pathsToOutput : TPaths) : string;

    function PathOpt(Opt :string; Path :TPath) :string;
  public
    constructor Create(Owner: TScriptElement); override;
    destructor  Destroy; override;

    class function TagName: string; override;

    procedure Init; override;
    procedure Execute; override;

    procedure AddUnitPath(Path: TPath);
    procedure AddResourcePath(Path: TPath);
    procedure AddIncludePath(Path: TPath);
    procedure AddObjectPath(Path : TPath);
    procedure AddDefine(Name, Value :string);
    procedure AddPackage(Name :string);
    procedure AddWarning(Name :TWarning; Value :boolean);

    procedure RestoreCFGs;

  published
    property basedir; // from TTask

    function CreateUnitPath     :TUnitPathElement;

    function CreateResourcePath :TResourcePathElement;
    function CreateIncludePath  :TIncludePathElement;
    function CreateObjectPath   :TObjectPathElement;

    // these properties are mapped to XML attributes
    property Arguments;
    property ArgumentList stored False;
    property SkipLines;

    property exeoutput: TPath read FExesPath write FExesPath;
    property dcuoutput: TPath read FDCUPath  write FDCUPath;
    property bploutput: TPath read FBPLPath  write FBPLPath;
    property dcpoutput: TPath read FDCPPath  write FDCPPath;

    property quiet: boolean read FQuiet write FQuiet default true;
    property make:  boolean read FMake  write FMake;
    property build: boolean read FBuild write FBuild;

    property optimize: boolean read FOptimize write FOptimize;
    property debug:    boolean read FDebug    write FDebug;
    property console:  boolean read FConsole  write FConsole;
    property warnings: boolean read FEnableWarnings write FEnableWarnings default true;
    property usecfg:   boolean read FUseCFG   write FUseCFG;

    property longstrings :boolean read FLongStrings   write FLongStrings default true;

    property map       :TMapType read FMap write FMap;

    property uselibrarypath : boolean read FUseLibraryPath write FUseLibraryPath;

    property source : TPath read FSource  write FSource;
  end;

  TResourceCompileTask = class(TCustomDelphiTask)
  protected
    FFile:   string;
    FOutput: string;

    class function ToolName :string; override;

    function BuildArguments: string; override;
  public
    class function TagName: string; override;

    constructor Create(Owner: TScriptElement); override;

    procedure Init;    override;
    procedure Execute; override;
  published
    property _file:  string read FFile   write FFile;
    property output: string read FOutput write FOutput;
  end;

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
  public
    procedure Init; override;
  published
    property Name;
  end;

  TMapElement = class(TCustomAttributeElement)
  protected
    FValue :TMapType;
  public
  published
    property value :TMapType read FValue write FValue;
  end;

  TWarningElement = class(TScriptElement)
  protected
    FName  :TWarning;
    FValue :boolean;

  public
    procedure Init; override;
  published
    property Name :TWarning read FName write FName;
    property Value :boolean read FValue write FValue;
  end;


implementation
var
  WarningVersion : Array[TWarning] of double=(7.0,6.0,6.0,6.0,6.0);

{ TCustomDelphiTask }

class function TCustomDelphiTask.FindDelphi(V: string) : TDelphiVersion;
var
  vers: TStringArray;
  i     :Integer;
  Path  :string;
  Tool  :string;
begin
  FillChar(Result, SizeOf(Result), #0);
  vers := nil;
  if V = '' then begin
    WantUtils.GetEnvironmentVar('delphi_version', V, true);
  end;
  if V = '' then
     V := '10,9,8,7,6,5,4';

  vers := StringToArray(V);
  for i := 0 to High(vers) do
  begin
     if StrLeft(vers[i], 2) <> '.0' then
       vers[i] := vers[i] + '.0';
     Path := ReadDelphiDir(vers[i]);
     if Path <> '' then
     begin
       Tool := Path + '\' + ToolName;
       if FileExists(Tool) then // found it !
       begin
         Result.Version   := vers[i];
         Result.Directory := Path;
         Result.ToolPath  := Tool;
         BREAK;
       end;
     end;
  end;
end;



procedure TCustomDelphiTask.FindTool;
begin
  with FindDelphi(versions) do
  begin
    FVersionFound := Version;
    FDelphiDir    := Directory;
    FToolPath     := ToolPath;
    DecimalSeparator := '.';
    FVersionNumber := StrToFloat(Version);
    GetFormatSettings();
  end;
  if FToolPath = '' then
    TaskError('Could not find ' + ToolName);
end;


class function TCustomDelphiTask.ReadDelphiDir(ver: string): string;
begin
  assert(ver <> '');
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, RootForVersion(ver), DelphiRootKey, '');
end;

class function TCustomDelphiTask.ReadMachineOption(Key, Name, Ver: string): string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, RootForVersion(Ver)+'\'+Key, Name, '');
end;

class function TCustomDelphiTask.ReadUserOption(Key, Name, Ver: string): string;
begin
  Result := RegReadStringDef(HKEY_CURRENT_USER, RootForVersion(Ver)+'\'+Key, Name, '');
end;

class function TCustomDelphiTask.RootForVersion(version: string): string;
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
   if (Pos('Fatal:', Line) <> 0) or  (Pos('Error:', Line) <> 0) then
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
 else if (Pos('Fatal:', Line) <> 0) or  (Pos('Error:', Line) <> 0) then
   TaskFailure(Line)
 else if (Pos('Warning', Line) <> 0) then
     Log(vlWarnings, Line)
 else
   inherited HandleOutputLine(Line);
end;

procedure TCustomDelphiTask.Execute;
begin
  FindTool;
  Executable := ToWantPath(ToolPath);
  inherited;
end;

function TCustomDelphiTask.BuildExecutable: string;
begin
  FindTool;

  Executable := ToWantPath(ToolPath);

  Result := inherited BuildExecutable;
end;

{ TDelphiCompileTask }

constructor TDelphiCompileTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  SkipLines  := 2;
  quiet      := true;

  FUnitPaths      := TUnitPathElement.Create(Self);
  FResourcePaths  := TResourcePathElement.Create(Self);
  FIncludePaths   := TIncludePathElement.Create(Self);
  FObjectPaths    := TObjectPathElement.Create(Self);

  FDefines        := TStringList.Create;
  FPackages       := TStringList.Create;

  FEnableWarnings := true;
  FLongStrings    := true;

  AddWarning(UNSAFE_CODE,       false);
  AddWarning(SYMBOL_PLATFORM,   false);
  AddWarning(SYMBOL_DEPRECATED, false);
  AddWarning(UNIT_PLATFORM,     false);
  AddWarning(UNIT_DEPRECATED,   false);
end;

destructor TDelphiCompileTask.Destroy;
begin
  FreeAndNil(FDefines);
  FreeAndNil(FPackages);
  FreeAndNil(FWarnings);
  inherited Destroy;
end;

procedure TDelphiCompileTask.Init;
begin
  inherited Init;
  RequireAttribute('source');
end;


class function TDelphiCompileTask.ToolName: string;
begin
  Result := 'bin\dcc32.exe';
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

class function TDelphiCompileTask.TagName: string;
begin
  Result := 'dcc';
end;

function TDelphiCompileTask.OutputPathElements(const optionDescription,
      optionFlag : string;pathsToOutput : TPaths) : string;
var
  path : integer;
begin
  result := '';
  for path := Low(pathsToOutput) to High(pathsToOutput) do
  begin
    Log(vlVerbose, '%s %s', [optionDescription,ToRelativePath(pathsToOutput[path])]);
    Result := Result + PathOpt(optionFlag, pathsToOutput[path]);
  end;
end;

function TDelphiCompileTask.PathOpt(Opt :string; Path :TPath) :string;
begin
  Result := Format(' -%s%s', [Opt, ToSystemPath(ToPath(Path))] );
end;

function TDelphiCompileTask.BuildArguments: string;
var
  Sources: TPaths;
  d      : Integer;
  s      : Integer;
  p      : Integer;
  w      : TWarning;
  PS     : TStringArray;
  cfg    : TPath;
  wname  : string;
begin
  Result := inherited BuildArguments + ' ';

  Log(vlVerbose, 'sources %s', [ToRelativePath(source)]);
  Sources := WildPaths.Wild(Source, BasePath);

  for s := Low(Sources) to High(Sources) do
  begin
    Log(vlVerbose, 'source %s', [ToRelativePath(Sources[s])]);
    Result := Result + ' ' + ToSystemPath(Sources[s]);
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

  if bploutput <> '' then
  begin
    Log(vlVerbose, 'bploutput=' + ToRelativePath(bploutput));
    Result := Result + PathOpt('LE', bploutput);
  end;

  if dcpoutput <> '' then
  begin
    Log(vlVerbose, 'dcpoutput=' + ToRelativePath(dcpoutput));
    Result := Result + PathOpt('LN', dcpoutput);
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

  if longstrings then
    Result := Result + ' -$H+'
  else
    Result := Result + ' -$H-';

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


  for w := Low(TWarning) to High(TWarning) do
  begin
    if  FVersionNumber >= WarningVersion[w] then
    begin
      wname := GetEnumName(TypeInfo(TWarning), Ord(w));
      if w in FWarnings then
        Result := Result + ' -W+' + wname
      else
        Result := Result + ' -W-' + wname;
    end;
  end;

  for p := 0 to FPackages.Count-1 do
  begin
    Log(vlVerbose, 'package %s', [FPackages[p]]);
    Result := Result + ' -LU' + FPackages[p];
  end;

  PS := nil;
  if not useLibraryPath then
  begin
    Result := Result + PathOpt('U', DelphiDir + '\Lib');
    Result := Result + PathOpt('R', DelphiDir + '\Lib');
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
          PS[p] := StringReplace(PS[p], '$(DELPHI)', DelphiDir, [rfReplaceAll, rfIgnoreCase]);
          Result := Result + PathOpt('U', PS[p]);
          Result := Result + PathOpt('R', PS[p]);
        end;
      end;
    finally
      PS := nil;
    end;
  end;

  result := result + OutputPathElements('unitpath','U',FUnitPaths.Paths);

  result := result + OutputPathElements('resourcepath','R',FResourcePaths.Paths);

  result := result + OutputPathElements('includepath','I',FIncludePaths.Paths);

  result := result + OutputPathElements('objectpath','O',FObjectPaths.Paths);


  if Length(Sources) = 0 then
    TaskFailure(Format('could not find %s to compile', [ToSystemPath(PathConcat(BasePath, source))]));
end;

procedure TDelphiCompileTask.AddUnitPath(Path: TPath);
begin
  FUnitPaths.Include(Path);
end;

procedure TDelphiCompileTask.AddIncludePath(Path: TPath);
begin
  FIncludePaths.Include(Path);
end;

procedure TDelphiCompileTask.AddObjectPath(Path : TPath);
begin
  FObjectPaths.Include(Path);
end;

procedure TDelphiCompileTask.AddResourcePath(Path: TPath);
begin
  FResourcePaths.Include(Path);
end;

function TDelphiCompileTask.ReadLibraryPaths: string;
begin
  Result := ReadUserOption('Library', 'Search Path', FVersionFound) + ';' +
            ReadUserOption('Library', 'SearchPath',  FVersionFound)
end;


procedure TDelphiCompileTask.AddDefine(Name, Value: string);
begin
  if Trim(Value) <> '' then
    FDefines.Values[Name] := Value
  else
    FDefines.Add(Name + '=');
end;

procedure TDelphiCompileTask.AddPackage(Name: string);
begin
  FPackages.Add(Name);
end;

function TDelphiCompileTask.CreateUnitPath: TUnitPathElement;
begin
  Result := FUnitPaths;
end;

function TDelphiCompileTask.CreateIncludePath: TIncludePathElement;
begin
  Result := FIncludePaths;
end;

function TDelphiCompileTask.CreateObjectPath: TObjectPathElement;
begin
  Result := FObjectPaths;
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


procedure TDelphiCompileTask.AddWarning(Name: TWarning; Value :boolean);
begin
  if Value then
    Include(FWarnings, Name)
  else
    Exclude(FWarnings, Name);
end;

{ TResourceCompileTask }

function TResourceCompileTask.BuildArguments: string;
begin
  Result := inherited BuildArguments;

  Result := Result + ' -r ' + ToSystemPath(_file);

  if output <> '' then
    Result := Result + ' -fo' + ToSystemPath(output);
end;

constructor TResourceCompileTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  SkipLines := 2;
end;

procedure TResourceCompileTask.Execute;
begin
  Log(ToRelativePath(_file));
  inherited;
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

class function TResourceCompileTask.ToolName: string;
begin
  Result := 'bin\brcc32.exe';
end;

{ TDefineElement }

procedure TDefineElement.Init;
begin
  inherited Init;
  Log(vlDebug, '%s %s=%s', [TagName, Name, Value]);
  RequireAttribute('name');
  dcc.AddDefine(Name, Value);
end;


{ TPathSet }

constructor TPathSet.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  AddDefaultPatterns;
end;

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

{ TOptionElement }

function TOptionElement.dcc: TDelphiCompileTask;
begin
  Result := Owner as TDelphiCompileTask;
end;

{ TUsePackageElement }

procedure TUsePackageElement.Init;
begin
  inherited Init;
  RequireAttribute('name');
  dcc.AddPackage(Name);
end;

{ TMapElement }

{ TWarningElement }

procedure TWarningElement.Init;
begin
  inherited;
  TDelphiCompileTask(Owner).AddWarning(Name, Value);
end;

initialization
  RegisterTasks( [TDelphiCompileTask, TResourceCompileTask]);
  RegisterElements(TDelphiCompileTask, [
                         TDefineElement ,
                         TUsePackageElement,
                         TWarningElement
                         ]);
  with TDelphiCompileTask.FindDelphi('') do
  begin
    JclSysInfo.SetEnvironmentVar('delphi.version', Version);
    JclSysInfo.SetEnvironmentVar('delphi.dir',     Directory);
  end;
end.
