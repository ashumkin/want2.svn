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

unit ExecTasks;

interface
uses
  SysUtils,
  Math,
  Classes,

  JclBase,
  JclMiscel,
  JclSysInfo,
  JclSecurity,

  JalPaths,

  XPerlRE,

  WildPaths,
  WantUtils,
  WantClasses,
  Attributes,
  ChildProcesses;



type
  TArgElement = class(TScriptElement)
  protected
    FVAlue :string;
    FPath  :IPath;
  public
    procedure Init; override;
  published
    property value :string read FValue write FValue;
    property path  :IPath  read FPath  write FPath;
  end;

  TCustomExecTask = class(TTask)
  protected
    FOS          :string;
    FExecutable  :TPath;
    FArguments   :TStrings;
    FSkipLines   :Integer;
    FFailOnError :boolean;
    FErrorLevel  :Integer;
    FTimeOut     :Longint;
    FOutput      :string;
    FQuiet       :boolean;

    FFilters        :TStrings;
    FErrorFilters   :TStrings;
    FWarningFilters :TStrings;

    FDefaultFilters :boolean;

    procedure Init; override;
    
    function BuildExecutable :string; virtual;
    function BuildArguments  :string; virtual;
    function BuildCmdLine    :string; virtual;

    function  GetArguments :string;
    procedure SetArguments(Value :string);
    procedure SetArgumentList(Value :TStrings);

    function  GetFilters: string;
    procedure AddFilter(const Value: string);

    function  GetErrorFilters: string;
    procedure AddErrorFilter(const Value: string);

    function  GetWarningFilters: string;
    procedure AddWarningFilter(const Value: string);

    procedure Run(CmdLine: string);
    procedure HandleOutput(Child :TChildProcess);
    procedure HandleOutputLine(Line :string); virtual;
  public
    constructor Create(Owner: TScriptElement);  override;
    destructor Destroy; override;

    procedure Execute; override;
    function  ToSystemPath(const Path: TPath; const Base: TPath = ''):string; override;

  protected
    property Arguments:    string   read GetArguments write SetArguments;
    property ArgumentList: TStrings read FArguments   write SetArgumentList stored False;
    property Executable:   TPath    read FExecutable  write FExecutable;
    property SkipLines:    Integer  read FSkipLines   write FSkipLines;
    property OS:           string   read FOS          write FOS;
    property failonerror:  boolean  read FFailOnError write FFailOnError default True;
    property errorlevel:   Integer  read FErrorLevel  write FErrorLevel;
    property output:       string   read FOutput      write FOutput;
    property quiet:        boolean  read FQuiet       write FQuiet;

    property filter:       string   read GetFilters    write AddFilter;
    property errorfilter:  string   read GetErrorFilters    write AddErrorFilter;
    property warningfilter:string   read GetWarningFilters  write AddWarningFilter;

    property defaultfilters :boolean read FDefaultFilters write FDefaultFilters;
    {:@TODO Implement a TWaitableTimer class to implement timeouts.
      Use Windows.CreateWaitableTimer and Windows.SetWaitableTimer.
      !!!
    }
    property timeout:      Longint  read FTimeout     write FTimeout;
  published
  end;

  TExecTask = class(TCustomExecTask)
  public
    procedure Init; override;
    procedure Execute; override;
  published
    property basedir;
    property Arguments;
    property ArgumentList stored False;
    property Executable;
    property SkipLines :Integer     read FSkipLines   write FSkipLines;
    property OS;
    property failonerror;
    property errorlevel;
    property output;
    property quiet;
    property filter;
    property errorfilter;
    property warningfilter;
    property defaultfilters;
  end;

  // this class will pass commands through the command processor
  TShellTask = class(TExecTask)
  protected
    function BuildExecutable :string; override;
  end;

implementation

{ TExecTask }

procedure TExecTask.Execute;
begin
  Log(PathFile(Executable));
  Log(vlDebug, 'executable=' + Executable);
  Log(vlDebug, 'arguments='  + BuildArguments);
  inherited Execute;
end;

procedure TExecTask.Init;
begin
  inherited Init;
  RequireAttribute('executable');
end;

{ TShellTask }

function TShellTask.BuildExecutable: string;
begin
  Result := inherited BuildExecutable;
  if IsWinNT then
    Result := 'cmd.exe /c ' + Result
  else
    Result := 'command.com /c ' + Result;
end;

{ TCustomExecTask }

function TCustomExecTask.BuildExecutable: string;
begin
  Result := WildPaths.ToSystemPath(Executable);
end;

function TCustomExecTask.BuildCmdLine: string;
begin
  Result := Trim(Trim(BuildExecutable) + ' ' + BuildArguments);
end;

function TCustomExecTask.BuildArguments: string;
var
  i: Integer;
begin
  Result := '';
  { Arguments.CommaText screws with the contents. See unit test }
  for i := 0 to ArgumentList.Count - 1 do
    Result := Result + ' ' +ArgumentList[i];
  Result := Trim(Result);
end;

constructor TCustomExecTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FArguments      := TStringList.Create;
  FFilters        := TStringList.Create;
  FErrorFilters   := TStringList.Create;
  FWarningFilters := TStringList.Create;
  FFailOnError := True;
end;

destructor TCustomExecTask.Destroy;
begin
  FreeAndNil(FArguments);
  FreeAndNil(FFilters);
  FreeAndNil(FErrorFilters);
  FreeAndNil(FWarningFilters);
  inherited;
end;

procedure TCustomExecTask.Execute;
var
  CmdLine :string;
begin
  Log(vlDebug, 'currentDir=%s', [CurrentDir] );
  CmdLine := BuildCmdLine;
  Log(vlVerbose, CmdLine);
  Run(CmdLine);
end;


function TCustomExecTask.GetArguments: string;
begin
  Result := FArguments.CommaText;
end;

procedure TCustomExecTask.SetArguments(Value: string);
begin
  FArguments.CommaText := Value;
end;

procedure TCustomExecTask.SetArgumentList(Value: TStrings);
begin
  FArguments.Assign(Value);
end;

procedure TCustomExecTask.Run(CmdLine :string);
var
  Child    :TChildProcess;
  ExitCode :Integer;
begin
  if ChildProcessClass = nil then
    TaskError('No Child Process implementation?');

  Child := ChildProcessClass.Create;
  try
    try
      Child.Run(CmdLine);
    except
      on e :Exception do
        TaskFailure(e.Message, ExceptAddr);
    end;
    HandleOutput(Child);
    ExitCode := Child.ExitCode;
    if (ExitCode > errorlevel) and FailOnError then
      TaskFailure(Format('Exit code was %d', [ExitCode]));
  finally
    FreeAndNil(Child);
  end;
end;


procedure TCustomExecTask.HandleOutput(Child :TChildProcess);
const
  TimeOutMillis = 100;
var
  Line          :String;
  LineNo        :Integer;
  OutFile       :Text;
begin
  LineNo := 0;

  if output <> '' then
  begin
    Log(vlVerbose, 'output to "%s"', [output]);
    Assign(OutFile, ToSystemPath(output));
    Rewrite(OutFile);
  end;

  try
    while not Child.EOF
    and (LineNo < SkipLines) do
    begin
      Child.ReadLn;
      Inc(LineNo);
    end;

    while not Child.EOF do
    begin
      Line := Child.ReadLn;
      //!!! Inc(LineNo); // never used
      if output <> '' then
        Writeln(OutFile, Line)
      else
        HandleOutputLine(Line);
    end;
  finally
    if output <> '' then
      Close(OutFile);
  end;
end;


procedure TCustomExecTask.HandleOutputLine(Line: string);
  function MatchFilters(F :TStrings) :boolean;
  var
    i :Integer;
  begin
    Result := false;
    for i := 0 to F.Count-1 do
    begin
      if XPerlre.regex.Match(F[i], Line) then
      begin
        Result := True;
        break
      end;
    end;
  end;

begin
  if not quiet then
    Log(Line)
  else
  begin
    if MatchFilters(FErrorFilters) then
      Log(vlErrors, Line)
    else if MatchFilters(FWarningFilters) then
      Log(vlWarnings, Line)
    else if MatchFilters(FFilters) then
      Log(Line)
  end;
end;

function TCustomExecTask.GetFilters: string;
begin
  Result := FFilters.CommaText;
end;

procedure TCustomExecTask.AddFilter(const Value: string);
begin
  FFilters.Add(Value);
end;

procedure TCustomExecTask.AddErrorFilter(const Value: string);
begin
  FErrorFilters.Add(Value);
end;

function TCustomExecTask.GetErrorFilters: string;
begin
  Result := FErrorFilters.CommaText;
end;

procedure TCustomExecTask.AddWarningFilter(const Value: string);
begin
  FWarningFilters.Add(Value);
end;

function TCustomExecTask.GetWarningFilters: string;
begin
  Result := FWarningFilters.CommaText;
end;

procedure TCustomExecTask.Init;
begin
  inherited Init;
  if defaultfilters then
  begin
    quiet         := true;
    errorfilter   := '[Ee]rror';
    errorfilter   := 'ERROR';
    errorfilter   := '[Ff]atal';
    errorfilter   := 'FATAL';
    warningfilter := '[Ww]arning';
    warningfilter := 'WARNING';
  end;
end;

function TCustomExecTask.ToSystemPath(const Path, Base: TPath): string;
begin
  Result :=inherited ToSystemPath(Path, Base);
  if Pos(' ', Result) > 0 then
    Result := '"' + Trim(Result) + '"';
end;

{ TArgElement }

procedure TArgElement.Init;
begin
  inherited Init;
  if value = '' then
  begin
    if GetAttribute('path') = '' then
      RequireAttribute('value');
    RequireAttribute('path');
    value := ToSystemPath(path.asString);
  end;
  (Owner as TCustomExecTask).FArguments.Add(Value);
end;

initialization
  RegisterTasks([TCustomExecTask, TExecTask, TShellTask]);
  RegisterElements(TCustomExecTask, [TArgElement]);
end.

