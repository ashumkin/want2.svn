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

unit ScriptRunner;

interface

uses
  SysUtils,
  Classes,

  JclFileUtils,

  JALStrings,

  WildPaths,

  WantUtils,
  WantClasses,
  BuildListeners,
  ScriptParser;


type
  TScriptRunner = class
  protected
    FListener    :TBuildListener;
    FListenerCreated :boolean;

    procedure DoCreateListener;  virtual;
    procedure CreateListener;    virtual;
    procedure SetListener(Value :TBuildListener);

    procedure BuildTarget(Target :TTarget);
    procedure ExecuteTask(Task :TTask);

  public
    constructor Create;
    destructor  Destroy; override;

    procedure LoadProject(Project :TProject; BuildFile: TPath; SearchUp :boolean = false);

    procedure BuildProject(Project :TProject; Target: string = '');   overload;
    procedure BuildProject(Project :TProject; Targets: TStringArray); overload;

    procedure Build(BuildFile: TPath; Targets :TStringArray; Level :TLogLevel = vlNormal); overload;
    procedure Build(BuildFile: TPath; Target  :string;       Level :TLogLevel = vlNormal); overload;
    procedure Build(BuildFile: TPath; Level   :TLogLevel = vlNormal); overload;

    procedure Log(Level: TLogLevel; Msg: string);

    class function DefaultBuildFileName: TPath;
    function FindBuildFile(BuildFile: TPath; SearchUp :boolean = False):TPath; overload;
    function FindBuildFile(SearchUp :boolean= False) :TPath; overload;

    property Listener :TBuildListener read FListener write SetListener;
    property ListenerCreated :boolean read FListenerCreated;
  end;

implementation



{ TScriptRunner }

constructor TScriptRunner.Create;
begin
  inherited Create;
  DoCreateListener;
end;

destructor TScriptRunner.Destroy;
begin
  if ListenerCreated then
  begin
    FListener.BuildFinished;
    FreeAndNil(FListener);
  end;
  inherited Destroy;
end;

procedure TScriptRunner.LoadProject(Project :TProject; BuildFile: TPath; SearchUp :boolean);
begin
  if not IsLocalPath(BuildFile) then
    BuildFile := ToPath(BuildFile);
  BuildFile := FindBuildFile(BuildFile, SearchUp);

  try
    TScriptParser.Parse(Project, BuildFile);
    Listener.BuildFileLoaded(Project, WildPaths.ToRelativePath(BuildFile, CurrentDir));
  except
    on e :Exception do
    begin
      Listener.BuildFailed(Project, e.Message);
      raise;
    end;
  end;
end;

procedure TScriptRunner.Build( BuildFile: TPath;
                               Targets:    TStringArray;
                               Level:      TLogLevel = vlNormal);
var
  Project :TProject;
begin
  Listener.Level := Level;
  Project := TProject.Create;
  try
    Project.Listener := Listener;
    try
      LoadProject(Project, BuildFile);
      BuildProject(Project, Targets);
    except
      on e :EWantException do
      begin
        Log(vlDebug, e.Message);
        raise;
      end;
      on e :Exception do
      begin
        Log(vlDebug, e.Message);
        Listener.BuildFailed(Project, e.Message);
        raise;
      end;
    end;
  finally
    Project.Free;
  end;
end;


procedure TScriptRunner.DoCreateListener;
begin
  if Listener = nil then
  begin
    CreateListener;
    FListenerCreated := True;
  end;
end;

procedure TScriptRunner.CreateListener;
begin
  FListener := TBasicListener.Create;
end;

procedure TScriptRunner.Build(BuildFile: TPath; Level: TLogLevel);
begin
  Build(BuildFile, nil, Level);
end;

procedure TScriptRunner.Build(BuildFile: TPath; Target: string; Level: TLogLevel);
var
  T :TStringArray;
begin
  SetLength(T, 1);
  T[0] := Target;
  Build(BuildFile, T, Level);
end;

procedure TScriptRunner.BuildProject(Project: TProject; Targets: TStringArray);
var
  t       :Integer;
begin
  if Length(Targets) = 0 then
    BuildProject(Project)
  else
  begin
    for t := Low(Targets) to High(Targets) do
      BuildProject(Project, Targets[t]);
  end;
end;

procedure TScriptRunner.BuildProject(Project :TProject; Target: string);
var
  i:       Integer;
  Sched:   TTargetArray;
  LastDir: TPath;
begin
  Sched := nil;
  Listener.ProjectStarted(Project);
  try
    try
      Sched := nil;
      Project.Listener := Listener;

      Log(vlDebug, Format('basedir="%s"',   [Project.RootPath]));
      Log(vlDebug, Format('basedir="%s"',   [Project.BaseDir]));
      Log(vlDebug, Format('basepath="%s"',  [Project.BasePath]));

      if Target = '' then
      begin
        if Project._Default <> '' then
          Target := Project._Default
        else
          raise ENoDefaultTargetError.Create('No default target');
      end;

      Project.Configure;

      Sched := Project.Schedule(Target);

      if Length(Sched) = 0 then
        Listener.Log(vlWarnings, 'Nothing to build')
      else
      begin
        LastDir := CurrentDir;
        try
          for i := Low(Sched) to High(Sched) do
          begin
              ChangeDir(Project.BasePath);
              BuildTarget(Sched[i]);
          end;
        finally
          ChangeDir(LastDir);
        end;
      end;
    except
      on e :Exception do
      begin
        Log(vlDebug, e.Message);
        if e is ETaskException then
          Listener.BuildFailed(Project)
        else
          Listener.BuildFailed(Project, e.Message);
        raise;
      end;
    end;
  finally
    Listener.ProjectFinished(Project);
    Project.Listener := nil;
  end;
end;

procedure TScriptRunner.BuildTarget(Target: TTarget);
var
  i: Integer;
  LastDir :TPath;
begin
  if not Target.Enabled then
    EXIT;

  Listener.TargetStarted(Target);

  Log(vlDebug, Format('basedir="%s"',   [Target.BaseDir]));
  Log(vlDebug, Format('basepath="%s"',  [Target.BasePath]));

  LastDir := CurrentDir;
  try
    ChangeDir(Target.BasePath);

    for i := 0 to Target.TaskCount-1 do
      ExecuteTask(Target.Tasks[i]);

    Listener.TargetFinished(Target);
  finally
    ChangeDir(LastDir)
  end;
end;


procedure TScriptRunner.ExecuteTask(Task: TTask);
var
  LastDir: TPath;
begin
  if not Task.Enabled then
  begin
    Log(vlVerbose, Format('skipping disabled task <%s>', [Task.TagName]));
    EXIT;
  end;
  Listener.TaskStarted(Task);

  Log(vlDebug, Format('basedir="%s"',   [Task.BaseDir]));
  Log(vlDebug, Format('basepath="%s"',  [Task.BasePath]));

  LastDir := CurrentDir;
  try
    try
      ChangeDir(Task.BasePath);
      Task.Execute;
      Listener.TaskFinished(Task);
    finally
      ChangeDir(LastDir);
    end;
  except
    on e: Exception do
    begin
      Log(vlDebug, 'caught: ' + e.Message);
      if e is EWantException then
      begin
        Listener.TaskFailed(Task, e.Message);
        raise
      end
      else
      begin
        Log(vlErrors, e.Message);
        Listener.TaskFailed(Task, e.Message);
        raise ETaskError.Create(e.Message);
      end;
    end;
  end;
end;


procedure TScriptRunner.Log(Level: TLogLevel; Msg: string);
begin
  Assert(Listener <> nil);
  Listener.Log(Level, Msg);
end;

procedure TScriptRunner.SetListener(Value: TBuildListener);
begin
  if FListener <> Value then
  begin
    if FListenerCreated then
    begin
      FListener.Free;
      FListener := nil;
    end;
    FListenerCreated := False;
    FListener := Value;
  end;
end;

class function TScriptRunner.DefaultBuildFileName: TPath;
var
  AppName :string;
begin
  AppName := ExtractFileName(GetModulePath(hInstance));
  Result  := ChangeFileExt(LowerCase(AppName),'.xml');
end;

function TScriptRunner.FindBuildFile(BuildFile: TPath; SearchUp: boolean): TPath;
var
  Dir: TPath;
begin
  if BuildFile = '' then
    Result := FindBuildFile(SearchUp)
  else
  begin
    Log(vlDebug, Format('Findind buildfile %s', [BuildFile]));
    Result := PathConcat(CurrentDir, BuildFile);
    Dir    := SuperPath(Result);

    Log(vlDebug, Format('Looking for "%s in "%s"', [BuildFile, Dir]));
    while not PathIsFile(Result)
    and SearchUp
    and (Dir <> '')
    and (Dir <> SuperPath(Dir))
    do
    begin
      if PathIsDir(Dir) then
      begin
        Result := PathConcat(Dir, BuildFile);
        Dir    := SuperPath(Dir);
        Log(vlDebug, Format('Looking for "%s in "%s"', [BuildFile, Dir]));
      end
      else
        break;
    end;

    if not PathIsFile(Result) then
      Result := BuildFile;
  end;
end;



function TScriptRunner.FindBuildFile(SearchUp: boolean): TPath;
begin
  Result := FindBuildFile(DefaultBuildFileName, SearchUp);
  if not PathIsFile(Result) then
     Result := FindBuildFile(AntBuildFileName, SearchUp);
  if not PathIsFile(Result) then
     Result := DefaultBuildFileName;
end;




end.


