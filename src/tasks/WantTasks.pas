(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit WantTasks;

interface
uses
  SysUtils,
  Classes,

  JclSysUtils,

  WantClasses,
  ScriptRunner,
  WildPaths,
  PatternSets;


type
  TSubProjectPropertyElement = class(TScriptElement)
  public
    class function TagName :string;              override;
    procedure SetProperty(Name, Value :string); override;
  end;

  TCustomWantTask = class(TTask)
  protected
    FTarget     :string;
  public
    property _target   :string read FTarget    write FTarget;
  end;

  TWantTask = class(TCustomWantTask)
  protected
    FBuildFile  :TPath;
    FSubProject :TProject;
  public
    constructor Create(Owner: TScriptElement = nil); override;
    destructor  Destroy; override;

    procedure Init; override;
    procedure Execute;  override;
  published
    property _target;
    property buildfile :TPath read FBuildFile write FBuildFile;
    property dir       :TPath read GetBaseDir write SetBaseDir;
  end;


implementation

{ TWantTask }

constructor TWantTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FSubProject := TProject.Create(Self);
  FSubProject.Listener := Self.Project.Listener;
  FSubProject.RootPath := ToAbsolutePath(Self.Project.RootPath);
end;

destructor TWantTask.Destroy;
begin
  FSubProject.Free;
  inherited Destroy;
end;


procedure TWantTask.Init;
begin
  inherited Init;
  if dir <> '' then
    FSubProject.SetInitialBaseDir(dir);
end;

procedure TWantTask.Execute;
var
  FRunner :TScriptRunner;
begin
  try
    FRunner := TScriptRunner.Create;
    try
      FRunner.Listener  := Self.Project.Listener;
      ChangeDir(BasePath);
      FRunner.LoadProject(FSubProject, buildfile, false);
      FRunner.BuildProject(FSubProject, _target);
    finally
      FRunner.Free;
    end;
  except
    on e :Exception do
      TaskError(e.Message, ExceptAddr);
  end;
end;

{ TSubProjectPropertyElement }

class function TSubProjectPropertyElement.TagName: string;
begin
  Result := 'property';
end;

procedure TSubProjectPropertyElement.SetProperty(Name, Value: string);
begin
  (Owner as TWantTask).FSubProject.SetProperty(Name, Value);
end;

initialization
 RegisterTasks([TWantTask]);
end.
