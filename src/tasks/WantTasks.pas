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
  ScriptParser,
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
    FBuildFile  :string;
    FDir        :string;

    FSubProject :TProject;

  public
    constructor Create(Owner: TScriptElement = nil); override;
    destructor  Destroy; override;

    procedure Init; override;
    procedure Execute;  override;
  published
    property _target;
    property buildfile :string read FBuildFile write FBuildFile;
    property dir       :string read FDir       write FDir;
  end;

  TWantCallTask = class(TCustomWantTask)
  public
    procedure Init; override;
    procedure Execute;  override;
  published
    property _target;
  end;


implementation

{ TWantTask }

constructor TWantTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FSubProject := TProject.Create(Self);
  FSubProject.RootPath := ToAbsolutePath(Project.RootPath);
  FSubProject.Listener := Project.Listener;
end;

destructor TWantTask.Destroy;
begin
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
  bfile :string;
begin
  bfile := TScriptParser.Parse(FSubProject, buildfile);
  Log('building "%s"', [ ToRelativePath(bfile) ]);
  FSubProject.Build(_target);
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

{ TWantCallTask }

procedure TWantCallTask.Init;
begin
  RequireAttribute('target');
end;

procedure TWantCallTask.Execute;
begin
  Project.Build(_target);
end;

initialization
 RegisterTasks([TWantTask,TWantCallTask]);
end.
