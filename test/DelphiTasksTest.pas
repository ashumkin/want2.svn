(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit DelphiTasksTest;

interface

uses
  JclFileUtils,
  WildPaths,
  WantClasses,
  DelphiTasks,
  TestFramework,
  WantClassesTest;

type
  TDelphiCompileTests = class(TProjectBaseCase)
    FDelphiTask: TDelphiCompileTask;
  protected
    procedure BuildProject;
    procedure SetUp;    override;
    procedure TearDown; override;
  published
    procedure TestCompile;
  end;

implementation

{ TDelphiCompileTests }

procedure TDelphiCompileTests.BuildProject;
var
  T: TTarget;
begin
  with FProject do
  begin
    BaseDir := PathConcat(SuperPath(ToPath(GetModulePath(hInstance))), '..');
    Name := 'delphi_compile';

    T := AddTarget('compile');
    FDelphiTask := TDelphiCompileTask.Create(T);
    with FDelphiTask do
    begin
      basedir := 'src';
      source  := 'Want.dpr';
      exeoutput := '/bin/test';
      dcuoutput := '/tmp';
      build   := true;
      quiet   := true;
      uselibrarypath := false;

      AddUnitPath('../lib/**');
      AddUnitPath('../src/**');
    end;
  end;
end;

procedure TDelphiCompileTests.SetUp;
begin
  inherited SetUp;
  BuildProject;
end;

procedure TDelphiCompileTests.TearDown;
begin
  FDelphiTask := nil;
end;

procedure TDelphiCompileTests.TestCompile;
var
  exe:  string;
begin
  MakeDir(FDelphiTask.exeoutput);
  exe := PathConcat(FDelphiTask.exeoutput, 'Want.exe');
  if PathIsFile(exe) then
    DeleteFile(exe);
  RunProject('compile');
  Check(PathIsFile(exe), 'Want exe not found');
end;

{ TTestIncVerRcTask }

initialization
  RegisterTests('Delphi Tasks', [TDelphiCompileTests.Suite]);
end.
