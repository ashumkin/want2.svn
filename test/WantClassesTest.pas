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
unit DanteClassesTest;

interface

uses
  WildPaths,
  DanteClasses,
  ExecTasks,
  DelphiTasks,

  TestFramework,

  JclFileUtils,
  JclShell,

  SysUtils,
  Classes;


type
  TProjectBaseCase = class(TTestCase)
  protected
    FProject: TProject;

    procedure SetUp;    override;
    procedure TearDown; override;
  published
  end;

  TTestDirCase = class(TProjectBaseCase)
  protected
    FTestDir: string;

    function MakeSampleTextFile: string;
  public
    procedure Setup; override;
    procedure TearDown; override;
  end;

  TBuildTests = class(TProjectBaseCase)
  protected
    procedure BuildProject;
    procedure SetUp; override;
  published
    procedure TestSchedule;
    procedure TestBuild;
  end;

  TDummyTask1 = class(TTask)
  public
    class function TagName: String; override;
    procedure Execute; override;
  end;

  TDummyTask2 = class(TDummyTask1)
    class function TagName: String; override;
  end;

  TDummyTask3 = class(TDummyTask1)
  protected
    FAProp: string;
  public
    class function TagName: String; override;
    function SetAttribute(const aName: String; const aValue: String): Boolean; override;
  end;

  TTestExecTask = class(TTestDirCase)
  private
    FExecTask: TExecTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestExecTask;
  end;

implementation


{ TProjectBaseCase }

procedure TProjectBaseCase.SetUp;
begin
  FProject := TProject.Create(nil);
end;

procedure TProjectBaseCase.TearDown;
begin
  FreeAndNil(FProject);
end;

{ TTestDirCase }

function TTestDirCase.MakeSampleTextFile: string;
var
  F: TextFile;
begin
  Result := FTestDir + '\sample.txt';
  AssignFile(F, Result);
  Rewrite(F);
  WriteLn(F, 'this is a sample file');
  CloseFile(F);
end;

procedure TTestDirCase.Setup;
begin
  inherited;
  FTestDir := ExtractFilePath(ParamStr(0)) + 'test';
  JclFileUtils.ForceDirectories(FTestDir);
end;

procedure TTestDirCase.TearDown;
begin
  JclShell.SHDeleteFolder(0, FTestDir, [doSilent]);
  inherited;
end;

{ TSaveProjectTests }

const
  CR = #13#10;

  ExpectedXML =
    CR+
    '<project basedir="." default="compile" description="a test project" name="test">' + CR +
    '  <target name="prepare">'                                            + CR +
    '    <dummy1 />'                                                       + CR +
    '  </target>'                                                          + CR +
    '  <target depends="prepare" name="compile">'                          + CR +
    '    <dummy2 />'                                                       + CR +
    '    <dummy3 aprop="25" />'                                            + CR +
    '  </target>'                                                          + CR +
    '</project>'                                                           + CR;


{ TTestExecTask }

procedure TTestExecTask.Setup;
begin
  inherited;
  FExecTask := TShellTask.Create(FProject.AddTarget('test_exec_task'));
end;

procedure TTestExecTask.TearDown;
begin
  FExecTask.Free;
  inherited;
end;

procedure TTestExecTask.TestExecTask;
var
  CurrentFileName: string;
  NewFileName: string;
begin
  CurrentFileName := MakeSampleTextFile;
  NewFileName := ExtractFilePath(CurrentFileName) + 'new.txt';
  FExecTask.Executable := 'copy';
  FExecTask.ArgumentList.Add(CurrentFileName);
  FExecTask.ArgumentList.Add(NewFileName);
  FExecTask.Execute;
  Check(FileExists(NewFileName), 'TExecTask copy file failed');
end;


{ TBuildTests }

procedure TBuildTests.BuildProject;
var
  Target: TTarget;
  Task:   TExecTask;
  fname:  String;
begin
//  FProject.ParseXMLText(ExpectedXML);

  FProject.SetAttribute('name', 'my_project');

  Target := FProject.AddTarget('prepare');

  Target.Add(TDummyTask1.Create(Target));

  Target := FProject.AddTarget('compile');

  Target.ParseDepends('prepare');

  Target.Add(TDummyTask2.Create(Target));

  Target.Add(TDummyTask3.Create(Target));

  Target := FProject.AddTarget('copy');

  Target.ParseDepends('compile');

  Task := TExecTask.Create(Target);

  Task.Executable := 'copy';

  fname := ExtractFilePath(ParamStr(0)) + 'test\sample.txt';

  Task.ArgumentList.Add(fname);

  fname := ExtractFilePath(fname) + 'new.txt';

  Task.ArgumentList.Add(fname);

  Target.Add(Task);
end;


procedure TBuildTests.SetUp;
begin
  inherited SetUp;
  BuildProject;
end;

procedure TBuildTests.TestSchedule;
var
  s: TTargetList;
begin
  s := TTargetList.Create(nil);

  try
    FProject.Schedule(FProject.Names['copy'], s);

    CheckEquals(3, s.Count);
    CheckEquals('prepare', s.Target[0].Name);
    CheckEquals('compile', s.Target[1].Name);
    CheckEquals('copy',    s.Target[2].Name);

  finally
    FreeAndNil(s);
  end;
end;

procedure TBuildTests.TestBuild;
var
  OldFileName: String;
  NewFileName: String;
  Target:      TTarget;
  ExecTask:    TExecTask;
  F          :Text;
begin
  Target := FProject.Names['copy'];

  ExecTask := TExecTask(Target.Task[0]);

  OldFileName := ExecTask.ArgumentList[0];
  NewFileName := ExecTask.ArgumentList[1];

  try
    CreateDir(ExtractFileDir(OldFileName));
    Assign(F, OldFileName);
    Rewrite(F);
    Writeln(F, 'A test');
    Close(F);

    Check(not FileExists(NewFileName), 'file not copied');

    FProject.Build(['copy']);

    Check(FileExists(NewFileName), 'file not copied');
  finally
    DeleteFile(OldFileName);
    DeleteFile(NewFileName);
    RemoveDir(ExtractFileDir(OldFileName));
  end;
end;

{ TDummyTask1 }

procedure TDummyTask1.Execute;
begin

end;

class function TDummyTask1.TagName: String;
begin
  Result := 'dummy1';
end;

{ TDummyTask2 }

class function TDummyTask2.TagName: String;
begin
  Result := 'dummy2';
end;

{ TDummyTask3 }

class function TDummyTask3.TagName: String;
begin
  Result := 'dummy3';
end;

function TDummyTask3.SetAttribute(const aName, aValue: String): Boolean;
begin
  Result := True;

  if CompareText(aName, 'aprop') = 0 then
    FAProp := aValue
  else
    Result := inherited SetAttribute(aName, aValue);
end;



initialization
  RegisterTasks([TDummyTask1, TDummyTask2, TDummyTask3]);

  RegisterTests('Unit Tests', [
             TTestExecTask.Suite,
             TBuildTests.Suite
           ]);
end.

