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
unit DelphiTasksTest;

interface

uses
  WildPaths, FileOps, DanteClasses, DelphiTasks, TestFramework,
  DanteClassesTest, clVersionRcUnitTest;

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

  TTestIncVerRcTask = class(TTestRcUnit)
  private
    FIncVerRcTask: TIncVerRcTask;
    FProject: TProject;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestIncVerRcTask;
  end;

implementation

{ TDelphiCompileTests }

procedure TDelphiCompileTests.BuildProject;
var
  T: TTarget;
begin
  with FProject do
  begin
    BaseDir := PathConcat(SuperPath(ToPath(ParamStr(0))), '..');
    Name := 'delphi_compile';
    Verbosity := vlDebug;

    T := AddTarget('compile');
    FDelphiTask := TDelphiCompileTask.Create(T);
    with FDelphiTask do
    begin
      basedir := PathConcat(FProject.BasePath, 'src');
      writeln(ToSystemPath(basedir));
      writeln(CurrentDir);
      source  := 'dante.dpr';
      exes    := '/tmp';
      dcus    := '/tmp';
      build   := true;
      quiet   := true;

      AddUnitPath('jcl');
      AddUnitPath('paths');
      AddUnitPath('xml');
      AddUnitPath('tasks');
      AddUnitPath('zip');
      AddUnitPath('../lib/paszlib');
      AddUnitPath('../lib/paszlib/minizip');
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
  exe := PathConcat(FDelphiTask.exes, 'dante.exe');
  if IsFile(exe) then
    DeleteFile(exe);
  FProject.Build('compile');
  Check(IsFile(exe), 'dante exe not found');
end;

{ TTestIncVerRcTask }

procedure TTestIncVerRcTask.Setup;
var
  T: TTarget;
begin
  inherited;
  FProject := TProject.Create;
  T := FProject.AddTarget('update_rc_file');
  FIncVerRcTask := TIncVerRcTask.Create(T);
end;

procedure TTestIncVerRcTask.TearDown;
begin
  FProject.Free;
  inherited;
end;

procedure TTestIncVerRcTask.TestIncVerRcTask;
begin
  FIncVerRcTask.RcFileName := FTestRcName;
  FIncVerRcTask.Increment  := True;
  FIncVerRcTask.Init;
  FIncVerRcTask.Execute;
  CheckEquals('53', FProject.PropertyValue('build'), 'build property');
end;

initialization
  RegisterTests('Delphi Tasks',
    [TDelphiCompileTests.Suite, TTestIncVerRcTask.Suite]);
end.
