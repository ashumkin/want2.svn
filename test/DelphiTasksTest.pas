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
  WildPaths,
  DanteClasses,
  DelphiTasks,
  TestFramework,
  DanteClassesTest;

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
    BaseDir := PathConcat(SuperPath(ToPath(ParamStr(0))), '..');
    Name := 'delphi_compile';

    T := AddTarget('compile');
    FDelphiTask := TDelphiCompileTask.Create(T);
    with FDelphiTask do
    begin
      basedir := 'src';
      source  := 'dante.dpr';
      exeoutput := '/bin/test';
      dcuoutput := '/tmp';
      build   := true;
      quiet   := true;
      uselibrarypath := false;

      AddUnitPath('lib');
      AddUnitPath('tasks');
      AddUnitPath('elements');

      AddUnitPath('../lib/jcl');
      AddUnitPath('../lib/xml');
      AddUnitPath('../lib/paszlib');
      AddUnitPath('../lib/paszlib/minizip');
      AddUnitPath('../lib/perlre');
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
  exe := PathConcat(FDelphiTask.exeoutput, 'dante.exe');
  if PathIsFile(exe) then
    DeleteFile(exe);
  FProject.Build('compile');
  Check(PathIsFile(exe), 'dante exe not found');
end;

{ TTestIncVerRcTask }

initialization
  RegisterTests('Delphi Tasks', [TDelphiCompileTests.Suite]);
end.
