{ $Id$ }
{
--------------------------------------------------------------------------
Copyright (c) 2001, Chris Morris
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The names Chris Morris, Dante and the names of contributors to this software
may not be used to endorse or promote products derived from this software
without specific prior written permission.

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

Contributor(s): Juancarlo Añez
--------------------------------------------------------------------------------
(based on BSD Open Source License)
}
unit DanteClassesTest;

interface

uses
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

  TSaveProjectTests = class(TProjectBaseCase)
    procedure BuildTestProject;
  published
    procedure TestInMemoryConstruction;
    procedure TestParse;
    procedure TestParseXML;
    procedure TestSaveLoad;
  end;

  TBuildTests = class(TProjectBaseCase)
  protected
    procedure BuildProject;
    procedure SetUp; override;
  published
    procedure TestSchedule;
    procedure TestBuild;
  end;

  TDelphiCompileTests = class(TProjectBaseCase)
  protected
    procedure BuildProject;
    procedure SetUp; override;
  published
    procedure TestCompile;
  end;

  TDummyTask1 = class(TTask)
  public
    class function Tag :string; override;
    procedure Execute; override;
  end;

  TDummyTask2 = class(TDummyTask1)
    class function Tag :string; override;
  end;

  TDummyTask3 = class(TDummyTask1)
  protected
    FAnInt: Integer;
  published
    class function Tag :string; override;
    property AnInt: Integer read FAnInt write FAnInt;
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
  FProject := TProject.Create;
end;

procedure TProjectBaseCase.TearDown;
begin
  FProject.Free;
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

  Expected =
    'object my_project: TProject' + CR +
    '  object prepare: TTarget' + CR +
    '    object TDummyTask1_0: TDummyTask1' + CR +
    '    end' + CR +
    '  end' + CR +
    '  object compile: TTarget' + CR +
    '    object TDummyTask2_0: TDummyTask2' + CR +
    '    end' + CR +
    '    object TDummyTask3_0: TDummyTask3' + CR +
    '      AnInt = 0' + CR +
    '    end' + CR +
    '  end' + CR +
    'end' + CR;


  ExpectedXML =
    '<project description="a test project"'               + CR +
    '         default="compile" >'                        + CR +
    '  <target name="prepare" >'                          + CR +
    '    <dummy1 />' + CR +
    '  </target>'                                         + CR +
    '  <target name="compile" depends="prepare">'         + CR +
    '    <dummy2 />'                                      + CR +
    '    <dummy3 anint="25" />'                           + CR +
    '  </target>'                                         + CR +
    '</project>'                                          + CR;


procedure TSaveProjectTests.BuildTestProject;
var
  T: TTarget;
begin
  with FProject do
  begin
    Name := 'my_project';
    T := AddTarget('prepare');
    TDummyTask1.Create(T);

    T := AddTarget('compile');
    TDummyTask2.Create(T);
    TDummyTask3.Create(T);
  end;
end;

procedure TSaveProjectTests.TestInMemoryConstruction;
begin
  BuildTestProject;
  CheckEquals(Expected, FProject.AsString);
  CheckEquals(2, FProject.TargetCount);
  CheckEquals(1, FProject[0].TaskCount);
  CheckEquals(2, FProject[1].TaskCount);
  CheckEquals('TDummyTask3', FProject[1][1].ClassName);
end;

procedure TSaveProjectTests.TestParse;
begin
  FProject.Parse(Expected);
  CheckEquals(Expected, FProject.AsString);
end;

procedure TSaveProjectTests.TestParseXML;
begin
  FProject.ParseXMLText(ExpectedXML);
end;

procedure TSaveProjectTests.TestSaveLoad;
var
  P: TProject;
begin
  BuildTestProject;
  FProject.Save('build1.dfm');
  P := TProject.Create;
  try
    P.Load('build1.dfm');
    CheckEquals(FProject.AsString, P.AsString);
    CheckEquals(2, P.TargetCount);
    CheckEquals(1, P[0].TaskCount);
    CheckEquals(2, P[1].TaskCount);
    CheckEquals('TDummyTask3', P[1][1].ClassName);
  finally
    P.Free;
  end;
end;

{ TTestExecTask }

procedure TTestExecTask.Setup;
begin
  inherited;
  FExecTask := TShellExecTask.Create(FProject.AddTarget('test_exec_task'));
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
  FExecTask.Arguments.Add('copy');
  FExecTask.Arguments.Add(CurrentFileName);
  FExecTask.Arguments.Add(NewFileName);
  FExecTask.Execute;
  Check(FileExists(NewFileName), 'TExecTask copy file failed');
end;


{ TBuildTests }

procedure TBuildTests.BuildProject;
var
  T: TTarget;
  fname: string;
begin
  with FProject do
  begin
    Name := 'my_project';
    T := AddTarget('prepare');
    TDummyTask1.Create(T);

    T := AddTarget('compile');
    T.Depends := 'prepare';
    TDummyTask2.Create(T);
    TDummyTask3.Create(T);

    T := AddTarget('copy');
    T.Depends := 'compile';
    with TShellExecTask.Create(T) do
    begin
      Executable := 'copy';
      fname := ExtractFilePath(ParamStr(0)) + 'test\sample.txt';
      Arguments.Add(fname);
      fname := ExtractFilePath(fname) + 'new.txt';
      Arguments.Add(fname);
    end;
  end;
end;


procedure TBuildTests.SetUp;
begin
  inherited SetUp;
  BuildProject;
end;

procedure TBuildTests.TestSchedule;
var
  S: TTargetArray;
begin
  S := FProject.Schedule('copy');
  CheckEquals(3, Length(S));
  CheckEquals('prepare', S[0].Name);
  CheckEquals('compile', S[1].Name);
  CheckEquals('copy',    S[2].Name);
end;

procedure TBuildTests.TestBuild;
var
  OldFileName,
  NewFileName: string;
begin
 with FProject.Names['copy'].Tasks[0] as TExecTask do
 begin
   OldFileName := Arguments[0];
   NewFileName := Arguments[1];
 end;

 try
   CreateDir(ExtractFileDir(OldFileName));
   FileClose(FileCreate(OldFileName));

   Check(not FileExists(NewFileName), 'file not copied');

   FProject.Build('copy');

   Check(FileExists(NewFileName), 'file not copied');
 finally
   DeleteFile(OldFileName);
   DeleteFile(NewFileName);
   RemoveDir(ExtractFileDir(OldFileName));
 end;
end;

{ TDelphiCompileTests }

procedure TDelphiCompileTests.BuildProject;
var
  T :TTarget;
  RootDir :string;
begin
  with FProject do
  begin
    Name := 'delphi_compile';
    T := AddTarget('compile');
    with TDelphiCompileTask.Create(T) do
    begin
      RootDir := ExtractFilePath(ParamStr(0));
      Arguments.Add(RootDir + '..\src\dante.dpr');
      Arguments.Add('/B');
      Arguments.Add('/Q');
      Arguments.Add('/E..\bin');
      Arguments.Add('/N..\dcu');
      Arguments.Add('/U..\src;..\src\tasks;..\src\jcl;..\src\paths;..\src\xml');
    end;
  end;
end;

procedure TDelphiCompileTests.SetUp;
begin
  inherited SetUp;
  BuildProject;
end;

procedure TDelphiCompileTests.TestCompile;
const
  exe = 'dante.exe';
begin
  if FileExists(exe) then
    DeleteFile(exe);
  FProject.Build('compile');
  Check(FileExists(exe), 'dante exe not found');
end;

{ TDummyTask1 }

procedure TDummyTask1.Execute;
begin
end;

class function TDummyTask1.Tag: string;
begin
  Result := 'dummy1';
end;
{ TDummyTask2 }

class function TDummyTask2.Tag: string;
begin
  Result := 'dummy2';
end;

{ TDummyTask3 }

class function TDummyTask3.Tag: string;
begin
  Result := 'dummy3';
end;

initialization
  RegisterTasks([TDummyTask1, TDummyTask2, TDummyTask3]);

  RegisterTests('Unit Tests', [
             TSaveProjectTests.Suite,
             TTestExecTask.Suite,
             TBuildTests.Suite,
             TDelphiCompileTests.Suite
           ]);
end.

