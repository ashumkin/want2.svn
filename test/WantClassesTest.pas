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

  TSaveProjectTests = class(TProjectBaseCase)
    procedure BuildTestProject;
    procedure __TestParse;
  published
    procedure TestInMemoryConstruction;
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

  TPropertyTests = class(TProjectBaseCase)
  published
    procedure TestLocalProperties;
  end;

  TDummyTask1 = class(TTask)
  public
    class function XMLTag :string; override;
    procedure Execute; override;
  end;

  TDummyTask2 = class(TDummyTask1)
    class function XMLTag :string; override;
  end;

  TDummyTask3 = class(TDummyTask1)
  protected
    FAProp: string;
  published
    class function XMLTag :string; override;
    property AProp: string read FAProp write FAProp;
  end;

  TCompareValuesTask = class(TTask)
  protected
    FExpected :string;
    FActual   :string;
  public
    class function XMLTag :string; override;
    procedure Init;    override;
    procedure Execute; override;
  published
    property expected :string read FExpected write FExpected;
    property actual   :string read FActual   write FActual;
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
    'object my_project: TProject'                                          + CR +
    '  BaseDir = ''..'''                                                   + CR +
    '  object prepare: TTarget'                                            + CR +
    '    object TDummyTask1'                                               + CR +
    '    end'                                                              + CR +
    '  end'                                                                + CR +
    '  object compile: TTarget'                                            + CR +
    '    object TDummyTask2'                                               + CR +
    '    end'                                                              + CR +
    '    object TDummyTask3'                                               + CR +
    '      AProp = ''aValue'''                                             + CR +
    '    end'                                                              + CR +
    '  end'                                                                + CR +
    'end'                                                                  + CR;


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


procedure TSaveProjectTests.BuildTestProject;
var
  T: TTarget;
begin
  with FProject do
  begin
    BaseDir := '..';
    Name := 'my_project';
    T := AddTarget('prepare');
    TDummyTask1.Create(T);

    T := AddTarget('compile');
    TDummyTask2.Create(T);
    TDummyTask3.Create(T).AProp := 'aValue';
  end;
end;

procedure TSaveProjectTests.TestInMemoryConstruction;
begin
  BuildTestProject;
  CheckEquals(2, FProject.TargetCount);
  CheckEquals(1, FProject[0].TaskCount);
  CheckEquals(2, FProject[1].TaskCount);
  CheckEquals('TDummyTask3', FProject[1][1].ClassName);
end;

procedure TSaveProjectTests.__TestParse;
begin
  FProject.Parse(Expected);
  CheckEquals(Expected, FProject.AsString);
end;

procedure TSaveProjectTests.TestParseXML;
begin
  FProject.ParseXMLText(ExpectedXML);
  CheckEquals(ExpectedXML, CR+FProject.AsXML);
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
    with TShellTask.Create(T) do
    begin
      Executable := 'copy';
      fname := ExtractFilePath(ParamStr(0)) + 'test\sample.txt';
      ArgumentList.Add(fname);
      fname := ExtractFilePath(fname) + 'new.txt';
      ArgumentList.Add(fname);
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
  F          :Text;
begin
 with FProject.TargetNames['copy'].Tasks[0] as TExecTask do
 begin
   OldFileName := ArgumentList[0];
   NewFileName := ArgumentList[1];
 end;

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

class function TDummyTask1.XMLTag: string;
begin
  Result := 'dummy1';
end;
{ TDummyTask2 }

class function TDummyTask2.XMLTag: string;
begin
  Result := 'dummy2';
end;

{ TDummyTask3 }

class function TDummyTask3.XMLTag: string;
begin
  Result := 'dummy3';
end;

{ TCompareValuesTask }

class function TCompareValuesTask.XMLTag: string;
begin
  Result := 'check';
end;

procedure TCompareValuesTask.Init;
begin
  inherited Init;
  RequireAttribute('expected');
  RequireAttribute('actual');
end;

procedure TCompareValuesTask.Execute;
begin
  if expected <> actual then
    raise ETestFailure.Create(Format('Expected <%s> but was <%s>.', [expected, actual]) );
end;

{ TPropertyTests }

procedure TPropertyTests.TestLocalProperties;
const
  build_xml = ''
  +#10'<project name="test" default="dotest" >'
  +#10'  <property name="global" value="0" />'
  +#10'  <target name="target1">'
  +#10'    <property name="local" value="1" />'
  +#10'    <check expected="1" actual="${local}" />'
  +#10'    <check expected="0" actual="${global}" />'
  +#10'  </target>'
  +#10'  <target name="target2">'
  +#10'    <property name="local" value="2" />'
  +#10'    <check expected="2" actual="${local}" />'
  +#10'    <check expected="0" actual="${global}" />'
  +#10'  </target>'
  +#10'  <target name="dotest" depends="target1,target2" />'
  +#10'</project>'
  +'';
begin
  FProject.ParseXMLText(build_xml);
  FProject.Build;
end;

initialization
  RegisterTasks([TDummyTask1, TDummyTask2, TDummyTask3, TCompareValuesTask]);

  RegisterTests('Dante Classes', [
             TSaveProjectTests.Suite,
             TBuildTests.Suite,
             TPropertyTests.Suite
           ]);
end.

