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
--------------------------------------------------------------------------------
(based on BSD Open Source License)
}
unit DanteClassesTest;

interface
uses
  Projects,
  SysUtils,
  Classes,
  TestFramework;

type
  TProjectBaseCase = class(TTestCase)
  protected
    FProject: TProject;

    procedure SetUp;    override;
    procedure TearDown; override;
  published
  end;

  TSaveProjectTests = class(TProjectBaseCase)
    procedure BuildTestProject;
  published
    procedure testInMemoryConstruction;
    procedure testParse;
    procedure testSaveLoad;
  end;

  TDummyTask1 = class(TTask);
  TDummyTask2 = class(TTask);
  TDummyTask3 = class(TTask)
  protected
    FAnInt :Integer;
  published
    property AnInt :Integer
    read  FAnInt
    write FAnInt;
  end;

implementation

{ TProjectBaseCase }

procedure TProjectBaseCase.SetUp;
begin
  FProject := TProject.Create(nil);
end;

procedure TProjectBaseCase.TearDown;
begin
  FProject.Free;
end;

{ TSaveProjectTests }

const
  expected =
    'object my_project: TProject' + #$D#$A +
    '  object prepare: TTarget' + #$D#$A +
    '    object TDummyTask1_0: TDummyTask1' + #$D#$A +
    '    end' + #$D#$A +
    '  end' + #$D#$A +
    '  object compile: TTarget' + #$D#$A +
    '    object TDummyTask2_0: TDummyTask2' + #$D#$A +
    '    end' + #$D#$A +
    '    object TDummyTask3_0: TDummyTask3' + #$D#$A +
    '      AnInt = 0' + #$D#$A +
    '    end' + #$D#$A +
    '  end' + #$D#$A +
    'end' + #$D#$A;


procedure TSaveProjectTests.BuildTestProject;
var
  T :TTarget;
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

procedure TSaveProjectTests.testInMemoryConstruction;
begin
  BuildTestProject;
  CheckEquals(expected, FProject.AsString);
  CheckEquals(2, FProject.TargetCount);
  CheckEquals(1, FProject[0].TaskCount);
  CheckEquals(2, FProject[1].TaskCount);
  CheckEquals('TDummyTask3', FProject[1][1].ClassName);
end;

procedure TSaveProjectTests.testParse;
begin
  FProject.Parse(expected);
  CheckEquals(expected, FProject.AsString);
end;

procedure TSaveProjectTests.testSaveLoad;
var
  P :TProject;
begin
  BuildTestProject;
  FProject.Save('build1.dfm');
  P := TProject.Create(nil);
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

initialization
  RegisterClasses([TDummyTask1, TDummyTask2, TDummyTask3]);
  RegisterTest('', TSaveProjectTests);
end.

