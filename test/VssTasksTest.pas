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
unit VssTasksTest;

interface

uses
  VssTasks, TestFramework, DanteClassesTest, JclMiscel, SysUtils, JclShell,
  DanteClasses;

type
  TTestVssTasksBase = class(TTestDirCase)
  protected
    FRevOneContents: string;
    FRevTwoContents: string;
    FSampleFileName: string;
    FSamplePathFileName: string;
    FUserPwd: string;
    FVssPath: string;

    function CompareSampleContents(Contents: string): boolean;
    procedure MakeSample(Contents: string);
    procedure MakeSampleRevOne;
    procedure MakeSampleRevTwo;
    function ReadSampleContents: string;
  public
    procedure Setup; override;
    procedure TearDown; override;
  end;

  TTestVssGetTask = class(TTestVssTasksBase)
  private
    FVssGetTask: TVssGetTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestVssGetTask;
    procedure TestVssGetTaskLabel;
  end;

implementation

{ TTestVssTasksBase }

function TTestVssTasksBase.CompareSampleContents(
  Contents: string): boolean;
begin
  Result := (Contents = ReadSampleContents);
end;

procedure TTestVssTasksBase.MakeSample(Contents: string);
var
  F: TextFile;
begin
  AssignFile(F, FSamplePathFileName);
  Rewrite(F);
  WriteLn(F, Contents);
  CloseFile(F);
end;

procedure TTestVssTasksBase.MakeSampleRevOne;
begin
  FRevOneContents := 'rev one contents';
  MakeSample(FRevOneContents);
end;

procedure TTestVssTasksBase.MakeSampleRevTwo;
begin
  FRevTwoContents := 'rev Two contents';
  MakeSample(FRevTwoContents);
end;

function TTestVssTasksBase.ReadSampleContents: string;
var
  F: TextFile;
  ReadContents: string;
  ReadLine: string;
begin
  AssignFile(F, FSamplePathFileName);
  Reset(F);
  ReadContents := '';
  while not Eof(F) do
  begin
    ReadLn(F, ReadLine);
    ReadContents := ReadContents + ReadLine;
  end;
  Result := ReadContents;
  CloseFile(F);
end;

procedure TTestVssTasksBase.Setup;
begin
  inherited;
  {$IFNDEF RUN_VSS_TESTS}
  Fail('By default, Visual SourceSafe tests will not execute because they ' +
    'currently require an actual VSS installation in place. If you''d like ' +
    'to execute these tests, look at VssTasksTest.pas');
  {$ELSE}
    { to run these tests, you must change the following settings for your
      specific installation. Make sure the VssPath is set to an empty project }
    FVssPath := '$/Front Office Development/DanteVssTaskTest';
    FUserPwd := 'chrism,saltnpepper';

    FSamplePathFileName := MakeSampleTextFile;
    FSampleFileName := ExtractFileName(FSamplePathFileName);
    WinExec32AndWait('ss CP "' + FVssPath + '" -I- -Y' + FUserPwd, 0);
    WinExec32AndWait('ss Add "' + FSamplePathFileName + '" -I- -Y' + FUserPwd, 0);
    if FSamplePathFileName <> '' then
      JclShell.SHDeleteFiles(0, FSamplePathFileName, [doSilent, doAllowUndo]);
  {$ENDIF}
end;

procedure TTestVssTasksBase.TearDown;
begin
  {$IFDEF RUN_VSS_TESTS}
    { if you want to purge these test files, you'll have to do that manually.
      Some of the tests run cleaner if the test file was purged, because
      repeated deletes don't do anything. However, when I wrote this I did
      not have access to a test SourceSafe database that could survive an
      accidental purging while writing this fixture code. -- Chrismo }
    WinExec32AndWait('ss CP "' + FVssPath + '" -I- -Y' + FUserPwd, 0);
    if FSampleFileName <> '' then
      WinExec32AndWait('ss Delete "' + FVssPath + '/' + FSampleFileName +
        '" -I- -Y' + FUserPwd, 0);
  {$ENDIF}
  inherited;
end;

{ TTestVssGetTask }

procedure TTestVssGetTask.Setup;
begin
  inherited;
  FProject := TProject.Create;
  FVssGetTask := TVssGetTask.Create(FProject.AddTarget('test'));
end;

procedure TTestVssGetTask.TearDown;
begin
  FVssGetTask.Free;
  inherited;
end;

procedure TTestVssGetTask.TestVssGetTask;
begin
  try
    FVssGetTask.Validate;
    Fail('validation should have failed');
  except
    on EDanteError do { nada }
  end;

  FVssGetTask.VssPath := FVssPath;
  FVssGetTask.Validate;

  FVssGetTask.Login := FUserPwd;
  FVssGetTask.LocalPath := FTestDir;
  FVssGetTask.Execute;
  Check(FileExists(FSamplePathFileName), 'file not retrieved');
end;

procedure TTestVssGetTask.TestVssGetTaskLabel;
var
  LabelName: string;
begin
  { because the TearDown doesn't purge the test file (for reasons stated there),
    and after the 1st time its run can't delete the file without purging the
    previous one, the test below won't actually be operating against a clean
    file. The label command, run subsequent times, will fail because of the
    previous labelling in the test file's SS history. However, this test still
    does its job properly. If you want a clean, clean test, do a purge of the
    test file first. }
  ChDir(FTestDir);
  WinExec32AndWait('ss Checkout ' + FSampleFileName + ' -GL"' + FTestDir + '"' +
    ' -Y' + FUserPwd, 0);
  MakeSampleRevOne;
  WinExec32AndWait('ss Checkin ' + FSampleFileName + ' -CComment -K ' +
    ' -Y' + FUserPwd, 0);
  LabelName := 'RevOne';
  WinExec32AndWait('ss Label "' + FVssPath + '" -CComment -L' + LabelName +
    ' -I- -Y' + FUserPwd, 0);
  MakeSampleRevTwo;
  WinExec32AndWait('ss Checkin ' + FSampleFileName + ' -CComment ' +
    ' -Y' + FUserPwd, 0);

  FVssGetTask.VssPath := FVssPath;
  FVssGetTask.Login := FUserPwd;
  FVssGetTask.LocalPath := FTestDir;
  FVssGetTask._Label := LabelName;
  FVssGetTask.Execute;
  CheckEquals(FRevOneContents, ReadSampleContents, 'wrong version retrieved');
end;

initialization
  RegisterTest('Acceptance Suite', TTestVssGetTask);

end.
