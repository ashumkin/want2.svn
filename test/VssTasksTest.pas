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
  DanteClasses, JclSysInfo, Classes;

type
  TTestVssTasksBase = class(TTestDirCase)
  private
    function GetTestFileName(Index: Integer): string;
  protected
    FRevOneContents: string;
    FRevTwoContents: string;
    //FSampleFileName: string;
    //FSamplePathFileName: string;
    FTestPathFileNames: TStringList;
    FUserPwd: string;
    FVssPath: string;

    procedure AddTestFile;
    function CompareSampleContents(Contents: string): boolean;
    procedure DeleteTestFiles;
    procedure Exec(Cmd: string);
    procedure MakeSample(Contents: string);
    procedure MakeSampleRevOne;
    procedure MakeSampleRevTwo;
    function ReadSampleContents: string;
  public
    procedure Setup; override;
    procedure TearDown; override;

    property TestFileNames[Index: Integer]: string read GetTestFileName;
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
    procedure TestVssGetTaskRelPath;
  end;

  TTestVssCheckoutTask = class(TTestVssTasksBase)
  private
    FVssCheckoutTask: TVssCheckoutTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestVssCheckoutTask;
  end;

  TTestVssCheckinTask = class(TTestVssTasksBase)
  private
    FVssCheckinTask: TVssCheckinTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestVssCheckinTask;
  end;

implementation

uses Windows;

{ TTestVssTasksBase }

procedure TTestVssTasksBase.AddTestFile;
var
  PathFileName: string;
begin
  PathFileName := MakeSampleTextFile;
  Exec('ss CP "' + FVssPath + '" -I- -Y' + FUserPwd);
  Exec('ss Add "' + PathFileName + '" -I- -Y' + FUserPwd);
  FTestPathFileNames.Add(PathFileName);
end;

function TTestVssTasksBase.CompareSampleContents(
  Contents: string): boolean;
begin
  Result := (Contents = ReadSampleContents);
end;

procedure TTestVssTasksBase.DeleteTestFiles;
var
  i: Integer;

  procedure DeleteFileInVss(FileName: string);
  begin
    if FileName <> '' then
    begin
      { -I-Y needed in TestVssCheckoutTask to override deleting a checked out
        file }
      Exec('ss Delete "' + FVssPath + '/' + FileName +
        '" -I-Y -Y' + FUserPwd);
      Exec('ss Purge "' + FVssPath + '/' + FileName +
        '" -I- -Y' + FUserPwd);
    end;
  end;

  procedure DeleteFileOnDisk(PathFileName: string);
  begin
    if PathFileName <> '' then
      JclShell.SHDeleteFiles(0, PathFileName, [doSilent, doAllowUndo]);
  end;
begin
  for i := 0 to FTestPathFileNames.Count - 1 do
  begin
    DeleteFileInVss(TestFileNames[i]);
    DeleteFileOnDisk(FTestPathFileNames[i]);
  end;
end;

procedure TTestVssTasksBase.Exec(Cmd: string);
begin
  if WinExec32AndWait(Cmd, 0) <> 0 then
    raise Exception.Create('Internal error: WinExec call failed: ' + Cmd);
end;

function TTestVssTasksBase.GetTestFileName(Index: Integer): string;
begin
  Result := ExtractFileName(FTestPathFileNames[Index]);
end;

procedure TTestVssTasksBase.MakeSample(Contents: string);
var
  F: TextFile;
begin
  AssignFile(F, FTestPathFileNames[0]);
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
  AssignFile(F, FTestPathFileNames[0]);
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
    { TO RUN THESE TESTS: you must change the following setup to work for your
      specific installation. Set the SSDIR environment variable to a
      valid srcsafe.ini. Also, change the FVssPath to a valid path and the
      FUserPwd to a valid user/password combination.

      These tests were built against a VSS version 5 database.

      NOTE: It is HIGHLY recommended you DO NOT run these tests on a production
      Visual SourceSafe database. TearDown will issue a Delete and Purge
      command on the test file. If there's any bug introduced in any of this
      code, you run the risk of purging production data. Please re-read the
      disclaimer contained within the Dante license. }

    { sets SSDIR env variable for this session to a path containing a
      srcsafe.ini file which has the settings indicating which VSS database
      is to be used. This allows you to point to a test VSS db and not
      disturb your normal client settings.

      If you don't set this, ss.exe attempts to find a srcsafe.ini in its
      parent directory. }
    if not JclSysInfo.SetEnvironmentVar('SSDIR', 'D:\Dev\ssafe') then
      raise Exception.Create('Internal error: could not set SSDIR env var. ' +
        '[TTestVssTasksBase.Setup]');

    FVssPath := '$/test';

    { if the test user you use has a password, set it like so:
        FUserPwd := 'dante,password'; }
    FUserPwd := 'dante';

    FTestPathFileNames := TStringList.Create;
    AddTestFile;

    FProject.Level := vlDebug;
  {$ENDIF}
end;

procedure TTestVssTasksBase.TearDown;
begin
  {$IFDEF RUN_VSS_TESTS}
    { NOTE: It is HIGHLY recommended you not run these tests on a production
      Visual SourceSafe database. TearDown will issue a Delete and Purge
      command on the test file. If there's any bug introduced in any of this
      code, you run the risk of purging production data. Please re-read the
      disclaimers contained within the Dante license. }
    Exec('ss CP "' + FVssPath + '" -I- -Y' + FUserPwd);
    DeleteTestFiles;
    FTestPathFileNames.Free;
  {$ENDIF}
  inherited;
end;

{ TTestVssGetTask }

procedure TTestVssGetTask.Setup;
begin
  inherited;
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
    FVssGetTask.Init;
    Fail('validation should have failed');
  except
    on EDanteError do { nada }
  end;

  FVssGetTask.VssPath := FVssPath;
  FVssGetTask.Init;

  FVssGetTask.Login := FUserPwd;
  FVssGetTask.LocalPath := FTestDir;
  FVssGetTask.Execute;
  Check(FileExists(FTestPathFileNames[0]), 'file not retrieved');
end;

procedure TTestVssGetTask.TestVssGetTaskLabel;
var
  LabelName: string;
  CurrDir: string;
begin
  CurrDir := GetCurrentDir;
  ChDir(FTestDir);
  Exec('ss Checkout ' + TestFileNames[0] + ' -GL"' + FTestDir + '"' +
    ' -Y' + FUserPwd);
  MakeSampleRevOne;
  Exec('ss Checkin ' + TestFileNames[0] + ' -CComment -K -Y' + FUserPwd);
  LabelName := 'RevOne';
  Exec('ss Label "' + FVssPath + '" -CComment -L' + LabelName +
    ' -I- -Y' + FUserPwd);

  { if the following Checkin command is sent immediately, sometimes it
    beats the labeling command (don't know why) and gets applied to the
    2nd revision instead of the first }
  Sleep(1000);
  MakeSampleRevTwo;
  Exec('ss Checkin ' + TestFileNames[0] + ' -CComment -Y' + FUserPwd);
  ChDir(CurrDir);

  FVssGetTask.VssPath := FVssPath;
  FVssGetTask.Login := FUserPwd;
  FVssGetTask.LocalPath := FTestDir;
  FVssGetTask._Label := LabelName;
  FVssGetTask.Execute;
  CheckEquals(FRevOneContents, ReadSampleContents, 'wrong version retrieved');

  { remove the label. The only way to remove a label is to re-submit a label
    command that adds a single space label to the version of the previous
    label (see MS KBase article Q126786)

    -I-Y required to answer yes to removing the label confirmation  }
  Exec('ss Label ' + FVssPath + ' "-vl ' + LabelName + '" "-l " ' +
    '-I-Y -Y' + FUserPwd);
end;

procedure TTestVssGetTask.TestVssGetTaskRelPath;
begin
  FLongFNTestDir := '.\my test dir';
  FVssGetTask.VssPath := FVssPath;
  FVssGetTask.Login := FUserPwd;
  FVssGetTask.LocalPath := FLongFNTestDir;
  FVssGetTask.Execute;
  Check(FileExists(FLongFNTestDir + '\' + TestFileNames[0]), 'file not retrieved');
end;

{ TTestVssCheckoutTask }

procedure TTestVssCheckoutTask.Setup;
begin
  inherited;
  FVssCheckoutTask := TVssCheckoutTask.Create(FProject.AddTarget('test'));
end;

procedure TTestVssCheckoutTask.TearDown;
begin
  FVssCheckoutTask.Free;
  inherited;
end;

procedure TTestVssCheckoutTask.TestVssCheckoutTask;
begin
  AddTestFile; // 2nd file

  FVssCheckoutTask.VssPath := FVssPath;
  FVssCheckoutTask.Login := FUserPwd;
  FVssCheckoutTask.LocalPath := FTestDir;
  FVssCheckoutTask.FileName := TestFileNames[0];
  FVssCheckoutTask.Execute;
  Check(WinExec32AndWait('ss Status ' + FVssPath + '/' + TestFileNames[0] +
    ' -U -Y' + FUserPwd, 0) <> 0, 'file should be checked out, but it ain''t');
  Check(WinExec32AndWait('ss Status ' + FVssPath + '/' + TestFileNames[1] +
    ' -U -Y' + FUserPwd, 0) = 0, '2nd file shouldn''t be checked out, but is');
end;

{ TTestVssCheckinTask }

procedure TTestVssCheckinTask.Setup;
begin
  inherited;
  FVssCheckinTask := TVssCheckinTask.Create(FProject.AddTarget('test'));
end;

procedure TTestVssCheckinTask.TearDown;
begin
  FVssCheckinTask.Free;
  inherited;
end;

procedure TTestVssCheckinTask.TestVssCheckinTask;
begin
  AddTestFile;

  Exec('ss Checkout ' + TestFileNames[0] + ' -GL"' + FTestDir + '"' +
    ' -Y' + FUserPwd);
  Exec('ss Checkout ' + TestFileNames[1] + ' -GL"' + FTestDir + '"' +
    ' -Y' + FUserPwd);
  Check(WinExec32AndWait('ss Status ' + FVssPath + '/' + TestFileNames[0] +
    ' -U -Y' + FUserPwd, 0) <> 0, 'file should be checked out, but it ain''t');

  { modify the content, otherwise vss will undo the checkout rather than
    actually checking in }
  MakeSampleRevTwo;
  FVssCheckinTask.VssPath := FVssPath;
  FVssCheckinTask.Login := FUserPwd;
  FVssCheckinTask.LocalPath := FTestDir;
  FVssCheckinTask.FileName := TestFileNames[0];
  FVssCheckinTask.Comment := 'test checkin';
  FVssCheckinTask.Execute;

  Check(WinExec32AndWait('ss Status ' + FVssPath + '/' + TestFileNames[0] +
    ' -U -Y' + FUserPwd, 0) = 0, 'file shouldn''t be checked out, but is');

  Check(WinExec32AndWait('ss Status ' + FVssPath +
    ' -U -Y' + FUserPwd, 0) <> 0,
    '2nd file should still be checked out, but isn''t');

  { currently no way to double check that the comment went in without parsing
    a ss History call (any other way?), and I don't have time to mess with that
    right now, unfortunately. }
end;

initialization
  {$IFNDEF NO_RUN_VSS_TESTS}
  RegisterTest('Acceptance Suite', TTestVssGetTask);
  RegisterTest('Acceptance Suite', TTestVssCheckoutTask);
  RegisterTest('Acceptance Suite', TTestVssCheckinTask);
  {$ENDIF}
end.

