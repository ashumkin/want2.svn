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
    FSampleFileName: string;
    FUserPwd: string;
    FVssPath: string;
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
  end;

implementation

{ TTestVssTasksBase }

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
    
    FSampleFileName := MakeSampleTextFile;
    WinExec32AndWait('ss CP "' + FVssPath + '" -I- -Y' + FUserPwd, 0);
    WinExec32AndWait('ss Add "' + FSampleFileName + '" -I- -Y' + FUserPwd, 0);
    if FSampleFileName <> '' then
      JclShell.SHDeleteFiles(0, FSampleFileName, [doSilent, doAllowUndo]);
  {$ENDIF}
end;

procedure TTestVssTasksBase.TearDown;
begin
  {$IFDEF RUN_VSS_TESTS}
    { if you want to purge these test files, you'll have to do that manually }
    WinExec32AndWait('ss CP "' + FVssPath + '" -I- -Y' + FUserPwd, 0);
    WinExec32AndWait('ss Delete "' + ExtractFileName(FSampleFileName) +
      '" -I- -Y' + FUserPwd, 0);
  {$ENDIF}
  inherited;
end;

{ TTestVssGetTask }

procedure TTestVssGetTask.Setup;
begin
  inherited;
  FVssGetTask := TVssGetTask.Create(nil);
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
    on EDanteError do
      { nada }
  end;
  
  FVssGetTask.VssPath := FVssPath;
  FVssGetTask.Validate;
  
  FVssGetTask.Login := FUserPwd;
  FVssGetTask.LocalPath := FTestDir;
  FVssGetTask.FileName := ExtractFileName(FSampleFileName);
  FVssGetTask.Execute;
  Check(FileExists(FSampleFileName), 'file not retrieved');
end;

initialization
  RegisterTest('Acceptance Suite', TTestVssGetTask);

end.

