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
unit ExecTasksTest;

interface

uses ExecTasks, TestFramework;

type
  THackedCustomExecTask = class(TCustomExecTask);
  THackedShellTask = class(TShellTask);

  TTestCustomExecTask = class(TTestCase)
  private
    FCustomExecTask: THackedCustomExecTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestBuildCmdLine;
  end;

  TTestShellTask = class(TTestCase)
  private
    FShellTask: THackedShellTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestBuildCmdLine;
  end;

implementation

uses JclSysInfo;

{ TTestCustomExecTask }

procedure TTestCustomExecTask.Setup;
begin
  inherited;
  FCustomExecTask := THackedCustomExecTask.Create(nil);
end;

procedure TTestCustomExecTask.TearDown;
begin
  FCustomExecTask.Free;
  inherited;
end;

procedure TTestCustomExecTask.TestBuildCmdLine;
begin
  FCustomExecTask.Executable := 'cmd.exe';
  FCustomExecTask.ArgumentList.Add('/c copy');
  FCustomExecTask.ArgumentList.Add('file1.txt');
  FCustomExecTask.ArgumentList.Add('"c:\dir w space\filecpy.txt"');
  CheckEquals(
    'cmd.exe /c copy file1.txt "c:\dir w space\filecpy.txt"',
    FCustomExecTask.BuildCmdLine, 'BuildCmdLine failed');
end;

{ TTestShellTask }

procedure TTestShellTask.Setup;
begin
  inherited;
  FShellTask := THackedShellTask.Create(nil);
end;

procedure TTestShellTask.TearDown;
begin
  FShellTask.Free;
  inherited;
end;

procedure TTestShellTask.TestBuildCmdLine;
var
  OrigValue: boolean;
begin
  OrigValue := JclSysInfo.IsWinNT;
  try
    FShellTask.Executable := 'dir';
    JclSysInfo.IsWinNT := false;
    CheckEquals('command.com /c dir', FShellTask.BuildCmdLine);
    JclSysInfo.IsWinNT := true;
    CheckEquals('cmd.exe /c dir', FShellTask.BuildCmdLine);
  finally
    JclSysInfo.IsWinNT := OrigValue;
  end;
end;

initialization
  RegisterTest('Unit Tests', TTestCustomExecTask);
  RegisterTest('Unit Tests', TTestShellTask);

end.

