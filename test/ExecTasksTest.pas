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

