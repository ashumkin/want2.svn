unit ExecTasksTest;

interface

uses ExecTasks, TestFramework;

type
  THackedCustomExecTask = class(TCustomExecTask);
  THackedShellExecTask = class(TShellExecTask);

  TTestCustomExecTask = class(TTestCase)
  private
    FCustomExecTask: THackedCustomExecTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestBuildCmdLine;
  end;

  TTestShellExecTask = class(TTestCase)
  private
    FShellExecTask: THackedShellExecTask;
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

{ TTestShellExecTask }

procedure TTestShellExecTask.Setup;
begin
  inherited;
  FShellExecTask := THackedShellExecTask.Create(nil);
end;

procedure TTestShellExecTask.TearDown;
begin
  FShellExecTask.Free;
  inherited;
end;

procedure TTestShellExecTask.TestBuildCmdLine;
var
  OrigValue: boolean;
begin
  OrigValue := JclSysInfo.IsWinNT;
  try
    FShellExecTask.Executable := 'dir';
    JclSysInfo.IsWinNT := false;
    CheckEquals('command.com /c dir', FShellExecTask.BuildCmdLine);
    JclSysInfo.IsWinNT := true;
    CheckEquals('cmd.exe /c dir', FShellExecTask.BuildCmdLine);
  finally
    JclSysInfo.IsWinNT := OrigValue;
  end;
end;

initialization
  RegisterTest('Unit Tests', TTestCustomExecTask);
  RegisterTest('Unit Tests', TTestShellExecTask);

end.

