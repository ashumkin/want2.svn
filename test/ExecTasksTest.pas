unit ExecTasksTest;

interface

uses ExecTasks, TestFramework;

type
  THackedCustomExecTask = class(TCustomExecTask);

  TTestCustomExecTask = class(TTestCase)
  private
    FCustomExecTask: THackedCustomExecTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestBuildCmdLine;
  end;

implementation

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

initialization
  RegisterTest('Unit Tests', TTestCustomExecTask);
  
end.

