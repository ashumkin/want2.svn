unit FileTasksTest;

interface

uses TestFramework, FileTasks, DanteClassesTest;

type
  TTestDeleteTask = class(TTestDirCase)
  private
    FDeleteTask: TDeleteTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestDeleteDir;
    procedure TestDeleteDirRelative;
  end;

implementation

uses JclFileUtils;

{ TTestDeleteTask }

procedure TTestDeleteTask.Setup;
begin
  inherited;
  FDeleteTask := TDeleteTask.Create(FProject.AddTarget('test_delete_task'));
end;

procedure TTestDeleteTask.TearDown;
begin
  FDeleteTask.Free;
  inherited;
end;

procedure TTestDeleteTask.TestDeleteDir;
begin
  CheckEquals('delete', TDeleteTask.XMLTag, 'XMLTag is wrong');
  MakeSampleTextFile;
  FDeleteTask.Dir := FTestDir;
  FDeleteTask.Execute;
  Check(DirectoryExists(FTestDir), 'directory not deleted');

  // ensure it doesn't blow up trying to delete a directory that's gone
  FDeleteTask.Execute;
end;

procedure TTestDeleteTask.TestDeleteDirRelative;
var
  SiblingDir: string;
begin
  MakeSampleTextFile;

  // need routine (add to clLib) to grab FTestDir parent (ExtractFilePathParent)
  SiblingDir := FTestDir;

end;

initialization
  RegisterTest('Unit Tests', TTestDeleteTask);

end.

