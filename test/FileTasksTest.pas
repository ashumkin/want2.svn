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
    procedure TestDeleteTask;
  end;

implementation

{ TTestDeleteTask }

procedure TTestDeleteTask.Setup;
begin
  inherited;
  FDeleteTask := TDeleteTask.Create(nil);
end;

procedure TTestDeleteTask.TearDown;
begin
  FDeleteTask.Free;
  inherited;
end;

procedure TTestDeleteTask.TestDeleteTask;
begin

end;

initialization
  RegisterTest('Unit Tests', TTestDeleteTask);

end.

