unit VersionInfoTasksTest;

interface

uses
  TestFramework, VersionInfoTasks, clVersionRcUnitTest, DanteClasses;

type
  TTestIncVerRcTask = class(TTestRcUnit)
  private
    FIncVerRcTask: TVersionInfoTask;
    FProject: TProject;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestIncVerRcTask;
  end;

implementation

procedure TTestIncVerRcTask.Setup;
var
  T: TTarget;
begin
  inherited;
  FProject := TProject.Create;
  T := FProject.AddTarget('update_rc_file');
  FIncVerRcTask := TVersionInfoTask.Create(T);
end;

procedure TTestIncVerRcTask.TearDown;
begin
  FProject.Free;
  inherited;
end;

procedure TTestIncVerRcTask.TestIncVerRcTask;
begin
  FIncVerRcTask.RcFileName := FIncVerRcTask.ToDantePath(FTestRcName);
  FIncVerRcTask.Increment  := True;
  FIncVerRcTask.Init;
  FIncVerRcTask.Execute;
  CheckEquals('53', FProject.PropertyValue('build'), 'build property');
end;

initialization
  RegisterTests('Version Info Tasks', [TTestIncVerRcTask.Suite]);


end.

