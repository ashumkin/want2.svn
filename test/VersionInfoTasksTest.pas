unit VersionInfoTasksTest;

interface

uses
  TestFramework,
  clVersionRcUnitTest,
  VersionInfoTasks,
  WantClasses;

type
  TTestVersioninfoTask = class(TTestRcUnit)
  private
    FIncVerRcTask: TVersionInfoTask;
    FProject:TProject;

  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestIncVerRcTask;
  end;

implementation

procedure TTestVersioninfoTask.Setup;
var
  T: TTarget;
begin
  inherited;
  FProject := TProject.Create;
  T := FProject.AddTarget('update_rc_file');
  FIncVerRcTask := TVersionInfoTask.Create(T);
end;

procedure TTestVersioninfoTask.TearDown;
begin
  FProject.Free;
  inherited;
end;

procedure TTestVersioninfoTask.TestIncVerRcTask;
begin
  FIncVerRcTask.RcFileName := FIncVerRcTask.ToWantPath(FTestRcName);
  FIncVerRcTask.Increment  := True;
  FIncVerRcTask.Init;
  FIncVerRcTask.Execute;
  CheckEquals('53', FProject.PropertyValue('build'), 'build property');
end;

initialization
  RegisterTests('Version Info Tasks', [TTestVersioninfoTask.Suite]);


end.

