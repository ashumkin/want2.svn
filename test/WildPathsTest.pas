unit WildPathsTest;

interface

uses
  TestFramework, WildPaths;

type
  TTestToRelativePath = class(TTestCase)
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestToRelativePath;
  end;

implementation

{ TTestToRelativePath }

procedure TTestToRelativePath.Setup;
begin
  inherited;

end;

procedure TTestToRelativePath.TearDown;
begin
  inherited;

end;

procedure TTestToRelativePath.TestToRelativePath;
var
  Path: string;
  Base: string;
begin
  Path := '/dev/Borland/Delphi5/bin/dcc32.exe';
  Base := '/SF-dante/dante/src';
  CheckEquals('../../..' + Path, WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path not in Base');

  Path := '/SF-dante/dante/src/tasks';
  Base := '/SF-dante/dante/src';
  CheckEquals('./tasks', WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path subdir');

  Path := '/SF-dante/dante/src';
  Base := '/SF-dante/dante/src';
  CheckEquals('.', WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path = Base');

  Path := '/SF-dante/dante/src';
  Base := '/dante/src';
  CheckEquals('../..' + Path, WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path not in Base');

  Path := '/SF-dante';
  Base := '/SF-dante/dante/src';
  CheckEquals('../../', WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path parent of Base');

  Path := '/d:/dev/Borland/Delphi5/bin/dcc32.exe';
  Base := '/S:/SF-dante/dante/src';
  CheckEquals(Path, WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath with different drive absolute');

  { any case like previous where drive letter was not in same place in both
    paths? }   
end;

initialization
  RegisterTest('Path Tests', TTestToRelativePath);

end.

