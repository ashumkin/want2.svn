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
  Base := '/SomeRoot/subdir/src';
  Path := '/dev/Borland/Delphi5/bin/dcc32.exe';
  CheckEquals('../../..' + Path, WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path not in Base');

  Base := '/SomeRoot/subdir/src';
  Path := '/SomeRoot/subdir/src/tasks';
  CheckEquals('tasks', WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path subdir');

  Base := '/SomeRoot/subdir/src';
  Path := '/SomeRoot/subdir/src';
  CheckEquals('.', WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path = Base');

  Base := '/subdir/src';
  Path := '/SomeRoot/subdir/src';
  CheckEquals('../..' + Path, WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path not in Base');

  Base := '/SomeRoot/subdir/src';
  Path := '/SomeRoot';
  CheckEquals('../../', WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath - Path parent of Base');

  Path := '/d:/dev/Borland/Delphi5/bin/dcc32.exe';
  Base := '/S:/SomeRoot/subdir/src';
  CheckEquals(Path, WildPaths.ToRelativePath(Path, Base),
    'ToRelativePath with different drive absolute');

  { any case like previous where drive letter was not in same place in both
    paths? -- shouldn't be }

  (* If you remove the checks in ToRelativePath ensuring both Path and Base
     are already absolute paths, then a lot of these tests below work ... but
     this means ToRelativePath is assuming that any relative markers (..) in
     a path represent the same thing, of which there's no guarantee, and this
     could deceive code using this method.

     However, this assumptive behavior could be useful as in the first test
     below. If I know that the up (..) placeholders are equivalent (represent
     the same absolute), then I could use ToRelativePath successfully to get
     ./testA/AStuff. Otherwise, I have to go the extra step of replacing ..
     before calling ToRelativePath. However, some cases are more obviously
     arbitrary than others. If the following was implemented, there'd have to be
     some way of forcing the user to state what's common ... but that's probably
     as much or more work than simply forcing them to use Absolute paths.
     -- Chrismo

  Base := '../sample/test';
  Path := '../sample/test/testA/AStuff';
  CheckEquals('./testA/AStuff', WildPaths.ToRelativePath(Path, Base),
    'relative base and path, path subdir of base');

  == Additional examples of assumptive relative behavior

  // Works
  Base := '../sample/test';
  Path := '../sample/tst/testA/AStuff';
  CheckEquals('../tst/testA/AStuff', WildPaths.ToRelativePath(Path, Base),
    'relative base and path, path on different branch of base');

  // Works
  Base := '../sample/test';
  Path := '/tst/testA/AStuff';
  CheckEquals('/tst/testA/AStuff', WildPaths.ToRelativePath(Path, Base),
    'relative base, absolute path, path not in base');

  // Works
  Base := '../sample/test';
  Path := '/tst/testA/AStuff';
  CheckEquals('/tst/testA/AStuff', WildPaths.ToRelativePath(Path, Base),
    'relative base, absolute path, path not in base');

  // Doesn't work
  Base := '/sample/test';
  Path := '../test/testA/AStuff';
  { Absolute of Path could be /sample/test/testA/AStuff OR
    /different/yuk/test/testA/AStuff OR
    anything, can't assume the two 'test' directories are on the same path }
  CheckEquals('../test/testA/AStuff', WildPaths.ToRelativePath(Path, Base),
    'absolute base, relative path, cannot determine if Path in Base or not');*)
end;

initialization
  RegisterTest('Path Tests', TTestToRelativePath);

end.

