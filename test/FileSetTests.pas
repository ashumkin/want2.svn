unit FileSetTests;

interface
uses
  WildPaths,
  FileSets,
  FileOps,
  TestFramework,

  SysUtils,
  Classes,

  DanteClassesTest;

type
  TPathsTestCase = class(TProjectBaseCase)
  protected
    procedure CheckMatch(Path, Spec :string; Msg :string = '');
    procedure CheckNoMatch(Path, Spec :string; Msg :string = '');
  published
    procedure TestSystemPaths;
    procedure TestRelative;
    procedure TestPathMatches;
    procedure TestFileMatches;
    procedure TestResolve;
  end;

implementation


{ TPathsTestCase }

procedure TPathsTestCase.CheckMatch(Path, Spec, Msg: string);
begin
  if not IsMatch(Path, Spec) then
    fail(Format('%s"<%s> does not match <%s>', [Msg, Path, Spec]));
end;


procedure TPathsTestCase.CheckNoMatch(Path, Spec, Msg: string);
begin
  if IsMatch(Path, Spec) then
    fail(Format('%s <%s> should not match <%s>', [Msg, Path, Spec]));
end;


procedure TPathsTestCase.TestSystemPaths;
begin
  CheckEquals('/c:/', ToPath('c:\'));
  CheckEquals('/c:/tmp', ToPath('c:\tmp'));
  CheckEquals('//machine/c', ToPath('\\machine\c'));
end;

procedure TPathsTestCase.TestRelative;
var
  B, P :TPath;
begin
  P := '//machine/c';
  B := '';
  ForceRelativePath(P, B);
  CheckEquals('c', P);
  CheckEquals('//machine', B);

  P := '/c:/';
  B := '';
  ForceRelativePath(P, B);
  CheckEquals('', P);
  CheckEquals('/c:', B);

  P := ToPath('c:\');
  B := '';
  CheckEquals('/c:/', P);
  ForceRelativePath(P, B);
  CheckEquals('/c:', B);
end;

procedure TPathsTestCase.TestFileMatches;
begin
  CheckMatch('abcde', 'abcde');
  CheckMatch('abcde', '*');
  CheckMatch('abcde', 'a*e');
  CheckMatch('abcde', 'a*?e');
  CheckMatch('abcde', 'a???e');
  CheckMatch('abcde', 'a??????e');
  CheckMatch('/x/y/abcde', '**/a*c*e');
  CheckMatch('/home/dunit/examples/structure/Makefile', '/**/eXamPles/**/*');
  CheckMatch('test/Test.dpr', 'test/*');

  CheckNoMatch('abcde', 'a??e');
  CheckNoMatch('/x/y/abcde', '**/a*x*e');
end;

procedure TPathsTestCase.TestPathMatches;
begin
  CheckMatch('/a/b/c/d', '/a/b/c/d');
  CheckMatch('/a/b/c/d', '/a/b/c/*');
  CheckMatch('/a/b/c/d', '/a/*/c/d');
  CheckMatch('/a/b/c/d', '/a/**/d');
  CheckMatch('/a/b/c/d', '/a/**/*');
  CheckMatch('/a/b/c/d', '/**/*');
  CheckMatch('/a/b/c/d', '/**/d');
  CheckNoMatch('/a/b/c/d', '/a/b/c/f');
  CheckNoMatch('/a/b/c/d', '/a/b/x/d');
  CheckNoMatch('/a/b/c/d', '/**/f');
end;

procedure TestMatch(p, s :string);
begin
   write(p,'  ', s, ' ');
   writeln(IsMatch(p,s));
end;


procedure TPathsTestCase.TestResolve;
var
  FS    :TFileSet;
begin
  FS := TFileSet.Create(FProject);
  try
    FS.Dir := '/c:/tmp';
    FS.Include('**/*.pas');
    FS.Include('**/*.dpr');
    FS.Include('**/*.html');
    FS.Include('**/*.css');
    FS.Include('doc/**/*');
    FS.Include('du/**/*.txt');
    FS.Exclude('test/*');
    FS.Exclude('**/*Test*');
    TouchFile('/tmp/test.txt');
    Check(IsFile('/tmp/test.txt'));
    (*
    FS.DeleteFiles;
    FS.CopyFiles('//dumbo/c/temp');
    FS.MoveFiles('//dumbo/c/temp');
    *)
  finally
    FS.Free;
  end;
end;

initialization
  RegisterTests('', [TestSuiteOf(TPathsTestCase)]);
end.
