{ $Id$ }
{
--------------------------------------------------------------------------------
Copyright (c) 2001, Dante Authors -- See authors.txt for complete list
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The name Dante, the names of the authors in authors.txt and the names of
other contributors to this software may not be used to endorse or promote
products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------
}
unit FileSetTests;

interface
uses
  WildPaths,
  FileSets,
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
    procedure TestConcat;
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


procedure TPathsTestCase.TestConcat;
const
  empty  = '';
  abs    = '/c:/tmp';
  rel    = 'some/path';
  dot    = '.';
  dotrel = dot + '/' + rel;
  up     = '..';
  uprel  = up + '/' + rel;
begin
  CheckEquals(dot,       PathConcat(empty, dot));
  CheckEquals(dot,       PathConcat(dot,    empty));

  CheckEquals(up,        PathConcat(empty, up));
  CheckEquals(up,        PathConcat(up,    empty));

  CheckEquals(up,        PathConcat(up,    dot));
  CheckEquals(up,        PathConcat(dot,   up), 'dot + up');

  CheckEquals(abs,       PathConcat(abs, empty));
  CheckEquals(abs,       PathConcat(empty, abs));
  CheckEquals(abs,       PathConcat(abs, abs));
  CheckEquals(abs,       PathConcat(abs, dot));
  CheckEquals(abs,       PathConcat(dot, abs));
  CheckEquals('/c:',     PathConcat(abs, up));
  CheckEquals(abs,       PathConcat(up,  abs));

  CheckEquals(rel,       PathConcat(rel, empty));
  CheckEquals(rel,       PathConcat(empty, rel));
  CheckEquals(rel,       PathConcat(rel, dot));
  CheckEquals(dotrel,    PathConcat(dot, rel));
  CheckEquals('some',    PathConcat(rel, up));
  CheckEquals(uprel,     PathConcat(up,  rel));

  CheckEquals(uprel,     PathConcat(empty, uprel));
  CheckEquals(uprel,     PathConcat(uprel, empty));

  CheckEquals(abs +'/' + rel,   PathConcat(abs, rel));
  CheckEquals(abs +'/' + rel,   PathConcat(abs, dotrel));
  CheckEquals(abs           ,   PathConcat(rel, abs));
  CheckEquals('/c:/' + rel,     PathConcat(abs, uprel));
  CheckEquals(rel + '/'+ rel,   PathConcat(rel, rel));
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
  CheckEquals('', P);
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
const
  test_dir = '/c:/tmp';
var
  FS    :TFileSet;
begin
  FS := TFileSet.Create(FProject);
  FS.basedir := test_dir;
  try
    FS.Include('**/*.pas');
    FS.Include('**/*.dpr');
    FS.Include('**/*.html');
    FS.Include('**/*.css');
    FS.Include('doc/**/*');
    FS.Include('du/**/*.txt');
    FS.Exclude('test/*');
    FS.Exclude('**/*Test*');
    TouchFile('/tmp/test.txt');
    Check(PathIsFile('/tmp/test.txt'));
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
  RegisterTests('FileSet', [TestSuiteOf(TPathsTestCase)]);
end.
