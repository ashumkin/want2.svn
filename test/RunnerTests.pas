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
unit DanteMainTest;

interface

uses
  SysUtils,
  TestFramework,
  WildPaths,
  DanteMain,
  DanteTestUtil,
  DanteClassesTest;

type
  TTestDanteMain = class(TTestDirCase)
  private
    FBuildFile: TextFile;
    FBuildFileName: string;
    FCopyOfFileName: string;
    FDante: TDante;
    FNewCopyOfFileName: string;
    FNewDir: string;
  protected
    procedure MakeTestBuildFile;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestDanteMain;
  end;

implementation

uses JclFileUtils;

{ TTestDanteMain }

procedure TTestDanteMain.MakeTestBuildFile;
const
  CR = #$D#$A;
var
  Content: string;
begin
  AssignFile(FBuildFile, FBuildFileName);
  Rewrite(FBuildFile);

  Content :=
    CR+
    '<project name="test_project" default="main">'                     + CR +
{    '  <echo message="Test message with ""quotes"" in it" />'          + CR +
     this fails -- should it? I'm not up on DOM/SAX reqs                 }
    '  <property name="test" value="sample" />'                        + CR +
    '  <target name="main">'                                           + CR +
    '    <shell executable="mkdir ' + FNewDir + '" />'                 + CR +
    '    <shell executable="copy '
             + FBuildFileName + ' ' + FCopyOfFileName + '" />'         + CR +
    '    <shell executable="copy '
             + FBuildFileName + ' ' + FNewCopyOfFileName + '" />'      + CR +
    '    <delete dir="' + ToPath(FNewDir) + '" />'                     + CR +
    '  </target>'                                                      + CR +
    '</project>'                                                       + CR;

  WriteLn(FBuildFile, Content);
  CloseFile(FBuildFile);
end;

procedure TTestDanteMain.Setup;
begin
  inherited;
  FDante := TDante.Create;
  FBuildFileName := FTestDir + '\build.xml';
  FCopyOfFileName := FTestDir + '\copyofbuild.xml';
  FNewDir := FTestDir + '\new';
  FNewCopyOfFileName := FNewDir + '\copyofbuild.xml';
end;

procedure TTestDanteMain.TearDown;
begin
  FDante.Free;
  inherited;
end;

procedure TTestDanteMain.TestDanteMain;
begin
  MakeTestBuildFile;
  FDante.DoBuild(FBuildFileName);
  Check(FileExists(FCopyOfFileName), 'copy doesn''t exist');
  Check(not DirectoryExists(FNewDir), 'directory exists');
end;

initialization
  RegisterTest('Acceptance Suite', TTestDanteMain);

end.

