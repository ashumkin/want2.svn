(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit RunnerTests;

interface

uses
  SysUtils,
  TestFramework,
  WildPaths,
  WantClasses,
  WantRunner,
  WantClassesTest,
  StandardElements,
  StandardTasks;

type
  TTestWantMain = class(TTestDirCase)
  private
    FBuildFile: TextFile;
    FBuildFileName: string;
    FCopyOfFileName: string;
    FWant: TWant;
    FNewCopyOfFileName: string;
    FNewDir: string;
    FCopyDir: string;
  protected
    procedure MakeTestBuildFile;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestWantMain;
  end;

implementation

uses JclFileUtils;

{ TTestWantMain }

procedure TTestWantMain.MakeTestBuildFile;
const
  CR = #$D#$A;
var
  Content: string;
begin
  AssignFile(FBuildFile, FBuildFileName);
  Rewrite(FBuildFile);

  Content :=
    CR+
    '<project name="test_project" default="main">                    '+ CR +
{    '  <echo message="Test message with ""quotes"" in it" />         '+ CR +
     this fails -- should it? I'm not up on DOM/SAX reqs                 }
    '  <property name="test" value="sample" />                       '+ CR +
    '  <target name="main">                                          '+ CR +
    '    <shell executable="mkdir" arguments="' + FNewDir + '"       '+ CR +
    '           failonerror="no" />                                  '+ CR +
    '    <mkdir dir="' + ToPath(FCopyDir) + '" />                    '+ CR +
    '    <shell executable="copy" arguments="                        '+
             FBuildFileName + ' ' + FCopyOfFileName + '"             '+ CR +
    '           failonerror="no" />                                  '+ CR +
    '    <shell executable="copy" arguments="                        '+
             FBuildFileName + ' ' + FNewCopyOfFileName + '" />       '+ CR +
    '    <copy todir="' + ToPath(FCopyDir) + '">                     '+ CR +
    '      <fileset dir="' + ToPath(FNewDir) + '">                   '+ CR +
    '        <include name="**/*.*" />                               '+ CR +
    '      </fileset>                                                '+ CR +
    '    </copy>                                                     '+ CR +
    '    <delete dir="' + ToPath(FNewDir) + '" />                    '+ CR +
    '  </target>                                                     '+ CR +
    '</project>                                                      '+ CR;

  WriteLn(FBuildFile, Content);
  CloseFile(FBuildFile);                                   
end;

procedure TTestWantMain.Setup;
begin
  inherited;
  FWant := TWant.Create;
  {$IFNDEF USE_TEXT_RUNNER}
  FWant.CreateLogManager;
  {$ENDIF}

  FBuildFileName := FTestDir + '\build.xml';
  FCopyOfFileName := FTestDir + '\copyofbuild.xml';
  FNewDir := FTestDir + '\new';
  FNewCopyOfFileName := FNewDir + '\copyofbuild.xml';
  FCopyDir := FTestDir + '\copy';
end;

procedure TTestWantMain.TearDown;
begin
  FWant.Free;
  inherited;
end;

procedure TTestWantMain.TestWantMain;
var
  CurDir: string;
begin
  CurDir := GetCurrentDir;
  MakeTestBuildFile;
  FWant.DoBuild(FBuildFileName, vlWarnings);

  { leaving CurrentDir is important for other tests depend on it, because
    TProject.FRootDir defaults to CurrentDir. }
  CheckEquals(CurDir, GetCurrentDir, 'current dir not left intact');

  Check(FileExists(FCopyOfFileName), 'copy doesn''t exist');
  Check(not DirectoryExists(FNewDir), 'directory exists');
  Check(FileExists(FCopyDir + '\copyofbuild.xml'), 'copy doesn''t exist');
end;

initialization
  RegisterTest('Acceptance Suite', TTestWantMain);

end.

