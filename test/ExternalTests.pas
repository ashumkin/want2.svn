(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit ExternalTests;

interface

uses
  TestFramework,
  clUtilFile,
  ZipStreams;

type
  TExternalTest = class(TTestCase)
  private
    FRootTestDataDir: string;
    FRootTestExeDir: string;
    FTestExeFinalDir: string;
    FTestExeSetupDir: string;
    FTestPath: string;
    procedure SetTestName;
    procedure SetTestPath(const Value: string);
  protected
    function BuildFileName: string;
    procedure CompareActualToFinal;
    procedure DeleteSubFolders;
    function FinalFileName: string;
    function SetupFileName: string;
    procedure Unzip(ZipFileName: string; Directory: string);
    procedure UnzipFinal;
    procedure UnzipSetup;
    procedure VerifyFinal;
  public
    constructor Create; reintroduce; overload;
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure DoTest;

    property TestPath: string read FTestPath write SetTestPath;
  end;

implementation

uses
  JclFileUtils,
  Classes,
  Math,
  Windows,
  SysUtils,
  ScriptRunner,
  JclMiscel,
  JclShell,
  WildPaths,
  WantClasses;

procedure LoadTests;
var
  Files: TStringList;
  i: Integer;
  ATest: TExternalTest;
  BasePath: string;
  SuitePath: string;
begin
  BasePath := '../test/data';
  Files := TStringList.Create;
  try
    WildPaths.Wild(Files, '**/*.xml', BasePath);
    for i := 0 to Files.Count - 1 do
    begin
      { TODO: it'd be nice in GUITestRunner to have the test name be the
        last directory in the test path. Right now it's always 'build' and
        adds an unecessary child node in the test tree. Original design
        had unique test name in TESTNAME.xml. Now it's a unique folder
        with a build.xml file in it. }
      ATest := TExternalTest.Create;
      ATest.TestPath := ExtractFilePath(ToSystemPath(Files[i]));

      // need to chop off last entry in path for suite path. Last entry test
      // will make its name.
      SuitePath := SuperPath(ToPath(ATest.TestPath));
      RegisterTest('Acceptance Suite.External Tests.' + SuitePath, ATest);
    end;
  finally
    Files.Free;
  end;
end;

{ TExternalTest }

function TExternalTest.BuildFileName: string;
begin
  Result := 'build.xml';
end;

procedure TExternalTest.CompareActualToFinal;
var
  ADirComp: TclDirectoryCompare;
  p       : Integer;
begin
  ADirComp := TclDirectoryCompare.Create(FTestExeSetupDir, FTestExeFinalDir);
  try
     with ADirComp do
     begin
       GetRelativeFiles;

       for p := 0 to AFiles.Count-1 do
         if Pos('CVS\', AFiles[p]) = 0 then
           Check(BFiles.IndexOf(AFiles[p]) >= 0, Format('%s in setup but not in final', [AFiles[p]]));

       for p := 0 to BFiles.Count-1 do
         if Pos('CVS\', BFiles[p]) = 0 then
           Check(AFiles.IndexOf(BFiles[p]) >= 0, Format('%s in final but not in setup', [BFiles[p]]));

       GetFiles;
       for p := 0 to Min(AFiles.Count, BFiles.Count)-1 do
       begin
         if Pos('CVS\', AFiles[p]) = 0 then
         begin
           CheckEquals(IsDirectory(AFiles[p]), IsDirectory(BFiles[p]), Format('%s files not both directories', [ExtractFileName(AFiles[p])]));;
           if not IsDirectory(Afiles[p]) then
             Check(TclFileCompare.CompareFiles(AFiles[p], BFiles[p]), Format('%s files are different', [ExtractFileName(AFiles[p])]));;
           end;
       end;
     end;
  finally
    ADirComp.Free;
  end;
end;

constructor TExternalTest.Create;
begin
  Create('DoTest');

  FRootTestDataDir := '..\test\data\';
  FRootTestExeDir := ExtractFilePath(ParamStr(0)) + 'test\';
  FTestExeSetupDir := FRootTestExeDir + 'setup\';
  FTestExeFinalDir := FRootTestExeDir + 'final\';
end;

procedure TExternalTest.DeleteSubFolders;
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  { make sure we haven't got off on the root dir or something heinous }
  if DirectoryExists(FRootTestExeDir + 'setup') then
    JclFileUtils.DelTree(FRootTestExeDir);
end;

procedure TExternalTest.DoTest;
var
  Runner: TScriptRunner;
begin
  Runner := TScriptRunner.Create;
  {$IFNDEF USE_TEXT_RUNNER}
     Runner.CreateLogManager;
  {$ENDIF}
  try
    Runner.DoBuild(FTestExeSetupDir + BuildFileName, vlVerbose);
  finally
    Runner.Free;
  end;
  VerifyFinal;
end;

function TExternalTest.FinalFileName: string;
begin
  Result := 'final.zip';
end;

procedure TExternalTest.SetTestName;
var
  Paths: TPaths;
begin
  { FTestPath is now used in the Suite hierarchy. See LoadTests }
  Paths := SplitPath(ToPath(FTestPath));
  FTestName := Paths[High(Paths)];
end;

procedure TExternalTest.SetTestPath(const Value: string);
begin
  FTestPath := StringReplace(Value, FRootTestDataDir, '', []);
  SetTestName;
end;

procedure TExternalTest.Setup;
begin
  inherited;
  UnzipSetup;
end;

function TExternalTest.SetupFileName: string;
begin
  Result := 'setup.zip';
end;

procedure TExternalTest.TearDown;
begin
  DeleteSubFolders;
  inherited;
end;

procedure TExternalTest.Unzip(ZipFileName, Directory: string);

  procedure DoCopy(FileName: string);
  begin
    if not Windows.CopyFile(
      PChar(FRootTestDataDir + FTestPath + FileName),
      PChar(Directory + FileName), false) then
      RaiseLastWin32Error;
  end;
var
  ZipLocation  :string;
  DirLocation  :string;
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  JclFileUtils.ForceDirectories(Directory);
  DoCopy(BuildFileName);
  ZipLocation := FRootTestDataDir + FTestPath + ZipFileName;
  if FileExists(ZipLocation) then
    ZipStreams.ExtractAll(ToPath(ZipLocation), ToPath(Directory))
  else
  begin
    DirLocation := ChangeFileExt(ZipLocation, '');
    if DirectoryExists(DirLocation) then
      CopyFiles('**', ToPath(DirLocation), ToPath(Directory))
    else
      Fail('Could not find ' + ZipLocation);
  end;
end;

procedure TExternalTest.UnzipFinal;
begin
  Unzip(FinalFileName, FTestExeFinalDir);
end;

procedure TExternalTest.UnzipSetup;
begin
  Unzip(SetupFileName, FTestExeSetupDir)
end;

procedure TExternalTest.VerifyFinal;
begin
  UnzipFinal;
  CompareActualToFinal;
end;

initialization
  LoadTests;

end.

