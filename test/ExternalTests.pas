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
Original author: Chris Morris
Contributors   : Juancarlo Añez 
}
unit DanteExternalTest;

interface

uses
  TestFramework, clUtilFile;

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
  Windows,
  SysUtils,
  DanteMain,
  JclMiscel,
  JclShell,
  WildPaths,
  DanteClasses;

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
begin
  ADirComp := TclDirectoryCompare.Create(FTestExeSetupDir, FTestExeFinalDir);
  try
     Check(ADirComp.CompareByFileContent);
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
  Dante: TDante;
begin
  Dante := TDante.Create;
  try
    Dante.DoBuild(FTestExeSetupDir + BuildFileName, vlVerbose);
  finally
    Dante.Free;
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
var
  CurrentDir: string;

  procedure DoCopy(FileName: string);
  begin
    if not Windows.CopyFile(
      PChar(FRootTestDataDir + FTestPath + FileName),
      PChar(Directory + FileName), false) then
      RaiseLastWin32Error;
  end;
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  JclFileUtils.ForceDirectories(Directory);
  DoCopy(BuildFileName);
  DoCopy(ZipFileName);
  CurrentDir := GetCurrentDir;
  ChDir(Directory);
  WinExec32AndWait(
    '..\..\..\test\data\unzip.exe ' + ZipFileName, 0);
  SysUtils.DeleteFile(ZipFileName);
  ChDir(CurrentDir);
end;

procedure TExternalTest.UnzipFinal;
begin
  Unzip(FinalFileName, FTestExeFinalDir);
end;

procedure TExternalTest.UnzipSetup;
begin
  Unzip(SetupFileName, FTestExeSetupDir);
end;

procedure TExternalTest.VerifyFinal;
begin
  UnzipFinal;
  CompareActualToFinal;
end;

initialization
  LoadTests;

end.

