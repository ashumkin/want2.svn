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
Major author: Chris Morris
Contributors: 
}
unit DanteExternalTest;

interface

uses
  TestFramework;

type
  TExternalTest = class(TTestCase)
  private
    FRootName: string;
    FRootTestDataDir: string;
    FRootTestExeDir: string;
    FTestExeFinalDir: string;
    FTestExeSetupDir: string;
    procedure SetRootName(const Value: string);
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

    property TestRootName: string read FRootName write SetRootName;
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
  WildPaths;

procedure LoadTests;
var
  Files: TStringList;
  i: Integer;
  ATest: TExternalTest;
begin
  Files := TStringList.Create;
  try
    JclFileUtils.BuildFileList('..\test\data\*.xml', 0, Files);
    for i := 0 to Files.Count - 1 do
    begin
      ATest := TExternalTest.Create;
      ATest.TestRootName := JclFileUtils.PathExtractFileNameNoExt(Files[i]);
      RegisterTest('Acceptance Suite.External Tests', ATest);
    end;
  finally
    Files.Free;
  end;
end;

{ TExternalTest }

function TExternalTest.BuildFileName: string;
begin
  Result := FRootName + '.xml';
end;

procedure TExternalTest.CompareActualToFinal;
var
  SetupList: TStringList;
  FinalList: TStringList;

  procedure RemoveBaseFromFullNames(Strings: TStringList; Base: string);
  var
    i: Integer;
  begin
    for i := 0 to Strings.Count - 1 do
      Strings[i] := StringReplace(Strings[i], Base, '', [rfIgnoreCase]);
  end;
begin
  { this would be a good start for a contribution to DUnit }
  SetupList := TStringList.Create;
  FinalList := TStringList.Create;
  try
    (* these calls hang under my setup -- Juanco
    JclFileUtils.AdvBuildFileList(FTestExeSetupDir + '*.*', faAnyFile,
      SetupList, [flRecursive, flFullNames]);
    JclFileUtils.AdvBuildFileList(FTestExeFinalDir + '*.*', faAnyFile,
      FinalList, [flRecursive, flFullNames]);
    *)

    WildPaths.Wild(SetupList, '**', ToPath(FTestExeSetupDir));
    WildPaths.Wild(FinalList, '**', ToPath(FTestExeFinalDir));
    ToSystemPaths(SetupList, ToPath(FTestExeSetupDir));
    ToSystemPaths(FinalList, ToPath(FTestExeFinalDir));

    { need flFullNames to make sure directory structure is also checked, but
      we need to chop off the different bases }
    RemoveBaseFromFullNames(SetupList, FTestExeSetupDir);
    RemoveBaseFromFullNames(FinalList, FTestExeFinalDir);

    Check(SetupList.Equals(FinalList),
      'setup file structure does not match final structure: ' + #13#10 +
      'SetupFiles: ' + SetupList.CommaText + #13#10 +
      'FinalFiles: ' + FinalList.CommaText);
  finally
    SetupList.Free;
    FinalList.Free;
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
    Dante.DoBuild(FTestExeSetupDir + BuildFileName);
  finally
    Dante.Free;
  end;
  VerifyFinal;
end;

function TExternalTest.FinalFileName: string;
begin
  Result := FRootName + '.final.zip';
end;

procedure TExternalTest.SetRootName(const Value: string);
begin
  FRootName := Value;
  FTestName := FRootName;
end;

procedure TExternalTest.Setup;
begin
  inherited;
  UnzipSetup;
end;

function TExternalTest.SetupFileName: string;
begin
  Result := FRootName + '.setup.zip';
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
    Windows.CopyFile(
      PChar(FRootTestDataDir + FileName),
      PChar(Directory + FileName), false);
  end;
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  JclFileUtils.ForceDirectories(Directory);
  DoCopy(BuildFileName);
  DoCopy(ZipFileName);
  CurrentDir := GetCurrentDir;
  ChDir(Directory);
  WinExec32AndWait('..\..\..\test\data\unzip.exe ' + ZipFileName, 0);
  DeleteFile(ZipFileName);
  ChDir(CurrentDir);
end;

procedure TExternalTest.UnzipFinal;
begin
  Unzip(FRootName + '.final.zip', FTestExeFinalDir);
end;

procedure TExternalTest.UnzipSetup;
begin
  Unzip(FRootName + '.setup.zip', FTestExeSetupDir);
end;

procedure TExternalTest.VerifyFinal;
begin
  UnzipFinal;
  CompareActualToFinal;
end;

initialization
  LoadTests;

end.

