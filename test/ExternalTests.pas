(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo A�ez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit ExternalTests;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Math,

  Dialogs,

  JclSysUtils,
  JclFileUtils,
  JclMiscel,
  JclShell,

  WildPaths,
  WantClasses,
  ScriptRunner,
  ZipStreams,

  TestFramework;


type
  TExternalTest = class(TTestCase)
  private
    FTestPath: TPath;
    FRootPath: TPath;

    procedure SetTestName;
    procedure SetTestPath(const Value: TPath);
  protected
    function BuildFileName: string;
    class function CompareFiles(AFileName, BFileName: string): boolean;
    procedure CompareActualToFinal;
    procedure DeleteSubFolders;
    function FinalFileName: string;
    function SetupFileName: string;
    procedure Unzip(ZipFileName, Directory: TPath);
    procedure UnzipFinal;
    procedure UnzipSetup;
    procedure VerifyFinal;

    function SetupPath :TPath;
    function FinalPath :TPath;
  public
    constructor Create; reintroduce; overload;
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure DoTest;

    property TestPath: TPath read FTestPath write SetTestPath;
  end;

implementation


procedure LoadTests;
var
  Files: TPaths;
  i: Integer;
  ATest: TExternalTest;
  BasePath: string;
begin
  Files := nil;
  try
    BasePath := PathConcat(CurrentDir, '../test/data');
    Files := WildPaths.Wild('**/*.xml', BasePath);
    for i := 0 to High(Files) do
    begin
      ATest := TExternalTest.Create;
      ATest.TestPath := SuperPath(Files[i]);
      RegisterTest('External Tests/' + ToRelativePath(ATest.TestPath, BasePath), ATest);
    end;
  except
    on e :Exception do
      ShowMessage('Error loading external tests: ' + e. Message);
  end;
end;

{ TExternalTest }

function TExternalTest.BuildFileName: string;
begin
  Result := 'build.xml';
end;

function TExternalTest.SetupPath: TPath;
begin
  Result := PathConcat(FRootPath, 'setup');
end;

function TExternalTest.FinalPath: TPath;
begin
  Result := PathConcat(FRootPath, 'final');
end;

procedure TExternalTest.CompareActualToFinal;
var
  SetupFiles :TStrings;
  FinalFiles :TStrings;
  SF,
  FF         :TPath;
  p :Integer;
begin
  SetupFiles := TStringList.Create;
  FinalFiles := TStringList.Create;
  try
     Wild(SetupFiles, '**', SetupPath);
     Wild(FinalFiles, '**', FinalPath);

     ToRelativePaths(SetupFiles, SetupPath);
     ToRelativePaths(FinalFiles, FinalPath);

     for p := 0 to SetupFiles.Count-1 do
       if Pos('CVS\', SetupFiles[p]) = 0 then
         Check(FinalFiles.IndexOf(SetupFiles[p]) >= 0, Format('%s in setup but not in final', [SetupFiles[p]]));

     for p := 0 to FinalFiles.Count-1 do
       if Pos('CVS\', FinalFiles[p]) = 0 then
         Check(SetupFiles.IndexOf(FinalFiles[p]) >= 0, Format('%s in final but not in setup', [FinalFiles[p]]));

     for p := 0 to Min(SetupFiles.Count, FinalFiles.Count)-1 do
     begin
       if Pos('CVS', SetupFiles[p]) = 0 then
       begin
         SF := PathConcat(SetupPath, SetupFiles[p]);
         FF := PathConcat(FinalPath, FinalFiles[p]);

         CheckEquals(   IsDirectory(SF),
                        IsDirectory(FF),
                        Format('%s files not both directories', [SetupFiles[p]]));;

         if not PathIsDir(SF) then
           Check(CompareFiles(SF, FF), Format('%s files are different', [SetupFiles[p]]));;
         end;
     end;
  finally
    SetupFiles.Free;
    FinalFiles.Free;
  end;
end;

class function TExternalTest.CompareFiles(AFileName, BFileName: string): boolean;
var
  A: TFileStream;
  B: TFileStream;
  ARead: Integer;
  BRead: Integer;
  ABuf: array[1..2048] of Char;
  BBuf: array[1..2048] of Char;
begin
  { read-only, required for read-only files, and all we need here anyway }
  FileMode := 0;
  A := TFileStream.Create(ToSystemPath(AFileName), fmOpenRead);
  try
    B := TFileStream.Create(ToSystemPath(BFileName), fmOpenRead);
    try
      repeat
        FillChar(ABuf, SizeOf(ABuf), #0);
        FillChar(BBuf, SizeOf(BBuf), #0);

        ARead := A.Read(ABuf, SizeOf(ABuf));
        BRead := B.Read(BBuf, SizeOf(BBuf));

        if ARead = BRead then
          Result := (ABuf = BBuf)
        else
          Result := False;
      until (not Result) or (ARead <> SizeOf(ABuf));
    finally
      FreeAndNil(B);
    end;
  finally
    FreeAndNil(A);
  end;
end;

constructor TExternalTest.Create;
begin
  Create('DoTest');

  FRootPath := PathConcat(ToPath(ExtractFilePath(ParamStr(0))) ,'test');
end;

procedure TExternalTest.DeleteSubFolders;
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  { make sure we haven't got off on the root dir or something heinous }
  if  PathIsDir(SetupPath)
  and  PathIsDir(FinalPath)
  and  PathIsFile(PathConcat(SetupPath, BuildFileName))
  then
    DeleteFiles('**', FRootPath);
end;

procedure TExternalTest.DoTest;
var
  Runner: TScriptRunner;
begin
  {$IFNDEF USE_TEXT_RUNNER}
  Runner := TScriptRunner.Create;
  {$ELSE}
  Runner := TConsoleScriptRunner.Create;
  {$ENDIF}
  try
    Runner.Build(PathConcat(SetupPath, BuildFileName), vlVerbose);
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
begin
  FTestName := ToRelativePath(FTestPath, SuperPath(FTestPath));
end;

procedure TExternalTest.SetTestPath(const Value: TPath);
begin
  FTestPath := Value;
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

procedure TExternalTest.Unzip(ZipFileName, Directory: TPath);

  procedure DoCopy(FileName: TPath);
  begin
    CopyFiles(FileName, FTestPath, Directory);
  end;
var
  ZipLocation  :TPath;
  DirLocation  :TPath;
begin
  ChangeDir(FRootPath);

  MakeDir(Directory);

  DoCopy(BuildFileName);

  ZipLocation := PathConcat(FTestPath, ZipFileName);
  if PathIsFile(ZipLocation) then
    ZipStreams.ExtractAll(ZipLocation, Directory)
  else
  begin
    DirLocation := ChangeFileExt(ZipLocation, '');
    if PathIsDir(DirLocation) then
      CopyFiles('**', DirLocation, Directory)
    else
      Fail('Could not find ' + ZipLocation);
  end;
end;

procedure TExternalTest.UnzipFinal;
begin
  Unzip(FinalFileName, FinalPath);
end;

procedure TExternalTest.UnzipSetup;
begin
  Unzip(SetupFileName, SetupPath)
end;

procedure TExternalTest.VerifyFinal;
begin
  UnzipFinal;
  CompareActualToFinal;
end;

initialization
  LoadTests;

end.

