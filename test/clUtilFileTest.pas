{ $Id$ }
unit clUtilFileTest;

interface                          

uses
  SysUtils, JclFileUtils, TestFramework, clUtilDelphi, clUtilFile;

type
  TTestVersionInfo = class(TTestCase)
  private
    FVersionInfo: TclVersionInfo;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestVersionInfo;
  end;

  TCharArray = array of Char;
  TTestDirCompare = class(TTestCase)
  private
    FDirComp: TclDirectoryCompare;
    procedure GenerateRandomBinaryData(var Contents: TCharArray);
    procedure WriteSampleFile(Dir, FileName: string; Contents: TCharArray); overload;
    procedure WriteSampleFile(Dir: string; FileName: string = 'test.txt';
      Contents: string = 'contents'; RepeatContents: Integer = 1); overload;
  protected
    function DirA: string;
    function DirB: string;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestDirCompareByFileName;
    procedure TestDirCompareByFileContents;
    procedure TestDirCompareByLargeFileContents;
    procedure TestDirCompareByBinFileContents;
    procedure TestDirCompareWithSubDirs;
  end;

implementation

{ TTestVersionInfo }

procedure TTestVersionInfo.Setup;
begin
  inherited;
  FVersionInfo := TclVersionInfo.Create;
end;

procedure TTestVersionInfo.TearDown;
begin
  FVersionInfo.Free;
  inherited;
end;

procedure TTestVersionInfo.TestVersionInfo;
var
  ADelReg: TDelphiRegistry;
begin
  ADelReg := TDelphiRegistry.Create;
  try
    {$IFDEF VER130}
      FVersionInfo.FileName := ADelReg.GetRootDir(dvDelphi5) + '\bin\delphi32.exe';
      CheckEquals(5, FVersionInfo.Major  );
      CheckEquals(0, FVersionInfo.Minor  );
      CheckEquals(6, FVersionInfo.Release);
      CheckEquals(18, FVersionInfo.Build  );

      { this doesn't work -- dunno why. Explorer picks it up fine, must be
        a small difference and a bug in VersInfo

        read something on the ngs that it's a problem in dcc32.exe
      FVersionInfo.FileName := ADelReg.GetRootDir(dvDelphi5) + '\bin\dcc32.exe';
      CheckEquals(5 , FVersionInfo.Major  );
      CheckEquals(0 , FVersionInfo.Minor  );
      CheckEquals(5 , FVersionInfo.Release);
      CheckEquals(62, FVersionInfo.Build  );     }
    {$ENDIF}

    {$IFDEF VER120}
      FVersionInfo.FileName := ADelReg.GetRootDir(dvDelphi4) + '\bin\delphi32.exe';
      CheckEquals(4, FVersionInfo.Major  );
      CheckEquals(0, FVersionInfo.Minor  );
      CheckEquals(5, FVersionInfo.Release);
      CheckEquals(108, FVersionInfo.Build  );

      { this doesn't work -- dunno why. Explorer picks it up fine, must be
        a small difference and a bug in VersInfo

        read something on the ngs that it's a problem in dcc32.exe
      FVersionInfo.FileName := ADelReg.GetRootDir(dvDelphi4) + '\bin\dcc32.exe';
      CheckEquals(4, FVersionInfo.Major  );
      CheckEquals(0, FVersionInfo.Minor  );
      CheckEquals(0, FVersionInfo.Release);
      CheckEquals(0, FVersionInfo.Build  );       }
    {$ENDIF}
  finally
    ADelReg.Free;
  end;
end;

{ TTestDirCompare }

{ TearDown needs to delete these dirs. By making these functions, it ensures
  TearDown will get the correct directory path and not rely on Setup's
  assignment to a private field to still be accurate. I did that once and had
  a TearDown run when Setup failed, and did a DelTree from root. D'oh!
  Fortunately, the 'root' was a subst drive and damage was limited. }
function TTestDirCompare.DirA: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'dira\';
end;

function TTestDirCompare.DirB: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'dirb\';
end;

procedure TTestDirCompare.GenerateRandomBinaryData(
  var Contents: TCharArray);
var
  i: Integer;
begin
  for i := Low(Contents) to High(Contents) do
    Contents[i] := Chr(Random(128));
end;

procedure TTestDirCompare.Setup;
begin
  inherited;
  ForceDirectories(DirA);
  ForceDirectories(DirB);
  FDirComp := TclDirectoryCompare.Create(DirA, DirB);
end;

procedure TTestDirCompare.TearDown;
  procedure DeleteDir(Dir: string);
  begin
    if DirectoryExists(Dir) and (Dir <> '') then
      DelTree(Dir);
  end;
begin
  FDirComp.Free;
  DeleteDir(DirA);
  DeleteDir(DirB);
  inherited;
end;

procedure TTestDirCompare.TestDirCompareByBinFileContents;
var
  Contents: TCharArray;
begin
  SetLength(Contents, 20000);
  try
    GenerateRandomBinaryData(Contents);
    WriteSampleFile(DirA, 'z.dat', Contents);
    WriteSampleFile(DirB, 'z.dat', Contents);
    Check(FDirComp.CompareByFileContent);
    WriteSampleFile(DirA, 'zz.dat', Contents);
    Contents[15879] := Chr(Ord(Contents[15879]) + 1);
    WriteSampleFile(DirB, 'zz.dat', Contents);
    Check(not FDirComp.CompareByFileContent);
  finally
    Contents := nil;
  end;
end;

procedure TTestDirCompare.TestDirCompareByFileContents;
begin
  WriteSampleFile(DirA);
  WriteSampleFile(DirB);
  Check(FDirComp.CompareByFileContent);
  WriteSampleFile(DirA, 'z.txt', 'content');
  WriteSampleFile(DirB, 'z.txt', 'context');
  Check(not FDirComp.CompareByFileContent);
end;

procedure TTestDirCompare.TestDirCompareByFileName;
begin
  WriteSampleFile(DirA);
  WriteSampleFile(DirB);
  Check(FDirComp.CompareByFileName);
  WriteSampleFile(DirA, 'testb.txt');
  Check(not FDirComp.CompareByFileName);
end;

procedure TTestDirCompare.TestDirCompareByLargeFileContents;
begin
  WriteSampleFile(DirA, 'z.txt', 'this is a test ', 3000);
  WriteSampleFile(DirB, 'z.txt', 'this is a test ', 3000);
  Check(FDirComp.CompareByFileContent);
end;

procedure TTestDirCompare.TestDirCompareWithSubDirs;
begin
  ForceDirectories(DirA + 'sub');
  ForceDirectories(DirB + 'sub');
  WriteSampleFile(DirA + 'sub\', 'z.txt');
  WriteSampleFile(DirB + 'sub\', 'z.txt');
  Check(FDirComp.CompareByFileContent);
  WriteSampleFile(DirA + 'sub\', 'zz.txt', 'blargh');
  Check(not FDirComp.CompareByFileContent);
  WriteSampleFile(DirB + 'sub\', 'zz.txt', 'blergh');
  Check(not FDirComp.CompareByFileContent);
end;

procedure TTestDirCompare.WriteSampleFile(Dir, FileName, Contents: string;
  RepeatContents: Integer);
var
  Buf: TCharArray;
  i: Integer;
  FullContent: string;
begin
  for i := 0 to RepeatContents - 1 do
    FullContent := FullContent + Contents;
    
  SetLength(Buf, Length(FullContent) + 1);
  try
    StrPCopy(@Buf[0], FullContent);
    WriteSampleFile(Dir, FileName, Buf);
  finally
    Buf := nil;
  end;
end;

procedure TTestDirCompare.WriteSampleFile(Dir, FileName: string;
  Contents: TCharArray);
const
  PageSize: Integer = 2048;
var
  F: file;
  Buf: PChar;
  Pages: Integer;
  Page: Integer;
  AmtWritten: Integer;
  ThisPageSize: Integer;

  function LastPage: boolean;
  begin
    Result := (Page = Pages);
  end;
begin
  AssignFile(F, Dir + FileName);
  Rewrite(F, 1);
  try
    Pages := (High(Contents) div PageSize) + 1;
    Buf := @Contents[Low(Contents)];
    for Page := 1 to Pages do
    begin
      if LastPage then
        ThisPageSize := High(Contents) mod PageSize
      else
        ThisPageSize := PageSize;

      BlockWrite(F, Buf^, ThisPageSize, AmtWritten);
      if AmtWritten <> ThisPageSize then
        raise Exception.Create('Internal error: bad page size');

      Inc(Buf, ThisPageSize);
    end;
  finally
    CloseFile(F);
  end;
end;

initialization
  RegisterTest('clUtilFile', TTestVersionInfo);
  RegisterTest('clUtilFile', TTestDirCompare);

end.


