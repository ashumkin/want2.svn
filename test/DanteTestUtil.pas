unit DanteTestUtil;

interface

uses TestFramework, JclFileUtils, JclShell, SysUtils;

type
  TTestDirCase = class(TTestCase)
  protected
    FTestDir: string;

    function MakeSampleTextFile: string;
  public
    procedure Setup; override;
    procedure TearDown; override;
  end;

implementation

{ TTestDirCase }

function TTestDirCase.MakeSampleTextFile: string;
var
  F: TextFile;
begin
  Result := FTestDir + '\sample.txt';
  AssignFile(F, Result);
  Rewrite(F);
  WriteLn(F, 'this is a sample file');
  CloseFile(F); 
end;

procedure TTestDirCase.Setup;
begin
  inherited;
  FTestDir := ExtractFilePath(ParamStr(0)) + 'test';
  JclFileUtils.ForceDirectories(FTestDir);
end;

procedure TTestDirCase.TearDown;
begin
  JclShell.SHDeleteFolder(0, FTestDir, [doSilent]);
  inherited;
end;

end.

