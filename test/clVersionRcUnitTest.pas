{ $Id$ }
{
--------------------------------------------------------------------------
Copyright (c) 2000, Chris Morris 
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The names Chris Morris, cLabs and the names of contributors to this 
software may be used to endorse or promote products derived from this software 
without specific prior written permission. 

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
(based on BSD Open Source License)
}
unit clVersionRcUnitTest;

interface

uses clVersionRcUnit, TestFramework, SysUtils, JclFileUtils, JclShell;

type
  TTestRcUnit = class(TTestCase)
  protected
    FclVersionRc: TclVersionRc;
    FDir: string;
    FTestRcName: string;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestRcUnit;
  end;

implementation

{ TTestRcUnit }

procedure TTestRcUnit.Setup;
var
  F: TextFile;
begin
  inherited;
  FDir := ExtractFilePath(ParamStr(0)) + 'test\';
  JclFileUtils.ForceDirectories(FDir);
  FTestRcName := FDir + 'test.rc';
  AssignFile(F, FTestRcName);
  Rewrite(F);
  try
    WriteLn(F, 'VS_VERSION_INFO VERSIONINFO');
    WriteLn(F, 'FILEVERSION 1,0,0,52');
    WriteLn(F, 'PRODUCTVERSION 1,0,0,52');
    WriteLn(F, 'FILEFLAGSMASK 0x3fL');
    WriteLn(F, 'FILEFLAGS 0x0L');
    WriteLn(F, 'FILEOS 0x4L');
    WriteLn(F, 'FILETYPE 0x1L');
    WriteLn(F, 'FILESUBTYPE 0x0L');
    WriteLn(F, 'BEGIN');
    WriteLn(F, '        BLOCK "StringFileInfo"');
    WriteLn(F, '        BEGIN');
    WriteLn(F, '                BLOCK "080904B0"');
    WriteLn(F, '                BEGIN');
    WriteLn(F, '                VALUE "CompanyName", "cLabs\0"');
    WriteLn(F, '                VALUE "FileDescription", "SampleApp\0"');
    WriteLn(F, '                VALUE "FileVersion", "1.0.0.52\0"');
    WriteLn(F, '                VALUE "InternalName", "SampleApp\0"');
    WriteLn(F, '                VALUE "LegalCopyright", "Copyright \251 1999-2000 cLabs\0"');
    WriteLn(F, '                VALUE "LegalTrademarks", "SampleApp\0"');
    WriteLn(F, '                VALUE "OriginalFileName", "SampleApp.exe\0"');
    WriteLn(F, '                VALUE "ProductName", "SampleApp\0"');
    WriteLn(F, '                VALUE "ProductVersion","1.0.0.52\0"');
    WriteLn(F, '                END');
    WriteLn(F, '        END');
    WriteLn(F, '        BLOCK "VarFileInfo"');
    WriteLn(F, '        BEGIN');
    WriteLn(F, '                VALUE "Translation", 0x809,1200');
    WriteLn(F, '        END');
    WriteLn(F, 'END');
  finally
    CloseFile(F);
  end;
  FclVersionRc := TclVersionRc.Create(FTestRcName);
end;

procedure TTestRcUnit.TearDown;
begin
  FclVersionRc.Free;
  JclShell.SHDeleteFolder(0, FDir, [doSilent, doAllowUndo]);
  inherited;
end;

procedure TTestRcUnit.TestRcUnit;
begin
  Check(FclVersionRc.VersionInfo.Build = 52);
  FclVersionRc.IncBuild;
  Check(FclVersionRc.VersionInfo.Build = 53);
end;

initialization
  RegisterTest('', TTestRcUnit);

end.

