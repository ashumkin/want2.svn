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
unit clVersionRcUnit;

interface

uses SysUtils, Classes, clUtilFile, JclFileUtils;

type
  TclVersionRc = class(TObject)
  private
    FRcFileName: string;
    FVersionInfo: TclVersionInfo;
    procedure InitVersionInfo;
    procedure SetVersionInfo(const Value: TclVersionInfo);
    procedure SetRcFileName(const Value: string);
  protected
    function ReadVersionInfo: boolean;
  public
    constructor Create; overload;
    constructor Create(ARcFileName: string); overload;
    destructor Destroy; override;
    function IncBuild: boolean;

    property RcFileName: string read FRcFileName write SetRcFileName;
    property VersionInfo: TclVersionInfo read FVersionInfo write SetVersionInfo;
  end;

const
  FILE_VERSION_TOKEN = 'FILEVERSION';
  PRODUCT_VERSION_TOKEN = 'PRODUCTVERSION';

implementation

{ TclVersionRc }

constructor TclVersionRc.Create(ARcFileName: string);
begin
  Create;
  RcFileName := ARcFileName;
end;

constructor TclVersionRc.Create;
begin
  FVersionInfo := TclVersionInfo.Create;
end;

destructor TclVersionRc.Destroy;
begin
  FVersionInfo.Free;
  inherited;
end;

function TclVersionRc.IncBuild: boolean;
var
  InFile: TextFile;
  OutFile: TextFile;
  OutFileName: string;
  InFileLn: string;
  OutFileLn: string;
  OldBuildStrA: string;
  OldBuildStrB: string;
  NewBuildStrA: string;
  NewBuildStrB: string;
begin
  OutFileName := FRcFileName + '.tmp';
  if FileExists(OutFileName) then
    DeleteFile(OutFileName);

  if ReadVersionInfo then
  begin
    OldBuildStrA := IntToStr(FVersionInfo.Major) + ',' +
                    IntToStr(FVersionInfo.Minor) + ',' +
                    IntToStr(FVersionInfo.Release) + ',' +
                    IntToStr(FVersionInfo.Build);
    OldBuildStrB := IntToStr(FVersionInfo.Major) + '.' +
                    IntToStr(FVersionInfo.Minor) + '.' +
                    IntToStr(FVersionInfo.Release) + '.' +
                    IntToStr(FVersionInfo.Build);

    FVersionInfo.Build := FVersionInfo.Build + 1;

    NewBuildStrA := IntToStr(FVersionInfo.Major) + ',' +
                    IntToStr(FVersionInfo.Minor) + ',' +
                    IntToStr(FVersionInfo.Release) + ',' +
                    IntToStr(FVersionInfo.Build);
    NewBuildStrB := IntToStr(FVersionInfo.Major) + '.' +
                    IntToStr(FVersionInfo.Minor) + '.' +
                    IntToStr(FVersionInfo.Release) + '.' +
                    IntToStr(FVersionInfo.Build);

    AssignFile(InFile, FRcFileName);
    Reset(InFile);

    AssignFile(OutFile, OutFileName);
    Rewrite(OutFile);

    while not Eof(InFile) do
    begin
      ReadLn(InFile, InFileLn);
      OutFileLn := StringReplace(InFileLn, OldBuildStrA, NewBuildStrA,
        [rfReplaceAll, rfIgnoreCase]);
      OutFileLn := StringReplace(OutFileLn, OldBuildStrB, NewBuildStrB,
        [rfReplaceAll, rfIgnoreCase]);
      WriteLn(OutFile, OutFileLn);
    end;

    CloseFile(InFile);
    CloseFile(OutFile);

    DeleteFile(FRcFileName);
    RenameFile(OutFileName, FRcFileName);
    Result := true;
  end
  else begin
    DeleteFile(OutFileName);
    Result := false;
  end;
end;

procedure TclVersionRc.InitVersionInfo;
begin
  FVersionInfo.Major := 0;
  FVersionInfo.Minor := 0;
  FVersionInfo.Release := 0;
  FVersionInfo.Build := 0;
end;

function TclVersionRc.ReadVersionInfo: boolean;
var
  InFileStream: TStream;
  AParser: TParser;
  IntVerInc: integer;
  TokenType: Char;
  IntVer: array[0..3] of integer;
begin
  InFileStream := TFileStream.Create(FRcFileName,
    fmOpenRead or fmShareExclusive);
  InFileStream.Seek(0, soFromBeginning);
  AParser := TParser.Create(InFileStream);
  Result := false;
  try
    while true do
    begin
      case AParser.NextToken of
      toEOF:     break;
      toSymbol:
        begin
          if (UpperCase(AParser.TokenString) = FILE_VERSION_TOKEN) or
            (UpperCase(AParser.TokenString) = PRODUCT_VERSION_TOKEN) then
          begin
            Result := true;
            for IntVerInc := 0 to 3 do
            begin
              repeat
                TokenType := AParser.NextToken;
              until (TokenType <= #5);
              if TokenType <> toInteger then
              begin
                Result := false;
                break;
              end;
              IntVer[IntVerInc] := AParser.TokenInt;
            end;
            break;
          end;
        end;
      end;
    end;
  finally
    AParser.Free;
    InFileStream.Free;
  end;

  if Result then
  begin
    FVersionInfo.Major   := IntVer[0];
    FVersionInfo.Minor   := IntVer[1];
    FVersionInfo.Release := IntVer[2];
    FVersionInfo.Build   := IntVer[3];
  end;
end;

procedure TclVersionRc.SetRcFileName(const Value: string);
begin
  FRcFileName := Value;
  InitVersionInfo;
  ReadVersionInfo;
end;

procedure TclVersionRc.SetVersionInfo(const Value: TclVersionInfo);
begin
  FVersionInfo := Value;
end;

end.

