unit clUtilFile;
{ $Id$ }
{
--------------------------------------------------------------------------
Copyright (c) 2001 Chris Morris
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

interface

uses
  SysUtils, JclFileUtils;

type
  TclVersionInfo = class(TObject)
  private
    FRelease: integer;
    FMajor: integer;
    FBuild: integer;
    FMinor: integer;
    FFileName: string;
    function GetVersionString: string;
    procedure SetFileName(const Value: string);
    procedure SetVersionString(const Value: string);
  public
    property Build: integer read FBuild write FBuild;
    property Major: integer read FMajor write FMajor;
    property Minor: integer read FMinor write FMinor;
    property Release: integer read FRelease write FRelease;
    property VersionString: string read GetVersionString write SetVersionString;

    property FileName: string read FFileName write SetFileName;
  end;

implementation

uses Classes;

function TclVersionInfo.GetVersionString: string;
begin
  Result := IntToStr(FMajor) + '.' +
            IntToStr(FMinor) + '.' +
            IntToStr(FRelease) + '.' +
            IntToStr(FBuild);
end;

procedure TclVersionInfo.SetFileName(const Value: string);
var
  AVersInfo: TJclFileVersionInfo;
  Version: string;
  ASL: TStringList;
begin
  FFileName := Value;
  AVersInfo := TJclFileVersionInfo.Create(FFileName);
  ASL := TStringList.Create;
  try
    Version := AVersInfo.FileVersion;
    ASL.CommaText := StringReplace(Version, '.', ',', [rfReplaceAll]);
    FMajor := StrToInt(ASL[0]);
    FMinor := StrToInt(ASL[1]);
    FRelease := StrToInt(ASL[2]);
    FBuild := StrToInt(ASL[3]);
  finally
    AVersInfo.Free;
    ASL.Free;
  end;
end;

procedure TclVersionInfo.SetVersionString(const Value: string);
var
  SL: TStringList;
  ValueComma: string;
begin
  SL := TStringList.Create;
  try
    ValueComma := StringReplace(Value, '.', ',', [rfReplaceAll, rfIgnoreCase]);
    SL.CommaText := ValueComma;
    FMajor := StrToInt(SL[0]);
    FMinor := StrToInt(SL[1]);
    FRelease := StrToInt(SL[2]);
    FBuild := StrToInt(SL[3]);
  finally
    SL.Free;
  end;
end;

end.

