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
Original Author: Chris Morris
Contributors   :
}

interface

uses
  SysUtils, Classes, JclFileUtils, WildPaths;

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

  TclFileCompare = class(TObject)
  public
    function CompareFiles(AFileName, BFileName: string): boolean;
  end;

  TclDirectoryCompare = class(TObject)
  private
    FAFiles: TStringList;
    FBFiles: TStringList;
    FDirA: string;
    FDirB: string;
  protected
    procedure GetFiles;
  public
    constructor Create(DirectoryA, DirectoryB: string);
    destructor Destroy; override;

    function CompareByFileName: boolean;
    function CompareByFileContent: boolean;
  end;

implementation

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


function CompareFilesContent(AFiles, BFiles: TStrings): boolean;
var
  i: Integer;
begin
  Result := AFiles.Equals(BFiles);
  for i := 0 to AFiles.Count - 1 do
  begin

  end;
end;

{ TclDirectoryCompare }

function TclDirectoryCompare.CompareByFileContent: boolean;
var
  i: Integer;
  AFileComp: TclFileCompare;
begin
  Result := CompareByFileName;
  if Result then
  begin
    AFileComp := TclFileCompare.Create;
    try
      { this isn't a good performance option, because it re-reads the disk.
        The need is because CompareByFileName lops off the base directory
        names. This could be easily optimized to read from disk once and pass
        a copy of the StringLists to CompareByFileName }
      GetFiles;
      for i := 0 to FAFiles.Count - 1 do
      begin
        if not IsDirectory(FAFiles[i]) then
          Result := AFileComp.CompareFiles(FAFiles[i], FBFiles[i]);
        if not Result then break;
      end;
    finally
      AFileComp.Free;
    end;
  end;
end;

function TclDirectoryCompare.CompareByFileName: boolean;
  procedure RemoveBaseDir(Strings: TStringList; Base: string);
  var
    i: Integer;
  begin
    for i := 0 to Strings.Count - 1 do
      Strings[i] := StringReplace(Strings[i], Base, '', [rfIgnoreCase]);
  end;
begin
  GetFiles;

  { we need to chop off the different base names }
  RemoveBaseDir(FAFiles, FDirA);
  RemoveBaseDir(FBFiles, FDirB);

  Result := FAFiles.Equals(FBFiles);
end;

constructor TclDirectoryCompare.Create(DirectoryA, DirectoryB: string);
begin
  FDirA := DirectoryA;
  FDirB := DirectoryB;
  FAFiles := TStringList.Create;
  FBFiles := TStringList.Create;
end;

destructor TclDirectoryCompare.Destroy;
begin
  FAFiles.Free;
  FBFiles.Free;
  inherited;
end;

procedure TclDirectoryCompare.GetFiles;
begin
  FAFiles.Clear;
  FBFiles.Clear;

  FAFiles.Sorted := True;
  FBFiles.Sorted := True;
  WildPaths.Wild(FAFiles, '**', ToPath(FDirA));
  WildPaths.Wild(FBFiles, '**', ToPath(FDirB));

  FAFiles.Sorted := False;
  FBFiles.Sorted := False;
  ToSystemPaths(FAFiles, ToPath(FDirA));
  ToSystemPaths(FBFiles, ToPath(FDirB));
end;

{ TclFileCompare }

function TclFileCompare.CompareFiles(AFileName,
  BFileName: string): boolean;
var
  A: file;
  B: file;
  ARead: Integer;
  BRead: Integer;
  ABuf: array[1..2048] of Char;
  BBuf: array[1..2048] of Char;
begin
  { read-only, required for read-only files, and all we need here anyway }
  FileMode := 0;
  AssignFile(A, AFileName);
  AssignFile(B, BFileName);
  Reset(A, 1);
  Reset(B, 1);
  try
    repeat
      FillChar(ABuf, SizeOf(ABuf), #0);
      FillChar(BBuf, SizeOf(BBuf), #0);
      BlockRead(A, ABuf, SizeOf(ABuf), ARead);
      BlockRead(B, BBuf, SizeOf(BBuf), BRead);
      Result := (ARead = BRead);
      if Result then
        Result := (ABuf = BBuf);
    until (not Result) or (ARead <> SizeOf(ABuf));
  finally
    CloseFile(A);
    CloseFile(B);
  end;
end;

end.

