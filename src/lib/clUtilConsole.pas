unit clUtilConsole;
{
--------------------------------------------------------------------------
Copyright (c) 2001, Chris Morris
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. Neither the names Chris Morris, cLabs nor the names of contributors to this
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

uses SysUtils, Windows, Classes;

function GetCmdLineSwitch(const Switch: string; SwitchChars: TSysCharSet;
  IgnoreCase: Boolean): string;

type
  TMorePage = record
    StartLine: Integer;
    EndLine: Integer;
    Content: string;
    EmptyPage: boolean;
  end;

  TConsoleMore = class(TObject)
  private
    FLines: TStringList;
    FText: string;
    FPageNo: Integer;
    function GetCurrentPage: TMorePage;
    function GetCurrentPageContent: string;
    function GetFinished: boolean;
  protected
    procedure FillLines;
    function GetPage(PageNo: Integer): TMorePage;
  public
    constructor Create(Text: string);

    procedure NextPage;

    property CurrentPage: string read GetCurrentPageContent;
    property Finished: boolean read GetFinished;
  end;

implementation

uses JclStrings;

{:Modified version of FindCmdLineSwitch }
// refactor: remove magic number 2
function GetCmdLineSwitch(const Switch: string; SwitchChars: TSysCharSet;
  IgnoreCase: Boolean): string;
var
  I: Integer;
  S: string;

  function SetSwitchValue: string;
  begin
    Result := Copy(S, 2 + Length(Switch), MaxInt);
  end;
begin
  Result := '';
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if (SwitchChars = []) or (S[1] in SwitchChars) then
    begin
      if IgnoreCase then
      begin
        if (AnsiCompareText(Copy(S, 2, Length(Switch)), Switch) = 0) then
          Result := SetSwitchValue;
      end
      else begin
        if (AnsiCompareStr(Copy(S, 2, Length(Switch)), Switch) = 0) then
          Result := SetSwitchValue;
      end;
    end;
  end;
end;

{ JclMiscel.WinExec32AndWait can be used instead of this }
function RunConsole(CmdLine: string): DWORD;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);

  if not CreateProcess(nil, PChar(CmdLine), nil, nil, false,
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
    RaiseLastWin32Error;

  if WaitforSingleObject(ProcessInfo.hProcess, INFINITE) = WAIT_FAILED then
    RaiseLastWin32Error;

  GetExitCodeProcess(ProcessInfo.hProcess, Result);
end;

{ TConsoleMore }

constructor TConsoleMore.Create(Text: string);
begin
  FText := Text;
  FPageNo := 0;
  FLines := TStringList.Create;
  FillLines;
  NextPage;
end;

procedure TConsoleMore.FillLines;
begin
  FLines.Clear;
  JclStrings.StrToStrings(FText, #13#10, FLines);
end;

function TConsoleMore.GetCurrentPage: TMorePage;
begin
  Result := GetPage(FPageNo);
end;

function TConsoleMore.GetCurrentPageContent: string;
begin
  Result := GetCurrentPage.Content;
end;

function TConsoleMore.GetFinished: boolean;
begin
  Result := GetCurrentPage.EmptyPage;
end;

function TConsoleMore.GetPage(PageNo: Integer): TMorePage;
var
  i: Integer;
begin
  Result.StartLine := (25 * PageNo) - 24;
  Result.EndLine := Result.StartLine + 24;

  if Result.EndLine > FLines.Count then
    Result.EndLine := FLines.Count;

  Result.EmptyPage := (Result.StartLine > FLines.Count);

  Result.Content := '';
  for i := Result.StartLine to Result.EndLine do
    { strings are zero based, page lines are 1 based }
    Result.Content := Result.Content + FLines[i-1] + #13#10;
end;

procedure TConsoleMore.NextPage;
begin
  Inc(FPageNo);
end;

end.
