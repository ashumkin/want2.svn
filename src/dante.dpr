{ $Id$ }
{
--------------------------------------------------------------------------------
Copyright (c) 2001, Chris Morris
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The names Chris Morris, Dante and the names of contributors to this software
may not be used to endorse or promote products derived from this software
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

{$APPTYPE CONSOLE}
program dante;

uses
  SysUtils,
  clUtilConsole,
  DanteClasses,
  StandardTasks,
  DanteMain;

function DanteHeader: string;
begin
  Result := 'Dante v0.0.0 Build 0. Build Management tool for Delphi'#13#10;
end;

function License: string;
const
  CR = '';  // #13#10 is not necessary because text is exactly 80 col
begin
  Result :=
    '--------------------------------------------------------------------------------'+ CR +
    'Copyright (c) 2001, Chris Morris                                                '+ CR +
    'All rights reserved.                                                            '+ CR +
    '                                                                                '+ CR +
    'Redistribution and use in source and binary forms, with or without modification,'+ CR +
    'are permitted provided that the following conditions are met:                   '+ CR +
    '                                                                                '+ CR +
    '1. Redistributions of source code must retain the above copyright notice, this  '+ CR +
    'list of conditions and the following disclaimer.                                '+ CR +
    '                                                                                '+ CR +
    '2. Redistributions in binary form must reproduce the above copyright notice,    '+ CR +
    'this list of conditions and the following disclaimer in the documentation and/or'+ CR +
    'other materials provided with the distribution.                                 '+ CR +
    '                                                                                '+ CR +
    '3. The names Chris Morris, Dante and the names of contributors to this software '+ CR +
    'may not be used to endorse or promote products derived from this software       '+ CR +
    'without specific prior written permission.                                      '+ CR +
    '                                                                                '+ CR +
    'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''''   '+ CR +
    'AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE       '+ CR +
    'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE  '+ CR +
    'DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE   '+ CR +
    'FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL      '+ CR +
    'DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR      '+ CR +
    'SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      '+ CR +
    'CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR'+ CR +
    'TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF   '+ CR +
    'THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               '+ CR +
    '--------------------------------------------------------------------------------'+ CR +
    '                                                                                '+ CR +
    '--------------------------------------------------------------------------------'+ CR +
    'The JCL (JEDI Code Library) used in Dante is governed by the terms of the       '+ CR +
    'MPL (Mozilla Public License) found at http://www.mozilla.org/MPL/MPL-1.1.html.  '+ CR +
    '                                                                                '+ CR +
    'The source code for JCL itself can be found on JEDI Web site:                   '+ CR +
    'http://delphi-jedi.org/Jedi:CODELIBJCL.                                         '+ CR +
    '--------------------------------------------------------------------------------';
end;

const
  SwitchChars = ['-', '/'];

function FindBuildFile(BuildFile :string):string;
begin
  Result := ExpandFileName('.\'+ BuildFile);
  // find the Result in the current or super directories
  while not FileExists(Result)
        and FileExists(ExtractFileDir(Result))
  do begin
    Result := ExpandFileName(ExtractFilePath(Result) + '..\' + ExtractFilename(Result));
  end;
  if not FileExists(Result) then
    Result := BuildFile;
end;


procedure Run;
var
  BuildFile: string;
  ADante:    TDante;
  p:         Integer;
begin
  BuildFile := '';
  p := 1;
  while p <= ParamCount do
  begin
    if ParamStr(p) = '-buildfile' then
    begin
      Inc(p);
      BuildFile := ParamStr(p);
    end;
    Inc(p);
  end;

  if BuildFile = '' then
    BuildFile := FindBuildFile(BuildFileName);
  if not FileExists(BuildFile) then
  begin
    // in the future add -find support and -buildfile support
    WriteLn('Cannot find ' + ExtractFileName(BuildFile));
  end
  else
  begin
    ADante := TDante.Create;
    try
      try
        ADante.DoBuild(BuildFile);
        WriteLn('Build complete.');
      except
        on E: Exception do
          WriteLn(E.message);
      end;
    finally
      ADante.Free;
    end;
  end
end;

procedure Usage;
begin
    WriteLn('For licensing info, use the -L switch');
    WriteLn;
    WriteLn('Usage:');
    WriteLn('  dante.exe [options]');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  -h, -H, -?          Displays this help text.');
    WriteLn('  -buildfile [file]   Specifies the build file. Default is');
    WriteLn('                      build.xml');
end;

begin
  WriteLn(DanteHeader);
  if FindCmdLineSwitch('?', SwitchChars, true) or
     FindCmdLineSwitch('h', SwitchChars, true) then
  begin
    Usage;
  end
  else if FindCmdLineSwitch('L', SwitchChars, false) then
  begin
    // need to add More functionality ... going to add it in clUtilConsole
    WriteLn(License);
  end
  else
    Run;
end.

