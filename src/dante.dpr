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
}

{$APPTYPE CONSOLE}
program dante;

uses
  SysUtils,
  JclStrings,
  clUtilConsole,
  DanteClasses,
  StandardTasks,
  CustomTasks,
  DanteMain,
  JclFileUtils,
  VersionInfoTasks in 'tasks\VersionInfoTasks.pas';

{$R dantever.res}

function GetVersionString: string;
var
  AFileVer: TJclFileVersionInfo;
begin
  try
    AFileVer := TJclFileVersionInfo.Create(ParamStr(0));
    try
      Result := AFileVer.FileVersion;
    finally
      AFileVer.Free;
    end;
  except
    Result := '?.?.?.?';
  end;
end;

function DanteHeader: string;
const
  CR = #13#10;
begin
  Result :=
    'Dante ' + GetVersionString + ' Build Management tool                  '+ CR +
    'Copyright (c) 2001, Dante Authors -- See authors.txt for complete list'+ CR +
    'For complete licensing info, execute with -L switch';
end;

function License: string;
const
  CR = '';  // #13#10 is not necessary because text is exactly 80 col
begin
  Result :=
    '--------------------------------------------------------------------------------'+ CR +
    'Copyright (c) 2001, Dante Authors -- See authors.txt for complete list          '+ CR +
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
    '3. The name Dante, the names of the authors in authors.txt and the names of     '+ CR +
    'other contributors to this software may not be used to endorse or promote       '+ CR +
    'products derived from this software without specific prior written permission.  '+ CR +
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
    '--------------------------------------------------------------------------------'+ CR +
    '                                                                                '+ CR +
    'Portions include:                                                               '+ CR +
    '--------------------------------------------------------------------------------'+ CR +
    'Copyright (c) 2000, Chris Morris                                                '+ CR +
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
    '3. The names Chris Morris, cLabs and the names of contributors to this          '+ CR +
    'software may be used to endorse or promote products derived from this software  '+ CR +
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
    '--------------------------------------------------------------------------------';

end;

const
  SwitchChars = ['-', '/'];

procedure Run;
var
  BuildFile: string;
  Targets:   string;
  ADante:    TDante;
  p:         Integer;
  VLevel:    TVerbosityLevel;
begin
  BuildFile := '';
  Targets   := '';
  VLevel    := vlNormal;
  p := 1;
  while p <= ParamCount do
  begin
    if ParamStr(p)[1] <> '-' then
    begin
      if Targets = '' then
        Targets := ParamStr(p)
      else
        Targets := Targets + ',' + ParamStr(p);
    end
    else if ParamStr(p) = '-buildfile' then
    begin
      Inc(p);
      BuildFile := ParamStr(p);
    end
    else if ParamStr(p) = '-verbose' then
      VLevel := vlVerbose
    else if ParamStr(p) = '-debug' then
      VLevel := vlDebug
    else begin
    end;
    Inc(p);
  end;

  if BuildFile = '' then
    BuildFile := BuildFileName;
  if not FileExists(BuildFile) then
    WriteLn('Cannot find ' + ExtractFileName(BuildFile))
  else
  begin
    ADante := TDante.Create;
    try
      try
        Writeln('buildfile: ', BuildFile);
        Writeln;
        ADante.DoBuild(BuildFile, Targets, VLevel);
        Writeln;
        WriteLn('Build complete.');
      except
        on E: Exception do
        begin
          if not E.ClassType.InheritsFrom(EDanteException) then
            Writeln(E.ClassName + ': ' + E.Message);
          Writeln;
          WriteLn('BUILD FAILED');
        end;
      end;
    finally
      ADante.Free;
    end;
  end
end;

procedure Usage;
begin
    WriteLn;
    WriteLn('Usage:');
    WriteLn('  dante.exe [options]');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  -h, -H, -?          Displays this help text.');
    WriteLn('  -buildfile [file]   Specifies the build file. Default is');
    WriteLn('                      build.xml');
    WriteLn('  -verbose            Be extra verbose.');
    WriteLn('  -debug              Print debugging information.');
end;

begin
  if FindCmdLineSwitch('?', SwitchChars, true) or
     FindCmdLineSwitch('h', SwitchChars, true) then
  begin
    WriteLn(DanteHeader);
    Usage;
  end
  else if FindCmdLineSwitch('L', SwitchChars, false) then
  begin
    // need to add More functionality ... going to add it in clUtilConsole
    WriteLn(DanteHeader);
    WriteLn(License);
  end
  else
    Run;
end.

