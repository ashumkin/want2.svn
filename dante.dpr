{ $Id$ }
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
  DanteUnit, SysUtils;

function DanteHeader: string;
begin
  Result := 'Dante v0.0.0 Build 0. Build Management tool for Delphi';
end;

const
  SwitchChars = ['-', '/'];
begin
  if FindCmdLineSwitch('?', SwitchChars, true) or
     FindCmdLineSwitch('h', SwitchChars, true) then
  begin
    WriteLn(DanteHeader);
    WriteLn('For licensing info, use the -L switch');
    WriteLn;
    WriteLn('Usage:');
    WriteLn('  dante.exe [options]');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  -buildfile [file]   Specifies the build file. Default is');
    WriteLn('                      build.txt (will be build.xml in future)');
  end
  else if FindCmdLineSwitch('L', SwitchChars, false) then
  begin
    WriteLn(DanteHeader);
    //WriteLn(License);
  end
  else begin
    WriteLn('not functional yet.');
  end;
end.

