{ $Id$ }
{
--------------------------------------------------------------------------------
Copyright (c) 2001, Want Authors -- See authors.txt for complete list
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The name Want, the names of the authors in authors.txt and the names of
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

{//APPTYPE CONSOLE}
program want;

uses
  SysUtils,
  EditTasks in 'tasks\EditTasks.pas',
  WantStandardTasks in 'tasks\WantStandardTasks.pas',
  Attributes in 'elements\Attributes.pas',
  DUnitTasks in 'tasks\DUnitTasks.pas',
  ZipStreams in 'lib\ZipStreams.pas',
  OwnedTrees in 'lib\OwnedTrees.pas',
  WildPaths in 'lib\WildPaths.pas',
  ConsoleLogMgr in 'lib\ConsoleLogMgr.pas',
  ZipTasks in 'tasks\ZipTasks.pas',
  DelphiTasks in 'tasks\DelphiTasks.pas',
  EchoTasks in 'tasks\EchoTasks.pas',
  ExecTasks in 'tasks\ExecTasks.pas',
  FileTasks in 'tasks\FileTasks.pas',
  LoggerTask in 'tasks\LoggerTask.pas',
  StandardTasks in 'tasks\StandardTasks.pas',
  VssTasks in 'tasks\VssTasks.pas',
  CustomTasks in 'tasks\CustomTasks.pas',
  TimeElements in 'elements\TimeElements.pas',
  PatternSets in 'elements\PatternSets.pas',
  Properties in 'elements\Properties.pas',
  RegexpElements in 'elements\RegexpElements.pas',
  StandardElements in 'elements\StandardElements.pas',
  crt32 in 'lib\CRT32.pas',
  LogMgr in 'lib\LogMgr.pas',
  ScriptParser in 'lib\ScriptParser.pas',
  ScriptFrm in 'forms\ScriptFrm.pas' {ScriptForm},
  ScriptRunner in 'ScriptRunner.pas',
  WantBase in 'WantBase.pas',
  WantClasses in 'WantClasses.pas',
  WantTasks in 'tasks\WantTasks.pas';

{$R wantver.res}

const
  SwitchChars = ['-', '/'];

procedure Run;
var
  Runner :TConsoleScriptRunner;
begin
  try
    Runner := TConsoleScriptRunner.Create;
    try
      Runner.Execute;
    finally
      FreeAndNil(Runner);
    end;
  except
    Halt(1);
  end;
end;

begin
  if FindCmdLineSwitch('?', SwitchChars, true) or
     FindCmdLineSwitch('h', SwitchChars, true) then
  begin
    WriteLn(WantHeader);
    Usage;
  end
  else if FindCmdLineSwitch('L', SwitchChars, false) then
  begin
    // need to add More functionality ... going to add it in clUtilConsole
    WriteLn(WantHeader);
    WriteLn(License);
  end
  else
    Run;
end.

