(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

program want;

uses
  SysUtils,
  WIN32 in 'win32\WIN32.pas',
  crt32 in 'win32\CRT32.pas',
  Win32ChildProcesses in 'win32\Win32ChildProcesses.pas',
  ConsoleListener in 'win32\ConsoleListener.pas',
  Win32Implementations in 'win32\Win32Implementations.pas',
  Resources in 'win32\Resources.pas',
  EditTasks in 'tasks\EditTasks.pas',
  WantStandardTasks in 'tasks\WantStandardTasks.pas',
  Attributes in 'elements\Attributes.pas',
  DUnitTasks in 'tasks\DUnitTasks.pas',
  ZipStreams in 'lib\ZipStreams.pas',
  OwnedTrees in 'lib\OwnedTrees.pas',
  WildPaths in 'lib\WildPaths.pas',
  ZipTasks in 'tasks\ZipTasks.pas',
  DelphiTasks in 'tasks\DelphiTasks.pas',
  EchoTasks in 'tasks\EchoTasks.pas',
  ExecTasks in 'tasks\ExecTasks.pas',
  FileTasks in 'tasks\FileTasks.pas',
  LoggerTask in 'tasks\LoggerTask.pas',
  StandardTasks in 'tasks\StandardTasks.pas',
  CustomTasks in 'tasks\CustomTasks.pas',
  TimeElements in 'elements\TimeElements.pas',
  PatternSets in 'elements\PatternSets.pas',
  Properties in 'elements\Properties.pas',
  RegexpElements in 'elements\RegexpElements.pas',
  StandardElements in 'elements\StandardElements.pas',
  ScriptParser in 'lib\ScriptParser.pas',
  ScriptFrm in 'forms\ScriptFrm.pas' {ScriptForm},
  ScriptRunner in 'ScriptRunner.pas',
  WantClasses in 'WantClasses.pas',
  WantTasks in 'tasks\WantTasks.pas',
  XPerlRE in 'lib\XPerlRE.pas',
  ChildProcesses in 'lib\ChildProcesses.pas',
  WantResources in 'WantResources.pas',
  ConsoleScriptRunner in 'win32\ConsoleScriptRunner.pas',
  BuildListeners in 'BuildListeners.pas';

{$r wantver.res}
{$r ..\bin\license.res }

const
  SwitchChars = ['-', '/'];

procedure Run;
var
  Runner  :TConsoleScriptRunner;
begin
  try
    Runner := TConsoleScriptRunner.Create;
    try
      Runner.Execute;
    finally
      FreeAndNil(Runner);
    end;
  except
    on e :EWantException do
      Halt(1);
    on e :Exception do
      raise;
  end;
end;

begin
  Run;
end.

