(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }
program WantTests;

uses
  GUITestRunner,
  TextTestRunner,
  TestFramework,
  RunnerTests,
  WantClassesTest in 'WantClassesTest.pas',
  FileSetTests in 'FileSetTests.pas',
  ExecTasksTest in 'ExecTasksTest.pas',
  FileTasksTest in 'FileTasksTest.pas',
  DelphiTasksTest in 'DelphiTasksTest.pas',
  WildPathsTest in 'WildPathsTest.pas',
  RegexpElementsTest in 'RegexpElementsTest.pas',
  ExternalTests in 'ExternalTests.pas',
  ConsoleScriptRunner in '..\src\win32\ConsoleScriptRunner.pas',
  StyleTasks in '..\src\tasks\StyleTasks.pas',
  MSXMLEngineImpl in '..\src\win32\MSXMLEngineImpl.pas';

{$R *.RES}

begin
  {$IFDEF USE_TEXT_RUNNER}
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures)
  {$ELSE}
    GUITestRunner.RunRegisteredTests;
  {$ENDIF}
end.

