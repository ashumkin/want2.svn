(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

library WantTestLib;

uses
  TestFramework,

  {$IFDEF WIN32}
  Win32Implementations in '..\src\win32\Win32Implementations.pas',
  {$ENDIF}

  WantClassesTest in 'WantClassesTest.pas',
  FileSetTests in 'FileSetTests.pas',
  ExecTasksTest in 'ExecTasksTest.pas',
  FileTasksTest in 'FileTasksTest.pas',
  DelphiTasksTest in 'DelphiTasksTest.pas',
  WildPathsTest in 'WildPathsTest.pas',
  RegexpElementsTest in 'RegexpElementsTest.pas';

{$R *.RES}

exports
  RegisteredTests index 1 name 'Test';

end.

