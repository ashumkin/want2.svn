(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

library WantAcceptTestLib;

uses
  TestFramework,
  Win32Implementations in '..\src\win32\Win32Implementations.pas',
  RunnerTests,
  ExternalTests in 'ExternalTests.pas',
  ConsoleScriptRunner in '..\src\win32\ConsoleScriptRunner.pas';

{$R *.RES}

exports
  RegisteredTests index 1 name 'Test';
end.

