{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca (R)                               }
{    ~                                         }
{     Copyright (C) 1995,2001 Juancarlo Añez   }
{     All rights reserved.                     }
{            http://www.suigeneris.org/juanca  }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{#(@)$Id$}

library WantTestLib;

uses
  TestFramework,
  Win32Implementations in '..\src\win32\Win32Implementations.pas',
  WantClassesTest in 'WantClassesTest.pas',
  FileSetTests in 'FileSetTests.pas',
  ExecTasksTest in 'ExecTasksTest.pas',
  FileTasksTest in 'FileTasksTest.pas',
  DelphiTasksTest in 'DelphiTasksTest.pas',
  WildPathsTest in 'WildPathsTest.pas',
  CVSTasksTests in 'CVSTasksTests.pas',
  RegexpElementsTest in 'RegexpElementsTest.pas',
  WantStandardTasks in '..\src\tasks\WantStandardTasks.pas',
  StandardTasks in '..\src\tasks\StandardTasks.pas',
  LoadFileTests,
  FilterChainsElementsTests;

{$R *.RES}

exports
  RegisteredTests name 'Test';
end.

