{ $Id$ }
{--------------------------------------------------------------------------}
{ Copyright (c) 2001, Dante Authors -- See authors.txt for complete list   }
{ All rights reserved.                                                     }
{                                                                          }
{ Redistribution and use in source and binary forms, with or without       }
{ modification, are permitted provided that the following conditions       }
{ are met:                                                                 }
{                                                                          }
{ 1. Redistributions of source code must retain the above copyright        }
{    notice, this list of conditions and the following disclaimer.         }
{                                                                          }
{ 2. Redistributions in binary form must reproduce the above copyright     }
{    notice, this list of conditions and the following disclaimer in       }
{    the documentation and/or other materials provided with the            }
{    distribution.                                                         }
{                                                                          }
{ 3. The name Dante, the names of the authors in authors.txt and the       }
{    names of other contributors to this software may not be used to       }
{    endorse or promote products derived from this software without        }
{    specific prior written permission.                                    }
{                                                                          }
{  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     }
{  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     }
{  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       }
{  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          }
{  COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   }
{  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    }
{  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS   }
{  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND  }
{  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR   }
{  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE  }
{  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.}
{                                                                          }
{--------------------------------------------------------------------------}
{ The JCL (JEDI Code Library) used in Dante is governed by                 }
{ the terms of the MPL (Mozilla Public License) found at                   }
{ http://www.mozilla.org/MPL/MPL-1.1.html.                                 }
{                                                                          }
{ The source code for JCL itself can be found on JEDI Web site:            }
{ http://delphi-jedi.org/Jedi:CODELIBJCL.                                  }
{--------------------------------------------------------------------------}
{ (based on BSD Open Source License)                                       }
{--------------------------------------------------------------------------}

{$APPTYPE CONSOLE}
program dante;

uses
  DanteBase,
  DanteClasses,
  SysUtils,
  clUtilConsole,
  LogMgr,
  JclStrings;

var
  Log:     TLogManager;
  Project: TProject;

  BuildFilename: String;
  Targets:       String;

function IsCmdLineSwitch(const aSwitch: String): Boolean;
var
  i: Integer;
  l: Integer;
  c: Char;
  s: String;
begin
  Result := False;
  i      := 1;

  while i <= ParamCount do
    begin
      s := ParamStr(i);
      l := Length(s);

      if l > 0 then
        begin
          c := s[1];

          if (c in SwitchChars) and
             (CompareText(StrRight(s, l - 1), aSwitch) = 0) then
            begin
              Result := True;
              i      := ParamCount;
            end;
        end;

      Inc(i);
    end;
end;

function GetCmdLineSwitch(const aSwitch: String): String;
var
  i: Integer;
  l: Integer;
  c: Char;
  s: String;
begin
  Result := '';
  i      := 1;

  while i <= ParamCount do
    begin
      s := ParamStr(i);
      l := Length(s);

      if l > 0 then
        begin
          c := s[1];

          if (c in SwitchChars) and
             (CompareText(StrRight(s, l - 1), aSwitch) = 0) then
            begin
              if i < ParamCount then
                Result := ParamStr(i + 1);

              i := ParamCount;
            end;
        end;

      Inc(i);
    end;
end;

function ParseTargets: String;
var
  s: String;
begin
  s := ParamStr(ParamCount);

  if (Length(s) > 0) and not (s[1] in SwitchChars) then
    Result := s
  else
    Result := '';
end;

begin
  if IsCmdLineSwitch('?') or IsCmdLineSwitch('h') then
    begin
      WriteLn(DanteHeaderText);
      WriteLn(DanteUsageText);
    end
  else if IsCmdLineSwitch('L') then
    begin
        // need to add More functionality ... going to add it in clUtilConsole
      WriteLn(DanteHeaderText);

      WriteLn(DanteLicenseText1);
      WriteLn(DanteLicenseText2);
      WriteLn(DanteLicenseText3);
      WriteLn(DanteLicenseText4);
    end
  else
    begin
      WriteLn(DanteHeaderText);

      Log     := TLogManager.Create;
      Project := TProject.Create(nil);

      if Assigned(Log) and Assigned(Project) then
        begin
          Log.Start;

          try
            BuildFilename := GetCmdLineSwitch('buildfile');

            if IsCmdLineSwitch('verbose') then
              Log.Level := vlVerbose
            else if IsCmdLineSwitch('debug') then
              Log.Level := vlDebug;

            Targets := ParseTargets;

            Project.Initialize(Log, BuildFilename);

            Project.Build(Targets);

          finally
            Log.Stop;

            FreeAndNil(Project);
            FreeAndNil(Log);
          end;
        end
      else
        WriteLn(F_DanteStartupFailed);
    end;
end.

