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
  DanteBase,
  DanteClasses,
  DanteMain;

{ this was ../bin/dantever.res, but bootstrap.bat on a clean machine didn't
  leave dantever.res in the src directory, so the subsequent dcc32 call failed
  -- Chrismo }
{$R dantever.res}
{$R license.res}

const
  SwitchChars = ['-', '/'];

procedure ShowLicense;
var
  Output: string;
  AConsoleMore: TConsoleMore;
begin
  Output := DanteHeader + #13#10 + License;
  AConsoleMore := TConsoleMore.Create(Output);
  try
    while not AConsoleMore.Finished do
    begin
      WriteLn(AConsoleMore.CurrentPage);
      ReadLn;
      AConsoleMore.NextPage;
    end;
  finally
    AConsoleMore.Free;
  end;
end;

procedure Run;
var
  ADante :TConsoleDante;
begin
  try
    ADante := TConsoleDante.Create;
    try
      ADante.UseColor := False;
      ADante.DoBuild;
    finally
      ADante.Free;
    end;
  except
    Halt(1);
  end;
end;

begin
  if FindCmdLineSwitch('?', SwitchChars, true) or
     FindCmdLineSwitch('h', SwitchChars, true) then
  begin
    WriteLn(DanteBase.DanteHeader);
    Usage;
  end
  else if FindCmdLineSwitch('L', SwitchChars, false) then
  begin
    ShowLicense;
  end
  else
    Run;
end.

