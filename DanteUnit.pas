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
unit DanteUnit;

interface

uses Windows, SysUtils, JclMiscel;

type
  TDante = class(TObject)
  protected
    function RunConsole(CmdLine: string): boolean;
  public
    procedure DoBuild(ABuildFileName: string);
  end;

implementation


{ TDante }

procedure TDante.DoBuild(ABuildFileName: string);
var
  F: TextFile;
  CmdLine: string;
begin
  AssignFile(F, ABuildFileName);
  Reset(F);
  try
    while not Eof(F) do
    begin
      ReadLn(F, CmdLine);
      if not RunConsole(CmdLine) then
        // need to report errors somehow
        RaiseLastWin32Error;
    end;
  finally
    CloseFile(F);
  end;
end;

function TDante.RunConsole(CmdLine: string): boolean;
begin
  Result := (WinExec32AndWait(CmdLine, SW_HIDE) = 0);
end;

end.


