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

Contributor(s): Juancarlo Añez
--------------------------------------------------------------------------------
(based on BSD Open Source License)
}
unit DelphiTasks;

interface
uses
  DanteClasses,
  ExecTasks,

  JclBase,
  JclMiscel,
  JclSysInfo,
  JclRegistry,

  Windows,
  SysUtils,
  Classes;


type
  EDelphiTaskError       = class(ETaskError);
  EDelphiNotFoundError   = class(EDelphiTaskError);
  ECompilerNotFoundError = class(EDelphiTaskError);

  TDelphiCompileTask = class(TCustomExecTask)
  protected
    function FindDelphiDir :string;
    function FindCompiler :string;
  public
    procedure Execute; override;
  published
    property Arguments;
  end;

implementation

{ TDelphiCompileTask }

procedure TDelphiCompileTask.Execute;
begin
  self.Executable := FindCompiler;
  inherited Execute;
end;

function TDelphiCompileTask.FindCompiler: string;
begin
  Result := FindDelphiDir + '\bin\dcc32.exe';
  if not FileExists(Result) then
     raise ECompilerNotFoundError.Create(Result);
end;

function TDelphiCompileTask.FindDelphiDir: string;
const
  DelphiRegRoot  = 'SOFTWARE\Borland\Delphi';
  DelphiRootKey  = 'RootDir';

  function RootFor(version :Integer) :string;
  begin
    Result := Format('%s\%d.0', [DelphiRegRoot, version]);
  end;

  function ReadRootFor(version :Integer):string;
  begin
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, RootFor(version), DelphiRootKey, '');
  end;

var
  ver :Integer;
begin
  for ver := 6 downto 4 do
  begin
    Result := ReadRootFor(ver);
    if Result <> '' then EXIT;
  end;
  raise EDelphiNotFoundError.Create('');
end;

initialization
  RegisterClasses([TDelphiCompileTask]);
end.
