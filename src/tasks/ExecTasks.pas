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
unit ExecTasks;

interface
uses
  DanteClasses,

  JclBase,
  JclMiscel,
  JclSysInfo,

  Windows,
  SysUtils,
  Classes;


type
  TCustomExecTask = class(TTask)
  protected
    FOS: string;
    FExecutable: string;
    FArguments: TStrings;

    function BuildCmdLine: string; virtual;

    function  GetArguments :string;
    procedure SetArguments(Value :string);
    procedure SetArgumentList(Value :TStrings);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    class function XMLTag :string; override;

    procedure Execute; override;
  protected
    property Arguments: string      read GetArguments write SetArguments;
    property ArgumentList: TStrings read FArguments   write SetArgumentList stored False;
    property Executable: string     read FExecutable  write FExecutable;
    property OS: string read FOS    write FOS;
  end;

  TExecTask = class(TCustomExecTask)
  public
    class function XMLTag :string; override;
  published
    property Arguments;
    property ArgumentList stored False;
    property Executable;
    property OS;
  end;

  // this class will pass commands through the command processor
  TShellExecTask = class(TExecTask)
  protected
    function BuildCmdLine: string; override;
  public
    class function XMLTag :string; override;
  end;


implementation

{ TCustomExecTask }

function TCustomExecTask.BuildCmdLine: string;
var
  i: Integer;
begin
  Result := Executable;
  { Arguments.CommaText screws with the contents. See unit test }
  for i := 0 to ArgumentList.Count - 1 do
    Result := Result + ' ' + ArgumentList[i];
end;

constructor TCustomExecTask.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FArguments := TStringList.Create;
end;

destructor TCustomExecTask.Destroy;
begin
  FArguments.Free;
  inherited;
end;

procedure TCustomExecTask.Execute;
var
  CmdLine : string;
  ExitCode: Cardinal;
begin
  CmdLine := BuildCmdLine;
  Log(CmdLine, vlVerbose);

  ExitCode := WinExec32AndWait(CmdLine, 0);

  if ExitCode = Cardinal(-1) then
    raise ETaskError.Create(SysErrorMessage(GetLastError))
  else if ExitCode <> 0 then
    raise ETaskFailure.Create('Execution failed (' + CmdLine + '). ExitCode: ' +
      IntToStr(ExitCode));
end;


function TCustomExecTask.GetArguments: string;
begin
  Result := FArguments.CommaText;
end;

procedure TCustomExecTask.SetArguments(Value: string);
begin
  FArguments.CommaText := Value;
end;

procedure TCustomExecTask.SetArgumentList(Value: TStrings);
begin
  FArguments.Assign(Value);
end;

class function TCustomExecTask.XMLTag: string;
begin
  result := 'custom_exec';
end;

{ TExecTask }

class function TExecTask.XMLTag: string;
begin
  result := 'exec';
end;


{ TShellExecTask }

function TShellExecTask.BuildCmdLine: string;
begin
  Result := inherited BuildCmdLine;
  if IsWinNT then
    Result := 'cmd.exe /c ' + Result
  else
    Result := 'command.com /c ' + Result;
end;

class function TShellExecTask.XMLTag: string;
begin
  Result := 'shell';
end;

initialization
  RegisterTasks([TCustomExecTask, TExecTask, TShellExecTask]);
end.

