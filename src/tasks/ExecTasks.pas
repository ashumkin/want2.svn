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
  JclSecurity,

  Math,
  Windows,
  SysUtils,
  Classes;


type
  TChildProcess = class;

  TCustomExecTask = class(TTask)
  protected
    FOS: string;
    FExecutable: string;
    FArguments : TStrings;
    FSkipLines : Integer;

    function BuildExecutable :string; virtual;
    function BuildArguments  :string; virtual;
    function BuildCmdLine    :string; virtual;

    function  GetArguments :string;
    procedure SetArguments(Value :string);
    procedure SetArgumentList(Value :TStrings);

    procedure Run(CmdLine: string);
    procedure HandleOutput(Child :TChildProcess);
  public
    constructor Create(Owner: TDanteElement);  override; 
    destructor Destroy; override;

    procedure Execute; override;
  protected
    property Arguments: string      read GetArguments write SetArguments;
    property ArgumentList: TStrings read FArguments   write SetArgumentList stored False;
    property Executable: string     read FExecutable  write FExecutable;
    property SkipLines :Integer     read FSkipLines   write FSkipLines;
    property OS: string read FOS    write FOS;
  end;

  TExecTask = class(TCustomExecTask)
  public
    procedure Execute; override;
  published
    property Arguments;
    property ArgumentList stored False;
    property Executable;
    property SkipLines :Integer     read FSkipLines   write FSkipLines;
    property OS;
  end;

  // this class will pass commands through the command processor
  TShellTask = class(TExecTask)
  protected
    function BuildExecutable :string; override;
  end;

  TChildProcess = class
  protected
    hChild,
    hOutputRead :THandle;
    FExitCode   :Cardinal;
    FLine       :String;

    function  Launch(const CmdLine: string; hInput, hOutput, hError: THandle): THandle;
    function  __Read(Count :Integer = 80)   :string;
  public
    destructor Destroy; override;

    procedure Run(CmdLine: string);

    function ExitCode :Cardinal;

    function EOF  :boolean;
    function Read(Count :Integer = 80)   :string;
    function ReadLn :string;
  end;


implementation

{ TExecTask }

procedure TExecTask.Execute;
begin
  Log(Executable);
  Log(vlVerbose, BuildArguments);
  inherited Execute;
end;

{ TShellTask }

function TShellTask.BuildExecutable: string;
begin
  Result := inherited BuildExecutable;
  if IsWinNT then
    Result := 'cmd.exe /c ' + Result
  else
    Result := 'command.com /c ' + Result;
end;

{ TCustomExecTask }

function TCustomExecTask.BuildExecutable: string;
begin
  Result := Executable;
end;

function TCustomExecTask.BuildCmdLine: string;
begin
  Result := BuildExecutable + BuildArguments;
end;

function TCustomExecTask.BuildArguments: string;
var
  i: Integer;
begin
  Result := '';
  { Arguments.CommaText screws with the contents. See unit test }
  for i := 0 to ArgumentList.Count - 1 do
    Result := Result + ' ' + ArgumentList[i];
end;

constructor TCustomExecTask.Create(Owner: TDanteElement);
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
  CmdLine :string;
begin
  CmdLine := BuildCmdLine;
  Log(vlDebug, CmdLine);
  Run(CmdLine);
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

procedure TCustomExecTask.Run(CmdLine :string);
var
  Child :TChildProcess;
begin
  Child := TChildProcess.Create;
  try
    Child.Run(CmdLine);
    HandleOutput(Child);
    if Child.ExitCode <> 0 then
      raise ETaskFailure.Create('failed');
  finally
    Child.Free;
  end;
end;


procedure TCustomExecTask.HandleOutput(Child :TChildProcess);
const
  TimeOutMillis = 100;
var
  Line          :String;
  LineNo        :Integer;
begin
  LineNo := 0;
  while not Child.EOF do
  begin
    Line := Child.ReadLn;
    Inc(LineNo);
    if LineNo > SkipLines then
      Log(LIne);
  end;
end;

{ TChildProcess }

destructor TChildProcess.Destroy;
begin
  if (hOutputRead <> 0)
  and not CloseHandle(hOutputRead) then
    RaiseLastSystemError('CloseHandle');
  if (hChild <> 0) and not CloseHandle(hChild) then
    RaiseLastSystemError('CloseHandle');
  inherited Destroy;
end;


function TChildProcess.EOF: boolean;
begin
  Result := (hOutputRead = 0);
end;

function TChildProcess.ExitCode: Cardinal;
begin
  if WaitForSingleObject(hChild, INFINITE) = WAIT_OBJECT_0 then
  begin
    if not GetExitCodeProcess(hChild, Result) then
      RaiseLastSystemError
  end;
end;

function TChildProcess.Launch(const CmdLine: string; hInput, hOutput, hError: THandle): THandle;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Success:     boolean;
begin
  Result := 0;
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;

  StartupInfo.hStdInput   := hInput;
  StartupInfo.hStdOutput  := hOutput;
  StartupInfo.hStdError   := hError;

  Success := CreateProcess(nil, PChar(CmdLine), nil, nil, True,
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo);
  if not Success then
    RaiseLastSystemError
  else begin
    WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hThread);
    Result := ProcessInfo.hProcess;
  end
end;

function TChildProcess.Read(Count :Integer): string;
begin
  if FLine <> '' then
  begin
    Result := FLine;
    FLIne  := '';
  end
  else
    Result := __Read(Count);
end;

function TChildProcess.__Read(Count: Integer): string;
var
  BytesRead     :DWORD;
begin
  repeat
    SetLength(Result, Max(Count, 1));
    if not ReadFile(hOutputRead, Result[1], Length(Result), BytesRead, nil)
    or (BytesRead = 0) then
    begin
      if GetLastError <> ERROR_BROKEN_PIPE then
         RaiseLastSystemError('ReadFile') // Something bad happened.
      else begin
         CloseHandle(hOutputRead);
         hOutputRead := 0;
         break // pipe done - normal exit path.
      end
    end
  until BytesRead > 0;
  SetLength(Result, BytesRead);
end;


function TChildProcess.ReadLn: string;
var
  c             :Char;
  p             :Integer;
begin
  Result := '';
  p := 0;
  while not EOF do
  begin
    FLine := FLine + __Read;
    while p < Length(FLine) do
    begin
      Inc(p);
      c := FLine[p];
      if c in [#10,#13] then
      begin
        Result := Copy(FLine, 1, p-1);
        if (c = #13) and (p < Length(FLine)) and (FLine[p+1] = #10) then
          Inc(p);
        Delete(FLine, 1, p);
        Exit;
      end;
    end;
  end;
end;

procedure TChildProcess.Run(CmdLine: string);
var
  hCurrentProcess,
  hOutputReadTmp,
  hOutputWrite,
  hInputRead,
  hErrorWrite  :THandle;
  sa            :TSecurityAttributes;
begin
  hCurrentProcess := GetCurrentProcess;
  FillChar(sa, SizeOf(sa), 0);
  sa.nLength  := SizeOf(sa);
  sa.bInheritHandle := true;


  // Create the child output pipe.
  if not CreatePipe(hOutputReadTmp, hOutputWrite, @sa, 0) then
     RaiseLastSystemError('CreatePipe');

  // Create child input handle
  if not DuplicateHandle(hCurrentProcess, GetStdHandle(STD_INPUT_HANDLE),
                         hCurrentProcess, @hInputRead,0,
                       TRUE,DUPLICATE_SAME_ACCESS) then
     RaiseLastSystemError('DuplicateHandle');

  // Create a duplicate of the output write handle for the std error
  // write handle. This is necessary in case the child application
  // closes one of its std output handles.
  if not DuplicateHandle(hCurrentProcess,hOutputWrite,
                       hCurrentProcess,  @hErrorWrite,0,
                       TRUE,DUPLICATE_SAME_ACCESS) then
     RaiseLastSystemError('DuplicateHandle');


  // Create new output read handle. Set
  // the Properties to FALSE. Otherwise, the child inherits the
  // properties and, as a result, non-closeable handles to the pipes
  // are created.
  if not DuplicateHandle(hCurrentProcess,hOutputReadTmp,
                       hCurrentProcess,
                       @hOutputRead, // Address of new handle.
                       0,FALSE, // Make it uninheritable.
                       DUPLICATE_SAME_ACCESS) then
     RaiseLastSystemError('DupliateHandle');


  // Close inheritable copies of the handles you do not want to be
  // inherited.
  if not CloseHandle(hOutputReadTmp) then RaiseLastSystemError('CloseHandle');

  hChild := Launch(CmdLine, hInputRead, hOutputWrite, hErrorWrite);


  // Close pipe handles (do not continue to modify the parent).
  // You need to make sure that no handles to the write end of the
  // output pipe are maintained in this process or else the pipe will
  // not close when the child process exits and the ReadFile will hang.
  if not CloseHandle(hOutputWrite) then RaiseLastSystemError('CloseHandle');
  if not CloseHandle(hInputRead )  then RaiseLastSystemError('CloseHandle');
  if not CloseHandle(hErrorWrite)  then RaiseLastSystemError('CloseHandle');
end;


initialization
  RegisterTasks([TCustomExecTask, TExecTask, TShellTask]);
end.

