(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

{$LONGSTRINGS ON}
unit Win32ChildProcesses;

interface
uses
  Windows,
  SysUtils,
  Math,

  WIN32,

  ChildProcesses;

const
  rcs_id :string = '#(@)$Id$';

type
  EChildProcessException = class(Exception);

  TWin32ChildProcess = class(TChildProcess)
  protected
    hChild,
    hOutputRead :THandle;

    function  Launch(const CmdLine: string; hInput, hOutput, hError: THandle): THandle;
    function  __Read(Count :Integer = 80)   :string; override;

    procedure Error(Msg :string);
  public
    destructor Destroy; override;

    procedure Run(CmdLine: string);  override;
    function  ExitCode :Cardinal;    override;
    function  EOF  :boolean;         override;
  end;


implementation

{ TWin32ChildProcess }

destructor TWin32ChildProcess.Destroy;
begin
  if (hOutputRead <> 0)
  and not CloseHandle(hOutputRead) then
    Error('CloseHandle:' + SysErrorMessage(GetLastError));
  if (hChild <> 0) then
  begin
    TerminateProcess(hChild, Cardinal(-1));
    if not CloseHandle(hChild) then
      Error('CloseHandle' + SysErrorMessage(GetLastError));
  end;
  inherited Destroy;
end;


function TWin32ChildProcess.EOF: boolean;
begin
  Result := (hOutputRead = 0) and inherited EOF;
end;

function TWin32ChildProcess.ExitCode: Cardinal;
begin
  if (WaitForSingleObject(hChild, INFINITE) <> WAIT_OBJECT_0)
  or not GetExitCodeProcess(hChild, Result) then
      Error(SysErrorMessage(GetLastError));
end;

function TWin32ChildProcess.Launch(const CmdLine: string; hInput, hOutput, hError: THandle): THandle;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Success:     boolean;
begin
  Result := 0;
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOW;

  StartupInfo.hStdInput   := hInput;
  StartupInfo.hStdOutput  := hOutput;
  StartupInfo.hStdError   := hError;

  Success := CreateProcess(nil, PChar(CmdLine), nil, nil, True,
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo);
  if not Success then
    Error('CreateProcess:' + SysErrorMessage(GetLastError))
  else begin
    WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hThread);
    Result := ProcessInfo.hProcess;
  end
end;

function TWin32ChildProcess.__Read(Count: Integer): string;
var
  BytesRead     :DWORD;
begin
  Result := '';
  if hOutputRead = 0 then
  begin
    Result := FLine;
    FLine := '';
    EXIT;
  end;
  repeat
    SetLength(Result, Max(Count, 1));
    if not ReadFile(hOutputRead, Result[1], Length(Result), BytesRead, nil)
    or (BytesRead = 0) then
    begin
      if GetLastError <> ERROR_BROKEN_PIPE then
         Error('ReadFile:' + SysErrorMessage(GetLastError)) // Something bad happened.
      else begin
         CloseHandle(hOutputRead);
         hOutputRead := 0;
         break // pipe done - normal exit path.
      end
    end
  until BytesRead > 0;
  SetLength(Result, BytesRead);
end;


procedure TWin32ChildProcess.Run(CmdLine: string);
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
     Error('CreatePipe:' + SysErrorMessage(GetLastError));

  // Create child input handle
  if not IsConsole then
    hInputRead := 0
  else if not DuplicateHandle(hCurrentProcess, GetStdHandle(STD_INPUT_HANDLE),
                         hCurrentProcess, @hInputRead,0,
                       TRUE,DUPLICATE_SAME_ACCESS) then
     Error('DuplicateHandle:' + SysErrorMessage(GetLastError));

  // Create a duplicate of the output write handle for the std error
  // write handle. This is necessary in case the child application
  // closes one of its std output handles.
  if not DuplicateHandle(hCurrentProcess,hOutputWrite,
                       hCurrentProcess,  @hErrorWrite,0,
                       TRUE,DUPLICATE_SAME_ACCESS) then
     Error('DuplicateHandle:' + SysErrorMessage(GetLastError));


  // Create new output read handle. Set
  // the Properties to FALSE. Otherwise, the child inherits the
  // properties and, as a result, non-closeable handles to the pipes
  // are created.
  if not DuplicateHandle(hCurrentProcess,hOutputReadTmp,
                       hCurrentProcess,
                       @hOutputRead, // Address of new handle.
                       0,FALSE, // Make it uninheritable.
                       DUPLICATE_SAME_ACCESS) then
     Error('DupliateHandle:' + SysErrorMessage(GetLastError));


  // Close inheritable copies of the handles you do not want to be
  // inherited.
  if not CloseHandle(hOutputReadTmp) then Error('CloseHandle:' + SysErrorMessage(GetLastError));

  hChild := Launch(CmdLine, hInputRead, hOutputWrite, hErrorWrite);


  // Close pipe handles (do not continue to modify the parent).
  // You need to make sure that no handles to the write end of the
  // output pipe are maintained in this process or else the pipe will
  // not close when the child process exits and the ReadFile will hang.
  if not CloseHandle(hOutputWrite) then Error('CloseHandle:' + SysErrorMessage(GetLastError));
  if hInputRead <> 0 then
  begin
    if not CloseHandle(hInputRead )  then Error('CloseHandle:' + SysErrorMessage(GetLastError));
  end;
  if not CloseHandle(hErrorWrite)  then Error('CloseHandle:' + SysErrorMessage(GetLastError));
end;


procedure TWin32ChildProcess.Error(Msg: string);
begin
  raise EChildProcessException.Create(Msg);
end;

initialization
 ChildProcesses.ChildProcessClass := TWin32ChildProcess;
end.
