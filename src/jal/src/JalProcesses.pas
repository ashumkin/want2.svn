{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca �                                 }
{    ~                                         }
{  Copyright � 1995-2003 Juancarlo A�ez        }
{  http://www.suigeneris.org/juanca            }
{  All rights reserved.                        }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{ $Id$ }

{$LONGSTRINGS ON}

unit JalProcesses;

interface
uses
  Windows,
  SysUtils,
  Classes;

const
  rcs_id :string = '#(@)$Id$';


type
  EChildProcessException = class(Exception);

  TPipeStream = class(THandleStream)
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

  TChildProcess = class
  protected
    FLastChar :char;

    FRedirectedInput :boolean;
    FInputStream,
    FOutputStream :THandleStream;

    function  __Read(Count :Integer = 80)   :string; virtual; abstract;
    procedure Error(Msg :string);


    procedure CloseStreams;
  public
    constructor Create(RedirectInput :boolean=false); virtual;
    destructor Destroy; override;

    procedure Run(CmdLine: string);  virtual; abstract;
    function  ExitCode :Cardinal;    virtual; abstract;

    function EOF :boolean; virtual;
    function ReadLine :string;

    property RedirectedInput :boolean read FRedirectedInput;

    property Input  :THandleStream read FInputStream;
    property Output :THandleStream read FOutputStream;
  end;

  TWin32ChildProcess = class(TChildProcess)
  protected
    hChild,
    hInputWrite,
    hOutputRead,
    hInputRead,
    hOutputWrite,
    hErrorWrite :THandle;

    function  Launch(const CmdLine: string; hInput, hOutput, hError: THandle): THandle;

    procedure CreatePipes;
    procedure CloseStreams;

    procedure CreatePipe(var hReadPipe, hWritePipe: THandle; const PipeAttributes: TSecurityAttributes; nSize: DWORD);
    procedure DuplicateHandle(hSourceProcessHandle, hSourceHandle, hTargetProcessHandle: THandle;
              var TargetHandle: THandle; dwDesiredAccess: DWORD;
              bInheritHandle: boolean; dwOptions: DWORD);
    procedure CloseHandle(Handle :THandle);
  public
    constructor Create(RedirectInput :Boolean = false);  override;
    destructor Destroy; override;

    procedure Run(CmdLine: string);  override;
    function  ExitCode :Cardinal;    override;
  end;

  TChildProcessClass = class of TChildProcess;

var
  ChildProcessClass :TChildProcessClass = nil;


implementation

{ TChildProcess }

procedure TChildProcess.CloseStreams;
begin
  FreeAndNil(FInputStream);
  FreeAndNil(FOutputStream);
end;

constructor TChildProcess.Create(RedirectInput :boolean);
begin
  inherited Create;
  FRedirectedInput := RedirectInput;
end;


destructor TChildProcess.Destroy;
begin
  CloseStreams;
  inherited;
end;


function TChildProcess.EOF: boolean;
begin
  Result := (Output.Handle = 0)
end;

procedure TChildProcess.Error(Msg: string);
begin
  raise EChildProcessException.Create(Msg);
end;

function TChildProcess.ReadLine: string;
var
  c :Char;
begin
  if FLastChar <> #0 then
    Result := FLastChar
  else
    Result := '';
  FLastChar := #0;
  while not EOF do
  begin
    if Output.Read(c, 1) = 1 then
    begin
      if not (c in [#13,#10]) then
        Result := Result+c
      else
      begin
        if (c = #13) and not EOF then
        begin
          Output.ReadBuffer(c, 1);
          if c <> #10 then
            FLastChar := c;
        end;
        break;
      end;
    end;
  end;
end;

{ TWin32ChildProcess }

destructor TWin32ChildProcess.Destroy;
begin
  inherited Destroy;
end;


procedure TWin32ChildProcess.CreatePipes;
var
  hCurrentProcess,
  hOutputReadTmp,
  hInputWriteTmp :THandle;
  sa            :TSecurityAttributes;
begin
  hCurrentProcess := GetCurrentProcess;
  FillChar(sa, SizeOf(sa), 0);
  sa.nLength  := SizeOf(sa);
  sa.bInheritHandle := true;

  hInputWriteTmp := 0;


  // Create the child output pipe.
  CreatePipe(hOutputReadTmp, hOutputWrite, sa, 0);

  // Create child input handle
  if RedirectedInput then
  begin
    // Create the child input pipe.
    CreatePipe(hInputRead, hInputWriteTmp, sa, 0);
    DuplicateHandle( hCurrentProcess,
                     hInputWriteTmp,
                     hCurrentProcess,
                     hInputWrite, // Address of new handle.
                     0, false, // Make it uninheritable.
                     DUPLICATE_SAME_ACCESS);
    CloseHandle(hInputWriteTmp);
  end
  else if not IsConsole then
    hInputRead := 0
  else
  begin
    DuplicateHandle( hCurrentProcess,
                     GetStdHandle(STD_INPUT_HANDLE),
                     hCurrentProcess,
                     hInputRead,
                     0, true,
                     DUPLICATE_SAME_ACCESS);
  end;

  // Create a duplicate of the output write handle for the std error
  // write handle. This is necessary in case the child application
  // closes one of its std output handles.
  DuplicateHandle( hCurrentProcess,
                   hOutputWrite,
                   hCurrentProcess,
                   hErrorWrite,
                   0, true,
                   DUPLICATE_SAME_ACCESS);

  // Create new output read handle. Set
  // the Properties to FALSE. Otherwise, the child inherits the
  // properties and, as a result, non-closeable handles to the pipes
  // are created.
  DuplicateHandle( hCurrentProcess,
                   hOutputReadTmp,
                   hCurrentProcess,
                   hOutputRead, // Address of new handle.
                   0, false,    // Make it uninheritable.
                   DUPLICATE_SAME_ACCESS);

  // Close inheritable copies of the handles you do not want to be
  // inherited.
  CloseHandle(hOutputReadTmp);
end;

procedure TWin32ChildProcess.CloseStreams;
begin
  inherited;
  if (hOutputRead <> 0) then
    CloseHandle(hOutputRead);

  if (hInputWrite <> 0) then
    CloseHandle(hInputWrite);

  if (hChild <> 0) then
  begin
    TerminateProcess(hChild, Cardinal(-1));
    CloseHandle(hChild);
  end;
end;

procedure TWin32ChildProcess.Run(CmdLine: string);
begin
  try
    hChild := Launch(CmdLine, hInputRead, hOutputWrite, hOutputWrite);
  finally
    // Close pipe handles (do not continue to modify the parent).
    // You need to make sure that no handles to the write end of the
    // output pipe are maintained in this process or else the pipe will
    // not close when the child process exits and the ReadFile will hang.
    CloseHandle(hOutputWrite);

    if hInputRead <> 0 then
    begin
      CloseHandle(hInputRead);
    end;
    CloseHandle(hErrorWrite);
  end;
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

procedure TWin32ChildProcess.CreatePipe(var hReadPipe, hWritePipe: THandle; const PipeAttributes: TSecurityAttributes; nSize: DWORD);
var
  Success : boolean;
begin
  Success := Windows.CreatePipe(hREadPipe, hWritePipe, @PipeAttributes, nSize);
  if not Success then
  begin
    Error('DupliateHandle:' + SysErrorMessage(GetLastError));
  end;
end;

procedure TWin32ChildProcess.DuplicateHandle(hSourceProcessHandle,
  hSourceHandle, hTargetProcessHandle: THandle; var TargetHandle: THandle;
  dwDesiredAccess: DWORD; bInheritHandle: boolean; dwOptions: DWORD);
var
  Success : boolean;
begin
  Success := Windows.DuplicateHandle( hSourceProcessHandle,
                                      hSourceHandle,
                                      hTargetProcessHandle,
                                      @TargetHandle,
                                      dwDesiredAccess,
                                      bInheritHandle,
                                      dwOptions);
  if not Success then
  begin
     Error('CreatePipe:' + SysErrorMessage(GetLastError));
  end;
end;

procedure TWin32ChildProcess.CloseHandle(Handle: THandle);
var
  Success : boolean;
begin
  Success := Windows.CloseHandle(Handle);
  if not Success then
  begin
     Error('CloseHandle:' + SysErrorMessage(GetLastError));
  end;
end;

constructor TWin32ChildProcess.Create(RedirectInput: Boolean);
begin
  inherited;
  CreatePipes;

  FInputStream  := TPipeStream.Create(hInputWrite);
  FOutputStream := TPipeStream.Create(hOutputRead);
end;

{ TPipeStream }

function TPipeStream.Read(var Buffer; Count: Integer): Longint;
var
  BytesRead     :DWORD;
begin
  Result := 0;
  if FHandle = 0 then
  begin
    EXIT;
  end;
  repeat
    if not ReadFile(FHandle, Buffer, Count, BytesRead, nil)
    or (BytesRead = 0) then
    begin
      if GetLastError <> ERROR_BROKEN_PIPE then
         raise EStreamError('ReadFile:' + SysErrorMessage(GetLastError)) // Something bad happened.
      else
      begin
         CloseHandle(FHandle);
         FHandle := 0;
         break; // pipe done - normal exit path.
      end;
    end
  until BytesRead > 0;
  Result := BytesRead;
end;

initialization
 ChildProcessClass := TWin32ChildProcess;
end.
