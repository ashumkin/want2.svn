(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

{ Original Author: Juancarlo Añez
  Contributors   :
  Chris Morris
  Mark Watts 
}
unit ExecTasks;

interface
uses
  Windows,
  SysUtils,
  Math,
  Classes,

  JclBase,
  JclMiscel,
  JclSysInfo,
  JclSecurity,

  WildPaths,
  WantClasses,
  Attributes;



type
  TChildProcess = class;

  TArgElement = class(TScriptElement)
  protected
    FVAlue :string;
  public
    procedure Init; override;
  published
    property value :string read FValue write FValue;
  end;

  TPathElement = class(TArgElement)
  public
    procedure Init; override;
  end;

  TCustomExecTask = class(TTask)
  protected
    FOS          :string;
    FExecutable  :string;
    FArguments   :TStrings;
    FSkipLines   :Integer;
    FFailOnError :boolean;
    FTimeOut     :Longint;
    FOutput      :string;

    function BuildExecutable :string; virtual;
    function BuildArguments  :string; virtual;
    function BuildCmdLine    :string; virtual;

    function  GetArguments :string;
    procedure SetArguments(Value :string);
    procedure SetArgumentList(Value :TStrings);

    procedure Run(CmdLine: string);
    procedure HandleOutput(Child :TChildProcess);
  public
    constructor Create(Owner: TScriptElement);  override;
    destructor Destroy; override;

    procedure Execute; override;

  protected
    property Arguments:    string   read GetArguments write SetArguments;
    property ArgumentList: TStrings read FArguments   write SetArgumentList stored False;
    property Executable:   string   read FExecutable  write FExecutable;
    property SkipLines:    Integer  read FSkipLines   write FSkipLines;
    property OS:           string   read FOS          write FOS;
    property failonerror:  boolean  read FFailOnError write FFailOnError default True;
    property output:       string   read FOutput      write FOutput;
    {:@TODO Implement a TWaitableTimer class to implement timeouts.
      Use Windows.CreateWaitableTimer and Windows.SetWaitableTimer.
    }
    property timeout:      Longint  read FTimeout     write FTimeout;
  published
    function CreateArg  :TArgElement;
    function CreatePath :TPathElement;
  end;

  TExecTask = class(TCustomExecTask)
  public
    procedure Init; override;
    procedure Execute; override;
  published
    property Arguments;
    property ArgumentList stored False;
    property Executable;
    property SkipLines :Integer     read FSkipLines   write FSkipLines;
    property OS;
    property failonerror;
    property output;
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
  Log(ToRelativePath(Executable));
  Log(vlVerbose, BuildArguments);
  inherited Execute;
end;

procedure TExecTask.Init;
begin
  inherited Init;
  RequireAttribute('executable');
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
  Result := WildPaths.ToSystemPath(Executable);
end;

function TCustomExecTask.BuildCmdLine: string;
begin
  Result := Trim(BuildExecutable + ' ' + BuildArguments);
end;

function TCustomExecTask.BuildArguments: string;
var
  i: Integer;
begin
  Result := '';
  { Arguments.CommaText screws with the contents. See unit test }
  for i := 0 to ArgumentList.Count - 1 do
    Result := Result + ' ' + ArgumentList[i];
  Result := Trim(Result);
end;

constructor TCustomExecTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FArguments   := TStringList.Create;
  FFailOnError := True;
end;

destructor TCustomExecTask.Destroy;
begin
  FreeAndNil(FArguments);
  inherited;
end;

procedure TCustomExecTask.Execute;
var
  CmdLine :string;
begin
  Log(vlDebug, 'currentDir=%s', [CurrentDir] );
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
    if (Child.ExitCode <> 0) and FailOnError then
    begin
      Log(vlVerbose, 'Exit code not zero');
      TaskFailure('failed');
    end;
  finally
    FreeAndNil(Child);
  end;
end;


procedure TCustomExecTask.HandleOutput(Child :TChildProcess);
const
  TimeOutMillis = 100;
var
  Line          :String;
  LineNo        :Integer;
  OutFile       :Text;
begin
  LineNo := 0;

  if output <> '' then
  begin
    Log(vlVerbose, 'output to "%s"', [output]);
    Assign(OutFile, ToSystemPath(output));
    Rewrite(OutFile);
  end;

  try
    while not Child.EOF
    and (LineNo < SkipLines) do
    begin
      Child.ReadLn;
      Inc(LineNo);
    end;

    while not Child.EOF do
    begin
      Line := Child.ReadLn;
      //!!! Inc(LineNo); // never used
      if output <> '' then
        Writeln(OutFile, Line)
      else
        Log(Line);
    end;
  finally
    if output <> '' then
      Close(OutFile);
  end;
end;


function TCustomExecTask.CreateArg: TArgElement;
begin
  Result := TArgElement.Create(Self);
end;

function TCustomExecTask.CreatePath: TPathElement;
begin
  Result := TPathElement.Create(Self);
end;

{ TChildProcess }

destructor TChildProcess.Destroy;
begin
  if (hOutputRead <> 0)
  and not CloseHandle(hOutputRead) then
    RaiseLastSystemError('CloseHandle');
  if (hChild <> 0) then
  begin
    TerminateProcess(hChild, Cardinal(-1));
    if not CloseHandle(hChild) then
      RaiseLastSystemError('CloseHandle');
  end;
  inherited Destroy;
end;


function TChildProcess.EOF: boolean;
begin
  Result := (hOutputRead = 0) and (FLine = '');
end;

function TChildProcess.ExitCode: Cardinal;
begin
  if (WaitForSingleObject(hChild, INFINITE) <> WAIT_OBJECT_0) 
  or not GetExitCodeProcess(hChild, Result) then
      RaiseLastSystemError;
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
  StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOW;

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
  if not IsConsole then
    hInputRead := 0
  else if not DuplicateHandle(hCurrentProcess, GetStdHandle(STD_INPUT_HANDLE),
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
  if hInputRead <> 0 then
  begin
    if not CloseHandle(hInputRead )  then RaiseLastSystemError('CloseHandle');
  end;
  if not CloseHandle(hErrorWrite)  then RaiseLastSystemError('CloseHandle');
end;


{ TArgElement }

procedure TArgElement.Init;
begin
  inherited Init;
  RequireAttribute('value');
  (Owner as TCustomExecTask).FArguments.Add(Value);
end;

{ TPathElement }

procedure TPathElement.Init;
begin
  RequireAttribute('value');
  value := ToSystemPath(value);
  inherited Init;
end;

initialization
  RegisterTasks([TCustomExecTask, TExecTask, TShellTask]);
end.

