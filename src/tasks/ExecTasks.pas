(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit ExecTasks;

interface
uses
  SysUtils,
  Math,
  Classes,

  JclBase,
  JclMiscel,
  JclSysInfo,
  JclSecurity,

  WildPaths,
  WantUtils,
  WantClasses,
  Attributes,
  ChildProcesses;



type
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
    FErrorLevel  :Integer;
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
    procedure HandleOutputLine(Line :string); virtual;
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
    property errorlevel:   Integer  read FErrorLevel  write FErrorLevel;
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
    property basedir;
    property Arguments;
    property ArgumentList stored False;
    property Executable;
    property SkipLines :Integer     read FSkipLines   write FSkipLines;
    property OS;
    property failonerror;
    property errorlevel;
    property output;
  end;

  // this class will pass commands through the command processor
  TShellTask = class(TExecTask)
  protected
    function BuildExecutable :string; override;
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
  Child    :TChildProcess;
  ExitCode :Integer;
begin
  if ChildProcessClass = nil then
    TaskError('No Child Process implementation?');

  Child := ChildProcessClass.Create;
  try
    try
      Child.Run(CmdLine);
    except
      on e :Exception do
        TaskFailure(e.Message, ExceptAddr);
    end;
    HandleOutput(Child);
    ExitCode := Child.ExitCode;
    if (ExitCode > errorlevel) and FailOnError then
      TaskFailure(Format('Exit code was %d', [ExitCode]));
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
        HandleOutputLine(Line);
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

procedure TCustomExecTask.HandleOutputLine(Line: string);
begin
  Log(Line);
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

