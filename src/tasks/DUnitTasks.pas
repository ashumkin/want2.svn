(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit DUnitTasks;

interface
uses
  SysUtils,
  Classes,

  JclSysUtils,

  WildPaths,
  WantClasses,
  TestFramework,
  TestModules;

type
  TDUnitTask = class(TTask, ITestListener)
  // implement IInterface
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  // implement the ITestListener interface
  protected
    procedure AddSuccess(test: ITest); virtual;
    procedure AddError(error: TTestFailure); virtual;
    procedure AddFailure(failure: TTestFailure); virtual;
    procedure StartTest(test: ITest); virtual;
    procedure EndTest(test: ITest); virtual;
    procedure TestingStarts; virtual;
    procedure TestingEnds(testResult: TTestResult); virtual;

 // implement the task
  protected
    FTestLib :TPath;
  public
    procedure Init; override;
    procedure Execute; override;
  published
    property basedir;
    property dir: TPath     read GetBaseDir  write SetBaseDir;
    property testlib :TPath read FTestLib write FTestLib;
  end;

implementation

{ TDUnitTask }

function TDUnitTask.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = $80004002;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := HResult(E_NOINTERFACE);
end;

function TDUnitTask._AddRef: Integer;
begin
  Result := MaxInt;
end;

function TDUnitTask._Release: Integer;
begin
  Result := MaxInt;
end;

procedure TDUnitTask.AddError(error: TTestFailure);
begin
  Log(vlErrors,  '! %s: "%s" at %s', [ error.FailedTest.Name,
                                       error.ThrownExceptionMessage,
                                       PointerToLocationInfo(error.ThrownExceptionAddress)
                                       ]);
end;

procedure TDUnitTask.AddFailure(failure: TTestFailure);
begin
  Log(vlWarnings, '- %s: "%s" at %s', [ failure.FailedTest.Name,
                                        failure.ThrownExceptionMessage,
                                        PointerToLocationInfo(failure.ThrownExceptionAddress)
                                        //PointerToAddressInfo(failure.ThrownExceptionAddress)
                                        ]);
end;

procedure TDUnitTask.AddSuccess(test: ITest);
begin
  Log(vlVerbose, '+ ' + test.Name);
end;

procedure TDUnitTask.StartTest(test: ITest);
begin
  Log(vlDebug, '[ %s', [test.Name]);
end;

procedure TDUnitTask.EndTest(test: ITest);
begin
  Log(vlDebug, '] %s', [test.Name]);
end;

procedure TDUnitTask.TestingStarts;
begin
  Log(vlVerbose, 'start');
end;

procedure TDUnitTask.TestingEnds(testResult: TTestResult);
begin
  Log(vlVerbose, 'done');
  if testResult.WasSuccessful then
    Log(vlVerbose, '%4d Tests OK', [testResult.RunCount])
  else
  begin
    Log(vlVerbose, '%4d Run', [testResult.RunCount]);
    if testResult.FailureCount > 0 then
      Log(vlWarnings, '%4d Failures', [testResult.FailureCount]);
    if testResult.ErrorCount > 0 then
      Log(vlErrors, '%4d Errors',   [testResult.ErrorCount]);
  end;
end;

procedure TDUnitTask.Init;
begin
  inherited Init;
  RequireAttribute('testlib');
end;

procedure TDUnitTask.Execute;
var
  Test :ITest;
begin
  inherited Execute;
  Log(ToRelativePath(testlib));
  try
    Test := LoadModuleTests(ToSystemPath(testlib));
    try
      if not TestFramework.RunTest(Test, [Self]).WasSuccessful then
        TaskFailure('');
    finally
      Test := nil;
      UnloadTestModules;
    end;
  except
    on e :EWantException do
      raise;
    on e :Exception do
      TaskError(e.Message, ExceptAddr);
  end;
end;

initialization
  RegisterTask(TDUnitTask);
end.
