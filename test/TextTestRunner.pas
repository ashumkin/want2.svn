{ $Id: TextTestRunner.pas,v 1.10 2000/12/20 21:14:05 juanco Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.10 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo A�ez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco A�ez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
unit TextTestRunner;

interface
uses
   TestFramework;

type
  TTextTestListener = class(TInterfacedObject, ITestListener)
  protected
      startTime,
      endTime   :TDateTime;
      runTime   :TDateTime;
  public
      // implement the ITestListener interface
      procedure AddError(  error: TTestFailure);    virtual;
      procedure AddFailure(failure: TTestFailure);  virtual;
      procedure StartTest( test :ITest);            virtual;
      procedure EndTest(   test :ITest);            virtual;
      procedure TestingStarts;                         virtual;
      procedure TestingEnds(testResult :TTestResult); virtual;
      function  Report(  r :TTestResult) :string;

  protected
      function  PrintErrors   (r :TTestResult):string;  virtual;
      function  PrintFailures (r :TTestResult):string;  virtual;
      function  PrintHeader   (r :TTestResult):string;  virtual;
      function  TruncateString(s :string; len :integer) :string; virtual;
  end;

  {: This type defines what the RunTest and RunRegisteredTests methods will do when
     testing has ended.
     @enum rxbContinue Just return the TestResult.
     @enum rxbPause    Pause with a ReadLn before returnng the TestResult.
     @enum rxbHaltOnFailures   Halt the program if errors or failures occurred, setting
                               the program exit code to FailureCount+ErrorCount;
                               behave like rxbContinue if all tests suceeded.
     @seeAlso <See Unit="TextTestRunner" Routine="RunTest">
     @seeAlso <See Unit="TextTestRunner" Routine="RunRegisteredTests">
     }
  TRunnerExitBehavior = (
    rxbContinue,
    rxbPause,
    rxbHaltOnFailures
    );

{: Run the given test suite
}
function RunTest(suite :ITest; exitBehavior :TRunnerExitBehavior = rxbContinue) :TTestResult; overload;
function RunTest(aclass :TTestCaseClass; exitBehavior :TRunnerExitBehavior = rxbContinue) :TTestResult; overload;
function RunRegisteredTests(exitBehavior :TRunnerExitBehavior = rxbContinue) :TTestResult; overload;

implementation
uses
   SysUtils;

const
  CRLF = #13#10;

{ TTExtTestListener }

procedure TTextTestListener.AddError( error :TTestFailure);
begin
    write('E');
end;

procedure TTextTestListener.AddFailure(failure :TTestFailure);
begin
    write('F');
end;

{:
   Prints failures to the standard output
 }
function TTextTestListener.Report(r :TTestResult) :string;
begin
    result := PrintHeader(r) +
              PrintErrors(r) +
              PrintFailures(r);
end;

{:
   Prints the errors to the standard output
 }
function TTextTestListener.PrintErrors(r :TTestResult) :string;
var
  i       :integer;
  error   :TTestFailure;
begin
    result := '';
    if (r.errorCount <> 0) then begin
        if (r.errorCount = 1) then
            result := result + format('There was %d error:', [r.errorCount]) + CRLF
        else
            result := result + format('There were %d errors:', [r.errorCount]) + CRLF;

        for i := 0 to r.errors.Count-1 do begin
            error :=  TObject(r.errors[i]) as TTestFailure;
            result := result + format('%d) %s: %s: %s', [
                                       i+1,
                                       error.failedTest.name,
                                       error.thrownExceptionName,
                                       error.thrownExceptionMessage
                                       ]) + CRLF;
        end;
        result := result + CRLF
    end
end;

{:
   Prints failures to the standard output
 }
function TTextTestListener.PrintFailures(r :TTestResult): string;
var
  i       :integer;
  failure :TTestFailure;
begin
    result := '';
    if (r.failureCount <> 0) then begin
        if (r.failureCount = 1) then
            result := result + format('There was %d failure:', [r.failureCount]) + CRLF
        else
            result := result + format('There were %d failures:', [r.failureCount]) + CRLF;

        for i := 0 to r.failures.Count-1 do begin
            failure := TObject(r.failures[i]) as TTestFailure;
            result := result + format('%d) %s: %s: %s', [
                                       i+1,
                                       failure.failedTest.name,
                                       failure.thrownExceptionName,
                                       failure.thrownExceptionMessage
                                       ]) + CRLF;
        end;
        result := result + CRLF
    end
end;

{:
   Prints the header of the Report
 }
function TTextTestListener.PrintHeader(r :TTestResult):string;
begin
  result := '';
  if r.wasSuccessful then begin
      result := result + CRLF;
      result := result + format('OK: %d tests'+CRLF, [r.runCount]);
  end
  else begin
      result := result + CRLF;
      result := result + 'FAILURES!!!'+CRLF;
      result := result + 'Test Results:'+CRLF;
      result := result + format('Run:      %8d'+CRLF+'Failures: %8d'+CRLF+'Errors:   %8d'+CRLF,
                        [r.runCount, r.failureCount, r.errorCount]
                        );
  end
end;

procedure TTextTestListener.StartTest(test :ITest);
begin
    write('.');
end;

procedure TTextTestListener.EndTest(test: ITest);
begin

end;

function TTextTestListener.TruncateString(s :string; len :integer) :string;
begin
    if Length(s) > len then
        result := copy(s, 1, len) + '...'
    else
        result := s
end;

procedure TTextTestListener.TestingStarts;
begin
  writeln;
  writeln('DUnit / Testing');
  startTime := now;
end;

procedure TTextTestListener.TestingEnds(testResult :TTestResult);
var
  h, m, s, l :Word;
begin
  endTime   := now;
  runTime   := endTime-startTime;
  writeln;
  DecodeTime(runTime, h,  m, s, l);
  writeln(Format('Time: %d:%2.2d:%2.2d.%d', [h, m, s, l]));
  writeln(Report(testResult));
  writeln;
end;

function RunTest(suite :ITest; exitBehavior :TRunnerExitBehavior = rxbContinue) :TTestResult;
begin
  Result := RunTest(suite, [TTextTestListener.Create]);
  case exitBehavior of
    rxbPause:
      try
        writeln('Press <RETURN> to continue.');
        readln
      except
      end;
    rxbHaltOnFailures:
      with Result do
      begin
        if not WasSuccessful then
          System.Halt(ErrorCount+FailureCount);
      end
    // else fall through
  end;
end;

function RunTest(aclass :TTestCaseClass; exitBehavior :TRunnerExitBehavior = rxbContinue) :TTestResult;
begin
   Result := RunTest(aclass, exitBehavior);
end;

function RunRegisteredTests(exitBehavior :TRunnerExitBehavior = rxbContinue) :TTestResult;
begin
   Result := RunTest(registeredTests, exitBehavior);
end;

end.
