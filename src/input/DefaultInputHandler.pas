unit DefaultInputHandler;

interface

uses
  InputRequest, InputHandler;

type
  TDefaultInputHandler = class(TInterfacedObject, IInputHandler)
  protected
    function GetPrompt(aInputRequest: TInputRequest): string;
    function GetInputStream: string;
  public
    procedure handleInput(aRequest: TInputRequest);
  end;

implementation

uses
  SysUtils,
  MultipleChoiceInputRequest;

{ TDefaultInputHandler }

function TDefaultInputHandler.GetInputStream: string;
var
  istr: string;
begin
  readln(istr);
  Result := istr;
end;

function TDefaultInputHandler.GetPrompt(aInputRequest: TInputRequest): string;
var
  p: string;
  i: integer;
begin
  p := aInputRequest.Prompt;
  if aInputRequest is TMultipleChoiceInputRequest then 
  begin
    p := p + '(';
    for i := 0 to (aInputRequest as TMultipleChoiceInputRequest).Choices.Count - 1 do 
    begin
      p := p + (aInputRequest as TMultipleChoiceInputRequest).Choices[i];
      if i < (aInputRequest as TMultipleChoiceInputRequest).Choices.Count - 1 then
        p := p + ',';
    end;
    p := p + ')';
  end;
  Result := p;
end;

procedure TDefaultInputHandler.handleInput(aRequest: TInputRequest);
var
  p, inp: string;
begin
  p := GetPrompt(aRequest);
  repeat
    try
      writeln(ErrOutput, p);
      readln(inp);
      aRequest.Input := inp;
    except
      on E: Exception do 
      begin
        raise Exception.Create('Failed to read input from Console.'#13#10 + E.Message);
      end;
    end;
  until aRequest.isInputValid;
end;

end.
