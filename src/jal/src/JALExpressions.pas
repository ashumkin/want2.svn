{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca ®                                 }
{    ~                                         }
{  Copyright © 1995-2002 Juancarlo Añez        }
{  http://www.suigeneris.org/juanca            }
{  All rights reserved.                        }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{#(@)$Id$}

unit JALExpressions;

interface
uses
  SysUtils,
  Math,

  JALMath,
  JALParse;

type
  EExpressionError = class(Exception);
  ExpressionValue  = Extended;

  TExpressionParser = class(TParser)
  protected
    function expression  : ExpressionValue;
    function term        : ExpressionValue;
    function factor      : ExpressionValue;
    function signedValue : ExpressionValue;
    function value       : ExpressionValue;
    function simpleValue : ExpressionValue;
    function exponent    : ExpressionValue;
    function functionEval: ExpressionValue;
  public
    function evaluate(expre :WideString): ExpressionValue;
  end;

  function evaluate(expre :WideString): ExpressionValue;

implementation

var
  __parser :TExpressionParser = nil;

function evaluate(expre :WideString): ExpressionValue;
begin
  if __parser = nil then
    __parser := TExpressionParser.Create;
  Result := __parser.evaluate(expre);
end;


{ TExpressionParser }

function TExpressionParser.evaluate(expre: WideString): ExpressionValue;
begin
  setText(expre);
  try
    Result := expression;
  except
    on e :Exception do
      raise EExpressionError.CreateFmt('at %d: %s', [getColumnNumber, e.Message]);
  end;
  if currentChar <> eofch then
    raise EExpressionError.CreateFmt('unexpected character in expression at pos %d: %s',
                                     [getColumnNumber, currentChar]
                                     );
end;

function TExpressionParser.expression: ExpressionValue;
begin
  Result := term;
  repeat
    skipSpaces;
    case currentChar of
      '+'   : begin skip; Result := Result + term; end;
      '-'   : begin skip; Result := Result - term; end;
    else
      break;
    end;
  until false;
end;

function TExpressionParser.term: ExpressionValue;
begin
  Result := factor;
  repeat
    skipSpaces;
    case currentChar of
      '*' : begin skip; Result := Result * factor; end;
      '/' : begin skip; Result := Result / factor; end;
    else
      break;
    end;
  until false;
end;

function TExpressionParser.factor: ExpressionValue;
begin
  Result := signedValue;
  repeat
    skipSpaces;
    case currentChar of
      '^' : begin skip; Result := Power(Result, signedValue); end;
    else
      break;
    end;
  until false;
end;

function TExpressionParser.signedValue: ExpressionValue;
var
  negate :boolean;
begin
  skipSpaces;
  negate := false;
  while (currentChar = '+') or (currentChar = '-') do
  begin
    if currentChar = '-' then
      negate := not negate;
    skip;
    skipSpaces;
  end;
  Result := value;
  if negate then
    Result := -Result;
end;

function TExpressionParser.value: ExpressionValue;
begin
  skipSpaces;
  if isNameStartChar(currentChar) then
    Result := functionEval
  else if currentChar = '(' then
  begin
    skip;
    Result := expression;
    skipSpaces;
    check(')');
  end
  else
  begin
    Result := simpleValue;
    if (currentChar = 'e') or (currentChar = 'E') then
    begin
      skip;
      Result := Result * power(10, exponent);
    end;
  end;
end;

function TExpressionParser.simpleValue: ExpressionValue;
var
  num :WideString;
begin
  num := number;
  if currentChar = '.' then
  begin
    skip;
    num := num + '.' + number;
  end;
  Result := StrToFloat(num);
end;

function TExpressionParser.exponent: ExpressionValue;
var
  negate : boolean;
begin
  negate := false;
  while (currentChar = '+') or (currentChar = '-') do
  begin
    if currentChar = '-' then
      negate := true;
    skip;
  end;
  Result := simpleValue;
  if negate then
    Result := -Result;
end;



function TExpressionParser.functionEval: ExpressionValue;
var
  fn    :WideString;
  arg   :ExpressionValue;
begin
  fn := name;
  arg := value;
  if fn = 'ln' then
    Result := ln(arg)
  else if fn = 'exp' then
    Result := exp(arg)
  else
    raise EExpressionError.CreateFmt('unknown function at pos %d: %s',
                                     [getColumnNumber, fn]
                                     );
end;


initialization
finalization
  __parser.Free;
end.
