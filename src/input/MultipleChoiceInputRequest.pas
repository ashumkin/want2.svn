unit MultipleChoiceInputRequest;

interface

uses
  Classes, InputRequest;

type
  TMultipleChoiceInputRequest = class(TInputRequest)
  private
    FChoices: TStrings;
  public
    constructor Create(APrompt: string; AChoices: TStrings);
    destructor  Destroy; override;

    function isInputValid: boolean;  override;

    property Choices: TStrings read FChoices write FChoices;
  end;

implementation

uses
  SysUtils;
{ TMultipleChoiceInputRequest }

constructor TMultipleChoiceInputRequest.Create(APrompt: string;
  AChoices: TStrings);
begin
  if Assigned(AChoices) then if AChoices.Count = 0 then
      raise Exception.Create('choices must not be null');
  FPrompt  := APrompt;
  FChoices := TStringList.Create;
  FChoices.Assign(aChoices);
end;

destructor TMultipleChoiceInputRequest.Destroy;
begin
  FChoices.Free;
  inherited;
end;

function TMultipleChoiceInputRequest.isInputValid: boolean;
var
  i: integer;
begin
  Result := False;
  // fChoices.IndexOf is case insensitive!!!
  for i := 0 to fChoices.Count - 1 do
  begin
    if CompareStr(FInput, FChoices[i]) = 0 then
    begin
      Result := True;
      break;
    end;
  end;
end;

end.
