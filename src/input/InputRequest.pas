unit InputRequest;

interface


type
  TInputRequest = class(TObject)
  protected
    FPrompt: string;
    FInput:  string;
  public
    constructor  Create(APrompt: string);
    function     isInputValid: boolean; virtual;

    property Prompt: string read FPrompt;
    property Input:  string read FInput   write FInput;

  end;

implementation

uses
  SysUtils;
{ TInputRequest }

constructor TInputRequest.Create(APrompt: string);
begin
  FPrompt := APrompt;
end;

function TInputRequest.isInputValid: boolean;
begin
  Result := True;
end;

end.
