unit InputHandler;

interface

uses
  InputRequest;

type
    (**
     * Handle the request encapsulated in the argument.
     *
     * <p>Precondition: the request.getPrompt will return a non-null
     * value.</p>
     *
     * <p>Postcondition: request.getInput will return a non-null
     * value, request.isInputValid will return true.</p>
     *)
  IInputHandler = interface
    ['{E0EFF434-A33F-4CAF-88FE-94E74BE1EB2A}']
    procedure handleInput(aRequest: TInputRequest);
  end;

implementation

end.
