unit InputTasks;

interface

uses
  WantClasses,
  Classes;

type
  TInputTask = class(TTask)
  private
    FValidArgs:   string;
    FMessage:     string;
    FAddProperty: string;
  public
    procedure Execute; override;
  published
    property _message:    string read FMessage     write FMessage;
    property validargs:   string read FValidArgs   write FValidArgs;
    property addproperty: string read FAddProperty write FAddProperty;
  end;

implementation

uses
  InputRequest, MultipleChoiceInputRequest;

{ TInputTask }

procedure TInputTask.Execute;
var
  bInputRequest: TInputRequest;
  bArgs:         TStringList;
  bValue:        string;
begin
  inherited;
  if GetAttribute('validargs') <> '' then
  begin
    bArgs := TStringList.Create;
    try
      bArgs.Delimiter := ',';
      bArgs.DelimitedText := FValidArgs;
      bInputRequest := TMultipleChoiceInputRequest.Create(FMessage, bArgs);
    finally
      bArgs.Free;
    end;
  end
  else
  begin
    bInputRequest := TInputRequest.Create(FMessage);
  end;

  Project.InputHandler.handleInput(bInputRequest);

  bValue := bInputRequest.Input;
  if (FAddProperty <> '') and (bValue <> '') then
    Project.SetProperty(FAddProperty, bValue);
end;

initialization
  RegisterTask(TInputTask);
end.
