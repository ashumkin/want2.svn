(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit Properties;

interface
uses
  WantClasses;

type
  TPropertyElement = class(TScriptElement)
  protected
    FName: string;
    FValue: string;
  public
    procedure Init; override;
  published
    property name: string read FName  write FName;
    property value: string read FValue write FValue;
  end;

implementation

{ TPropertyElement }

procedure TPropertyElement.Init;
begin
  inherited Init;
  if Enabled then
  begin
    RequireAttribute('name');
    RequireAttribute('value');

    Assert(Owner <> nil);
    Owner.SetProperty(name, value);
  end;
end;


initialization
  RegisterElement(TPropertyElement);
end.
 