(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit RegexpElements;

interface
uses
  SysUtils,
  XPerlRE,
  WantClasses;

type
  TRegexpElement = class(TScriptElement)
  protected
    FProperty :string;
    FText     :string;
    FPattern  :string;
    FSubst    :string;
    FTrim     :boolean;

    function Substitute(Text, Pattern, Subst :string) :string;
  public
    procedure Init; override;
  published
    property _property :string   read FProperty write FProperty;
    property _text     :string   read FText     write FText;
    property pattern   :string   read FPattern  write FPattern;
    property subst     :string   read FSubst    write FSubst;
    property trim      :boolean  read FTrim     write FTrim;
  end;

implementation

{ TRegexpElement }

procedure TRegexpElement.Init;
begin
  inherited Init;
  RequireAttribute('property');
  RequireAttribute('pattern');

  // this task sets a property
  // that needs to be implemented here, in the Init method.
  if subst = '' then
    Owner.SetProperty(_property, Substitute(_text, '.*('+pattern+').*', '\1'))
  else
    Owner.SetProperty(_property, Substitute(_text, pattern, subst));
end;

function TRegexpElement.Substitute(Text, Pattern, Subst: string): string;
begin
  Log(vlVerbose, 'Replacing /%s/ with /%s/', [Pattern, Subst]);
  Result := XPerlRe.Replace(Pattern, Subst, Text, True);
end;

initialization
  RegisterElement(TRegexpElement);
end.
