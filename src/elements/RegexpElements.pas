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
  UPerlRE,
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
var
  Regexp      :TPerlRE;
  p           :Integer;
  i           :Integer;
  Matched     :TSubExp;
  Replacement :string;
begin
  Result := Text;
  Regexp := TPerlRE.Create(True);
  try
    Regexp.Text   := Text;
    Regexp.RegExp := Pattern;

    while Regexp.Match do
    begin
      Matched := Regexp.SubExp[0];
      Replacement := '';
      p := 1;
      while p <= Length(Subst) do
      begin
        if Subst[p] <> '\' then
          Replacement := Replacement + Subst[p]
        else begin
          Inc(p);
          if not (Subst[p] in ['0'..'9']) then
            Replacement := Replacement + Subst[p]
          else
          begin
            i := StrToInt(Subst[p]);
            if i < Regexp.SubExpCount then
              Replacement := Replacement + Regexp.SubExp[i].Text;
          end;
        end;
        Inc(p);
      end;
      System.Delete(Result, Matched.StartP, Matched.Len);
      System.Insert(Replacement, Result, Matched.StartP);
    end;
  finally
    FreeAndNil(Regexp);
  end;
end;

initialization
  RegisterElement(TRegexpElement);
end.
