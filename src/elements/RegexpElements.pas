{ $Id$ }
{
--------------------------------------------------------------------------------
Copyright (c) 2001, Dante Authors -- See authors.txt for complete list
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The name Dante, the names of the authors in authors.txt and the names of
other contributors to this software may not be used to endorse or promote
products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------
Original Author: Juancarlo Añez
Contributors   : 
}
unit RegexpElements;

interface
uses
  SysUtils,
  UPerlRE,
  DanteClasses;

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
