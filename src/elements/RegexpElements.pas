(****************************************************************************
 * WANT - A build management tool.                                          *
 * Copyright (c) 2001-2003 Juancarlo Anez, Caracas, Venezuela.              *
 * All rights reserved.                                                     *
 *                                                                          *
 * This library is free software; you can redistribute it and/or            *
 * modify it under the terms of the GNU Lesser General Public               *
 * License as published by the Free Software Foundation; either             *
 * version 2.1 of the License, or (at your option) any later version.       *
 *                                                                          *
 * This library is distributed in the hope that it will be useful,          *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *
 * Lesser General Public License for more details.                          *
 *                                                                          *
 * You should have received a copy of the GNU Lesser General Public         *
 * License along with this library; if not, write to the Free Software      *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA *
 ****************************************************************************)
{
    @brief 

    @author Juancarlo Añez
}

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

    function Substitute(Pattern, Subst, Text :string) :string;
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
  if not HasAttribute('subst') then
    Owner.SetProperty(_property, Substitute('.*('+pattern+').*', '\1', _text))
  else
    Owner.SetProperty(_property, Substitute(pattern, subst, _text));
end;

function TRegexpElement.Substitute(Pattern, Subst, Text : string): string;
begin
  Log(vlDebug, 'Replacing /%s/ with /%s/', [Pattern, Subst]);
  Result := XPerlRe.Replace(Pattern, Subst, Text, True);
end;

initialization
  RegisterElement(TRegexpElement);
end.
