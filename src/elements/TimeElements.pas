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
unit TimeElements;

interface
uses
  SysUtils,
  DanteClasses;

type
  TFormatElement = class(TScriptElement)
  protected
    FProperty :string;
    FPattern  :string;
  public
    procedure Init; override;
  published
    property _property :string read FProperty write FProperty;
    property pattern   :string read FPattern  write FPattern;
  end;

  TTStampElement = class(TScriptElement)
  protected
    FTime :TDateTime;
  public
    procedure Init; override;

    property Time :TDateTime read FTime;
  end;


implementation

{ TTStampElement }


procedure TTStampElement.Init;
begin
  inherited Init;

  FTime := Now;
  Owner.SetProperty('dstamp', FormatDateTime('yyyymmdd',       Time));
  Owner.SetProperty('tstamp', FormatDateTime('hhnn',           Time));
  Owner.SetProperty('today',  FormatDateTime('mmm ddd d yyyy', Time));

  Owner.SetProperty('year',   FormatDateTime('yyyy',     Time));
  Owner.SetProperty('month',  FormatDateTime('mm',       Time));
  Owner.SetProperty('day',    FormatDateTime('dd',       Time));

  Owner.SetProperty('hour',   FormatDateTime('hh',       Time));
  Owner.SetProperty('minute', FormatDateTime('nn',       Time));
  Owner.SetProperty('second', FormatDateTime('ss',       Time));

  Owner.SetProperty('ticks',  Format('%8.8d', [Round(24*60*60*1000*Frac(Time))]));
end;

{ TFormatElement }

procedure TFormatElement.Init;
begin
  inherited Init;
  RequireAttributes(['property', 'pattern']);

  with Owner as TTStampElement do
    Owner.SetProperty(_property, FormatDateTime(pattern, Time));
end;

initialization
  RegisterElement(TTStampElement);
  RegisterElement(TTStampElement, TFormatElement);
end.
