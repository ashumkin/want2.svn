(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit TimeElements;

interface
uses
  SysUtils,
  WantClasses;

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
