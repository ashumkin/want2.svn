unit dofLinkerFlags;
{
Unit        : dofLinkerFlags

Description : gets the linker flags from the dof reader

Programmer  : mike

Date        : 11-Dec-2002
}

interface

uses
  dofFlagExtractor;
type

  TLinkerFlagExtractor = class(TDOFFlagExtractor)
  public
    procedure ExtractValues; override;
  end;

implementation

uses
  typ_dofReader,
  sysUtils;
  
procedure TLinkerFlagExtractor.ExtractValues;

const
  BoolToStr : array[boolean] of string =
  (
   'False',
   'True'
  );
  dof_MapFile = 'MapFile';
  dof_ConsoleApp = 'ConsoleApp';
  want_mapElement = 'map';
  want_consoleAppElement = 'console';
var
 flagValue : string;
 flagState : boolean;
 flagInt : integer;
begin
  freader.DofSection := tsLinker;
  try
    flagValue := freader.sectionValues.Values[dof_MapFile];       
    flagInt := StrToInt(flagValue);
    case flagInt of
      0 : fvalues.Values[want_mapElement] := 'none';
      1 : fvalues.Values[want_mapElement] := 'segments';
      2 : fvalues.Values[want_mapElement] := 'publics';
      3 : fvalues.Values[want_mapElement] := 'detailed';
    end;    
  except
  end;  
  try
   flagValue := freader.sectionValues.values[dof_ConsoleApp];
   flagState := Boolean(StrToInt(flagValue));
   fvalues.Values[want_consoleAppElement] := BoolToStr[flagState];
  except
  end;
end;

end.






