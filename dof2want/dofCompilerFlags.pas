unit dofCompilerFlags;
{
Unit        : dofCompilerFlags

Description : extracts information from the compiler flags

Programmer  : mike

Date        : 10-Dec-2002
}

interface

uses
  classes,
  dccflags,
  dofFlagExtractor,
  dofReader;
  
type

 TCompilerFlag = record
   flagSymbol : string;
   flagDefault : boolean;
 end;

const

  DCCCompilerFlags : array[TVerboseFlagEnum] of string =
  (
  ('O'), {vfOptimization    }
  ('W'), {vfStackFrames     }
  ('U'), {vfSaveDivide	    }
  ('R'), {vfRangeChecks     }
  ('I'), {vfIOChecks        }
  ('Q'), {vfOverflowchecks  }
  ('V'), {vfVarStringChecks }
  ('B'), {vfBoolEval	       }
  ('X'), {vfExtendedSyntax  }
  ('T'), {vfTypedAddress    }
  ('P'), {vfOpenStrings     }
  ('H'), {vfLongStrings     }
  ('J'), {vfWriteableConst  }
  ('D'), {vfDebugInfo       }
  ('L'), {vfLocalSymbols    }
  ('U'), {vfDefinitionInfo  }
  ('C')  {vfAssertions      }
  );
     
type                      
  TCompilerFlagExtractor = class(TDOFFlagExtractor)
  public
    procedure ExtractValues; override;
  end;

implementation

uses
  typ_dofReader,
  sysUtils;
  
const
  dof_ShowHints    = 'ShowHints';
  dof_ShowWarnings = 'ShowWarnings';
  dcc_warnings     = 'warnings';
  
procedure TCompilerFlagExtractor.ExtractValues;
const
  BoolToStr : array[boolean] of string =
  (
   'False',
   'True'
  );
var
 sectionFlag : TVerboseFlagEnum;
 flagValue : string;
 flagState : boolean;
begin
  fvalues.clear;
  
  freader.DofSection := tsCompiler;
  for sectionFlag := low(TVerboseFlagEnum) to high(TVerboseFlagEnum) do
    begin
      flagValue := freader.sectionValues.Values[DCCCompilerFlags[sectionFlag]];
      flagState := Boolean(StrToInt(flagValue));
      if flagState <> VerboseFlagDescription[sectionFlag].flagDefault then
        begin
          try
          fvalues.Values[VerboseFlagDescription[sectionFlag].flagDescription] := BoolToStr[flagState];
          except
          end;
        end;
    end;
  if fvalues.Count > 0 then
    begin
      fvalues.Values['UseVerbose'] := BoolToStr[True];
    end;  
  try
    flagValue := freader.sectionValues.Values[dof_ShowWarnings];       
    flagState := Boolean(StrToInt(flagValue));
    fvalues.Values[dcc_warnings] := BoolToStr[flagState];
  except
  end;  
end;

end.
