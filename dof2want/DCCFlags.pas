unit dccFlags;

interface

uses
  classes;

type
  TVerboseFlagEnum = (vfOptimization, vfStackFrames, vfSaveDivide,
    vfRangeChecks, vfIOChecks, vfOverflowchecks,
    vfVarStringChecks, vfBoolEval, vfExtendedSyntax,
    vfTypedAddress, vfOpenStrings, vfLongStrings,
    vfWriteableConst, vfDebugInfo, vfLocalSymbols,
    vfDefinitionInfo, vfAssertions);

  TVerboseFlagDescription = record
    flagSymbol: string;
    flagDefault: boolean;
    flagDescription: string;
  end;

const

  VerboseFlagDescription: array[TVerboseFlagEnum] of TVerboseFlagDescription =
  (
    (flagSymbol: '$O'; flagDefault: true; flagDescription: 'Optimization'), {vfOptimization    }
    (flagSymbol: '$W'; flagDefault: false; flagDescription: 'StackFrames'), {vfStackFrames     }
    (flagSymbol: '$U'; flagDefault: false; flagDescription: 'SaveDivide'), {vfSaveDivide	    }
    (flagSymbol: '$R'; flagDefault: false; flagDescription: 'RangeChecks'), {vfRangeChecks     }
    (flagSymbol: '$I'; flagDefault: true; flagDescription: 'IOChecks'), {vfIOChecks        }
    (flagSymbol: '$Q'; flagDefault: false; flagDescription: 'Overflowchecks'), {vfOverflowchecks  }
    (flagSymbol: '$V'; flagDefault: true; flagDescription: 'VarStringChecks'), {vfVarStringChecks }
    (flagSymbol: '$B'; flagDefault: false; flagDescription: 'BoolEval'), {vfBoolEval	       }
    (flagSymbol: '$X'; flagDefault: true; flagDescription: 'ExtendedSyntax'), {vfExtendedSyntax  }
    (flagSymbol: '$T'; flagDefault: false; flagDescription: 'TypedAddress'), {vfTypedAddress    }
    (flagSymbol: '$P'; flagDefault: true; flagDescription: 'OpenStrings'), {vfOpenStrings     }
    (flagSymbol: '$H'; flagDefault: true; flagDescription: 'LongStrings'), {vfLongStrings     }
    (flagSymbol: '$J'; flagDefault: false; flagDescription: 'WriteableConst'), {vfWriteableConst  }
    (flagSymbol: '$D'; flagDefault: true; flagDescription: 'DebugInfo'), {vfDebugInfo       }
    (flagSymbol: '$L'; flagDefault: true; flagDescription: 'LocalSymbols'), {vfLocalSymbols    }
    (flagSymbol: '$U'; flagDefault: true; flagDescription: 'DefinitionInfo'), {vfDefinitionInfo  }
    (flagSymbol: '$C'; flagDefault: true; flagDescription: 'Assertions') {vfAssertions      }
    );
implementation

end.


