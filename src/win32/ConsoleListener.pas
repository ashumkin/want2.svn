unit ConsoleLogMgr;

interface
uses
  CRT32,
  LogMgr;

const
  PrefixColorMap :array[TLogLevel] of WORD = (
    CRT32.LightRed,
    CRT32.Magenta,
    CRT32.Green,
    CRT32.Blue,
    CRT32.DarkGray
   );

  MsgColorMap :array[TLogLevel] of WORD = (
    CRT32.Yellow,
    CRT32.LightMagenta,
    CRT32.White,
    CRT32.DarkGray,
    CRT32.LightGray
  );


type
  TConsoleLogManager = class(TLogManager)
  protected
    procedure   OutputLog(   const aLine: String; const aFlush: Boolean;   const aLevel: TLogLevel);  override;
    procedure   OutputPrefix(const aPrefix: String; const aFlush: Boolean; const aLevel: TLogLevel);  override;
  end;

implementation

{ TConsoleLogManager }

procedure TConsoleLogManager.OutputLog(const aLine: String; const aFlush: Boolean; const aLevel: TLogLevel);
begin
  if IsConsole then
  begin
    CRT32.TextColor(MsgColorMap[aLevel]);
    try
      Writeln(aLine);
    finally
      CRT32.Restore;
    end;
  end;
  inherited OutputLog(aLine, aFlush, aLevel);
end;

procedure TConsoleLogManager.OutputPrefix(const aPrefix: String; const aFlush: Boolean; const aLevel: TLogLevel);
begin
  if IsConsole then
  begin
    CRT32.TextColor(PrefixColorMap[aLevel]);
    try
      Write(aPrefix);
    finally
      CRT32.Restore;
    end;
  end;
  inherited OutputPrefix(aPrefix, aFlush, aLevel);
end;

end.
