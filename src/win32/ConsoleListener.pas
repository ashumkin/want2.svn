unit ConsoleListener;

interface
uses
  Windows,
  SysUtils,
  Classes,

  JclStrings,

  CRT32,
  WantClasses;

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

  DEFAULT_RIGTH_MARGIN = 78;

type
  TConsoleListener = class(TBuildListener)
  protected
    FUseColor     :boolean;
    FRightMargin  :Word;
    FPrefix       :string;

    procedure LogPrefix( Prefix :string; Level :TLogLevel);  virtual;
    procedure LogMessage(Prefix, Msg :string; Level :TLogLevel);  virtual;

    procedure LogLine(Msg: string; Level: TLogLevel = vlNormal); override;

    procedure DeleteTaskPrefix(Task :TTask);
  public
    constructor Create;

    procedure BuildFileLoaded(Project :TProject; FileName :string); override;

    procedure BuildStarted(Project :TProject);   override;
    procedure BuildFinished(Project :TProject);  override;
    procedure BuildFailed(Project :TProject; Msg :string = '');    override;

    procedure TargetStarted(Target :TTarget);    override;

    procedure TaskStarted(Task :TTask);          override;
    procedure TaskFinished(Task :TTask);         override;
    
    procedure TaskFailed(  Task :TTask; Msg :string);  override;

    property UseColor    :boolean read FUseColor    write FUseColor     default True;
    property RightMargin :wORD    read FRightMargin write FRightMargin  default DEFAULT_RIGTH_MARGIN;
  end;

implementation

{ TConsoleListener }

constructor TConsoleListener.Create;
begin
  inherited Create;
  FUseColor := True;
  FRightMargin := DEFAULT_RIGTH_MARGIN;
end;

procedure TConsoleListener.LogLine(Msg: string; Level: TLogLevel);
var
  Fragments :TStrings;
  n         :Integer;
begin
  Fragments := TStringList.Create;
  try
    Msg := WrapText(Msg, '@@', [' ',#13,#10,#9], RightMargin - Length(FPrefix));
    JclStrings.StrToStrings(Msg, '@@', Fragments);
    for n := 0 to Fragments.Count-1 do
      LogMessage(FPrefix, Fragments[n], Level);
  finally
    FreeAndNil(Fragments);
  end;
end;

procedure TConsoleListener.LogMessage(Prefix, Msg :string; Level: TLogLevel);
begin
  LogPrefix(Prefix, Level);
  if IsConsole then
  begin
    if UseColor then CRT32.TextColor(MsgColorMap[Level]);
    try
      Write(Msg);
      ClrEOL;
      WriteLn;
    finally
      if UseColor then CRT32.Restore;
    end;
  end;
end;

procedure TConsoleListener.LogPrefix(Prefix :string; Level: TLogLevel);
begin
  if IsConsole then
  begin
    if UseColor then CRT32.TextColor(PrefixColorMap[Level]);
    try
      Write(Prefix);
      ClrEOL;
    finally
      if UseColor then CRT32.Restore;
    end;
  end;
end;

procedure TConsoleListener.DeleteTaskPrefix(Task: TTask);
var
  p :Integer;
  S :string;
begin
  S := '[' + Task.TagName + ']';
  p := Pos(S, FPrefix);
  if p <> 0 then
    Delete(FPrefix, p, Length(S));
end;



procedure TConsoleListener.BuildFileLoaded(Project: TProject; FileName: string);
begin
  Log('buildfile: ' + FileName);
end;

procedure TConsoleListener.BuildStarted(Project: TProject);
begin
  Log(Project.Description);
end;

procedure TConsoleListener.BuildFinished(Project: TProject);
begin
  FPrefix := '';
  Log;
  LogMessage('', 'Build complete.', vlNormal);
end;

procedure TConsoleListener.BuildFailed(Project: TProject; Msg :string);
begin
  FPrefix := '';
  Log(Msg, vlErrors);
  LogMessage('BUILD FAILED', '', vlErrors);
end;

procedure TConsoleListener.TargetStarted(Target: TTarget);
begin
  FPrefix := '';
  LogMessage('', Target.TagName + ': ' + Target.Description, vlNormal);
  FPrefix := '     ';
end;

procedure TConsoleListener.TaskStarted(Task: TTask);
begin
  FPrefix := Format('%14s ', [Trim(FPrefix + '[' + Task.TagName + ']') ] );
  Log(Task.Description);
end;

procedure TConsoleListener.TaskFinished(Task: TTask);
begin
  DeleteTaskPrefix(Task);
end;

procedure TConsoleListener.TaskFailed(Task: TTask; Msg: string);
begin
  Log(Msg, vlErrors);
  DeleteTaskPrefix(Task);
end;



end.
