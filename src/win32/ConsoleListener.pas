unit ConsoleListener;

interface
uses
  Windows,
  SysUtils,
  Classes,

  JclStrings,

  CRT32,
  WantClasses,
  BuildListeners;

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

  DEFAULT_RIGTH_MARGIN = 72;

type
  TConsoleListener = class(TBasicListener)
  protected
    FUseColor     :boolean;
    FRightMargin  :Word;
    FPrefix       :string;

    FFragments    :TStrings;

    procedure LogPrefix( Prefix :string; Level :TLogLevel);  virtual;
    procedure LogMessage(Prefix, Msg :string; Level :TLogLevel);  virtual;

    procedure LogLine(Msg: string; Level: TLogLevel = vlNormal); override;

    procedure DeleteTaskPrefix(Task :TTask);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure BuildFileLoaded(Project :TProject; FileName :string); override;

    procedure BuildStarted;                        override;
    procedure BuildFinished;                       override;

    procedure ProjectStarted(Project :TProject);   override;
    procedure ProjectFinished(Project :TProject);  override;

    procedure TargetStarted(Target :TTarget);    override;
    procedure TargetFinished(Target: TTarget);   override;

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

  FFragments := TStringList.Create;
end;



destructor TConsoleListener.Destroy;
begin
  FFragments.Free;
  inherited Destroy;
end;

procedure TConsoleListener.LogLine(Msg: string; Level: TLogLevel);
var
  n         :Integer;
begin

  if (Length(Msg) = 0) or (Msg[Length(Msg)] = #13) then
    EXIT;
    
  Msg := WrapText(Msg, '@@', [' ',#10,#9], RightMargin - Length(FPrefix));
  if Pos('@@', Msg) = 0 then
    LogMessage(FPrefix, Msg, Level)
  else
  begin
    FFragments.Clear;
    JclStrings.StrToStrings(Msg, '@@', FFragments);
    for n := 0 to FFragments.Count-1 do
      LogMessage(FPrefix, FFragments[n], Level);
    FFragments.Clear;
  end;
end;

procedure TConsoleListener.LogMessage(Prefix, Msg :string; Level: TLogLevel);
begin
  LogPrefix(Prefix, Level);
  if UseColor then CRT32.TextColor(MsgColorMap[Level]);
  try
    Write(Msg);
    ClrEOL;
    WriteLn;
  finally
    if UseColor then begin
      CRT32.Restore;
      ClrEOL;
    end;
  end;
end;

procedure TConsoleListener.LogPrefix(Prefix :string; Level: TLogLevel);
begin
  if UseColor then CRT32.TextColor(PrefixColorMap[Level]);
  try
    Write(Prefix);
    ClrEOL;
  finally
    if UseColor then
    begin
      CRT32.Restore;
      ClrEOL;
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
  if Trim(FPrefix) = '' then
    FPrefix := ''; 
end;



procedure TConsoleListener.BuildFileLoaded(Project: TProject; FileName: string);
begin
  inherited BuildFileLoaded(Project, FileName);
  Log(vlNormal, 'buildfile: ' + FileName);
end;

procedure TConsoleListener.ProjectStarted(Project: TProject);
begin
  inherited ProjectStarted(Project);
  Log(vlNormal, Project.Description);
end;

procedure TConsoleListener.ProjectFinished(Project: TProject);
begin
  inherited ProjectFinished(Project);
end;

procedure TConsoleListener.TargetStarted(Target: TTarget);
begin
  inherited TargetStarted(Target);
  Log(vlNormal, Target.Name + ': ' + Target.Description);
end;

procedure TConsoleListener.TargetFinished(Target: TTarget);
begin
  inherited TargetFinished(Target);
  Log(vlNormal);
end;

procedure TConsoleListener.TaskStarted(Task: TTask);
begin
  inherited TaskStarted(Task);
  FPrefix := Format('%14s ', [Trim(FPrefix) + '[' + Task.TagName + ']' ] );
  if Task.Description <> '' then
    Log(vlNormal, Task.Description);
end;

procedure TConsoleListener.TaskFinished(Task: TTask);
begin
  inherited TaskFinished(Task);
  DeleteTaskPrefix(Task);
end;

procedure TConsoleListener.TaskFailed(Task: TTask; Msg: string);
begin
  inherited TaskFailed(Task, Msg);
  Log(vlErrors, Msg);
  DeleteTaskPrefix(Task);
end;


procedure TConsoleListener.BuildStarted;
begin

end;

procedure TConsoleListener.BuildFinished;
begin
  inherited BuildFinished;
  if Failures or Errors then
    LogMessage('BUILD FAILED', '', vlErrors)
  else
  begin
    Log(vlNormal);
    Log(vlNormal, 'Build complete.');
  end;
end;

end.
