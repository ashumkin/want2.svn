(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit ScriptRunner;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  Math,

  JclSysUtils,
  JclMiscel,
  JclStrings,

  CRT32,

  ConsoleListener,
  WildPaths,

  WantResources,
  WantClasses,
  ScriptParser,

  StandardElements,
  StandardTasks,
  CustomTasks;

type
  TScriptRunner = class(TProject)
  public
    procedure DoBuild( ABuildFileName: TPath;
                       Targets:    TStringArray;
                       Level:      TLogLevel = vlNormal); overload;
    procedure DoBuild( ABuildFileName: TPath;
                       Target:     string;
                       Level:      TLogLevel = vlNormal); overload;
    procedure DoBuild( ABuildFileName: TPath;
                       Level:      TLogLevel = vlNormal); overload;

    procedure CreateListener;
  end;

  TConsoleScriptRunner = class(TScriptRunner)
  protected
    FBuildFile   :string;
    FTargets     :TStringArray;

    procedure ParseCommandLine;              virtual;
    function  ParseOption(Switch :string) :boolean;  virtual;

    function  GetUseColor :boolean;
    procedure SetUseColor(Value :boolean);
  public
    constructor Create(Owner :TScriptElement = nil); override;
    destructor  Destroy; override;


    procedure Execute; virtual;

    property  UseColor :Boolean read GetUseColor write SetUseColor;
  end;

implementation



{ TScriptRunner }

procedure TScriptRunner.DoBuild( ABuildFileName: TPath;
                          Targets:    TStringArray;
                          Level:      TLogLevel = vlNormal);
var
  t:    Integer;
begin
  if not IsSystemIndependentPath(ABuildFileName) then
    ABuildFileName := ToPath(ABuildFileName);

  try
    ABuildFileName := TScriptParser.Parse(Self, ABuildFileName);

    if Listener <> nil then
    begin
      Listener.Level := Level;
      Listener.BuildFileLoaded(Self, WildPaths.ToRelativePath(ToAbsolutePath(ABuildFileName), CurrentDir));
    end;

    if Length(Targets) = 0 then
      Build
    else
    begin
      for t := Low(Targets) to High(Targets) do
        Build(Targets[t]);
    end;
  except
    on e: EWantParseException do
    begin
      if Listener <> nil then
        Listener.BuildFailed(Self, e.Message);
      raise;
    end;
    on e: EWantException do
      raise;
    on e: Exception do
    begin
      Log(E.ClassName + ': ' + E.Message, vlErrors);
      raise;
    end;
  end;
end;


procedure TScriptRunner.CreateListener;
begin
  Listener := TConsoleListener.Create;
end;

procedure TScriptRunner.DoBuild(ABuildFileName: TPath; Level: TLogLevel);
begin
  DoBuild(ABuildFileName, nil, Level);
end;

procedure TScriptRunner.DoBuild(ABuildFileName: TPath; Target: string; Level: TLogLevel);
var
  T :TStringArray;
begin
  SetLength(T, 1);
  T[0] := Target;
  DoBuild(ABuildFileName, T, Level);
end;

{ TConsoleScriptRunner }

constructor TConsoleScriptRunner.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  CreateListener;
end;

destructor TConsoleScriptRunner.Destroy;
begin
  FreeAndNil(FListener);
  inherited Destroy;
end;

procedure TConsoleScriptRunner.Execute;
begin
  ParseCommandLine;
  if FBuildFile = '' then
    FBuildFile := FindBuildFile(True);
  DoBuild(FBuildFile, FTargets, Verbosity)
end;

procedure More(Text :string);
var
  S :TStrings;
  i :Integer;
begin
  S := TStringList.Create;
  try
    S.Text := Text;
    i := 0;
    while i < S.Count do
    begin
      if (Pos('---', S[i]) <> 1) then
      begin
        Writeln(S[i]);
        Inc(i);
      end
      else
      begin
        Write(Format('-- More (%d%%) --'#13, [100*(i+2) div S.Count]));
        Inc(i);
        repeat until ReadKey in [' ',#13,#10, 'q'];
        writeln(#13' ': 70);
      end;
    end;
  finally
    S.Free;
  end;
end;


function TConsoleScriptRunner.ParseOption(Switch: string):boolean;
var
  PropName:  string;
  PropValue: string;
  EqPos:     Integer;
begin
  Result := True;
  if (Switch = 'h')
  or (Switch = '?')
  then
  begin
    WriteLn(Copyright );
    Usage;
    Halt(2);
  end
  else if (Switch = 'v')
  or (Switch = 'version')
  or (Switch = '-version') then
  begin
    WriteLn(Copyright );
    Halt(2);
  end
  else if (Switch = 'L') then
  begin
    More(License);
    Halt(3);
  end
  else if Switch = 'verbose' then
    Verbosity := vlVerbose
  else if Switch = 'debug' then
  begin
    Verbosity := vlDebug;
    Listener.Level := Verbosity;
    Log(vlDebug, 'Parsing commandline');
  end
  else if Switch = 'quiet' then
    Verbosity := vlQuiet
  else if Switch = 'color'then
    UseColor := True
  else if StrLeft(Switch, 1) = 'D' then
  begin
    Delete(Switch, 1, 1);

    EqPos := Pos('=', Switch);
    if EqPos = 0 then
       EqPos := 1+Length(Switch);

    PropName  := Copy(Switch, 1, EqPos-1);
    PropValue := Copy(Switch, EqPos+1, Length(Switch));

    PropValue := StrTrimQuotes(PropValue);

    Project.SetProperty(PropName, PropValue);
  end
  else
    Result := False;
end;

procedure TConsoleScriptRunner.ParseCommandLine;
var
  p:         Integer;
  Param:     string;
begin
  try
    p := 1;
    while p <= ParamCount do
    begin
      Param := ParamStr(p);
      if Param = '-buildfile' then
      begin
        Inc(p);
        FBuildFile := ToPath(ParamStr(p));
      end
      else if Param[1] in ['-','/'] then
      begin
        if not ParseOption(Copy(Param, 2, Length(Param))) then
          WantError('Unknown commandline option: ' + Param);
      end
      else
      begin
        SetLength(FTargets, 1+Length(FTargets));
        FTargets[High(FTargets)] := Param;
      end;
      Inc(p);
    end;
  except
    on e :Exception do
    begin
      Log(vlErrors, e.Message);
      raise;
    end;
  end;
end;

function TConsoleScriptRunner.GetUseColor: boolean;
begin
  Result := TConsoleListener(Listener).UseColor;
end;

procedure TConsoleScriptRunner.SetUseColor(Value: boolean);
begin
  TConsoleListener(Listener).UseColor := Value;
end;


end.


