(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo A�ez, Caracas, Venezuela.          *
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

  JclSysUtils,
  JclMiscel,
  JclStrings,

  ConsoleListener,
  WildPaths,

  WantBase,
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

function TConsoleScriptRunner.ParseOption(Switch: string):boolean;
var
  PropName:  string;
  PropValue: string;
  EqPos:     Integer;
begin
  Result := True;
  if (Switch = '-h') or (Switch = '-?') then
    // nothing: handled elsewhere
  else if (Switch = '-L') then
    // nothing: handled elsewhere
  else if Switch = '-verbose' then
    Verbosity := vlVerbose
  else if Switch = '-debug' then
  begin
    Verbosity := vlDebug;
    Listener.Level := Verbosity;
    Log(vlDebug, 'Parsing commandline');
  end
  else if Switch = '-quiet' then
    Verbosity := vlQuiet
  else if Switch = '-color'then
    UseColor := True
  else if StrLeft(Switch, 2) = '-D' then
  begin
    Delete(Switch, 1, 2);

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
      else if (StrLeft(Param, 1) = '-') then
      begin
        if not ParseOption(Param) then
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


