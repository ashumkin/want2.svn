(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit ConsoleScriptRunner;

interface
uses
  SysUtils,
  Classes,

  JclStrings,

  CRT32,

  WildPaths,
  WantClasses,
  WantResources,
  ConsoleListener,
  ScriptRunner;

const
  rcs_id :string = '#(@)$Id$';

type
  TConsoleScriptRunner = class(TScriptRunner)
  protected
    FBuildFile   :string;
    FTargets     :TStringArray;

    procedure ParseCommandLine(Project :TProject);  virtual;
    function  ParseArgument(Project :TProject; var N :Integer; Argument:string) :boolean;  virtual;
    function  ParseOption(  Project :TProject; var N :Integer; Switch :string)  :boolean;  virtual;

    function  GetUseColor :boolean;
    procedure SetUseColor(Value :boolean);
  public
    procedure CreateListener; override;
    property  UseColor :Boolean read GetUseColor write SetUseColor;

    procedure Execute;    virtual;
  end;

implementation

{ TConsoleScriptRunner }


procedure TConsoleScriptRunner.CreateListener;
begin
  FListener := TConsoleListener.Create;
end;

function TConsoleScriptRunner.GetUseColor: boolean;
begin
  Result := TConsoleListener(Listener).UseColor;
end;

procedure TConsoleScriptRunner.SetUseColor(Value: boolean);
begin
  TConsoleListener(Listener).UseColor := Value;
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


procedure TConsoleScriptRunner.Execute;
var
  Project :TProject;
begin
  Project := TProject.Create;
  try
    Project.Listener := Listener;
    ParseCommandLine(Project);
    if FBuildFile = '' then
      FBuildFile := FindBuildFile(True);
    LoadProject(Project, FBuildFile);
    BuildProject(Project, FTargets);
  finally
    FreeAndNil(Project);
  end;
end;


procedure TConsoleScriptRunner.ParseCommandLine(Project: TProject);
var
  p:         Integer;
  Param:     string;
begin
  try
    p := 1;
    while p <= ParamCount do
    begin
      Param := ParamStr(p);
      if Param[1] in ['-','/'] then
      begin
        if not ParseOption(Project, p, Copy(Param, 2, Length(Param))) then
          WantError('Unknown commandline option: ' + Param);
      end
      else if not ParseArgument(Project, p, Param) then
          WantError('Don''t know what to do with argument : ' + Param);
      Inc(p);
    end;
  except
    on e :Exception do
    begin
      Listener.Log(vlErrors, e.Message);
      raise;
    end;
  end;
end;


function TConsoleScriptRunner.ParseArgument(Project: TProject; var N: Integer; Argument: string): boolean;
begin
  SetLength(FTargets, 1+Length(FTargets));
  FTargets[High(FTargets)] := Argument;
  Result := True;
end;

function TConsoleScriptRunner.ParseOption(Project :TProject; var N :Integer; Switch: string):boolean;
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
  else if Switch = 'buildfile' then
  begin
    Inc(N);
    FBuildFile := ToPath(ParamStr(N));
  end
  else if Switch = 'verbose' then
    Listener.Level := vlVerbose
  else if Switch = 'debug' then
  begin
    Listener.Level := vlDebug;
    Log(vlDebug, 'Parsing commandline');
  end
  else if Switch = 'quiet' then
    Listener.Level := vlQuiet
  else if Switch = 'color'then
    UseColor := True
  else if Copy(Switch, 1, 1) = 'D' then
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

end.
