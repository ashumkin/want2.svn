{ $Id$ }
{
--------------------------------------------------------------------------------
Copyright (c) 2001, Dante Authors -- See authors.txt for complete list
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The name Dante, the names of the authors in authors.txt and the names of
other contributors to this software may not be used to endorse or promote
products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------
Original Author: Juancarlo Añez
Contributors   :
}
unit DanteMain;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Forms,

  JclSysUtils,
  JclMiscel,
  JclStrings,

  LogMgr,
  ConsoleLogMgr,
  WildPaths,

  DanteBase,
  DanteClasses,
  ScriptParser,

  StandardElements,
  StandardTasks,
  CustomTasks,

  ScriptFrm;

type
  TDante = class(TProject)
  public
    procedure DoBuild( ABuildFileName: TPath;
                       Targets:    TStringArray;
                       Level:      TLogLevel = vlNormal); overload;
    procedure DoBuild( ABuildFileName: TPath;
                       Target:     string;
                       Level:      TLogLevel = vlNormal); overload;
    procedure DoBuild( ABuildFileName: TPath;
                       Level:      TLogLevel = vlNormal); overload;

    procedure CreateLogManager;
  end;

  TConsoleDante = class(TDante)
  protected
    FBuildFile   :string;
    FTargets     :TStringArray;
    FDoEdit      :boolean;

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



{ TDante }

procedure TDante.DoBuild( ABuildFileName: TPath;
                          Targets:    TStringArray;
                          Level:      TLogLevel = vlNormal);
var
  t:    Integer;
begin
  if not IsSystemIndependentPath(ABuildFileName) then
    ABuildFileName := ToPath(ABuildFileName);
  Log('buildfile: ' + ToRelativePath(FindBuildFile(ABuildFileName, False)));
  Log(Description);

  try
    TScriptParser.Parse(Self, ABuildFileName);
    if LogManager <> nil then
      LogManager.Level := Level;
    try
      if Length(Targets) = 0 then
        Build
      else
      begin
        for t := Low(Targets) to High(Targets) do
          Build(Targets[t]);
      end;
    finally
      Log;
    end;
    Log('Build complete.');
  except
    on e: Exception do
    begin
      if e is ETaskException then
        Log('BUILD FAILED','', vlErrors)
      else if e is EDanteException then
        Log('BUILD FAILED',e.Message, vlErrors)
      else
        Log('BUILD FAILED', E.ClassName + ': ' + E.Message, vlErrors);
      raise;
    end;
  end;
end;


procedure TDante.CreateLogManager;
begin
  LogManager := TConsoleLogManager.Create;
end;

procedure TDante.DoBuild(ABuildFileName: TPath; Level: TLogLevel);
begin
  DoBuild(ABuildFileName, nil, Level);
end;

procedure TDante.DoBuild(ABuildFileName: TPath; Target: string; Level: TLogLevel);
var
  T :TStringArray;
begin
  SetLength(T, 1);
  T[0] := Target;
  DoBuild(ABuildFileName, T, Level);
end;

{ TConsoleDante }

constructor TConsoleDante.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  CreateLogManager;
end;

destructor TConsoleDante.Destroy;
begin
  FreeAndNil(FLogManager);
  inherited Destroy;
end;

procedure TConsoleDante.Execute;
begin
  ParseCommandLine;
  if not FDoEdit then
    DoBuild(FBuildFile, FTargets, FVerbosity)
  else
  begin
    Application.Initialize;
    Application.CreateForm(TScriptForm, ScriptForm);
    Application.Run;
  end;
end;

function TConsoleDante.ParseOption(Switch: string):boolean;
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
    Verbosity := vlDebug
  else if Switch = '-quiet' then
    Verbosity := vlQuiet
  else if Switch = '-color'then
    UseColor := True
  else if Switch = '-edit' then
    FDoEdit := True
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

procedure TConsoleDante.ParseCommandLine;
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
        FBuildFile := FindBuildFile(ToPath(ParamStr(p)), True);
      end
      else if (StrLeft(Param, 1) = '-') then
      begin
        if not ParseOption(Param) then
          DanteError('Unknown commandline option: ' + Param);
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

function TConsoleDante.GetUseColor: boolean;
begin
  Result := TConsoleLogManager(LogManager).UseColor;
end;

procedure TConsoleDante.SetUseColor(Value: boolean);
begin
  TConsoleLogManager(LogManager).UseColor := Value;
end;


end.


