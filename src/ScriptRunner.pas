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

  JclMiscel,
  JclStrings,

  LogMgr,
  ConsoleLogMgr,
  WildPaths,

  DanteBase,
  DanteClasses,

  StandardElements,
  StandardTasks,
  CustomTasks;

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

    procedure ParseCommandLine;              virtual;
    function  ParseOption(Switch :string) :boolean;  virtual;

    function  GetUseColor :boolean;
    procedure SetUseColor(Value :boolean);
  public
    constructor Create(Owner :TDanteElement = nil); override;
    destructor  Destroy; override;


    procedure DoBuild; overload;

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
    LoadXML(ABuildFileName);
    if LogManager <> nil then
      LogManager.Level := Level;
    if Length(Targets) = 0 then
      Build
    else
    begin
      for t := Low(Targets) to High(Targets) do
        Build(Targets[t]);
    end;
    
    Log;
    Log('Build complete.');
  except
    on e: Exception do
    begin
      if not (e is EDanteException) then
        Log(vlErrors, E.ClassName + ': ' + E.Message);
      Log;
      Log(vlErrors, 'BUILD FAILED');
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

constructor TConsoleDante.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  CreateLogManager;
end;

destructor TConsoleDante.Destroy;
begin
  LogManager.Free;
  LogManager := nil;
  inherited Destroy;
end;

procedure TConsoleDante.DoBuild;
begin
  ParseCommandLine;
  DoBuild(FBuildFile, FTargets, FVerbosity);
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
        DanteError('Unknown commandline option: ' + Param)
    end
    else
    begin
      SetLength(FTargets, 1+Length(FTargets));
      FTargets[High(FTargets)] := Param;
    end;
    Inc(p);
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


