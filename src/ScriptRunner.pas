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
}
unit DanteMain;

interface

uses
  Windows,
  SysUtils,

  JclMiscel,
  JclStrings,

  DanteClasses;


type
  TDante = class(TObject)
  protected
    function RunConsole(CmdLine: string): boolean;

    procedure SetCommandLineProperties(Project :TProject);
  public
    procedure DoBuild( ABuildFileName: string;
                       Verbosity: TVerbosityLevel = vlNormal); overload;
    procedure DoBuild( ABuildFileName: string;
                       Targets:        string;
                       Verbosity:      TVerbosityLevel = vlNormal); overload;
  end;

implementation

{ TDante }

procedure TDante.DoBuild( ABuildFileName: string;
                          Targets:        string;
                          Verbosity:      TVerbosityLevel = vlNormal);
var
  Project: TProject;
  T:       string;
begin
  Project := TProject.Create(nil);
  try
    SetCommandLineProperties(Project);
    Project.LoadXML(ABuildFileName);
    Project.Verbosity := Verbosity;
    if Targets = '' then
      Project.Build
    else begin
      T := StrToken(Targets, ',');
      while T <> '' do
      begin
        Project.Build(T);
        T := StrToken(Targets, ',');
      end;
    end;
  finally
    Project.Free;
  end;
end;

procedure TDante.DoBuild(ABuildFileName: string; Verbosity: TVerbosityLevel);
begin
  DoBuild(ABuildFileName, '', Verbosity);
end;

function TDante.RunConsole(CmdLine: string): boolean;
begin
  Result := (WinExec32AndWait(CmdLine, SW_HIDE) = 0);
end;

procedure TDante.SetCommandLineProperties(Project: TProject);
var
  i:         Integer;
  Param:     string;
  PropName:  string;
  PropValue: string;
  EqPos:     Integer;
begin
  for i := 1 to ParamCount do
  begin
    Param := ParamStr(i);
    if Copy(Param, 1, 2) = '-D' then
    begin
      Delete(Param, 1, 2);

      EqPos := Pos('=', Param);
      if EqPos = 0 then
         EqPos := 1+Length(Param);

      PropName  := Copy(Param, 1, EqPos-1);
      PropValue := Copy(Param, EqPos+1, Length(Param));

      PropValue := StrTrimQuotes(PropValue);

      Project.SetProperty(PropName, PropValue);
    end;
  end;
end;

end.


