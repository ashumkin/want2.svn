{ $Id$ }
{
--------------------------------------------------------------------------
Copyright (c) 2001, Chris Morris
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The names Chris Morris, Dante and the names of contributors to this software
may not be used to endorse or promote products derived from this software
without specific prior written permission.

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

Contributor(s): Juancarlo Añez
--------------------------------------------------------------------------------
(based on BSD Open Source License)
}
unit DelphiTasks;

interface
uses
  DanteClasses,
  ExecTasks,
  WildPaths,

  Collections,
  MiniDom,

  JclBase,
  JclMiscel,
  JclSysInfo,
  JclRegistry,

  Windows,
  SysUtils,
  Classes;


type
  EDelphiTaskError       = class(ETaskError);
  EDelphiNotFoundError   = class(EDelphiTaskError);
  ECompilerNotFoundError = class(EDelphiTaskError);

  TDelphiCompileTask = class(TCustomExecTask)
  protected
    FOutputPath      :string;
    FUnitOutputPath  :string;
    FSource          :string;
    FQuiet           :boolean;
    FMake            :boolean;
    FBuild           :boolean;

    FUnitPaths     :TStrings;

    function BuildCmdLine: string; override;
    function FindDelphiDir :string;
    function FindCompiler :string;
  public
    constructor Create(owner :TComponent); override;
    destructor  Destroy; override;

    class function XMLTag :string; override;
    procedure ParseXML(Node :MiniDom.IElement); override;


    procedure Execute; override;
  published

    property Arguments;
    property ArgumentList stored False;
    property SkipLines :Integer     read FSkipLines   write FSkipLines;

    property exes :string    read FOutputPath     write FOutputPath;
    property dcus :string    read FUnitOutputPath write FUnitOutputPath;

    property quiet :boolean read FQuiet write FQuiet default true;
    property make  :boolean read FMake  write FMake;
    property build :boolean read FBuild write FBuild;

    property source :string read FSource write FSource;
  end;

implementation

{ TDelphiCompileTask }

function TDelphiCompileTask.BuildCmdLine: string;
var
  Paths: string;
  i:     Integer;
begin
  Result := inherited BuildCmdLine;

  if source <> '' then
    Result := Result + ' ' + ToSystemPath(source);

  if exes <> '' then
    Result := Result + ' -E' + ToSystemPath(exes);
  if dcus <> '' then
    Result := Result + ' -N' + ToSystemPath(dcus);
  if quiet then
    Result := Result + ' -Q';

  if build then
    Result := Result + ' -B'
  else if make then
    Result := Result + ' -M';

  if FUnitPaths.Count > 0 then
  begin
    Paths := '';
    for i := 0 to FUnitPaths.Count-1 do
      Paths := Paths + ';' + ToSystemPath(FUnitPaths[i]);
    Result := Result + ' -U' + Paths;
  end;
end;

constructor TDelphiCompileTask.Create(owner: TComponent);
begin
  inherited Create(owner);
  FUnitPaths := TStringList.Create;
  SkipLines  := 2;
  quiet      := true;
end;

destructor TDelphiCompileTask.Destroy;
begin
  FUnitPaths.Free;
  inherited Destroy;
end;

procedure TDelphiCompileTask.Execute;
begin
  self.Executable := FindCompiler;
  inherited Execute;
end;

function TDelphiCompileTask.FindCompiler: string;
begin
  Result := FindDelphiDir + '\bin\dcc32.exe';
  if not FileExists(Result) then
     raise ECompilerNotFoundError.Create(Result);
end;

function TDelphiCompileTask.FindDelphiDir: string;
const
  DelphiRegRoot  = 'SOFTWARE\Borland\Delphi';
  DelphiRootKey  = 'RootDir';

  function RootFor(version :Integer) :string;
  begin
    Result := Format('%s\%d.0', [DelphiRegRoot, version]);
  end;

  function ReadRootFor(version :Integer):string;
  begin
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, RootFor(version), DelphiRootKey, '');
  end;

var
  ver :Integer;
begin
  for ver := 6 downto 4 do
  begin
    Result := ReadRootFor(ver);
    if Result <> '' then EXIT;
  end;
  raise EDelphiNotFoundError.Create('');
end;

procedure TDelphiCompileTask.ParseXML(Node: IElement);
var
  i:    IIterator;
  paths :TStringArray;
  p:    Integer;
begin
  inherited ParseXML(Node);

  paths := nil;
  i := Node.Children('unit').Iterator;
  while i.HasNext do
  begin
    paths := CommaTextToArray((i.Next as IElement).attributeValue('path') );
    for p := Low(paths) to High(paths) do
      FUnitPaths.Add(paths[p]);
  end;
end;

class function TDelphiCompileTask.XMLTag: string;
begin
  Result := 'dcc';
end;

initialization
  RegisterTasks([TDelphiCompileTask]);
end.
