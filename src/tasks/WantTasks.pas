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
unit DanteTasks;

interface
uses
  SysUtils,
  Classes,

  JclSysUtils,

  DanteBase,
  DanteClasses,
  ScriptParser,
  WildPaths,
  PatternSets;


type
  TSubProjectPropertyElement = class(TScriptElement)
  public
    class function TagName :string;              override;
    procedure SetProperty(Name, Value :string); override;
  end;

  TCustomDanteTask = class(TTask)
  protected
    FTarget     :string;
  public
    property _target   :string read FTarget    write FTarget;
  end;

  TWantTask = class(TCustomDanteTask)
  protected
    FBuildFile  :string;
    FDir        :string;

    FSubProject :TProject;

  public
    constructor Create(Owner: TScriptElement = nil); override;
    destructor  Destroy; override;

    procedure Init; override;
    procedure Execute;  override;
  published
    property _target;
    property buildfile :string read FBuildFile write FBuildFile;
    property dir       :string read FDir       write FDir;
  end;

  TWantCallTask = class(TCustomDanteTask)
  public
    procedure Init; override;
    procedure Execute;  override;
  published
    property _target;
  end;


implementation

{ TWantTask }

constructor TWantTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FSubProject := TProject.Create(Self);
  FSubProject.RootPath := ToAbsolutePath(Project.RootPath);
  FSubProject.LogManager := Project.LogManager;
  FSubProject.OnLog := Self.Log;
end;

destructor TWantTask.Destroy;
begin
  inherited Destroy;
end;


procedure TWantTask.Init;
begin
  inherited Init;
  if dir <> '' then
    FSubProject.SetInitialBaseDir(dir);
end;

procedure TWantTask.Execute;
var
  bfile :string;
begin
  bfile := TScriptParser.Parse(FSubProject, buildfile);
  Log('building "%s"', [ ToRelativePath(bfile) ]);
  FSubProject.Build(_target);
end;

{ TSubProjectPropertyElement }

class function TSubProjectPropertyElement.TagName: string;
begin
  Result := 'property';
end;

procedure TSubProjectPropertyElement.SetProperty(Name, Value: string);
begin
  (Owner as TWantTask).FSubProject.SetProperty(Name, Value);
end;

{ TWantCallTask }

procedure TWantCallTask.Init;
begin
  RequireAttribute('target');
end;

procedure TWantCallTask.Execute;
begin
  Project.Build(_target);
end;

initialization
 RegisterTasks([TWantTask,TWantCallTask]);
end.
