{ $Id$ }
{
--------------------------------------------------------------------------
Copyright (c) 2001, Juancarlo Añez, Caracas, Venezuela.
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
--------------------------------------------------------------------------------
(based on BSD Open Source License)
}
unit DanteTasks;

interface
uses
  DanteClasses,
  WildPaths,
  FileSets,
  FileOps,

  SysUtils,
  Classes;

type
  TSubProjectPropertyElement = class(TDanteElement)
  public
    class function XMLTag :string;              override;
    procedure SetProperty(Name, Value :string); override;
  end;

  TCustomDanteTask = class(TTask)
  protected
    FTarget     :string;
  public
    property _target   :string read FTarget    write FTarget;
  end;

  TDanteTask = class(TCustomDanteTask)
  protected
    FBuildFile  :string;
    FDir        :string;

    FSubProject :TProject;

  public
    constructor Create(Owner: TDanteElement = nil); override;
    destructor  Destroy; override;

    procedure Validate; override;
    procedure Execute;  override;
  published
    function CreateProperty :TPropertyElement; override;

    property _target;
    property buildfile :string read FBuildFile write FBuildFile;
    property dir       :string read FDir       write FDir;
  end;

  TDanteCallTask = class(TCustomDanteTask)
  public
    procedure Validate; override;
    procedure Execute;  override;
  published
    property _target;
  end;


implementation

{ TDanteTask }

constructor TDanteTask.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  FSubProject := TProject.Create;
  FSubProject.OnLog := Self.Log;
  FSubProject.Properties := Self.Project.Properties;
  FSubProject.SetInitialBaseDir('');

  buildfile := DanteClasses.BuildFileName;
end;

destructor TDanteTask.Destroy;
begin
  FSubProject.Free;
  inherited Destroy;
end;


procedure TDanteTask.Validate;
begin
  inherited Validate;
  // nothing required
end;

procedure TDanteTask.Execute;
begin
  // this allows the sub project to inherit paths
  // calculated by the current project
  FSubProject.RunPath := ToAbsolutePath(Project.RunPath);
  if dir <> '' then
    FSubProject.SetInitialBaseDir(dir);

  Log('building "%s" in directory "%s"', [
                     ToRelativePath(buildfile),
                     ToRelativePath(dir)
                     ]);


  FSubProject.LoadXML(buildfile, false);
  FSubProject.Build(_target);
end;

function TDanteTask.CreateProperty: TPropertyElement;
begin
  Result := FSubProject.CreateProperty;
end;

{ TSubProjectPropertyElement }

class function TSubProjectPropertyElement.XMLTag: string;
begin
  Result := 'property';
end;

procedure TSubProjectPropertyElement.SetProperty(Name, Value: string);
begin
  (Owner as TDanteTask).FSubProject.SetProperty(Name, Value);
end;

{ TDanteCallTask }

procedure TDanteCallTask.Validate;
begin
  RequireAttribute('target', _target);
end;

procedure TDanteCallTask.Execute;
begin
  Project.Build(_target);
end;

initialization
 RegisterTasks([TDanteTask,TDanteCallTask]);
end.
