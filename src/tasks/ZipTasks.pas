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
unit ZipTasks;

interface
uses
  SysUtils,
  WildPaths,
  ZipStreams,
  DanteClasses,
  FileTasks;

type
  TZipTask = class(TFileSetTask)
  protected
    FZipFile  :string;
    FBaseDir  :string;
    FCompress :boolean;

    procedure SetBaseDir(Value :string);

  public
    constructor Create(Owner :TDanteElement); override;

    procedure Validate; override;
    procedure Execute;  override;
  published
    property zipfile  :string  read FZipFile  write FZipFile;
    property basedir  :string  read FBaseDir  write SetBaseDir;
    property compress :boolean read FCompress write FCompress    default true;

    property includes :string write AddCommaSeparatedIncludes;
    property excludes :string write AddCommaSeparatedExcludes;
  end;

implementation

{ TZipTask }

procedure TZipTask.Validate;
begin
  if zipfile = '' then
    TaskError('zipfile attribute not set');
end;

constructor TZipTask.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  FCompress := true;
end;

procedure TZipTask.Execute;
var
  Paths :TPaths;
  Zip   :TZipStream;
  p     :Integer;
begin
  Paths := FFileSet.RelativePaths;
  Log(Format('Zipping %d files to %s', [Length(Paths), zipfile]));

  Zip := TZipStream.Create(ToSystemPath(zipfile));
  try
    for p := Low(Paths) to High(Paths) do
    begin
      Log(vlVerbose, Paths[p]);
      Zip.WriteFile(ToSystemPath(Paths[p]));
    end;
  finally
    Zip.Free;
  end
end;

procedure TZipTask.SetBaseDir(Value: string);
begin
  FBaseDir := Value;
  FFileSet.Dir := ToRelativePath(Value);
end;

initialization
  RegisterTask(TZipTask);
end.
