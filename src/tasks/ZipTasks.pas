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
unit ZipTasks;

interface
uses
  SysUtils,
  WildPaths,
  PatternSets,
  ZipStreams,
  DanteClasses,
  FileTasks;


type
  TZipTask = class(TFileSetTask)
  protected
    FZipFile   :TPath;
    FCompress  :boolean;

    FZipStream :TZipStream;

  public
    constructor Create(Owner :TDanteElement); override;

    procedure Init; override;
    procedure DoFileset(Fileset :TFileSet); override;

    procedure Execute; override;
  published
    property basedir;

    property zipfile  :TPath   read FZipFile   write FZipFile;
    property compress :boolean read FCompress  write FCompress    default true;

    property includes :string write AddCommaSeparatedIncludes;
    property excludes :string write AddCommaSeparatedExcludes;
  end;

  TUnzipTask = class(TTask)
  protected
    FZipFile   :TPath;
    FToDir     :TPath;

    FUnzipStream :TUnzipStream;

  public
    procedure Init; override;
    procedure Execute; override;
  published
    property zipfile  :TPath read FZipFile write FZipFile;
    property src      :TPath read FZipFile write FZipFile;

    property todir    :TPath read FToDir     write FToDir;
    property dest     :TPath read FToDir     write FToDir;
  end;

implementation

{ TZipTask }

constructor TZipTask.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  FCompress := true;
end;

procedure TZipTask.Init;
begin
  inherited Init;
  RequireAttribute('zipfile');
end;

procedure TZipTask.DoFileset(Fileset: TFileSet);
var
  Paths   :TPaths;
  p       :Integer;
begin
  Log(vlVerbose, Format('Fileset with basedir "%s"', [Fileset.dir]));
  Log(vlVerbose, CurrentDir);
  AboutToScratchPath(zipfile);

  Paths := ToRelativePaths(FileSet.Paths, BasePath);

  if Length(Paths) = 0 then
    Log
  else
    Log(Format('Zipping %4d files from %s to %s', [Length(Paths), ToRelativePath(Fileset.dir), ToRelativePath(zipfile)]));

  for p := Low(Paths) to High(Paths) do
  begin
    Log(vlDebug, Paths[p]);
    FZipStream.WriteFile(Paths[p]);
  end;
end;

procedure TZipTask.Execute;
begin
  AboutToScratchPath(zipfile);
  FZipStream := TZipStream.Create(zipfile);
  try
    if not compress then
      FZipStream.CompressionLevel := zlNone;
    inherited Execute;
  finally
    FZipStream.Free;
    FZipStream := nil;
  end
end;


{ TUnzipTask }

procedure TUnzipTask.Init;
begin
  inherited Init;
  if FZipFile = '' then
    TaskError('zipfile (or src) attribute is required');
end;

procedure TUnzipTask.Execute;
var
  ToPath :TPath;
  e :Integer;
begin
  Log(vlVerbose);
  ToPath := ToRelativePath(ToDir);
  FUnzipStream := TUnzipStream.Create(zipfile);
  try
    with FUnzipStream do
    begin
      if Entries.Count > 0 then
      begin
        Log('Unzipping %d files to "%s"', [Entries.Count, ToPath]);

        for e := 0 to Entries.Count-1 do
        begin
          Log(vlVerbose, Entries[e]);
          AboutToScratchPath(Entries[e]);
          ExtractFile(Entries[e], ToPath);
        end;
      end;
    end;
  finally
    FUnzipStream.Free;
    FUnzipStream := nil;
  end
end;

initialization
  RegisterTasks([TZipTask, TUnzipTask]);
end.
