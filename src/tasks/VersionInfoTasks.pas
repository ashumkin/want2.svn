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
unit VersionInfoTasks;

interface
uses
  SysUtils,
  DanteClasses,
  clVersionRcUnit;

type
  TVersionInfoTask = class(TTask)
  private
    FRcFileName: string;
    FIncrement:  boolean;
  public
    procedure Execute; override;
    procedure Init; override;
  published
    property rcfilename: string  read FRcFileName write FRcFileName;
    property increment:  boolean read FIncrement  write FIncrement;
  end;

implementation

{ TVersionInfoTask }

procedure TVersionInfoTask.Execute;
var
  FclVerRc: TclVersionRc;
begin
  Log('Incrementing build in ' + ToRelativePath(FRcFileName));
  FclVerRc := TclVersionRc.Create(ToSystemPath(FRcFileName));
  try
    if increment then
      FclVerRc.IncBuild;
  finally
    FclVerRc.Free;
  end;
end;

procedure TVersionInfoTask.Init;
var
  FclVerRc: TclVersionRc;
  BuildNo:  Integer;
begin
  inherited Init;

  RequireAttribute('rcfilename');

  FclVerRc := TclVersionRc.Create(ToSystemPath(FRcFileName));
  try
    BuildNo := FclVerRc.VersionInfo.Build;
    if increment then
      Inc(BuildNo);
    Project.SetProperty('build', IntToStr(BuildNo));
  finally
    FclVerRc.Free;
  end;
end;

initialization
  RegisterTasks([TVersionInfoTask]);
end.
