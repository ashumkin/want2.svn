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
unit FileTasksTest;

interface

uses TestFramework, FileTasks, DanteClassesTest;

type
  TTestDeleteTask = class(TTestDirCase)
  private
    FDeleteTask: TDeleteTask;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestDeleteDir;
    procedure TestDeleteDirRelative;
  end;

implementation

uses JclFileUtils;

{ TTestDeleteTask }

procedure TTestDeleteTask.Setup;
begin
  inherited;
  FDeleteTask := TDeleteTask.Create(FProject.AddTarget('test_delete_task'));
end;

procedure TTestDeleteTask.TearDown;
begin
  FDeleteTask.Free;
  inherited;
end;

procedure TTestDeleteTask.TestDeleteDir;
begin
  CheckEquals('delete', TDeleteTask.XMLTag, 'XMLTag is wrong');
  MakeSampleTextFile;
  FDeleteTask.Dir := FTestDir;
  FDeleteTask.Execute;
  Check(DirectoryExists(FTestDir), 'directory not deleted');

  // ensure it doesn't blow up trying to delete a directory that's gone
  FDeleteTask.Execute;
end;

procedure TTestDeleteTask.TestDeleteDirRelative;
var
  SiblingDir: string;
begin
  MakeSampleTextFile;

  // need routine (add to clLib) to grab FTestDir parent (ExtractFilePathParent)
  SiblingDir := FTestDir;

end;

initialization
  RegisterTest('File Tasks', TTestDeleteTask);

end.

