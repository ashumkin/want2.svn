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
Original Author: Chris Morris
Contributors   :
}
unit VssTasks;

interface

uses
  DanteClasses, ExecTasks, JclStrings, WildPaths;

type
  TVssBaseTask = class(TCustomExecTask)
  private
    FFileName: string;
    FVssPath: string;
    FLogin: string;
    FLocalPath: string;
  public
    procedure Execute; override;
    procedure Init; override;

    property FileName: string read FFileName write FFileName;
    property LocalPath: string read FLocalPath write FLocalPath;
    property Login: string read FLogin write FLogin;
    property VssPath: string read FVssPath write FVssPath;
  end;

  TVssGetTask = class(TVssBaseTask)
  private
    F_Label: string;
  public
    procedure Execute; override;
  published
    property _Label: string read F_Label write F_Label;
    property LocalPath;
    property Login;
    property VssPath;
  end;

  TVssCheckoutTask = class(TVssBaseTask)
  public
    procedure Execute; override;
  published
    property FileName;
    property LocalPath;
    property Login;
    property VssPath;
  end;

  { could refactor a base class out of checkout/checkin (basecheck), but
    not a huge deal -- only benefit is not redeclaring FileName }
  TVssCheckinTask = class(TVssBaseTask)
  private
    FComment: string;
  public
    procedure Execute; override;
  published
    property Comment: string read FComment write FComment;
    property FileName;
    property LocalPath;
    property Login;
    property VssPath;
  end;

implementation

{ TVssBaseTask }

procedure TVssBaseTask.Execute;
  function FormatVssPath: string;
  begin
    Result := JclStrings.StrEnsureSuffix('/', FVssPath);
    if FFileName = '' then
      Result := JclStrings.StrChopRight(Result, 1)
    else
      Result := Result + FFileName;
    Result := '"' + Result + '"';
  end;
begin
  ArgumentList.Add(FormatVssPath);

  if FLogin <> '' then
    ArgumentList.Add('-Y' + FLogin);

  if FLocalPath <> '' then
    ArgumentList.Add('"-GL' + WildPaths.ToSystemPath(ToAbsolutePath(ToDantePath(FLocalPath))) + '"');

  { ignore prompts for information - chooses default. Vss is, AFAIK, good
    about having non-destructive defaults. }
  ArgumentList.Add('-I-');
  inherited Execute;
end;

procedure TVssBaseTask.Init;
begin
  inherited;
  RequireAttribute('vsspath');
end;

{ TVssGetTask }

procedure TVssGetTask.Execute;
begin
  Executable := 'ss Get';

  { skip writable files }
  ArgumentList.Add('-GWS');

  if F_Label <> '' then
    ArgumentList.Add('"-VL' + F_Label + '"');

  inherited Execute;
end;

{ TVssCheckoutTask }

procedure TVssCheckoutTask.Execute;
begin
  Executable := 'ss Checkout';

  inherited Execute;
end;

{ TVssCheckinTask }

procedure TVssCheckinTask.Execute;
begin
  Executable := 'ss Checkin';

  ArgumentList.Add('"-C' + FComment + '"');

  inherited Execute;
end;

initialization
  RegisterTasks([TVssGetTask]);
  RegisterTasks([TVssCheckoutTask]);
  RegisterTasks([TVssCheckinTask]);

end.

