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
unit DanteClasses;

interface
uses
  SysUtils,
  Classes;

type
  TProject    = class;
  TTarget     = class;
  TTask       = class;

  TDanteComponent = class(TComponent)
  protected
    function GetOwner: TPersistent; override;
    function GetProject: TProject;
    function NewName: string;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;
  public
    constructor Create(Owner: TComponent); override;

    property Project: TProject read GetProject;
  published
    property Name;
  end;

  TProject = class(TDanteComponent)
  protected
    FTargets: TList;

    function GetTarget(Index: Integer):TTarget;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent);  override;
    destructor  Destroy; override;

    procedure Parse(const Image: string);
    procedure Load(const Path: string);
    procedure Save(const Path: string);

    function AsString: string;
    function AddTarget(Name: string = ''): TTarget;
    function TargetCount: Integer;

    property Targets[i: Integer]: TTarget read GetTarget; default;
  end;

  TTarget = class(TDanteComponent)
  protected
    FTasks: TList;
    FDepends: TStrings;

    procedure SetDepends(Value: TStrings);
    function GetTask(Index: Integer):TTask;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor  Destroy; override;

    function TaskCount: Integer;

    property Tasks[i: Integer]: TTask read GetTask; default;
  published
    property Depends: TStrings read FDepends write SetDepends;
  end;

  TTask = class(TDanteComponent)
  public
    procedure Execute; virtual; abstract;
  end;

  TExecTask = class(TTask)
  private
    FOS: string;
    FExecutable: string;
    FArguments: TStringList;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure Execute; override;
  published
    property Arguments: TStringList read FArguments write FArguments;
    property Executable: string read FExecutable write FExecutable;
    property OS: string read FOS write FOS;
  end;


implementation

{ TDanteComponent }

constructor TDanteComponent.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  Name := NewName;
end;

function TDanteComponent.GetChildOwner: TComponent;
begin
  Result := self;
end;

procedure TDanteComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to ComponentCount - 1 do
  begin
    Proc(Components[I]);
  end;
end;


function TDanteComponent.GetOwner: TPersistent;
begin
  Result := inherited GetOwner;
end;

function TDanteComponent.GetProject: TProject;
begin
  if self is TProject then
     Result := TProject(self)
  else if Owner is TProject then
    Result := TProject(Owner)
  else if Owner is TDanteComponent then
    Result := TDanteComponent(Owner).Project
  else
    Result := nil;
end;

function TDanteComponent.NewName: string;
var
  i: Integer;
begin
  for i := 0 to MaxInt do
  begin
    Result := Format('%s_%d', [ClassName, i]);
    if (Owner = nil) or (Owner.FindComponent(Result) = nil) then
      break;
  end;
end;

{ TProject }

constructor TProject.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FTargets := TList.Create;
end;

destructor TProject.Destroy;
begin
  FTargets.Free;
  inherited Destroy;
end;

function TProject.AsString: string;
var
  MemStream: TMemoryStream;
  StrStream: TStringStream;
begin
  MemStream  := TMemoryStream.Create;
  try
    MemStream.WriteComponent(self);
    MemStream.Position := 0;
    StrStream := TStringStream.Create('');
    try
      ObjectBinaryToText(MemStream, StrStream);
      Result := StrStream.DataString;
    finally
       StrStream.Free
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TProject.Parse(const Image: string);
var
  MemStream: TMemoryStream;
  StrStream: TStringStream;
begin
  MemStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(Image);
    try
      ObjectTextToBinary(StrStream, MemStream);
    finally
      StrStream.Free;
    end;
    MemStream.Position := 0;
    MemStream.ReadComponent(self);
  finally
    MemStream.Free
  end;
end;


function TProject.AddTarget(Name: string): TTarget;
begin
  Result := TTarget.Create(self);
  if Name <> '' then
    Result.Name := Name;
end;

function TProject.GetTarget(Index: Integer): TTarget;
begin
  Result := FTargets[Index];
end;

procedure TProject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TTarget then
    if Operation = opRemove then
      FTargets.Remove(AComponent)
    else
      FTargets.Add(AComponent)
end;

function TProject.TargetCount: Integer;
begin
  Result := FTargets.Count;
end;

procedure TProject.Load(const Path: string);
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    S.LoadFromFile(Path);
    Parse(S.Text);
  finally
    S.Free;
  end;
end;

procedure TProject.Save(const Path: string);
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    S.Text := self.AsString;
    S.SaveToFile(Path);
  finally
    S.Free;
  end;
end;


{ TTarget }

constructor TTarget.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FTasks   := TList.Create;
  FDepends := TStringList.Create;
end;

destructor TTarget.Destroy;
begin
  FDepends.Free;
  FTasks.Free;
  inherited Destroy;
end;


function TTarget.GetTask(Index: Integer): TTask;
begin
  Result := FTasks[Index];
end;

procedure TTarget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TTask then
    if Operation = opRemove then
      FTasks.Remove(AComponent)
    else
      FTasks.Add(AComponent)
end;

procedure TTarget.SetDepends(Value: TStrings);
begin
  FDepends.Assign(Value);
end;

function TTarget.TaskCount: Integer;
begin
  Result := FTasks.Count;
end;

{ TExecTask }

constructor TExecTask.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FArguments := TStringList.Create;
end;

destructor TExecTask.Destroy;
begin
  FArguments.Free;
  inherited;
end;

procedure TExecTask.Execute;
begin
end;

initialization
  RegisterClasses([TProject, TTarget, TTask, TExecTask]);
end.


