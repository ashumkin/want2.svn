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

  EDanteException   = class(Exception);
  ETargetException  = class(EDanteException);
  ETaskException    = class(EDanteException);


  ENoDefaultTargetError     = class(ETargetException);
  ETargetNotFoundException  = class(ETargetException);
  ECircularTargetDependency = class(ETargetException);


  ETaskError       = class(ETaskException);
  ETaskFailure     = class(ETaskException);


  TTargetArray = array of TTarget;


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
    FDefaultTarget: string;
    FBeQuiet: boolean;
    // base directory: where the build script was found
    FBasePath: string;
    FProperties: TStrings;

    function  GetTarget(Index: Integer):TTarget;

    procedure BuildSchedule(TargetName :string; Sched :TList);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent = nil);  override;
    destructor  Destroy; override;

    procedure Parse(const Image: string);
    procedure Load(const Path: string);
    procedure Save(const Path: string);

    // use this function in Tasks to let the user specify relative
    // directories that work consistently
    function  RelativePath(SubPath :string) :string;

    function AsString: string;
    function AddTarget(Name: string = ''): TTarget;
    function TargetCount: Integer;

    function GetTargetByName(Name :string):TTarget;

    procedure SetProperties(Value :TStrings);
    function  PropertyDefined(Name :string): boolean;

    function Schedule(TargetName :string) :TTargetArray;
    procedure Build(TargetName :string); overload;
    procedure Build;                     overload;

    procedure Log(Msg :string = '');

    property Targets[i: Integer]: TTarget read GetTarget; default;
    property Names[TargetName :string] :TTarget read GetTargetByName;
    property BasePath :string read FBasePath write FBasePath;

  published
    property DefaultTarget :string read FDefaultTarget write FDefaultTarget;
    property BeQuiet: boolean read FBeQuiet write FBeQuiet;
    property Properties :TStrings read FProperties write SetProperties;
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
    procedure Build;

    procedure Log(Msg :string = '');

    property Tasks[i: Integer]: TTask read GetTask; default;
  published
    property Depends: TStrings read FDepends write SetDepends;
  end;

  TTask = class(TDanteComponent)
  protected
    FTag :string;

    function GetTag :string; virtual;
    procedure DoExecute;
  public
    function Target :TTarget;

    procedure Execute; virtual; abstract;
    procedure Log(Msg :string = '');

  published
    // Tag is the Name to use for the task in build scripts
    property Tag : string read GetTag;
  end;


  // for properties
  TWriteOnceStringList = class(TStringList)
  public
    constructor Create;
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
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
  FTargets    := TList.Create;
  FProperties := TWriteOnceStringList.Create;
end;

destructor TProject.Destroy;
begin
  FTargets.Free;
  FProperties.Free;
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
    FBasePath := ExtractFilePath(ExpandFileName(Path));
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


function TProject.GetTargetByName(Name: string): TTarget;
begin
  Result := self.FindComponent(Name) as TTarget;
  if Result = nil then
    raise ETargetNotFoundException.Create(Name);
end;

procedure TProject.BuildSchedule(TargetName: string; Sched: TList);
var
  Target : TTarget;
  i      : Integer;
begin
  Target := GetTargetByName(TargetName);
  if Sched.IndexOf(Target) >= 0 then
     EXIT; // done
  for i := 0 to Target.Depends.Count-1 do
     BuildSchedule(Target.Depends[i], Sched);
  if Sched.IndexOf(Target) >= 0 then
     raise ECircularTargetDependency.Create(TargetName);
  Sched.Add(Target);
end;

function TProject.Schedule(TargetName: string): TTargetArray;
var
  Sched :TList;
  i     :Integer;
begin
  Sched := TList.Create;
  try
    BuildSchedule(TargetName, Sched);
    SetLength(Result, Sched.Count);
    for i := 0 to Sched.Count-1 do
      Result[i] := Sched[i];
  finally
    Sched.Free;
  end;
end;

procedure TProject.Log(Msg: string);
begin
  // bare bones implementation
  // something smarter can be thought of later
  writeln(Msg);
end;

procedure TProject.Build(TargetName: string);
var
  i     :Integer;
  Sched :TTargetArray;
begin
  Sched := Schedule(Targetname);
  try
    for i := Low(Sched) to High(Sched) do
    begin
      Sched[i].Build;
      Log;
    end;
  except
    on e :Exception do
    begin
      Log(Format('%s: %s', [e.ClassName, e.Message]));
      raise;
    end;
  end;
end;

procedure TProject.Build;
begin
  if DefaultTarget = '' then
  begin
    raise ENoDefaultTargetError.Create('No default target');
  end;
  Build(DefaultTarget);
end;

function TProject.RelativePath(SubPath: string): string;
begin
  Result := BasePath + SubPath;
end;

procedure TProject.SetProperties(Value: TStrings);
begin
  FProperties.Assign(Value);
end;

function TProject.PropertyDefined(Name: string): boolean;
begin
  Result :=   (Properties.IndexOf(Name) >= 0)
           or (Properties.IndexOfName(Name) >= 0)
end;

{ TTarget }

procedure TTarget.Build;
var
  i :Integer;
begin
  Log;
  for i := 0 to TaskCount-1 do
    Tasks[i].DoExecute;
end;

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

procedure TTarget.Log(Msg: string);
begin
  Project.Log(Format('%s: %s', [Name, Msg]));
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


{ TTask }

function TTask.GetTag: string;
begin
  Result := FTag;
  if Result = '' then
  begin
    Result := copy(ClassName, 2, 255);
    Result := StringReplace(Result, 'Task','', [rfIgnoreCase]);
    Result := LowerCase(Result);
  end;
end;

procedure TTask.Log(Msg: string);
begin
  Project.Log(Format('%16s %s', ['['+Tag+']', Msg]));
end;

function TTask.Target: TTarget;
begin
  Result := Owner as TTarget;
end;

procedure TTask.DoExecute;
begin
  try
    Execute;
  except
    on e :Exception do
    begin
      Log(Format('%s: %s', [e.ClassName, e.Message]));
      raise;
    end;
  end;
end;

{ TWriteOnceStringList }

function TWriteOnceStringList.Add(const S: string): Integer;
begin
  Result := IndexOf(S);
  if Result < 0 then
    Result := inherited Add(S);
end;

constructor TWriteOnceStringList.Create;
begin
  inherited Create;
  Sorted := true;
  Duplicates := dupError;
end;

procedure TWriteOnceStringList.Insert(Index: Integer; const S: string);
begin
  if IndexOf(S) < 0 then
    inherited Insert(Index, S);
end;

initialization
  RegisterClasses([TProject, TTarget, TTask]);
end.


