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
  WildPaths,
  FileOps,
  FileSets,

  Collections,
  MiniDom,

  SysUtils,
  Classes,
  TypInfo;

type
  TProject    = class;
  TTarget     = class;
  TTask       = class;
  TTaskClass  = class of TTask;

  EDanteException   = class(Exception);
  ETargetException  = class(EDanteException);
  ETaskException    = class(EDanteException);


  ENoDefaultTargetError     = class(ETargetException);
  ETargetNotFoundException  = class(ETargetException);
  ECircularTargetDependency = class(ETargetException);


  ETaskError       = class(ETaskException);
  ETaskFailure     = class(ETaskException);

  EDanteParseException = class(EDanteException);


  TTargetArray = array of TTarget;

  TVerbosityLevel = (
     vlErrors,
     vlWarnings,
     vlVeryQuiet,
     vlQuiet,
     vlNormal,
     vlVerbose,
     vlDebug
  );


  TDanteComponent = class(TComponent)
  protected
    function GetOwner: TPersistent; override;
    function GetProject: TProject;
    function NewName: string;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;

  public
    constructor Create(Owner: TComponent); override;

    class function Tag :string; virtual;
    procedure ParseXML(Node :MiniDom.IElement); virtual;

    property Project: TProject read GetProject;
  published
    property Name;
    // Tag is the Name to use for the task in build scripts
  end;



  TProject = class(TDanteComponent)
  protected
    FTargets:       TList;
    FDefaultTarget: string;
    FVerbosity:     TVerbosityLevel;
    FBaseDir:      string;
    FDescription:   string;
    FProperties:    TStrings;

    function  GetTarget(Index: Integer):TTarget;

    procedure BuildSchedule(TargetName :string; Sched :TList);

    procedure DoParseXML(Node :MiniDom.IElement);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent = nil);  override;
    destructor  Destroy; override;

    class function Tag :string; override;
    procedure ParseXML(Node :MiniDom.IElement); override;

    procedure Parse(const Image: string);
    procedure ParseXMLText(const XML :string);

    procedure Load(const Path: string);
    procedure LoadXML(const SystemPath: string);
    procedure Save(const Path: string);

    // use this function in Tasks to let the user specify relative
    // directories that work consistently
    function  RelativePath(SubPath :string) :string;

    function  AsString: string;
    function  AddTarget(Name: string = ''): TTarget;
    function  TargetCount: Integer;

    function  GetTargetByName(Name :string):TTarget;

    procedure SetProperties(Value :TStrings);
    procedure SetProperty(Name, Value :string);
    function  PropertyDefined(Name :string): boolean;

    function Schedule(TargetName :string) :TTargetArray;
    procedure Build(TargetName :string); overload;
    procedure Build;                     overload;

    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal);

    property Targets[i: Integer]: TTarget read GetTarget; default;
    property Names[TargetName :string] :TTarget read GetTargetByName;
    property BaseDir :string read FBaseDir write FBaseDir;

  published
    property Default:       string          read FDefaultTarget  write FDefaultTarget;
    property Verbosity:     TVerbosityLevel read FVerbosity      write FVerbosity default vlNormal;
    property Properties:    TStrings        read FProperties     write SetProperties;
    property Description:   string          read FDescription    write FDescription;
  end;

  TTarget = class(TDanteComponent)
  protected
    FTasks:   TList;
    FDepends: string;

    function GetTask(Index: Integer):TTask;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(Owner: TComponent); override;
    destructor  Destroy; override;

    class function Tag :string; override;
    procedure ParseXML(Node :MiniDom.IElement); override;

    function TaskCount: Integer;
    procedure Build;

    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal);

    property Tasks[i: Integer]: TTask read GetTask; default;
  published
    property Depends: string read FDepends write FDepends;
  end;

  TTask = class(TDanteComponent)
  protected

    procedure DoExecute;
  public
    function Target :TTarget;

    procedure Execute; virtual; abstract;
    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal);

  published
  end;


  // for properties
  TWriteOnceStringList = class(TStringList)
  public
    constructor Create;
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
  end;


function  FindTask(Tag :string): TTaskClass;
procedure RegisterTask(TaskClass :TTaskClass);
procedure RegisterTasks(TaskClasses :array of TTaskClass);

implementation

var
  __TaskRegistry :TStringList;

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

class function TDanteComponent.Tag: string;
begin
  Result := copy(ClassName, 2, 255);
  Result := LowerCase(Result);
end;


function TDanteComponent.NewName: string;
var
  i:   Integer;
begin
  for i := 0 to MaxInt do
  begin
    Result := Format('%s_%d', [ClassName, i]);
    if (Owner = nil) or (Owner.FindComponent(Result) = nil) then
      break;
  end;
end;

procedure TDanteComponent.ParseXML(Node: IElement);
var
  i        :IIterator;
  att:     IAttribute;
  AName    :string;
  prop     :IElement;
  TypeInfo :PTypeInfo;
  PropInfo :PPropInfo;
begin
  if Node.Name <> Self.Tag then
    raise EDanteParseException.Create(Format('expected <%s>', [Self.Tag]) );

  TypeInfo := Self.ClassInfo;

  i := Node.Attributes;
  while i.HasNext do
  begin
    att := i.Next as IAttribute;
    AName := att.Name;
    PropInfo := GetPropInfo(TypeInfo, AName);
    if PropInfo = nil then
      raise EDanteParseException.Create( Format('Unknown attribute %s.%s', [Tag, AName]) )
    else if PropInfo.SetProc <> nil then
      // read only property (Name), do nothing
    else
    begin
      try
        with PropInfo^, PropType^^ do
          if Kind in [tkString, tkLString, tkWString] then
            SetStrProp(Self, PropInfo, att.Value)
          else if Kind in [tkInteger] then
            SetOrdProp(Self, PropInfo, StrToInt(att.Value))
          else if Kind in [tkEnumeration] then
            SetOrdProp(Self, PropInfo, GetEnumValue(PropType^, att.Value))
          else
            raise EDanteParseException.Create(Format('Cannot handle type of attribute %s.%s', [Tag, AName]) )
      except
        on e :Exception do
          raise EDanteParseException.Create(Format('Error %s setting value of attribute %s.%s', [e.Message, Tag, AName]) );
      end;
    end;
  end;

  i := Node.Children('property').Iterator;
  while i.HasNext do
  begin
    prop := i.Next as IElement;
    Project.SetProperty(prop.attributeValue('name'), prop.attributeValue('value'));
  end;

end;

{ TProject }

constructor TProject.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FTargets    := TList.Create;
  FProperties := TWriteOnceStringList.Create;
  FVerbosity  := vlNormal;
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
  FBaseDir := ToPath(ExtractFilePath(ExpandFileName(Path)));
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
  Deps   : TStrings;
begin
  Target := GetTargetByName(TargetName);
  if Sched.IndexOf(Target) >= 0 then
     EXIT; // done

  Deps := TStringList.Create;
  try
    Deps.CommaText := Target.Depends;
    for i := 0 to Deps.Count-1 do
       BuildSchedule(Deps[i], Sched);
  finally
    Deps.Free;
  end;

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

procedure TProject.Log(Msg: string; Verbosity :TVerbosityLevel);
begin
  // bare bones implementation
  // something smarter can be thought of later
  if Self.Verbosity >= Verbosity then
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
      Log(Format('%s: %s', [e.ClassName, e.Message]), vlErrors);
      raise;
    end;
  end;
end;

procedure TProject.Build;
begin
  if Default = '' then
  begin
    raise ENoDefaultTargetError.Create('No default target');
  end;
  Build(Default);
end;

function TProject.RelativePath(SubPath: string): string;
begin
  Result := BaseDir + SubPath;
end;

procedure TProject.SetProperties(Value: TStrings);
begin
  FProperties.Assign(Value);
end;

procedure TProject.SetProperty(Name, Value: string);
begin
  Properties.Values[Name] := Value;
end;

function TProject.PropertyDefined(Name: string): boolean;
begin
  Result :=   (Properties.IndexOf(Name) >= 0)
           or (Properties.IndexOfName(Name) >= 0)
end;


// XML handling

procedure TProject.LoadXML(const SystemPath: string);
var
  Dom :IDocument;
begin
  FBaseDir := ToPath(ExtractFilePath(ExpandFileName(SystemPath)));
  Dom := MiniDom.ParseToDom(SystemPath);
  Self.DoParseXML(Dom.Root);
end;

class function TProject.Tag: string;
begin
  Result := 'project';
end;

procedure TProject.ParseXML(Node: IElement);
var
  i     :IIterator;
  child :IElement;
begin
  inherited ParseXML(Node);

  i := Node.Children(TTarget.Tag).Iterator;
  while i.HasNext do
  begin
    child := i.Next as IElement;
    AddTarget(child.attributeValue('name')).ParseXML(child);
  end;
end;


procedure TProject.DoParseXML(Node: IElement);
begin
  try
    ParseXML(Node);
  except
    on e :Exception do
    begin
      Log(e.Message);
      raise;
    end;
  end;
end;

procedure TProject.ParseXMLText(const XML: string);
begin
  DoParseXML(MiniDom.ParseTextToDOM(XML).Root);
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
end;

destructor TTarget.Destroy;
begin
  FTasks.Free;
  inherited Destroy;
end;


class function TTarget.Tag: string;
begin
  Result := 'target';
end;

function TTarget.GetTask(Index: Integer): TTask;
begin
  Result := FTasks[Index];
end;

procedure TTarget.Log(Msg: string; Verbosity :TVerbosityLevel);
begin
  Project.Log(Format('%s: %s', [Name, Msg]), Verbosity);
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

function TTarget.TaskCount: Integer;
begin
  Result := FTasks.Count;
end;


procedure TTarget.ParseXML(Node: IElement);
var
  i         :IIterator;
  child     :IElement;
  Task      :TTask;
begin
  inherited ParseXML(Node);

  i := Node.Children.Iterator;
  while i.HasNext do
  begin
    child := i.Next as IElement;
    Task := FindTask(child.Name).Create(Self);
    Task.ParseXML(child);
  end;
end;


{ TTask }

procedure TTask.Log(Msg: string; Verbosity :TVerbosityLevel);
begin
  Project.Log(Format('%16s %s', ['['+Tag+']', Msg]), Verbosity);
end;

function TTask.Target: TTarget;
begin
  Result := Owner as TTarget;
end;

procedure TTask.DoExecute;
begin
  try
    try
      Execute;
    except
      on e :Exception do
      begin
        Log(Format('%s: %s', [e.ClassName, e.Message]), vlErrors);
        raise;
      end;
    end;
  finally
    ChangeDir(Project.BaseDir);
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

function FindTask(Tag :string) :TTaskClass;
var
  Index :Integer;
begin
  Index := __TaskRegistry.IndexOf(Tag);
  if Index < 0 then
    raise ETaskError.Create( Format('Task class <%s> not found', [Tag]) )
  else
    Result := Pointer(__TaskRegistry.Objects[Index])
end;


procedure RegisterTask(TaskClass :TTaskClass);
begin
  __TaskRegistry.AddObject(TaskClass.Tag, Pointer(TaskClass));
  if GetClass(TaskClass.ClassName) = nil then
    RegisterClass(TaskClass);
end;

procedure RegisterTasks(TaskClasses :array of TTaskClass);
var
  i :Integer;
begin
  for i := Low(TaskClasses) to High(TaskClasses) do
    RegisterTask(TaskClasses[i]);
end;

initialization
  __TaskRegistry := TStringList.Create;
  __TaskRegistry.Sorted := true;
  __TaskRegistry.Duplicates := dupIgnore;

  RegisterClasses([TProject, TTarget, TTask]);
finalization
  __TaskRegistry.Free;
end.


