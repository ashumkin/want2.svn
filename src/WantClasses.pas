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

const
  BuildFileName = 'build.xml';

  SupportedPropertyTypes = [
     //tkInteger,
     tkEnumeration,
     tkString,
     tkLString,
     tkWString];

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


  TStringArray = array of string;

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

    class function XMLTag :string; virtual;
    procedure ParseXML(Node :MiniDom.IElement); virtual;
    function  AsXML     :string;                virtual;
    function  ToXML(Dom :IDocument) : IElement; virtual;

    // use this to get the fully qualified base path
    function  BasePath :string; virtual;
    // use this function in Tasks to let the user specify relative
    // directories that work consistently
    function  RelativePath(SubPath :string) :string; virtual;

    property Project: TProject read GetProject;
    property Tag stored False;
  published
    property Name stored False;
  end;



  TProject = class(TDanteComponent)
  protected
    FTargets:       TList;
    FDefaultTarget: string;
    FVerbosity:     TVerbosityLevel;
    FBaseDir:       string;
    FRunPath:       string;
    FDescription:   string;
    FProperties:    TStrings;

    function  GetTarget(Index: Integer):TTarget;

    procedure BuildSchedule(TargetName :string; Sched :TList);

    procedure DoParseXML(Node :MiniDom.IElement);

    procedure SetBaseDir(Path :TPath);
    function  GetBaseDir :TPath;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent = nil);  override;
    destructor  Destroy; override;

    class function XMLTag :string; override;
    procedure ParseXML(Node :MiniDom.IElement); override;
    function  ToXML(Dom :IDocument) : IElement; override;

    procedure Parse(const Image: string);
    procedure ParseXMLText(const XML :string);

    procedure Load(const Path: string);
    procedure LoadXML(const SystemPath: string);
    procedure Save(const Path: string);

    // use this to get the fully qualified base path
    function  BasePath :string; override;
    // use this function in Tasks to let the user specify relative
    // directories that work consistently
    function  RelativePath(SubPath :string) :string; override;

    function  AsString: string;
    function  AddTarget(Name: string = ''): TTarget;
    function  TargetCount: Integer;

    function  GetTargetByName(Name :string):TTarget;

    procedure SetProperties(Value :TStrings);
    procedure SetProperty(Name, Value :string);
    function  PropertyDefined(Name :string): boolean;

    function  Schedule(TargetName :string) :TTargetArray;
    procedure Build(TargetName :string = '');

    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal);

    property Targets[i: Integer]: TTarget read GetTarget; default;
    property Names[TargetName :string] :TTarget read GetTargetByName;

  published
    property BaseDir :string read GetBaseDir write SetBaseDir;
    property Default:       string          read FDefaultTarget  write FDefaultTarget;
    property Verbosity:     TVerbosityLevel
      read    FVerbosity
      write   FVerbosity
      stored  False
      default vlNormal;
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

    class function XMLTag :string; override;
    procedure ParseXML(Node :MiniDom.IElement); override;
    function  ToXML(Dom :IDocument) : IElement; override;

    function TaskCount: Integer;
    procedure Build;

    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal);

    property Tasks[i: Integer]: TTask read GetTask; default;
  published
    property Depends: string read FDepends write FDepends;
  end;

  TTask = class(TDanteComponent)
  protected
    function GetName :string;
    procedure DoExecute;
  public
    function Target :TTarget;

    procedure Execute; virtual; abstract;
    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal);

    property Name :string read GetName stored False;
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

function CommaTextToArray(Text :string) :TStringArray;

implementation

function CommaTextToArray(Text :string) :TStringArray;
var
  S: TStrings;
  i: Integer;
begin
  S := TStringList.Create;
  try
    S.CommaText := Text;
    SetLength(Result, S.Count);
    for i := 0 to S.Count-1 do
       Result[i] := S[i];
  finally
    S.Free;
  end;
end;

var
  __TaskRegistry :TStringList;

{ TDanteComponent }

constructor TDanteComponent.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  //!!!Name := NewName;
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

class function TDanteComponent.XMLTag: string;
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
  if Node.Name <> Self.XMLTag then
    raise EDanteParseException.Create(Format('expected <%s>', [Self.XMLTag]) );

  TypeInfo := Self.ClassInfo;

  i := Node.Attributes;
  while i.HasNext do
  begin
    att := i.Next as IAttribute;
    AName := att.Name;
    PropInfo := GetPropInfo(TypeInfo, AName);
    if PropInfo = nil then
      raise EDanteParseException.Create( Format('Unknown attribute %s.%s', [XMLTag, AName]) )
    else if not IsStoredProp(Self, PropInfo) then
      continue
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
            raise EDanteParseException.Create(Format('Cannot handle type of attribute %s.%s', [Name, AName]) )
      except
        on e :Exception do
          raise EDanteParseException.Create(Format('Error %s setting value of attribute %s.%s', [e.Message, Name, AName]) );
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

function TDanteComponent.AsXML: string;
begin
  Result := toXML(TDocument.Create).toString;
end;

function TDanteComponent.ToXML(Dom :MiniDom.IDocument) :MiniDom.IElement;
var
  TypeInfo:  PTypeInfo;
  PropList:  PPropList;
  PropInfo:  PPropInfo;
  PropCount: Integer;
  i:         Integer;
  PropName:  string;
  PropValue: string;
begin
  Result := Dom.NewElement(XMLTag);

  TypeInfo  := Self.ClassInfo;
  PropCount := GetTypeData(TypeInfo).PropCount;
  if PropCount > 0 then
  begin
    GetMem(PropList, PropCount * SizeOf(PPropInfo));
    try
      GetPropInfos(TypeInfo, PropList);
      for i := 0 to PropCount-1 do
      begin
        PropInfo  := PropList[i];
        PropName  := AnsiLowerCase(PropInfo.Name);
        PropValue := '';
        with PropInfo^, PropType^^ do
          if IsStoredProp(Self, PropInfo)
          and (SetProc <> nil)
          and (GetProc <> nil)
          and (Kind in SupportedPropertyTypes)
          then
          begin
            if Kind in [tkString, tkLString, tkWString] then
              PropValue := GetStrProp(Self, PropInfo)
            else if Kind in [tkInteger] then
              PropValue := IntToStr(GetOrdProp(Self, PropInfo))
            else if Kind in [tkEnumeration] then
              PropValue := GetEnumName(PropType^, GetOrdProp(Self, PropInfo))
            else
            begin
              // do nothing
            end
          end;
        if PropValue <> '' then
          Result.SetAttribute(PropName, PropValue);
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;

function TDanteComponent.BasePath: string;
begin
  Result := Project.BasePath;
end;

function TDanteComponent.RelativePath(SubPath: string): string;
begin
  Result := Project.RelativePath(SubPath);
end;

{ TProject }

constructor TProject.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FTargets    := TList.Create;
  FProperties := TWriteOnceStringList.Create;
  FVerbosity  := vlNormal;
  FRunPath    := ToPath(GetCurrentDir);
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
  Deps   : TStringArray;
begin
  Target := GetTargetByName(TargetName);
  if Sched.IndexOf(Target) >= 0 then
     EXIT; // done

  Deps := CommaTextToArray(Target.Depends);
  for i := Low(Deps) to High(Deps) do
     BuildSchedule(Deps[i], Sched);

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
  if TargetName = '' then
  begin
    if Default = '' then
      raise ENoDefaultTargetError.Create('No default target')
    else
      TargetName := Default;
  end;
  Sched := Schedule(Targetname);
  try
    try
      for i := Low(Sched) to High(Sched) do
      begin
        Sched[i].Build;
        Log;
      end;
    finally
      ChangeDir(FRunpath);
    end;
  except
    on e :Exception do
    begin
      Log(Format('%s: %s', [e.ClassName, e.Message]), vlErrors);
      raise;
    end;
  end;
end;

function TProject.RelativePath(SubPath: string): string;
begin
  Result := BasePath + SubPath;
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
  Dom := MiniDom.ParseToDom(SystemPath);
  Self.DoParseXML(Dom.Root);
end;

class function TProject.XMLTag: string;
begin
  Result := 'project';
end;

procedure TProject.ParseXML(Node: IElement);
var
  i     :IIterator;
  child :IElement;
begin
  inherited ParseXML(Node);
  Self.Name := Node.attributeValue('name');

  i := Node.Children(TTarget.XMLTag).Iterator;
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

function TProject.ToXML(Dom: IDocument): IElement;
var
  i :Integer;
begin
  Result := inherited ToXML(Dom);
  Result.setAttribute('name', Self.Name);
  for i := 0 to TargetCount-1 do
    Result.Add(Targets[i].toXML(Dom));
end;

function TProject.BasePath: string;
begin
  if PathIsAbsolute(BaseDir) then
    Result := BaseDir
  else
    Result := PathConcat(FRunPath, BaseDir);
end;

function TProject.GetBaseDir: string;
begin
  Result := AbsoluteToRelativePath(FBaseDir, FRunPath);
end;

procedure TProject.SetBaseDir(Path: TPath);
begin
  FBaseDir := AbsoluteToRelativePath(Path, FRunPath);
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


class function TTarget.XMLTag: string;
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


function TTarget.ToXML(Dom: IDocument): IElement;
var
  i :Integer;
begin
  Result := inherited ToXML(Dom);
  Result.setAttribute('name', Self.Name);
  for i := 0 to TaskCount-1 do
    Result.Add(Tasks[i].toXML(Dom));
end;

{ TTask }

procedure TTask.Log(Msg: string; Verbosity :TVerbosityLevel);
begin
  Project.Log(Format('%12s %s', ['['+XMLTag+']', Msg]), Verbosity);
end;

function TTask.Target: TTarget;
begin
  Result := Owner as TTarget;
end;

procedure TTask.DoExecute;
begin
  try
    try
      ChangeDir(Project.BasePath);
      Execute;
    except
      on e :Exception do
      begin
        Log(Format('%s: %s', [e.ClassName, e.Message]), vlErrors);
        raise;
      end;
    end;
  finally
    ChangeDir(Project.BasePath);
  end;
end;

function TTask.GetName: string;
begin
  Result := inherited Name;
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
  __TaskRegistry.AddObject(TaskClass.XMLTag, Pointer(TaskClass));
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

