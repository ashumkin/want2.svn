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
  Windows,
  SysUtils,
  Classes,
  TypInfo,

  WildPaths,
  FileOps,

  Collections,
  MiniDom,

  JclSysInfo,
  JclStrings;


const
  BuildFileName = 'build.xml';

  SupportedPropertyTypes = [
     //tkInteger,
     tkEnumeration,
     tkString,
     tkLString,
     tkWString];

  LabeledMsgFormat = '%12s %s';
  
type
  TDanteElement = class;
  TDanteElementClass = class of TDanteElement;

  TProject    = class;
  TTarget     = class;
  TTask       = class;
  TTaskClass  = class of TTask;

  EDanteException   = class(Exception);
  EDanteError       = class(Exception);
  ETargetException  = class(EDanteException);
  ETaskException    = class(EDanteException);


  ENoDefaultTargetError     = class(ETargetException);
  ETargetNotFoundException  = class(ETargetException);
  ECircularTargetDependency = class(ETargetException);


  ETaskError       = class(ETaskException);
  ETaskFailure     = class(ETaskException);

  EDanteParseException = class(EDanteException);


  TStringArray = array of string;

  TDanteElementArray = array of TDanteElement;

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

  TCreateElementMethod = function :TDanteElement of object;


  TDanteElement = class(TComponent)
  protected
    FName :string; // ditch TComponent.Name

    function GetOwner: TPersistent; override;
    function GetProject: TProject;
    function NewName: string;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function  GetChildOwner: TComponent; override;
    function  GetChildrenTyped(AClass :TDanteElementClass) : TDanteElementArray;

    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal); overload; virtual;
    procedure Log(Verbosity :TVerbosityLevel; Msg :string = ''); overload;
    procedure Log(Tag :string; Msg :string; Verbosity :TVerbosityLevel = vlNormal); overload; virtual;
  public
    constructor Create(Owner: TComponent);    overload; override;
    constructor Create(Owner: TDanteElement); reintroduce; overload; virtual;

    class function XMLTag :string; virtual;
    
    procedure ParseXML(Node :MiniDom.IElement);               virtual;
    function  ParseXMLChild(Child :MiniDom.IElement):boolean; virtual;
    procedure ParseError(Msg :string; Line :Integer);
    procedure Validate;                              virtual;

    function  AsXML     :string;                virtual;
    function  ToXML(Dom :IDocument) : IElement; virtual;

    procedure SetProperty(Name, Value :string);          virtual;
    function  PropertyDefined(Name :string): boolean;    virtual;
    function  PropertyValue(Name :string) :string;       virtual;
    function  EnvironmentValue(Name :string): string;    virtual;
    function  ExpandMacros(Value :string) :string;       virtual;

    function  SetAttribute(Name, Value :string) :boolean; virtual;

    // use this to get the fully qualified base path
    function  BasePath :string; virtual;
    // use this function in Tasks to let the user specify relative
    // directories that work consistently
    function  ToSystemPath(Path :string):string;
    function  ToDantePath(Path :string) :string;
    function  ToAbsolutePath(Path :string) :string; virtual;
    function  ToRelativePath(Path :string) :string; virtual;
    procedure AboutToScratchPath(Path :TPath);

    property Project: TProject read GetProject;
    property Tag stored False;
  published
    property Name : string read FName write FName stored True;
  end;

  TProject = class(TDanteElement)
  protected
    FTargets:       TList;
    FDefaultTarget: string;
    FVerbosity:     TVerbosityLevel;
    FBaseDir:       string;
    FRunPath:       string;
    FDescription:   string;
    FProperties:    TStrings;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetTarget(Index: Integer):TTarget;
    procedure BuildSchedule(TargetName :string; Sched :TList);
    procedure DoParseXML(Node :MiniDom.IElement);
    procedure SetBaseDir(Path :TPath);
    function  GetBaseDir :TPath;

  public
    constructor Create(Owner: TDanteElement = nil); override;
    destructor  Destroy; override;

    class function XMLTag :string; override;
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
    function  ToAbsolutePath(SubPath :string) :string; override;

    function  AsString:     string;
    function  AddTarget(Name :string) :TTarget;
    function  TargetCount:  Integer;

    function  GetTargetByName(Name :string):TTarget;

    procedure SetProperty(Name, Value :string);          override;
    function  PropertyDefined(Name :string): boolean;    override;
    function  PropertyValue(Name :string) :string;       override;
    function  EnvironmentValue(Name :string): string;    override;
    function  ExpandMacros(Value :string) :string;       override;

    procedure SetProperties(Value :TStrings);

    function  Schedule(Target :string) :TTargetArray;
    procedure Build(Targets :array of string); overload;
    procedure Build(Target :string = ''); overload;

    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal); override;

    property RunPath :string read FRunPath;
    property Targets[i: Integer]: TTarget read GetTarget; default;
    property Names[TargetName :string] :TTarget read GetTargetByName;

  published
    function  CreateTarget: TTarget;

    property Name stored True;
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



  TTarget = class(TDanteElement)
  protected
    FTasks:   TList;
    FDepends: string;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetTask(Index: Integer):TTask;
  public
    constructor Create(Owner: TDanteElement); override;
    destructor  Destroy; override;

    class function XMLTag :string; override;
    function  ParseXMLChild(Child :MiniDom.IElement):boolean; override;
    function  ToXML(Dom :IDocument) : IElement; override;

    function TaskCount: Integer;
    procedure Build;

    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal); override;

    property Tasks[i: Integer]: TTask read GetTask; default;
  published
    property Name stored True;
    property Depends: string read FDepends write FDepends;
  end;



  TTask = class(TDanteElement)
  protected
    procedure DoExecute;

  public
    class function XMLTag :string; override;

    function Target :TTarget;

    procedure Execute; virtual; abstract;
    procedure Log(Msg :string = ''; Verbosity :TVerbosityLevel = vlNormal); override;

    property Name stored False;
  published
  end;


function  FindTask(Tag :string): TTaskClass;
procedure RegisterTask(TaskClass :TTaskClass);
procedure RegisterTasks(TaskClasses :array of TTaskClass);

function CommaTextToArray(Text :string) :TStringArray;
function StringsToSystemPathList(List: TStrings): string;

procedure RaiseLastSystemError(Msg :string = '');
procedure DanteError(Msg :string = '');
procedure TaskError(Msg :string = '');
procedure TaskFailure(Msg :string = '');

implementation

var
  __TaskRegistry :TStringList = nil;


procedure RaiseLastSystemError(Msg :string = '');
begin
  raise ETaskError.Create(SysErrorMessage(GetLastError) + Msg)
end;

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


function StringsToSystemPathList(List: TStrings): string;
var
  i, p  :Integer;
  Paths :TStringArray;
begin
  Result := '';
  Paths  := nil;
  for i := 0 to List.Count-1 do
  begin
    Paths := CommaTextToArray(List[i]);
    for p := Low(Paths) to High(Paths) do
      Result := Result + ';' + ToSystemPath(Paths[p]);
  end;
end;


procedure DanteError(Msg :string = '');
begin
   raise EDanteError.Create('!!! ' + Msg + ' !!!' );
end;

procedure TaskError(Msg :string);
begin
   raise ETaskError.Create('!!! ' + Msg + ' !!!' );
end;

procedure TaskFailure(Msg :string);
begin
   raise ETaskFailure.Create(Msg);
end;


{ TDanteElement }

constructor TDanteElement.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
end;

constructor TDanteElement.Create(Owner: TComponent);
begin
  Self.Create(Owner as TDanteElement);
end;

function TDanteElement.GetChildOwner: TComponent;
begin
  Result := self;
end;

procedure TDanteElement.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to ComponentCount - 1 do
  begin
    Proc(Components[I]);
  end;
end;


function TDanteElement.GetOwner: TPersistent;
begin
  Result := inherited GetOwner;
end;

function TDanteElement.GetProject: TProject;
begin
  if self is TProject then
     Result := TProject(self)
  else if Owner is TProject then
    Result := TProject(Owner)
  else if Owner is TDanteElement then
    Result := TDanteElement(Owner).Project
  else
    Result := nil;
end;

class function TDanteElement.XMLTag: string;
const
  Comp = 'component';
  Elem = 'element';
begin
  Result := copy(ClassName, 2, 255);
  Result := LowerCase(Result);
  if Pos(Comp, Result) = (1 + Length(Result) - Length(Comp)) then
    Result := StringReplace(Result, Comp, '', [])
  else if Pos(Elem, Result) = (1 + Length(Result) - Length(Elem)) then
    Result := StringReplace(Result, Elem, '', []);
end;


function TDanteElement.NewName: string;
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

procedure TDanteElement.Validate;
begin
  // do nothing
end;

procedure TDanteElement.ParseXML(Node: IElement);
var
  i     :IIterator;
  valid :boolean;
  child :MiniDom.INode;
  elem  :MiniDom.IElement;
  text  :MiniDom.ITextNode;
begin
  if Node.Name <> Self.XMLTag then
    ParseError(Format('XML tag of class <%s> is <%s> but found <%s>',
                      [ClassName, XMLTag, NOde.Name]
                      ), Node.LineNo);

  i := Node.Attributes.Iterator;
  while i.HasNext do
  begin
    with i.Next as IAttribute do
    begin
      valid := False;
      try
        valid := Self.SetAttribute(Name, ExpandMacros(Value));
      except
        on e :Exception do
          ParseError(e.Message, Node.LineNo);
      end;
      if not valid then
        ParseError(Format('Unknown attribute <%s>.%s', [XMLTag, Name]), Node.LineNo);
    end;
  end;

  i := Node.Children.Iterator;
  while i.HasNext do
  begin
    child := i.Next as INode;
    if 0 = child.QueryInterface(IElement, elem)  then
    begin
      if not ParseXMLChild(elem) then
        ParseError(Format('Unknown element <%s><%s>', [XMLTag, elem.Name] ), Child.LineNo);
    end
    else if 0 = child.QueryInterface(ITextNode, text)  then
    begin
      if not SetAttribute('text', trim(text.text)) then
        ParseError(Format('Element <%s> does not accept text', [XMLTag]), Child.LineNo);
    end;
  end;

  Validate;
end;

function TDanteElement.SetAttribute(Name, Value: string): boolean;
var
  TypeInfo :PTypeInfo;
  PropInfo :PPropInfo;
begin
  Result := True;

  TypeInfo := Self.ClassInfo;
  PropInfo := GetPropInfo(TypeInfo, Name);
  if PropINfo = nil then
    PropInfo := GetPropInfo(TypeInfo, '_' + Name);
  if PropInfo = nil then
     Result := False
  else if not IsStoredProp(Self, PropInfo) then
    Result := True
  else begin
    try
      with PropInfo^, PropType^^ do
      begin
        if Kind in [tkString, tkLString, tkWString] then
          SetStrProp(Self, PropInfo, Value)
        else if Kind in [tkInteger] then
          SetOrdProp(Self, PropInfo, StrToInt(Value))
        else if Kind in [tkEnumeration] then
          SetOrdProp(Self, PropInfo, GetEnumValue(PropType^, Value))
        else
          Result := False;
      end;
    except
      on e :EDanteParseException do
        raise;
      on e :Exception do
        raise EDanteException.Create(Format('ERROR: "%s", setting value of attribute %s.%s', [e.Message, XMLTag, Name]));
    end;
  end;
end;

function TDanteElement.ParseXMLChild(Child: IElement) :boolean;
var
  MethodName :string;
  Method     :TMethod;
  Comp       :TDanteElement;
begin
  Result := true;
  if Child.Name = 'property' then
  begin
    {@TODO !!! Make property be an element of TDanteElement (procedure CreateProperty etc.): }
    if Child.attribute('name') = nil then
      ParseError('"name" attribute required', Child.LineNo)
    else if Child.attribute('value') = nil then
      ParseError('"value" attribute required', Child.LineNo)
    else if Child.Attributes.Size <> 2 then
      ParseError('only "name" and "value" attributes supported', Child.LineNo);
    SetProperty(Child.attributeValue('name'), Child.attributeValue('value'))
  end
  else if Child.Name = 'echo' then
    Log('echo', ExpandMacros(Child.attributeValue('message')))
  else
  begin
    Method.Data  := Self;
    MethodName   := 'Create' + Child.Name;
    Method.Code  := MethodAddress(MethodName);
    if Method.Code = nil then
      Result := False
    else begin
      Comp := TCreateElementMethod(Method)();
      if Comp <> nil then
        Comp.ParseXML(Child)
      else
        Result := false;
    end;
  end;
end;

procedure TDanteElement.ParseError(Msg: string; Line :Integer);
begin
  raise EDanteParseException.Create(Format('(%d): %s',[Line, Msg]));
end;

function TDanteElement.AsXML: string;
begin
  Result := toXML(TDocument.Create).toString;
end;

function TDanteElement.ToXML(Dom :MiniDom.IDocument) :MiniDom.IElement;
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

function TDanteElement.BasePath: string;
begin
  Result := Project.BasePath;
end;

function TDanteElement.ToAbsolutePath(Path: string): string;
begin
  Result := Project.ToAbsolutePath(Path);
end;

function TDanteElement.ToRelativePath(Path: string): string;
begin
  Result := WildPaths.ToRelativePath(Path, BasePath);
end;

procedure TDanteElement.AboutToScratchPath(Path: TPath);
begin
  if  PathExists(Path)
  and PathIsAbsolute(ToRelativePath(Path))
  then
    TaskError(Format('Will not scratch %s outside of %s',
                         [ToSystemPath(Path), ToSystemPath(BasePath)]
                         ));
end;

function TDanteElement.GetChildrenTyped(AClass: TDanteElementClass): TDanteElementArray;
var
  List :TList;
  C    :TComponent;
  i    :Integer;
begin
  List := TList.Create;
  try
    for i := 0 to ComponentCount-1 do
    begin
      C := Components[i];
      if C.InheritsFrom(AClass) then
        List.Add(C);
    end;
    SetLength(Result, List.Count);
    for i := 0 to List.Count-1 do
      Result[i] := List[i];
    finally
    List.Free
  end;
end;

procedure TDanteElement.Log(Msg: string; Verbosity: TVerbosityLevel);
begin
  Project.Log(Msg, Verbosity);
end;

procedure TDanteElement.Log(Verbosity: TVerbosityLevel; Msg: string);
begin
  Log(Msg, Verbosity);
end;

procedure TDanteElement.Log(Tag, Msg: string; Verbosity: TVerbosityLevel);
var
  Lines :TStrings;
  i     :Integer;
begin
  Msg := WrapText(Msg, '...@@... ', [' ',#13,#10,#9], 64);
  Lines := TStringList.Create;
  try
    JclStrings.StrToStrings(Msg, '@@', Lines);
    for i := 0 to Lines.Count-1 do
      Project.Log(Format(LabeledMsgFormat, ['['+ Tag +']', Lines[i]]), Verbosity);
  finally
    Lines.Free;
  end;
end;

function TDanteElement.PropertyDefined(Name: string): boolean;
begin
  Result := Project.PropertyDefined(Name);
end;

function TDanteElement.PropertyValue(Name: string): string;
begin
  Result := Project.PropertyValue(Name);
end;

procedure TDanteElement.SetProperty(Name, Value: string);
begin
  Project.SetProperty(Name, Value);
end;

function TDanteElement.ExpandMacros(Value: string): string;
begin
  Result := Project.ExpandMacros(Value);
end;

function TDanteElement.EnvironmentValue(Name: string): string;
begin
  Result := Project.EnvironmentValue(Name);
end;

function TDanteElement.ToDantePath(Path: string): string;
begin
  Result := WildPaths.ToPath(Path, BasePath);
end;

function TDanteElement.ToSystemPath(Path: string): string;
begin
  Result := ToAbsolutePath(Path);
  Result := ToRelativePath(Result);
  Result := WildPaths.ToSystemPath(Result);
end;

{ TProject }

constructor TProject.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  FTargets    := TList.Create;
  FProperties := TStringList.Create;
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


function TProject.CreateTarget: TTarget;
begin
  Result := TTarget.Create(self);
end;

function TProject.AddTarget(Name: string): TTarget;
begin
  Result := CreateTarget;
  Result.Name := Name;
end;

function TProject.GetTarget(Index: Integer): TTarget;
begin
  Result := FTargets[Index];
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
var
  t :Integer;
begin
  Result := nil;
  for t := 0 to FTargets.Count-1 do
  begin
    if TTarget(FTargets[t]).Name = Name then
    begin
      Result := FTargets[t];
      break;
    end;
  end;
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

function TProject.Schedule(Target :string): TTargetArray;
var
  Sched :TList;
  i     :Integer;
begin
  Sched := TList.Create;
  try
    BuildSchedule(Target, Sched);
    SetLength(Result, Sched.Count);
    for i := 0 to Sched.Count-1 do
      Result[i] := Sched[i];
  finally
    Sched.Free;
  end;
end;

procedure TProject.Log(Msg: string; Verbosity :TVerbosityLevel);
begin
  if Self.Verbosity >= Verbosity then
    writeln(Msg);
end;

procedure TProject.Build(Target: string);
var
  i     :Integer;
  Sched :TTargetArray;
begin
  if Target = '' then
  begin
    if Default = '' then
       raise ENoDefaultTargetError.Create('No default target')
    else
      Target := Default;
  end;
  Sched := nil;
  try
    Sched := Schedule(Target);
    for i := Low(Sched) to High(Sched) do
    begin
      try
        Sched[i].Build;
        Log;
      finally
        ChangeDir(FRunpath);
      end;
    end;
  except
    on e :ETaskException do
      raise;
    on e :ETargetException do
      raise;
    on e :Exception do
    begin
      Log(vlErrors, Format('%s: %s', [e.ClassName, e.Message]));
      raise;
    end;
  end;
end;


procedure TProject.Build(Targets :array of string);
var
  t     :Integer;
  Sched :TTargetArray;
begin
  Sched := nil;
  if Length(Targets) = 0 then
    Build
  else begin
    for t := Low(Targets) to High(Targets) do
      Build(Targets[t])
  end;
end;

function TProject.ToAbsolutePath(SubPath: string): string;
begin
  Result := PathConcat(Self.BasePath, SubPath);
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

function TProject.PropertyValue(Name: string): string;
begin
  Result := ExpandMacros(Properties.Values[Name]);
end;

function TProject.EnvironmentValue(Name: string): string;
begin
  JclSysInfo.GetEnvironmentVar(Name, Result, True);
end;


// XML handling

procedure TProject.LoadXML(const SystemPath: string);
var
  Dom :IDocument;
begin
  try
    Dom := MiniDom.ParseToDom(SystemPath);
    Self.DoParseXML(Dom.Root);
  except
    on e:EDanteParseException do
    begin
      Log(vlErrors, Format('%s %s', [SystemPath, e.Message]));
      raise;
    end;
    on e:Exception do
    begin
      Log(vlErrors, Format('%s %s', [SystemPath, e.Message]));
      ParseError(e.Message, 0);
    end;
  end;
end;

class function TProject.XMLTag: string;
begin
  Result := 'project';
end;

procedure TProject.DoParseXML(Node: IElement);
begin
  try
    ParseXML(Node);
  except
    on e :EDanteParseException do
      raise;
    on e :Exception do
    begin
      Log(vlErrors, e.Message);
      ParseError(e.Message, Node.LineNo);
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
  Result := WildPaths.ToRelativePath(FBaseDir, FRunPath);
end;

procedure TProject.SetBaseDir(Path: TPath);
begin
  FBaseDir := WildPaths.ToRelativePath(Path, FRunPath);
end;


function TProject.ExpandMacros(Value: string): string;
type
  TMacroExpansion = function(Name :string) :string of object;

  function Expand(StartPat, EndPat, Val :string; MacroExpansion : TMacroExpansion) :string;
  var
    MacroStart,
    MacroEnd    :Integer;
    SubPropName :string;
  begin
    Result := Val;
    MacroStart := StrSearch(StartPat, Result);
    while MacroStart <> 0 do
    begin
      MacroEnd := StrSearch(EndPat, Result, macroStart+1);
      if MacroEnd =0  then
        break
      else begin
        SubPropName := StrMid(Result, MacroStart+2, -2 + MacroEnd-MacroStart);
        Delete(Result, MacroStart, 3 + Length(SubPropName));
        Insert(MacroExpansion(SubPropName), Result, MacroStart);
        MacroStart := StrSearch(StartPat, Result);
      end;
    end;
  end;

begin
  Properties.Values['basedir'] := BasePath;
  Result := Value;
  Result := Expand('%{', '}', Result, EnvironmentValue);
  Result := Expand('${', '}', Result, PropertyValue);
end;



procedure TProject.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TTarget then
  begin
    if Operation = opInsert then
      FTargets.Add(AComponent)
    else
      FTargets.Remove(AComponent)
  end;
end;

{ TTarget }

procedure TTarget.Build;
var
  i :Integer;
begin
  Project.Log;
  Log;
  for i := 0 to TaskCount-1 do
    Tasks[i].DoExecute;
end;

constructor TTarget.Create(Owner: TDanteElement);
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

function TTarget.TaskCount: Integer;
begin
  Result := FTasks.Count;
end;


function TTarget.ParseXMLChild(Child: IElement): boolean;
var
  Task :TTask;
begin
  Result := inherited ParseXMLChild(Child);
  if not Result then
  begin
    Task := FindTask(Child.Name).Create(Self);
    Task.ParseXML(child);
    Result := true;
  end;
end;

function TTarget.ToXML(Dom: IDocument): IElement;
var
  i :Integer;
begin
  Result := inherited ToXML(Dom);
  for i := 0 to TaskCount-1 do
    Result.Add(Tasks[i].toXML(Dom));
end;

procedure TTarget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TTask then
  begin
    if Operation = opInsert then
      FTasks.Add(AComponent)
    else
      FTasks.Remove(AComponent)
  end;
end;

{ TTask }

procedure TTask.Log(Msg: string; Verbosity :TVerbosityLevel);
begin
  Project.Log(XMLTag, Msg, Verbosity);
end;

function TTask.Target: TTarget;
begin
  Result := Owner as TTarget;
end;

procedure TTask.DoExecute;
begin
  try
    try
      ChangeDir(BasePath);
      Execute;
    except
      on e :EDanteException do
      begin
        Log(vlErrors, e.Message);
        raise;
      end;
      on e :Exception do
      begin
        Log(vlErrors, e.Message);
        TaskFailure(e.Message);
      end;
    end;
  finally
    ChangeDir(BasePath);
  end;
end;

class function TTask.XMLTag: string;
begin
  Result := copy(ClassName, 2, 255);
  Result := LowerCase(Result);
  Result := StringReplace(Result, 'task', '', []);
end;


{ TaskRegistry }

function FindTask(Tag :string) :TTaskClass;
var
  Index :Integer;
begin
  Result := nil;
  Index := __TaskRegistry.IndexOf(Tag);
  if Index < 0 then
    TaskError(Format('Task class <%s> not found', [Tag]) )
  else
    Result := Pointer(__TaskRegistry.Objects[Index])
end;


procedure RegisterTask(TaskClass :TTaskClass);
var
  Index :Integer;
begin
  Index :=__TaskRegistry.IndexOf(TaskClass.XMLTag);
  if Index >= 0 then
    TaskError(Format('Duplicate task tag <%s> in class <%s>', [TaskClass.XMLTag, TaskClass.ClassName]))
  else
  begin
    __TaskRegistry.AddObject(TaskClass.XMLTag, Pointer(TaskClass));
    if GetClass(TaskClass.ClassName) = nil then
      RegisterClass(TaskClass);
  end;
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

