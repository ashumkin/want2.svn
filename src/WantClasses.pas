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
unit DanteClasses;

interface
uses
  Windows,
  SysUtils,
  Classes,
  TypInfo,

  WildPaths,

  Collections,
  MiniDom,

  JclSysInfo,
  JclStrings,

  OwnedTrees;


{$M+} { TURN ON RTTI (RunTime Type Information) }

const
  BuildFileName = 'build.xml';

  SupportedPropertyTypes = [
     tkInteger,
     tkEnumeration,
     tkString,
     tkLString,
     tkWString];

  LabeledMsgFormat = '%14s %s';

type
  TDanteElement = class;
  TDanteElementClass = class of TDanteElement;
  TDanteElementClassArray = array of TDanteElementClass;

  TProject    = class;
  TTarget     = class;
  TTask       = class;
  TTaskClass  = class of TTask;

  EDanteException   = class(Exception);
  EDanteError       = class(EDanteException);
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

  TCreateElementMethod = function: TDanteElement of object;
  TLogMethod = procedure(Msg: string; Verbosity: TVerbosityLevel) of object;


  TDanteElement = class(TTree)
  protected
    FName   : string;
    FId     : string;      // element Id
    FBaseDir: TPath;       // where paths for this object are based

    FProperties:  TStrings;
    FAttributes:  TStrings;


    function GetChild(i :Integer):TDanteElement;

    function  GetBaseDir: TPath;              virtual;
    procedure SetBaseDir(const Value: TPath); virtual;

    procedure SetID(Value: string); virtual;

    function GetOwner: TDanteElement; reintroduce;
    function GetProject: TProject;

    function  GetChildrenTyped(AClass: TDanteElementClass = nil):  TDanteElementArray;

    procedure Log(Msg: string = ''; Verbosity: TVerbosityLevel = vlNormal);           overload; virtual;
    procedure Log(Verbosity: TVerbosityLevel; Msg: string = ''); overload;

    function  Log(const Format: string; const Args: array of const; Verbosity: TVerbosityLevel = vlNormal): string; overload;
    function  Log(Verbosity: TVerbosityLevel; const Format: string; const Args: array of const): string; overload;

    procedure Log(Tag: string; Msg: string; Verbosity: TVerbosityLevel = vlNormal);  overload; virtual;

    procedure RequireAttribute(Name: string);
    procedure AttributeRequiredError(AttName: string);

    procedure ParseXML(Node: MiniDom.IElement);               virtual;
    function  ParseXMLChild(Child: MiniDom.IElement):boolean; virtual;
    procedure ParseError(Msg: string; Line: Integer);
  public
    constructor Create(Owner: TDanteElement); reintroduce; overload; virtual;

    destructor Destroy; override;

    class function TagName: string; virtual;

    procedure Init;                                       virtual;

    function  AsXML    : string;                virtual;
    function  ToXML(Dom: IDocument):  IElement; virtual;

    procedure SetProperty(Name, Value: string);          virtual;
    function  PropertyDefined(Name: string): boolean;    virtual;
    function  PropertyValue(Name: string): string;       virtual;
    function  EnvironmentValue(Name: string): string;    virtual;
    function  ExpandMacros(Value: string): string;       virtual;

    procedure SetProperties(Value: TStrings);

    function  HasAttribute(Name :string) : boolean;
    function  SetAttribute(Name, Value: string): boolean; virtual;
    function  GetAttribute(Name :string) : string;        virtual;
    procedure SetAttributes(Value :TStrings);

    function  GetDelphiProperty(Name :string) :Variant;
    function  SetDelphiProperty(Name, Value :string) :boolean;

    // use this to get the fully qualified base path
    function  BasePath: string; virtual;
    // use this function in Tasks to let the user specify relative
    // directories that work consistently
    function  ToSystemPath(const Path: TPath; const Base: TPath = ''):string;
    function  ToDantePath(Path: TSystemPath): TPath;
    function  ToAbsolutePath(const Path: TPath): TPath; virtual;
    function  ToRelativePath(const Path: TPath; const Base: TPath = ''): TPath; virtual;
    function  StringsToSystemPathList(List: TStrings; const Base: TPath = ''): TSystemPath;
    procedure AboutToScratchPath(const Path: TPath);

    property  Project: TProject      read GetProject;
    property  Owner :  TDanteElement read GetOwner;

    property id     :    string   read FId         write SetId;
    property basedir:    TPath    read GetBaseDir  write SetBaseDir;
    property Properties: TStrings read FProperties write SetProperties;
    property Attributes: TStrings read FAttributes write SetAttributes;
    property Name:  string read FName write FName stored True;

    property Children[i :Integer] :TDanteElement read GetChild;
  published
    property Tag :  string read TagName stored False;
  end;




  TProject = class(TDanteElement)
  protected
    FTargets:       TList;
    FDefaultTarget: string;
    FVerbosity:     TVerbosityLevel;
    FRootPath:      TPath;  // root for all path calculations
    FRootPathSet:   boolean;
    FDescription:   string;

    FOnLog: TLogMethod;

    procedure InsertNotification(Child :TTree); override;
    procedure RemoveNotification(Child :TTree); override;

    function  GetTarget(Index: Integer):TTarget;
    procedure BuildSchedule(TargetName: string; Sched: TList);

    procedure DoParseXML(Node: MiniDom.IElement);

    procedure SetBaseDir(const Value: TPath); override;
    function  GetBaseDir: TPath;              override;

    procedure SetRootPath(const Path :TPath);

  public
    constructor Create(Owner: TDanteElement = nil); override;
    destructor  Destroy; override;

    procedure SetInitialBaseDir(Path: TPath);

    class function TagName: string; override;
    function FindBuildFile(BuildFile: TPath = ''):string;


    function  ToXML(Dom: IDocument):  IElement; override;
    procedure ParseXMLText(const XML: string);
    procedure LoadXML(const SystemPath: TSystemPath = ''; FindFile: boolean = true);

    function  FindChild(Id: string; ChildClass: TClass = nil): TDanteElement;

    // use this to get the fully qualified base path
    function  BasePath: string; override;
    // use this function in Tasks to let the user specify relative
    // directories that work consistently

    function  AddTarget(Name: string): TTarget;
    function  TargetCount:  Integer;

    function  GetTargetByName(Name: string):TTarget;


    function  Schedule(Target: string): TTargetArray;
    procedure Build(Targets: array of string); overload;
    procedure Build(Target: string = ''); overload;

    procedure Log(Msg: string = ''; Verbosity: TVerbosityLevel = vlNormal); override;

    property RootPath: TPath read FRootPath write SetRootPath;

    property Targets[i: Integer]: TTarget             read GetTarget; default;
    property TargetNames[TargetName: string]: TTarget read GetTargetByName;

    property OnLog: TLogMethod read FOnLog write FOnLog;
  published
    function CreateTarget    : TTarget;


    property basedir;

    property Name stored True;
    property Default:       string          read FDefaultTarget  write FDefaultTarget;
    property Verbosity:     TVerbosityLevel
      read    FVerbosity
      write   FVerbosity
      stored  False
      default vlNormal;
    property Description:   string          read FDescription    write FDescription;
  end;



  TTarget = class(TDanteElement)
  protected
    FTasks:   TList;
    FDepends: string;

    procedure InsertNotification(Child :TTree); override;
    procedure RemoveNotification(Child :TTree); override;

    function GetTask(Index: Integer):TTask;
  public
    constructor Create(Owner: TDanteElement); override;
    destructor  Destroy; override;

    class function TagName: string; override;
    function  ToXML(Dom: IDocument):  IElement; override;

    function TaskCount: Integer;
    procedure Build;

    procedure Log(Msg: string = ''; Verbosity: TVerbosityLevel = vlNormal); override;

    property Tasks[i: Integer]: TTask read GetTask; default;
  published
    property Name stored True;
    property Depends: string read FDepends write FDepends;
  end;



  TTask = class(TDanteElement)
  protected
    procedure DoExecute;
  public
    class function TagName: string; override;

    function BasePath: string; override;
    function Target: TTarget;

    procedure Execute; virtual;
    procedure Log(Msg: string = ''; Verbosity: TVerbosityLevel = vlNormal); override;

    property Name stored False;
  published
  end;


function  FindTask(Tag: string): TTaskClass;
procedure RegisterTask(TaskClass: TTaskClass);
procedure RegisterTasks(TaskClasses: array of TTaskClass);

function  FindElement(Tag :string; AppliedTo :TClass = nil) :TDanteElementClass;
procedure RegisterElement(ElementClass :TDanteElementClass; AppliesTo : TDanteElementClass = nil); overload;
procedure RegisterElement(ElementClass :TDanteElementClass; AppliesTo : TDanteElementClassArray); overload;

function  TextToArray(const Text: string; const Delimiter :string = ','): TStringArray;

procedure RaiseLastSystemError(Msg: string = '');
procedure DanteError(Msg: string = '');
procedure TaskError(Msg: string = '');
procedure TaskFailure(Msg: string = '');

implementation

type
  TElementRecord = record
    ElementClass :TDanteElementClass;
    AppliesTo    :TDanteElementClassArray;
  end;

var
  __ElementRegistry :array of TElementRecord;


function  FindElement(Tag :string; AppliedTo :TClass) :TDanteElementClass;
var
  i :Integer;
  c :Integer;
begin
  Assert(Tag <> '');

  Tag := LowerCase(Tag);
  Result := nil;
  // going from High to Low lets customizer override existing elements 
  for i := High(__ElementRegistry) downto Low(__ElementRegistry) do
    with __ElementRegistry[i] do
    begin
      if (ElementClass.TagName <> Tag) then
        continue;
      if AppliedTo = nil then
      begin
          Result := ElementClass;
          Break;
      end;
      for c := Low(AppliesTo) to High(AppliesTo) do
      begin
        if AppliedTo.InheritsFrom(AppliesTo[c]) then
        begin
          Result := ElementClass;
          Break;
        end;
      end;
    end;
end;

procedure RegisterElement(ElementClass :TDanteElementClass; AppliesTo :TDanteElementClassArray);
var
  pos :Integer;
begin
  Assert(ElementClass <> nil);
  Assert(Length(AppliesTo) > 0);

  pos := Length(__ElementRegistry);
  SetLength(__ElementRegistry, 1 + pos);

  __ElementRegistry[pos].ElementClass := ElementClass;
  __ElementRegistry[pos].AppliesTo    := AppliesTo;
end;

procedure RegisterElement(ElementClass :TDanteElementClass; AppliesTo : TDanteElementClass);
var
  Applies :TDanteElementClassArray;
begin
  Assert(ElementClass <> nil);

  if AppliesTo = nil then
    AppliesTo := TDanteElement;

  SetLength(Applies, 1);
  Applies[0] := AppliesTo;
  RegisterElement(ElementClass, Applies);
end;

function FindTask(Tag: string): TTaskClass;
var
  C :TDanteElementClass;
begin
  Result := nil;
  C := FindElement(Tag, TTarget);
  if (C = nil) or not C.InheritsFrom(TTask) then
    TaskError(Format('Task class <%s> not found', [Tag]) )
  else
    Result := TTaskClass(C);
end;


procedure RegisterTask(TaskClass: TTaskClass);
begin
  RegisterElement(TaskClass, TTarget);
end;

procedure RegisterTasks(TaskClasses: array of TTaskClass);
var
  i: Integer;
begin
  for i := Low(TaskClasses) to High(TaskClasses) do
    RegisterTask(TaskClasses[i]);
end;

procedure RaiseLastSystemError(Msg: string = '');
begin
  raise ETaskError.Create(SysErrorMessage(GetLastError) + Msg)
end;

function TextToArray(const Text: string; const Delimiter :string): TStringArray;
var
  S: TStrings;
  i: Integer;
begin
  S := TStringList.Create;
  try
    JclStrings.StrToStrings(Text, Delimiter, S);
    SetLength(Result, S.Count);
    for i := 0 to S.Count-1 do
       Result[i] := Trim(S[i]);
  finally
    S.Free;
  end;
end;


procedure DanteError(Msg: string = '');
begin
   raise EDanteError.Create('!!! ' + Msg + ' !!!' );
end;

procedure TaskError(Msg: string);
begin
   raise ETaskError.Create('!!! ' + Msg + ' !!!' );
end;

procedure TaskFailure(Msg: string);
begin
   raise ETaskFailure.Create('fail: '+ Msg);
end;

{ TDanteElement }

constructor TDanteElement.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  FProperties := TStringList.Create;
  FAttributes := TStringList.Create;
end;

destructor TDanteElement.Destroy;
begin
  FAttributes.Free;
  FProperties.Free;
  inherited Destroy;
end;

function TDanteElement.GetChild(i: Integer): TDanteElement;
begin
  Result := inherited GetChild(i) as TDanteElement;
end;

function TDanteElement.GetOwner: TDanteElement;
begin
  Result := Parent as TDanteElement;
end;

function TDanteElement.GetProject: TProject;
begin
  if self is TProject then
     Result := TProject(self)
  else if Owner = nil then
    Result := nil
  else
    Result := Owner.Project;
end;

class function TDanteElement.TagName: string;
const
  Elem = 'element';
begin
  Result := copy(ClassName, 2, 255);
  Result := LowerCase(Result);
  if Pos(Elem, Result) = (1 + Length(Result) - Length(Elem)) then
    Result := StringReplace(Result, Elem, '', []);
end;


procedure TDanteElement.Init;
begin
  // do nothing
end;

procedure TDanteElement.ParseXML(Node: IElement);
var
  i    : IIterator;
  valid: boolean;
  child: MiniDom.INode;
  elem : MiniDom.IElement;
  text : MiniDom.ITextNode;
begin
  Log(vlDebug, 'Parsing %s', [Node.Name]);
  if Node.Name <> Self.TagName then
    ParseError(Format('XML tag of class <%s> is <%s> but found <%s>',
                      [ClassName, TagName, NOde.Name]
                      ), Node.LineNo);

  i := Node.Attributes.Iterator;
  while i.HasNext do
  begin
    with i.Next as IAttribute do
    begin
      valid := False;
      try
        valid := Self.SetAttribute(Name, Value);
      except
        on e: Exception do
          ParseError(e.Message, Node.LineNo);
      end;
      if not valid then
        ParseError(Format('Unknown attribute <%s>.%s', [TagName, Name]), Node.LineNo);
    end;
  end;

  Log(vlDebug, 'Init, BasePath ="%s"', [BasePath]);
  ChangeDir(BasePath);
  Self.Init;

  i := Node.Children.Iterator;
  while i.HasNext do
  begin
    child := i.Next as INode;
    if 0 = child.QueryInterface(IElement, elem)  then
    begin
      if not ParseXMLChild(elem) then
        ParseError(Format('Unknown element <%s><%s>', [TagName, elem.Name] ), Child.LineNo);
    end
    else if 0 = child.QueryInterface(ITextNode, text)  then
    begin
      if not SetAttribute('text', trim(text.text)) then
        ParseError(Format('Element <%s> does not accept text', [TagName]), Child.LineNo);
    end;
  end;
end;


function TDanteElement.HasAttribute(Name: string): boolean;
begin
  Result := FAttributes.IndexOf(Name) >= 0;
end;


function TDanteElement.SetAttribute(Name, Value: string): boolean;
begin
  FAttributes.Values[Name] := Value;
  Result := SetDelphiProperty(Name, ExpandMacros(Value));
  Log(vlDebug, 'attribute %s="%s"', [Name,ExpandMacros(Value)]);
end;


function TDanteElement.GetAttribute(Name: string): string;
begin
  Result := FAttributes.Values[Name];
  if Result = '' then
    Result := GetDelphiProperty(Name);

  Result := ExpandMacros(Result);
end;

procedure TDanteElement.SetAttributes(Value: TStrings);
begin
  FAttributes.Assign(Value);
end;

function TDanteElement.ParseXMLChild(Child: IElement): boolean;
var
  MethodName: string;
  Method    : TMethod;
  ElemClass : TDanteElementClass;
  Elem      : TDanteElement;
begin
  Result := true;

  Elem := nil;

  Method.Data  := Self;
  MethodName   := 'Create' + Child.Name;
  Method.Code  := MethodAddress(MethodName);

  if Method.Code <> nil then
    Elem := TCreateElementMethod(Method)()
  else
  begin
    ElemClass := FindElement(Child.Name, Self.ClassType);
    if ElemClass <> nil then
      Elem := ElemClass.Create(Self);
  end;

  if Elem <> nil then
    Elem.ParseXML(Child)
  else
    Result := false;
end;

procedure TDanteElement.ParseError(Msg: string; Line: Integer);
begin
  raise EDanteParseException.Create(Format('(%d): %s',[Line, Msg]));
end;

function TDanteElement.AsXML: string;
begin
  Result := toXML(TDocument.Create).toString;
end;

function TDanteElement.ToXML(Dom: MiniDom.IDocument): MiniDom.IElement;
var
  TypeInfo:  PTypeInfo;
  PropList:  PPropList;
  PropInfo:  PPropInfo;
  PropCount: Integer;
  i:         Integer;
  PropName:  string;
  PropValue: string;
begin
  Result := Dom.NewElement(TagName);

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
  if (Owner = nil) or PathIsAbsolute(FBaseDir) then
    Result := FBaseDir
  else
    Result := PathConcat((Owner as TDanteElement).BasePath, FBaseDir);
end;

function TDanteElement.ToAbsolutePath(const Path: TPath): TPath;
begin
  Result := PathConcat(BasePath, Path);
end;

function TDanteElement.ToRelativePath(const Path: TPath; const Base: TPath): TPath;
begin
  if Base = '' then
    Result := WildPaths.ToRelativePath(Path, Self.BasePath)
  else
    Result := WildPaths.ToRelativePath(Path, Base);
end;

function TDanteElement.StringsToSystemPathList(List: TStrings; const Base: TPath): TSystemPath;
var
  i, p : Integer;
  Paths: TStringArray;
begin
  Result := '';
  Paths  := nil;
  for i := 0 to List.Count-1 do
  begin
    Paths := TextToArray(List[i]);
    for p := Low(Paths) to High(Paths) do
      Result := Result + ';' + ToSystemPath(Paths[p], Base);
  end;
end;


procedure TDanteElement.AboutToScratchPath(const Path: TPath);
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
  List: TList;
  E   : TDanteElement;
  i   : Integer;
begin
  List := TList.Create;
  try
    for i := 0 to ChildCount-1 do
    begin
      E := Children[i];
      if (AClass = nil) or E.InheritsFrom(AClass) then
        List.Add(E);
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
  Lines: TStrings;
  i    : Integer;
begin
  Msg := Msg + ' ';
  Msg := StringReplace(Msg, #13#10,'@@', [rfReplaceAll]);
  Msg := StringReplace(Msg, #10#13,'@@', [rfReplaceAll]);
  Msg := StringReplace(Msg, #13,'@@',    [rfReplaceAll]);
  Msg := StringReplace(Msg, #10,'@@',    [rfReplaceAll]);

  Msg := WrapText(Msg, '@@   ', [' ',#13,#10,#9,';',','], 64);
  Lines := TStringList.Create;
  try
    JclStrings.StrToStrings(Msg, '@@', Lines);
    for i := 0 to Lines.Count-1 do
      Project.Log(Format(LabeledMsgFormat, ['['+ Tag +']', Lines[i]]), Verbosity);
  finally
    Lines.Free;
  end;
end;

function TDanteElement.Log(const Format: string; const Args: array of const; Verbosity: TVerbosityLevel): string;
begin
  Log(SysUtils.Format(Format, Args), Verbosity);
end;

function TDanteElement.Log(Verbosity: TVerbosityLevel; const Format: string; const Args: array of const): string;
begin
  Log(Format, Args, Verbosity);
end;


function TDanteElement.ToDantePath(Path: TSystemPath): TPath;
begin
  Result := WildPaths.ToPath(Path, BasePath);
end;

function TDanteElement.ToSystemPath(const Path: TPath; const Base: TPath): TSystemPath;
begin
  Result := ToRelativePath(Path, ToAbsolutePath(Base));
  Result := WildPaths.ToSystemPath(Result);
end;

procedure TDanteElement.AttributeRequiredError(AttName: string);
begin
  DanteError(Format('"%s" attribute is required', [AttName]));
end;

procedure TDanteElement.RequireAttribute(Name: string);
begin
  if GetAttribute(Name) = '' then
    AttributeRequiredError(Name);
end;


function TDanteElement.GetBaseDir: TPath;
begin
  if Owner <> nil then
    Result := Owner.ToRelativePath(FBaseDir)
  else
    Result := FBaseDir;
end;

procedure TDanteElement.SetBaseDir(const Value: TPath);
begin
  FBaseDir := Value;
  //!!!SetProperty('basedir', BasePath);
end;

procedure TDanteElement.SetID(Value: string);
begin
  FId := Value;
end;



procedure TDanteElement.SetProperties(Value: TStrings);
begin
  Assert(Value <> nil);
  FProperties.Assign(Value);
end;

procedure TDanteElement.SetProperty(Name, Value: string);
begin
  Assert(Name <> '');
  if not PropertyDefined(Name) then
    Properties.Values[Name] := Value;
end;

function TDanteElement.PropertyDefined(Name: string): boolean;
begin
  Assert(Name <> '');
  Result :=   (Properties.IndexOfName(Name) >= 0)
           or (Owner <> nil) and (Owner.PropertyDefined(Name));
end;

function TDanteElement.PropertyValue(Name: string): string;
begin
  Assert(Name <> '');
  if Properties.IndexOfName(Name) >= 0 then
    Result := ExpandMacros(Properties.Values[Name])
  else if Owner <> nil then
    Result := Owner.PropertyValue(Name);
end;

function TDanteElement.EnvironmentValue(Name: string): string;
begin
  Assert(Name <> '');
  JclSysInfo.GetEnvironmentVar(Name, Result, True);
end;


function TDanteElement.ExpandMacros(Value: string): string;
type
  TMacroExpansion = function(Name: string): string of object;

  function Expand(StartPat, EndPat, Val: string; MacroExpansion:  TMacroExpansion): string;
  var
    MacroStart,
    MacroEnd   : Integer;
    SubPropName: string;
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
  Result := Value;
  Result := Expand('%{', '}', Result, EnvironmentValue);
  Result := Expand('${', '}', Result, PropertyValue);
end;

function TDanteElement.GetDelphiProperty(Name: string): Variant;
var
  TypeInfo :PTypeInfo;
  PropInfo :PPropInfo;
begin
  Result := '';
  TypeInfo := Self.ClassInfo;
  PropInfo := TypInfo.GetPropInfo(Self.ClassInfo, Name);
  if PropINfo = nil then
    PropInfo := GetPropInfo(TypeInfo, '_' + Name);

  if PropInfo <> nil then
  begin
    with PropInfo^, PropType^^ do
    begin
      if IsStoredProp(Self, PropInfo)
      and (SetProc <> nil)
      and (GetProc <> nil)
      and (Kind in SupportedPropertyTypes)
      then
      begin
        if Kind in [tkString, tkLString, tkWString] then
          Result := GetStrProp(Self, PropInfo)
        else if Kind in [tkInteger] then
          Result := IntToStr(GetOrdProp(Self, PropInfo))
        else if Kind in [tkEnumeration] then
          Result := GetEnumName(PropType^, GetOrdProp(Self, PropInfo))
        else
        begin
          // do nothing
        end
      end;
    end;
  end;
end;

function TDanteElement.SetDelphiProperty(Name, Value: string) :boolean;
var
  TypeInfo: PTypeInfo;
  PropInfo: PPropInfo;
begin
  Result := True;

  TypeInfo := Self.ClassInfo;
  PropInfo := GetPropInfo(TypeInfo, Name);
  if PropINfo = nil then
    PropInfo := GetPropInfo(TypeInfo, '_' + Name);
  if PropInfo = nil then
     Result := False
  else if not IsStoredProp(Self, PropInfo) then
    Result := False
  else
  begin
    with PropInfo^, PropType^^ do
    begin
      if Kind in [tkString, tkLString, tkWString] then
      begin
        if (PropType^^.Name = 'TPath')
        and not WildPaths.IsSystemIndependentPath(Value) then
          DanteError(Format('expected system-independent path but got: "%s"', [Value]) );
        SetStrProp(Self, PropInfo, Value);
      end
      else if Kind in [tkInteger] then
        SetOrdProp(Self, PropInfo, StrToInt(Value))
      else if Kind in [tkEnumeration] then
      begin
        if Name <> 'Boolean' then
          SetOrdProp(Self, PropInfo, GetEnumValue(PropType^, Value))
        else
        begin
          Value := LowerCase(Value);
          if (Value = 'true') or (Value = 'yes') or (Value = 'on') then
            SetOrdProp(Self, PropInfo, GetEnumValue(PropType^, 'true'))
          else if (Value = 'false') or (Value = 'no') or (Value = 'off')  or (Value = '') then
            SetOrdProp(Self, PropInfo, GetEnumValue(PropType^, 'false'))
          else
            DanteError('expected one of true/false, yes/no, on/off');
        end
      end
      else
        Result := False;
    end;
  end;
end;




{ TProject }

constructor TProject.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  FTargets    := TList.Create;
  FVerbosity  := vlNormal;

  FRootPath    := CurrentDir;
  FRootPathSet := False;
end;

destructor TProject.Destroy;
begin
  FTargets.Clear;
  inherited Destroy;
  FTargets.Free;
  FTargets := nil;
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

function TProject.FindChild(Id: string; ChildClass: TClass): TDanteElement;
var
  E   : TDanteElement;
  i   : Integer;
begin
  Result := nil;
  for i := 0 to ChildCount-1 do
  begin
    E := Children[i];
    if (E.Id = Id) and ((ChildClass = nil) or E.InheritsFrom(ChildClass)) then
    begin
      Result := E;
      break;
    end;
  end;
  if Result = nil then
    DanteError(Format('element id="%s" not found', [Id]));
end;

function TProject.GetTargetByName(Name: string): TTarget;
var
  t: Integer;
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
    raise ETargetNotFoundException.Create(Format('Target "%s" not found',[Name]));
end;

procedure TProject.BuildSchedule(TargetName: string; Sched: TList);
var
  Target:  TTarget;
  i     :  Integer;
  Deps  :  TStringArray;
begin
  Target := GetTargetByName(TargetName);
  if Sched.IndexOf(Target) >= 0 then
     EXIT; // done

  Deps := TextToArray(Target.Depends);
  for i := Low(Deps) to High(Deps) do
     BuildSchedule(Deps[i], Sched);

  if Sched.IndexOf(Target) >= 0 then
     raise ECircularTargetDependency.Create(TargetName);
  Sched.Add(Target);
end;

function TProject.Schedule(Target: string): TTargetArray;
var
  Sched: TList;
  i    : Integer;
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

procedure TProject.Log(Msg: string; Verbosity: TVerbosityLevel);
begin
  if Assigned(FOnLog) then
    FOnLog(Msg, Verbosity)
  else if Self.Verbosity >= Verbosity then
  begin
    if IsConsole then writeln(Msg);
  end;
end;

procedure TProject.Build(Target: string);
var
  i    : Integer;
  Sched: TTargetArray;
  LastDir: TPath;
begin
  Log(vlDebug, 'runpath="%s"',  [RootPath]);
  Log(vlDebug, 'basepath="%s"', [BasePath]);
  Log(vlDebug, 'basedir="%s"',  [BaseDir]);
  Sched := nil;
  try
    if Target = '' then
    begin
      if Default = '' then
         raise ENoDefaultTargetError.Create('No default target')
      else
        Target := Default;
    end;

    Sched := Schedule(Target);

    LastDir := CurrentDir;
    Sched := Schedule(Target);
    for i := Low(Sched) to High(Sched) do
    begin
      try
        Sched[i].Build;
      finally
        ChangeDir(LastDir);
      end;
    end;
  except
    on e: ETaskException do
      raise;
    on e: Exception do
    begin
      Log(vlErrors, Format('!ERROR: %s', [e.Message]));
      raise;
    end;
  end;
end;


procedure TProject.Build(Targets: array of string);
var
  t    : Integer;
begin
  if Length(Targets) = 0 then
    Build
  else begin
    for t := Low(Targets) to High(Targets) do
      Build(Targets[t])
  end;
end;

// XML handling

procedure TProject.ParseXMLText(const XML :string);
var
  Dom      : IDocument;
begin
  try
    Dom := MiniDom.ParseTextToDom(XML);
    Self.DoParseXML(Dom.Root);
  except
    on e:EDanteParseException do
    begin
      Log(vlErrors, e.Message);
      raise;
    end;
    on e:Exception do
    begin
      Log(vlErrors, e.Message);
      ParseError(e.Message, 0);
    end;
  end;
end;

procedure TProject.LoadXML(const SystemPath: string; FindFile: boolean);
var
  Dom:       IDocument;
  BuildFile: TPath;
  LastDir:   TPath;
begin
  BuildFile := ToPath(SystemPath);
  if FindFile then
    BuildFile := FindBuildFile(BuildFile);
  try
    if not FRootPathSet then
      RootPath := SuperPath(ToAbsolutePath(BuildFile));
    Log(vlDebug, 'Runpath="%s"', [ RootPath ] );

    LastDir := CurrentDir;
    try
      ChangeDir(BasePath);
      Dom := MiniDom.ParseToDom(ToSystemPath(BuildFile));
      Self.DoParseXML(Dom.Root);
    finally
      ChangeDir(LastDir);
    end;
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

class function TProject.TagName: string;
begin
  Result := 'project';
end;

procedure TProject.DoParseXML(Node: IElement);
begin
  try
    ParseXML(Node);
  except
    on e: EDanteParseException do
      raise;
    on e: Exception do
    begin
      Log(vlErrors, e.Message);
      ParseError(e.Message, Node.LineNo);
    end;
  end;
end;

function TProject.ToXML(Dom: IDocument): IElement;
var
  i: Integer;
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
    Result := PathConcat(RootPath, BaseDir);
end;

procedure TProject.SetBaseDir(const Value: TPath);
begin
  inherited SetBaseDir(Value);
  SetProperty('basedir', PathConcat(RootPath, Value));
end;

function TProject.GetBaseDir: TPath;
begin
  Result := WildPaths.ToRelativePath(PropertyValue('basedir'), FRootPath);
end;

procedure TProject.SetRootPath(const Path: TPath);
begin
  FRootPath := Path;
  FRootPathSet := True;
end;

procedure TProject.SetInitialBaseDir(Path: TPath);
begin
  SetBaseDir(Path);
  Properties.Values['basedir'] := PathConcat(RootPath, Path);
end;

procedure TTarget.InsertNotification(Child: TTree);
begin
  inherited InsertNotification(Child);
  if Child is TTask then
      FTasks.Add(Child)
end;

procedure TTarget.RemoveNotification(Child: TTree);
begin
  inherited RemoveNotification(Child);
  if Child is TTask then
      FTasks.Remove(Child)
end;

function TProject.FindBuildFile(BuildFile: TPath): string;
var
  Dir: string;
begin
  if BuildFile = '' then
    BuildFile := BuildFileName;

  Result := ToAbsolutePath(BuildFile);
  Dir    := SuperPath(Result);

  while not PathIsFile(Result) do
  begin
    if PathIsDir(SuperPath(Dir)) then
    begin
      Dir := SuperPath(Dir);
      Result := PathConcat(Dir, BuildFile)
    end
    else
      break;
  end;

  if not PathIsFile(Result) then
    DanteError(Format('cannot find build file "%s" in "%s": ',[BuildFile, BaseDir]));
end;



{ TTarget }

procedure TTarget.Build;
var
  i: Integer;
begin
  Project.Log;
  Log;

  Log(vlDebug, 'basepath="%s"', [BasePath]);
  Log(vlDebug, 'basedir="%s"',  [BaseDir]);

  for i := 0 to TaskCount-1 do
  begin
    Tasks[i].DoExecute;
  end;
end;

constructor TTarget.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
  FTasks   := TList.Create;
end;

destructor TTarget.Destroy;
begin
  FTasks.Clear;
  inherited Destroy;
  FTasks.Free;
  FTasks := nil;
end;


class function TTarget.TagName: string;
begin
  Result := 'target';
end;

function TTarget.GetTask(Index: Integer): TTask;
begin
  Result := FTasks[Index];
end;

procedure TTarget.Log(Msg: string; Verbosity: TVerbosityLevel);
begin
  Project.Log(Format('%s: %s', [Name, Msg]), Verbosity);
end;

function TTarget.TaskCount: Integer;
begin
  Result := FTasks.Count;
end;


function TTarget.ToXML(Dom: IDocument): IElement;
var
  i: Integer;
begin
  Result := inherited ToXML(Dom);
  for i := 0 to TaskCount-1 do
    Result.Add(Tasks[i].toXML(Dom));
end;

procedure TProject.InsertNotification(Child: TTree);
begin
  inherited InsertNotification(Child);
  if Child is TTarget then
    FTargets.Add(Child)
end;

procedure TProject.RemoveNotification(Child: TTree);
begin
  inherited RemoveNotification(Child);
  if Child is TTarget then
    FTargets.Remove(Child)
end;

{ TTask }

procedure TTask.Log(Msg: string; Verbosity: TVerbosityLevel);
begin
  Log(TagName, Msg, Verbosity);
end;

function TTask.Target: TTarget;
begin
  Result := Owner as TTarget;
end;

procedure TTask.DoExecute;
var
  LastDir: TPath;
begin
  Log(vlDebug, 'basepath="%s"', [BasePath]);
  Log(vlDebug, 'basedir="%s"',  [BaseDir]);

  LastDir := CurrentDir;
  try
    try
      ChangeDir(BasePath);
      Execute;
    except
      on e: EDanteException do
      begin
        Log(vlErrors, e.Message);
        raise;
      end;
      on e: Exception do
      begin
        Log(vlErrors, e.Message);
        TaskFailure(e.Message);
      end;
    end;
  finally
    ChangeDir(LastDir);
  end;
end;

class function TTask.TagName: string;
begin
  Result := copy(ClassName, 2, 255);
  Result := LowerCase(Result);
  Result := StringReplace(Result, 'task', '', []);
end;


{ TaskRegistry }

function TTask.BasePath: string;
begin
  if (Owner = nil) or PathIsAbsolute(BaseDir) then
    Result := FBaseDir
  else
    Result := PathConcat((Owner as TDanteElement).BasePath, BaseDir);
end;



procedure TTask.Execute;
begin
  // do nothing
end;

initialization
  __ElementRegistry := nil;
finalization
  __ElementRegistry := nil;
end.

