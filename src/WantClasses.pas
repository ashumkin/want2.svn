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

  JclSysUtils,
  JclSysInfo,
  JclStrings,
  JclFileUtils,
  LogMgr,

  OwnedTrees;


{$M+} { TURN ON RTTI (RunTime Type Information) }

const
  DanteBuildFileName = 'dante.xml';
  AntBuildFileName   = 'build.xml';

  SupportedPropertyTypes = [
     tkInteger,
     tkEnumeration,
     tkString,
     tkLString,
     tkWString];

  LabeledMsgFormat = '%14s %s';

  vlErrors     = LogMgr.vlErrors;
  vlWarnings   = LogMgr.vlWarnings;
  vlVeryQuiet  = LogMgr.vlVeryQuiet;
  vlQuiet      = LogMgr.vlQuiet;
  vlNormal     = LogMgr.vlNormal;
  vlVerbose    = LogMgr.vlVerbose;
  vlDebug      = LogMgr.vlDebug;

type
  TLogLevel = LogMgr.TLogLevel;

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

  TStringArray = array of string;

  TDanteElementArray = array of TDanteElement;

  TTargetArray = array of TTarget;

  TCreateElementMethod = function: TDanteElement of object;

  TDanteElement = class(TTree)
  protected
    FName   : string;
    FId     : string;      // element Id
    FBaseDir: TPath;       // where paths for this object are based

    FProperties:  TStrings;
    FAttributes:  TStrings;

    FDescription:   string;

    class function SynthesizeTagName(Suffix :string): string; virtual;

    function  GetChild(i :Integer):TDanteElement;

    function  GetBaseDir: TPath;              virtual;
    procedure SetBaseDir(const Value: TPath); virtual;

    procedure SetID(Value: string); virtual;

    function GetOwner: TDanteElement; reintroduce;
    function GetProject: TProject;

    function  GetChildrenTyped(AClass: TDanteElementClass = nil):  TDanteElementArray;

    procedure Log(Msg: string = ''; Level: TLogLevel = vlNormal); overload; virtual;
    procedure Log(Level: TLogLevel; Msg: string = '');            overload;

    function  Log(const Format: string; const Args: array of const; Level: TLogLevel = vlNormal): string; overload;
    function  Log(Level: TLogLevel; const Format: string; const Args: array of const): string; overload;

    procedure Log(Tag: string; Msg: string; Level: TLogLevel = vlNormal);  overload; virtual;

    procedure RequireAttribute(Name: string);
    procedure RequireAttributes(Names: array of string);
    procedure AttributeRequiredError(AttName: string);

  public
    constructor Create(Owner: TDanteElement); reintroduce; overload; virtual;
    destructor Destroy; override;

    class function TagName: string; virtual;

    procedure SetUp(Name :string; Atts :TStrings);                         virtual;
    function  SetupChild(ChildName :string; Atts :TStrings):TDanteElement; virtual;
    procedure Init;   virtual;

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
    procedure AboutToScratchPath(const Path: TPath);

    property  Project: TProject      read GetProject;
    property  Owner :  TDanteElement read GetOwner;

    property id     :    string   read FId         write SetId;
    property basedir:    TPath    read GetBaseDir  write SetBaseDir;
    property Properties: TStrings read FProperties write SetProperties;
    property Attributes: TStrings read FAttributes write SetAttributes;
    property Name:       string   read FName       write FName stored True;

    property Children[i :Integer] :TDanteElement read GetChild;
  published
    property Tag :  string        read TagName stored False;
    property Description: string  read FDescription write FDescription;
  end;



  TProject = class(TDanteElement)
  protected
    FTargets:       TList;
    FDefaultTarget: string;
    FVerbosity:      TLogLevel;
    FRootPath:      TPath;  // root for all path calculations
    FRootPathSet:   boolean;

    FLogManager :TLogManager;
    FOnLog      :TLogMethod;

    procedure InsertNotification(Child :TTree); override;
    procedure RemoveNotification(Child :TTree); override;

    function  GetTarget(Index: Integer):TTarget;
    procedure BuildSchedule(TargetName: string; Sched: TList);

    procedure SetBaseDir(const Value: TPath); override;
    function  GetBaseDir: TPath;              override;

    procedure SetRootPath(const Path :TPath);

  public
    constructor Create(Owner: TDanteElement = nil); override;
    destructor  Destroy; override;

    procedure SetInitialBaseDir(Path: TPath);

    class function TagName: string; override;

    class function DefaultBuildFileName: TPath;
    function FindBuildFile(BuildFile: TPath; SearchUp :boolean = False):TPath; overload;
    function FindBuildFile(SearchUp :boolean= False) :TPath; overload;

    function  FindChild(Id: string; ChildClass: TClass = nil): TDanteElement;

    // use this to get the fully qualified base path
    function  BasePath: string; override;
    // use this function in Tasks to let the user specify relative
    // directories that work consistently

    function  AddTarget(Name: string): TTarget;
    function  TargetCount:  Integer;

    function  GetTargetByName(Name: string):TTarget;


    function  Schedule(Target: string): TTargetArray;
    procedure Build(Target: string = '');      overload; virtual;

    procedure Log(Tag: string; Msg: string; Level: TLogLevel = vlNormal);  override;

    property RootPath: TPath read FRootPath write SetRootPath;

    property Targets[i: Integer]: TTarget             read GetTarget; default;
    property TargetNames[TargetName: string]: TTarget read GetTargetByName;

    property LogManager: TLogManager read FLogManager write FLogManager;
  published
    function CreateTarget    : TTarget;


    property basedir;

    property Name stored True;
    property _Default:  string read FDefaultTarget  write FDefaultTarget;
    property Verbosity:   TLogLevel
      read   FVerbosity
      write  FVerbosity
      stored False
      default vlNormal;

    property OnLog :TLogMethod read FOnLog write FOnLog;
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

    function TaskCount: Integer;
    procedure Build; virtual;

    procedure Log(Msg: string = ''; Level: TLogLevel = vlNormal);          override;

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
    procedure Log(Tag: string; Msg: string; Level: TLogLevel = vlNormal);  override;

    property Name stored False;
  published
  end;


function  FindTask(Tag: string): TTaskClass;
procedure RegisterTask(TaskClass: TTaskClass);
procedure RegisterTasks(TaskClasses: array of TTaskClass);

function  FindElement(Tag :string; AppliedTo :TClass = nil) :TDanteElementClass;

procedure RegisterElement(ElementClass :TDanteElementClass);                                            overload;
procedure RegisterElement(AppliesTo, ElementClass :TDanteElementClass);                                 overload;
procedure RegisterElements(ElementClasses:array of TDanteElementClass);                                 overload;
procedure RegisterElements(AppliesTo : TDanteElementClass; ElementClasses:array of TDanteElementClass); overload;

function  TextToArray(const Text: string; const Delimiter :string = ','): TStringArray;

procedure RaiseLastSystemError(Msg: string = '');
procedure DanteError(Msg: string = '');
procedure TaskError(Msg: string = '');
procedure TaskFailure(Msg: string = '');

function CallerAddr: Pointer;
{$IFNDEF DELPHI5_UP}
const FreeAndNil : procedure(var Obj) = JclSysUtils.FreeAndNil;
{$ENDIF}

implementation

type
  TElementRecord = record
    _TagName      :string;
    _ElementClass :TDanteElementClass;
    _AppliesTo    :TDanteElementClass;
  end;

var
  __ElementRegistry :array of TElementRecord;


function  FindElement(Tag :string; AppliedTo :TClass) :TDanteElementClass;
var
  i :Integer;
begin
  Assert(Tag <> '');

  Tag := LowerCase(Tag);
  Result := nil;
  // going from High to Low lets customizer override existing elements
  for i := High(__ElementRegistry) downto Low(__ElementRegistry) do
    with __ElementRegistry[i] do
    begin
      if (_TagName <> Tag) then
        continue;
      if (AppliedTo = nil)
      or (_AppliesTo = nil)
      or (AppliedTo.InheritsFrom(_AppliesTo))
      then
      begin
          Result := _ElementClass;
          Break;
      end;
    end;
end;

procedure RegisterElement(ElementClass :TDanteElementClass);
begin
  RegisterElement(TDanteElementClass(nil), ElementClass);
end;

procedure RegisterElement(AppliesTo, ElementClass :TDanteElementClass); overload;
var
  pos :Integer;
begin
  Assert(ElementClass <> nil);

  pos := Length(__ElementRegistry);
  SetLength(__ElementRegistry, 1 + pos);

  with __ElementRegistry[pos] do
  begin
    _ElementClass := ElementClass;
    _TagName      := ElementClass.TagName;
    _AppliesTo    := AppliesTo;
  end;
end;

procedure RegisterElements(ElementClasses:array of TDanteElementClass);
begin
  RegisterElements(TDanteElementClass(nil), ElementClasses);
end;

procedure RegisterElements(AppliesTo : TDanteElementClass; ElementClasses:array of TDanteElementClass);
var
  i :Integer;
begin
  for i := Low(ElementClasses) to High(ElementClasses) do
    RegisterElement(AppliesTo, ElementClasses[i]);
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
  RegisterElement(TTarget, TaskClass);
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
    FreeAndNil(S);
  end;
end;


function IsBadPointer(P: Pointer):boolean; register;
begin
  try
    Result  := (p = nil)
              or ((Pointer(P^) <> P) and (Pointer(P^) = P));
  except
    Result := false
  end
end;


function CallerAddr: Pointer; assembler;
const
  CallerIP = $4;
asm
   mov   eax, ebp
   call  IsBadPointer
   test  eax,eax
   jne   @@Error

   mov   eax, [ebp].CallerIP
   sub   eax, 5   // 5 bytes for call

   push  eax
   call  IsBadPointer
   test  eax,eax
   pop   eax
   je    @@Finish

@@Error:
   xor eax, eax
@@Finish:
end;

procedure DanteError(Msg: string = '');
begin
   raise EDanteError.Create(Msg + '!' ) at CallerAddr;
end;

procedure TaskError(Msg: string);
begin
   raise ETaskError.Create(Msg + '!' ) at CallerAddr;;
end;

procedure TaskFailure(Msg: string);
begin
   raise ETaskFailure.Create(Msg) at CallerAddr;
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
  FreeAndNil(FAttributes);
  FreeAndNil(FProperties);
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

class function TDanteElement.SynthesizeTagName(Suffix: string): string;
begin
  Result := ClassName;
  if StrLeft(Result, 1) = 'T' then
    Delete(Result, 1, 1);
  if StrRight(Result, Length(Suffix)) = Suffix then
    Delete(Result, 1 + Length(Result) - Length(Suffix), Length(Suffix));
  Result := LowerCase(Result);
end;

class function TDanteElement.TagName: string;
begin
  Result := SynthesizeTagName('Element');
end;


procedure TDanteElement.Init;
begin
  // do nothing
end;

procedure TDanteElement.SetUp(Name: string; Atts: TStrings);
var
  i        :Integer;
  LastDir  :TPath;
begin
  Log(vlDebug, 'SetUp %s', [Name]);

  LastDir := CurrentDir;
  ChangeDir(BasePath);
  try
    if Name <> Self.TagName then
      DanteError(Format('XML tag of class <%s> is <%s> but found <%s>',
                        [ClassName, TagName, Name]
                        ));

    for i := 0 to Atts.Count-1 do
    begin
       if not Self.SetAttribute(Atts.Names[i], Atts.Values[Atts.Names[i]]) then
         DanteError(Format('Unknown attribute <%s>.%s', [TagName, Atts.Names[i]]));
    end;

    Log(vlDebug, 'Init, BasePath ="%s"', [BasePath]);
    ChangeDir(BasePath);

    Self.Init;
  finally
    ChangeDir(LastDir);
  end;
end;

function TDanteElement.HasAttribute(Name: string): boolean;
begin
  Result := FAttributes.IndexOf(Name) >= 0;
end;


function TDanteElement.SetAttribute(Name, Value: string): boolean;
begin
  if (Name = 'if') or (Name = 'unless')
  or (Name = 'ifdef') or (Name = 'ifndef')
  then
    Result := true // Do nothing. Conditionals are processed elsewhere
  else
  begin
    Result := true;
    if (Name = 'text') then
    begin
       if Trim(Value) = '' then
         EXIT // ignore it
       else
         Value := TrimRight(Value);
    end;
    Log(vlDebug, 'attribute %s="%s"', [Name,ExpandMacros(Value)]);
    FAttributes.Values[Name] := Value;
    Result := SetDelphiProperty(Name, ExpandMacros(Value));
  end;
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

function TDanteElement.SetupChild(ChildName: string; Atts: TStrings) :TDanteElement;
var
  MethodName: string;
  Method    : TMethod;
  ElemClass : TDanteElementClass;
  CondPropName: string;
begin
  Result := nil;
  // conditionals

  CondPropName := Atts.Values['if'];
  if CondPropName = '' then
    CondPropName := Atts.Values['ifdef'];
  if (CondPropName <> '')
  and not PropertyDefined(CondPropName) then
  begin
    Log(vlDebug, 'skipping <%s> because "%s" not defined', [ChildName, CondPropName]);
    EXIT;
  end;

  CondPropName := Atts.Values['unless'];
  if CondPropName = '' then
    CondPropName := Atts.Values['ifndef'];
  if (CondPropName <> '')
  and PropertyDefined(CondPropName) then
  begin
    Log(vlDebug, 'skipping <%s> because "%s" defined', [ChildName, CondPropName]);
    EXIT;
  end;

  Method.Data  := Self;
  MethodName   := 'Create' + ChildName;
  Method.Code  := MethodAddress(MethodName);

  if Method.Code <> nil then
    Result := TCreateElementMethod(Method)()
  else
  begin
    ElemClass := FindElement(ChildName, Self.ClassType);
    if ElemClass <> nil then
      Result := ElemClass.Create(Self)
    else
      DanteError(Format('Unknown element <%s><%s>', [TagName, ChildName] ));
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
    FreeAndNil(List);
  end;
end;

procedure TDanteElement.Log(Msg: string; Level: TLogLevel);
begin
  Log('', Msg, Level);
end;

procedure TDanteElement.Log(Level: TLogLevel; Msg: string);
begin
  Log(Msg, Level);
end;

procedure TDanteElement.Log(Tag, Msg: string; Level: TLogLevel);
begin
  Owner.Log(Tag, Msg, Level);
end;

function TDanteElement.Log(const Format: string; const Args: array of const; Level: TLogLevel): string;
begin
  Log(SysUtils.Format(Format, Args), Level);
end;

function TDanteElement.Log(Level: TLogLevel; const Format: string; const Args: array of const): string;
begin
  Log(Format, Args, Level);
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

procedure TDanteElement.RequireAttributes(Names: array of string);
var
  i :Integer;
begin
  for i := Low(Names) to High(Names) do
    RequireAttribute(Names[i]);
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
        if (Name = 'TPath')
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
  FreeAndNil(FTargets);
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
    DanteError(Format('Target "%s" not found',[Name]));
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
    FreeAndNil(Sched);
  end;
end;

procedure TProject.Log(Tag, Msg: string; Level: TLogLevel);
begin
  if Assigned(FOnLog) then
    FOnLog(Tag, Msg, Level)
  else if LogManager <> nil then
  begin
    if Tag <> '' then
      LogManager.Log(Format('%14s ', [ Tag ]), Msg, Level)
    else
      LogManager.Log(Msg, Level);
  end;
end;

procedure TProject.Build(Target: string = '');
var
  i:       Integer;
  Sched:   TTargetArray;
  LastDir: TPath;
begin
  Log(vlDebug, 'runpath="%s"',  [RootPath]);
  Log(vlDebug, 'basepath="%s"', [BasePath]);
  Log(vlDebug, 'basedir="%s"',  [BaseDir]);

  Sched := nil;
  if Target = '' then
  begin
    if _Default <> '' then
      Target := _Default
    else
      raise ENoDefaultTargetError.Create('No default target');
  end;

  Sched := Schedule(Target);
  LastDir := CurrentDir;
  for i := Low(Sched) to High(Sched) do
  begin
    try
      Sched[i].Build;
    finally
      ChangeDir(LastDir);
    end;
  end;
end;


// XML handling

class function TProject.TagName: string;
begin
  Result := 'project';
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
  Result := WildPaths.ToRelativePath(PropertyValue('basedir'), RootPath);
end;

procedure TProject.SetRootPath(const Path: TPath);
begin
  if not FRootPathSet then
  begin
    Project.Log(vlDebug, 'rootpath="%s"', [ RootPath ] );
    FRootPath := Path;
    FRootPathSet := True;
  end;
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


class function TProject.DefaultBuildFileName: TPath;
var
  AppName :string;
begin
  AppName := ExtractFileName(GetModulePath(hInstance));
  Result  := ChangeFileExt(LowerCase(AppName),'.xml');
end;


function TProject.FindBuildFile(BuildFile: TPath; SearchUp: boolean): TPath;
var
  Dir: TPath;
begin
  if BuildFile = '' then
    Result := FindBuildFile(SearchUp)
  else
  begin
    Result := PathConcat(BasePath, BuildFile);
    Dir    := SuperPath(BuildFile);

    Log(vlDebug, 'Looking for "%s', [Result]);
    while not PathIsFile(Result)
    and SearchUp
    and (Dir <> '')
    and (Dir <> SuperPath(Dir))
    do
    begin
      if PathIsDir(Dir) then
      begin
        Result := PathConcat(Dir, BuildFile);
        Dir    := SuperPath(Dir);
      end
      else
        break;
    end;

    if not PathIsFile(Result) then
      Result := BuildFile;
  end;
end;


function TProject.FindBuildFile(SearchUp: boolean): TPath;
begin
  Result := FindBuildFile(DefaultBuildFileName, SearchUp);
  if not PathIsFile(Result) then
     Result := FindBuildFile(DanteBuildFileName, SearchUp);
  if not PathIsFile(Result) then
     Result := FindBuildFile(AntBuildFileName, SearchUp);
  if not PathIsFile(Result) then
     Result := DefaultBuildFileName;
end;


{ TTarget }

procedure TTarget.Build;
var
  i: Integer;
begin
  Project.Log;
  Log(Description);

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
  FreeAndNil(FTasks);
end;


class function TTarget.TagName: string;
begin
  Result := 'target';
end;

function TTarget.GetTask(Index: Integer): TTask;
begin
  Result := FTasks[Index];
end;

procedure TTarget.Log(Msg: string; Level: TLogLevel);
begin
  Project.Log(Format('%s: %s', [Name, Msg]), Level);
end;

function TTarget.TaskCount: Integer;
begin
  Result := FTasks.Count;
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

procedure TTask.Log(Tag, Msg: string; Level: TLogLevel);
begin
  inherited Log('['+TagName+']'+Tag, Msg, Level);
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

  if Description <> '' then
    Log(Description);

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
  Result := SynthesizeTagName('Task');
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

