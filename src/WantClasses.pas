(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit WantClasses;

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

  OwnedTrees;


{$M+} { TURN ON RTTI (RunTime Type Information) }

const
  AntBuildFileName   = 'build.xml';

  SupportedPropertyTypes = [
     tkInteger,
     tkEnumeration,
     tkString,
     tkLString,
     tkWString];

  LabeledMsgFormat = '%14s %s';

type
  TLogLevel = ( vlErrors,
                vlWarnings,
                vlNormal,
                vlVerbose,
                vlDebug     );

const
  vlVeryQuiet = vlErrors;
  vlQuiet     = vlWarnings;

type
  TScriptElement = class;
  TScriptElementClass = class of TScriptElement;
  TScriptElementClassArray = array of TScriptElementClass;

  TProject       = class;
  TTarget        = class;
  TTask          = class;
  TTaskClass     = class of TTask;

  TBuildListener = class;


  EWantException   = class(Exception);
  EWantError       = class(EWantException);
  ETargetException  = class(EWantException);


  ENoDefaultTargetError     = class(ETargetException);
  ETargetNotFoundException  = class(ETargetException);
  ECircularTargetDependency = class(ETargetException);


  ETaskException    = class(EWantException);
  ETaskError       = class(ETaskException);
  ETaskFailure     = class(ETaskException);

  TStringArray = array of string;

  TScriptElementArray = array of TScriptElement;

  TTargetArray = array of TTarget;

  TCreateElementMethod = function: TScriptElement of object;

  TScriptElement = class(TTree)
  protected
    FName   : string;
    FId     : string;      // element Id
    FBaseDir: TPath;       // where paths for this object are based

    FProperties:  TStrings;
    FAttributes:  TStrings;

    FDescription:   string;

    FIf     :string;
    FUnless :string;

    class function SynthesizeTagName(Suffix :string): string; virtual;

    function  GetChild(i :Integer):TScriptElement;

    function  GetBaseDir: TPath;              virtual;
    procedure SetBaseDir(const Value: TPath); virtual;

    procedure SetID(Value: string); virtual;

    function GetOwner: TScriptElement; reintroduce;
    function GetProject: TProject;

    function  GetChildrenTyped(AClass: TScriptElementClass = nil):  TScriptElementArray;

    procedure Log(Msg: string = ''; Level: TLogLevel = vlNormal); overload; virtual;
    procedure Log(Level: TLogLevel; Msg: string = '');            overload;

    function  Log(const Format: string; const Args: array of const; Level: TLogLevel = vlNormal): string; overload;
    function  Log(Level: TLogLevel; const Format: string; const Args: array of const): string; overload;

    procedure RequireAttribute(Name: string);
    procedure RequireAttributes(Names: array of string);
    procedure AttributeRequiredError(AttName: string);

    procedure Init;   virtual;

    function GetListener :TBuildListener;
  public
    constructor Create(Owner: TScriptElement); reintroduce; overload; virtual;
    destructor Destroy; override;

    class function TagName: string; virtual;

    function  Enabled :boolean; virtual;
    procedure SetUp(Name :string; Atts :TStrings); virtual;
    function  SetupChild(ChildName :string; Atts :TStrings):TScriptElement; virtual;
    procedure Initialize; virtual;

    procedure SetProperty(Name, Value: string);          virtual;
    function  PropertyDefined(Name: string): boolean;    virtual;
    function  PropertyValue(Name: string): string;       virtual;
    function  EnvironmentValue(Name: string): string;    virtual;
    function  Evaluate(Value: string): string;       virtual;

    procedure SetProperties(Value: TStrings);

    function  HasAttribute(Name :string) : boolean;
    function  SetAttribute(Name, Value: string): boolean; virtual;
    function  GetAttribute(Name :string) : string;        virtual;

    function  GetDelphiProperty(Name :string) :Variant;
    function  SetDelphiProperty(Name, Value :string) :boolean;

    // use this to get the fully qualified base path
    function  BasePath: string; virtual;
    // use this function in Tasks to let the user specify relative
    // directories that work consistently
    function  ToSystemPath(const Path: TPath; const Base: TPath = ''):string;
    function  ToWantPath(Path: TSystemPath): TPath;
    function  ToAbsolutePath(const Path: TPath): TPath; virtual;
    function  ToRelativePath(const Path: TPath; const Base: TPath = ''): TPath; virtual;
    procedure AboutToScratchPath(const Path: TPath);

    property  Project: TProject      read GetProject;
    property  Owner :  TScriptElement read GetOwner;

    property id     :    string   read FId         write SetId;
    property basedir:    TPath    read GetBaseDir  write SetBaseDir;
    property Properties: TStrings read FProperties write SetProperties;
    property Attributes: TStrings read FAttributes;
    property Name:       string   read FName       write FName stored True;

    property Children[i :Integer] :TScriptElement read GetChild;
  published
    property Listener :TBuildListener read GetListener;

    property Tag :  string        read TagName stored False;
    property Description: string  read FDescription write FDescription;

    property _if    :string read FIf     write FIf;
    property unless :string read FUnless write FUnless;

    property ifdef  :string read FIf     write FIf;
    property ifndef :string read FUnless write FUnless;
  end;



  TProject = class(TScriptElement)
  protected
    FTargets:       TList;
    FDefaultTarget: string;
    FVerbosity:     TLogLevel;
    FRootPath:      TPath;  // root for all path calculations
    FRootPathSet:   boolean;

    FListener :TBuildListener;

    procedure InsertNotification(Child :TTree); override;
    procedure RemoveNotification(Child :TTree); override;

    function  GetTarget(Index: Integer):TTarget;
    procedure BuildSchedule(TargetName: string; Sched: TList);

    procedure SetBaseDir(const Value: TPath); override;
    function  GetBaseDir: TPath;              override;

    procedure SetRootPath(const Path :TPath);

  public
    constructor Create(Owner: TScriptElement = nil); override;
    destructor  Destroy; override;

    procedure SetInitialBaseDir(Path: TPath);

    class function TagName: string; override;

    class function DefaultBuildFileName: TPath;
    function FindBuildFile(BuildFile: TPath; SearchUp :boolean = False):TPath; overload;
    function FindBuildFile(SearchUp :boolean= False) :TPath; overload;

    function  FindChild(Id: string; ChildClass: TClass = nil): TScriptElement;

    // use this to get the fully qualified base path
    function  BasePath: string; override;
    // use this function in Tasks to let the user specify relative
    // directories that work consistently

    function  AddTarget(Name: string): TTarget;
    function  TargetCount:  Integer;

    function  GetTargetByName(Name: string):TTarget;


    function  Schedule(Target: string): TTargetArray;
    procedure Build(Target: string = '');      overload; virtual;

    procedure Log(Msg: string; Level: TLogLevel = vlNormal);  override;

    property RootPath: TPath read FRootPath write SetRootPath;

    property Targets[i: Integer]: TTarget             read GetTarget; default;
    property TargetNames[TargetName: string]: TTarget read GetTargetByName;

    property Listener: TBuildListener read FListener write FListener;
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
  end;



  TTarget = class(TScriptElement)
  protected
    FTasks:   TList;
    FDepends: string;

    procedure InsertNotification(Child :TTree); override;
    procedure RemoveNotification(Child :TTree); override;

    function GetTask(Index: Integer):TTask;
  public
    constructor Create(Owner: TScriptElement); override;
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



  TTask = class(TScriptElement)
  protected
    procedure DoExecute;
    procedure TaskFailure(Msg: string = '');
  public
    class function TagName: string; override;

    function BasePath: string; override;
    function Target: TTarget;

    procedure Execute; virtual;
    property Name stored False;
  published
  end;


  TBuildListener = class
  protected
    FLevel        :TLogLevel;

    procedure LogLine(Msg: string; Level: TLogLevel = vlNormal); virtual; abstract;
  public
    procedure Log(Level: TLogLevel; Msg: string = '');               overload;
    procedure Log(Msg: string = ''; Level: TLogLevel = vlNormal);    overload;

    procedure BuildFileLoaded(Project :TProject; FileName :string); virtual;

    procedure BuildStarted(Project :TProject);   virtual;
    procedure BuildFinished(Project :TProject);  virtual;
    procedure BuildFailed(Project :TProject; Msg :string = ''); virtual;

    procedure TargetStarted(Target :TTarget);    virtual;
    procedure TargetFinished(Target :TTarget);   virtual;

    procedure TaskStarted( Task :TTask);         virtual;
    procedure TaskFinished(Task :TTask);         virtual;
    procedure TaskFailed(  Task :TTask; Msg :string);  virtual;

    property Level :TLogLevel read FLevel write FLevel;
  end;


function  FindTask(Tag: string): TTaskClass;
procedure RegisterTask(TaskClass: TTaskClass);
procedure RegisterTasks(TaskClasses: array of TTaskClass);

function  FindElement(Tag :string; AppliedTo :TClass = nil) :TScriptElementClass;

procedure RegisterElement(ElementClass :TScriptElementClass);                                            overload;
procedure RegisterElement(AppliesTo, ElementClass :TScriptElementClass);                                 overload;
procedure RegisterElements(ElementClasses:array of TScriptElementClass);                                 overload;
procedure RegisterElements(AppliesTo : TScriptElementClass; ElementClasses:array of TScriptElementClass); overload;

function  TextToArray(const Text: string; const Delimiter :string = ','): TStringArray;

procedure RaiseLastSystemError(Msg: string = '');
procedure WantError(Msg: string = '');
procedure TaskError(Msg: string = '');

function CallerAddr: Pointer;
{$IFNDEF DELPHI5_UP}
const FreeAndNil : procedure(var Obj) = JclSysUtils.FreeAndNil;
{$ENDIF}

implementation

type
  TElementRecord = record
    _TagName      :string;
    _ElementClass :TScriptElementClass;
    _AppliesTo    :TScriptElementClass;
  end;

var
  __ElementRegistry :array of TElementRecord;


function  FindElement(Tag :string; AppliedTo :TClass) :TScriptElementClass;
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

procedure RegisterElement(ElementClass :TScriptElementClass);
begin
  RegisterElement(TScriptElementClass(nil), ElementClass);
end;

procedure RegisterElement(AppliesTo, ElementClass :TScriptElementClass); overload;
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

procedure RegisterElements(ElementClasses:array of TScriptElementClass);
begin
  RegisterElements(TScriptElementClass(nil), ElementClasses);
end;

procedure RegisterElements(AppliesTo : TScriptElementClass; ElementClasses:array of TScriptElementClass);
var
  i :Integer;
begin
  for i := Low(ElementClasses) to High(ElementClasses) do
    RegisterElement(AppliesTo, ElementClasses[i]);
end;


function FindTask(Tag: string): TTaskClass;
var
  C :TScriptElementClass;
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
  raise Exception.Create(SysErrorMessage(GetLastError) + Msg)
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

procedure WantError(Msg: string = '');
begin
   raise EWantError.Create(Msg + '!' ) at CallerAddr;
end;

procedure TaskError(Msg: string);
begin
   raise ETaskError.Create(Msg + '!' ) at CallerAddr;;
end;

{ TScriptElement }

constructor TScriptElement.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FProperties := TStringList.Create;
  FAttributes := TStringList.Create;
end;

destructor TScriptElement.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(FProperties);
  inherited Destroy;
end;

function TScriptElement.GetChild(i: Integer): TScriptElement;
begin
  Result := inherited GetChild(i) as TScriptElement;
end;

function TScriptElement.GetOwner: TScriptElement;
begin
  Result := Parent as TScriptElement;
end;

function TScriptElement.GetProject: TProject;
begin
  if self is TProject then
     Result := TProject(self)
  else if Owner = nil then
    Result := nil
  else
    Result := Owner.Project;
end;

class function TScriptElement.SynthesizeTagName(Suffix: string): string;
begin
  Result := ClassName;
  if StrLeft(Result, 1) = 'T' then
    Delete(Result, 1, 1);
  if StrRight(Result, Length(Suffix)) = Suffix then
    Delete(Result, 1 + Length(Result) - Length(Suffix), Length(Suffix));
  Result := LowerCase(Result);
end;

class function TScriptElement.TagName: string;
begin
  Result := SynthesizeTagName('Element');
end;


procedure TScriptElement.Init;
begin
  // do nothing
end;

procedure TScriptElement.Initialize;
var
  a       :Integer;
  i       :Integer;
  LastDir :TPath;
begin
  if not Enabled then
    Log(vlDebug, '%s disabled', [TagName])
  else
  begin
    for a := 0 to Attributes.Count-1 do
      with Attributes do
        SetDelphiProperty(Names[a], Evaluate(Values[Names[a]]) );

    LastDir := CurrentDir;
    ChangeDir(BasePath);
    try
      Log(vlDebug, 'Init, BasePath ="%s"', [BasePath]);
      ChangeDir(BasePath);

      Self.Init;

      for i := 0 to ChildCount-1 do
        Children[i].Initialize;
    finally
      ChangeDir(LastDir);
    end;
  end;
end;

function TScriptElement.Enabled: boolean;
var
  PropName: string;
begin
  Result := true;

  PropName := GetAttribute('if');
  if PropName = '' then
    PropName := GetAttribute('ifdef');
  PropName := Evaluate(PropName);
  if (PropName <> '')
  and not PropertyDefined(PropName) then
  begin
    Log(vlDebug, 'disabling <%s> because "%s" not defined', [TagName, PropName]);
    Result := False
  end
  else
  begin
    PropName := GetAttribute('unless');

    if PropName = '' then
      PropName := GetAttribute('ifndef');
    PropName := Evaluate(PropName);
    if (PropName <> '')
    and PropertyDefined(PropName) then
    begin
      Log(vlDebug, 'skipping <%s> because "%s" defined', [TagName, PropName]);
      Result := False;
    end;
  end
end;

procedure TScriptElement.SetUp(Name: string; Atts: TStrings);
var
  i        :Integer;
begin
  Log(vlDebug, 'SetUp %s', [Name]);

  if Name <> Self.TagName then
    WantError(Format('XML tag of class <%s> is <%s> but found <%s>',
                      [ClassName, TagName, Name]
                      ));

  for i := 0 to Atts.Count-1 do
  begin
     if not Self.SetAttribute(Atts.Names[i], Atts.Values[Atts.Names[i]]) then
       WantError(Format('Unknown attribute <%s>.%s', [TagName, Atts.Names[i]]));
  end;
end;

function TScriptElement.HasAttribute(Name: string): boolean;
begin
  Result := FAttributes.IndexOf(Name) >= 0;
end;


function TScriptElement.SetAttribute(Name, Value: string): boolean;
begin
  Log(vlDebug, 'attribute %s="%s"', [Name,Value]);
  FAttributes.Values[Name] := Value;
  Result := SetDelphiProperty(Name, Value);
end;


function TScriptElement.GetAttribute(Name: string): string;
begin
  Result := FAttributes.Values[Name];
  if Result = '' then
    Result := GetDelphiProperty(Name);
end;

function TScriptElement.SetupChild(ChildName: string; Atts: TStrings) :TScriptElement;
var
  MethodName: string;
  Method    : TMethod;
  ElemClass : TScriptElementClass;
begin
  Result := nil;
  // conditionals

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
      WantError(Format('Unknown element <%s><%s>', [TagName, ChildName]));
  end;
end;

function TScriptElement.BasePath: string;
begin
  if (Owner = nil) or PathIsAbsolute(FBaseDir) then
    Result := FBaseDir
  else
    Result := PathConcat((Owner as TScriptElement).BasePath, FBaseDir);
end;

function TScriptElement.ToAbsolutePath(const Path: TPath): TPath;
begin
  Result := PathConcat(BasePath, Path);
end;

function TScriptElement.ToRelativePath(const Path: TPath; const Base: TPath): TPath;
begin
  if Base = '' then
    Result := WildPaths.ToRelativePath(Path, Self.BasePath)
  else
    Result := WildPaths.ToRelativePath(Path, Base);
end;

procedure TScriptElement.AboutToScratchPath(const Path: TPath);
begin
  if  PathExists(Path)
  and PathIsAbsolute(ToRelativePath(Path))
  then
    TaskError(Format('Will not scratch %s outside of %s',
                         [ToSystemPath(Path), ToSystemPath(BasePath)]
                         ));
end;

function TScriptElement.GetChildrenTyped(AClass: TScriptElementClass): TScriptElementArray;
var
  List: TList;
  E   : TScriptElement;
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

procedure TScriptElement.Log(Msg: string; Level: TLogLevel);
begin
  Project.Log(Msg, Level);
end;

procedure TScriptElement.Log(Level: TLogLevel; Msg: string);
begin
  Log(Msg, Level);
end;

function TScriptElement.Log(const Format: string; const Args: array of const; Level: TLogLevel): string;
begin
  Log(SysUtils.Format(Format, Args), Level);
end;

function TScriptElement.Log(Level: TLogLevel; const Format: string; const Args: array of const): string;
begin
  Log(Format, Args, Level);
end;


function TScriptElement.ToWantPath(Path: TSystemPath): TPath;
begin
  Result := WildPaths.ToPath(Path, BasePath);
end;

function TScriptElement.ToSystemPath(const Path: TPath; const Base: TPath): TSystemPath;
begin
  Result := ToRelativePath(Path, ToAbsolutePath(Base));
  Result := WildPaths.ToSystemPath(Result);
end;

procedure TScriptElement.AttributeRequiredError(AttName: string);
begin
  WantError(Format('"%s" attribute is required', [AttName]));
end;

procedure TScriptElement.RequireAttribute(Name: string);
begin
  if GetAttribute(Name) = '' then
    AttributeRequiredError(Name);
end;

procedure TScriptElement.RequireAttributes(Names: array of string);
var
  i :Integer;
begin
  for i := Low(Names) to High(Names) do
    RequireAttribute(Names[i]);
end;

function TScriptElement.GetBaseDir: TPath;
begin
  if Owner <> nil then
    Result := Owner.ToRelativePath(FBaseDir)
  else
    Result := FBaseDir;
end;

procedure TScriptElement.SetBaseDir(const Value: TPath);
begin
  FBaseDir := Value;
  //!!!SetProperty('basedir', BasePath);
end;

procedure TScriptElement.SetID(Value: string);
begin
  FId := Value;
end;



procedure TScriptElement.SetProperties(Value: TStrings);
begin
  Assert(Value <> nil);
  FProperties.Assign(Value);
end;

procedure TScriptElement.SetProperty(Name, Value: string);
begin
  Assert(Name <> '');
  if not PropertyDefined(Name) then
    Properties.Values[Name] := Value;
end;

function TScriptElement.PropertyDefined(Name: string): boolean;
begin
  Assert(Name <> '');
  Result :=   (Properties.IndexOfName(Name) >= 0)
           or (Owner <> nil) and (Owner.PropertyDefined(Name));
end;

function TScriptElement.PropertyValue(Name: string): string;
begin
  Assert(Name <> '');
  if Properties.IndexOfName(Name) >= 0 then
    Result := Evaluate(Properties.Values[Name])
  else if Owner <> nil then
    Result := Owner.PropertyValue(Name)
  else
    Result := '${' + Name + '}'
end;

function TScriptElement.EnvironmentValue(Name: string): string;
begin
  Assert(Name <> '');
  JclSysInfo.GetEnvironmentVar(Name, Result, True);
end;


function TScriptElement.Evaluate(Value: string): string;
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
      Result := Copy(Result, 1, MacroStart+1) + Evaluate(Copy(Result, MacroStart+2, Length(Result)));
      MacroEnd := StrSearch(EndPat, Result, macroStart+1);
      if MacroEnd =0  then
        break
      else begin
        SubPropName := StrMid(Result, MacroStart+2, -2 + MacroEnd-MacroStart);
        Delete(Result, MacroStart, 3 + Length(SubPropName));
        Insert(MacroExpansion(SubPropName), Result, MacroStart);
        MacroStart := StrSearch(StartPat, Result, macroEnd+1);
      end;
    end;
  end;

begin
  Result := Value;
  Result := Expand('%{', '}', Result, EnvironmentValue);
  Result := Expand('${', '}', Result, PropertyValue);
end;

function TScriptElement.GetDelphiProperty(Name: string): Variant;
var
  TypeInfo :PTypeInfo;
  PropInfo :PPropInfo;
begin
  Result := Null;
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

function TScriptElement.SetDelphiProperty(Name, Value: string) :boolean;
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
          WantError(Format('expected system-independent path but got: "%s"', [Value]) );
        SetStrProp(Self, PropInfo, Value);
      end
      else if Kind in [tkInteger] then
        SetOrdProp(Self, PropInfo, StrToInt(Value))
      else if Kind in [tkEnumeration] then
      begin
        if Name = 'TLogLevel' then
          Value := 'vl' + Value;
          
        if Name <> 'Boolean' then
          SetOrdProp(Self, PropInfo, GetEnumValue(PropType^, Value))
        else
        begin
          Value := LowerCase(Value);
          if (Value = 'true') or (Value = 'yes') or (Value = 'on') then
            SetOrdProp(Self, PropInfo, GetEnumValue(PropType^, 'true'))
          else
            SetOrdProp(Self, PropInfo, GetEnumValue(PropType^, 'false'));
        end
      end
      else
        Result := False;
    end;
  end;
end;

function TScriptElement.GetListener: TBuildListener;
begin
  Result := Project.Listener;
end;

{ TProject }

constructor TProject.Create(Owner: TScriptElement);
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

function TProject.FindChild(Id: string; ChildClass: TClass): TScriptElement;
var
  E   : TScriptElement;
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
    WantError(Format('element id="%s" not found', [Id]));
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
    WantError(Format('Target "%s" not found',[Name]));
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
  if not Target.Enabled then
  begin
    Log(vlVerbose, 'Skipping disabled target "%s"', [Target.TagName]);
    EXIT;
  end;

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

procedure TProject.Log(Msg: string; Level: TLogLevel);
begin
  if Listener <> nil then
      Listener.Log(Msg, Level);
end;

procedure TProject.Build(Target: string = '');
var
  i:       Integer;
  Sched:   TTargetArray;
  LastDir: TPath;
begin
  with FProperties do
  begin
    if Verbosity >= vlVerbose then
      Values['verbose'] := 'true'
    else if PropertyDefined('verbose') then
      Delete(IndexOfName('verbose'));

    if Verbosity >= vlDebug then
      Values['debug'] := 'true'
    else if PropertyDefined('debug') then
      Delete(IndexOfName('debug'));
  end;

  Log(vlDebug, 'rootpath="%s"',  [RootPath]);
  Log(vlDebug, 'basepath="%s"',  [BasePath]);
  Log(vlDebug, 'basedir="%s"',   [BaseDir]);

  Sched := nil;
  if Target = '' then
  begin
    if _Default <> '' then
      Target := _Default
    else
      raise ENoDefaultTargetError.Create('No default target');
  end;

  Initialize;
  Sched := Schedule(Target);

  if Length(Sched) = 0 then
    Log(vlWarnings, 'Nothing to build')
  else
  begin
    LastDir := CurrentDir;
    try
      try
        if Listener <> nil then Listener.BuildStarted(Self);
        for i := Low(Sched) to High(Sched) do
        begin
            ChangeDir(BasePath);
            Sched[i].Build;
        end;
        if Listener <> nil then
          Listener.BuildFinished(Self);
      except
        if Listener <> nil then
          Listener.BuildFailed(Self);
        raise;
      end;
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
  if not PropertyDefined('basedir') then
    Result := ''
  else
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
    Result := ToAbsolutePath(BuildFile);
    Dir    := SuperPath(Result);

    Log(vlDebug, 'Looking for "%s in "%s"', [BuildFile, Dir]);
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
        Log(vlDebug, 'Looking for "%s in "%s"', [BuildFile, Dir]);
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
  Log(vlDebug, 'Findind buildfile', [Result]);
  Result := FindBuildFile(DefaultBuildFileName, SearchUp);
  if not PathIsFile(Result) then
     Result := FindBuildFile(AntBuildFileName, SearchUp);
  if not PathIsFile(Result) then
     Result := DefaultBuildFileName;
end;


{ TTarget }

procedure TTarget.Build;
var
  i: Integer;
  LastDir :TPath;
begin
  if Listener <> nil then
    Listener.TargetStarted(Self);

  Log(vlDebug, 'basepath="%s"', [BasePath]);
  Log(vlDebug, 'basedir="%s"',  [BaseDir]);

  LastDir := CurrentDir;
  try
    ChangeDir(BasePath);

    for i := 0 to TaskCount-1 do
      Tasks[i].DoExecute;

    if Listener <> nil then
      Listener.TargetFinished(Self);
  finally
    ChangeDir(LastDir)
  end;
end;

constructor TTarget.Create(Owner: TScriptElement);
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

function TTask.Target: TTarget;
begin
  Result := Owner as TTarget;
end;

procedure TTask.DoExecute;
var
  LastDir: TPath;
begin
  if not Enabled then
    EXIT;
  if Description <> '' then
    Log(Description);

  if Listener <> nil then
    Listener.TaskStarted(Self);

  Log(vlDebug, 'basepath="%s"', [BasePath]);
  Log(vlDebug, 'basedir="%s"',  [BaseDir]);

  LastDir := CurrentDir;
  try
    try
      ChangeDir(BasePath);
      Execute;
      if Listener <> nil then
        Listener.TaskFinished(Self);
    finally
      ChangeDir(LastDir);
    end;
  except
    on e: ETaskException do
      raise;
    on e: Exception do
      TaskFailure(e.Message);
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
    Result := PathConcat((Owner as TScriptElement).BasePath, BaseDir);
end;



procedure TTask.Execute;
begin
  // do nothing
end;

procedure TTask.TaskFailure(Msg: string);
begin
  if Listener <> nil then Listener.TaskFailed(Self, Msg);
  raise ETaskFailure.Create(Msg) //at CallerAddr;
end;

{ TBuildListener }

procedure TBuildListener.BuildFileLoaded(Project: TProject; FileName: string);
begin

end;

procedure TBuildListener.BuildStarted(Project: TProject);
begin

end;

procedure TBuildListener.BuildFinished(Project: TProject);
begin

end;

procedure TBuildListener.BuildFailed(Project: TProject; Msg :string);
begin

end;

procedure TBuildListener.TargetStarted(Target: TTarget);
begin

end;

procedure TBuildListener.TargetFinished(Target: TTarget);
begin

end;

procedure TBuildListener.TaskStarted(Task: TTask);
begin

end;

procedure TBuildListener.TaskFinished(Task: TTask);
begin

end;

procedure TBuildListener.Log(Level: TLogLevel; Msg: string);
begin
  Log(Msg, Level);
end;

procedure TBuildListener.Log(Msg: string; Level: TLogLevel);
var
  Lines     :TStringList;
  i         :Integer;
begin
  if (Self.Level >= Level) then
  begin
    Lines := TStringList.Create;
    try
      Msg := Msg + ' ';
      Lines.Text := Msg;
      for i := 0 to Lines.Count-1 do
        LogLine(Lines[i], Level);
    finally
      FreeAndNil(Lines);
    end;
  end;
end;

procedure TBuildListener.TaskFailed(Task: TTask; Msg :string);
begin

end;

initialization
  __ElementRegistry := nil;
finalization
  __ElementRegistry := nil;
end.


