{--------------------------------------------------------------------------}
{ Copyright (c) 2001, Dante Authors -- See authors.txt for complete list   }
{ All rights reserved.                                                     }
{                                                                          }
{ Redistribution and use in source and binary forms, with or without       }
{ modification, are permitted provided that the following conditions       }
{ are met:                                                                 }
{                                                                          }
{ 1. Redistributions of source code must retain the above copyright        }
{    notice, this list of conditions and the following disclaimer.         }
{                                                                          }
{ 2. Redistributions in binary form must reproduce the above copyright     }
{    notice, this list of conditions and the following disclaimer in       }
{    the documentation and/or other materials provided with the            }
{    distribution.                                                         }
{                                                                          }
{ 3. The name Dante, the names of the authors in authors.txt and the       }
{    names of other contributors to this software may not be used to       }
{    endorse or promote products derived from this software without        }
{    specific prior written permission.                                    }
{                                                                          }
{  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     }
{  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     }
{  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       }
{  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          }
{  COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   }
{  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    }
{  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS   }
{  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND  }
{  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR   }
{  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE  }
{  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.}
{                                                                          }
{--------------------------------------------------------------------------}
{ The JCL (JEDI Code Library) used in Dante is governed by                 }
{ the terms of the MPL (Mozilla Public License) found at                   }
{ http://www.mozilla.org/MPL/MPL-1.1.html.                                 }
{                                                                          }
{ The source code for JCL itself can be found on JEDI Web site:            }
{ http://delphi-jedi.org/Jedi:CODELIBJCL.                                  }
{--------------------------------------------------------------------------}
{ (based on BSD Open Source License)                                       }
{--------------------------------------------------------------------------}

unit DanteClasses;

interface

uses
  Classes,
  SysUtils,
  MiniDOM,
  LogMgr,
  WildPaths,
  DanteBase;

type
  TDanteElement = class;
  TProject      = class;
  TTarget       = class;
  TTask         = class;

  TTaskClass         = class of TTask;
  TDanteElementClass = class of TDanteElement;

  TStringArray = array of String;

  TPropertyItem = class
    private
      FName:  String;
      FValue: String;

    public
      constructor Create(const aName: String; const aValue: String);

      property    Name:  String read FName;
      property    Value: String read FValue write FValue;
  end;

  TDanteElement = class(TDanteList)
    private
      FProject: TProject;

      FProperties: TStringList;

    protected
      class function TagName: String; override;

      procedure   ClearProperties; virtual;

      procedure   Validate;                                        virtual;
      procedure   ParseXML(Node: MiniDom.IElement);                virtual;
      function    ParseXMLChild(Child: MiniDom.IElement): Boolean; virtual;

      function    SetAttribute(const aName: String; const aValue: String): Boolean; override;

      function    GetLog: TLogManager;

    public
      constructor Create(const aParent: TDanteBase); override;

      destructor  Destroy; override;

      procedure   Setup; virtual;

      procedure   ParseXMLText(const aXML: String);

      function    IsPropertyDefined(aName: String): Boolean;        virtual;
      function    GetProperty(const aName: String): String;         virtual;
      procedure   SetProperty(const aName: String; aValue: String); virtual;

      function    Get(const aName: String): TDanteElement; overload;

      function    BasePath: String; virtual;

      function    EnvironmentValue(const aName: String): String;    virtual;

      function    ExpandMacros(const aValue: String): String;

      procedure   AboutToScratchPath(Path: String);

      procedure   Log(const aMsg: String = ''; const aLevel: TLogLevel = vlNormal); overload; virtual;
      procedure   Log(const aLevel: TLogLevel; const aMsg: String = ''); overload;
      procedure   Log(const aPrefix: String; const aMsg: String; const aLevel: TLogLevel = vlNormal); overload; virtual;

      property    Project: TProject read FProject;
  end;

  TTarget = class(TDanteElement)
    private
      FDependsList: TStringList;

    protected
      class function TagName: String; override;

      function    SetAttribute(const aName: String; const aValue: String): Boolean; override;

      function    GetTask(aIndex: Integer): TTask;

      function    GetDependencyCount: Integer;
      function    GetDependencyList: String;
      function    GetDependency(aIndex: Integer): String;

    public
      constructor Create(const aParent: TDanteBase);  override;
      destructor  Destroy;                            override;

      procedure   Build;

      procedure   Add(aTask: TTask);               overload;
      function    Get(const aName: String): TTask; overload;

      function    ParseDepends(const aList: String): Boolean;

      property    Task[aIndex: Integer]: TTask read GetTask; default;

      property    DependencyCount:             Integer read GetDependencyCount;
      property    DependencyList:              String  read GetDependencyList;
      property    Dependency[aIndex: Integer]: String  read GetDependency;

  end;

  TTargetList = class(TDanteList)
    protected
      function    GetTarget(aIndex: Integer): TTarget;

    public
      procedure   Clear; override;

      procedure   Add(aTarget: TTarget);             overload;
      function    Get(const aName: String): TTarget; overload;

      property    Target[aIndex: Integer]: TTarget read GetTarget; default;
  end;

  TProject = class(TDanteElement)
    private
      FLog: TLogManager;

      FDescription:   String;

      FBuildFilename: String;
      FBaseDir:       String;
      FRunPath:       String;
      FDefaultTarget: String;

    protected
      class function TagName: String; override;

      function    FindBuildFile(const aBuildFile: String): String;

      function    GetLogLevel: TLogLevel;
      procedure   SetLogLevel(const aLogLevel: TLogLevel);

      function    GetBaseDir: String;
      procedure   SetBaseDir(const aValue: String);

      function    GetTarget(aIndex: Integer): TTarget;
      function    GetTargetByName(aTargetName: String): TTarget;

      procedure   SetTarget(aTarget: TTarget);

      procedure   LoadFile;

      procedure   BuildTarget(const aTarget: TTarget);

    public
      constructor Create(const aParent: TDanteBase); override;

      function    SetAttribute(const aName: String; const aValue: String): Boolean; override;

      procedure   Initialize(const aLog: TLogManager; const aBuildFilename: String); virtual;

      function    AddTarget(const aName: String): TTarget;

        // use this to get the fully qualified base path

      function    BasePath: String; override;

        // use this function in Tasks to let the user specify relative
        // directories that work consistently

      function    ToAbsolutePath(const aSubPath: String): String; virtual;

      procedure   AssignProperties(const aProperties: TDanteList);

      function    IsPropertyDefined(aName: String): Boolean;        override;
      function    GetProperty(const aName: String): String;         override;
      procedure   SetProperty(const aName: String; aValue: String); override;

      procedure   Schedule(const aTarget: TTarget; aTargetList: TTargetList);

      procedure   Build(const aTarget: String = ''); overload;
      procedure   Build(aTargets: array of String); overload;

      property    BuildFilename: String read FBuildFilename;
      property    BaseDir:       String read GetBaseDir write SetBaseDir;
      property    RunPath:       String read FRunPath   write FRunPath;

      property    Targets[Index: Integer]:   TTarget read GetTarget; default;
      property    Names[TargetName: String]: TTarget read GetTargetByName;

      property    LogManager: TLogManager read FLog;
      property    Verbosity: TLogLevel    read GetLogLevel write SetLogLevel;

  end;

  TTask = class(TDanteElement)
    private
      FTarget:     TTarget;
      FIsExecutable: Boolean;

    protected
      class function TagName :string; override;

      function    SetAttribute(const aName: String; const aValue: String): Boolean; override;

      procedure   DoExecute;

    public
      destructor  Destroy; override;

      procedure   Execute; virtual; abstract;

      property    Target:       TTarget read FTarget;
      property    IsExecutable: Boolean read FIsExecutable;
  end;

  TTestTask = class(TTask)
    private
      FMsg: String;

    protected
      class function TagName: String; override;

      function    SetAttribute(const aName: String; const aValue: String): Boolean; override;

    public
      procedure   Execute; override;

      property    Msg: String read FMsg;

  end;


function CommaTextToArray(const Text: String): TStringArray;
function StringsToSystemPathList(List: TStrings): String;

procedure DanteError(const Msg: String = '');
procedure TaskError(const Msg: String = '');
procedure TaskFailure(const Msg: String = '');

function  FindTask(const Tag: String): TDanteElementClass;
procedure RegisterTask(DanteClass: TDanteElementClass);
procedure RegisterTasks(DanteClasses: array of TDanteElementClass);


implementation

uses
  Collections,
  JCLSysInfo,
  JclStrings,
  FileOps,
  StandardTasks;
  //CustomTasks;

var
  __TaskRegistry: TStringList = nil;

function CommaTextToArray(const Text: String): TStringArray;
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

function StringsToSystemPathList(List: TStrings): String;
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

procedure DanteError(const Msg: String = '');
begin
  raise EDanteError.CreateFmt(F_DanteError, [Msg]);
end;

procedure TaskError(const Msg: String = '');
begin
  raise ETaskError.CreateFmt(F_TaskError, [Msg]);
end;

procedure TaskFailure(const Msg: String = '');
begin
  raise ETaskFailure.CreateFmt(F_TaskFailure, [Msg]);
end;

  { TPropertyItem }

constructor TPropertyItem.Create(const aName, aValue: String);
begin
  FName  := aName;
  FValue := aValue;
end;

  { TDanteElement }

constructor TDanteElement.Create(const aParent: TDanteBase);
begin
  inherited Create(aParent);

  if aParent is TProject then
    FProject := TProject(aParent)
  else
    FProject := nil;

  FProperties := TStringList.Create;
end;

destructor TDanteElement.Destroy;
begin
  ClearProperties;

  FreeAndNil(FProperties);

  FProject := nil;

  inherited;
end;

class function TDanteElement.TagName: String;
begin
  Result := 'DanteElement';
end;

procedure TDanteElement.ClearProperties;
var
  i: Integer;
begin
  for i := 0 to Pred(FProperties.Count) do
    TPropertyItem(FProperties.Objects[i]).Free;

  FProperties.Clear;
end;

function TDanteElement.SetAttribute(const aName, aValue: String): Boolean;
begin
  Result := inherited SetAttribute(aName, aValue);
end;

function TDanteElement.BasePath: String;
begin
  if Assigned(FProject) then
    Result := FProject.BasePath
  else
    Result := '';
end;

function TDanteElement.EnvironmentValue(const aName: String): String;
begin
  JclSysInfo.GetEnvironmentVar(aName, Result, True);
end;

function TDanteElement.GetLog: TLogManager;
begin
  if Assigned(FProject) then
    Result := Project.LogManager
  else
    Result := nil;
end;

procedure TDanteElement.Setup;
begin
  //
end;

function TDanteElement.IsPropertyDefined(aName: String): Boolean;
begin
  Result := Project.IsPropertyDefined(aName);
end;

function TDanteElement.GetProperty(const aName: String): String;
begin
  Result := Project.GetProperty(aName);
end;

procedure TDanteElement.SetProperty(const aName: String; aValue: String);
begin
  Project.SetProperty(aName, aValue);
end;

function TDanteElement.Get(const aName: String): TDanteElement;
begin
  Result := TDanteElement(inherited Get(aName));
end;

procedure TDanteElement.Validate;
begin
  // do nothing
end;

procedure TDanteElement.ParseXML(Node: MiniDom.IElement);
var
  i:       IIterator;
  Child:   MiniDom.INode;
  Attr:    MiniDom.IAttribute;
  Element: MiniDom.IElement;
  Text:    MiniDom.ITextNode;
begin
  if CompareText(Node.Name, TagName) = 0 then
    begin
      i := Node.Attributes.iterator;

      while i.HasNext do
        begin
          Attr := i.Next as IAttribute;

          try
            if not SetAttribute(Attr.Name, ExpandMacros(Attr.Value)) then
              raise EDanteParseException.CreateFmt(F_ParseAttributeError, [Node.LineNo, TagName, Attr.Name]);

          except
            on e:Exception do
              raise EDanteParseException.CreateFmt(F_ParseError, [Node.LineNo, e.Message]);
          end;
        end;

      i := Node.Children.Iterator;

      while i.HasNext do
        begin
          Child := i.Next as INode;

          if 0 = Child.QueryInterface(IElement, Element)  then
            begin
              if not ParseXMLChild(Element) then
                raise EDanteParseException.CreateFmt(F_ParseChildError, [Child.LineNo, TagName, Element.Name]);
            end
          else if 0 = child.QueryInterface(ITextNode, Text)  then
            begin
              if not SetAttribute('text', Trim(Text.text)) then
                raise EDanteParseException.CreateFmt(F_ParseChildTextError, [Child.LineNo, TagName]);
            end;
        end;

      Validate;
    end
  else
    raise EDanteParseException.Create(Format(F_ExpectedTagError, [TagName]));
end;

function TDanteElement.ParseXMLChild(Child: MiniDom.IElement): Boolean;
var
  ElementClass: TDanteElementClass;
  Element:      TDanteElement;
begin
  Result := True;

  if      CompareText(Child.Name, 'property') = 0 then
    SetProperty(Child.attributeValue('name'), Child.attributeValue('value'))
  else if CompareText(Child.Name, 'echo') = 0 then
    Log('echo', ExpandMacros(Child.attributeValue('message')))
  else
    begin
      ElementClass := FindTask(Child.Name);

      if Assigned(ElementClass) then
        begin
          Element := ElementClass.Create(Self);

          if Assigned(Element) then
            begin
              if Element is TTask then
                TTask(Element).Setup;

              Element.ParseXML(Child);

              Add(Element);
            end;
        end
      else
        Result := False;
    end;
end;

procedure TDanteElement.ParseXMLText(const aXML: string);
begin
  ParseXML(MiniDom.ParseTextToDOM(aXML).Root);
end;

procedure TDanteElement.AboutToScratchPath(Path: String);
begin
  if  PathExists(Path)
  and PathIsAbsolute(ToRelativePath(Path, BasePath))
  then
    TaskError(Format('Will not scratch %s outside of %s',
                         [ToSystemPath(Path), ToSystemPath(BasePath)]
                         ));
end;

function TDanteElement.ExpandMacros(const aValue: String): String;
type
  TMacroExpansion = function(const Name :string) :string of object;

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
  SetProperty('basedir', BasePath);

  Result := aValue;
  Result := Expand('%{', '}', Result, EnvironmentValue);
  Result := Expand('${', '}', Result, GetProperty);
end;

procedure TDanteElement.Log(const aMsg: String; const aLevel: TLogLevel);
begin
  if Assigned(FProject) and Assigned(Project.LogManager) then
    Project.LogManager.LogMsg(aLevel, aMsg, '');
end;

procedure TDanteElement.Log(const aLevel: TLogLevel; const aMsg: String);
begin
  if Assigned(FProject) and Assigned(Project.LogManager) then
    Project.LogManager.LogMsg(aLevel, aMsg, '');
end;

procedure TDanteElement.Log(const aPrefix: String; const aMsg: String; const aLevel: TLogLevel);
begin
  if Assigned(FProject) and Assigned(Project.LogManager) then
    Project.LogManager.LogMsg(aLevel, aMsg, aPrefix);
end;


  { TTarget }

constructor TTarget.Create(const aParent: TDanteBase);
begin
  inherited Create(aParent);

  FDependsList := TStringList.Create;
end;

destructor TTarget.Destroy;
begin
  inherited;

  FreeAndNil(FDependsList);
end;

function TTarget.SetAttribute(const aName: String; const aValue: String): Boolean; { override }
begin
  if CompareText(aName, 'depends') = 0 then
    Result := ParseDepends(aValue)
  else
    Result := inherited SetAttribute(aName, aValue);
end;

class function TTarget.TagName: String; { override }
begin
  Result := 'target';
end;

function TTarget.ParseDepends(const aList: String): Boolean;
begin
  FDependsList.CommaText := aList;

  Result := (FDependsList.Count > 0)
end;

procedure TTarget.Add(aTask: TTask);
begin
  inherited Add(aTask);
end;

function TTarget.Get(const aName: String): TTask;
begin
  Result := TTask(inherited Get(aName));
end;

function TTarget.GetTask(aIndex: Integer): TTask;
begin
  if (aIndex >= 0) and (aIndex < List.Count) then
    Result := TTask(List.Objects[aIndex])
  else
    Result := nil;
end;

function TTarget.GetDependency(aIndex: Integer): String;
begin
  if (aIndex >= 0) and (aIndex < FDependsList.Count) then
    Result := FDependsList[aIndex]
  else
    Result := '';
end;

function TTarget.GetDependencyCount: Integer;
begin
  Result := FDependsList.Count;
end;

function TTarget.GetDependencyList: String;
begin
  Result := FDependsList.CommaText;
end;

procedure TTarget.Build;
var
  i :Integer;
begin
  Log(Format(F_TargetStartMsg, [Name]));

  for i := 0 to Pred(List.Count) do
    Task[i].DoExecute;
end;

  { TTargetList }

procedure TTargetList.Add(aTarget: TTarget);
begin
  inherited Add(aTarget);
end;

procedure TTargetList.Clear;
begin
  FList.Clear;    // do not call inherited as each target
                  // stored within FList is a copy
end;

function TTargetList.Get(const aName: String): TTarget;
begin
  Result := TTarget(inherited Get(aName));
end;

function TTargetList.GetTarget(aIndex: Integer): TTarget;
begin
  if (aIndex >= 0) and (aIndex < List.Count) then
    Result := TTarget(List.Objects[aIndex])
  else
    Result := nil;
end;

  { TProject }

constructor TProject.Create(const aParent: TDanteBase);
begin
  inherited Create(aParent);

  FLog := nil;

  FDescription   := '';
  FBuildFilename := '';
  FBaseDir       := '';
  FDefaultTarget := '';
  FRunPath       := WildPaths.ToPath(GetCurrentDir);
end;

class function TProject.TagName: String;
begin
  Result := 'Project';
end;

function TProject.SetAttribute(const aName: String; const aValue: String): Boolean; { override }
begin
  Result  := True;
  if CompareText(aName, 'basedir') = 0 then
    BaseDir := aValue
  else if CompareText(aName, 'default') = 0 then
    FDefaultTarget := aValue
  else if CompareText(aName, 'description') = 0 then
    FDescription := aValue
  else
    Result := inherited SetAttribute(aName, aValue);
end;

procedure TProject.Initialize(const aLog: TLogManager; const aBuildFilename: String);
begin
  FLog           := aLog;
  FBuildFilename := Trim(aBuildFileName);

  if FBuildFilename = '' then
    begin
      FBuildFilename := FindBuildFile(Default_BuildFilename);

      if FBuildFilename = '' then
        FBuildFilename := FindBuildFile(FBuildFilename);

      if not FileExists(FBuildFilename) then
        Log(vlErrors, 'Cannot find ' + FBuildFilename);
    end;
end;

function TProject.GetLogLevel: TLogLevel;
begin
  if Assigned(FLog) then
    Result := FLog.Level
  else
    Result := vlNormal;
end;

procedure TProject.SetLogLevel(const aLogLevel: TLogLevel);
begin
  if Assigned(FLog) then
    FLog.Level := aLogLevel;
end;

procedure TProject.SetBaseDir(const aValue: String);
begin
  FBaseDir := WildPaths.ToRelativePath(aValue, FRunPath);
end;

function TProject.GetBaseDir: String;
begin
  Result := WildPaths.ToRelativePath(FBaseDir, FRunPath);
end;

function TProject.BasePath: String;
begin
  if PathIsAbsolute(BaseDir) then
    Result := BaseDir
  else
    Result := PathConcat(RunPath, BaseDir);
end;

function TProject.ToAbsolutePath(const aSubPath: String): String;
begin
  Result := PathConcat(Self.BasePath, aSubPath);
end;

function TProject.AddTarget(const aName: String): TTarget;
begin
  Result := TTarget.Create(Self);

  Result.SetAttribute('name', aName);

  SetTarget(Result);
end;

procedure TProject.AssignProperties(const aProperties: TDanteList);
var
  i: Integer;
begin
  for i := 0 to Pred(aProperties.Count) do
    SetProperty(TPropertyItem(FProperties.Objects[i]).Name, TPropertyItem(FProperties.Objects[i]).Value);
end;

function TProject.IsPropertyDefined(aName: String): Boolean;
begin
  Result := FProperties.IndexOf(LowerCase(aName)) <> -1;
end;

function TProject.GetProperty(const aName: String): String;
var
  p: Integer;
begin
  p := FProperties.IndexOf(LowerCase(aName));

  if p <> -1 then
    Result := TPropertyItem(FProperties.Objects[p]).Value
  else
    Result := '';
end;

procedure TProject.SetProperty(const aName: String; aValue: String);
var
  p: Integer;
  s: String;
begin
  s := LowerCase(aName);
  p := FProperties.IndexOf(s);

  if p <> -1 then
    TPropertyItem(FProperties.Objects[p]).Value := aValue
  else
    FProperties.AddObject(s, TPropertyItem.Create(aName, aValue));
end;

function TProject.GetTarget(aIndex: Integer): TTarget;
begin
  if (aIndex >= 0) and (aIndex < List.Count) then
    Result := TTarget(List.Objects[aIndex])
  else
    Result := nil;
end;

function TProject.GetTargetByName(aTargetName: String): TTarget;
begin
  Result := TTarget(inherited Get(aTargetName));
end;

procedure TProject.SetTarget(aTarget: TTarget);
begin
  Add(aTarget);
end;

function TProject.FindBuildFile(const aBuildFile: String): String;
begin
  Result := ExpandFileName('.\'+ aBuildFile);
  // find the Result in the current or super directories
  while not FileExists(Result)
        and FileExists(ExtractFileDir(Result))
  do begin
    Result := ExpandFileName(ExtractFilePath(Result) + '..\' + ExtractFilename(Result));
  end;
  if not FileExists(Result) then
    Result := aBuildFile;
end;

procedure TProject.LoadFile;
var
  Dom: IDocument;
begin
  if Length(FBuildFilename) > 0 then
    if FileExists(FBuildFilename) then
      begin
        Dom := MiniDom.ParseToDom(BuildFilename);

        try
          Clear;

          ParseXML(Dom.Root);

        finally
          Dom := nil;
        end;
      end
    else
      begin
          // in the future add -find support

        Log(Format(F_BuildFileNotFound, [BuildFilename]));
      end;
end;

procedure TProject.Schedule(const aTarget: TTarget; aTargetList: TTargetList);
var
  i:      Integer;
  Target: TTarget;
begin
  if not Assigned(aTargetList.Get(aTarget.Name)) then
    for i := 0 to Pred(aTarget.DependencyCount) do
      begin
        Target := GetTargetByName(aTarget.Dependency[i]);

        if Assigned(Target) then
          Schedule(Target, aTargetList);
      end;

  if Assigned(aTargetList.Get(aTarget.Name)) then
    raise ECircularTargetDependency.Create(aTarget.Name);

  aTargetList.Add(aTarget);
end;

procedure TProject.BuildTarget(const aTarget: TTarget);
var
  i:          Integer;
  TargetList: TTargetList;
begin
  if Assigned(aTarget) then
    begin
      TargetList := TTargetList.Create(Self);

      try
        try
          Schedule(aTarget, TargetList);

          for i := 0 to Pred(TargetList.Count) do
            begin
              try
                TargetList[i].Build;

              finally
                ChangeDir(RunPath);
              end;
            end;

        except
          on e:ETaskException do
            raise;
          on e:ETargetException do
            raise;
          on e:Exception do
            begin
              Log(vlErrors, Format(F_BuildTargetUnhandledError, [e.ClassName, e.Message]));

              raise;
            end;
        end;

      finally
        FreeAndNil(TargetList);
      end;
    end;
end;

procedure TProject.Build(const aTarget: String);
var
  i:            Integer;
  Target:       TTarget;
  BuildTargets: TStringList;
begin
  Log(Format(F_BuildStartMsg, [BuildFilename]));

  BuildTargets := TStringList.Create;

  try
    try
      LoadFile;

      BuildTargets.CommaText := aTarget;

      if BuildTargets.Count = 0 then
        begin
          Target := GetTargetByName(FDefaultTarget);

          if Assigned(Target) then
            BuildTarget(Target)
          else
            raise ENoDefaultTargetError.Create('No default target');
        end
      else
        for i := 0 to Pred(BuildTargets.Count) do
          begin
            Target := GetTargetByName(aTarget);

            if Assigned(Target) then
              BuildTarget(Target)
            else
              Log(vlErrors, Format(F_BuildTargetNotFound, [BuildTargets[i]]));
          end;

      Log(Format(F_BuildDoneMsg, [BuildFilename]));

    except
      on e:Exception do
        begin
          if e.ClassType.InheritsFrom(EDanteException) then
            Log(vlErrors, e.Message)
          else
            Log(vlErrors, e.ClassName + ': ' + e.Message);

          Log(vlErrors, Format(F_BuildFailedMsg, [BuildFilename]));
        end;
    end;

  finally
    FreeAndNil(BuildTargets);
  end;
end;

procedure TProject.Build(aTargets: array of String);
var
  i: Integer;
begin
  if Length(aTargets) = 0 then
    Build
  else
    for i := Low(aTargets) to High(aTargets) do
      Build(aTargets[i]);
end;

  { TTask }

destructor TTask.Destroy;
begin
  inherited;

  FTarget := nil;
end;

class function TTask.TagName: string;
begin
  Result := 'task';
end;

function TTask.SetAttribute(const aName, aValue: String): Boolean;
begin
  Result := True;

  if CompareText(aName, 'executable') = 0 then
    FIsExecutable := True
  else
    Result := inherited SetAttribute(aName, aValue);
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

  { TTestTask }

function TTestTask.SetAttribute(const aName, aValue: String): Boolean;
begin
  Result := True;

  if CompareText(aName, 'msg') = 0 then
    FMsg := aValue
  else
    Result := inherited SetAttribute(aName, aValue);
end;

class function TTestTask.TagName: String;
begin
  Result := 'test';
end;

procedure TTestTask.Execute;
begin
  Log(Msg);
end;



function  FindTask(const Tag: String): TDanteElementClass;
var
  Index :Integer;
begin
  Result := nil;

  if Assigned(__TaskRegistry) then
    begin
      Index := __TaskRegistry.IndexOf(Tag);

      if Index < 0 then
        TaskError(Format(F_DanteClassNotFound, [Tag]) )
      else
        Result := Pointer(__TaskRegistry.Objects[Index])
    end;
end;

procedure RegisterTask(DanteClass: TDanteElementClass);
var
  Index :Integer;
begin
  if not Assigned(__TaskRegistry) then
    begin
      __TaskRegistry := TStringList.Create;

      __TaskRegistry.Capacity   := 50;
      __TaskRegistry.Sorted     := True;
      __TaskRegistry.Duplicates := dupIgnore;
    end;

  Index :=__TaskRegistry.IndexOf(DanteClass.TagName);

  if Index >= 0 then
    TaskError(Format(F_DuplicateDanteClass, [DanteClass.TagName, DanteClass.ClassName]))
  else
    __TaskRegistry.AddObject(DanteClass.TagName, Pointer(DanteClass));
end;

procedure RegisterTasks(DanteClasses: array of TDanteElementClass);
var
  i :Integer;
begin
  for i := Low(DanteClasses) to High(DanteClasses) do
    RegisterTask(DanteClasses[i]);
end;

initialization

  RegisterTask(TTarget);
  RegisterTask(TTestTask);

finalization
  __TaskRegistry.Free;

end.

