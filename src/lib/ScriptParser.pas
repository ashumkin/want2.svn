{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca (R)                               }
{    ~                                         }
{     Copyright (C) 1995,2001 Juancarlo A�ez   }
{     All rights reserved.                     }
{            http://www.suigeneris.org/juanca  }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{#(@)$Id$}

unit ScriptParser;

interface
uses
  SysUtils,
  Classes,

  JclSysUtils,

  JALCollections,
  JALSAX,
  JALMiniDom,

  WildPaths,
  WantClasses;

type
  EWantParseException = class(EWantException)
  public
    constructor Create(Msg :string; Line, Col :Integer); 
  end;


  TScriptParser = class
  protected
    class procedure ParseError(Msg :string; Line :Integer =0; Col :Integer = 0);

    class function  XMLAttsToStrings(Node :IElement) :TStrings;
    class procedure ParseXML(Elem :TScriptElement; Node: JALMiniDOM.IElement; Atts :TStrings);
    class procedure ParseXMLChild(Parent :TScriptElement; Child: JALMiniDOM.IElement);
    class procedure ParseProject(Project :TProject; Dom : JALMiniDOM.IDocument);
  public
    class procedure ParseText(Project :TProject; XML: string);
    class procedure Parse(Project :TProject; const Path: TPath = '');
  end;

implementation

{ EWantParseException }

constructor EWantParseException.Create(Msg :string; Line, Col :Integer);
begin
  inherited Create(Format('(%d:%d): %s',[Line, Col, Msg]));
end;

class procedure TScriptParser.ParseError(Msg :string; Line, Col :Integer);
begin
  raise EWantParseException.Create(Msg, Line, Col) at CallerAddr;
end;

class function TScriptParser.XMLAttsToStrings(Node: IElement): TStrings;
var
  i :IIterator;
  child: JALMiniDOM.INode;
  text : JALMiniDOM.ITextNode;
  s    : string;
begin
  Result := TStringList.Create;
  try
    i := Node.Attributes.Iterator;
    while i.HasNext do
    begin
      with i.Next as IAttribute do
          Result.Values[Name] := Value;
    end;

    s := '';
    i := Node.Children.Iterator;
    while i.HasNext do
    begin
      child := i.next as INode;
      if 0 = child.QueryInterface(ITextNode, text)  then
        s := s + text.text;
    end;
    s := TrimRight(s);
    if s <> '' then
       Result.Values['text'] := s;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class procedure TScriptParser.ParseXMLChild(Parent:TScriptElement; Child: IElement);
var
  Elem  :TScriptElement;
  Atts  :TStrings;
begin
  Atts := XMLAttsToStrings(Child);
  try
    Elem := nil;
    try
      Elem        := Parent.SetupChild(Child.Name, Atts);
      Elem.Line   := Child.Location.LineNumber;
      Elem.Column := Child.Location.ColumnNumber;
    except
      on e :EWantParseException do
        raise;
      on e :Exception do
        ParseError(e.Message, Child.Location.LineNumber, Child.Location.ColumnNumber);
    end;
    if Elem <> nil then
      ParseXML(Elem, Child, Atts);
  finally
    FreeAndNil(Atts);
  end;
end;

class procedure TScriptParser.ParseXML(Elem :TScriptElement; Node: IElement; Atts : TStrings);
var
  i     :IIterator;
  child :JALMiniDOM.IElement;
begin
  i := nil;
  try
    Elem.SetUp(Node.Name, Atts);
  except
    on e :EWantParseException do
      raise;
    on e :Exception do
      ParseError(e.Message, Node.Location.LineNumber, Node.Location.ColumnNumber);
  end;
  i := Node.Children.Iterator;
  while i.HasNext do
  begin
    if 0 = (i.Next as INode).QueryInterface(IElement, child)  then
      ParseXMLChild(Elem, child)
  end;
end;


class procedure TScriptParser.ParseProject(Project: TProject; Dom : JALMiniDOM.IDocument);
var
  Atts    :TStrings;
begin
  Atts := XMLAttsToStrings(Dom.Root);
  try
    ParseXML(Project, Dom.Root, Atts);
  finally
    FreeAndNil(Atts);
  end;
end;


class procedure TScriptParser.ParseText(Project: TProject; XML: string);
begin
  ParseProject(Project, JALMiniDOM.ParseTextToDom(XML));
end;


class procedure TScriptParser.Parse(Project: TProject; const Path: TPath);
var
  BuildFile :TPath;
  Dom       :IDocument;
begin
  BuildFile := ToPath(Path);

  if not PathIsFile(BuildFile) then
    ParseError(Format('Cannot find build file "%s"',[BuildFile]));

  BuildFile := Project.ToAbsolutePath(BuildFile);
  try
    Project.RootPath := SuperPath(BuildFile);
    Dom := JALMiniDOM.ParseToDom(ToSystemPath(BuildFile));
    ParseProject(Project, Dom);
  except
    on e :SAXParseException do
      ParseError(ToRelativePath(BuildFile, CurrentDir) + ' ' +  e.Message);
    on e :Exception do
    begin
      e.Message := ToRelativePath(BuildFile, CurrentDir) + ' ' +  e.Message;
      raise;
    end;
  end;
end;



end.
