unit ScriptParser;

interface
uses
  SysUtils,
  Classes,

  JclSysUtils,

  Collections,
  MiniDom,

  WildPaths,
  WantClasses;

type
  EWantParseException = class(EWantException)
  public
    constructor Create(Msg :string; Line, Col :Integer); 
  end;


  TScriptParser = class
  protected
    class procedure ParseError(Msg :string; Line, Col :Integer);

    class function  XMLAttsToStrings(Node :IElement) :TStrings;
    class procedure ParseXML(Elem :TScriptElement; Node: MiniDom.IElement; Atts :TStrings);
    class procedure ParseXMLChild(Parent :TScriptElement; Child: MiniDom.IElement);
    class procedure ParseProject(Project :TProject; Dom : MiniDom.IDocument);
  public
    class procedure ParseText(Project :TProject; XML: string);
    class function Parse(Project :TProject; const SystemPath: TSystemPath = ''):TPath;
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
  child: MiniDom.INode;
  text : MiniDom.ITextNode;
begin
  Result := TStringList.Create;
  try
    i := Node.Attributes.Iterator;
    while i.HasNext do
    begin
      with i.Next as IAttribute do
          Result.Values[Name] := Value;
    end;
    i := Node.Children.Iterator;
    while i.HasNext do
    begin
      child := i.next as INode;
      if 0 = child.QueryInterface(ITextNode, text)  then
        Result.Values['text'] := Result.Values['text'] + text.text;
    end;
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
    try
      Elem   := Parent.SetupChild(Child.Name, Atts);
    except
      on e :EWantParseException do
        raise;
      on e :Exception do
        raise; // ParseError(e, Child.Location.LineNumber, Child.Location.ColumnNumber);
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
  child :MiniDom.IElement;
  L     :TLocation;
begin
  i := nil;
  try
    Elem.SetUp(Node.Name, Atts);
  except
    on e :EWantParseException do
    begin
      Node := nil;
      raise;
    end;
    on e :Exception do
    begin
      L := Node.Location;
      Node := nil;
      ParseError(e.Message, L.LineNumber, L.ColumnNumber);
    end;
  end;
  i := Node.Children.Iterator;
  while i.HasNext do
  begin
    if 0 = (i.Next as INode).QueryInterface(IElement, child)  then
      ParseXMLChild(Elem, child)
  end;
end;


class procedure TScriptParser.ParseProject(Project: TProject; Dom : MiniDom.IDocument);
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
  ParseProject(Project, MiniDom.ParseTextToDom(XML));
end;


class function TScriptParser.Parse(Project: TProject; const SystemPath: TSystemPath):TPath;
var
  BuildFile :TPath;
  Dom       :IDocument;
begin
  BuildFile := ToPath(ToSystemPath(SystemPath));
  if SystemPath = '' then
    BuildFile := Project.FindBuildFile(False)
  else
    BuildFile := Project.FindBuildFile(BuildFile, False);

  BuildFile := Project.ToAbsolutePath(BuildFile);
  if not PathIsFile(BuildFile) then
    WantError(Format('Cannot find build file "%s"',[BuildFile]));

  Project.RootPath := SuperPath(BuildFile);
  Dom := MiniDom.ParseToDom(ToSystemPath(BuildFile));
  try
    ParseProject(Project, Dom);
    Result := BuildFile;
  except
    on e :Exception do
    begin
      e.Message := ToRelativePath(BuildFile, CurrentDir) + ' ' +  e.Message;
      raise;
    end;
  end;
end;



end.
