{@(#)$Id$}
unit MiniDOM;

interface
uses
 SysUtils,
 Classes,
 Collections,
 SAX;

const
  rcs_id :string = '@(#)$Id$';

type
  IAttribute = interface(IObject)
  ['{CD2605D8-4108-4372-B28E-8EED1C09A42C}']
    function name  :string;
    function value :string;
  end;

  INode = interface(IObject)
  ['{C646E358-DE27-4BB5-8897-DC8F6B3E1413}']
  end;

  ITextNode = interface(INode)
   ['{ECECD7D6-5623-4749-946A-1E2BCCB881A0}']
    function text :string;
  end;

  IElement = interface(INode)
  ['{77EE9C64-31B5-4672-AFE7-3A860E7D21AA}']
    function name :string;
    function children :IList;                overload;
    function children(name :string) :IList;  overload;
    function attributes :IIterator;
    function attribute(name :string) :IAttribute;
    function attributeValue(name :string) :string;
    function add(n :INode):INode;
  end;

  IDocument = interface(INode)
  ['{1539F033-50A0-4F82-B8E9-E9D6B7A71291}']

    function newElement(name :string; attributes :IMap)  :IElement;
    function newTextNode(text :string) :ITextNode;
    function newAttribute(name, value :string) :IAttribute;
    function root :IElement;
    function newRoot(name :string; attributes :IMap) :IElement;
  end;


  TAttribute = class(TAbstractObject, IAttribute)
  protected
    _name  :string;
    _value :string;
  public
    constructor create(name, value :string);
    function name  :string;  virtual;
    function value :string;  virtual;
  end;

  TNode = class(TAbstractObject, INode)
  protected
  public
    constructor create;
  end;

  TTextNode = class(TNode, ITextNode)
  protected
    _text :string;
  public
    constructor create(value :string);
    function text :string;
  end;

  TElement = class(TNode, IElement)
  protected
    _name       :string;
    _children   :IList;
    _attributes :IMap;
  public
    constructor create(name :string; attributes :IMap);
    function name :string;                   virtual;
    function children :IList;                overload; virtual;
    function children(name :string) :IList;  overload; virtual;

    function attributes :IIterator;                virtual;
    function attribute(name :string) :IAttribute;  virtual;
    function attributeValue(name :string) :string; virtual;

    function add(n :INode):INode;            virtual;
  end;

  TDocument = class(TNode, IDocument)
    _root :IElement;
  public
    function newElement(name :string; attributes :IMap)  :IElement;   virtual;
    function newTextNode(text :string) :ITextNode;                     virtual;
    function newAttribute(name, value :string) :IAttribute;            virtual;
    function root :IElement;                                           virtual;
    function newRoot(name :string; attributes :IMap) :IElement;       virtual;
  end;

  TSAXtoDOMHandler = class(SAX.THandlerBase, IErrorHandler)
    constructor create(dom :IDocument);

    procedure startDocument; override;
    procedure endDocument;   override;

    procedure startElement (name :WideString; atts: TAttributeList);
    override;
    procedure endElement (name :WideString);
    override;
    procedure characters(ch: WideString; start, length: Integer);
    override;
    procedure position(pos, len: Integer);
    override;
    procedure setDocumentLocator (locator :TLocator);
    override;

    procedure warning (exception :SAXParseException);
    override;
    procedure error (exception :SAXParseException);
    override;
    procedure fatalError (exception :SAXParseException);
    override;
  protected
    _dom        :IDocument;
    _nodes      :IStack;
    _elements   :array[char] of IMap;
    _locator    :TLocator;
  end;

function parseToDOM(src :IInputSource) :IDocument; overload;
function parseToDOM(strm :TStream)     :IDocument; overload;
function parseToDOM(fname :string)     :IDocument; overload;
function parseTextToDOM(text :string)  :IDocument;

implementation
uses
  XMLParser;

{ TNode }

constructor TNode.create;
begin
  inherited create;
end;

{ TTextNode }

constructor TTextNode.create(value: string);
begin
  inherited create;
  _text := value;
end;

function TTextNode.text: string;
begin
  result := _text;
end;

{ TElement }

constructor TElement.create(name :string; attributes :IMap);
begin
  inherited create;
  self._name  := name;
  _attributes := attributes;
  _children   := TLinkedList.Create;
end;

function TElement.children(name: string): IList;
var
  i :IIterator;
  n :INode;
begin
  result := TLinkedList.create;
  i := _children.iterator;
  while i.hasNext do
  begin
    n := i.next as INode;
    if n.instanceOf(IElement)
    and ((n as IElement).name = name)
    then
      result.add(n);
  end;
end;

function TElement.children: IList;
begin
  result := _children;
end;

function TElement.name: string;
begin
  result := _name;
end;


function TElement.add(n: INode): INode;
begin
  if _children.add(n) then
    result := n
  else
    result := nil;
end;

function TElement.attribute(name: string): IAttribute;
begin
  result := _attributes.get(name) as IAttribute
end;

function TElement.attributes: IIterator;
begin
  result := _attributes.values.iterator;
end;

function TElement.attributeValue(name: string): string;
var
  a :IAttribute;
begin
  a := attribute(name);
  if a <> nil then
    result := a.value
  else
    result := '';
end;

{ TDocument }

function TDocument.newElement(name: string; attributes :IMap): IElement;
begin
  result := TElement.create(name, attributes);
end;

function TDocument.newRoot(name: string; attributes :IMap): IElement;
begin
  assert(_root = nil);
  _root := newElement(name, attributes);
  result := _root;
end;

function TDocument.root: IElement;
begin
  result := _root;
end;

function TDocument.newTextNode(text: string): ITextNode;
begin
  result := TTextNode.create(text);
end;

function TDocument.newAttribute(name, value: string): IAttribute;
begin
  result := TAttribute.create(name, value);
end;

{ TSAXtoDOMHandler }

constructor TSAXtoDOMHandler.create(dom :IDocument);
begin
  inherited create;
  _dom   := dom;
  _nodes := TStack.create(TLinkedList.create);
end;

procedure TSAXtoDOMHandler.startElement(name: WideString; atts: TAttributeList);
var
  n    :INode;
  amap :IMap;
  i    :Integer;
begin
  amap := TTreeMap.create;
  for i := 0 to atts.getLength-1 do
    amap.put(iref(atts.getName(i)), _dom.newAttribute(atts.getName(i), atts.getValue(i) ));
  if _nodes.isEmpty then
    n := _dom.newRoot(name, amap)
  else
    n := (_nodes.top as IElement).add(_dom.newElement(name, amap));
  _nodes.push(n);
end;

procedure TSAXtoDOMHandler.endElement(name: WideString);
begin
  _nodes.pop;
end;

procedure TSAXtoDOMHandler.characters(ch: WideString; start, length: Integer);
begin
  (_nodes.top as IElement).add(_dom.newTextNode(copy(ch, start, length)));
end;

procedure TSAXtoDOMHandler.error(exception: SAXParseException);
begin
  inherited error(exception)
end;

procedure TSAXtoDOMHandler.fatalError(exception: SAXParseException);
begin
  inherited fatalError(exception)
end;

procedure TSAXtoDOMHandler.warning(exception: SAXParseException);
begin
  inherited warning(exception)
end;

procedure TSAXtoDOMHandler.position(pos, len: Integer);
begin

end;

procedure TSAXtoDOMHandler.setDocumentLocator(locator: TLocator);
begin
  _locator := locator
end;

procedure TSAXtoDOMHandler.endDocument;
begin
end;

procedure TSAXtoDOMHandler.startDocument;
begin
end;

{ TAttribute }

constructor TAttribute.create(name, value: string);
begin
  inherited create;
  _name  := name;
  _value := value;
end;

function TAttribute.name: string;
begin
  result := _name;
end;

function TAttribute.value: string;
begin
  result := _value;
end;

function parseToDOM(src :IInputSource) :IDocument; overload;
var
  parser  :IParser;
  handler :IDocumentHandler;
begin
  result := TDocument.create;
  try
    parser := TXMLParser.create;
    handler := TSAXtoDOMHandler.create(result);
    //!!!parser.setDocumentHandler(THandlerBase.Create);
    parser.setDocumentHandler(handler);
    parser.setErrorHandler(handler as IErrorHandler);
    try
     parser.parse(src);
    finally
     parser := nil;
    end
  except
    result := nil;
    raise;
  end;
end;

function parseToDOM(strm :TStream) :IDocument;
begin
  result := parseToDOM(TInputSource.create(strm));
end;

function parseTextToDOM(text :string) :IDocument;
var
  s :TMemoryStream;
begin
  s := TMemoryStream.create;
  try
    s.WriteBuffer(PChar(text)^, length(text));
    s.Position := 0;
    result := parseToDOM(s);
  finally
    s.free;
  end;
end;

function parseToDOM(fname :string) :IDocument;
var
  s :TMemoryStream;
begin
  s := TMemoryStream.create;
  try
    s.LoadFromFile(fname);
    result := parseToDOM(s);
  finally
    s.free;
  end;
end;


end.
