{ $Id$ }
{
--------------------------------------------------------------------------
Copyright (c) 2001, Juancarlo Añez, Caracas, Venezuela.
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
--------------------------------------------------------------------------------
(based on BSD Open Source License)
}
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
  IMiniDomObject = interface(IObject)
  ['{C13B66E0-2263-11D5-8DC7-B0FF333FE70A}']
    function LineNo :Integer;
  end;

  IAttribute = interface(IMiniDomObject)
  ['{CD2605D8-4108-4372-B28E-8EED1C09A42C}']
    function name  :string;
    function value :string;
  end;

  INode = interface(IMiniDomObject)
  ['{C646E358-DE27-4BB5-8897-DC8F6B3E1413}']
    function toPrefixedString(prefix :string):string;
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
    function attributes :IList;
    function attribute(name :string) :IAttribute;
    function attributeValue(name :string) :string;
    function add(n :INode):INode;
    function setAttribute(Name, Value :string): IAttribute;
  end;

  IDocument = interface(INode)
  ['{1539F033-50A0-4F82-B8E9-E9D6B7A71291}']

    function newElement(name :string; LineNo :Integer = -1; attributes :IMap = nil)  :IElement;
    function newTextNode(text :string; LineNo :Integer = -1) :ITextNode;
    function newAttribute(name, value :string; LineNo :Integer = -1) :IAttribute;
    function root :IElement;
    function newRoot(name :string; LineNo :Integer; attributes :IMap = nil) :IElement;
  end;

  TMiniDomObject = class(TAbstractObject, IMiniDomObject)
  protected
    _lineNo :Integer;
  public
    constructor Create(LineNo :Integer);

    function LineNo :Integer; 
  end;

  TAttribute = class(TMiniDomObject, IAttribute)
  protected
    _name  :string;
    _value :string;
  public
    constructor create(name, value :string; LineNo :Integer);
    function name  :string;  virtual;
    function value :string;  virtual;

    function toString :string; override;
  end;

  TNode = class(TMiniDomObject, INode)
  protected
  public
    constructor Create(LineNo :Integer);
    function toPrefixedString(prefix :string):string; virtual;
  end;

  TTextNode = class(TNode, ITextNode)
  protected
    _text :string;
  public
    constructor create(value :string; LineNo :Integer);
    function text :string;

    function toString :string; override;
  end;

  TElement = class(TNode, IElement)
  protected
    _name       :string;
    _children   :IList;
    _attributes :IMap;
  public
    constructor create(name :string; LineNo :Integer = -1; attributes :IMap = nil);
    function name :string;                   virtual;
    function children :IList;                overload; virtual;
    function children(name :string) :IList;  overload; virtual;

    function attributes :IList;                    virtual;
    function attribute(name :string) :IAttribute;  virtual;
    function attributeValue(name :string) :string; virtual;

    function add(n :INode):INode;                  virtual;
    function setAttribute(Name, Value :string): IAttribute; virtual;

    function toString :string; override;
    function toPrefixedString(prefix :string):string; override;
  end;

  TDocument = class(TNode, IDocument)
    _root :IElement;
  public
    constructor Create;
    function newElement(name :string; LineNo :Integer; attributes :IMap)  :IElement;    virtual;
    function newTextNode(text :string; LineNo :Integer) :ITextNode;                     virtual;
    function newAttribute(name, value :string; LineNo :Integer) :IAttribute;            virtual;
    function root :IElement;                                                            virtual;
    function newRoot(name :string; LineNo :Integer; attributes :IMap) :IElement;        virtual;

    function toString :string; override;
  end;

  ISAXToDomHandler = interface(IDocumentHandler)
  ['{5545E11B-D69A-4436-A65E-9F663F74D31B}']
    function locator :TLocator;
  end;

  TSAXtoDOMHandler = class(SAX.THandlerBase, ISAXToDomHandler,  IErrorHandler)
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

    function LineNo :Integer;

    function locator :TLocator;
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

{ TMiniDomObject }

constructor TMiniDomObject.Create(LineNo: Integer);
begin
  inherited Create;
  _lineNo := LineNO;  
end;
function TMiniDomObject.LineNo: Integer;
begin
  Result := _LineNo;
end;

{ TNode }

constructor TNode.create(LineNo :Integer);
begin
  inherited create(LineNo);
end;

function TNode.toPrefixedString(prefix: string): string;
begin
  Result := prefix + toString;
end;

{ TTextNode }

constructor TTextNode.create(value: string; LineNo :Integer);
begin
  inherited create(LineNo);
  _text := value;
end;

function TTextNode.text: string;
begin
  result := _text;
end;

function TTextNode.toString: string;
begin
  Result := Text;
end;

{ TElement }

constructor TElement.create(name :string; LineNo :Integer; attributes :IMap);
begin
  inherited create(LineNo);
  self._name  := name;
  if attributes <> nil then
    _attributes := attributes
  else
    _attributes := TTreeMap.create;
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

function TElement.attributes: IList;
begin
  result := TArrayList.create(_attributes.values.size);
  result.addAll(_attributes.values);
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

function TElement.setAttribute(Name, Value: string): IAttribute;
begin
  Result := TAttribute.Create(Name, Value, -1);
  _attributes.put(iref(Name), Result);
end;

function TElement.toString: string;
begin
  Result := toPrefixedString('');
end;

function TElement.toPrefixedString(prefix: string): string;
var
  i :IIterator;
begin
  Result := prefix + '<' + name;
  i := attributes.iterator;
  while i.HasNext do
    Result := Result + ' ' + (i.Next as IAttribute).toString;

  if Children.Size <= 0 then
    Result := Result + ' />'
  else
  begin
    Result := Result + '>'#13#10;
    i := Children.Iterator;
    while i.HasNext do
      Result := Result + (i.Next as INode).toPrefixedString(prefix + '  ');
    Result := Result + prefix + '</' + name + '>';
  end;
  Result := Result + #13#10;
end;


{ TDocument }

function TDocument.newElement(name: string; LineNo :Integer; attributes :IMap): IElement;
begin
  result := TElement.create(name, LineNo, attributes);
end;

function TDocument.newRoot(name: string; LineNo :Integer; attributes :IMap): IElement;
begin
  assert(_root = nil);
  _root := newElement(name, LineNo, attributes);
  result := _root;
end;

function TDocument.root: IElement;
begin
  result := _root;
end;

function TDocument.newTextNode(text: string; LineNo :Integer): ITextNode;
begin
  result := TTextNode.create(text, LineNo);
end;

function TDocument.newAttribute(name, value: string; LineNo :Integer): IAttribute;
begin
  result := TAttribute.create(name, value, LineNo);
end;

function TDocument.toString: string;
begin
  Result := Root.toString;
end;

constructor TDocument.Create;
begin
  inherited Create(0);
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
    amap.put( iref(atts.getName(i)),
              _dom.newAttribute( atts.getName(i),
                                 atts.getValue(i),
                                 LineNo
                                 )
              );
  if _nodes.isEmpty then
    n := _dom.newRoot(name, LineNo, amap )
  else
    n := (_nodes.top as IElement).add(_dom.newElement(name, LineNo, amap));
  _nodes.push(n);
end;

procedure TSAXtoDOMHandler.endElement(name: WideString);
begin
  _nodes.pop;
end;

procedure TSAXtoDOMHandler.characters(ch: WideString; start, length: Integer);
begin
  (_nodes.top as IElement).add(_dom.newTextNode(copy(ch, start, length), LineNo));
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

function TSAXtoDOMHandler.LineNo: Integer;
begin
  Result := 1+_locator.getLineNumber;
end;

function TSAXtoDOMHandler.locator: TLocator;
begin
  Result := _locator;
end;

{ TAttribute }

constructor TAttribute.create(name, value: string; LineNo :Integer);
begin
  inherited create(LineNo);
  _name  := name;
  _value := value;
end;

function TAttribute.name: string;
begin
  result := _name;
end;

function TAttribute.toString: string;
begin
  Result := Format('%s="%s"', [Name, Value]);
end;

function TAttribute.value: string;
begin
  result := _value;
end;

function parseToDOM(src :IInputSource) :IDocument; overload;
var
  parser  :IParser;
  handler :ISAXToDOMHandler;
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
    on e :SAXParseException do
    begin
      result := nil;
      raise;
    end;
    on e :Exception do
    begin
      result := nil;
      raise SAXParseException.Create(e.Message, handler.locator);
    end;
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


