{ $Id$ }
{
--------------------------------------------------------------------------
Copyright (c) 2001, Juancarlo A�ez, Caracas, Venezuela.
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
unit XMLParser;
interface
uses
  SysUtils,
  SAX,
  XML;

type
  TCachedAttributeList = class(TAttributeList)
  protected
    next_  :TCachedAttributeList;
    class function new :TCachedAttributeList;
    procedure clear;
  end;

  TXMLParser = class(TInterfacedObject, IParser)
  protected
    _locator          :TLocator;
  public
    constructor Create; overload;
    destructor  Destroy; override;

    procedure parse (source :IInputSource); overload;
    procedure parse (systemId :WideString); overload;

    // Locator interface
    function getColumnNumber: Integer;
    function getLineNumber: Integer;
    function getPublicId: WideString;
    function getSystemId: WideString;

    // Parser interface
    procedure setLocale (locale :WideString);                  virtual;
    procedure setEntityResolver (resolver :IEntityResolver);   virtual;
    procedure setDTDHandler (handler :IDTDHandler);            virtual;
    procedure setDocumentHandler (handler :IDocumentHandler);  virtual;
    procedure setErrorHandler (handler :IErrorHandler);        virtual;

    property locator :TLocator read _locator;
  protected
    buffer_           :WideString;
    position_         :Integer;
    markIndex_        :Integer;
    marks_            :array of integer;
    line_             :Integer;
    column_           :Integer;

    publicId_         :WideString;
    systemId_         :WideString;

    locale_           :WideString;

    entityResolver_   :IEntityResolver;
    DTDHandler_       :IDTDHandler;
    documentHandler_  :IDocumentHandler;
    errorHandler_     :IErrorHandler;
    currentChar       :WideChar;

    procedure warning(msg :string);
    procedure error(msg :string);
    procedure fatalError(msg :string);

    procedure skip;                            overload;
    procedure skip(n:Integer);                 overload;
    function  peek(n :Integer):WideChar;       overload;
    function  peek(s :WideString):boolean;     overload;
    function  scan(s :WideString):boolean;
    procedure check(n :Integer; s :WideString);
    procedure mark;
    procedure unmark;
    function  markedText: WideString;
    function  markedTextLength :Integer;
    function  nextChar: WideChar;
    procedure skipSpace;
    procedure space;
    function  attValue :WideString;
    function  ignorableWhiteSpace :boolean;
    function  charData :boolean;
    procedure entityValue;
    function  name :WideString;
    function  scanName :boolean;
    function  nameChar(c: WideChar): boolean;
    procedure names;
    function  nameStartChar(c: WideChar): boolean;
    procedure nmtoken;
    procedure nmtokens;
    function  pubIdChar(c: WideChar): boolean;
    procedure pubIdLiteral;
    function  quote: WideChar;
    procedure systemLiteral;

    procedure document;
    procedure prolog;
    function  element :boolean;
    procedure misc;
    function  peReference :boolean;
    function  reference: WideString;
    function  comment: boolean;
    function  pi: boolean;
    function  cdsect: boolean;
    procedure xmldecl;
    procedure doctypedecl;
    procedure versionNum;
    procedure eq;
    procedure externalId;
    function  markupdecl :boolean;
    procedure encondingDecl;
    procedure sddecl;
    function  attribute(list :TCachedAttributeList): boolean;
    function  attributes: TCachedAttributeList;
    procedure content;
    procedure etag(tagName: WideString);
    procedure encName;
  end;

  TLocatorProxy = class(TLocator)
    constructor Create(parser :TXMlParser);
    function    getColumnNumber: Integer;                 override;
    function    getLineNumber: Integer;                   override;
    function    getPublicId: WideString;                  override;
    function    getSystemId: WideString;                  override;
  protected
    _parser :TXMLParser;
  end;

implementation

const
  EOFCH = #0;

{ TXMLParser }

constructor TXMLParser.Create;
var
   handler : THandlerBase;
begin
   inherited Create;
   _locator := TLocatorProxy.Create(self);

   handler := THandlerBase.Create;

   entityResolver_   := handler;
   DTDHandler_       := nil;
   documentHandler_  := nil;
   errorHandler_     := handler;
   markIndex_        := -1;
end;

destructor TXMLParser.Destroy;
begin
  _locator.Free;
  inherited Destroy;
end;

procedure TXMLParser.warning(msg: string);
begin
   errorHandler_.warning(SAXParseException.Create(msg, _locator))
end;

procedure TXMLParser.error(msg :string);
begin
   errorHandler_.error(SAXParseException.Create(msg, _locator))
end;

procedure TXMLParser.fatalError(msg: string);
begin
   errorHandler_.fatalError(SAXParseException.Create(msg, _locator))
end;

    // 2.2 Characters
    // Character Range
    //  [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF]        // any Unicode character, excluding the
    //               | [#xE000-#xFFFD] | [#x10000-#x10FFFF] // surrogate blocks, FFFE, and FFFF.

function TXMLParser.nextChar : WideChar;
begin
  skip;
  result := currentChar;
end;

function TXMLParser.peek(n: Integer): WideChar;
begin
  if (position_ + n) <= length(buffer_) then
     result := buffer_[position_ + n]
  else
     result := EOFCH
end;

function TXMLParser.peek(s: WideString): boolean;
var
  i :Integer;
begin
  result := true;
  for i := 1 to length(s) do begin
      if s[i] <> peek(i-1) then begin
         result := false;
         break
      end
  end
end;

function TXMLParser.scan(s: WideString): boolean;
begin
   result := peek(s);
   if result then
      skip(length(s))
end;

procedure TXMLParser.check(n :Integer; s: WideString);
begin
     if not scan(s) then
        error(format('[%d] Expected "%s"', [n, s]))
end;

procedure TXMLParser.mark;
begin
  inc(markIndex_);
  if markIndex_ >= length(marks_) then
    setLength(marks_, length(marks_)+1);
  marks_[markIndex_] := position_
end;

procedure TXMLParser.unmark;
begin
  assert(markIndex_ >= 0);
  dec(markIndex_);
end;

function TXMLParser.markedText :WideString;
begin
   result := copy(buffer_, marks_[markIndex_], markedTextLength);
   unmark;
end;

function TXMLParser.markedTextLength: Integer;
begin
   if markIndex_ < 0 then
     result := 0
   else
     result := position_ - marks_[markIndex_]
end;

procedure TXMLParser.skip(n :Integer);
var
  i :Integer;
begin
  for i := 1 to n do begin
      if (position_ > 0) and (position_ <= length(buffer_)) then begin
        if buffer_[position_] = #$A then begin
           inc(line_);
           column_ := 1
        end
        else
           inc(column_)
      end;
      inc(position_);
  end;
  if position_ > length(buffer_) then
     currentChar := EOFCH
  else
     currentChar := buffer_[position_];
  case Integer(currentChar) of
     $0,$9, $A, $D, $20..$D7FF,$E000..$FFFD://,$10000-$10FFFF:
      ;  // fine
     //!!! should exclude FFFE to FFFF
  else
     error(format('[2] Invalid XML character #x%x', [Ord(currentChar)]))
  end;
  if (self.documentHandler_ <> nil)
  and (((position_ mod 16) = 0) or (currentChar = EOFCH)) then
      self.documentHandler_.position(position_, length(buffer_));
end;

procedure TXMLParser.skip;
begin
   skip(1)
end;

    // 2.3 Common Syntactic Constructs
    // White Space
    //  [3] S ::= (#x20 | #x9 | #xD | #xA)+

procedure TXMLParser.space;
begin
   case currentChar of
     #$20, #$9, #$D, #$A:
        skip
   else
      error('[3] Space expected');
   end;
   skipSpace;
end;

procedure TXMLParser.skipSpace;
begin
   while (currentChar <= High(Char))
   and (Char(currentChar) in [#$20, #$9, #$D, #$A]) do
       skip;
end;

    // Names and Tokens
    //  [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender

function TXMLParser.nameChar(c :WideChar) :boolean;
begin
     case c of
        'a'..'z', 'A'..'Z',      // letter
        '0'..'9',                // digit
        '.', '-', '_', ':':      //
                                 // combiningChar
                                 // extender
            result := true
     else
        result := false
     end
end;

    //  [5] Name ::= (Letter | '_' | ':') (NameChar)*

function TXMLParser.nameStartChar(c :WideChar) : boolean;
begin
     case c of
        'a'..'z', 'A'..'Z',
        '_', ':':
            result := true
     else
        result := false
     end;
end;

function TXMLParser.name :WideString;
begin
     mark;
     if not scanName then
        error('[5] Expected Letter, "_", or ":" ');
     result := markedText
end;

function TXMLParser.scanName: boolean;
begin
     if not nameStartChar(currentChar) then
        result := false
     else begin
         repeat
            skip
         until not nameChar(currentChar);
         result := true
     end
end;

   //  [6] Names ::= Name (S Name)*
procedure TXMLParser.names;
begin
   repeat
      name;
      skipSpace;
   until not nameStartChar(currentChar);
end;

    //  [7] Nmtoken ::= (NameChar)+

procedure TXMLParser.nmtoken;
begin
  if not nameChar(currentChar) then
     error('[7] NameChar expected');
  repeat
     skip
  until not nameChar(currentChar);
end;

    //  [8] Nmtokens ::= Nmtoken (S Nmtoken)*
procedure TXMLParser.nmtokens;
begin
   repeat
      nmtoken;
      skipSpace;
   until not nameChar(currentChar);
end;

    // Literals
    //  [9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
    //                      | "'" ([^%&'] | PEReference | Reference)* "'"

function TXMLParser.quote : WideChar;
begin
   result := currentChar;
   case result of
      '"', '''':
          skip
   else
      error('[9] '', or " expected')
   end
end;

procedure TXMLParser.entityValue;
var
   q :WideChar;
begin
   q := quote;
   while currentChar <> q do begin
       case currentChar of
         '%': peReference;
         '&': reference
         else
            skip 
         end
   end
end;

    // [10] AttValue ::= '"' ([^<&"] | Reference)* '"'
    //                   | "'" ([^<&'] | Reference)* "'"
function TXMLParser.attValue :WideString;
var
   q     :WideChar;
   value :string;
begin
   value := '';
   mark;
   q := quote;
   while currentChar <> q do begin
       case currentChar of
         '&': begin
                value := value + markedText + reference;
                mark;
              end; 
         else
            skip
         end
   end;
   check(10, q);
   result := XMLUnquote(value + markedText)
end;

    // [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")

procedure TXMLParser.systemLiteral;
var
   q :WideChar;
begin
   q := quote;
   while currentChar <> q do begin
         case currentChar of
           '<', '&':
              error('[109] Invalide character for attribute value')
         else
            skip
         end
   end;
   skip
end;

    // [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"

procedure TXMLParser.pubIdLiteral;
var
   q :WideChar;
begin
   q := quote;
   while (currentChar <> q)
   and   pubIdChar(currentChar) do
       skip;
   if currentChar = q then
      skip
   else
      error('[12] Expected ' + q)
end;


    // [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]

function TXMLParser.pubIdChar(c :WideChar) : boolean;
begin
  case c of
    #$20, #$D, #$A,
    'a'..'z', 'A'..'Z',
    '-','(',')','+',',',':','=',
    '?',';','!','*','#','@','_','%':
         result := true
    else
         result := false
    end
end;

function TXMLParser.ignorableWhiteSpace :boolean;
var
  text :string;
begin
   mark;
   skipSpace;
   result := markedTextLength > 0;
   text   := markedText;
   if result and (documentHandler_ <> nil) then
      documentHandler_.ignorableWhiteSpace(text, 1, length(text));
end;

    // 2.4 Character Data and Markup
    // [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)

function TXMLParser.charData :boolean;
var
  text :string;
begin
  result := ignorableWhiteSpace;
  mark;
  while true do begin
    case currentChar of
       '<', #0:
           break;
       ']':
          if peek(']>') then
             break;
       else begin
          skip
       end
    end
  end;
  text := markedText;
  if length(text) > 0 then begin
     if documentHandler_ <> nil then
          documentHandler_.characters(XMLUnquote(text), 1, length(text));
     result := true;
  end
end;

    // 2.5 Comments
    // [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'

function TXMLParser.comment :boolean;
begin
  if not scan('<!--') then
     result := false
  else begin
     repeat
       skip
     until scan('-->');
     result := true
  end
end;

    // 2.6 Processing Instructions
    // [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    // [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))

function TXMLParser.pi : boolean;
var
  target,
  data    :WideString;
begin
  if not scan('<?') then
      result := false
  else begin
     mark;
     name;
     target := markedText;
     if UpperCase(markedText) = 'XML' then
        error('[17] Illegal PI target');
     space;
     mark; // data starta here
     repeat
       skip
     until peek('?>');
     data := markedText;
     scan('?>');
     if documentHandler_ <> nil then
        documentHandler_.processingInstruction(target, data);
     result := true
  end
end;

    // 2.7 CDATA Sections
    // [18] CDSect ::= CDStart CData CDEnd
    // [19] CDStart ::= '<![CDATA['
    // [20] CData ::= (Char* - (Char* ']]>' Char*))
    // [21] CDEnd ::= ']]>'

function TXMLParser.cdsect :boolean;
begin
  if  not scan('<![CDATA[') then
     result := false
  else begin
     mark;
     while not peek(']]>') do
       skip;
     if documentHandler_ <> nil then
        documentHandler_.characters(markedText, 1, length(markedText));
     check(21,']]>');
     result := true
  end
end;

    // 2.8 Prolog and Document Type Declaration
    // Prolog
    // [22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?

procedure TXMLParser.prolog;
begin
   xmldecl;
   misc;
   doctypedecl;
   misc
end;

    // [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    // [24] VersionInfo ::= S 'version' Eq (' VersionNum ' | " VersionNum ")

procedure TXMLParser.xmldecl;
var
   q :WideChar;
begin
   // this is optional
   if (position_ = 1)
   and scan('<?xml ') then begin
        skipSpace;
        check(24, 'version');
        eq;
        q := quote;
           versionNum;
        check(24, q);
        skipSpace;
        encondingDecl;
        sdDecl;
        check(24, '?>')
   end
end;

    // [25] Eq ::= S? '=' S?
procedure TXMLParser.eq;
begin
  skipSpace;
  check(25, '=');
  skipSpace;
end;

    // [26] VersionNum ::= ([a-zA-Z0-9_.:] | '-')+
procedure TXMLParser.versionNum;
begin
   while true do begin
     case currentChar of
        'a'..'z', 'A'..'Z',
        '0'..'9',
        '_','.',':','-':
           skip
     else
        break
     end
   end
end;

    // [27] Misc ::= Comment | PI |  S
procedure TXMLParser.misc;
var
  goOn :boolean;
begin
  skipSpace;
  goOn := true;
  while goOn do begin
       goOn := false;
       if currentChar = '<' then begin
          case peek(1) of
            '!' : goOn := comment;
            '?' : goOn := pi
          end
       end;
       skipSpace
  end
end;

    // Document Type Definition
    // [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S?
    //                      ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
procedure TXMLParser.doctypedecl;
var
  done : boolean;
begin
  if scan('<!DOCTYPE ') then begin
     skipSpace;
     name;
     skipSpace;
     externalId;
     skipSpace;
     if scan('[') then begin
        repeat
            case currentChar of
              '<': done := markupDecl;
              '%': done := peReference
              else done := true;
            end;
            skipSpace;
        until done;
        check(28, ']');
        skipSpace;
     end;
     check(28, '>')
  end
end;

    // [29] markupdecl ::= elementdecl | AttlistDecl | EntityDecl
    //                     | NotationDecl | PI | Comment
function TXMLParser.markupdecl:boolean;
begin
  result := false
  //!!! pending
end;

    // External Subset
    // [30] extSubset ::= TextDecl? extSubsetDecl
    // [31] extSubsetDecl ::= ( markupdecl | conditionalSect | PEReference | S )*

    // 2.9 Standalone Document Declaration
    // [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'")
    //                 | ('"' ('yes' | 'no') '"'))
procedure TXMLParser.sddecl;
var
  q :WideChar;
begin
  skipSpace;
  if scan('standalone') then begin
     eq;
     q := quote;
     if not scan('yes') then
        check(32, 'no');
     check(32, q)
  end
end;

    // 2.10 White Space Handling
    // 2.11 End-of-Line Handling

    // 2.12 Language Identification
    // [33] LanguageID ::= Langcode ('-' Subcode)*
    // [34] Langcode ::= ISO639Code |  IanaCode |  UserCode
    // [35] ISO639Code ::= ([a-z] | [A-Z]) ([a-z] | [A-Z])
    // [36] IanaCode ::= ('i' | 'I') '-' ([a-z] | [A-Z])+
    // [37] UserCode ::= ('x' | 'X') '-' ([a-z] | [A-Z])+
    // [38] Subcode ::= ([a-z] | [A-Z])+

    // 3. Logical Structures
    // Element
    // [39] element ::= EmptyElemTag
    //                  | STag content ETag

    // 3.1 Start-Tags, End-Tags, and Empty-Element Tags
    // Start-tag
    // [40] STag ::= '<' Name (S Attribute)* S? '>'

function TXMLParser.element :boolean;
var
  tagName :WideString;
  atts    :TCachedAttributeList;
begin
  result := false;
  if currentChar <> '<' then
     result := false
  else if peek(1) = '/' then
     result := false
  else begin
    skip;
    mark;
    if not scanName then begin
       unmark;
       Exit;
    end;
    tagName := markedText;
    atts    := attributes;
    if documentHandler_ <> nil then begin
       try
         documentHandler_.startElement(tagName, atts);
       finally
         atts.clear
       end
    end;
    skipSpace;
    if not scan('/>') then begin
       check(39, '>');
       content;
       etag(tagName);
    end;
    if documentHandler_ <> nil then
       documentHandler_.endElement(tagName);
    result := true
  end
end;

    // [41] Attribute ::= Name Eq AttValue
function TXMLParser.attribute(list :TCachedAttributeList) :boolean;
var
  name,
  value :WideString;
begin
  skipSpace;
  mark;
  if not scanName then begin
     unmark;
     result := false
  end
  else begin
       name := markedText;
       eq;
       value := attValue;
       if documentHandler_ <> nil then
          list.add(name, value, '');
       result := true
  end;
end;

function TXMLParser.attributes :TCachedAttributeList;
begin
  if documentHandler_ <> nil then
     result := TCachedAttributeList.new
  else
     result := nil;
  while attribute(result) do
        // nop
end;

    // End-tag
    // [42] ETag ::= '</' Name S? '>'

procedure TXMLParser.etag(tagName :WideString);
begin
  check(42, '</'+ tagName);
  skipSpace;
  check(42, '>')
end;

    // Content of Elements
    // [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*

procedure TXMLParser.content;
var
  goOn : boolean;
begin
  goOn := true;
  while goOn do begin
     case currentChar of
       '&': reference;
       '<': case peek(1) of
              '!': goOn := comment;
              '[': goOn := cdsect;
              '?': goOn := pi;
              else
                   goOn := element
            end
       else
          goOn := charData
       end
  end;
end;

    // Tags for Empty Elements
    // [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'

    // 3.2 Element Type Declarations
    // Element Type Declaration
    // [45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
    // [46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
    // 3.2.1 Element Content
    // Element-content Models
    // [47] children ::= (choice | seq) ('?' | '*' | '+')?
    // [48] cp ::= (Name | choice | seq) ('?' | '*' | '+')?
    // [49] choice ::= '(' S? cp ( S? '|' S? cp )* S? ')'
    // [50] seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'
    // 3.2.2 Mixed Content
    // Mixed-content Declaration
    // [51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
    //                | '(' S? '#PCDATA' S? ')'

    // 3.3 Attribute-List Declarations
    // Attribute-list Declaration
    // [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
    // [53] AttDef ::= S Name S AttType S DefaultDecl

    // 3.3.1 Attribute Types
    // Attribute Types
    // [54] AttType ::= StringType | TokenizedType | EnumeratedType
    // [55] StringType ::= 'CDATA'
    // [56] TokenizedType ::= 'ID'
    //                        | 'IDREF'
    //                        | 'IDREFS'
    //                        | 'ENTITY'
    //                        | 'ENTITIES'
    //                        | 'NMTOKEN'
    //                        | 'NMTOKENS'
    // Enumerated Attribute Types
    // [57] EnumeratedType ::= NotationType | Enumeration
    // [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
    // [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'

    // 3.3.2 Attribute Defaults
    // [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED'
    //                      | (('#FIXED' S)? AttValue)
    // 3.3.3 Attribute-Value Normalization

    // 3.4 Conditional Sections
    // [61] conditionalSect ::= includeSect | ignoreSect
    // [62] includeSect ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
    // [63] ignoreSect ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
    // [64] ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
    // [65] Ignore ::= Char* - (Char* ('<![' | ']]>') Char*)

    // 4. Physical Structures
    // 4.1 Character and Entity References
    // Character Reference
    // [66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
    // Entity Reference
    // [67] Reference ::= EntityRef | CharRef
    // [68] EntityRef ::= '&' Name ';'
function TXMLParser.reference :WideString;
  function parseHex(s :string):Integer;
  var
    i :Integer;
  begin
     s := UpperCase(s);
     result := 0;
     for i := 1 to length(s) do
     begin
       result := result * $10;
       case s[i] of
         'A'..'F' : result := result + $A + ord(s[i]) - ord('A');
         '0'..'9' : result := result + ord(s[i]) - ord('0');
         else
            raise SAXParseException.Create('expected hex numer: ' + s);
       end
     end
  end;

var
  digits :Integer;
  hex    :Boolean;
begin
    result := '';
    check(67, '&');
    if not scan('#') then begin
       // entityRef
       mark;
       name;
       result := markedText;
       check(68, ';');
    end
    else begin
        digits := 0;
        hex := scan('x');
        mark;
        while true do begin
          case currentChar of
             '0'..'9' :
                inc(digits);
             'a'..'f','A'..'F':
                 if hex then
                   inc(digits)
                 else
                   break;
             else
                break
          end;
          skip
        end;
        if digits < 1 then
           error('expected number');
        result := markedText;
        check(66, ';');
        if hex then
          result := chr(parseHex(result))
        else
          result := chr(StrToIntDef(result, 0));
    end;
end;

    // [69] PEReference ::= '%' Name ';'

function TXMLParser.peReference :boolean;
begin
   mark;
   if  not scan('%') then
     result := false
   else begin
      name;
      check(69, ';');
      result := true
   end;
end;

    // 4.2 Entity Declarations
    // [70] EntityDecl ::= GEDecl | PEDecl
    // [71] GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
    // [72] PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
    // [73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)
    // [74] PEDef ::= EntityValue | ExternalID

    // 4.2.1 Internal Entities
    // 4.2.2 External Entities
    // External Entity Declaration
    // [75] ExternalID ::= 'SYSTEM' S SystemLiteral
    //                     | 'PUBLIC' S PubidLiteral S SystemLiteral

procedure TXMLParser.externalId;
begin
    if scan('SYSTEM') then begin
       space;
       SystemLiteral;
    end
    else begin
       check(75, 'PUBLIC');
       space;
       pubIdLiteral;
       space;
       systemLiteral;
    end;
end;

    // [76] NDataDecl ::= S 'NDATA' S Name

    // 4.3 Parsed Entities
    // 4.3.1 The Text Declaration
    // [77] TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
    // 4.3.2 Well-Formed Parsed Entities
    // [78] extParsedEnt ::= TextDecl? content
    // [79] extPE ::= TextDecl? extSubsetDecl
    // 4.3.3 Character Encoding in Entities
    // Encoding Declaration
    // [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' |  "'" EncName "'" )

procedure TXMLParser.encondingDecl;
var
  q :WideChar;
begin
  skipSpace;
  if scan('encoding') then begin
     eq;
     q := quote;
     encName;
     check(80, q)
  end
end;

    // [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
procedure TXMLParser.encName;
begin
    while true do begin
       case currentChar of
          'a'..'z', 'A'..'Z',      // letter
          '0'..'9',                // digit
          '.', '-', '_':           //
              skip
       else
          break
       end
    end
end;

    // 4.4 XML Processor Treatment of Entities and References
    // 4.4.1 Not Recognized
    // 4.4.2 Included
    // 4.4.3 Included If Validating
    // 4.4.4 Forbidden
    // 4.4.5 Included in Literal
    // 4.4.6 Notify
    // 4.4.7 Bypassed
    // 4.4.8 Included as PE

    // 4.5 Construction of Internal Entity Replacement Text
    // 4.6 Predefined Entities

    // 4.7 Notation Declarations
    // [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID |  PublicID) S? '>'
    // [83] PublicID ::= 'PUBLIC' S PubidLiteral

    // 4.8 Document Entity

procedure TXMLParser.document;
begin
  skipSpace;
  prolog;
  if not element then
     error('no root object');
  misc;
end;

    // 5. Conformance
    // 5.1 Validating and Non-Validating Processors
    // 5.2 Using XML Processors

    // 6. Notation
    // Appendices
    // A. References
    // B. Character Classes
    // [84] Letter ::= BaseChar | Ideographic
    // [85] BaseChar ::= <see standard>
    // [86] Ideographic ::= <see standard>
    // [87] CombiningChar ::= <see standard>
    // [88] Digit ::= <see standard>
    // [89] Extender ::= <see standard>
    // C.-G. ... (Non-Normative)
    //

function TXMLParser.getColumnNumber: Integer;
begin
     result := self.column_
end;

function TXMLParser.getLineNumber: Integer;
begin
     result := self.line_
end;

function TXMLParser.getPublicId: WideString;
begin
     result := self.publicId_
end;

function TXMLParser.getSystemId: WideString;
begin
   result := self.systemId_
end;

procedure TXMLParser.parse(source: IInputSource);
var
   s :string;
begin
   publicId_ := source.getPublicId;
   systemId_ := source.getSystemId;
   SetLength(s, source.getCharacterStream.Size);
   if length(s) > 0 then
     source.getCharacterStream.ReadBuffer(s[1], length(s));
   buffer_ := s;
   if documentHandler_ <> nil then
      documentHandler_.setDocumentLocator(_locator);
   if documentHandler_ <> nil then
      documentHandler_.startDocument;
   position_ := 0;
   skip;
   document;
   skipSpace;
   // check for trailing stuff
   if currentChar <> #0 then
      error('trailing stuff');
   if documentHandler_ <> nil then
      documentHandler_.endDocument;
end;

procedure TXMLParser.parse(systemId: WideString);
begin
  parse(entityResolver_.resolveEntity('',systemId))
end;

procedure TXMLParser.setDocumentHandler(handler: IDocumentHandler);
begin
     documentHandler_ := handler
end;

procedure TXMLParser.setDTDHandler(handler: IDTDHandler);
begin
     DTDHandler_ := handler
end;

procedure TXMLParser.setEntityResolver(resolver: IEntityResolver);
begin
     entityResolver_ := resolver
end;

procedure TXMLParser.setErrorHandler(handler: IErrorHandler);
begin
    errorHandler_ := handler
end;

procedure TXMLParser.setLocale(locale: WideString);
begin
    locale_ := locale
end;

{ TCachedAttributeList }

var
  __attributeLists :TCachedAttributeList;

class function TCachedAttributeList.new :TCachedAttributeList;
begin
   if __attributeLists = nil then begin
      result := TCachedAttributeList.create;
   end
   else begin
      result := __attributeLists;
      __attributeLists := result.next_;
   end
end;


procedure TCachedAttributeList.clear;
begin
  list_.clear;
  map_.clear;
  if types_ <> nil then
     types_.clear;

  next_ := __attributeLists;
  __attributeLists := self;
end;

{ TLocatorProxy }

constructor TLocatorProxy.Create(parser: TXMlParser);
begin
  inherited Create;
  self._parser := parser;
end;

function TLocatorProxy.getColumnNumber: Integer;
begin
  result := _parser.getColumnNumber
end;

function TLocatorProxy.getLineNumber: Integer;
begin
  result := _parser.getLineNumber;
end;

function TLocatorProxy.getPublicId: WideString;
begin
  result := _parser.getPublicId;
end;

function TLocatorProxy.getSystemId: WideString;
begin
  result := _parser.getSystemId;
end;

end.

