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
unit SAX;
interface
uses
  SysUtils,
  Collections,
  Classes;

type
  SAXParseException = class;
  TLocator          = class;
  TAttributeList    = class;
  IInputSource      = interface;
  
(**
  * Interface for an element's attribute specifications.
  *
  * <p>The SAX parser implements this interface and passes an instance
  * to the SAX application as the second argument of each startElement
  * event.</p>
  *
  * <p>The instance provided will return valid results only during the
  * scope of the startElement invocation (to save it for future
  * use, the application must make a copy: the AttributeListImpl
  * helper class provides a convenient constructor for doing so).</p>
  *
  * <p>An AttributeList includes only attributes that have been
  * specified or defaulted: #IMPLIED attributes will not be included.</p>
  *
  * <p>There are two ways for the SAX application to obtain information
  * from the AttributeList.  First, it can iterate through the entire
  * list:</p>
  *
  * <pre>
  * public void startElement (String name, AttributeList atts) {
  *   for (int i = 0; i < atts.getLength(); i++) {
  *     String name = atts.getName(i);
  *     String type = atts.getType(i);
  *     String value = atts.getValue(i);
  *     [...]
  *   }
  * }
  * </pre>
  *
  * <p>(Note that the result of getLength() will be zero if there
  * are no attributes.)
  *
  * <p>As an alternative, the application can request the value or
  * type of specific attributes:</p>
  *
  * <pre>
  * public void startElement (String name, AttributeList atts) {
  *   String identifier = atts.getValue("id");
  *   String label = atts.getValue("label");
  *   [...]
  * }
  * </pre>
  *
  * <p>The AttributeListImpl helper class provides a convenience
  * implementation for use by parser or application writers.</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.DocumentHandler#startElement
  * @see org.xml.sax.helpers.AttributeListImpl
  *)


// SAX document handler.

(**
  * Receive notification of general document events.
  *
  * <p>This is the main interface that most SAX applications
  * implement: if the application needs to be informed of basic parsing
  * events, it implements this interface and registers an instance with
  * the SAX parser using the setDocumentHandler method.  The parser 
  * uses the instance to report basic document-related events like
  * the start and end of elements and character data.</p>
  *
  * <p>The order of events in this interface is very important, and
  * mirrors the order of information in the document itself.  For
  * example, all of an element's content (character data, processing
  * instructions, and/or subelements) will appear, in order, between
  * the startElement event and the corresponding endElement event.</p>
  *
  * <p>Application writers who do not want to implement the entire
  * interface can derive a class from HandlerBase, which implements
  * the default functionality; parser writers can instantiate
  * HandlerBase to obtain a default handler.  The application can find
  * the location of any document event using the Locator interface
  * supplied by the Parser through the setDocumentLocator method.</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.Parser#setDocumentHandler
  * @see org.xml.sax.Locator
  * @see org.xml.sax.HandlerBase
  *)
  IDocumentHandler = interface
  ['{01A64BFE-5C0C-43BE-9F02-D21D46AC489C}']
  (**
    * Receive an object for locating the origin of SAX document events.
    *
    * <p>SAX parsers are strongly encouraged (though not absolutely
    * required) to supply a locator: if it does so, it must supply
    * the locator to the application by invoking this method before
    * invoking any of the other methods in the DocumentHandler
    * interface.</p>
    *
    * <p>The locator allows the application to determine the end
    * position of any document-related event, even if the parser is
    * not reporting an error.  Typically, the application will
    * use this information for reporting its own errors (such as
    * character content that does not match an application's
    * business rules).  The information returned by the locator
    * is probably not sufficient for use with a search engine.</p>
    *
    * <p>Note that the locator will return correct information only
    * during the invocation of the events in this interface.  The
    * application should not attempt to use it at any other time.</p>
    *
    * @param locator An object that can return the location of
    *                any SAX document event.
    * @see org.xml.sax.Locator
    *)
  procedure setDocumentLocator (locator :TLocator);


  (**
    * Receive notification of the beginning of a document.
    *
    * <p>The SAX parser will invoke this method only once, before any
    * other methods in this interface or in DTDHandler (except for
    * setDocumentLocator).</p>
    *
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    *)
  procedure startDocument;
    // throws SAXException;


  (**
    * Receive notification of the end of a document.
    *
    * <p>The SAX parser will invoke this method only once, and it will
    * be the last method invoked during the parse.  The parser shall
    * not invoke this method until it has either abandoned parsing
    * (because of an unrecoverable error) or reached the end of
    * input.</p>
    *
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    *)
  procedure endDocument;
    //throws SAXException;


  (**
    * Receive notification of the beginning of an element.
    *
    * <p>The Parser will invoke this method at the beginning of every
    * element in the XML document; there will be a corresponding
    * endElement() event for every startElement() event (even when the
    * element is empty). All of the element's content will be
    * reported, in order, before the corresponding endElement()
    * event.</p>
    *
    * <p>If the element name has a namespace prefix, the prefix will
    * still be attached.  Note that the attribute list provided will
    * contain only attributes with explicit values (specified or
    * defaulted): #IMPLIED attributes will be omitted.</p>
    *
    * @param name The element type name.
    * @param atts The attributes attached to the element, if any.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see #endElement
    * @see org.xml.sax.AttributeList
    *)
  procedure startElement (name :WideString; atts: TAttributeList);
    // throws SAXException;


  (**
    * Receive notification of the end of an element.
    *
    * <p>The SAX parser will invoke this method at the end of every
    * element in the XML document; there will be a corresponding
    * startElement() event for every endElement() event (even when the
    * element is empty).</p>
    *
    * <p>If the element name has a namespace prefix, the prefix will
    * still be attached to the name.</p>
    *
    * @param name The element type name
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    *)
  procedure endElement (name :WideString);
    //throws SAXException;


  (**
    * Receive notification of character data.
    *
    * <p>The Parser will call this method to report each chunk of
    * character data.  SAX parsers may return all contiguous character
    * data in a single chunk, or they may split it into several
    * chunks; however, all of the characters in any single event
    * must come from the same external entity, so that the Locator
    * provides useful information.</p>
    *
    * <p>The application must not attempt to read from the array
    * outside of the specified range.</p>
    *
    * <p>Note that some parsers will report whitespace using the
    * ignorableWhitespace() method rather than this one (validating
    * parsers must do so).</p>
    *
    * @param ch The characters from the XML document.
    * @param start The start position in the array.
    * @param length The number of characters to read from the array.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see #ignorableWhitespace 
    * @see org.xml.sax.Locator
    *)
  procedure characters (ch :WideString; start, length :Integer);
    //throws SAXException;


  (**
    * Receive notification of ignorable whitespace in element content.
    *
    * <p>Validating Parsers must use this method to report each chunk
    * of ignorable whitespace (see the W3C XML 1.0 recommendation,
    * section 2.10): non-validating parsers may also use this method
    * if they are capable of parsing and using content models.</p>
    *
    * <p>SAX parsers may return all contiguous whitespace in a single
    * chunk, or they may split it into several chunks; however, all of
    * the characters in any single event must come from the same
    * external entity, so that the Locator provides useful
    * information.</p>
    *
    * <p>The application must not attempt to read from the array
    * outside of the specified range.</p>
    *
    * @param ch The characters from the XML document.
    * @param start The start position in the array.
    * @param length The number of characters to read from the array.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see #characters
    *)
  procedure ignorableWhitespace (ch :WideString; start, length :Integer);
    //throws SAXException;


  (**
    * Receive notification of a processing instruction.
    *
    * <p>The Parser will invoke this method once for each processing
    * instruction found: note that processing instructions may occur
    * before or after the main document element.</p>
    *
    * <p>A SAX parser should never report an XML declaration (XML 1.0,
    * section 2.8) or a text declaration (XML 1.0, section 4.3.1)
    * using this method.</p>
    *
    * @param target The processing instruction target.
    * @param data The processing instruction data, or null if
    *        none was supplied.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    *)
  procedure processingInstruction (target, data :WideString);
    // throws SAXException;

  (**
    * Receive notification of the current position int the input stream.
    * @param pos The current position.
    * @param len The total length of the input stream.
    *)
  procedure position(pos, len :Longint);

end;

// SAX DTD handler.

(**
  * Receive notification of basic DTD-related events.
  *
  * <p>If a SAX application needs information about notations and
  * unparsed entities, then the application implements this 
  * interface and registers an instance with the SAX parser using 
  * the parser's setDTDHandler method.  The parser uses the 
  * instance to report notation and unparsed entity declarations to 
  * the application.</p>
  *
  * <p>The SAX parser may report these events in any order, regardless
  * of the order in which the notations and unparsed entities were
  * declared; however, all DTD events must be reported after the
  * document handler's startDocument event, and before the first
  * startElement event.</p>
  *
  * <p>It is up to the application to store the information for 
  * future use (perhaps in a hash table or object tree).
  * If the application encounters attributes of type "NOTATION",
  * "ENTITY", or "ENTITIES", it can use the information that it
  * obtained through this interface to find the entity and/or
  * notation corresponding with the attribute value.</p>
  *
  * <p>The HandlerBase class provides a default implementation
  * of this interface, which simply ignores the events.</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.Parser#setDTDHandler
  * @see org.xml.sax.HandlerBase
  *)
   IDTDHandler = interface
   ['{CC7055A0-9F96-4631-B9E9-4BA0C6A25BBA}']
  (**
    * Receive notification of a notation declaration event.
    *
    * <p>It is up to the application to record the notation for later
    * reference, if necessary.</p>
    *
    * <p>If a system identifier is present, and it is a URL, the SAX
    * parser must resolve it fully before passing it to the
    * application.</p>
    *
    * @param name The notation name.
    * @param publicId The notation's public identifier, or null if
    *        none was given.
    * @param systemId The notation's system identifier, or null if
    *        none was given.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see #unparsedEntityDecl
    * @see org.xml.sax.AttributeList
    *)
  procedure notationDecl (name, publicId, systemId :WideString);
    //throws SAXException;


  (**
    * Receive notification of an unparsed entity declaration event.
    *
    * <p>Note that the notation name corresponds to a notation
    * reported by the notationDecl() event.  It is up to the
    * application to record the entity for later reference, if
    * necessary.</p>
    *
    * <p>If the system identifier is a URL, the parser must resolve it
    * fully before passing it to the application.</p>
    *
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @param name The unparsed entity's name.
    * @param publicId The entity's public identifier, or null if none
    *        was given.
    * @param systemId The entity's system identifier (it must always
    *        have one).
    * @param notation name The name of the associated notation.
    * @see #notationDecl
    * @see org.xml.sax.AttributeList
    *)
  procedure unparsedEntityDecl (name, publicId, systemId, notationName :WideString);
    //throws SAXException;

  end;

// SAX entity resolver.
(**
  * Basic interface for resolving entities.
  *
  * <p>If a SAX application needs to implement customized handling
  * for external entities, it must implement this interface and
  * register an instance with the SAX parser using the parser's
  * setEntityResolver method.</p>
  *
  * <p>The parser will then allow the application to intercept any
  * external entities (including the external DTD subset and external
  * parameter entities, if any) before including them.</p>
  *
  * <p>Many SAX applications will not need to implement this interface,
  * but it will be especially useful for applications that build
  * XML documents from databases or other specialised input sources,
  * or for applications that use URI types other than URLs.</p>
  *
  * <p>The following resolver would provide the application
  * with a special character stream for the entity with the system
  * identifier "http://www.myhost.com/today":</p>
  *
  * <pre>
  * import org.xml.sax.EntityResolver;
  * import org.xml.sax.InputSource;
  *
  * public class MyResolver implements EntityResolver {
  *   public InputSource resolveEntity (String publicId, String systemId)
  *   {
  *     if (systemId.equals("http://www.myhost.com/today")) {
  *              // return a special input source
  *       MyReader reader = new MyReader();
  *       return new InputSource(reader);
  *     } else {
  *              // use the default behaviour
  *       return null;
  *     }
  *   }
  * }
  * </pre>
  *
  * <p>The application can also use this interface to redirect system
  * identifiers to local URIs or to look up replacements in a catalog
  * (possibly by using the public identifier).</p>
  *
  * <p>The HandlerBase class implements the default behaviour for
  * this interface, which is simply always to return null (to request
  * that the parser use the default system identifier).</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.Parser#setEntityResolver
  * @see org.xml.sax.InputSource
  * @see org.xml.sax.HandlerBase
  *)
  IEntityResolver = interface
  ['{71A26825-8AA6-4D28-94D1-08EE52F05CF3}']
  (**
    * Allow the application to resolve external entities.
    *
    * <p>The Parser will call this method before opening any external
    * entity except the top-level document entity (including the
    * external DTD subset, external entities referenced within the
    * DTD, and external entities referenced within the document
    * element): the application may request that the parser resolve
    * the entity itself, that it use an alternative URI, or that it
    * use an entirely different input source.</p>
    *
    * <p>Application writers can use this method to redirect external
    * system identifiers to secure and/or local URIs, to look up
    * public identifiers in a catalogue, or to read an entity from a
    * database or other input source (including, for example, a dialog
    * box).</p>
    *
    * <p>If the system identifier is a URL, the SAX parser must
    * resolve it fully before reporting it to the application.</p>
    *
    * @param publicId The public identifier of the external entity
    *        being referenced, or null if none was supplied.
    * @param systemId The system identifier of the external entity
    *        being referenced.
    * @return An InputSource object describing the new input source,
    *         or null to request that the parser open a regular
    *         URI connection to the system identifier.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @exception java.io.IOException A Java-specific IO exception,
    *            possibly the result of creating a new InputStream
    *            or Reader for the InputSource.
    * @see org.xml.sax.InputSource
    *)
  function resolveEntity (publicId, systemId :WideString) :IInputSource;
    //throws SAXException, IOException;

  end;

// SAX error handler.
(**
  * Basic interface for SAX error handlers.
  *
  * <p>If a SAX application needs to implement customized error
  * handling, it must implement this interface and then register an
  * instance with the SAX parser using the parser's setErrorHandler
  * method.  The parser will then report all errors and warnings
  * through this interface.</p>
  *
  * <p> The parser shall use this interface instead of throwing an
  * exception: it is up to the application whether to throw an
  * exception for different types of errors and warnings.  Note,
  * however, that there is no requirement that the parser continue to
  * provide useful information after a call to fatalError (in other
  * words, a SAX driver class could catch an exception and report a
  * fatalError).</p>
  *
  * <p>The HandlerBase class provides a default implementation of this
  * interface, ignoring warnings and recoverable errors and throwing a
  * SAXParseException for fatal errors.  An application may extend
  * that class rather than implementing the complete interface
  * itself.</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.Parser#setErrorHandler
  * @see org.xml.sax.SAXParseException 
  * @see org.xml.sax.HandlerBase
  *)
  IErrorHandler = interface
  ['{1D62B415-7C67-46AE-AFD2-24162831A2DF}']
  (**
    * Receive notification of a warning.
    *
    * <p>SAX parsers will use this method to report conditions that
    * are not errors or fatal errors as defined by the XML 1.0
    * recommendation.  The default behaviour is to take no action.</p>
    *
    * <p>The SAX parser must continue to provide normal parsing events
    * after invoking this method: it should still be possible for the
    * application to process the document through to the end.</p>
    *
    * @param exception The warning information encapsulated in a
    *                  SAX parse exception.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.SAXParseException
    *)
  procedure warning (exception :SAXParseException);
    // throws SAXException;

  (**
    * Receive notification of a recoverable error.
    *
    * <p>This corresponds to the definition of "error" in section 1.2
    * of the W3C XML 1.0 Recommendation.  For example, a validating
    * parser would use this callback to report the violation of a
    * validity constraint.  The default behaviour is to take no
    * action.</p>
    *
    * <p>The SAX parser must continue to provide normal parsing events
    * after invoking this method: it should still be possible for the
    * application to process the document through to the end.  If the
    * application cannot do so, then the parser should report a fatal
    * error even if the XML 1.0 recommendation does not require it to
    * do so.</p>
    *
    * @param exception The error information encapsulated in a
    *                  SAX parse exception.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.SAXParseException 
    *)
  procedure error (exception :SAXParseException);
    //throws SAXException;


  (**
    * Receive notification of a non-recoverable error.
    *
    * <p>This corresponds to the definition of "fatal error" in
    * section 1.2 of the W3C XML 1.0 Recommendation.  For example, a
    * parser would use this callback to report the violation of a
    * well-formedness constraint.</p>
    *
    * <p>The application must assume that the document is unusable
    * after the parser has invoked this method, and should continue
    * (if at all) only for the sake of collecting addition error
    * messages: in fact, SAX parsers are free to stop reporting any
    * other events once this method has been invoked.</p>
    *
    * @param exception The error information encapsulated in a
    *                  SAX parse exception.  
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.SAXParseException
    *)
  procedure fatalError (exception :SAXParseException);
    //throws SAXException;
  end;

// SAX default handler base class.
(**
  * Default base class for handlers.
  *
  * <p>This class implements the default behaviour for four SAX
  * interfaces: EntityResolver, DTDHandler, DocumentHandler,
  * and ErrorHandler.</p>
  *
  * <p>Application writers can extend this class when they need to
  * implement only part of an interface; parser writers can
  * instantiate this class to provide default handlers when the
  * application has not supplied its own.</p>
  *
  * <p>Note that the use of this class is optional.</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.EntityResolver
  * @see org.xml.sax.DTDHandler
  * @see org.xml.sax.DocumentHandler
  * @see org.xml.sax.ErrorHandler
  *)
  THandlerBase = class( TInterfacedObject,
                        IEntityResolver,
                        IDTDHandler,
	                      IDocumentHandler,
                        IErrorHandler)
  public
  //////////////////////////////////////////////////////////////////////
  // Default implementation of the EntityResolver interface.
  //////////////////////////////////////////////////////////////////////

  (**
    * Resolve an external entity.
    *
    * <p>Always return null, so that the parser will use the system
    * identifier provided in the XML document.  This method implements
    * the SAX default behaviour: application writers can override it
    * in a subclass to do special translations such as catalog lookups
    * or URI redirection.</p>
    *
    * @param publicId The public identifer, or null if none is
    *                 available.
    * @param systemId The system identifier provided in the XML
    *                 document.
    * @return The new input source, or null to require the
    *         default behaviour.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.EntityResolver#resolveEntity
    *)
  function resolveEntity (publicId, systemId :WideString) :IInputSource;       virtual;
    //throws SAXException

  //////////////////////////////////////////////////////////////////////
  // Default implementation of DTDHandler interface.
  //////////////////////////////////////////////////////////////////////


  (**
    * Receive notification of a notation declaration.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method in a subclass if they wish to keep track of the notations
    * declared in a document.</p>
    *
    * @param name The notation name.
    * @param publicId The notation public identifier, or null if not
    *                 available.
    * @param systemId The notation system identifier.
    * @see org.xml.sax.DTDHandler#notationDecl
    *)
  procedure notationDecl (name, publicId, systemId :WideString);               virtual;


  (**
    * Receive notification of an unparsed entity declaration.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method in a subclass to keep track of the unparsed entities
    * declared in a document.</p>
    *
    * @param name The entity name.
    * @param publicId The entity public identifier, or null if not
    *                 available.
    * @param systemId The entity system identifier.
    * @param notationName The name of the associated notation.
    * @see org.xml.sax.DTDHandler#unparsedEntityDecl
    *)
  procedure unparsedEntityDecl (name, publicId, systemId, notationName :WideString);   virtual;

  //////////////////////////////////////////////////////////////////////
  // Default implementation of DocumentHandler interface.
  //////////////////////////////////////////////////////////////////////


  (**
    * Receive a Locator object for document events.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method in a subclass if they wish to store the locator for use
    * with other document events.</p>
    *
    * @param locator A locator for all SAX document events.
    * @see org.xml.sax.DocumentHandler#setDocumentLocator
    * @see org.xml.sax.Locator
    *)
  procedure setDocumentLocator (locator :TLocator);                                    virtual;


  (**
    * Receive notification of the beginning of the document.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method in a subclass to take specific actions at the beginning
    * of a document (such as allocating the root node of a tree or
    * creating an output file).</p>
    *
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.DocumentHandler#startDocument
    *)
  procedure startDocument;                                                             virtual;
    // throws SAXException

  (**
    * Receive notification of the end of the document.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method in a subclass to take specific actions at the beginning
    * of a document (such as finalising a tree or closing an output
    * file).</p>
    *
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.DocumentHandler#endDocument
    *)
  procedure endDocument;                                                               virtual;
    // throws SAXException


  (**
    * Receive notification of the start of an element.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method in a subclass to take specific actions at the start of
    * each element (such as allocating a new tree node or writing
    * output to a file).</p>
    *
    * @param name The element type name.
    * @param attributes The specified or defaulted attributes.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.DocumentHandler#startElement
    *)
  procedure startElement (name :WideString; atts :TAttributeList);
  virtual;
    // throws SAXException

  (**
    * Receive notification of the end of an element.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method in a subclass to take specific actions at the end of
    * each element (such as finalising a tree node or writing
    * output to a file).</p>
    *
    * @param name The element type name.
    * @param attributes The specified or defaulted attributes.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.DocumentHandler#endElement
    *)
  procedure endElement (name :WideString);
  virtual;
    // throws SAXException


  (**
    * Receive notification of character data inside an element.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method to take specific actions for each chunk of character data
    * (such as adding the data to a node or buffer, or printing it to
    * a file).</p>
    *
    * @param ch The characters.
    * @param start The start position in the character array.
    * @param length The number of characters to use from the
    *               character array.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.DocumentHandler#characters
    *)
  procedure characters (ch :WideString; start, length :Integer);
  virtual;
    // throws SAXException

  (**
    * Receive notification of ignorable whitespace in element content.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method to take specific actions for each chunk of ignorable
    * whitespace (such as adding data to a node or buffer, or printing
    * it to a file).</p>
    *
    * @param ch The whitespace characters.
    * @param start The start position in the character array.
    * @param length The number of characters to use from the
    *               character array.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.DocumentHandler#ignorableWhitespace
    *)
  procedure ignorableWhitespace (ch :WideString; start, length :Integer);
  virtual;
    // throws SAXException

  (**
    * Receive notification of a processing instruction.
    *
    * <p>By default, do nothing.  Application writers may override this
    * method in a subclass to take specific actions for each
    * processing instruction, such as setting status variables or
    * invoking other methods.</p>
    *
    * @param target The processing instruction target.
    * @param data The processing instruction data, or null if
    *             none is supplied.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.DocumentHandler#processingInstruction
    *)
  procedure processingInstruction (target, data :WideString);
  virtual;
    // throws SAXException

  //////////////////////////////////////////////////////////////////////
  // Default implementation of the ErrorHandler interface.
  //////////////////////////////////////////////////////////////////////


  (**
    * Receive notification of a parser warning.
    *
    * <p>The default implementation does nothing.  Application writers
    * may override this method in a subclass to take specific actions
    * for each warning, such as inserting the message in a log file or
    * printing it to the console.</p>
    *
    * @param e The warning information encoded as an exception.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.ErrorHandler#warning
    * @see org.xml.sax.SAXParseException
    *)
  procedure warning (e : SAXParseException);
  virtual;
    // throws SAXException

  (**
    * Receive notification of a recoverable parser error.
    *
    * <p>The default implementation does nothing.  Application writers
    * may override this method in a subclass to take specific actions
    * for each error, such as inserting the message in a log file or
    * printing it to the console.</p>
    *
    * @param e The warning information encoded as an exception.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.ErrorHandler#warning
    * @see org.xml.sax.SAXParseException
    *)
  procedure error (e :SAXParseException);
  virtual;
    // throws SAXException

  (**
    * Report a fatal XML parsing error.
    *
    * <p>The default implementation throws a SAXParseException.
    * Application writers may override this method in a subclass if
    * they need to take specific actions for each fatal error (such as
    * collecting all of the errors into a single report): in any case,
    * the application must stop all regular processing when this
    * method is invoked, since the document is no longer reliable, and
    * the parser may no longer report parsing events.</p>
    *
    * @param e The error information encoded as an exception.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @see org.xml.sax.ErrorHandler#fatalError
    * @see org.xml.sax.SAXParseException
    *)
    procedure fatalError (e :SAXParseException);
    virtual;
      // throws SAXException

    (**
      * Receive notification of the current position int the input stream.
      * @param pos The current position.
      * @param len The total length of the input stream.
      *)
    procedure position(pos, len :Longint);
    virtual;
  end;

// SAX input source.

  IInputSource = interface
  ['{9511A3E7-AD2D-43B7-A935-371C516E182A}']
  (**
    * Set the public identifier for this input source.
    *
    * <p>The public identifier is always optional: if the application
    * writer includes one, it will be provided as part of the
    * location information.</p>
    *
    * @param publicId The public identifier as a string.
    * @see #getPublicId
    * @see org.xml.sax.Locator#getPublicId
    * @see org.xml.sax.SAXParseException#getPublicId
    *)
  procedure setPublicId (publicId :WideString);


  (**
    * Get the public identifier for this input source.
    *
    * @return The public identifier, or null if none was supplied.
    * @see #setPublicId
    *)
  function getPublicId :WideString;


  (**
    * Set the system identifier for this input source.
    *
    * <p>The system identifier is optional if there is a byte stream
    * or a character stream, but it is still useful to provide one,
    * since the application can use it to resolve relative URIs
    * and can include it in error messages and warnings (the parser
    * will attempt to open a connection to the URI only if
    * there is no byte stream or character stream specified).</p>
    *
    * <p>If the application knows the character encoding of the
    * object pointed to by the system identifier, it can register
    * the encoding using the setEncoding method.</p>
    *
    * <p>If the system ID is a URL, it must be fully resolved.</p>
    *
    * @param systemId The system identifier as a string.
    * @see #setEncoding
    * @see #getSystemId
    * @see org.xml.sax.Locator#getSystemId
    * @see org.xml.sax.SAXParseException#getSystemId
    *)
  procedure setSystemId (systemId :WideString);


  (**
    * Get the system identifier for this input source.
    *
    * <p>The getEncoding method will return the character encoding
    * of the object pointed to, or null if unknown.</p>
    *
    * <p>If the system ID is a URL, it will be fully resolved.</p>
    *
    * @return The system identifier.
    * @see #setSystemId
    * @see #getEncoding
    *)
  function getSystemId :WideString;


  (**
    * Set the byte stream for this input source.
    *
    * <p>The SAX parser will ignore this if there is also a character
    * stream specified, but it will use a byte stream in preference
    * to opening a URI connection itself.</p>
    *
    * <p>If the application knows the character encoding of the
    * byte stream, it should set it with the setEncoding method.</p>
    *
    * @param byteStream A byte stream containing an XML document or
    *        other entity.
    * @see #setEncoding
    * @see #getByteStream
    * @see #getEncoding
    * @see java.io.InputStream
    *)
  procedure setByteStream (byteStream :TStream);


  (**
    * Get the byte stream for this input source.
    *
    * <p>The getEncoding method will return the character
    * encoding for this byte stream, or null if unknown.</p>
    *
    * @return The byte stream, or null if none was supplied.
    * @see #getEncoding
    * @see #setByteStream
    *)
  function getByteStream :TStream;


  (**
    * Set the character encoding, if known.
    *
    * <p>The encoding must be a string acceptable for an
    * XML encoding declaration (see section 4.3.3 of the XML 1.0
    * recommendation).</p>
    *
    * <p>This method has no effect when the application provides a
    * character stream.</p>
    *
    * @param encoding A string describing the character encoding.
    * @see #setSystemId
    * @see #setByteStream
    * @see #getEncoding
    *)
  procedure setEncoding (encoding :WideString);


  (**
    * Get the character encoding for a byte stream or URI.
    *
    * @return The encoding, or null if none was supplied.
    * @see #setByteStream
    * @see #getSystemId
    * @see #getByteStream
    *)
  function getEncoding :WideString;


  (**
    * Set the character stream for this input source.
    *
    * <p>If there is a character stream specified, the SAX parser
    * will ignore any byte stream and will not attempt to open
    * a URI connection to the system identifier.</p>
    *
    * @param characterStream The character stream containing the
    *        XML document or other entity.
    * @see #getCharacterStream
    * @see java.io.Reader
    *)
  procedure setCharacterStream (characterStream :TStream);


  (**
    * Get the character stream for this input source.
    *
    * @return The character stream, or null if none was supplied.
    * @see #setCharacterStream
    *)
  function getCharacterStream :TStream;
end;


(**
  * A single input source for an XML entity.
  *
  * <p>This class allows a SAX application to encapsulate information
  * about an input source in a single object, which may include
  * a public identifier, a system identifier, a byte stream (possibly
  * with a specified encoding), and/or a character stream.</p>
  *
  * <p>There are two places that the application will deliver this
  * input source to the parser: as the argument to the Parser.parse
  * method, or as the return value of the EntityResolver.resolveEntity
  * method.</p>
  *
  * <p>The SAX parser will use the InputSource object to determine how
  * to read XML input.  If there is a character stream available, the
  * parser will read that stream directly; if not, the parser will use
  * a byte stream, if available; if neither a character stream nor a
  * byte stream is available, the parser will attempt to open a URI
  * connection to the resource identified by the system
  * identifier.</p>
  *
  * <p>An InputSource object belongs to the application: the SAX parser
  * shall never modify it in any way (it may modify a copy if 
  * necessary).</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.Parser#parse
  * @see org.xml.sax.EntityResolver#resolveEntity
  * @see java.io.InputStream
  * @see java.io.Reader
  *)


  TInputSource = class(TInterfacedObject, IInputSource)

  (**
    * Zero-argument default constructor.
    *
    * @see #setPublicId
    * @see #setSystemId
    * @see #setByteStream
    * @see #setCharacterStream
    * @see #setEncoding
    *)
  constructor Create; overload;

  (**
    * Create a new input source with a system identifier.
    *
    * <p>Applications may use setPublicId to include a
    * public identifier as well, or setEncoding to specify
    * the character encoding, if known.</p>
    *
    * <p>If the system identifier is a URL, it must be full resolved.</p>
    *
    * @param systemId The system identifier (URI).
    * @see #setPublicId
    * @see #setSystemId
    * @see #setByteStream
    * @see #setEncoding
    * @see #setCharacterStream
    *)
  constructor Create(systemId :WideString); overload;

  (**
    * Create a new input source with a byte stream.
    *
    * <p>Application writers may use setSystemId to provide a base
    * for resolving relative URIs, setPublicId to include a
    * public identifier, and/or setEncoding to specify the object's
    * character encoding.</p>
    *
    * @param byteStream The raw byte stream containing the document.
    * @see #setPublicId
    * @see #setSystemId
    * @see #setEncoding
    * @see #setByteStream
    * @see #setCharacterStream
    *)
  constructor Create( byteStream :TStream); overload;
  (*
    setByteStream(byteStream);
  *)


  (**
    * Create a new input source with a character stream.
    *
    * <p>Application writers may use setSystemId() to provide a base 
    * for resolving relative URIs, and setPublicId to include a 
    * public identifier.</p>
    *
    * <p>The character stream shall not include a byte order mark.</p>
    *
    * @see #setPublicId
    * @see #setSystemId
    * @see #setByteStream
    * @see #setCharacterStream
    *)
  //constructor Create(characterStream :TStream); overload;
  (*
    setCharacterStream(characterStream);
  *)


  (**
    * Set the public identifier for this input source.
    *
    * <p>The public identifier is always optional: if the application
    * writer includes one, it will be provided as part of the
    * location information.</p>
    *
    * @param publicId The public identifier as a string.
    * @see #getPublicId
    * @see org.xml.sax.Locator#getPublicId
    * @see org.xml.sax.SAXParseException#getPublicId
    *)
  procedure setPublicId (publicId :WideString);
  (*
    this.publicId = publicId;
  *)


  (**
    * Get the public identifier for this input source.
    *
    * @return The public identifier, or null if none was supplied.
    * @see #setPublicId
    *)
  function getPublicId :WideString;
  (*
    return publicId;
  *)


  (**
    * Set the system identifier for this input source.
    *
    * <p>The system identifier is optional if there is a byte stream
    * or a character stream, but it is still useful to provide one,
    * since the application can use it to resolve relative URIs
    * and can include it in error messages and warnings (the parser
    * will attempt to open a connection to the URI only if
    * there is no byte stream or character stream specified).</p>
    *
    * <p>If the application knows the character encoding of the
    * object pointed to by the system identifier, it can register
    * the encoding using the setEncoding method.</p>
    *
    * <p>If the system ID is a URL, it must be fully resolved.</p>
    *
    * @param systemId The system identifier as a string.
    * @see #setEncoding
    * @see #getSystemId
    * @see org.xml.sax.Locator#getSystemId
    * @see org.xml.sax.SAXParseException#getSystemId
    *)
  procedure setSystemId (systemId :WideString);
  (*
    this.systemId = systemId;
  *)


  (**
    * Get the system identifier for this input source.
    *
    * <p>The getEncoding method will return the character encoding
    * of the object pointed to, or null if unknown.</p>
    *
    * <p>If the system ID is a URL, it will be fully resolved.</p>
    *
    * @return The system identifier.
    * @see #setSystemId
    * @see #getEncoding
    *)
  function getSystemId :WideString;
  (*
    return systemId;
  *)


  (**
    * Set the byte stream for this input source.
    *
    * <p>The SAX parser will ignore this if there is also a character
    * stream specified, but it will use a byte stream in preference
    * to opening a URI connection itself.</p>
    *
    * <p>If the application knows the character encoding of the
    * byte stream, it should set it with the setEncoding method.</p>
    *
    * @param byteStream A byte stream containing an XML document or
    *        other entity.
    * @see #setEncoding
    * @see #getByteStream
    * @see #getEncoding
    * @see java.io.InputStream
    *)
  procedure setByteStream (byteStream :TStream);
  (*
    this.byteStream = byteStream;
  *)


  (**
    * Get the byte stream for this input source.
    *
    * <p>The getEncoding method will return the character
    * encoding for this byte stream, or null if unknown.</p>
    *
    * @return The byte stream, or null if none was supplied.
    * @see #getEncoding
    * @see #setByteStream
    *)
  function getByteStream :TStream;
  (*
    return byteStream;
  *)


  (**
    * Set the character encoding, if known.
    *
    * <p>The encoding must be a string acceptable for an
    * XML encoding declaration (see section 4.3.3 of the XML 1.0
    * recommendation).</p>
    *
    * <p>This method has no effect when the application provides a
    * character stream.</p>
    *
    * @param encoding A string describing the character encoding.
    * @see #setSystemId
    * @see #setByteStream
    * @see #getEncoding
    *)
  procedure setEncoding (encoding :WideString);
  (*
    this.encoding = encoding;
  *)


  (**
    * Get the character encoding for a byte stream or URI.
    *
    * @return The encoding, or null if none was supplied.
    * @see #setByteStream
    * @see #getSystemId
    * @see #getByteStream
    *)
  function getEncoding :WideString;
  (*
    return encoding;
  *)


  (**
    * Set the character stream for this input source.
    *
    * <p>If there is a character stream specified, the SAX parser
    * will ignore any byte stream and will not attempt to open
    * a URI connection to the system identifier.</p>
    *
    * @param characterStream The character stream containing the
    *        XML document or other entity.
    * @see #getCharacterStream
    * @see java.io.Reader
    *)
  procedure setCharacterStream (characterStream :TStream);
  (*
    this.characterStream = characterStream;
  *)


  (**
    * Get the character stream for this input source.
    *
    * @return The character stream, or null if none was supplied.
    * @see #setCharacterStream
    *)
  function getCharacterStream :TStream;
  (*
    return characterStream;
  *)


  //////////////////////////////////////////////////////////////////////
  // Internal state.
  //////////////////////////////////////////////////////////////////////
  private
         publicId         :WideString;
         systemId         :WideString;
         encoding         :WideString;
         byteStream       :TStream;
         characterStream  :TStream;
  end;

// SAX locator interface for document events.

(**
  * Interface for associating a SAX event with a document location.
  *
  * <p>If a SAX parser provides location information to the SAX
  * application, it does so by implementing this interface and then
  * passing an instance to the application using the document
  * handler's setDocumentLocator method.  The application can use the
  * object to obtain the location of any other document handler event
  * in the XML source document.</p>
  *
  * <p>Note that the results returned by the object will be valid only
  * during the scope of each document handler method: the application
  * will receive unpredictable results if it attempts to use the
  * locator at any other time.</p>
  *
  * <p>SAX parsers are not required to supply a locator, but they are
  * very strong encouraged to do so.  If the parser supplies a
  * locator, it must do so before reporting any other document events.
  * If no locator has been set by the time the application receives
  * the startDocument event, the application should assume that a
  * locator is not available.</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.DocumentHandler#setDocumentLocator 
  *)
  TLocator = class
  (**
    * Return the public identifier for the current document event.
    * <p>This will be the public identifier
    * @return A string containing the public identifier, or
    *         null if none is available.
    * @see #getSystemId
    *)
  function getPublicId :WideString; virtual; abstract;


  (**
    * Return the system identifier for the current document event.
    *
    * <p>If the system identifier is a URL, the parser must resolve it
    * fully before passing it to the application.</p>
    *
    * @return A string containing the system identifier, or null
    *         if none is available.
    * @see #getPublicId
    *)
  function getSystemId :WideString; virtual; abstract;

  (**
    * Return the line number where the current document event ends.
    * Note that this is the line position of the first character
    * after the text associated with the document event.
    * @return The line number, or -1 if none is available.
    * @see #getColumnNumber
    *)
  function getLineNumber :Integer; virtual; abstract;


  (**
    * Return the column number where the current document event ends.
    * Note that this is the column number of the first
    * character after the text associated with the document
    * event.  The first column in a line is position 1.
    * @return The column number, or -1 if none is available.
    * @see #getLineNumber
    *)
  function getColumnNumber :Integer; virtual; abstract;

  end;

// SAX parser interface.

(**
  * Basic interface for SAX (Simple API for XML) parsers.
  *
  * <p>All SAX parsers must implement this basic interface: it allows
  * applications to register handlers for different types of events
  * and to initiate a parse from a URI, or a character stream.</p>
  *
  * <p>All SAX parsers must also implement a zero-argument constructor
  * (though other constructors are also allowed).</p>
  *
  * <p>SAX parsers are reusable but not re-entrant: the application
  * may reuse a parser object (possibly with a different input source)
  * once the first parse has completed successfully, but it may not
  * invoke the parse() methods recursively within a parse.</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.EntityResolver
  * @see org.xml.sax.DTDHandler
  * @see org.xml.sax.DocumentHandler
  * @see org.xml.sax.ErrorHandler
  * @see org.xml.sax.HandlerBase
  * @see org.xml.sax.InputSource
  *)
  IParser = interface
  ['{A73A5546-8237-4B82-ABDB-BA293790F812}']
  (**
    * Allow an application to request a locale for errors and warnings.
    *
    * <p>SAX parsers are not required to provide localisation for errors
    * and warnings; if they cannot support the requested locale,
    * however, they must throw a SAX exception.  Applications may
    * not request a locale change in the middle of a parse.</p>
    *
    * @param locale A Java Locale object.
    * @exception org.xml.sax.SAXException Throws an exception
    *            (using the previous or default locale) if the 
    *            requested locale is not supported.
    * @see org.xml.sax.SAXException
    * @see org.xml.sax.SAXParseException
    *)
  procedure setLocale (locale :WideString);
    // throws SAXException;


  (**
    * Allow an application to register a custom entity resolver.
    *
    * <p>If the application does not register an entity resolver, the
    * SAX parser will resolve system identifiers and open connections
    * to entities itself (this is the default behaviour implemented in
    * HandlerBase).</p>
    *
    * <p>Applications may register a new or different entity resolver
    * in the middle of a parse, and the SAX parser must begin using
    * the new resolver immediately.</p>
    *
    * @param resolver The object for resolving entities.
    * @see EntityResolver
    * @see HandlerBase
    *)
  procedure setEntityResolver (resolver :IEntityResolver);


  (**
    * Allow an application to register a DTD event handler.
    *
    * <p>If the application does not register a DTD handler, all DTD
    * events reported by the SAX parser will be silently
    * ignored (this is the default behaviour implemented by
    * HandlerBase).</p>
    *
    * <p>Applications may register a new or different
    * handler in the middle of a parse, and the SAX parser must
    * begin using the new handler immediately.</p>
    *
    * @param handler The DTD handler.
    * @see DTDHandler
    * @see HandlerBase
    *)
  procedure setDTDHandler (handler :IDTDHandler);


  (**
    * Allow an application to register a document event handler.
    *
    * <p>If the application does not register a document handler, all
    * document events reported by the SAX parser will be silently
    * ignored (this is the default behaviour implemented by
    * HandlerBase).</p>
    *
    * <p>Applications may register a new or different handler in the
    * middle of a parse, and the SAX parser must begin using the new
    * handler immediately.</p>
    *
    * @param handler The document handler.
    * @see DocumentHandler
    * @see HandlerBase
    *)
  procedure setDocumentHandler (handler :IDocumentHandler);


  (**
    * Allow an application to register an error event handler.
    *
    * <p>If the application does not register an error event handler,
    * all error events reported by the SAX parser will be silently
    * ignored, except for fatalError, which will throw a SAXException
    * (this is the default behaviour implemented by HandlerBase).</p>
    *
    * <p>Applications may register a new or different handler in the
    * middle of a parse, and the SAX parser must begin using the new
    * handler immediately.</p>
    *
    * @param handler The error handler.
    * @see ErrorHandler
    * @see SAXException
    * @see HandlerBase
    *)
  procedure setErrorHandler (handler :IErrorHandler);


  (**
    * Parse an XML document.
    *
    * <p>The application can use this method to instruct the SAX parser
    * to begin parsing an XML document from any valid input
    * source (a character stream, a byte stream, or a URI).</p>
    *
    * <p>Applications may not invoke this method while a parse is in
    * progress (they should create a new Parser instead for each
    * additional XML document).  Once a parse is complete, an
    * application may reuse the same Parser object, possibly with a
    * different input source.</p>
    *
    * @param source The input source for the top-level of the
    *        XML document.
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @exception java.io.IOException An IO exception from the parser,
    *            possibly from a byte stream or character stream
    *            supplied by the application.
    * @see org.xml.sax.InputSource
    * @see #parse(java.lang.String)
    * @see #setEntityResolver
    * @see #setDTDHandler
    * @see #setDocumentHandler
    * @see #setErrorHandler
    *)
  procedure parse (source :IInputSource); overload;
    //throws SAXException, IOException;


  (**
    * Parse an XML document from a system identifier (URI).
    *
    * <p>This method is a shortcut for the common case of reading a
    * document from a system identifier.  It is the exact
    * equivalent of the following:</p>
    *
    * <pre>
    * parse(new InputSource(systemId));
    * </pre>
    *
    * <p>If the system identifier is a URL, it must be fully resolved
    * by the application before it is passed to the parser.</p>
    *
    * @param systemId The system identifier (URI).
    * @exception org.xml.sax.SAXException Any SAX exception, possibly
    *            wrapping another exception.
    * @exception java.io.IOException An IO exception from the parser,
    *            possibly from a byte stream or character stream
    *            supplied by the application.
    * @see #parse(org.xml.sax.InputSource)
    *)
  procedure parse (systemId :WideString); overload;
    //throws SAXException, IOException;

  end;

// SAX exception class.

(**
  * Encapsulate a general SAX error or warning.
  *
  * <p>This class can contain basic error or warning information from
  * either the XML parser or the application: a parser writer or
  * application writer can subclass it to provide additional
  * functionality.  SAX handlers may throw this exception or
  * any exception subclassed from it.</p>
  *
  * <p>If the application needs to pass through other types of
  * exceptions, it must wrap those exceptions in a SAXException
  * or an exception derived from a SAXException.</p>
  *
  * <p>If the parser or application needs to include information about a
  * specific location in an XML document, it should use the
  * SAXParseException subclass.</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.SAXParseException
  *)

  SAXException = class (Exception)
  (**
    * Create a new SAXException.
    *
    * @param message The error or warning message.
    * @see org.xml.sax.Parser#setLocale
    *)
  constructor Create(message :WideString); overload;
  (*
    super();
    this.message = message;
    this.exception = null;
  *)


  (**
    * Create a new SAXException wrapping an existing exception.
    *
    * <p>The existing exception will be embedded in the new
    * one, and its message will become the default message for
    * the SAXException.</p>
    *
    * @param e The exception to be wrapped in a SAXException.
    *)
  constructor Create(e: Exception); overload;
  (*
    super();
    this.message = null;
    this.exception = e;
  *)


  (**
    * Create a new SAXException from an existing exception.
    *
    * <p>The existing exception will be embedded in the new
    * one, but the new exception will have its own message.</p>
    *
    * @param message The detail message.
    * @param e The exception to be wrapped in a SAXException.
    * @see org.xml.sax.Parser#setLocale
    *)
  constructor Create(message :WideString; e :Exception); overload;
  (*
    super();
    this.message = message;
    this.exception = e;
  *)


  (**
    * Return a detail message for this exception.
    *
    * <p>If there is a embedded exception, and if the SAXException
    * has no detail message of its own, this method will return
    * the detail message from the embedded exception.</p>
    *
    * @return The error or warning message.
    * @see org.xml.sax.Parser#setLocale
    *)
  function getMessage :WideString;
  (*
    if (message == null && exception != null) {
      return exception.getMessage();
    } else {
      return this.message;
    }
  *)


  (**
    * Return the embedded exception, if any.
    *
    * @return The embedded exception, or null if there is none.
    *)
  function getException :Exception;
  (*
    return exception;
  *)


  (**
    * Convert this exception to a string.
    *
    * @return A string version of this exception.
    *)
  function toString :WideString;
  (*
    return getMessage();
  *)


  //////////////////////////////////////////////////////////////////////
  // Internal state.
  //////////////////////////////////////////////////////////////////////

  private
     message   :WideString;
     exception :Exception ;
  end;

// SAX exception class.
(**
  * Encapsulate an XML parse error or warning.
  *
  * <p>This exception will include information for locating the error
  * in the original XML document.  Note that although the application
  * will receive a SAXParseException as the argument to the handlers
  * in the ErrorHandler interface, the application is not actually
  * required to throw the exception; instead, it can simply read the
  * information in it and take a different action.</p>
  *
  * <p>Since this exception is a subclass of SAXException, it
  * inherits the ability to wrap another exception.</p>
  *
  * @author David Megginson (ak117@freenet.carleton.ca)
  * @version 1.0
  * @see org.xml.sax.SAXException
  * @see org.xml.sax.Locator
  * @see org.xml.sax.ErrorHandler
  *)
  SAXParseException = class (SAXException)
  //////////////////////////////////////////////////////////////////////
  // Constructors.
  //////////////////////////////////////////////////////////////////////

  (**
    * Create a new SAXParseException from a message and a Locator.
    *
    * <p>This constructor is especially useful when an application is
    * creating its own exception from within a DocumentHandler
    * callback.</p>
    *
    * @param message The error or warning message.
    * @param locator The locator object for the error or warning.
    * @see org.xml.sax.Locator
    * @see org.xml.sax.Parser#setLocale 
    *)
  constructor Create(message :WideString; locator :TLocator); overload;
  (*
    super(message);
    this.publicId = locator.getPublicId();
    this.systemId = locator.getSystemId();
    this.lineNumber = locator.getLineNumber();
    this.columnNumber = locator.getColumnNumber();
  *)


  (**
    * Wrap an existing exception in a SAXParseException.
    *
    * <p>This constructor is especially useful when an application is
    * creating its own exception from within a DocumentHandler
    * callback, and needs to wrap an existing exception that is not a
    * subclass of SAXException.</p>
    *
    * @param message The error or warning message, or null to
    *                use the message from the embedded exception.
    * @param locator The locator object for the error or warning.
    * @param e Any exception
    * @see org.xml.sax.Locator
    * @see org.xml.sax.Parser#setLocale
    *)
  constructor Create(message :WideString; locator :TLocator; e :Exception); overload;
  (*
    super(message, e);
    this.publicId = locator.getPublicId();
    this.systemId = locator.getSystemId();
    this.lineNumber = locator.getLineNumber();
    this.columnNumber = locator.getColumnNumber();
  *)


  (**
    * Create a new SAXParseException.
    *
    * <p>This constructor is most useful for parser writers.</p>
    *
    * <p>If the system identifier is a URL, the parser must resolve it
    * fully before creating the exception.</p>
    *
    * @param message The error or warning message.
    * @param publicId The public identifer of the entity that generated
    *                 the error or warning.
    * @param systemId The system identifer of the entity that generated
    *                 the error or warning.
    * @param lineNumber The line number of the end of the text that
    *                   caused the error or warning.
    * @param columnNumber The column number of the end of the text that
    *                     cause the error or warning.
    * @see org.xml.sax.Parser#setLocale
    *)
  constructor Create(message, publicId, systemId :WideString;
			    lineNumber, columnNumber :Integer); overload;
  (*
    super(message);
    this.publicId = publicId;
    this.systemId = systemId;
    this.lineNumber = lineNumber;
    this.columnNumber = columnNumber;
  *)


  (**
    * Create a new SAXParseException with an embedded exception.
    *
    * <p>This constructor is most useful for parser writers who
    * need to wrap an exception that is not a subclass of
    * SAXException.</p>
    *
    * <p>If the system identifier is a URL, the parser must resolve it
    * fully before creating the exception.</p>
    *
    * @param message The error or warning message, or null to use
    *                the message from the embedded exception.
    * @param publicId The public identifer of the entity that generated
    *                 the error or warning.
    * @param systemId The system identifer of the entity that generated
    *                 the error or warning.
    * @param lineNumber The line number of the end of the text that
    *                   caused the error or warning.
    * @param columnNumber The column number of the end of the text that
    *                     cause the error or warning.
    * @param e Another exception to embed in this one.
    * @see org.xml.sax.Parser#setLocale
    *)
  constructor Create( message, publicId, systemId :WideString;
                      lineNumber, columnNumber: Integer; e :Exception); overload;
  (*
    super(message, e);
    this.publicId = publicId;
    this.systemId = systemId;
    this.lineNumber = lineNumber;
    this.columnNumber = columnNumber;
  *)


  (**
    * Get the public identifier of the entity where the exception occurred.
    *
    * @return A string containing the public identifier, or null
    *         if none is available.
    * @see org.xml.sax.Locator#getPublicId
    *)
  function getPublicId :WideString;
  (*
    return this.publicId;
  *)


  (**
    * Get the system identifier of the entity where the exception occurred.
    *
    * <p>If the system identifier is a URL, it will be resolved
    * fully.</p>
    *
    * @return A string containing the system identifier, or null
    *         if none is available.
    * @see org.xml.sax.Locator#getSystemId
    *)
  function getSystemId :WideString;
  (*
    return this.systemId;
  *)


  (**
    * The line number of the end of the text where the exception occurred.
    *
    * @return An integer representing the line number, or -1
    *         if none is available.
    * @see org.xml.sax.Locator#getLineNumber
    *)
  function getLineNumber :Integer;
  (*
    return this.lineNumber;
  *)


  (**
    * The column number of the end of the text where the exception occurred.
    *
    * <p>The first column in a line is position 1.</p>
    *
    * @return An integer representing the column number, or -1
    *         if none is available.
    * @see org.xml.sax.Locator#getColumnNumber
    *)
  function getColumnNumber :Integer;
  (*
    return this.columnNumber;
  *)


  //////////////////////////////////////////////////////////////////////
  // Internal state.
  //////////////////////////////////////////////////////////////////////

  private
     publicId_       :WideString;
     systemId_       :WideString;
     lineNumber_     :Integer;
     columnNumber_   :Integer;
  end;

  TBasicLocator = class(TLocator)
  public
    constructor Create;                    overload;
    constructor Create(locator: TLocator); overload;
    function    getColumnNumber: Integer;                 override;
    function    getLineNumber: Integer;                   override;
    function    getPublicId: WideString;                  override;
    function    getSystemId: WideString;                  override;
    procedure   setLineNumber(lineNumber: Integer);       virtual;
    procedure   setPublicId(publicId: WideString);        virtual;
    procedure   setSystemId(systemId: WideString);        virtual;
    procedure   setColumnNumber(columnNumber: Integer);   virtual;
  private
     publicId_       :WideString;
     systemId_       :WideString;
     lineNumber_     :Integer;
     columnNumber_   :Integer;
  end;

  TAttributeList = class
      constructor Create;

      procedure add(name, value, typ :WideString);

      function getLength:Integer;
      function getName(i :Integer): string;
      function getType (i :Integer) :WideString;       overload;
      function getValue (i :Integer):WideString;       overload;
      function getType (name :WideString) :WideString; overload;
      function getValue(name :WideString) :WideString; overload;
  protected
      list_  :IList;
      map_   :IMap;
      types_ :IMap;
  end;

implementation
{ THandlerBase }


procedure THandlerBase.characters(ch: WideString; start, length: Integer);
begin

end;

procedure THandlerBase.endDocument;
begin

end;

procedure THandlerBase.endElement(name: WideString);
begin

end;

procedure THandlerBase.error(e: SAXParseException);
begin
   raise e;
end;

procedure THandlerBase.fatalError(e: SAXParseException);
begin
   raise e;
end;

procedure THandlerBase.ignorableWhitespace(ch: WideString; start, length: Integer);
begin

end;

procedure THandlerBase.notationDecl(name, publicId, systemId: WideString);
begin

end;

procedure THandlerBase.position(pos, len: Integer);
begin

end;

procedure THandlerBase.processingInstruction(target, data: WideString);
begin

end;

function THandlerBase.resolveEntity(publicId, systemId: WideString): IInputSource;
begin
   result := TInputSource.Create;
   result.setPublicId(publicId);
   result.setSystemId(systemId);
   result.setByteStream(TFileStream.Create(systemId, fmOpenRead or fmShareDenyWrite));
   result.setCharacterStream(result.getByteStream)
end;

procedure THandlerBase.setDocumentLocator(locator: TLocator);
begin

end;

procedure THandlerBase.startDocument;
begin

end;

procedure THandlerBase.startElement(name: WideString; atts: TAttributeList);
begin

end;

procedure THandlerBase.unparsedEntityDecl(name, publicId, systemId,
  notationName: WideString);
begin

end;

procedure THandlerBase.warning(e: SAXParseException);
begin
  raise e;
end;

{ IInputSource }

constructor TInputSource.Create(byteStream: TStream);
begin
  inherited Create;
  self.byteStream      := byteStream;
  self.characterStream := byteStream
end;

constructor TInputSource.Create(systemId: WideString);
begin
  inherited Create;
  self.systemId := systemId
end;

(*constructor TInputSource.Create(characterStream: TStream);
begin
  inherited Create;
  self.characterStream := characterStream
end;
*)
function TInputSource.getByteStream: TStream;
begin
    result := self.byteStream
end;

function TInputSource.getCharacterStream: TStream;
begin
   result := self.characterStream
end;

function TInputSource.getEncoding: WideString;
begin
     result := self.encoding
end;

function TInputSource.getPublicId: WideString;
begin
   result := self.publicId
end;

function TInputSource.getSystemId: WideString;
begin
  result := self.systemId
end;

constructor TInputSource.Create;
begin
  inherited Create;

end;

procedure TInputSource.setByteStream(byteStream: TStream);
begin
     self.byteStream := byteStream
end;

procedure TInputSource.setCharacterStream(characterStream: TStream);
begin
     self.characterStream := characterStream
end;

procedure TInputSource.setEncoding(encoding: WideString);
begin
     self.encoding := encoding
end;

procedure TInputSource.setPublicId(publicId: WideString);
begin
     self.publicId := publicId
end;

procedure TInputSource.setSystemId(systemId: WideString);
begin
     self.systemId := systemId
end;

{ SAXException }

constructor SAXException.Create(message: WideString);
begin
     inherited Create(message);
     self.message := message
end;

constructor SAXException.Create(e: Exception);
begin
     inherited Create(e.Message);
     self.exception := e
end;

constructor SAXException.Create(message: WideString; e: Exception);
begin
     inherited Create(e.Message);
     self.message := message;
     self.exception := e
end;

function SAXException.getException: Exception;
begin
   result := self.exception
end;

function SAXException.getMessage: WideString;
begin
     result := self.message
end;

function SAXException.toString: WideString;
begin
     result := self.message
end;

{ SAXParseException }

constructor SAXParseException.Create(message, publicId,
  systemId: WideString; lineNumber, columnNumber: Integer);
begin
  inherited Create(message);
  self.publicId_     := publicId;
  self.systemId_     := systemId;
  self.lineNumber_   := lineNumber;
  self.columnNumber_ := columnNumber
end;

constructor SAXParseException.Create(message: WideString; locator: TLocator; e: Exception);
begin
    inherited Create(message, e);
    self.lineNumber_   := locator.getLineNumber;
    self.columnNumber_ := locator.getColumnNumber;
end;

constructor SAXParseException.Create(message: WideString; locator: TLocator);
begin
    self.lineNumber_   := locator.getLineNumber;
    self.columnNumber_ := locator.getColumnNumber;
    inherited Create(format('%s(%d:%d): %s', [locator.getSystemId,
                                              1+locator.getLineNumber,
                                              locator.getColumnNumber,
                                              message
                                              ]));
end;

constructor SAXParseException.Create(message, publicId,
  systemId: WideString; lineNumber, columnNumber: Integer; e: Exception);
begin

end;

function SAXParseException.getColumnNumber: Integer;
begin
     result := self.columnNumber_
end;

function SAXParseException.getLineNumber: Integer;
begin
     result := self.lineNumber_
end;

function SAXParseException.getPublicId: WideString;
begin
     result := self.publicId_
end;

function SAXParseException.getSystemId: WideString;
begin
     result := self.systemId_
end;

// SAX default implementation for Locator.
// No warranty; no copyright -- use this as you will.

(**
  * Provide an optional convenience implementation of Locator.
  *
  * <p>This class is available mainly for application writers, who
  * can use it to make a persistent snapshot of a locator at any
  * point during a document parse:</p>
  *
  * <pre>
  * Locator locator;
  * Locator startloc;
  *
  * public void setLocator (Locator locator)
  * {
  *         // note the locator
  *   this.locator = locator;
  * }
  *
  * public void startDocument ()
  * {
  *         // save the location of the start of the document
  *         // for future use.
  *   Locator startloc = new LocatorImpl(locator);
  * }
  *</pre>
  *
  * <p>Normally, parser writers will not use this class, since it
  * is more efficient to provide location information only when
  * requested, rather than constantly updating a Locator object.</p>
  *
  * @see org.xml.sax.Locator
  *)

  (**
    * Zero-argument constructor.
    *
    * <p>This will not normally be useful, since the main purpose
    * of this class is to make a snapshot of an existing Locator.</p>
    *)
  constructor TBasicLocator.Create;
  begin
     inherited Create;
  end;


  (**
    * Copy constructor.
    *
    * <p>Create a persistent copy of the current state of a locator.
    * When the original locator changes, this copy will still keep
    * the original values (and it can be used outside the scope of
    * DocumentHandler methods).</p>
    *
    * @param locator The locator to copy.
    *)
  constructor TBasicLocator.Create(locator :TLocator);
  begin
    setPublicId(locator.getPublicId);
    setSystemId(locator.getSystemId);
    setLineNumber(locator.getLineNumber);
    setColumnNumber(locator.getColumnNumber);
  end;


  //////////////////////////////////////////////////////////////////////
  // Implementation of org.xml.sax.Locator
  //////////////////////////////////////////////////////////////////////


  (**
    * Return the saved public identifier.
    *
    * @return The public identifier as a string, or null if none
    *         is available.
    * @see org.xml.sax.Locator#getPublicId
    * @see #setPublicId
    *)
  function TBasicLocator.getPublicId :WideString;
  begin
    result := publicId_;
  end;


  (**
    * Return the saved system identifier.
    *
    * @return The system identifier as a string, or null if none
    *         is available.
    * @see org.xml.sax.Locator#getSystemId
    * @see #setSystemId
    *)
  function TBasicLocator.getSystemId :WideString;
  begin
    result := systemId_;
  end;


  (**
    * Return the saved line number (1-based).
    *
    * @return The line number as an integer, or -1 if none is available.
    * @see org.xml.sax.Locator#getLineNumber
    * @see #setLineNumber
    *)
  function TBasicLocator.getLineNumber :Integer;
  begin
    result := lineNumber_;
  end;


  (**
    * Return the saved column number (1-based).
    *
    * @return The column number as an integer, or -1 if none is available.
    * @see org.xml.sax.Locator#getColumnNumber
    * @see #setColumnNumber
    *)
  function TBasicLocator.getColumnNumber :Integer;
  begin
    result := columnNumber_
  end;


  //////////////////////////////////////////////////////////////////////
  // Setters for the properties (not in org.xml.sax.Locator)
  //////////////////////////////////////////////////////////////////////


  (**
    * Set the public identifier for this locator.
    *
    * @param publicId The new public identifier, or null
    *        if none is available.
    * @see #getPublicId
    *)
  procedure TBasicLocator.setPublicId (publicId :WideString);
  begin
    self.publicId_ := publicId;
  end;


  (**
    * Set the system identifier for this locator.
    *
    * @param systemId The new system identifier, or null
    *        if none is available.
    * @see #getSystemId
    *)
  procedure TBasicLocator.setSystemId (systemId :WideString);
  begin
    self.systemId_ := systemId;
  end;


  (**
    * Set the line number for this locator (1-based).
    *
    * @param lineNumber The line number, or -1 if none is available.
    * @see #getLineNumber
    *)
  procedure TBasicLocator.setLineNumber (lineNumber :Integer);
  begin
    self.lineNumber_ := lineNumber;
  end;


  (*
    * Set the column number for this locator (1-based).
    *
    * @param columnNumber The column number, or -1 if none is available.
    * @see #getColumnNumber
    *)
  procedure TBasicLocator.setColumnNumber (columnNumber :Integer);
  begin
    self.columnNumber_ := columnNumber;
  end;

{ TAttributeList }

procedure TAttributeList.add(name, value, typ: WideString);
var
  iname,
  ivalue :IString;
begin
   iname  := iref(name);
   ivalue := iref(value);
   list_.add(iname);
   if value <> '' then
     map_.put(iname, ivalue);
   if typ <> '' then begin
     if types_ = nil then
        types_ := TTreeMap.create;
     types_.put(iname, iref(UpperCase(typ)));
   end
end;

constructor TAttributeList.Create;
begin
   inherited Create;
   list_  := TArrayList.create;
   map_   := TTreeMap.create;
end;

function TAttributeList.getLength: Integer;
begin
   result := list_.size
end;

function TAttributeList.getType(i: Integer): WideString;
begin
  if types_ = nil then
    result := ''
  else
    result := (types_.get(list_.at(i)) as IString).toString;
end;

function TAttributeList.getName(i: Integer): string;
begin
     result := (list_.at(i) as IString).toString;
end;

function TAttributeList.getType(name: WideString): WideString;
var
  r :IString;
begin
  if types_ = nil then
     result := ''
  else begin
    r := types_.get(iref(name)) as IString;
    if r = nil then
       result := ''
    else
       result := r.toString;
  end
end;

function TAttributeList.getValue(name: WideString): WideString;
var
  r :IString;
begin
  r := map_.get(iref(name)) as IString;
  if r = nil then
    result := ''
  else
    result := r.toString;
end;

function TAttributeList.getValue(i: Integer): WideString;
begin
  result := (map_.get(list_.at(i)) as IString).toString;
end;

end.
