unit MSXMLEngineImpl;

interface

uses
  SysUtils,
  Classes,
  ComObj,
  Activex;

type
  IXSLEngine = interface(IUnknown)
    procedure transform(_in, _out, style: string;
      Params, OutputProperties: array of string);
  end;

  TMSXMLEngineImpl = class(TInterfacedObject, IXSLEngine)
    constructor Create;
    destructor Destroy; override;
    procedure transform(_in, _out, style: string;
      Params,OutputProperties: array of string);
  private
    FXSLTemplateCache: TStrings;
    function LoadXMLDocument(fileName: string): Variant;
    function LoadXSLTemplate(fileName: string): Variant;
  end;

  TMSXMLTemplateCacheItem = class(TObject)
    Template : IDispatch;
    constructor create(t : IDispatch);
  end;

function XSLEngine :IXSLEngine;

implementation

var
  __XSLEngine: IXSLEngine = nil;

function XSLEngine :IXSLEngine;
begin
  if __XSLEngine = nil then
    __XSLEngine := TMSXMLEngineImpl.Create;
  Result := __XSLEngine;
end;

{ TDelphiCompileTests }


constructor TMSXMLEngineImpl.Create;
begin
  CoInitialize(nil);
  inherited;
  FXSLTemplateCache := TStringList.Create();
end;

destructor TMSXMLEngineImpl.Destroy;
begin
  FXSLTemplateCache.Free;
  inherited;
  CoUnInitialize;
end;

function TMSXMLEngineImpl.LoadXMLDocument(fileName: string): Variant;
begin
  Result:=CreateOleObject('Msxml2.FreeThreadedDOMDocument');
  Result.async := False;
  Result.load(fileName);
end;

function TMSXMLEngineImpl.LoadXSLTemplate(fileName: string): VAriant;
var
  d: Variant;
  t: Variant;
  i: Integer;
begin
  i := FXSLTemplateCache.IndexOf(lowercase(fileName));
  if (i > -1) then
  begin
    t:=TMSXMLTemplateCacheItem(FXSLTemplateCache.Objects[i]).Template ;
  end
  else
  begin
    d := LoadXMLDocument(fileName);
    t := CreateOleObject('Msxml2.XSLTemplate');
    t.stylesheet := d.documentElement;
    FXSLTemplateCache.AddObject(lowercase(fileName),TMSXMLTemplateCacheItem.Create(t))
  end;
  Result := t.createProcessor;
end;

procedure TMSXMLEngineImpl.transform(_in, _out, style: string;
  Params,OutputProperties: array of string);
var
  xslp: Variant;
  fs: TFileStream;
  i: integer;
  xmld: Variant;
begin
  xmld := LoadXMLDocument(_in);
  xslp := LoadXSLTemplate(style);
  if length(Params) > 0 then
    for i := 0 to high(Params) div 2 do
    begin
      xslp.addParameter(WideString(Params[i*2]),
        WideString(Params[(i*2) + 1]), '');
    end;
  xslp.input := xmld;
  fs := TFileStream.Create(_out, fmCreate);
  try
    xslp.output := IStream(TStreamAdapter.Create(fs));
    xslp.transform;
  finally
    fs.Free;
  end;
end;

{ TMSXMLTemplateCacheItem }

constructor TMSXMLTemplateCacheItem.create(t: IDispatch);
begin
    Template:=t;
end;

initialization
finalization
  __XSLEngine := nil;
end.
