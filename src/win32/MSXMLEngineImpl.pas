unit MSXMLEngineImpl;

interface

uses SysUtils,
  Classes,
  StyleTasks,ComObj,Activex;

type
  TMSXMLEngineImpl = class(TInterfacedObject, IStyleTaskXSLEngine)
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

implementation
{ TDelphiCompileTests }


constructor TMSXMLEngineImpl.Create;
begin
  inherited;
  FXSLTemplateCache := TStringList.Create();
end;

destructor TMSXMLEngineImpl.Destroy;
begin
  FXSLTemplateCache.Free;
  inherited;
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
  tt: string;
  fs: TFileStream;
  ss: TStringStream;
  i: integer;
  xmld: Variant;
begin
  xmld := LoadXMLDocument(_in);
  xslp := LoadXSLTemplate(style);
  if length(Params) > 0 then
    for i := 0 to high(Params) div 2 do
    begin
      xslp.addParameter(WideString(Params[i]),
        WideString(Params[i + 1]), '');
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


{ TMSXMLTemplateCacheItem }

{ TMSXMLTemplateCacheItem }

constructor TMSXMLTemplateCacheItem.create(t: IDispatch);
begin
    Template:=t;
end;

initialization
  XSLEngine:=TMSXMLEngineImpl.create;
  TProcedure(InitProc);
finalization
  XSLEngine:=nil;
end.