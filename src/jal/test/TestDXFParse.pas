unit TestDXFParse;

interface
uses
  TestFramework,

  JalStrings,

  JalDXF;

type
  TDXFParseTests = class(TTestCase)
  protected
    _parser :TDXFParser;
    
    procedure SetUp;    override;
    procedure TearDown; override;
  published
    procedure TestParse;
  end;

implementation

{ TDXFParseTests }

procedure TDXFParseTests.SetUp;
begin
  inherited SetUp;
  _parser := TDXFParser.Create;
end;

procedure TDXFParseTests.TearDown;
begin
  _parser.Free;
  _parser := nil;
  inherited TearDown;
end;

procedure TDXFParseTests.TestParse;
begin
  _parser.parse(FileToString('c:\home\studies\may\REDUTM.dxf'));
end;

initialization
  RegisterTest(TDXFParseTests.Suite);
end.
