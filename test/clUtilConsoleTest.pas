unit clUtilConsoleTest;

interface

uses
  TestFramework, clUtilConsole;

  { MORE functionality ... need to function to accept TStringList and
    repeatedly return 25? lines at a time until done. Should also handle
    embedded '' with proper spacing at the end }

type
  TTestMore = class(TTestCase)
  private
    FMore: TConsoleMore;
    FMaxLines: Integer;
  protected
    procedure CheckPageContent(PageNo: Integer; ActualContent: string);
    procedure SetupMore(MaxLines: Integer);
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestMore;
    procedure TestMoreNoLines;
  end;

implementation

uses SysUtils;

{ TTestMore }

procedure TTestMore.CheckPageContent(PageNo: Integer; ActualContent: string);
var
  i: Integer;
  StartLine: Integer;
  EndLine: Integer;
  ExpectedContent: string;
begin
  StartLine := (25 * PageNo) - 24;
  EndLine := StartLine + 24;
  ExpectedContent := '';
  if EndLine > FMaxLines then
    EndLine := FMaxLines;
  for i := StartLine to EndLine do
    ExpectedContent := ExpectedContent + IntToStr(i) + #13#10;

  CheckEquals(ExpectedContent, ActualContent, 'page mismatch');
end;

procedure TTestMore.Setup;
begin
  inherited;
end;

procedure TTestMore.SetupMore(MaxLines: Integer);
var
  Text: string;
  i: Integer;
begin
  FMaxLines := MaxLines;
  Text := '';
  for i := 1 to FMaxLines do
    Text := Text + IntToStr(i) + #13#10;
  FMore := TConsoleMore.Create(Text);
end;

procedure TTestMore.TearDown;
begin
  FMore.Free;
  inherited;
end;

procedure TTestMore.TestMore;
var
  PageNo: Integer;
begin
  SetupMore(30);
  PageNo := 0;
  while not FMore.Finished do
  begin
    Inc(PageNo);
    CheckPageContent(PageNo, FMore.CurrentPage);
    FMore.NextPage;
  end;
  CheckEquals(2, PageNo, 'pageno wrong');
end;

procedure TTestMore.TestMoreNoLines;
begin
  SetupMore(0);
  Check(FMore.Finished, '0 lines, should be finished');
end;

initialization
  RegisterTest('', TTestMore);

end.

