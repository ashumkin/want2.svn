(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit EditTasks;

interface
uses
  SysUtils,
  Classes,
  Math,

  JclSysUtils,
  JclStrings,

  WantClasses;

type
  TEditTask = class(TTask)
  protected
    FBuffer   :TStrings;
    FDot      :Integer;

    FFile     :string;
    FLastPat  :string;

    procedure SetDot(Value :Integer);

    function GetText :string;
    procedure SetText(Value :string);

    property Dot    :Integer     read FDot    write SetDot;
    property Buffer :TStrings    read FBuffer write FBuffer;

    function ParseLine(Line :string) :Integer;
  public
    constructor Create(Owner :TScriptElement); override;
    destructor Destroy; override;

    procedure Execute; override;
  published
    property _file :string read FFile   write FFile;
    property text  :string read GetTExt write SetText;
  end;
  TEditor = TEditTask;

  TCustomEditElement = class(TScriptElement)
  protected
    procedure Perform(Editor :TEditor);                                      overload; virtual;
    function  Perform(Buffer :TStrings; FromLine, ToLine :Integer) :Integer; overload; virtual;
    function  Perform(Buffer :TStrings; Line :Integer):Integer;              overload; virtual;
  end;

  TGotoElement = class(TCustomEditElement)
  protected
    FLine :string;

    procedure Perform(Editor :TEditor); override;
  public
    procedure Init; override;
  published
    property line :string read FLine write FLine;
  end;

  TRangeElement = class(TCustomEditElement)
  protected
    FFrom  :string;
    FTo    :string;

    procedure Perform(Editor :TEditor); override;
    function  Perform(Buffer :TStrings; FromLine, ToLine :Integer) :Integer; override;
  published
    property from :string read FFrom write FFrom;
    property _to  :string read FTo   write FTo;
  end;

  TEditElement = class(TRangeElement)
  protected
  end;

  TPrintElement = class(TEditElement)
  protected
    function Perform(Buffer :TStrings; Line :Integer) :Integer;  override;
  public
  end;

  TPatternElement = class(TRangeElement)
  protected
    FPattern :string;
  published
    property pattern :string read FPattern write FPattern;
  end;

  TSearchElement = class(TRangeElement)
  protected
    FPattern :string;
    procedure Perform(Editor :TEditor); override;
    function  Perform(Buffer :TStrings; FromLine, ToLine :Integer):Integer; override;
  published
    property pattern :string read FPattern write FPattern;
  end;

  TGlobalElement = class(TPatternElement)
  protected
    procedure Perform(Editor :TEditor);   override;
  end;


  TDeleteElement = class(TEditElement)
  protected
    function Perform(Buffer :TStrings; FromLine, ToLine :Integer) :Integer; override;
  public
  end;


  TEditFileElement = class(TEditElement)
  protected
    FFile :string;

    procedure Perform(Editor :TEditor);                                      override;
  published
    property _file :string read FFile write FFile;
  end;

  TReadElement = class(TEditFileElement)
  protected
    function  Perform(Buffer :TStrings; FromLine, ToLine :Integer) :Integer; override;
  end;

  TWriteElement = class(TEditFileElement)
  protected
    FAppend :boolean;

    procedure Perform(Editor :TEditor); override;
    function  Perform(Buffer :TStrings; FromLine, ToLine :Integer) :Integer; override;
  published
    property Append :boolean read FAppend write FAppend;
  end;

  TInsertElement = class(TEditElement)
  protected
    FText :TStrings;

    function GetText :string;
    procedure SetText(Value :string);

    function TargetLine(FromLine, ToLine :Integer) :Integer; virtual;

    function Perform(Buffer :TStrings; FromLine, ToLine :Integer) :Integer; override;
  public
    constructor Create(Owner :TScriptElement); override;
    destructor Destroy; override;
  published
    property text :string read GetText write SetText;
  end;

  TAppendElement = class(TInsertElement)
  protected
    function TargetLine(FromLine, ToLine :Integer) :Integer; override;
  end;

implementation

{ TEditTask }

constructor TEditTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FBuffer := TStringList.Create;
end;

destructor TEditTask.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

function TEditTask.GetText: string;
begin
  Result := Buffer.Text;
end;

procedure TEditTask.SetText(Value: string);
var
  S :TStrings;
  i :Integer;
begin
  S := TStringList.Create;
  try
    S.Text := Trim(Value);
    i := 0;
    while (i < S.Count) and (S[i] = '') do
      Inc(i);

    while (i < S.Count) do
    begin
      if S[i] = '.' then
        Buffer.Append('')
      else
        Buffer.Append(S[i]);
      Inc(i);
    end;
  finally
    FreeAndNil(S);
  end;
end;

function TEditTask.ParseLine(Line: string): Integer;
begin
  if (Line = '.') or (Line = '') then
    Result := Dot
  else if Line = '$' then
    Result := Buffer.Count-1
  else if StrLeft(Line, 1) = '+' then
    Result := Min(Buffer.Count-1, Dot + StrToInt(Copy(Line, 2, Length(Line))))
  else if StrLeft(Line, 1) = '-' then
    Result := Max(0, Dot - StrToInt(Copy(Line, 2, Length(Line))))
  else
    Result := Max(0, Min(Buffer.Count, StrToInt(Line)-1));
end;

procedure TEditTask.SetDot(Value: Integer);
begin
  if Value >= 0 then
    FDot := Min(Buffer.Count-1, Value);
end;

procedure TEditTask.Execute;
var
  i :Integer;
begin
  inherited Execute;
  Log;
  if _file <> '' then
    Buffer.LoadFromFile(_file);
  for i := 0 to ChildCount-1 do
  begin
    if Children[i] is TCustomEditElement then
      TCustomEditElement(Children[i]).Perform(Self);
  end;
end;

{ TCustomEditElement }

procedure TCustomEditElement.Perform(Editor: TEditor);
begin
  Editor.Dot := Perform(Editor.Buffer, Editor.Dot, Editor.Dot);
end;


function TCustomEditElement.Perform(Buffer :TStrings; FromLine, ToLine :Integer):Integer;
var
  l :Integer;
begin
  Result := FromLine;
  for l := Max(0,FromLine) to Min(Buffer.Count-1,ToLine) do
    Result := Perform(Buffer, l);
end;

function TCustomEditElement.Perform(Buffer: TStrings; Line: Integer) :Integer;
begin
  Result := Line;
end;

{ TGotoElement }

procedure TGotoElement.Init;
begin
  inherited Init;
  RequireAttribute('line');
end;

procedure TGotoElement.Perform(Editor: TEditor);
begin
  Log(vlVerbose, '%s %s', [TagName, Line]);
  Editor.Dot := Editor.ParseLine(line);
end;

{ TRangeElement }

procedure TRangeElement.Perform(Editor: TEditor);
var
  f, t :Integer;
begin
  f := Editor.ParseLine(from);
  t := Editor.ParseLine(_to);

  Editor.Dot := Perform(Editor.Buffer, f, t);
end;

function TRangeElement.Perform(Buffer: TStrings; FromLine, ToLine: Integer): Integer;
var
  i    :Integer;
begin
  Result := inherited Perform(Buffer, FromLine, ToLine);
  for i := 0 to ChildCount-1 do
  begin
    if Children[i] is TEditElement then
      Result := TCustomEditElement(Children[i]).Perform(Buffer, FromLine, ToLine);
  end;
end;

{ TGlobalElement }

procedure TGlobalElement.Perform(Editor: TEditor);
var
  f, t :Integer;
  l    :Integer;
begin
  Log(vlVerbose, '%s /%s/', [TagName, pattern]);
  if from = '' then
    from := '0';
  if _to = '' then
    _to := '$';

  if pattern = '' then
    pattern := Editor.FLastPat;

  f := Editor.ParseLine(from);
  t := Editor.ParseLine(_to);

  with Editor do
  begin
    for l := Max(0, f) to Min(Buffer.Count-1, t) do
    begin
      Dot := l;
      if (Pattern = '') or (Pos(Pattern, Buffer[l]) <> 0) then
        Dot := Perform(Editor.Buffer, l, l);
    end
  end;
end;

{ TSearchElement }

procedure TSearchElement.Perform(Editor: TEditor);
begin
  Log(vlVerbose, '%s /%s/', [TagName, pattern]);

  if pattern = '' then
    pattern := Editor.FLastPat
  else if pattern <> '' then
    Editor.FLastPat := pattern;

  if _to = '' then
    _to := '$';

  inherited Perform(Editor);
end;

function TSearchElement.Perform(Buffer :TStrings; FromLine, ToLine :Integer) :Integer;
var
  l     :Integer;
  Found :boolean;
begin
  Result := FromLine;
  Found := False;
  for l := Max(0, FromLine) to Min(Buffer.Count-1, ToLine) do
  begin
    if (pattern = '') or (Pos(pattern, Buffer[l]) <> 0) then
    begin
      Result := inherited Perform(Buffer, l, l);
      Found := true;
      break;
    end;
  end;
  if not Found then
    Log(vlWarnings, Format('Pattern "%s" not found', [pattern]));
end;

{ TPrintElement }

function TPrintElement.Perform(Buffer: TStrings; Line: Integer) :Integer;
begin
  Log(Buffer[Line]);
  Result := -1; // print doesn't alter dot
end;

{ TDeleteElement }

function TDeleteElement.Perform(Buffer: TStrings; FromLine, ToLine: Integer) :Integer;
var
  l :Integer;
begin
  Log(vlVerbose, '%s %d-%d', [TagName, FromLine, ToLine]);
  for l := Min(Buffer.Count-1,ToLine) downto Max(0,FromLine) do
    Buffer.Delete(l);
  Result := FromLine;
end;

{ TEditFileElement }

procedure TEditFileElement.Perform(Editor: TEditor);
begin
  if (_file = '') then
  begin
    if (Editor.FFile = '') then
      TaskError('No file name')
    else
      _file := Editor.FFile;
  end;
  inherited Perform(Editor);
end;

{ TReadElement }

function TReadElement.Perform(Buffer: TStrings; FromLine, ToLine: Integer): Integer;
var
  S   :TStringList;
  i   :Integer;
  pos :Integer;
begin
  Log(vlVerbose, '%s %d %s', [TagName, ToLine, _file]);
  Result := FromLIne;
  S := TStringList.Create;
  try
     S.LoadFromFile(_file);
     pos := Max(0, 1 + Min(Buffer.Count-1, ToLine));
     if pos >= Buffer.Count then
       Buffer.AddStrings(S)
     else
       for i := S.Count-1 downto 0 do
       begin
         Buffer.Insert(pos, S[i]);
       end;
  finally
    FreeAndNil(S);
  end;
end;

{ TWriteElement }

procedure TWriteElement.Perform(Editor: TEditor);
begin
  if (from <> '') or (_to <> '') then
    inherited Perform(Editor)
  else
    Perform(Editor.Buffer, 0, Editor.Buffer.Count-1);
end;

function TWriteElement.Perform(Buffer: TStrings; FromLine, ToLine: Integer): Integer;
var
  S :TStringList;
  i :Integer;
begin
  if append then
    Log(vlVerbose, '%s %d-%d >> %s', [TagName, FromLine, ToLine, _file])
  else
    Log(vlVerbose, '%s %d-%d %s', [TagName, FromLine, ToLine, _file]);

  Result := -1; // don't change the line
  S := TStringList.Create;
  try
     if append and FileExists(_file) then
       S.LoadFromFile(_file);
     for i := Max(0, FromLine) to Min(Buffer.Count-1, ToLine) do
       S.Append(Buffer[i]);
     S.SaveToFile(_file);
  finally
    FreeAndNil(S);
  end;
end;


{ TInsertElement }

constructor TInsertElement.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FText := TStringList.Create;
end;

destructor TInsertElement.Destroy;
begin
  FreeAndNil(FText);
  inherited Destroy;
end;

function TInsertElement.GetText: string;
begin
  Result := FText.Text;
end;

function TInsertElement.Perform(Buffer: TStrings; FromLine, ToLine: Integer): Integer;
var
   i :Integer;
begin
  ToLine := Max(0, TargetLine(FromLine, ToLine));
  Log(vlVerbose, '%s %d', [TagName, ToLine]);
  if ToLine > Buffer.Count then
  begin
    Buffer.AddStrings(FText);
    Result := Buffer.Count;
  end
  else
  begin
    for i := FText.Count-1 downto 0 do
      Buffer.Insert(ToLine, FText[i]);
    Result := ToLine + FText.Count;
  end;
end;

procedure TInsertElement.SetText(Value: string);
begin
  FText.Text := Value;
end;

function TInsertElement.TargetLine(FromLine, ToLine: Integer): Integer;
begin
  Result := ToLine;
end;

{ TAppendElement }

function TAppendElement.TargetLine(FromLine, ToLine: Integer): Integer;
begin
  Result := ToLine + 1;
end;


initialization
  RegisterTask(TEditTask);
  RegisterElements( TEditTask, [
                        TGotoElement,
                        TPrintElement,
                        TGlobalElement,
                        TSearchElement,
                        TDeleteElement,
                        TReadElement,
                        TWriteElement,
                        TInsertElement,
                        TAppendElement
                        ]);
  RegisterElements( TGlobalElement, [
                        TGotoElement,
                        TPrintElement,
                        TDeleteElement,
                        TReadElement,
                        TWriteElement,
                        TInsertElement,
                        TAppendElement
                        ]);
  RegisterElements( TSearchElement, [
                        TPrintElement,
                        TDeleteElement,
                        TReadElement,
                        TWriteElement,
                        TInsertElement,
                        TAppendElement
                        ]);
end.
