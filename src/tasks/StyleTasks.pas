unit StyleTasks;

{
@brief Apliying XSL transformations to xml files

@autor Juanco Añez
@autor Ignacio J. Ortega
}
{#(@)$Id$}

interface

uses
  SysUtils,
  Classes,
  Math,

  JclStrings,
  Filetasks,
  WantUtils,
  WantClasses,
  PatternSets,
  WildPaths,
  MSXMLEngineImpl;

type
  TParamElement          = class;
  TOutputPropertyElement = class;

  TStyleTask = class(TFileSetTask)
  private
    p, o: array of string;
  protected
    FIn: TPath;
    FOut: TPath;
    FBasedir: TPath;
    FDestdir: TPath;
    FExtension: string;
    FStyle: TPath;
    FParams: TList;
    FOutputProperties: TList;
    FSingleFile: boolean;
  public
    constructor Create(Owner: TScriptElement); override;
    destructor Destroy; override;
    procedure DoFileset(Fileset: TFileSet); override;
    procedure Init; override;
    procedure Execute; override;
  published
    property _in: TPath read Fin write Fin;
    property _out: TPath read Fout write Fout;
    property basedir: TPath read Fbasedir write Fbasedir;
    property destdir: TPath read Fdestdir write Fdestdir;
    property style: TPath read Fstyle write Fstyle;
    property extension: string read Fextension write Fextension;
    function CreateOutputProperty: TOutputPropertyElement;
    function CreateParam: TParamElement;
  end;

  TParamElement = class(TScriptElement)
  protected
    FName: string;
    FExpression: string;
  published
    property Name: string read FName write FName;
    property expression: string read FExpression write FExpression;
  end;

  TOutputPropertyElement = class(TScriptElement)
  protected
    FName: string;
    FValue: string;
  published
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;


implementation

{ TStyleTask }

constructor TStyleTask.Create(Owner: TScriptElement);
begin
  inherited Create(Owner);
  FParams := TList.Create;
  FOutputProperties := TList.Create;
end;

function TStyleTask.CreateOutputProperty: TOutputPropertyElement;
begin
  Result := TOutputPropertyElement.Create(Self);
  FOutputProperties.Add(Result);
end;

function TStyleTask.CreateParam: TParamElement;
begin
  Result := TParamElement.Create(Self);
  FParams.Add(Result);
end;

destructor TStyleTask.Destroy;
begin
  FParams.Free;
  FOutputProperties.Free;
  inherited;
end;

procedure TStyleTask.DoFileset(Fileset: TFileSet);
var
  i: integer;
  FromPaths, ToPaths: TPaths;
begin
  inherited;
  Log(SysUtils.Format('Transforming into %s', [destdir]));
  Log(SysUtils.Format('Using %s', [style]));
  FromPaths := Fileset.Paths;
  ToPaths := Fileset.MovePaths(destdir);
  for i := 0 to High(FromPaths) do
  begin
    ToPaths[i] := WildPaths.ChangeExtension(ToPaths[i], FExtension);
    Log(SysUtils.Format('Processing %s to %s ', [FromPaths[i], ToPaths[i]]));
    AboutToScratchPath(ToPaths[i]);
    XSLEngine.transform(FromPaths[i], ToPaths[i], style, p, o);
  end;
end;

procedure TStyleTask.Execute;
var
  i: integer;
begin
  SetLength(p, FOutputProperties.Count * 2);
  for i := 0 to FOutputProperties.Count - 1 do
    with TOutputPropertyElement(FOutputProperties[i]) do 
    begin
      p[i] := Name;
      p[i + 1] := Value;
    end;
  SetLength(p, FParams.Count * 2);
  for i := 0 to FParams.Count - 1 do
    with TParamElement(FParams[i]) do 
    begin
      p[i] := Name;
      p[i + 1] := expression;
    end;
  if Fsinglefile then 
  begin
    Log(SysUtils.Format('Processing %s to %s ', [Fin, Fout]));
    Log(SysUtils.Format('Using %s', [style]));
    AboutToScratchPath(Fout);
    XSLEngine.transform(Fin, Fout, style, p, o);
  end 
  else
    inherited Execute;
end;

procedure TStyleTask.Init;
begin
  inherited Init;
  Fsinglefile := (Fout <> '') and (Fin <> '');
  if (not Fsinglefile) then
    RequireAttribute('destdir');
  RequireAttribute('style');
end;

initialization
  RegisterTask(TStyleTask);
end.
