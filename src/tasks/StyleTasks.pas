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
  WildPaths;

type
  IStyleTaskXSLEngine = interface(IUnknown)
    procedure transform(_in, _out, style: string;
      Params,OutputProperties: array of string);
  end;

  TParamElement = class;
  TOutputPropertyElement = class;

  TStyleTask = class(TFileSetTask)
  private
    p,o : Array of String;
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
    procedure execute ; override;
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
    FName       : string;
    FExpression : string;
  published
    property name: string read FName write FName;
    property expression: string read FExpression write FExpression;
  end;
  TOutputPropertyElement = class(TScriptElement)
  protected
    FName       : string;
    FValue      : string;
  published
    property name: string read FName write FName;
    property value: string read FValue write FValue;
  end;
var
    XSLEngine : IStyleTaskXSLEngine;

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
  FromPaths,ToPaths:TPaths;
begin
  inherited;
  Log(SysUtils.Format('Transforming into %s', [destdir]));
  FromPaths := Fileset.Paths;
  ToPaths   := Fileset.MovePaths(destdir);
  for i := 0 to High(FromPaths) do
  begin
    ToPaths[i]:=WildPaths.ChangeExtension(ToPaths[i],FExtension);
    Log(SysUtils.Format('Processing %s to %s ', [FromPaths[i],ToPaths[i]]));
    AboutToScratchPath(ToPaths[i]);
    XSLEngine.transform(FromPaths[i],ToPaths[i],style,p,o);
  end;
end;

procedure TStyleTask.execute;
var
  i : integer;
begin
  SetLength(p,FOutputProperties.count*2);
  for i := 0 to FOutputProperties.count-1 do
  with TOutputPropertyElement(FOutputProperties[i]) do begin
      p[i]:=name;
      p[i+1]:=value;
  end;
  SetLength(p,FParams.count*2);
  for i := 0 to FParams.count-1 do
  with TParamElement(FParams[i]) do begin
      p[i]:=name;
      p[i+1]:=expression;
  end;
  if Fsinglefile then begin
    Log(SysUtils.Format('Processing %s to %s ', [Fin,Fout]));
    AboutToScratchPath(Fout);
    XSLEngine.transform(Fin,Fout,style,p,o);
  end else
    inherited execute ;
end;

procedure TStyleTask.Init;
begin
  inherited Init;
  try
    RequireAttributes(['in', 'out']);
    Fsinglefile := True;
  except
    Fsinglefile := False;
  end;
  if (not Fsinglefile) then
    RequireAttribute('destdir');
  RequireAttribute('style');
  if Not Assigned(XSLEngine) then
    WantError('XSL Engine not initialized correctly');
end;

initialization
  RegisterTask(TStyleTask);

end.
