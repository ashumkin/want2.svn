(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit IniFileTasks;

interface
uses
  IniFiles,
  WildPaths,
  WantClasses;

type
  TIniElement = class(TScriptElement)
  protected
    FFile     :TPath;
    FProperty :string;
    FSection  :string;
    FName     :string;
    FDefault  :string;
  public
    procedure Init; override;
  published
    property name;
    property _property :string read FProperty write FProperty;
    property _file :TPath read FFile write FFile;
    property section :string read FSection write FSection;
    property _default  :string read FDefault  write FDefault;
  end;

  TIniFileTask = class(TTask)
  protected
    FFile :TPath;
  public
    procedure Init; override;

    procedure Execute; override;
  published
    property _file :TPath read FFile write FFile;
  end;

  TSectionElement = class(TScriptElement)
  public
    procedure Init; override;

    procedure Perform;
  published
    property Name;
  end;

  TEntryElement = class(TScriptElement)
  protected
    FFileName :string;
    FSection  :string;
  public
    procedure Init; override;
    procedure Perform; virtual;
  published
    property Name;
  end;

  TReadElement = class(TEntryElement)
  protected
    FProperty :string;
    FDefault  :string;
  public
    procedure Init; override;
  published
    property _property :string read FProperty write FProperty;
    property _default  :string read FDefault  write FDefault;
  end;


  TWriteElement = class(TEntryElement)
  protected
    FValue :string;
  public
    procedure Init; override;

    procedure Perform; override;
  published
    property value :string read FValue write FValue;
  end;


implementation

{ TIniElement }

procedure TIniElement.Init;
var
  Ini     :TIniFile;
begin
  inherited Init;
  RequireAttributes(['property', 'section', 'name']);

  Ini := TIniFile.Create(WildPaths.ToSystemPath(PathConcat(BasePath, _file)));
  try
    Owner.SetProperty(_property, Ini.ReadString(section, Self.Name, _default));
  finally
    Ini.Free;
  end;
end;

{ TIniFileTask }

procedure TIniFileTask.Execute;
var
  i :Integer;
begin
  for i := 0 to ChildCount-1 do
    if Children[i] is TSectionElement then
      TSectionElement(Children[i]).Perform;
end;

procedure TIniFileTask.Init;
begin
  inherited Init;
  RequireAttribute('file');
end;

{ TSectionElement }

procedure TSectionElement.Init;
begin
  inherited Init;
  RequireAttribute('name');
end;

procedure TSectionElement.Perform;
var
  i :Integer;
begin
  for i := 0 to ChildCount-1 do
    if Children[i] is TEntryElement then
      TEntryElement(Children[i]).Perform;
end;

{ TEntryElement }

procedure TEntryElement.Init;
begin
  inherited Init;
  RequireAttribute('name');

  Assert(Owner is TSectionElement);
  Assert(Owner.Owner is TIniFileTask);
  Assert(Owner.Owner.Owner <> nil);

  FFileName := PathConcat(BasePath, (Owner.Owner as TIniFileTask)._file);
  FSection  := (Owner as TSectionElement).Name;
end;

procedure TEntryElement.Perform;
begin
  // by default, do nothing
end;

{ TReadElement }

procedure TReadElement.Init;
var
  Ini     :TIniFile;
begin
  inherited Init;
  RequireAttribute('property');

  Ini := TIniFile.Create(WildPaths.ToSystemPath(FFileName));
  try
    Owner.Owner.Owner.SetProperty(_property, Ini.ReadString(FSection, Self.Name, _default));
  finally
    Ini.Free;
  end;
end;

{ TWriteElement }

procedure TWriteElement.Init;
begin
  inherited Init;
end;

procedure TWriteElement.Perform;
var
  Ini     :TIniFile;
begin
  inherited Init;

  Ini := TIniFile.Create(WildPaths.ToSystemPath(FFileName));
  try
    Ini.WriteString(FSection, Self.Name, value);
  finally
    Ini.Free;
  end;
end;

initialization
  RegisterElement(TIniElement); // note that it can be used anywhire for reading
  RegisterTask(TIniFileTask);
  RegisterElements(TIniFileTask, [TSectionElement]);
  RegisterElements(TSectionElement, [TReadElement, TWriteElement]);
end.
