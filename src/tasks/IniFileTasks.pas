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
  TIniTask = class(TTask)
  protected
    FFile :TPath;
  public
    procedure Init; override;

    procedure Execute; override;
  published
    property _file :TPath read FFile write FFile;
  end;

  TEntryElement = class(TScriptElement)
  protected
    FFileName :string;
    FSection  :string;
    FKey      :string;
  public
    procedure Init; override;
    procedure Perform; virtual;
  published
    property Name;
    property section :string read FSection write FSection;
    property key     :string read FKey     write FKey;
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

{ TIniTask }

procedure TIniTask.Execute;
var
  i :Integer;
begin
  for i := 0 to ChildCount-1 do
    if Children[i] is TEntryElement then
      TEntryElement(Children[i]).Perform;
end;

procedure TIniTask.Init;
begin
  inherited Init;
  RequireAttribute('file');
end;

{ TEntryElement }

procedure TEntryElement.Init;
begin
  inherited Init;
  RequireAttributes(['section', 'key']);

  Assert(Owner is TIniTask);
  Assert(Owner.Owner <> nil);

  FFileName := PathConcat(BasePath, (Owner as TIniTask)._file);
end;

procedure TEntryElement.Perform;
begin
  // by default, do nothing
end;

{ TReadElement }

procedure TReadElement.Init;
begin
  inherited Init;
  RequireAttribute('property');

  with TIniFile.Create(WildPaths.ToSystemPath(FFileName)) do
  try
    Owner.Owner.SetProperty(_property, ReadString(section, key, _default));
  finally
    Free;
  end;
end;

{ TWriteElement }

procedure TWriteElement.Init;
begin
  inherited Init;
end;

procedure TWriteElement.Perform;
begin
  inherited Init;

  with TIniFile.Create(WildPaths.ToSystemPath(FFileName)) do
  try
    WriteString(section, key, value);
  finally
    Free;
  end;
end;

initialization
  RegisterTask(TIniTask);
  RegisterElements(TIniTask, [TReadElement, TWriteElement]);
end.
