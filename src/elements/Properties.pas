(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

{
  Contributors:
    Dan Hughes <dan@multiedit.com>
}

unit Properties;

interface
uses
  { Delphi }
  Classes,
  IniFiles,

  {Local}
  WantClasses,
  WildPaths,
  JalPaths;

type
  TPropertyElement = class(TScriptElement)
  protected
    FName: string;
    FValue: string;
    FFileName: TPath;
    FSection: string;
    procedure ProcessFile;
  public
    procedure Init; override;
  published
    property name: string read FName  write FName;
    property value: string read FValue write FValue;
    property _file: TPath read FFileName  write FFileName;
    property section: string read FSection write FSection;
  end;

implementation

{ TPropertyElement }

procedure TPropertyElement.Init;
begin
  inherited Init;
  if Enabled then
  begin
    RequireAttributes(['name|file', 'value|file']);
//    RequireAttributes(['name|file']);

    Assert(Owner <> nil);
    if ( _file <> '' ) then
    begin
      ProcessFile;
    end
    else
    begin
      Owner.SetProperty(name, value);
    end;
  end;
end;

procedure TPropertyElement.ProcessFile;
var
  I: Integer;
  IniFile : TMemIniFile;
  PropList : TStringList;

  Name : string;
  Value : string;

begin
  PropList := TStringList.Create;
  try
    if (Section <> '') then
    begin
      IniFile := TMemIniFile.Create(WildPaths.ToSystemPath(_file));
      try
        if IniFile.SectionExists(Section) then
        begin
          IniFile.ReadSectionValues(Section, PropList);
        end;
      finally
        IniFile.Free;
      end;
    end
    else begin
      PropList.LoadFromFile(WildPaths.ToSystemPath(_file));
    end;
    for I := PropList.Count - 1 downto 0 do
    begin
      Name := PropList.Names[I];
      Value := PropList.Values[Name];
      if (Name <> '') and (Name[1] <> '#') then
      begin
        Owner.SetProperty(Name, Value);
      end;
    end;
  finally
    PropList.Free;
  end;
end;


initialization
  RegisterElement(TPropertyElement);
end.

