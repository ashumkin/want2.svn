unit Attributes;

interface
uses
  SysUtils,
  WildPaths,
  WantClasses;

type
  TCustomAttributeElement = class(TScriptElement)
  protected
    function  ValueName :string; virtual;
  public
    procedure Init; override;
    function  SetAttribute(Name, Value :string) :boolean; override;
  end;

  TAttributeElement = class(TCustomAttributeElement)
  protected
    FValue :string;
  published
    property value :string read FValue write FValue;
  end;

  TBooleanAttributeElement = class(TCustomAttributeElement)
  protected
    FVAlue :boolean;
  published
    property value :boolean read FValue write FValue;
  end;

  TPathAttributeElement = class(TCustomAttributeElement)
  protected
    FPath :TPath;
    function  ValueName :string; override;
  published
    property path :TPath read FPath write FPath;
  end;



implementation

{ TCustomAttributeElement }

procedure TCustomAttributeElement.Init;
begin
  inherited Init;
  RequireAttribute(ValueName);
end;

function TCustomAttributeElement.ValueName: string;
begin
  Result := 'value';
end;

function TCustomAttributeElement.SetAttribute(Name, Value: string) :boolean;
begin
  Result := inherited SetAttribute(Name, Value);
  if Name = ValueName then
  begin
    Result := Owner.SetAttribute(Self.TagName, Value);
    Log(vlVerbose, '%s=%s', [Self.TagName, Value]);
  end;
end;

function TPathAttributeElement.ValueName: string;
begin
  Result := 'path';
end;

initialization
  RegisterElements([ TAttributeElement,
                     TPathAttributeElement,
                     TBooleanAttributeElement
                     ]);
end.
