unit Attributes;

interface
uses
  SysUtils,
  WildPaths,
  WantClasses;

type
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
