unit dofFlagExtractor;
{
Unit        : dofFlagExtractor

Description : interface for pulling .dof data from a .dof reader

Programmer  : mike

Date        : 11-Dec-2002
}

interface

uses
 classes,
 dofReader;

type
  TDOFFlagExtractor = class(Tobject)
  protected
    fvalues : TStringList;
    freader : TDelphiDOFReader;
  public
    constructor Create(reader : TDelphiDOFReader);
    destructor Destroy; override;
    procedure ExtractValues; virtual; abstract;
    property values : TStringList read fvalues;
  end;
  
implementation

{ TDOFFlagExtractor }

constructor TDOFFlagExtractor.Create(reader: TDelphiDOFReader);
begin
  inherited Create;
  freader := reader;
  fvalues := TStringList.Create;
end;

destructor TDOFFlagExtractor.Destroy;
begin
  fvalues.free;
  inherited;
end;

end.
