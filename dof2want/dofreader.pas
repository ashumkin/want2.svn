unit dofreader;
{
Unit        : dofreader

Description : provides an abstraction to read a .DOF file

Programmer  : mike

Date        : 09-Dec-2002
}

interface

uses
  classes,
  inifiles,
  sysUtils,
  const_dofReader,
  typ_dofReader;
  

  
type
    
  TDelphiDOFReader = class(Tobject)
  protected
    dof : TIniFile;
    fsectionValues : TStringList;
    fsectionNames : TStringList;
    fdofSection : TDofSection;
    procedure setDofSection(const Value: TDofSection); virtual;    
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(nameOfDOFFile : string);
    property DofSection : TDofSection read fdofSection write setDofSection;
    property sectionValues : TStringList read fsectionValues;
  end;

implementation


  
{ TDelphiDOFReader }

constructor TDelphiDOFReader.Create;
begin
  dof := nil;
  fsectionValues := TStringList.create;
  fsectionNames  := TStringList.create;
  inherited Create;
end;

destructor TDelphiDOFReader.Destroy;
begin
  fsectionNames.free;
  fsectionValues.free;
  dof.free;
  inherited Destroy;
end;

procedure TDelphiDOFReader.LoadFromFile(nameOfDOFFile: string);
const
  NotASection = -1;
var
  dofIsBad : boolean;
  section : TdofSection; 
begin
 dofIsBad := false;
 dof.free;
 try
   dof := TInifile.create(nameOfDofFile);
   dof.UpdateFile;
   dof.ReadSections(fsectionNames);
   if fsectionNames.count < 3 then 
     dofIsBad := true
   else
     begin
       for section := low(TdoFSection) to high(TdoFSection) do
         if fsectionNames.indexOf(NamesOfSections[section]) = NotASection then
           dofIsBad := true;
     end;   
 except
   dofIsBad := true;
 end;
 if dofIsBad then
   raise EBadDofFile.create(nameOfdofFile);    
end;

procedure TDelphiDOFReader.setDofSection(const Value: TDofSection);
begin
  fdofSection := Value;
  dof.ReadSectionValues(NamesOfSections[fdofSection],fsectionValues);
end;

end.
