unit const_dofReader;

interface

uses
 typ_dofReader;
 
const
  NamesOfSections : array[TDofSection] of string =
  (
   'COMPILER',
   'LINKER',
   'DIRECTORIES'
  );

implementation

end.
 