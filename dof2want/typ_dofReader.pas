unit typ_dofReader;
{
Unit        : typ_dofReader

Description : 

Programmer  : mike

Date        : 13-Dec-2002
}

interface

uses
 sysUtils;
 
type
  EBadDOFFile = class(Exception);
  
  TDofSection = (tsCompiler,tsLinker,tsDirectories);
implementation

end.
