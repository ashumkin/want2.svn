(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit WIN32;

interface
uses
  Windows,
  SysUtils;

const
  rcs_id :string = '#(@)$Id$';

procedure RaiseLastSystemError(Msg :string = '');

implementation

procedure RaiseLastSystemError(Msg :string = '');
begin
  raise Exception.Create(SysErrorMessage(GetLastError) + Msg)
end;

end.







