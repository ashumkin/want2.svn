{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca ®                                 }
{    ~                                         }
{  Copyright © 1995-2002 Juancarlo Añez        }
{  http://www.suigeneris.org/juanca            }
{  All rights reserved.                        }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{#(@)$Id$}

unit JALFiles;

interface
uses
  Windows,
  SysUtils;

type
   ECouldNotRenameToBackup = class(EInOutError);

function FileIsWriteable(const FName :string):boolean;
function RenameFileToBackup(const FileName :string):string;

procedure LogToFile(const FileName :string; const Msg :string);

implementation

function FileIsWriteable(const FName :string):boolean;
begin
  if FileExists(FName) then
     result := not FileIsReadOnly(FName)
  else
     result := not FileIsReadOnly(ExtractFileDir(FName));
end;

function RenameFileToBackup(const FileName :string):string;
var
  Path,
  Name,
  Ext   :string;
  Tag   :string;
  n     :Integer;

begin
  Path := ExtractFilePath(FileName) + 'backups';
  Name := ChangeFileExt(ExtractFileName(FileName), '');
  Ext  := ExtractFileExt(FileName);

  CreateDir(Path);

  n := 1;
  repeat
    Tag := FormatDateTime('yyyy-mm-dd', Now);
    Result := Format('%s\%s-%s-%3.3d%s', [Path, Name, Tag, n, Ext]);
    Inc(n);
  until not FileExists(Result);

  if not MoveFile(PChar(FileName), PChar(Result)) then
  begin
    // try copy and delete (MoveFileEx doesn't work on Win98 and relatives)
    if CopyFile(PChar(FileName), PChar(Result), TRUE) then
    begin
      if not DeleteFile(PChar(FileName)) then
        raise ECouldNotRenameToBackup.CreateFmt('Delete old file %s', [FileName]);
    end
    else
      raise ECouldNotRenameToBackup.CreateFmt('Could move old version of %s to %s', [FileName, Result]);
  end;
end;

procedure LogToFile(const FileName :string; const Msg :string);
var
  T :Text;
begin
  Assign(T, FileName);
  if not FileExists(FileName) then
    Rewrite(T)
  else
    Append(T);
  try
    Writeln(T, Msg);
  finally
    Close(T);
  end;
end;


end.
