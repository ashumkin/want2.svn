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

unit JalSetup;

interface
uses
  Windows,
  SysUtils,

  JclRegistry,

  JalStrings;

const
  SYSTEM_ENV = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';

procedure AddSystemSearchPath(const path :string; const pattern :string = '');
procedure AddUserSearchPath(const path :string; const pattern :string = '');

implementation

function ChangePath(PathSet, NewPath, Pattern :string) :string;
var
  Paths :TStringArray;
  i     :Integer;
begin
  Paths := StringToArray(PathSet, ';');

  if Pattern <> '' then
  begin
    Pattern := UpperCase(Pattern);
    for i := 0 to High(Paths) do
      if (Trim(Paths[i]) = '') or (Pos(Pattern, UpperCase(Paths[i])) <> 0) then
        Paths[i] := '';
    Paths := Pack(Paths);
  end;

  if NewPath <> '' then
  begin
    if (Pos(' ', NewPath) <> 0) and (NewPath[1] <> '"') then
       NewPath := '"' + NewPath + '"';

    StringArrayAppend(Paths, NewPath);
  end;

  Result := ArrayToString(Paths, ';');
end;

procedure ChangeAutoExecPath(NewPath, Pattern :string);
const
  autoexec_bat = 'C:\AUTOEXEC.BAT';
var
  autoexec :TStringArray;
  i        :Integer;
begin
  autoexec := nil;
  if FileExists(autoexec_bat) then
  begin
    autoexec := FileToStringArray(autoexec_bat);
    StringArrayToFile('C:\AUTOEXEC.TUS', autoexec);

    if Pattern <> '' then
    begin
      Pattern := UpperCase(Pattern);
      for i := 0 to High(autoexec) do
      begin
        if (Pos('PATH', autoexec[i]) = 1)
        and (Pos(Pattern, UpperCase(autoexec[i])) <> 0 )
        then
            autoexec[i] := '';
      end;
      autoexec := Pack(autoexec);
    end;

    if NewPath <> '' then
      StringArrayAppend(autoexec, Format('PATH %%PATH%%;%s', [NewPath]));

    StringArrayToFile(autoexec_bat, autoexec);
  end;
end;

procedure ChangeEnvironmentPath(const root :HKEY; key, path :string; pattern :string);
var
  CurrentValue :string;
  NewValue     :string;
begin
  CurrentValue := RegReadString(root, key, 'Path');
  NewValue := ChangePath(CurrentValue, path, pattern);
  if NewValue <> CurrentValue then
  begin
    RegWriteString(root, key, 'Path', NewValue);
  end;
end;

procedure AddSystemSearchPath(const path :string; const pattern :string = '');
begin
  ChangeEnvironmentPath(HKEY_LOCAL_MACHINE, SYSTEM_ENV, path, pattern);
  ChangeAutoexecPath(path, pattern);
end;

procedure AddUserSearchPath(const path :string; const pattern :string = '');
begin
  ChangeEnvironmentPath(HKEY_CURRENT_USER, 'Environment', path, pattern);
end;

end.
