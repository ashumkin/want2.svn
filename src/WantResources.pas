(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit WantBase;

interface

uses
  Classes,
  SysUtils,
  JclSysUtils,
  JclFileUtils,
  WildPaths;

const
  SwitchChars           = ['-', '/'];

  C_EOL = #13#10;

  BooleanToString : array[boolean] of string = ('false', 'true');

resourcestring
  WantUsageText   = 'For licensing info, use the -L switch'                      + C_EOL +
                                                                                    C_EOL +
                     'Usage:'                                                     + C_EOL +
                     '  Want.exe [options] [target]'                             + C_EOL +
                                                                                    C_EOL +
                     'Options:'                                                   + C_EOL +
                     '  -h, -H, -?          Displays this help text.'             + C_EOL +
                     '  -buildfile <file>   Specifies the build file. Default is' + C_EOL +
                     '                      build.xml'                            + C_EOL +
                     '  -Dname=value        Define property "name".'              + C_EOL +
                     '  -quiet              Be very quiet..'                      + C_EOL +
                     '  -verbose            Be extra verbose.'                    + C_EOL +
                     '  -debug              Print debugging information.'         + C_EOL +
                     '  -color              Output to console using color.'       + C_EOL;

  F_WantStartupFailed        = 'Want startup failed';

  F_WantError                = '!!! %s !!!';
  F_TaskError                 = '!!! %s !!!';
  F_TaskFailure               = '%s';

  F_BuildStartMsg             = 'buildfile: %s';
  F_BuildDoneMsg              = 'build complete';
  F_BuildFailedMsg            = 'BUILD FAILED';

  F_BuildFileNotFound         = 'Cannot find %s';
  F_BuildTargetUnhandledError = '%s: %s';
  F_BuildTargetNotFound       = 'target [%s] not found';

  F_TargetStartMsg            = '--> %s';

  F_ExpectedTagError          = 'expected <%s>';
  F_ParseError                = '(%d): %s';
  F_ParseAttributeError       = '(%d): Unknown attribute %s.%s';
  F_ParseChildError           = '(%d): Unknown element <%s><%s>';
  F_ParseChildTextError       = '(%d): Element <%s> does not accept text';

  F_WantClassNotFound        = 'Want class <%s> not found';
  F_DuplicateWantClass       = 'Duplicate Want tag <%s> in class <%s>';


procedure RaiseLastSystemError(Msg :string = '');

function ConvertToBoolean(const aValue: String): Boolean;

function  WantHeader: string;
function  License :string;
procedure Usage;
function  GetVersionString: string;

implementation

uses
  Windows;

procedure RaiseLastSystemError(Msg :string = '');
begin
  raise Exception.Create(SysErrorMessage(GetLastError) + Msg)
end;

function ConvertToBoolean(const aValue: String): Boolean;
var
  s: String;
begin
  s := LowerCase(Trim(aValue)) + ' ';

  case s[1] of
    'f': Result := False;
    'n': Result := False;
    '0': Result := False;
  else
    Result := True;
  end;
end;

function GetVersionString: string;
var
  AFileVer: TJclFileVersionInfo;
begin
  try
    AFileVer := TJclFileVersionInfo.Create(ParamStr(0));
    try
      Result := AFileVer.FileVersion;
    finally
      FreeAndNil(AFileVer);
    end;
  except
    Result := '?.?.?.?';
  end;
end;


function WantHeader: string;
begin
  Result :=
    'Want ' + GetVersionString + ' Build Management tool'              + C_EOL +
    'Copyright (c) 2001, Juancarlo Añez, Caracas, Venezuela.'          + C_EOL +
    'For complete licensing info, execute with -L switch';
end;

function License: string;
var
  FindHandle: THandle;
  ResHandle: THandle;
  ResPtr: Pointer;

  procedure RaiseError(ErrorTxt: string);
  begin
    raise Exception.Create('Internal error: ' + ErrorTxt + ' ' +
      '[WantBase.License]');
  end;
begin
  FindHandle := FindResource(HInstance, PChar('LICENSE'), 'TEXT');
  if FindHandle <> 0 then
  begin
    ResHandle := LoadResource(HInstance, FindHandle);
    try
      if ResHandle <> 0 then
      begin
        ResPtr := LockResource(ResHandle);
        try
          if ResPtr <> Nil then
            Result := PChar(ResPtr)
          else
            RaiseError('LockResource failed');
        finally
          UnlockResource(ResHandle);
        end;
      end
      else
        RaiseError('LoadResource failed');
   finally
     FreeResource(FindHandle);
   end;
  end
  else
    RaiseError('FindResource failed');
end;

procedure Usage;
begin
  Writeln(WantUsageText);
end;


end.
