{--------------------------------------------------------------------------}
{ Copyright (c) 2001, Dante Authors -- See authors.txt for complete list   }
{ All rights reserved.                                                     }
{                                                                          }
{ Redistribution and use in source and binary forms, with or without       }
{ modification, are permitted provided that the following conditions       }
{ are met:                                                                 }
{                                                                          }
{ 1. Redistributions of source code must retain the above copyright        }
{    notice, this list of conditions and the following disclaimer.         }
{                                                                          }
{ 2. Redistributions in binary form must reproduce the above copyright     }
{    notice, this list of conditions and the following disclaimer in       }
{    the documentation and/or other materials provided with the            }
{    distribution.                                                         }
{                                                                          }
{ 3. The name Dante, the names of the authors in authors.txt and the       }
{    names of other contributors to this software may not be used to       }
{    endorse or promote products derived from this software without        }
{    specific prior written permission.                                    }
{                                                                          }
{  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     }
{  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     }
{  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       }
{  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          }
{  COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   }
{  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    }
{  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS   }
{  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND  }
{  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR   }
{  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE  }
{  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.}
{                                                                          }
{--------------------------------------------------------------------------}
{ The JCL (JEDI Code Library) used in Dante is governed by                 }
{ the terms of the MPL (Mozilla Public License) found at                   }
{ http://www.mozilla.org/MPL/MPL-1.1.html.                                 }
{                                                                          }
{ The source code for JCL itself can be found on JEDI Web site:            }
{ http://delphi-jedi.org/Jedi:CODELIBJCL.                                  }
{--------------------------------------------------------------------------}
{ (based on BSD Open Source License)                                       }
{--------------------------------------------------------------------------}

unit DanteBase;

interface

uses
  Classes,
  SysUtils;

const
  Default_BuildFileName = 'build.xml';
  SwitchChars           = ['-', '/'];

  C_EOL = #13 + #10;

type
  EDanteException  = class(Exception);
  EDanteError      = class(EDanteException);
  ETargetException = class(EDanteException);
  ETaskException   = class(EDanteException);

  ENoDefaultTargetError     = class(ETargetException);
  ETargetNotFoundException  = class(ETargetException);
  ECircularTargetDependency = class(ETargetException);

  ETaskError   = class(ETaskException);
  ETaskFailure = class(ETaskException);

  EDanteParseException = class(EDanteException);

  TDanteBase = class
    private
      FParent: TDanteBase;
      FName:   String;

    protected
      class function TagName: String; virtual;

      function    SetAttribute(const aName: String; const aValue: String): Boolean; virtual;

    public
      constructor Create(const aParent: TDanteBase); virtual;

      property    Name:   String     read FName;
      property    Parent: TDanteBase read FParent;
  end;

  TDanteList = class(TDanteBase)
    protected
      FList:  TStringList;

      class function TagName: String; override;

      function    SetAttribute(const aName: String; const aValue: String): Boolean; override;

      function    GetCount: Integer;

    public
      constructor Create(const aParent: TDanteBase); override;
      destructor  Destroy;                           override;

      procedure   Clear; virtual;

      procedure   Add(aElement: TDanteBase);            overload;
      function    Get(const aName: String): TDanteBase; overload;

      property    List:  TStringList read FList;
      property    Count: Integer     read GetCount;
  end;

{$IFNDEF VER120}
resourcestring
  DanteHeaderText  = 'Dante v0.0.0 Build 0. Build Management tool for Delphi' + C_EOL;

  DanteUsageText   = 'For licensing info, use the -L switch'                      + C_EOL +
                                                                                    C_EOL +
                     'Usage:'                                                     + C_EOL +
                     '  dante.exe [options] [target]'                             + C_EOL +
                                                                                    C_EOL +
                     'Options:'                                                   + C_EOL +
                     '  -h, -H, -?          Displays this help text.'             + C_EOL +
                     '  -buildfile [file]   Specifies the build file. Default is' + C_EOL +
                     '                      build.xml'                            + C_EOL +
                     '  -verbose            Be extra verbose.'                    + C_EOL +
                     '  -debug              Print debugging information.'         + C_EOL;

  DanteLicenseText1 = '--------------------------------------------------------------------------' + C_EOL +
                      ' Copyright (c) 2001, Dante Authors -- See authors.txt for complete list   ' + C_EOL +
                      ' All rights reserved.                                                     ' + C_EOL +
                      '                                                                          ' + C_EOL +
                      ' Redistribution and use in source and binary forms, with or without       ' + C_EOL +
                      ' modification, are permitted provided that the following conditions       ' + C_EOL +
                      ' are met:                                                                 ' + C_EOL +
                      '                                                                          ';
  DanteLicenseText2 = ' 1. Redistributions of source code must retain the above copyright        ' + C_EOL +
                      '    notice, this list of conditions and the following disclaimer.         ' + C_EOL +
                      '                                                                          ' + C_EOL +
                      ' 2. Redistributions in binary form must reproduce the above copyright     ' + C_EOL +
                      '    notice, this list of conditions and the following disclaimer in       ' + C_EOL +
                      '    the documentation and/or other materials provided with the            ' + C_EOL +
                      '    distribution.                                                         ' + C_EOL +
                      '                                                                          ' + C_EOL +
                      ' 3. The name Dante, the names of the authors in authors.txt and the       ' + C_EOL +
                      '    names of other contributors to this software may not be used to       ' + C_EOL +
                      '    endorse or promote products derived from this software without        ' + C_EOL +
                      '    specific prior written permission.                                    ' + C_EOL +
                      '                                                                          ';
  DanteLicenseText3 = '  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     ' + C_EOL +
                      '  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     ' + C_EOL +
                      '  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       ' + C_EOL +
                      '  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          ' + C_EOL +
                      '  COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   ' + C_EOL +
                      '  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    ' + C_EOL +
                      '  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS   ' + C_EOL +
                      '  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND  ' + C_EOL +
                      '  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR   ' + C_EOL +
                      '  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE  ' + C_EOL +
                      '  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.';
  DanteLicenseText4 = '                                                                          ' + C_EOL +
                      '--------------------------------------------------------------------------' + C_EOL +
                      ' The JCL (JEDI Code Library) used in Dante is governed by                 ' + C_EOL +
                      ' the terms of the MPL (Mozilla Public License) found at                   ' + C_EOL +
                      ' http://www.mozilla.org/MPL/MPL-1.1.html.                                 ' + C_EOL +
                      '                                                                          ' + C_EOL +
                      ' The source code for JCL itself can be found on JEDI Web site:            ' + C_EOL +
                      ' http://delphi-jedi.org/Jedi:CODELIBJCL.                                  ' + C_EOL +
                      '--------------------------------------------------------------------------' + C_EOL +
                      ' (based on BSD Open Source License)                                       ' + C_EOL +
                      '--------------------------------------------------------------------------';

  F_DanteStartupFailed        = 'Dante startup failed';

  F_DanteError                = '!!! %s !!!';
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

  F_DanteClassNotFound        = 'Dante class <%s> not found';
  F_DuplicateDanteClass       = 'Duplicate Dante tag <%s> in class <%s>';

{$ENDIF}

procedure RaiseLastSystemError(Msg :string = '');

function ConvertToBoolean(const aValue: String): Boolean;

implementation

uses
  Windows;

procedure RaiseLastSystemError(Msg :string = '');
begin
  raise ETaskError.Create(SysErrorMessage(GetLastError) + Msg)
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


  { TDanteBase }

constructor TDanteBase.Create(const aParent: TDanteBase);
begin
  FParent := aParent;
  FName   := '';
end;

function TDanteBase.SetAttribute(const aName, aValue: String): Boolean;
begin
  if CompareText(aName, 'name') = 0 then
    begin
      FName  := aValue;
      Result := True;
    end
  else
    Result := False;
end;

class function TDanteBase.TagName: String;
begin
  Result := 'DanteBase';
end;

  { TDanteList }

constructor TDanteList.Create(const aParent: TDanteBase);
begin
  inherited Create(aParent);

  FList          := TStringList.Create;
  FList.Capacity := 100;
end;

destructor TDanteList.Destroy; { override }
begin
  Clear;

  FList.Free;
  FList := nil;

  inherited Destroy;
end;

class function TDanteList.TagName: String;
begin
  Result := 'DanteList';
end;

function TDanteList.SetAttribute(const aName, aValue: String): Boolean;
begin
  Result := inherited SetAttribute(aName, aValue);
end;

procedure TDanteList.Clear;
var
  i: Integer;
begin
  for i := 0 to Pred(FList.Count) do
    TDanteBase(FList.Objects[i]).Free;

  FList.Clear;
end;

procedure TDanteList.Add(aElement: TDanteBase);
begin
  FList.AddObject(aElement.Name, aElement);
end;

function TDanteList.Get(const aName: String): TDanteBase;
var
  i: Integer;
  o: TDanteBase;
begin
  Result := nil;

  if Length(aName) > 0 then
    begin
      i := 0;

      while i < FList.Count do
        begin
          o := TDanteBase(FList.Objects[i]);

          if o.Name = aName then
            begin
              Result := o;
              i      := FList.Count
            end
          else
            Inc(i);
        end;
    end;
end;

function TDanteList.GetCount: Integer;
begin
  Result := FList.Count;
end;

end.
