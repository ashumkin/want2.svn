(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit ChildProcesses;

interface
uses
  Windows,
  SysUtils,
  Math,
  
  WIN32;

const
  rcs_id :string = '#(@)$Id$';

type
  TChildProcess = class
  protected
    FLine       :String;

    function  __Read(Count :Integer = 80)   :string; virtual; abstract;
  public
    constructor Create;  virtual;

    procedure Run(CmdLine: string);  virtual; abstract;
    function  ExitCode :Cardinal;    virtual; abstract;
    function  EOF  :boolean;         virtual; 

    function Read(Count :Integer = 80) :string; virtual;
    function ReadLn :string;                    virtual;
  end;

  TChildProcessClass = class of TChildProcess;

var
  ChildProcessClass :TChildProcessClass = nil;

implementation

{ TChildProcess }

constructor TChildProcess.Create;
begin
  inherited Create;
end;


function TChildProcess.EOF: boolean;
begin
  Result := (FLine = '');
end;

function TChildProcess.Read(Count :Integer): string;
begin
  if FLine <> '' then
  begin
    Result := FLine;
    FLIne  := '';
  end
  else
    Result := __Read(Count);
end;

function TChildProcess.ReadLn: string;
var
  c             :Char;
  p             :Integer;
begin
  Result := '';
  p := 0;
  while not EOF do
  begin
    FLine := FLine + __Read;
    while p < Length(FLine) do
    begin
      Inc(p);
      c := FLine[p];
      if c in [#10,#13] then
      begin
        Result := Copy(FLine, 1, p-1);
        if (c = #13) and (p < Length(Fline)) and (FLine[p+1] = #10) then
          Inc(p);
        Delete(FLine, 1, p);
        Exit;
      end;
    end;
  end;
end;

end.
