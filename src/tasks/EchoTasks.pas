(*******************************************************************
*  WANT - A build management tool.                                 *
*  Copyright (c) 2001 Juancarlo Añez, Caracas, Venezuela.          *
*  All rights reserved.                                            *
*                                                                  *
*******************************************************************)

{ $Id$ }

unit EchoTasks;

interface
uses
  SysUtils,
  Classes,
  Math,

  JclStrings,

  WantClasses,
  WildPaths;

type
  TEchoTask = class(TTask)
  protected
    FMessage :string;
    FText    :string;
    FFile    :string;
    FAppend  :boolean;

  public
    procedure Execute; override;

    function FormatText :string;
  published
    property _message :string  read FMessage write FMessage;
    property _text    :string  read FText    write FText;
    property _file    :string  read FFile    write FFile;
    property append   :boolean read FAppend  write FAppend;
  end;

implementation

{ TEchoTask }

procedure TEchoTask.Execute;
var
  EchoFile: System.Text;
  msg:      string;
begin
  msg := _message + FormatText;
  if _file = '' then
    Log(msg)
  else
  begin
    Log(SysUtils.Format('echo to "%s"', [ToRelativePath(_file)]));
    AboutToScratchPath(_file);

    System.Assign(EchoFile, ToSystemPath(_file));
    if append and FileExists(ToSystemPath(_file)) then
      System.Append(EchoFile)
    else
      System.Rewrite(EchoFile);
    try
      Writeln( EchoFile, msg);
    finally
      System.Close(EchoFile);
    end;
  end;
end;


function TEchoTask.FormatText: string;
var
  S    :TStrings;
  Lead :Integer;
  i    :Integer;
  p    :Integer;
begin
  S := TStringList.Create;
  try
    S.Text := _text;
    Lead := MaxInt;

    while (S.Count > 0) and (S[0] = '') do
      S.Delete(0);

    // find first non blank column
    for i := 0 to S.Count-1 do
    begin
      if Length(Trim(S[i])) = 0 then
        continue;
      Lead := Min(Lead, Length(S[i]));
      for p := 1 to Lead do
      begin
        if not (S[i][p] in [' ',#9]) then
        begin
          Lead := p;
          break;
        end;
      end;
      if Lead <= 0 then
        break;
    end;

    if Lead > 0 then
    begin
      // remove leading spaces
      for i := 0 to S.Count-1 do
        S[i] := Copy(S[i], Lead, Length(S[i]));
    end;

    Result := S.Text;
  finally
    FreeAndNil(S);
  end;
end;

initialization
 RegisterTask(TEchoTask);
end.
