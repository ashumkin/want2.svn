{ $Id$ }
{
--------------------------------------------------------------------------------
Copyright (c) 2001, Dante Authors -- See authors.txt for complete list
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. The name Dante, the names of the authors in authors.txt and the names of
other contributors to this software may not be used to endorse or promote
products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------
Original Author: Juancarlo Añez
Contributors   : 
}
unit EchoTasks;

interface
uses
  DanteClasses,
  WildPaths,
  PatternSets,

  SysUtils,
  Classes;

type
  TEchoTask = class(TTask)
  protected
    FMessage :string;
    FText    :string;
    FFile    :string;
    FAppend  :boolean;

  public
    procedure Execute; override;
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
begin
  if _file = '' then
    Log(_message + _text)
  else
  begin
    Log(SysUtils.Format('echo to "%s"', [_file]));
    AboutToScratchPath(_file);

    System.Assign(EchoFile, ToSystemPath(_file));
    if append and FileExists(ToSystemPath(_file)) then
      System.Append(EchoFile)
    else
      System.Rewrite(EchoFile);
    try
      Writeln( EchoFile, _message + _text);
    finally
      System.Close(EchoFile);
    end;
  end;
end;


initialization
 RegisterTask(TEchoTask);
end.
