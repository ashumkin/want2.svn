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
unit LoggerTask;
interface
uses
  SysUtils,
  Classes,
  DanteClasses;

type
  TLogFormat = (brief, normal, detailed);

  TInfoElement = class(TDanteElement)
  protected
    FCode :string;
    FText :string;
  published
    property code :string  read FCode write FCode;
    property text :string  read FText write FText;
  end;

  TLoggerTask = class(TTask)
  protected
    FFile   :string;
    FFormat :TLogFormat;
    FInfos  :TList;
  public
    constructor Create(Owner :TDanteElement); override;
    destructor Destroy; override;

    class function TagName :string; override;

    procedure Init; override;
    procedure Execute;  override;
  published
    function CreateInfo :TInfoElement;

    property _file  :string     read FFile   write FFile;
    property format :TLogFormat read FFormat write FFormat;
  end;

implementation

class function TLoggerTask.TagName :string;
begin
  Result := 'log';
end;

constructor TLoggerTask.Create(Owner: TDanteElement);
begin
  inherited Create(Owner);
end;

destructor TLoggerTask.Destroy;
begin
  // no need to free the TInfoElements themselves
  FreeAndNil(FInfos);
  inherited Destroy;
end;

function TLoggerTask.CreateInfo :TInfoElement;
begin
  Result := TInfoElement.Create(Self);
  if FInfos = nil then
    FInfos := TList.Create;
  FInfos.Add(Result);
end;

procedure TLoggerTask.Init;
begin
  inherited Init;
  RequireAttribute('file');
end;

procedure TLoggerTask.Execute;
var
  LogFile: System.Text;
  i:   Integer;
begin
  Log(SysUtils.Format('writing log info to "%s"', [_file]));
  AboutToScratchPath(_file);
  System.Assign(LogFile, ToSystemPath(_file));
  try
    if FileExists(ToSystemPath(_file)) then
      System.Append(LogFile)
    else
      System.Rewrite(LogFile);
    try
      for i := 0 to FInfos.Count-1 do
      begin
        with TInfoElement(FInfos[i]) do
          Writeln( LogFile,
                   SysUtils.Format( '%-20s %12s %s',
                                     [
                                     FormatDateTime('yyyy/mm/dd hh:nn:ss', Now),
                                     '['+code+']',
                                     text
                                     ]));
      end;
    finally
      System.Close(LogFile);
    end;
  except
    TaskFailure('could not open log file');
  end;
end;


initialization
  RegisterTask(TLoggerTask);
end.

