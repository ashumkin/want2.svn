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

unit JalProxyStreams;

interface
uses
  Classes;

const
  rcs_id :string = '@(#)$Id$';

type
  TStreamProxy = class(TStream)
  protected
    _strm    :TStream;
  public
     constructor Create(Strm :TStream);
     function Read(var Buffer; Count: Integer): Longint;    override;
     function Write(const Buffer; Count: Integer): Longint; override;
     function Seek(Offset: Longint; Origin: Word): Longint; override;

     property Stream :TStream
       read _strm;
  end;

implementation

{ TStreamProxy }

constructor TStreamProxy.Create(Strm: TStream);
begin
  assert(strm <> nil);
  inherited Create;
  _strm := Strm;
end;

function TStreamProxy.Read(var Buffer; Count: Integer): Longint;
begin
  result := _strm.Read(Buffer, Count)
end;

function TStreamProxy.Seek(Offset: Integer; Origin: Word): Longint;
begin
  result := _strm.Seek(Offset, Origin);
end;

function TStreamProxy.Write(const Buffer; Count: Integer): Longint;
begin
  result := _strm.Write(Buffer, Count)
end;

end.
