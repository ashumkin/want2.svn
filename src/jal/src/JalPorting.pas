{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca (R)                               }
{    ~                                         }
{     Copyright (C) 1995,2001 Juancarlo Añez   }
{     All rights reserved.                     }
{            http://www.suigeneris.org/juanca  }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{#(@)$Id$}

unit JalPorting;

interface

const
   {$IFDEF WIN32}
   MaxInt2 = Longint($7FFF);
   MaxInt4 = MaxInt;
   {$ELSE}
   MaxInt2 = MaxInt;
   MaxInt4 = MaxLong;
   {$ENDIF}

   MaxSmallInt = High(SmallInt);
   SmallIntNext = Longint(MaxSmallInt)+1;

type
     TINTEGER4 = Longint;
     TINTEGER2 = SmallInt;
     TLOGICAL1 = ByteBool;
     TWORD2    = Word;
     TWORD4    = LongWord;
     TINTEGER1 = ShortInt;

     TREAL4    = Single;
     TREAL8    = DOUBLE;

     TINTEGER  = TINTEGER4;
     INTEGER2  = TINTEGER2;
     INTEGER4  = TINTEGER4;
     WORD2     = TWORD2;
     WORD4     = TWORD4;
     REAL4     = TREAL4;
     REAL8     = TREAL8;

     sInt      = INTEGER2;
     uInt      = WORD2;
     Long      = INTEGER4;
     uLong     = WORD4;
     uLongf    = uLong;
     Bytef     = Byte;
     PBytef    = ^Bytef;

{$ifndef VER120}
type
  LongWord = Longint;
{$endif}

function OverflowedSmallIntToInt(s :SmallInt):Integer;

implementation

function OverflowedSmallIntToInt(s :SmallInt):Integer;
begin
  if (s >= 0) then
     Result := s
  else
     Result := (2*SmallIntNext) + s
end;

end.

