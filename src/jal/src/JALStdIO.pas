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

unit JALStdIO;
interface
uses
   SysUtils,
   Windows;
var
   StdConsoleIn  :THandle = 0;
   StdConsoleOut :THandle = 0;
   StdConsoleErr :THandle = 0;

   STDIN  :Text absolute Input;
   STDOUT :Text absolute Output;
   STDERR :Text;

implementation

initialization
   StdConsoleIn  := GetStdHandle(STD_INPUT_HANDLE);
   StdConsoleOut := GetStdHandle(STD_OUTPUT_HANDLE);
   StdConsoleErr := GetStdHandle(STD_ERROR_HANDLE);

   if IsConsole then 
   begin
       Assign(STDERR, '');
       Rewrite(STDERR);
       TTextRec(STDERR).Handle := StdConsoleErr;

       Assign(Input,'');
       Reset(Input);
       TTextRec(Input).Handle := StdConsoleIn;

       Assign(Output, '');
       Rewrite(Output);
       TTextRec(Output).Handle := StdConsoleOut
   end;
finalization
   if IsConsole then 
   begin
     Flush(StdErr);
     Flush(Output);
     Flush(Input);
   end;
end.

