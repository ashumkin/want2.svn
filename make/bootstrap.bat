@ECHO OFF
REM Get a local Dante.exe built so it can then do a full build with itself
REM brcc32 and dcc32 must be in PATH already

brcc32 ..\src\dantever.rc

dcc32 -B -Q -N%TEMP% -E%TEMP% ..\src\dante.dpr -U..\src;..\src\lib;..\src\tasks;..\lib\jcl;..\lib\xml;..\lib\paszlib;..\lib\paszlib\minizip;..\src\elements;..\lib\perlre

if ERRORLEVEL 1 goto ERROR
%TEMP%\dante.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END
:ERROR
:END
