@ECHO OFF
REM Get a local Dante.exe built so it can then do a full build with itself

REM Compile -- where's the dcc32? reg.exe from NT res kit could grab it
REM For now, assume it's in the PATH

brcc32 ..\src\dantever.rc

dcc32 -B -Q -N%TEMP% -E%TEMP% ..\src\dante.dpr -U..\src;..\src\lib;..\src\tasks;..\lib\jcl;..\lib\xml;..\lib\paszlib;..\lib\paszlib\minizip;..\src\elements;..\lib\perlre
if ERRORLEVEL 1 goto ERROR
%TEMP%\dante.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END
:ERROR
:END
