@ECHO OFF
REM Get a local Dante.exe built so it can then do a full build with itself
REM brcc32 and dcc32 must be in PATH already

brcc32 ..\src\dantever.rc

cd ..\src
dcc32 -Q -B -N%TEMP% -E%TEMP% ..\src\dante.dpr -Ulib;tasks;elements;..\lib\jcl;..\lib\xml;..\lib\paszlib;..\lib\perlre
cd ..\make
if ERRORLEVEL 1 goto ERROR
%TEMP%\dante.exe -color %1 %2 %3 %4 %5 %6 %7 %8 %9
goto END
:ERROR
:END
