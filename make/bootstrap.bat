REM Get a local Dante.exe built so it can then do a full build with itself

REM Compile -- where's the dcc32? reg.exe from NT res kit could grab it
REM For now, assume it's in the PATH

dcc32.exe -B -Q -E"." ..\src\dante.dpr

dante.exe