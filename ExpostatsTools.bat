@echo off
set VENDOR=vendor
set OLDPATH=%PATH%
set LANG=%1
IF [%LANG%]==[] set LANG=Fr
:continue
set ARCHITECTURE=x64
set JAVA_HOME=%~dp0%VENDOR%\jdk_%ARCHITECTURE%
set R_HOME=%~dp0%VENDOR%\R

set R_BIN=%R_HOME%\bin\%ARCHITECTURE%
set PATH=%JAVA_HOME%\bin;%JAVA_HOME%\bin\server;%R_BIN%
set JAGS_HOME=%~dp0%VENDOR%\JAGS
set RUNCMD="%JAVA_HOME%"\bin\java.exe -Dfile.encoding=UTF-8 -Djava.library.path="%~dp0%VENDOR%\R\library\rJava\jri\%ARCHITECTURE%" -classpath "%~dp0%VENDOR%\R\library\JavaGD\java\javaGD.jar;%~dp0%VENDOR%\R\library\rJava\jri\JRI.jar;%~dp0dist\ExpostatsHorsLigne.jar" expostatshorsligne.Launcher %LANG%
%RUNCMD%
set PATH=%OLDPATH%