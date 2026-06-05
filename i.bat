@echo off
setlocal

set "I_SOURCE=%~1"
if "%I_SOURCE%"=="" set "I_SOURCE=src\main.i"

set "I_OUTPUT=%~2"
if "%I_OUTPUT%"=="" set "I_OUTPUT=build\i_gen\main.i.c"

python bunyan.py build debug || exit /b %errorlevel%
if not exist build\i_gen mkdir build\i_gen || exit /b %errorlevel%
build\I.exe compile "%I_SOURCE%" -o "%I_OUTPUT%" || exit /b %errorlevel%
clang.exe "%I_OUTPUT%" -I src -I src\runtime -o build\main_i.exe || exit /b %errorlevel%
build\main_i.exe
