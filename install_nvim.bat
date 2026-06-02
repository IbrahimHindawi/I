@echo off
setlocal

set "NVIM_HOME=%LOCALAPPDATA%\nvim"
if not "%~1"=="" set "NVIM_HOME=%~1"

if not exist "%NVIM_HOME%\ftdetect" mkdir "%NVIM_HOME%\ftdetect" || exit /b %errorlevel%
if not exist "%NVIM_HOME%\syntax" mkdir "%NVIM_HOME%\syntax" || exit /b %errorlevel%

copy /Y "%~dp0ftdetect\i.vim" "%NVIM_HOME%\ftdetect\i.vim" >nul || exit /b %errorlevel%
copy /Y "%~dp0syntax\i.vim" "%NVIM_HOME%\syntax\i.vim" >nul || exit /b %errorlevel%

echo installed I syntax to "%NVIM_HOME%"
