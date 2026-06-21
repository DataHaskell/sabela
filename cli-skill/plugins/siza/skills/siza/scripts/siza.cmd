@echo off
rem siza.cmd - native-Windows shim that locates the compiled siza-client
rem binary and execs it. Mirrors the bash `siza` shim. Resolution order:
rem   1. %SIZA_BIN%     - explicit path to the built binary (most reliable)
rem   2. %SABELA_REPO%  - your sabela checkout, resolved via cabal list-bin
rem   3. in-repo guess  - when run from its source checkout
rem If you build with non-default flags (e.g. -O2), set %SIZA_BIN%.
setlocal

if defined SIZA_BIN (
    "%SIZA_BIN%" %*
    exit /b %ERRORLEVEL%
)

set "bin="
if defined SABELA_REPO (
    for /f "usebackq delims=" %%B in (`cabal --project-dir="%SABELA_REPO%" list-bin exe:siza 2^>nul`) do set "bin=%%B"
)
if not defined bin (
    rem Repo root is six levels up from .../skills/siza/scripts.
    set "script_dir=%~dp0"
    for %%I in ("%~dp0..\..\..\..\..\..") do set "repo_root=%%~fI"
    for /f "usebackq delims=" %%B in (`cabal --project-dir="%repo_root%" list-bin exe:siza 2^>nul`) do set "bin=%%B"
)

if not defined bin (
    echo siza: cannot locate the siza binary. Set %%SIZA_BIN%% or %%SABELA_REPO%% and run 'cabal build exe:siza'.>&2
    exit /b 127
)
if not exist "%bin%" (
    echo siza: cannot locate the siza binary. Set %%SIZA_BIN%% or %%SABELA_REPO%% and run 'cabal build exe:siza'.>&2
    exit /b 127
)

"%bin%" %*
exit /b %ERRORLEVEL%
