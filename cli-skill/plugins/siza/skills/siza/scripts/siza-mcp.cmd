@echo off
rem siza-mcp.cmd - native-Windows shim that starts the siza MCP stdio server.
rem Delegates to the sibling siza.cmd (which locates the compiled binary) with
rem the `mcp` subcommand. Mirrors the bash `siza-mcp` shim.
"%~dp0siza.cmd" mcp %*
exit /b %ERRORLEVEL%
