@echo off
call CompileTex
call CompileLevels
echo --Creating MemPak--
Tools\MemPak\MemPak.exe Data\*.*
copy Tools\MemPak\MemPak.inc MemPak.inc
del Tools\MemPak\MemPak.inc
Pause