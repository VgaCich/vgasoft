@echo off
call CompileTex
call CompileLevels
echo --Creating MemPak--
Tools\MemPak\MemPak.exe Data\*.*
move Tools\MemPak\MemPak.inc MemPak.inc
del DCU\VSEMemPak.dcu
Pause