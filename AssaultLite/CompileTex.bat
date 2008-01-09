@echo off
echo --Compiling textures--
call ClearCache
Tools\STAsm\STAsm.exe Textures.sta Data\Textures.stc
if NOT "%ERRORLEVEL%" == "0" pause