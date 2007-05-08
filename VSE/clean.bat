del *.~*
del *.dcu
cd Tools\FontGen
"c:\program files\upx124w\stripreloc" FontGen.exe
del *.~*
del *.dcu
del *.exe.bak
del *.cfg
del *.res
cd ..\VSPak
"c:\program files\upx124w\stripreloc" VSPak.exe
del *.~*
del *.dcu
del *.exe.bak
del *.cfg
del *.res
