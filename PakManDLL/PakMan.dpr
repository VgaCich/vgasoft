//----------------------------------------------------------------------------//
//                                                                            //
// PakMan.dpr 1.3.0, 20.11.2006; 2:00                                         //
//                                                                            //
// VSE Package Manager 1.3.0                                                  //
//                                                                            //
// Copyright (C) 2004-2006 VgaSoft (www.vgasoft.narod.ru)                     //
//                                                                            //
//This program is free software; you can redistribute it and/or               //
//modify it under the terms of the GNU General Public License                 //
//as published by the Free Software Foundation; either version 2              //
//of the License, or any later version.                                       //
//                                                                            //
//This program is distributed in the hope that it will be useful,             //
//but WITHOUT ANY WARRANTY; without even the implied warranty of              //
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               //
//GNU General Public License (http://www.gnu.org/copyleft/gpl.html)           //
//for more details.                                                           //
//----------------------------------------------------------------------------//

library PakMan;

{$R *.res}

uses SysSfIni, UPakMan;

exports
  PakInit,
  PakFree,
  PakOpenFile,
  PakCreateFile,
  PakCloseFile,
  PakDeleteFile,
  PakFileExists,
  PakFindFiles,
  PakFileType,
  PakFileRead,
  PakFileWrite,
  PakFileSeek,
  PakFileSize,
  PakFilePosition;

begin
  LogCB:=nil;
end.
