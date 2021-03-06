//----------------------------------------------------------------------------//
//                                                                            //
// PakMan.pas 1.4.2, 06.06.2007; 9:00                                         //
//                                                                            //
// VSE Package Manager 1.4                                                    //
//                                                                            //
// Copyright (C) 2004-2007 VgaSoft                                            //
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

unit PakMan;

interface

type
  TLogCB=procedure(S: PChar); stdcall;
  TPakFileSource=(fsFile, fsPakNRV, fsPakLZMA, fsPakStore);

procedure PakInit(BaseDir: PChar; LogCallback: TLogCB); stdcall; external 'PakMan.dll';
procedure PakFree; stdcall; external 'PakMan.dll';
function  PakAddMountPoint(MountPoint, Source: PChar): Boolean; stdcall; external 'PakMan.dll';
function  PakDeleteMountPoint(MountPoint: PChar): Boolean; stdcall; external 'PakMan.dll';
function  PakOpenFile(Name: PChar; Flags: Cardinal): Cardinal; stdcall; external 'PakMan.dll';
function  PakCreateFile(Name: PChar; Flags: Cardinal): Cardinal; stdcall; external 'PakMan.dll';
procedure PakCloseFile(F: Cardinal); stdcall; external 'PakMan.dll';
procedure PakDeleteFile(Name: PChar); stdcall; external 'PakMan.dll';
function  PakFileExists(Name: PChar): Boolean; stdcall; external 'PakMan.dll';
function  PakFindFiles(Mask: PChar; Recursive: Boolean): PChar; stdcall; external 'PakMan.dll';

function  PakFileType(F: Cardinal): TPakFileSource; stdcall; external 'PakMan.dll';
function  PakFileRead(F: Cardinal; var Buffer; Count: Longint): Longint; stdcall; external 'PakMan.dll';
function  PakFileWrite(F: Cardinal; const Buffer; Count: Longint): Longint; stdcall; external 'PakMan.dll';
function  PakFileSeek(F: Cardinal; Offset: Longint; Origin: Word): Longint; stdcall; external 'PakMan.dll';
function  PakFileSize(F: Cardinal): Longint; stdcall; external 'PakMan.dll';
function  PakFilePosition(F: Cardinal): Longint; stdcall; external 'PakMan.dll';

function  PakExtractFileExt(const FileName: string): string;
function  PakExtractFileName(const FileName: string): string;
function  PakExtractFilePath(const FileName: string): string;

const
  InvalidPakFile: Cardinal=$FFFFFFFF;
  pmShareCompat=0;
  pmShareExclusive=$00000010;
  pmShareDenyWrite=$00000020;
  pmShareDenyRead=$00000030;
  pmShareDenyNone=$00000040;
  pmNoCreate=$00010000;
  pmNoCheck=$00020000;
  pmNoDelete=$00040000;

implementation

const
  Sep='/';

function PakExtractFileExt(const FileName: string): string;
var
  i: Integer;
begin
  i:=Length(FileName);
  while (i>1) and not (FileName[i] in [Sep, '.']) do Dec(i);
  if (i>1) and (FileName[i]='.')
    then Result:=Copy(FileName, i+1, MaxInt)
    else Result:='';
end;

function PakExtractFileName(const FileName: string): string;
var
  i: Integer;
begin
  i:=Length(FileName);
  while (i>=1) and (FileName[i]<>Sep) do Dec(i);
  Result:=Copy(FileName, i+1, MaxInt);
end;

function PakExtractFilePath(const FileName: string): string;
var
  i: Integer;
begin
  i:=Length(FileName);
  while (i>1) and (FileName[i]<>Sep) do Dec(i);
  if FileName[i]=Sep
    then Result:=Copy(FileName, 1, i)
    else Result:='';
end;

end.
