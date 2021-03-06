{(c)VgaSoft, 2004-2007}
unit avlUtils;

interface

uses Windows, AvL;

type
  TSysCharSet = set of Char;

function StrEnd(const Str: PChar): PChar;
function StrCopy(Dest: PChar; const Source: PChar): PChar;
function CompareStr(const S1, S2: string): Integer;
function StrCat(Dest, Source: PChar): PChar;
function ChangeFileExt(const FileName, Extension: string): string;
function FirstDelimiter(const Delimiters, S: String): Integer;
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
function TryStrToInt(const S: string; out Value: Integer): Boolean;
function TryStrToFloat(const S: string; out Value: Single): Boolean;
function StrToCar(const S: string): Cardinal;
function StrToInt64Def(const S: string; const Default: Int64): Int64;
function HexToByte(const Hex: string): byte;
function FloatToStr2(M: Real; I, D: Integer): string;
function FloatToStrF(X: Extended): string;
function BoolToStr(B: Boolean): string;
function FileTimeToDateTime(T: TFileTime): TDateTime;
function NoFirst(const S: string): string;
function ForceDirectories(Dir: string): Boolean;
function ExcludeTrailingBackslash(const S: string): string;
procedure ExcludeTrailingBackslashV(var S: string);
function AddTrailingBackslash(const S: string): string;
procedure AddTrailingBackslashV(var S: string);
function FlSize(const FileName: string): Cardinal;
function DirSize(Dir: string): Cardinal;
function FlModifyTime(const FileName: string): TDateTime;
procedure RemoveVoidStrings(Strings: TStringList);
procedure FreeMemAndNil(var P: Pointer; Size: integer);
procedure FAN(var Obj);
procedure ClearList(List: TList);
procedure FreeList(var List: TList);
function SLValueName(const S: string): string;
function ExePath: string;
procedure VGetMem(var P: Pointer; Size: Cardinal);
procedure VFreeAndNil(var P: Pointer; Size: Cardinal);
function UniTempFile: string;
function FontToStr(Font: TFont): string;
procedure StrToFont(S: string; Font: TFont);
function Tok(const Sep: string; var S: string): string;
function GetTempDir: string;
function MessageDlg(const Text, Title: string; Style: Cardinal): Integer;
procedure MessageFmt(const Fmt: string; const Args: array of const);
function FullExeName: string;
function SizeToStr(Size: Int64): string;
function CheckPath(const Path: string; Abs: Boolean): Boolean;
{function GetShiftState: TShiftState; }
function FindCmdLineSwitch(const Switch: string; SwitchChars: TSysCharSet; IgnoreCase: Boolean): Boolean;
procedure CutFirstDirectory(var S: string);
function MinimizeName(const Filename: string; MaxLen: Integer): string;
function IncPtr(Ptr: Pointer; N: Integer = 1): Pointer;
function IntToStrLZ(I, Len: Integer): string;
function ANSI2OEM(const S: string): string;
procedure GetPrivilege(const Privilege: string);
function ExecAndWait(const CmdLine: string): Boolean;
function GUIDToString(const GUID: TGUID): string;

implementation

function StrEnd(const Str: PChar): PChar; assembler;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        LEA     EAX,[EDI-1]
        MOV     EDI,EDX
end;

function StrCopy(Dest: PChar; const Source: PChar): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,3
        REP     MOVSB
        POP     ESI
        POP     EDI
end;

function CompareStr(const S1, S2: string): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@1
        MOV     EAX,[EAX-4]
@@1:    OR      EDX,EDX
        JE      @@2
        MOV     EDX,[EDX-4]
@@2:    MOV     ECX,EAX
        CMP     ECX,EDX
        JBE     @@3
        MOV     ECX,EDX
@@3:    CMP     ECX,ECX
        REPE    CMPSB
        JE      @@4
        MOVZX   EAX,BYTE PTR [ESI-1]
        MOVZX   EDX,BYTE PTR [EDI-1]
@@4:    SUB     EAX,EDX
        POP     EDI
        POP     ESI
end;

function StrCat(Dest, Source: PChar): PChar;
begin
  StrCopy(StrEnd(Dest), Source);
  Result:=Dest;
end;

function ChangeFileExt(const FileName, Extension: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.',Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function FirstDelimiter(const Delimiters, S: String): Integer;
var
  n: LongWord;
begin
  Result := 1;
  for n := Result to Length(S) do
   if S[n] = Delimiters then
    begin
     Result := n;
     Exit;
    end;
end;

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function TryStrToFloat(const S: string; out Value: Single): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function StrToCar(const S: string): Cardinal;
var
  E: integer;
begin
  Val(S, Result, E);
end;

function StrToInt64Def(const S: string; const Default: Int64): Int64;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Default;
end;

function HexToByte(const Hex: string): byte;
var
  i: byte;
begin
  Result:=0;
  if Length(Hex)<>2 then Exit;
  for i:=1 to 2 do
    if Hex[i] in ['0'..'9']
      then Result := (Result shl 4) or (Ord(Hex[i]) - Ord('0'))
      else if Hex[i] in ['A'..'F']
        then Result := (Result shl 4) or (Ord(Hex[i]) - Ord('A') + 10)
        else if Hex[i] in ['a'..'f']
          then Result := (Result shl 4) or (Ord(Hex[i]) - Ord('a') + 10)
          else Break;
end;

function FloatToStr2(M: Real; I, D: Integer): string;
begin
  Str(M:I:D, Result);
end;

function FloatToStrF(X: Extended): string;
begin
  Str(X, Result);
end;

function BoolToStr(B: Boolean): string;
const
  Str: array[Boolean] of string=('False', 'True');
begin
  Result:=Str[B];
end;

function FileTimeToDateTime(T: TFileTime): TDateTime;
var
  T1: TFileTime;
  S: TSystemTime;
begin
  FileTimeToLocalFileTime(T, T1);
  FileTimeToSystemTime(T1, S);
  SystemTimeToDateTime(S, Result);
end;

function NoFirst(const S: string): string;
begin
  Result:=Copy(S, 2, MaxInt);
end;

function ForceDirectories(Dir: string): Boolean;
begin
  Result := True;
  Dir := ExcludeTrailingBackslash(Dir);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;

function ExcludeTrailingBackslash(const S: string): string;
begin
  if (S<>'') and (S[Length(S)]='\')
    then Result:=Copy(S, 1, Length(S)-1)
    else Result:=S;
end;

procedure ExcludeTrailingBackslashV(var S: string);
begin
  S:=ExcludeTrailingBackslash(S);
end;

function AddTrailingBackslash(const S: string): string;
begin
  if (S<>'') and (S[Length(S)]<>'\')
    then Result:=S+'\'
    else Result:=S;
end;

procedure AddTrailingBackslashV(var S: string);
begin
  S:=AddTrailingBackslash(S);
end;

function FlSize(const FileName: string): Cardinal;
var
  Fl: TFileStream;
begin
  try
    Fl:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    Result:=Fl.Size;
  finally
    FAN(Fl);
  end;
end;

function DirSize(Dir: string): Cardinal;
var
  SR: TSearchRec;
begin
  Result := 0;
  AddTrailingBackslashV(Dir);
  if FindFirst(Dir+'*', faAnyFile, SR)=0 then
    repeat
      if SR.Attr and faDirectory = faDirectory then
      begin
        if (SR.Name<>'.') and (SR.Name<>'..')
          then Inc(Result, DirSize(Dir+SR.Name));
      end
        else Inc(Result, SR.Size);
    until FindNext(SR)<>0;
  FindClose(SR);
end;

function FlModifyTime(const FileName: string): TDateTime;
var
  Fl: TFileStream;
  FT: TFileTime;
begin
  try
    Fl:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    GetFileTime(Fl.Handle, nil, nil, @FT);
    Result:=FileTimeToDateTime(FT);
  finally
    FAN(Fl);
  end;
end;

procedure RemoveVoidStrings(Strings: TStringList);
var
  i: integer;
begin
  for i:=Strings.Count-1 downto 0 do
    if Strings[i]='' then Strings.Delete(i);
end;

procedure FreeMemAndNil(var P: Pointer; Size: integer);
begin
  if P<>nil then FreeMem(P, Size);
  P:=nil;
end;

procedure FAN(var Obj);
begin
  if Pointer(Obj)=nil then Exit;
  TObject(Obj).Free;
  Pointer(Obj):=nil;
end;

procedure ClearList(List: TList);
var
  i: integer;
begin
  if List=nil then Exit;
  for i:=0 to List.Count-1 do
    if Assigned(List[i]) then TObject(List[i]).Free;
end;

procedure FreeList(var List: TList);
begin
  ClearList(List);
  FAN(List);
end;

function SLValueName(const S: string): string;
begin
  Result:=Copy(S, 1, FirstDelimiter('=', S)-1);
end;

function ExePath: string;
begin
  Result:=ExtractFilePath(FullExeName);
end;

procedure VGetMem(var P: Pointer; Size: Cardinal);
begin
  P:=VirtualAlloc(nil, Size, MEM_COMMIT, PAGE_READWRITE);
  if P=nil then raise Exception.CreateFmt('VirtualAlloc error (%d)', [GetLastError]);
end;

procedure VFreeAndNil(var P: Pointer; Size: Cardinal);
begin
  if P=nil then Exit;
  if not VirtualFree(P, Size, MEM_DECOMMIT)
    then raise Exception.CreateFmt('VirtualFree error (%d)', [GetLastError]);
  P:=nil;
end;

function UniTempFile: string;
var
  i: Integer;
begin
  Result:=TempDir;
  AddTrailingBackslashV(Result);
  SetLength(Result, Length(Result)+8);
  repeat
    for i:=Length(Result)-7 to Length(Result) do
      Result[i]:=Chr(65+Random(25));
  until not FileExists(Result);
end;

const Sep='|';

function FontToStr(Font: TFont): string;

  function YesNo(Exp: Boolean): Char;
  begin
    if Exp
      then Result:='Y'
      else Result:='N';
  end;

begin
  Result:=IntToStr(Font.Charset)+Sep+IntToStr(Font.Color)+Sep+IntToStr(Font.Height)+
    Sep+IntToStr(Font.Width)+Sep+Font.Name+Sep+IntToStr(Ord(Font.Pitch))+Sep+
    IntToStr(Font.Size)+Sep+YesNo(fsBold in Font.Style)+YesNo(fsItalic in Font.Style)+
    YesNo(fsUnderline in Font.Style)+YesNo(fsStrikeOut in Font.Style);
end;

procedure StrToFont(S: string; Font: TFont);
begin
  if S='' then Exit;
  Font.Charset:=StrToInt(Tok(Sep, S));
  Font.Color:=StrToInt(Tok(Sep, S));
  Font.Height:=StrToInt(Tok(Sep, S));
  Font.Width:=StrToInt(Tok(Sep, S));
  Font.Name:=Tok(Sep, S);
  Font.Pitch:=TFontPitch(StrToInt(Tok(Sep, S)));
  Font.Size:=StrToInt(Tok(Sep, S));
  Font.Style:=[];
  if S[1]='Y' then
    Font.Style:=Font.Style+[fsBold];
  if S[2]='Y' then
    Font.Style:=Font.Style+[fsItalic];
  if S[3]='Y' then
    Font.Style:=Font.Style+[fsUnderline];
  if S[4]='Y' then
    Font.Style:=Font.Style+[fsStrikeOut];
end;

function Tok(const Sep: string; var S: string): string;

  function IsOneOf(C: Char; const S: string): Boolean;
  var
    i: integer;
  begin
    Result:=false;
    for i:=1 to Length(S) do
    begin
      if C=S[i] then
      begin
        Result:=true;
        Exit;
      end;
    end;
  end;

var
  C: Char;
begin
  Result:='';
  if S='' then Exit;
  C:=S[1];
  while IsOneOf(C, Sep) do
  begin
    Delete(S, 1, 1);
    if S='' then Exit;
    C:=S[1];
  end;
  while (not IsOneOf(C, Sep)) and (S<>'') do
  begin
    Result:=Result+C;
    Delete(S, 1, 1);
    if S='' then Exit;
    C:=S[1];
  end;
end;

function GetTempDir: string;
var
  i: Integer;
begin
  Result:=TempDir;
  Randomize;
  AddTrailingBackslashV(Result);
  SetLength(Result, Length(Result)+8);
  repeat
    for i:=Length(Result)-7 to Length(Result) do
      Result[i]:=Chr(65+Random(26));
  until not DirectoryExists(Result);
  AddTrailingBackslashV(Result);
end;

function MessageDlg(const Text, Title: string; Style: Cardinal): Integer;
begin
  Result:=MessageBox(MsgDefHandle, PChar(Text), PChar(Title), Style);
end;

procedure MessageFmt(const Fmt: string; const Args: array of const);
begin
  ShowMessage(Format(Fmt, Args));
end;

function FullExeName: string;
begin
  Result:=ExpandFileName(ExeName);
end;

function SizeToStr(Size: Int64): string;
const
  KB=$400;
  MB=$100000;
  GB=$40000000;
  TB=$10000000000;
  BS=' Bytes';
  KBS=' KB';
  MBS=' MB';
  GBS=' GB';
  TBS=' TB';
begin
  Result:=IntToStr(Size)+BS;
  if Size>=KB
    then Result:=FloatToStr2(Size/KB, 1, 2)+KBS;
  if Size>=MB
    then Result:=FloatToStr2(Size/MB, 1, 2)+MBS;
  if Size>=GB
    then Result:=FloatToStr2(Size/GB, 1, 2)+GBS;
  if Size>=TB
    then Result:=FloatToStr2(Size/TB, 1, 2)+TBS;
end;

function CheckPath(const Path: string; Abs: Boolean): Boolean;
const
  CIncorPathChar=['/', ':', '*', '?', '"', '<', '>', '|'];
var
  i, Start: Integer;
begin
  if Abs then
  begin
    Result:=not ((Length(Path)<2) or (UpperCase(Path[1])<#65) or (UpperCase(Path[1])>#90) or (Path[2]<>':') or
      ((Length(Path)>2) and (Path[3]<>'\')));
    Start:=3;
  end
  else begin
    Result:=true;
    Start:=1;
  end;
  if not Result then Exit;
  for i:=Start to Length(Path) do
  begin
    if Path[i] in CIncorPathChar then Result:=false;
    if (Path[i]='\') and (Path[i-1]='\') then Result:=false;
  end;
end;

{function GetShiftState: TShiftState;
begin
  Result := [];
  if Keys and MK_SHIFT <> 0 then Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end; }

function FindCmdLineSwitch(const Switch: string; SwitchChars: TSysCharSet; IgnoreCase: Boolean): Boolean;
var
  I: Integer;
  S: string;
begin
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if (SwitchChars = []) or (S[1] in SwitchChars) then
      if IgnoreCase then
      begin
        if (AnsiCompareText(Copy(S, 2, Maxint), Switch) = 0) then
        begin
          Result := True;
          Exit;
        end;
      end
      else begin
        if (AnsiCompareStr(Copy(S, 2, Maxint), Switch) = 0) then
        begin
          Result := True;
          Exit;
        end;
      end;
  end;
  Result := False;
end;

procedure CutFirstDirectory(var S: string);
var
  Root: Boolean;
  P: Integer;
begin
  if S = '\' then
    S := ''
  else
  begin
    if S[1] = '\' then
    begin
      Root := True;
      Delete(S, 1, 1);
    end
    else
      Root := False;
    if S[1] = '.' then
      Delete(S, 1, 4);
    P := AnsiPos('\',S);
    if P <> 0 then
    begin
      Delete(S, 1, P);
      S := '...\' + S;
    end
    else
      S := '';
    if Root then
      S := '\' + S;
  end;
end;

function MinimizeName(const Filename: string; MaxLen: Integer): string;
var
  Drive: string;
  Dir: string;
  Name: string;
begin
  Result:=FileName;
  Dir:=ExtractFilePath(Result);
  Name:=ExtractFileName(Result);
  if (Length(Dir)>=2) and (Dir[2]=':') then
  begin
    Drive:=Copy(Dir, 1, 2);
    Delete(Dir, 1, 2);
  end
  else
    Drive:='';
  while ((Dir<>'') or (Drive<>'')) and (Length(Result)>MaxLen) do
  begin
    if Dir='\...\' then
    begin
      Drive:='';
      Dir:='...\';
    end
    else if Dir='' then
      Drive:=''
    else
      CutFirstDirectory(Dir);
    Result:=Drive+Dir+Name;
  end;
end;

function IncPtr(Ptr: Pointer; N: Integer = 1): Pointer;
begin
  Result := Pointer(Cardinal(Ptr) + N);
end;

function IntToStrLZ(I, Len: Integer): string;
begin
  Result:=IntToStr(I);
  while Length(Result)<Len do Result:='0'+Result;
end;

function ANSI2OEM(const S: string): string;
begin
  SetLength(Result, Length(S));
  CharToOem(PChar(S), PChar(Result));
end;

procedure GetPrivilege(const Privilege: string);
var
  TokenPriv: TTokenPrivileges;
  TokenHandle: THandle;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, TokenHandle) then
      if LookupPrivilegeValue(nil, Pchar(Privilege), TokenPriv.Privileges[0].LUID) then
      begin
        TokenPriv.PrivilegeCount:=1;
        TokenPriv.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
        AdjustTokenPrivileges(TokenHandle, false, TokenPriv, 0, TTokenPrivileges(nil^), DWORD(nil^));
      end;
  end;
end;

function ExecAndWait(const CmdLine: string): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  FillChar(SI, SizeOf(SI) , 0);
  with SI do
  begin
    cb := SizeOf( SI);
  end;
  if not CreateProcess(nil, PChar(CmdLine), nil, nil, false, Create_default_error_mode,
                nil, nil, SI, PI)
    then begin
      //ShowMessage(SysErrorMessage(GetLastError));
      Result:=false;
      Exit;
    end;
  WaitForSingleObject(PI.hProcess, infinite);
  Result:=true;
end;

function GUIDToString(const GUID: TGUID): string;
begin
  with GUID do Result:=Format('{%08x-%04x-%04x-%02x%02x-%02x%02x%02x%02x%02x%02x}',
    [D1, D2, D3, D4[0], D4[1], D4[2], D4[3], D4[4], D4[5], D4[6], D4[7]]);
end;

end.
