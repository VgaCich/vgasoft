unit StrKey;

interface

uses
  Windows, SysUtils;

procedure InitStrKey;
function KeyToStr(Key: byte): string;

implementation

var
  Keys: array[0..255] of string;

procedure InitStrKey;
var
  i: integer;
  Buf: array[0..99] of Char;
begin
  for i := 0 to 255 do begin
    GetKeyNameText(MapVirtualKey(i, 0) shl 16, @Buf, 100);
    Keys[i] := UpperCase(Buf);
  end;
  Keys[$00] := 'NONE';
  Keys[$03] := 'CANCEL';
  Keys[$0C] := 'CLEAR';
  Keys[$1B] := 'ESCAPE';
  Keys[$13] := 'PAUSE';
  Keys[$20] := 'SPACE';
  Keys[$21] := 'PGUP';
  Keys[$22] := 'PGDN';
  Keys[$23] := 'END';
  Keys[$24] := 'HOME';
  Keys[$25] := 'LEFT';
  Keys[$26] := 'UP';
  Keys[$27] := 'RIGHT';
  Keys[$28] := 'DOWN';
  Keys[$29] := 'SELECT';
  Keys[$2D] := 'INS';
  Keys[$2E] := 'DEL';
  Keys[$5B] := 'LWIN';
  Keys[$5C] := 'RWIN';
  Keys[$5D] := 'APPS';
  Keys[$6F] := 'NUM /';
  Keys[$90] := 'NUMLOCK';
end;

function KeyToStr(Key: byte): string;
begin
  Result:=Keys[Key];
end;

end.
