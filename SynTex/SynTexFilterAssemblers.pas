unit SynTexFilterAssemblers;

interface

uses
  Windows, AvL, avlUtils, SynTexAssembler;

type
  TSynTexFilterAssemblers=class
  protected
    FAssembler: TSynTexAssembler;
    function IdentifierToIndex(Identifiers: array of string; Identifier: string): Integer;
  public
    constructor Create(SynTexAssembler: TSynTexAssembler);
    function AssembleFill(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleAdd(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssemblePixels(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleBlend(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleMakeAlpha(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssemblePerlin(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleBump(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
    function AssembleNormals(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
  end;

implementation

{$I SynTexFilters.inc}

constructor TSynTexFilterAssemblers.Create(SynTexAssembler: TSynTexAssembler);

  procedure AddFilterAssembler(ID: Byte; FilterAssembler: TSynTexFilterAssembler);
  begin
    FAssembler.AddFilterAssembler(FltNames[ID], ID, FilterAssembler);
  end;

begin
  inherited Create;
  FAssembler:=SynTexAssembler;
  AddFilterAssembler(FLT_FILL, AssembleFill);
  AddFilterAssembler(FLT_ADD, AssembleAdd);
  AddFilterAssembler(FLT_PIXELS, AssemblePixels);
  AddFilterAssembler(FLT_BLEND, AssembleBlend);
  AddFilterAssembler(FLT_MAKEALPHA, AssembleMakeAlpha);
  AddFilterAssembler(FLT_PERLIN, AssemblePerlin);
  AddFilterAssembler(FLT_BUMP, AssembleBump);
  AddFilterAssembler(FLT_NORMALS, AssembleNormals);
end;

function TSynTexFilterAssemblers.AssembleFill(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  Color: Integer;
begin
  Result:=false;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  if Token.TokenType<>stInteger then
  begin
    FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
    Exit;
  end;
  if not FAssembler.TokenValueInteger(Token, Color) then Exit;
  Params.Write(Color, SizeOf(Color));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter FILL color');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleAdd(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  Mode: Byte;
begin
  Result:=false;
  if RegsCount<2 then
  begin
    FAssembler.Error('Filter ADD needs at least 2 registers');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Identifier expected');
    Exit;
  end;
  if Token.TokenType<>stIdentifier then
  begin
    FAssembler.Error('Identifier expected, but '+TokenName[Token.TokenType]+' found');
    Exit;
  end;
  Mode:=IdentifierToIndex(AddModes, Token.Value);
  if Mode=$FF then
  begin
    FAssembler.Error('Unknown filter ADD mode');
    Exit;
  end;
  Params.Write(Mode, SizeOf(Mode));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter ADD mode');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssemblePixels(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  ParamsBuf: array[0..2] of Integer; //Count, Color0, Color1
  i: Integer;
begin
  Result:=false;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  for i:=0 to 2 do
  begin
    if Token.TokenType<>stInteger then
    begin
      FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
      Exit;
    end;
    if not FAssembler.TokenValueInteger(Token, ParamsBuf[i]) then Exit;
    if i<2 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  Params.Write(ParamsBuf, SizeOf(Integer)*3);
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter PIXELS parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleBlend(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
begin
  Result:=false;
  if RegsCount<>4 then
  begin
    FAssembler.Error('Filter BLEND needs 4 registers');
    Exit;
  end;
  if Assigned(Token) then
  begin
    FAssembler.Error('Extra token(s) after filter BLEND');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleMakeAlpha(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter MAKEALPHA needs 2 registers');
    Exit;
  end;
  if Assigned(Token) then
  begin
    FAssembler.Error('Extra token(s) after filter MAKEALPHA');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssemblePerlin(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  ParamsBuf: array[0..5] of Integer; //Freq, Octaves, Fade, Amp, Color0, Color1
  i: Integer;
begin
  Result:=false;
  if RegsCount<>1 then
  begin
    FAssembler.Error('Filter PERLIN needs 1 register');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  for i:=0 to 5 do
  begin
    if Token.TokenType<>stInteger then
    begin
      FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
      Exit;
    end;
    if not FAssembler.TokenValueInteger(Token, ParamsBuf[i]) then Exit;
    if i<5 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  if (ParamsBuf[0]<0) or (ParamsBuf[0]>255) then
  begin
    FAssembler.Error('Filter PERLIN parameter FREQ out of bounds [0..255]');
    Exit;
  end;
  if (ParamsBuf[1]<0) or (ParamsBuf[1]>7) then
  begin
    FAssembler.Error('Filter PERLIN parameter OCTAVES out of bounds [0..7]');
    Exit;
  end;
  if (ParamsBuf[2]<0) or (ParamsBuf[2]>255) then
  begin
    FAssembler.Error('Filter PERLIN parameter FADE out of bounds [0..255]');
    Exit;
  end;
  if (ParamsBuf[3]<0) or (ParamsBuf[3]>255) then
  begin
    FAssembler.Error('Filter PERLIN parameter AMP out of bounds [0..255]');
    Exit;
  end;
  Params.Write(ParamsBuf[0], 1);
  Params.Write(ParamsBuf[1], 1);
  Params.Write(ParamsBuf[2], 1);
  Params.Write(ParamsBuf[3], 1);
  Params.Write(ParamsBuf[4], SizeOf(Integer)*2);
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter PERLIN parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleBump(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
const
  ParamsNames: array[0..5] of string =
    ('THETA', 'PHI', 'AMPLIFY', 'DIFFAMOUNT', 'SPECAMOUNT', 'SPECPOWER'); 
var
  ParamsBuf: packed array[0..5] of Byte; //Theta, Phi, Amplify, Diffuse amount, Specular amount, Specular power
  i, Val: Integer;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter BUMP needs 2 registers');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  for i:=0 to 5 do
  begin
    if Token.TokenType<>stInteger then
    begin
      FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
      Exit;
    end;
    if not FAssembler.TokenValueInteger(Token, Val) then Exit;
    if (Val<0) or (Val>255) then
    begin
      FAssembler.Error('Filter BUMP parameter '+ParamsNames[i]+' out of bounds [0..255]');
      Exit;
    end;
    ParamsBuf[i]:=Val;
    if i<5 then
      if not FAssembler.NextToken(Token, 'Integer expected') then Exit;
  end;
  Params.Write(ParamsBuf[0], SizeOf(ParamsBuf));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter BUMP parameters');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.AssembleNormals(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  Amount: Integer;
begin
  Result:=false;
  if RegsCount<>2 then
  begin
    FAssembler.Error('Filter NORMALS needs 2 registers');
    Exit;
  end;
  if not Assigned(Token) then
  begin
    FAssembler.Error('Integer expected');
    Exit;
  end;
  if Token.TokenType<>stInteger then
  begin
    FAssembler.Error('Integer expected, but '+TokenName[Token.TokenType]+' found');
    Exit;
  end;
  if not FAssembler.TokenValueInteger(Token, Amount) then Exit;
  if Amount>255 then
  begin
    FAssembler.Error('Filter NORMALS parameter AMOUNT out of bounds [0..255]');
    Exit;
  end;
  Params.Write(Amount, SizeOf(Byte));
  if Assigned(Token.Next) then
  begin
    FAssembler.Error('Extra token(s) after filter NORMALS color');
    Exit;
  end;
  Result:=true;
end;

function TSynTexFilterAssemblers.IdentifierToIndex(Identifiers: array of string; Identifier: string): Integer;
begin
  Identifier:=UpperCase(Identifier);
  for Result:=Low(Identifiers) to High(Identifiers) do
    if Identifiers[Result]=Identifier then Exit;
  Result:=-1;
end;

end.
