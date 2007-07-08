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
  end;

implementation

{$I SynTexFilters.inc} 

constructor TSynTexFilterAssemblers.Create(SynTexAssembler: TSynTexAssembler);
begin
  inherited Create;
  FAssembler:=SynTexAssembler;
  SynTexAssembler.AddFilterAssembler('FILL', FLT_FILL, AssembleFill);
  SynTexAssembler.AddFilterAssembler('ADD', FLT_ADD, AssembleAdd);
  SynTexAssembler.AddFilterAssembler('PIXELS', FLT_PIXELS, AssemblePixels);
  SynTexAssembler.AddFilterAssembler('BLEND', FLT_BLEND, AssembleBlend);
  SynTexAssembler.AddFilterAssembler('MAKEALPHA', FLT_MAKEALPHA, AssembleMakeAlpha);
  SynTexAssembler.AddFilterAssembler('PERLIN', FLT_PERLIN, AssemblePerlin);
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
  Result:=RegsCount=4;
  if Assigned(Token) then
  begin
    FAssembler.Error('Extra token(s) after filter BLEND');
    Result:=false;
  end;
end;

function TSynTexFilterAssemblers.AssembleMakeAlpha(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
begin
  Result:=RegsCount=2;
  if Assigned(Token) then
  begin
    FAssembler.Error('Extra token(s) after filter MAKEALPHA');
    Result:=false;
  end;
end;

function TSynTexFilterAssemblers.AssemblePerlin(Token: PSynTexToken; Params: TStream; RegsCount: Integer): Boolean;
var
  ParamsBuf: array[0..5] of Integer; //Freq, Octaves, Fade, Amp, Color0, Color1
  i: Integer;
begin
  Result:=false;
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
  if ParamsBuf[0]*(1 shl ParamsBuf[1])>255 then
  begin
    FAssembler.Error('Filter PERLIN max octave frequency out of range');
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

function TSynTexFilterAssemblers.IdentifierToIndex(Identifiers: array of string; Identifier: string): Integer;
begin
  Identifier:=UpperCase(Identifier);
  for Result:=Low(Identifiers) to High(Identifiers) do
    if Identifiers[Result]=Identifier then Exit;
  Result:=-1;
end;

end.
