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
  ParamsBuf: array[0..2] of Integer;
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

function TSynTexFilterAssemblers.IdentifierToIndex(Identifiers: array of string; Identifier: string): Integer;
begin
  Identifier:=UpperCase(Identifier);
  for Result:=Low(Identifiers) to High(Identifiers) do
    if Identifiers[Result]=Identifier then Exit;
  Result:=-1;
end;

end.
