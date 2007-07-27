unit SynTexFilters;

interface

uses
  Windows, AvL, avlUtils, SynTex, Noise, avlVectors, avlMath;

type
  TSynTexFilters=class
  protected
    FSynTex: TSynTex;
    function  ClampVal(Val, Max: Integer): Integer;
    function  WrapVal(Val, Max: Integer): Integer;
    function  PixelIndex(X, Y: Integer; ClampX, ClampY: Boolean): Integer;
    {function  GetPixel(Reg: PSynTexRegister; X, Y: Integer; ClampX, ClampY: Boolean): TRGBA;
    procedure SetPixel(Reg: PSynTexRegister; X, Y: Integer; ClampX, ClampY: Boolean; Pixel: TRGBA);}
    function  BlendColors(Color1, Color2: TRGBA; Blend: Byte): TRGBA; overload;
    function  BlendColors(Color1, Color2: TRGBA; Blend: TRGBA): TRGBA; overload;
    function  Add(Color1, Color2: TRGBA): TRGBA;
    function  Sub(Color1, Color2: TRGBA): TRGBA;
    function  AddClamp(Color1, Color2: TRGBA): TRGBA;
    function  SubClamp(Color1, Color2: TRGBA): TRGBA;
    function  Diff(Color1, Color2: TRGBA): TRGBA;
    function  Mul(Color1, Color2: TRGBA): TRGBA; overload;
    function  Mul(Color: TRGBA; Scale: Byte): TRGBA; overload;
    function  MulClamp(Color: TRGBA; Scale: Integer): TRGBA;
  public
    constructor Create(SynTex: TSynTex);
    function FiltFill(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltAdd(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltPixels(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltBlend(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltMakeAlpha(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltPerlin(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltBump(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltNormals(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
  end;

implementation

const
  BDegToRad=pi/128;

{$I SynTexFilters.inc}

constructor TSynTexFilters.Create(SynTex: TSynTex);
begin
  inherited Create;
  FSynTex:=SynTex;
  SynTex.AddFilter(FLT_FILL, FiltFill);
  SynTex.AddFilter(FLT_ADD, FiltAdd);
  SynTex.AddFilter(FLT_PIXELS, FiltPixels);
  SynTex.AddFilter(FLT_BLEND, FiltBlend);
  SynTex.AddFilter(FLT_MAKEALPHA, FiltMakeAlpha);
  SynTex.AddFilter(FLT_PERLIN, FiltPerlin);
  SynTex.AddFilter(FLT_BUMP, FiltBump);
  SynTex.AddFilter(FLT_NORMALS, FiltNormals);
end;

function TSynTexFilters.FiltFill(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
var
  Color: TRGBA;
  i, j: Integer;
begin
  Result:=CheckRemain(Parameters, SizeOf(Color){$IFDEF SYNTEX_USELOG}, 'Filter:Fill'{$ENDIF});
  if not Result then Exit;
  try
    Parameters.Read(Color, SizeOf(Color));
    for i:=0 to RegsCount-1 do
      for j:=0 to FSynTex.TexSize*FSynTex.TexSize-1 do
        Regs[i]^[j]:=Color;
  except
    Result:=false;
  end;
end;

function TSynTexFilters.FiltAdd(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
var
  i, j: Integer;
  Mode: Byte;
  Clr: TRGBA;
begin
  Result:=false;
  if RegsCount<2 then Exit;
  Dec(RegsCount);
  if not CheckRemain(Parameters, SizeOf(Mode){$IFDEF SYNTEX_USELOG}, 'Filter:Add'{$ENDIF}) then Exit;
  Parameters.Read(Mode, SizeOf(Mode));
  for i:=0 to FSynTex.TexSize*FSynTex.TexSize-1 do
  begin
    Clr:=Regs[1]^[i];
    case Mode of
      ADDMODE_ADDCLAMP: for j:=2 to RegsCount do Clr:=AddClamp(Clr, Regs[j]^[i]);
      ADDMODE_ADDWRAP: for j:=2 to RegsCount do Clr:=Add(Clr, Regs[j]^[i]);
      ADDMODE_SUBCLAMP: for j:=2 to RegsCount do Clr:=SubClamp(Clr, Regs[j]^[i]);
      ADDMODE_SUBWRAP: for j:=2 to RegsCount do Clr:=Sub(Clr, Regs[j]^[i]);
      ADDMODE_DIFF: for j:=2 to RegsCount do Clr:=Diff(Clr, Regs[j]^[i]);
      ADDMODE_MUL: for j:=2 to RegsCount do Clr:=Mul(Clr, Regs[j]^[i]);
      else Exit;
    end;
    Regs[0]^[i]:=Clr;
  end;
  Result:=true;
end;

function TSynTexFilters.FiltPixels(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
var
  Clrs: packed array[0..1] of TRGBA;
  Count: Integer;
  i, j: Integer;
begin
  Result:=CheckRemain(Parameters, SizeOf(Clrs)+SizeOf(Integer){$IFDEF SYNTEX_USELOG}, 'Filter:Pixels'{$ENDIF});
  if not Result then Exit;
  try
    Parameters.Read(Count, SizeOf(Count));
    Parameters.Read(Clrs[0], SizeOf(Clrs));
    for i:=0 to RegsCount-1 do
      for j:=0 to Count-1 do
        Regs[i]^[Random(FSynTex.TexSize*FSynTex.TexSize)]:=BlendColors(Clrs[0], Clrs[1], Random(256));
  except
    Result:=false;
  end;
end;

function TSynTexFilters.FiltBlend(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
var
  i: Integer;
begin
  Result:=false;
  if RegsCount<>4 then Exit;
  for i:=0 to FSynTex.TexSize*FSynTex.TexSize-1 do
    Regs[0]^[i]:=BlendColors(Regs[1]^[i], Regs[2]^[i], Regs[3]^[i]);
  Result:=true;
end;

function TSynTexFilters.FiltMakeAlpha(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
var
  i: Integer;
begin
  Result:=false;
  if RegsCount<>2 then Exit;
  for i:=0 to FSynTex.TexSize*FSynTex.TexSize-1 do
    Regs[0]^[i].A:=Regs[1]^[i].R;
  Result:=true;
end;

function TSynTexFilters.FiltPerlin(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
var
  X, Y, i: Integer;
  PN: array[0..7] of TPerlinNoise;
  Weights: array[0..7] of Single;
  WeightsSum, Val: Single;
  Clrs: packed array[0..1] of TRGBA;
  Freq, Octaves, Fade, Amp: Byte;
begin
  Result:=false;
  if RegsCount<>1 then Exit;
  if not CheckRemain(Parameters, SizeOf(Clrs)+4*SizeOf(Byte){$IFDEF SYNTEX_USELOG}, 'Filter:Perlin'{$ENDIF}) then Exit;
  Parameters.Read(Freq, SizeOf(Freq));
  Parameters.Read(Octaves, SizeOf(Octaves));
  Parameters.Read(Fade, SizeOf(Fade));
  Parameters.Read(Amp, SizeOf(Amp));
  Parameters.Read(Clrs[0], SizeOf(Clrs));
  try
    WeightsSum:=0;
    for i:=0 to Octaves do
    begin
      PN[i]:=TPerlinNoise.Create((1 shl i)*Freq/FSynTex.TexSize, (1 shl i)*Freq);
      if i>0
        then Weights[i]:=Weights[i-1]*Fade/255
        else Weights[i]:=1;
      WeightsSum:=WeightsSum+Weights[i];
    end;
    for i:=0 to Octaves do Weights[i]:=(Amp/64)*Weights[i]/WeightsSum;
    for X:=0 to FSynTex.TexSize-1 do
      for Y:=0 to FSynTex.TexSize-1 do
      begin
        Val:=0;
        for i:=0 to Octaves do Val:=Val+PN[i].Noise(X, Y)*Weights[i];
        Regs[0]^[Y*FSynTex.TexSize+X]:=BlendColors(Clrs[0], Clrs[1], ClampVal(Trunc(128+128*Val), 256));
      end;
  finally
    for i:=0 to Octaves do PN[i].Free;
  end;
  Result:=true;
end;

function TSynTexFilters.FiltBump(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
type
  TParams=(pPhi, pTheta, pAmplify, pDiffAmount, pSpecAmount, pSpecPower);
const
  View: TVector3D = (X: 0; Y: 0; Z: 1);
var
  X, Y: Integer;
  LDir, H, Normal: TVector3D;
  Pix: Integer;
  I: Integer;
  Params: packed array[TParams] of Byte;
begin
  Result:=false;
  if RegsCount<>2 then Exit;
  if not CheckRemain(Parameters, SizeOf(Params){$IFDEF SYNTEX_USELOG}, 'Filter:Bump'{$ENDIF}) then Exit;
  Parameters.Read(Params, SizeOf(Params));
  LDir.X:=cos(Params[pPhi]*BDegToRad)*sin(Params[pTheta]*BDegToRad/2);
  LDir.Y:=sin(Params[pPhi]*BDegToRad)*sin(Params[pTheta]*BDegToRad/2);
  LDir.Z:=cos(Params[pTheta]*BDegToRad/2);
  H:=VectorAdd(LDir, View);
  VectorNormalize(H);
  for X:=0 to FSynTex.TexSize-1 do
    for Y:=0 to FSynTex.TexSize-1 do
    begin
      Pix:=X+Y*FSynTex.TexSize;
      Normal.X:=(Regs[1]^[Pix].R-128)/128;
      Normal.Y:=(Regs[1]^[Pix].G-128)/128;
      Normal.Z:=(Regs[1]^[Pix].B-128)/128;
      VectorNormalize(Normal);
      I:=Max(Round(VectorDotProduct(Normal, LDir)*Params[pDiffAmount]*8), 0)+
         Max(Round(Power(VectorDotProduct(Normal, H), Params[pSpecPower])*Params[pDiffAmount]*8), 0);
      Regs[0]^[Pix]:=MulClamp(MulClamp(Regs[0]^[Pix], I), Params[pAmplify]*32);
    end;
  Result:=true;
end;

function TSynTexFilters.FiltNormals(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
var
  Amount: Byte;
  A: Single;
  X, Y, Pix: Integer;
  Normal1, Normal2: TVector3D;
begin
  Result:=CheckRemain(Parameters, SizeOf(Amount){$IFDEF SYNTEX_USELOG}, 'Filter:Normals'{$ENDIF}) and (RegsCount=2);
  if not Result then Exit;
  try
    Parameters.Read(Amount, SizeOf(Amount));
    A:=Amount/512;
    for X:=0 to FSynTex.TexSize-1 do
      for Y:=0 to FSynTex.TexSize-1 do
      begin
        Pix:=PixelIndex(X, Y, true, true);
        Normal1.X:=A*(Regs[1]^[Pix].R-Regs[1]^[PixelIndex(X+1, Y, false, false)].R);
        Normal1.Y:=A*(Regs[1]^[Pix].R-Regs[1]^[PixelIndex(X, Y+1, false, false)].R);
        Normal1.Z:=1;
        Normal2.X:=A*(Regs[1]^[PixelIndex(X-1, Y, false, false)].R-Regs[1]^[Pix].R);
        Normal2.Y:=A*(Regs[1]^[PixelIndex(X, Y-1, false, false)].R-Regs[1]^[Pix].R);
        Normal2.Z:=1;
        Normal1:=VectorAdd(Normal1, Normal2);
        VectorNormalize(Normal1);
        Regs[0]^[Pix].R:=ClampVal(Round(Normal1.X*128+128), 256);
        Regs[0]^[Pix].G:=ClampVal(Round(Normal1.Y*128+128), 256);
        Regs[0]^[Pix].B:=ClampVal(Round(Normal1.Z*128+128), 256);
      end;
  except
    Result:=false;
  end;
end;

function TSynTexFilters.ClampVal(Val, Max: Integer): Integer;
begin
  if Val<0
    then Result:=0
    else if Val>=Max then Result:=Max-1
      else Result:=Val;
end;

function TSynTexFilters.WrapVal(Val, Max: Integer): Integer;
begin
  Val:=Val mod Max;
  if Val<0
    then Result:=Max+Val
    else Result:=Val; 
end;

function TSynTexFilters.PixelIndex(X, Y: Integer; ClampX, ClampY: Boolean): Integer;
begin
  if ClampX
    then X:=ClampVal(X, FSynTex.TexSize)
    else X:=WrapVal(X, FSynTex.TexSize);
  if ClampY
    then Y:=ClampVal(Y, FSynTex.TexSize)
    else Y:=WrapVal(Y, FSynTex.TexSize);
  Result:=Y*FSynTex.TexSize+X;
end;

{function TSynTexFilters.GetPixel(Reg: PSynTexRegister; X, Y: Integer; ClampX, ClampY: Boolean): TRGBA;
begin
  Result:=Reg^[PixelIndex(X, Y, ClampX, ClampY)];
end;

procedure TSynTexFilters.SetPixel(Reg: PSynTexRegister; X, Y: Integer; ClampX, ClampY: Boolean; Pixel: TRGBA);
begin
  Reg^[PixelIndex(X, Y, ClampX, ClampY)]:=Pixel;
end; }

function TSynTexFilters.BlendColors(Color1, Color2: TRGBA; Blend: Byte): TRGBA;
begin
  Color1:=Mul(Color1, $FF-Blend);
  Color2:=Mul(Color2, Blend);
  Result:=AddClamp(Color1, Color2);
end;

function TSynTexFilters.BlendColors(Color1, Color2: TRGBA; Blend: TRGBA): TRGBA;
const
  White: TRGBA=(R: $FF; G: $FF; B: $FF; A: $FF);
begin
  Color1:=Mul(Color1, SubClamp(White, Blend));
  Color2:=Mul(Color2, Blend);
  Result:=AddClamp(Color1, Color2);
end;

function TSynTexFilters.Add(Color1, Color2: TRGBA): TRGBA;
begin
  Result.R:=Color1.R+Color2.R;
  Result.G:=Color1.G+Color2.G;
  Result.B:=Color1.B+Color2.B;
  Result.A:=Color1.A+Color2.A;
end;

function TSynTexFilters.Sub(Color1, Color2: TRGBA): TRGBA;
begin
  Result.R:=Color1.R-Color2.R;
  Result.G:=Color1.G-Color2.G;
  Result.B:=Color1.B-Color2.B;
  Result.A:=Color1.A-Color2.A;
end;

function TSynTexFilters.AddClamp(Color1, Color2: TRGBA): TRGBA;
begin
  Result.R:=ClampVal(Word(Color1.R+Color2.R), 256);
  Result.G:=ClampVal(Word(Color1.G+Color2.G), 256);
  Result.B:=ClampVal(Word(Color1.B+Color2.B), 256);
  Result.A:=ClampVal(Word(Color1.A+Color2.A), 256);
end;

function TSynTexFilters.SubClamp(Color1, Color2: TRGBA): TRGBA;
begin
  Result.R:=ClampVal(SmallInt(Color1.R-Color2.R), 256);
  Result.G:=ClampVal(SmallInt(Color1.G-Color2.G), 256);
  Result.B:=ClampVal(SmallInt(Color1.B-Color2.B), 256);
  Result.A:=ClampVal(SmallInt(Color1.A-Color2.A), 256);
end;

function TSynTexFilters.Diff(Color1, Color2: TRGBA): TRGBA;
begin
  Result.R:=ClampVal(SmallInt(128+Color1.R-Color2.R), 256);
  Result.G:=ClampVal(SmallInt(128+Color1.G-Color2.G), 256);
  Result.B:=ClampVal(SmallInt(128+Color1.B-Color2.B), 256);
  Result.A:=ClampVal(SmallInt(128+Color1.A-Color2.A), 256);
end;

function TSynTexFilters.Mul(Color1, Color2: TRGBA): TRGBA;
begin
  Result.R:=Word(Color1.R*Color2.R) shr 8;
  Result.G:=Word(Color1.G*Color2.G) shr 8;
  Result.B:=Word(Color1.B*Color2.B) shr 8;
  Result.A:=Word(Color1.A*Color2.A) shr 8;
end;

function TSynTexFilters.Mul(Color: TRGBA; Scale: Byte): TRGBA;
begin
  Result.R:=Word(Color.R*Scale) shr 8;
  Result.G:=Word(Color.G*Scale) shr 8;
  Result.B:=Word(Color.B*Scale) shr 8;
  Result.A:=Word(Color.A*Scale) shr 8;
end;

function TSynTexFilters.MulClamp(Color: TRGBA; Scale: Integer): TRGBA;
begin
  Result.R:=ClampVal(Color.R*Scale shr 8, 256);
  Result.G:=ClampVal(Color.G*Scale shr 8, 256);
  Result.B:=ClampVal(Color.B*Scale shr 8, 256);
  Result.A:=ClampVal(Color.A*Scale shr 8, 256);
end;

end.
