unit SynTexFilters;

interface

uses
  Windows, AvL, avlUtils, SynTex, Noise, avlVectors, avlMath;

type
  TTransformFunc=function(X, Y: Integer): Integer; //returns 1D coordinate in register
  TSynTexFilters=class
  protected
    FSynTex: TSynTex;
    function  WrapVal(Val, Max: Integer; Clamp: Boolean): Integer;
    function  PixelIndex(X, Y: Integer; ClampX, ClampY: Boolean): Integer;
    function  GetPixel(Reg: PSynTexRegister; X, Y: Single; ClampX, ClampY, LERP: Boolean): TRGBA;
    function  BlendColors(Color1, Color2: TRGBA; Blend: Byte): TRGBA; overload;
    function  BlendColors(Color1, Color2: TRGBA; Blend: TRGBA): TRGBA; overload;
    function  Add(Color1, Color2: TRGBA; Clamp: Boolean): TRGBA;
    function  Sub(Color1, Color2: TRGBA; Clamp: Boolean): TRGBA;
    function  Diff(Color1, Color2: TRGBA): TRGBA;
    function  Mul(Color1, Color2: TRGBA): TRGBA; overload;
    function  Mul(Color: TRGBA; Scale: Byte): TRGBA; overload;
    function  Mul(Color: TRGBA; Scale: Integer; Clamp: Boolean): TRGBA; overload;
    procedure Convolution1D(Dst, Src: PSynTexRegister; Kernel: array of Single; Amp: Single; Horz, ClampX, ClampY: Boolean);
    function  BlurFilterFlat(D: Single): Single;
    function  BlurFilterLinear(D: Single): Single;
    function  BlurFilterGauss(D: Single): Single;
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
    function FiltGlowRect(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltDistort(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltTransform(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltBlur(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltColorRange(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
    function FiltAdjust(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
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
  SynTex.AddFilter(FLT_GLOWRECT, FiltGlowRect);
  SynTex.AddFilter(FLT_DISTORT, FiltDistort);
  SynTex.AddFilter(FLT_TRANSFORM, FiltTransform);
  SynTex.AddFilter(FLT_BLUR, FiltBlur);
  SynTex.AddFilter(FLT_COLORRANGE, FiltColorRange);
  SynTex.AddFilter(FLT_ADJUST, FiltAdjust);
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
      ADDMODE_ADDCLAMP: for j:=2 to RegsCount do Clr:=Add(Clr, Regs[j]^[i], true);
      ADDMODE_ADDWRAP: for j:=2 to RegsCount do Clr:=Add(Clr, Regs[j]^[i], false);
      ADDMODE_SUBCLAMP: for j:=2 to RegsCount do Clr:=Sub(Clr, Regs[j]^[i], true);
      ADDMODE_SUBWRAP: for j:=2 to RegsCount do Clr:=Sub(Clr, Regs[j]^[i], false);
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
        Regs[0]^[Y*FSynTex.TexSize+X]:=BlendColors(Clrs[0], Clrs[1], WrapVal(Trunc(128+128*Val), 256, true));
      end;
  finally
    for i:=0 to Octaves do PN[i].Free;
  end;
  Result:=true;
end;

function TSynTexFilters.FiltBump(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
type
  TParams=packed record
    Phi, Theta, Amplify, DiffAmount, SpecAmount, SpecPower: Byte;
  end;
const
  View: TVector3D = (X: 0; Y: 0; Z: 1);
var
  X, Y: Integer;
  LDir, H, Normal: TVector3D;
  Pix: Integer;
  I: Integer;
  Params: TParams;
begin
  Result:=false;
  if RegsCount<>2 then Exit;
  if not CheckRemain(Parameters, SizeOf(Params){$IFDEF SYNTEX_USELOG}, 'Filter:Bump'{$ENDIF}) then Exit;
  Parameters.Read(Params, SizeOf(Params));
  with Params, LDir do
  begin
    X:=cos(Phi*BDegToRad)*sin(Theta*BDegToRad/2);
    Y:=sin(Phi*BDegToRad)*sin(Theta*BDegToRad/2);
    Z:=cos(Theta*BDegToRad/2);
  end;
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
      I:=Max(Round(VectorDotProduct(Normal, LDir)*Params.DiffAmount*8), 0)+
         Max(Round(Power(VectorDotProduct(Normal, H), Params.SpecPower)*Params.DiffAmount*8), 0);
      Regs[0]^[Pix]:=Mul(Mul(Regs[0]^[Pix], I, true), Params.Amplify*32, true);
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
        Regs[0]^[Pix].R:=WrapVal(Round(Normal1.X*128+128), 256, true);
        Regs[0]^[Pix].G:=WrapVal(Round(Normal1.Y*128+128), 256, true);
        Regs[0]^[Pix].B:=WrapVal(Round(Normal1.Z*128+128), 256, true);
      end;
  except
    Result:=false;
  end;
end;

function TSynTexFilters.FiltGlowRect(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
type
  TParams=(pPosX, pPosY, pRadX, pRadY, pSizeX, pSizeY, pBlend, pPower);
var
  X, Y, i, Pix: Integer;
  ShiftX, ShiftY: Integer;
  Shift: Single;
  Blend: Byte;
  Color: TRGBA;
  Params: packed array[TParams] of Word;
  Rect: TRect;
begin
  Result:=false;
  if not CheckRemain(Parameters, SizeOf(Params)+SizeOf(Color){$IFDEF SYNTEX_USELOG}, 'Filter:GlowRect'{$ENDIF}) then Exit;
  Parameters.Read(Params, SizeOf(Params));
  Parameters.Read(Color, SizeOf(Color));
  for i:=Integer(pPosX) to Integer(pSizeY) do
    Params[TParams(i)]:=Round((Params[TParams(i)]/10000)*FSynTex.TexSize);
  with Rect, FSynTex do
  begin
    Left:=WrapVal(Params[pPosX]-Params[pSizeX], TexSize, true);
    Right:=WrapVal(Params[pPosX]+Params[pSizeX], TexSize, true);
    Top:=WrapVal(Params[pPosY]-Params[pSizeY], TexSize, true);
    Bottom:=WrapVal(Params[pPosY]+Params[pSizeY], TexSize, true);
  end;
  for X:=0 to FSynTex.TexSize-1 do
    for Y:=0 to FSynTex.TexSize-1 do
    begin
      Pix:=X+Y*FSynTex.TexSize;
      ShiftX:=Max(Rect.Left-X, X-Rect.Right);
      ShiftY:=Max(Rect.Top-Y, Y-Rect.Bottom);
      if Params[pRadX]>0
        then Shift:=Power(Max(ShiftX, 0)/Params[pRadX], 2)
        else Shift:=WrapVal(ShiftX, 2, true);
      if Params[pRadY]>0
        then Shift:=Shift+Power(Max(ShiftY, 0)/Params[pRadY], 2)
        else Shift:=Shift+WrapVal(ShiftY, 2, true);
      Shift:=Sqrt(Shift);
      if Shift<1
        then Blend:=WrapVal(Round(255*Power(Max(1-Shift, 0), Params[pPower]/1000)*Params[pBlend]/1000), 256, true)
        else Blend:=0;
      for i:=0 to RegsCount-1 do
        Regs[i]^[Pix]:=BlendColors(Regs[i]^[Pix], Color, Blend);
    end;
  Result:=true;
end;

function TSynTexFilters.FiltDistort(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
type
  TParams=packed record
    Amount, Flags: Byte;
  end;
var
  X, Y, Pix: Integer;
  Scale: Single;
  Params: TParams;
  ClampX, ClampY, LERP: Boolean;
begin
  Result:=false;
  if (RegsCount<>3) or not CheckRemain(Parameters, SizeOf(Params){$IFDEF SYNTEX_USELOG}, 'Filter:Distort'{$ENDIF}) then Exit;
  Parameters.Read(Params, SizeOf(Params));
  with Params do
  begin
    ClampX:=Flags and FlagClampX <> 0;
    ClampY:=Flags and FlagClampY <> 0;
    LERP:=Flags and FlagNoLERP = 0;
    Scale:=Amount/32;
  end;
  for X:=0 to FSynTex.TexSize-1 do
    for Y:=0 to FSynTex.TexSize-1 do
    begin
      Pix:=PixelIndex(X, Y, true, true);
      Regs[0]^[Pix]:=GetPixel(Regs[1], X+Scale*(Regs[2]^[Pix].R-128), Y+Scale*(Regs[2]^[Pix].G-128), ClampX, ClampY, LERP);
    end;
  Result:=true;
end;

function TSynTexFilters.FiltTransform(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
type
  TParams=packed record
    Angle: Byte;
    ZoomX, ZoomY: Word;
    ScrollX, ScrollY: Word;
    Flags: Byte;
  end;
var
  X, Y: Integer;
  Params: TParams;
  ClampX, ClampY, LERP: Boolean;
  S, C: Single;
  ScrX, ScrY, SclX, SclY: Single;
begin
  Result:=false;
  if (RegsCount<>2) or not CheckRemain(Parameters, SizeOf(Params){$IFDEF SYNTEX_USELOG}, 'Filter:Transform'{$ENDIF}) then Exit;
  Parameters.Read(Params, SizeOf(Params));
  with Params do
  begin
    ClampX:=Flags and FlagClampX <> 0;
    ClampY:=Flags and FlagClampY <> 0;
    LERP:=Flags and FlagNoLERP = 0;
    S:=sin(Angle*BDegToRad);
    C:=cos(Angle*BDegToRad);
    ScrX:=(ScrollX-32768)*(FSynTex.TexSize/10000);
    ScrY:=(ScrollY-32768)*(FSynTex.TexSize/10000);
    if ZoomX<>32768 then SclX:=1000/(ZoomX-32768) else SclX:=1;
    if ZoomY<>32768 then SclY:=1000/(ZoomY-32768) else SclY:=1;
  end;
  for X:=0 to FSynTex.TexSize-1 do
    for Y:=0 to FSynTex.TexSize-1 do
    begin
      Regs[0]^[PixelIndex(X, Y, true, true)]:=GetPixel(Regs[1],
        SclX*X*C-SclY*Y*S-ScrX, SclX*X*S+SclY*Y*C-ScrY, ClampX, ClampY, LERP);
    end;
  Result:=true;
end;

function TSynTexFilters.FiltBlur(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
type
  TParams=packed record
    SizeX, SizeY, Filter, Amp, Flags: Byte;
  end;
  TBlurFilter=function(D: Single): Single of object;

  procedure MakeKernel(var Kernel: array of Single; Size: Integer; Filter: TBlurFilter);
  var
    i: Integer;
    WeightsSum: Single;
  begin
    WeightsSum:=0;
    if Size=0
      then Kernel[0]:=1
    else begin
      for i:=0 to Size do
      begin
        Kernel[i]:=Filter(i/Size);
        WeightsSum:=WeightsSum+Kernel[i];
        if i>0 then WeightsSum:=WeightsSum+Kernel[i];
      end;
      for i:=0 to Size do Kernel[i]:=Kernel[i]/WeightsSum;
    end;
  end;

var
  Params: TParams;
  ClampX, ClampY: Boolean;
  Temp: TSynTexRegister;
  KernelX, KernelY: array of Single;
  BlurFilter: TBlurFilter;
begin
  Result:=false;
  if (RegsCount<>2) or not CheckRemain(Parameters, SizeOf(Params){$IFDEF SYNTEX_USELOG}, 'Filter:Blur'{$ENDIF}) then Exit;
  Parameters.Read(Params, SizeOf(Params));
  with Params do
  begin
    ClampX:=Flags and FlagClampX <> 0;
    ClampY:=Flags and FlagClampY <> 0;
    case Filter of
      BLURFILTER_FLAT: BlurFilter:=BlurFilterFlat;
      BLURFILTER_LINEAR: BlurFilter:=BlurFilterLinear;
      BLURFILTER_GAUSS: BlurFilter:=BlurFilterGauss;
      else BlurFilter:=BlurFilterGauss;
    end;
    try
      SetLength(KernelX, SizeX+1);
      MakeKernel(KernelX, SizeX, BlurFilter);
      SetLength(KernelY, SizeY+1);
      MakeKernel(KernelY, SizeY, BlurFilter);
      SetLength(Temp, FSynTex.TexSize*FSynTex.TexSize);
      Convolution1D(@Temp, Regs[1], KernelX, Params.Amp/8, true, ClampX, ClampY);
      Convolution1D(Regs[0], @Temp, KernelY, Params.Amp/8, false, ClampX, ClampY);
    finally
      Finalize(Temp);
      Finalize(KernelX);
      Finalize(KernelY);
    end;
  end;
  Result:=true;
end;

function TSynTexFilters.FiltColorRange(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
var
  Clrs: packed array[0..1] of TRGBA;
  i: Integer;
begin
  Result:=(RegsCount=2) and CheckRemain(Parameters, SizeOf(Clrs){$IFDEF SYNTEX_USELOG}, 'Filter:ColorRange'{$ENDIF});
  if not Result then Exit;
  try
    Parameters.Read(Clrs[0], SizeOf(Clrs));
    for i:=0 to FSynTex.TexSize*FSynTex.TexSize-1 do
      Regs[0]^[i]:=BlendColors(Clrs[0], Clrs[1], Regs[1]^[i]);
  except
    Result:=false;
  end;
end;

function TSynTexFilters.FiltAdjust(Regs: array of PSynTexRegister; RegsCount: Integer; Parameters: TStream): Boolean;
type
  TParams=packed record
    Brightness, Contrast, Gamma: Byte;
  end;
var
  Params: TParams;
  i: Integer;
begin
  Result:=(RegsCount=2) and CheckRemain(Parameters, SizeOf(Params){$IFDEF SYNTEX_USELOG}, 'Filter:Adjust'{$ENDIF});
  if not Result then Exit;
  try
    Parameters.Read(Params, SizeOf(Params));
    for i:=0 to FSynTex.TexSize*FSynTex.TexSize-1 do
      with Params do
      begin
        Regs[0]^[i].R:=WrapVal(Round(Brightness+8*Contrast*(2*Power(Regs[1]^[i].R/255, Gamma/64)-1)), 256, true);
        Regs[0]^[i].G:=WrapVal(Round(Brightness+8*Contrast*(2*Power(Regs[1]^[i].G/255, Gamma/64)-1)), 256, true);
        Regs[0]^[i].B:=WrapVal(Round(Brightness+8*Contrast*(2*Power(Regs[1]^[i].B/255, Gamma/64)-1)), 256, true);
        Regs[0]^[i].A:=WrapVal(Round(Brightness+8*Contrast*(2*Power(Regs[1]^[i].A/255, Gamma/64)-1)), 256, true);
      end;
  except
    Result:=false;
  end;
end;

function TSynTexFilters.WrapVal(Val, Max: Integer; Clamp: Boolean): Integer;
begin
  if Clamp then
  begin
    if Val<0
      then Result:=0
      else if Val>=Max then Result:=Max-1
        else Result:=Val;
  end
  else begin
    Val:=Val mod Max;
    if Val<0
      then Result:=Max+Val
      else Result:=Val;
  end;
end;

function TSynTexFilters.PixelIndex(X, Y: Integer; ClampX, ClampY: Boolean): Integer;
begin
  X:=WrapVal(X, FSynTex.TexSize, ClampX);
  Y:=WrapVal(Y, FSynTex.TexSize, ClampY);
  Result:=Y*FSynTex.TexSize+X;
end;

function TSynTexFilters.GetPixel(Reg: PSynTexRegister; X, Y: Single; ClampX, ClampY, LERP: Boolean): TRGBA;
var
  Pix0, Pix1: TRGBA;
  X0, X1, Y0, Y1: Integer;
  BlendX: Byte;
begin
  if LERP then
  begin
    X0:=Floor(X);
    X1:=X0+1;
    Y0:=Floor(Y);
    Y1:=Y0+1;
    BlendX:=WrapVal(Round((X-X0)*255), 256, true);
    Pix0:=BlendColors(Reg^[PixelIndex(X0, Y0, ClampX, ClampY)], Reg^[PixelIndex(X1, Y0, ClampX, ClampY)], BlendX);
    Pix1:=BlendColors(Reg^[PixelIndex(X0, Y1, ClampX, ClampY)], Reg^[PixelIndex(X1, Y1, ClampX, ClampY)], BlendX);
    Result:=BlendColors(Pix0, Pix1, WrapVal(Round((Y-Y0)*255), 256, true));
  end
    else Result:=Reg^[PixelIndex(Round(X), Round(Y), ClampX, ClampY)];
end;

function TSynTexFilters.BlendColors(Color1, Color2: TRGBA; Blend: Byte): TRGBA;
begin
  Color1:=Mul(Color1, $FF-Blend);
  Color2:=Mul(Color2, Blend);
  Result:=Add(Color1, Color2, true);
end;

function TSynTexFilters.BlendColors(Color1, Color2: TRGBA; Blend: TRGBA): TRGBA;
const
  White: TRGBA=(R: $FF; G: $FF; B: $FF; A: $FF);
begin
  Color1:=Mul(Color1, Sub(White, Blend, true));
  Color2:=Mul(Color2, Blend);
  Result:=Add(Color1, Color2, true);
end;

function TSynTexFilters.Add(Color1, Color2: TRGBA; Clamp: Boolean): TRGBA;
begin
  Result.R:=WrapVal(Word(Color1.R+Color2.R), 256, Clamp);
  Result.G:=WrapVal(Word(Color1.G+Color2.G), 256, Clamp);
  Result.B:=WrapVal(Word(Color1.B+Color2.B), 256, Clamp);
  Result.A:=WrapVal(Word(Color1.A+Color2.A), 256, Clamp);
end;

function TSynTexFilters.Sub(Color1, Color2: TRGBA; Clamp: Boolean): TRGBA;
begin
  Result.R:=WrapVal(SmallInt(Color1.R-Color2.R), 256, Clamp);
  Result.G:=WrapVal(SmallInt(Color1.G-Color2.G), 256, Clamp);
  Result.B:=WrapVal(SmallInt(Color1.B-Color2.B), 256, Clamp);
  Result.A:=WrapVal(SmallInt(Color1.A-Color2.A), 256, Clamp);
end;

function TSynTexFilters.Diff(Color1, Color2: TRGBA): TRGBA;
begin
  Result.R:=WrapVal(SmallInt(128+Color1.R-Color2.R), 256, true);
  Result.G:=WrapVal(SmallInt(128+Color1.G-Color2.G), 256, true);
  Result.B:=WrapVal(SmallInt(128+Color1.B-Color2.B), 256, true);
  Result.A:=WrapVal(SmallInt(128+Color1.A-Color2.A), 256, true);
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

function TSynTexFilters.Mul(Color: TRGBA; Scale: Integer; Clamp: Boolean): TRGBA;
begin
  Result.R:=WrapVal(Color.R*Scale shr 8, 256, Clamp);
  Result.G:=WrapVal(Color.G*Scale shr 8, 256, Clamp);
  Result.B:=WrapVal(Color.B*Scale shr 8, 256, Clamp);
  Result.A:=WrapVal(Color.A*Scale shr 8, 256, Clamp);
end;

procedure TSynTexFilters.Convolution1D(Dst, Src: PSynTexRegister; Kernel: array of Single; Amp: Single; Horz, ClampX, ClampY: Boolean);
const
  DirSel: array[Boolean] of record X, Y: Byte; end = ((X: 0; Y: 1), (X: 1; Y:0));
  DirSign: array[0..1] of ShortInt = (-1, 1);
var
  X, Y, i, j, Pix, SPix: Integer;
  R, G, B, A: Single;
begin
  for X:=0 to FSynTex.TexSize-1 do
    for Y:=0 to FSynTex.TexSize-1 do
    begin
      Pix:=PixelIndex(X, Y, ClampX, ClampY);
      R:=Kernel[0]*Src^[Pix].R;
      G:=Kernel[0]*Src^[Pix].G;
      B:=Kernel[0]*Src^[Pix].B;
      A:=Kernel[0]*Src^[Pix].A;
      for i:=1 to High(Kernel) do
        for j:=0 to 1 do
        begin
          SPix:=PixelIndex(X+i*DirSel[Horz].X*DirSign[j], Y+i*DirSel[Horz].Y*DirSign[j], ClampX, ClampY);
          R:=R+Kernel[i]*Src^[SPix].R;
          G:=G+Kernel[i]*Src^[SPix].G;
          B:=B+Kernel[i]*Src^[SPix].B;
          A:=A+Kernel[i]*Src^[SPix].A;
        end;
      Dst^[Pix].R:=WrapVal(Round(R*Amp), 256, true);
      Dst^[Pix].G:=WrapVal(Round(G*Amp), 256, true);
      Dst^[Pix].B:=WrapVal(Round(B*Amp), 256, true);
      Dst^[Pix].A:=WrapVal(Round(A*Amp), 256, true);
    end;
end;

function TSynTexFilters.BlurFilterFlat(D: Single): Single;
begin
  Result:=1;
end;

function TSynTexFilters.BlurFilterLinear(D: Single): Single;
begin
  Result:=1-D;
end;

function TSynTexFilters.BlurFilterGauss(D: Single): Single;
begin
  Result:=Exp(-3*D*D);
end;

end.
