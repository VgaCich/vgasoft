program FontGen;

uses Windows, Messages, AvL, avlUtils, avlMath;

{$R *.res}
{$R manifest.res}

function ChooseFont(var ChooseFont: TChooseFont): Bool; stdcall; external 'comdlg32.dll' name 'ChooseFontA';

type
  TTexPaint=class(TImage)
  protected
    FTexture: TBitmap;
    procedure WMPaint(var AMsg: TWMPaint); override;
  public
    constructor Create(AParent: TWinControl; Texture: TBitmap);
  end;
  TMainForm=class(TForm)
  private
    LTexSize, LTexSizeFrom, LTexSizeTo, LDepth, LFont: TLabel;
    BSave, BSelectFont: TButton;
    CBTexSizeFrom, CBTexSizeTo, CBDepth: TComboBox;
    TexPaint: TTexPaint;
    FTextures: array[0..3] of TBitmap;
    FCurFont: TLogFont;
    FFontWidth: array[0..255] of Byte;
  public
    constructor Create;
    procedure FormDestroy(Sender: TObject);
    procedure Generate;
    procedure TexSizeFromChange(Sender: TObject);
    procedure TexSizeToChange(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SelectFontClick(Sender: TObject);
  end;
  TFNTHdr=packed record
    ID: Cardinal;
    Width, Height: Word;
    Bits, MipLevels: Byte;
    FontWidth: packed array[0..255] of Byte;
  end;
  PByteArray=^TByteArray;
  TByteArray=array[0..MaxInt-1] of Byte;

const
  FNTID=$4E465356;
  TexSizes: array[0..3] of Integer=(128, 256, 512, 1024);

constructor TTexPaint.Create(AParent: TWinControl; Texture: TBitmap);
begin
  inherited Create(AParent);
  FTexture:=Texture;
end;

procedure TTexPaint.WMPaint(var AMsg: TWMPaint);
begin
  Dispatch(AMsg);
  FTexture.Draw(Canvas.Handle, 0, 0);
end;

constructor TMainForm.Create;
var
  i: Integer;
begin
  inherited Create(nil, 'VSE FontGen');
  BorderStyle:=bsSingle;
  BorderIcons:=[];
  SetSize(370+Width-ClientWidth, 256+Height-ClientHeight);
  OnDestroy:=FormDestroy;
  ZeroMemory(@FCurFont, SizeOf(FCurFont));
  with FCurFont do
  begin
    lstrcpy(@lfFaceName, 'Arial');
    lfHeight:=-11;
    lfWeight:=FW_BOLD;
    lfCharSet:=1;
    lfPitchAndFamily:=FF_DONTCARE
  end;
  LTexSize:=TLabel.Create(Self, 'Texture size:');
  LTexSize.SetBounds(260, 0, 105, 15);
  LTexSizeFrom:=TLabel.Create(Self, 'From:');
  LTexSizeFrom.SetBounds(260, 20, 30, 15);
  LTexSizeTo:=TLabel.Create(Self, 'To:');
  LTexSizeTo.SetBounds(260, 45, 30, 15);
  LDepth:=TLabel.Create(Self, 'Texture depth:');
  LDepth.SetBounds(260, 65, 105, 15);
  LFont:=TLabel.Create(Self, 'Font: '+FCurFont.lfFaceName);
  LFont.SetBounds(260, 105, 105, 15);
  BSave:=TButton.Create(Self, 'Save');
  BSave.SetPosition(290, 225);
  BSave.OnClick:=SaveClick;
  BSelectFont:=TButton.Create(Self, 'Select');
  BSelectFont.SetPosition(260, 120);
  BSelectFont.OnClick:=SelectFontClick;
  CBTexSizeFrom:=TComboBox.Create(Self, csDropDownList);
  CBTexSizeFrom.SetBounds(290, 15, 75, 24);
  CBTexSizeFrom.OnChange:=TexSizeFromChange;
  CBTexSizeTo:=TComboBox.Create(Self, csDropDownList);
  CBTexSizeTo.SetBounds(290, 40, 75, 24);
  CBTexSizeTo.OnChange:=TexSizeToChange;
  for i:=0 to 3 do
  begin
    CBTexSizeFrom.ItemAdd(IntToStr(TexSizes[i])+'x'+IntToStr(TexSizes[i]));
    if i>0 then CBTexSizeTo.ItemAdd(IntToStr(TexSizes[i])+'x'+IntToStr(TexSizes[i]));
    FTextures[i]:=TBitmap.CreateNew(TexSizes[i], TexSizes[i]);
  end;
  CBTexSizeFrom.ItemIndex:=1;
  CBTexSizeTo.ItemIndex:=1;
  CBDepth:=TComboBox.Create(Self, csDropDownList);
  CBDepth.SetBounds(260, 80, 105, 24);
  CBDepth.ItemAdd('1');
  CBDepth.ItemAdd('2');
  CBDepth.ItemAdd('4');
  CBDepth.ItemAdd('8');
  CBDepth.ItemIndex:=0;
  TexPaint:=TTexPaint.Create(Self, FTextures[1]);
  TexPaint.SetBounds(0, 0, 256, 256);
  TexPaint.CanvasInit;
  Generate;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to 3 do
    FAN(FTextures[i]);
end;

procedure TMainForm.Generate;
var
  i, Sz: Integer;
  j: Byte;
  CS: TSize;
  Font, OldFont: HFONT;
begin
  for i:=0 to 3 do
  begin
    FTextures[i].Width:=TexSizes[i];
    FTextures[i].Height:=TexSizes[i];
    FTextures[i].Canvas.Brush.Color:=clWhite;
    FTextures[i].Canvas.FillRect(Rect(0, 0, TexSizes[i], TexSizes[i]));
    Sz:=TexSizes[i] div 16;
    FCurFont.lfHeight:=Sz;
    Font:=CreateFontIndirect(FCurFont);
    OldFont:=SelectObject(FTextures[i].Canvas.Handle, Font);
    for j:=0 to 255 do
    begin
      GetTextExtentPoint(FTextures[i].Canvas.Handle, @string(Chr(j))[1], 1, CS);
      FTextures[i].Canvas.TextOut(j mod 16*Sz+(Sz-CS.cx) div 2, j div 16*Sz, Chr(j));
      if i=3 then FFontWidth[j]:=Ceil(CS.cx/4);
    end;
    if OldFont<>0 then SelectObject(FTextures[i].Canvas.Handle, OldFont);
    DeleteObject(Font);
  end;
  TexPaint.Invalidate;
end;

procedure TMainForm.TexSizeFromChange(Sender: TObject);
begin
  if CBTexSizeTo.ItemIndex<CBTexSizeFrom.ItemIndex-1
    then CBTexSizeTo.ItemIndex:=CBTexSizeFrom.ItemIndex-1;
end;

procedure TMainForm.TexSizeToChange(Sender: TObject);
begin
  if CBTexSizeFrom.ItemIndex>CBTexSizeTo.ItemIndex+1
    then CBTexSizeFrom.ItemIndex:=CBTexSizeTo.ItemIndex+1;
end;

procedure TMainForm.SaveClick(Sender: TObject);

  function GetTexel(X, Y, Lev: Integer): Byte;
  var
    Pixel: Integer;
    Texel: array[0..3] of Byte absolute Pixel;
  begin
    Pixel:=FTextures[Lev].Canvas.Pixels[X, Y];
    Result:=Round(0.299*Texel[0]+0.587*Texel[1]+0.114*Texel[2]);
  end;

var
  Hdr: TFNTHdr;
  OFile: TFileStream;
  i, Lev, Sz, W, H: Integer;
  Buffer: PByteArray;
  FN: string;
begin
  FN:='';
  if not OpenSaveDialog(Handle, false, '', '.vfn', 'VSE Fonts (*.vfn)|*.vfn|All files|*.*',
    '', 0, OFN_OVERWRITEPROMPT or OFN_PATHMUSTEXIST, FN) then Exit;
  Hdr.ID:=FNTID;
  Hdr.Width:=TexSizes[CBTexSizeTo.ItemIndex+1];
  Hdr.Height:=Hdr.Width;
  Hdr.Bits:=StrToInt(CBDepth.Text);
  Hdr.MipLevels:=CBTexSizeTo.ItemIndex+2-CBTexSizeFrom.ItemIndex;
  for i:=0 to 255 do Hdr.FontWidth[i]:=FFontWidth[i];
  try
    if FileExists(FN) then DeleteFile(FN);
    OFile:=TFileStream.Create(FN, fmCreate);
    OFile.Write(Hdr, SizeOf(Hdr));
    for Lev:=CBTexSizeTo.ItemIndex+1 downto CBTexSizeFrom.ItemIndex do
    begin
      Sz:=TexSizes[Lev]*TexSizes[Lev]*Hdr.Bits div 8;
      GetMem(Buffer, Sz);
      for i:=0 to Sz-1 do
      case Hdr.Bits of
        1: Buffer[i]:=((GetTexel(8*i mod TexSizes[Lev], TexSizes[Lev]-(8*i div TexSizes[Lev]), Lev) div 128) shl 7) or
                      ((GetTexel((8*i+1) mod TexSizes[Lev], TexSizes[Lev]-((8*i+1) div TexSizes[Lev]), Lev) div 128) shl 6) or
                      ((GetTexel((8*i+2) mod TexSizes[Lev], TexSizes[Lev]-((8*i+2) div TexSizes[Lev]), Lev) div 128) shl 5) or
                      ((GetTexel((8*i+3) mod TexSizes[Lev], TexSizes[Lev]-((8*i+3) div TexSizes[Lev]), Lev) div 128) shl 4) or
                      ((GetTexel((8*i+4) mod TexSizes[Lev], TexSizes[Lev]-((8*i+4) div TexSizes[Lev]), Lev) div 128) shl 3) or
                      ((GetTexel((8*i+5) mod TexSizes[Lev], TexSizes[Lev]-((8*i+5) div TexSizes[Lev]), Lev) div 128) shl 2) or
                      ((GetTexel((8*i+6) mod TexSizes[Lev], TexSizes[Lev]-((8*i+6) div TexSizes[Lev]), Lev) div 128) shl 1) or
                       (GetTexel((8*i+7) mod TexSizes[Lev], TexSizes[Lev]-((8*i+7) div TexSizes[Lev]), Lev) div 128);
        2: Buffer[i]:=((GetTexel(4*i mod TexSizes[Lev], TexSizes[Lev]-(4*i div TexSizes[Lev]), Lev) div 64) shl 6) or
                      ((GetTexel((4*i+1) mod TexSizes[Lev], TexSizes[Lev]-((4*i+1) div TexSizes[Lev]), Lev) div 64) shl 4) or
                      ((GetTexel((4*i+2) mod TexSizes[Lev], TexSizes[Lev]-((4*i+2) div TexSizes[Lev]), Lev) div 64) shl 2) or
                       (GetTexel((4*i+3) mod TexSizes[Lev], TexSizes[Lev]-((4*i+3) div TexSizes[Lev]), Lev) div 64);
        4: Buffer[i]:=((GetTexel(2*i mod TexSizes[Lev], TexSizes[Lev]-(2*i div TexSizes[Lev]), Lev) div 16) shl 4) or
                       (GetTexel((2*i+1) mod TexSizes[Lev], TexSizes[Lev]-((2*i+1) div TexSizes[Lev]), Lev) div 16);
        8: Buffer[i]:=GetTexel(i mod TexSizes[Lev], TexSizes[Lev]-(i div TexSizes[Lev]), Lev);
      end;
      OFile.Write(Buffer[0], Sz);
    end;
  finally
    FreeMem(Buffer, Sz);
    FAN(OFile);
  end;       
end;

procedure TMainForm.SelectFontClick(Sender: TObject);
var
  CFont: TChooseFont;
begin
  ZeroMemory(@CFont, SizeOf(CFont)); 
  CFont.lStructSize:=SizeOf(CFont);
  CFont.hWndOwner:=Handle;
  CFont.hDC:=0;
  CFont.lpLogFont:=@FCurFont;
  CFont.hInstance:=hInstance;
  CFont.Flags:=CF_FORCEFONTEXIST or CF_SCREENFONTS or CF_INITTOLOGFONTSTRUCT or CF_EFFECTS;
  if not ChooseFont(CFont) then Exit;
  LFont.Caption:='Font: '+FCurFont.lfFaceName;
  Generate;
end;

var
  MainForm: TMainForm;

begin
  InitCommonControls;
  MainForm:=TMainForm.Create;
  MainForm.Run;
end.