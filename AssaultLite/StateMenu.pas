unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, VSEImageCodec,
  VSEGameStates, VSEGUI, VSETexMan, VSEMemPak, VSEBindMan, StateGame, StateLoad;

type
  TStateMenu=class;
  TMainMenu=class(TGUIForm)
  protected
    FParent: TStateMenu;
    FResumeButton: Integer;
    procedure GameClick(Btn: PBtn);
    procedure OptionsClick(Btn: PBtn);
    procedure TextClick(Btn: PBtn);
    procedure ExitClick(Btn: PBtn);
  public
    constructor Create(Parent: TStateMenu);
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure ResumeEnable(Enable: Boolean);
  end;
  TOptions=class(TGUIForm)
  protected
    FParent: TStateMenu;
    FLResolution, FLRefreshRate, FLColorDepth, FLCacheSize, FCFullscreen, FCVSync, FCEnableBGM, FBToggleCache,
      FBClearCache, FCurrentResolution, FCurrentRefreshRate, FColorDepth: Integer;
    FResolutions: TResolutions;
    procedure DrawForm; override;
    procedure ResClick(Btn: PBtn);
    procedure RefrClick(Btn: PBtn);
    procedure DepthClick(Btn: PBtn);
    procedure ToggleCache(Btn: PBtn);
    procedure ClearCache(Btn: PBtn);
    procedure KeyConfig(Btn: PBtn);
    procedure OKClick(Btn: PBtn);
    procedure CancelClick(Btn: PBtn);
  public
    constructor Create(Parent: TStateMenu);
    destructor Destroy; override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure ReadOptions;
  end;
  TTextView=class(TGUIForm)
  protected
    FParent: TStateMenu;
    FCurPage, FPages, FLPage: Integer;
    FText: TStringList;
    procedure DrawForm; override;
    procedure ChangePage(Btn: PBtn);
    procedure Close(Btn: PBtn);
  public
    constructor Create(Parent: TStateMenu; const Caption, TextFile: string);
    destructor Destroy; override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
  end;
  TStateMenu=class(TGameState)
  private
    FMainMenu: TMainMenu;
    FOptions: TOptions;
    FKeyConfig: TBindManCfgForm;
    FCurFrm: TGUIForm;
    FGame: TStateGame;
    FLoad: TStateLoad;
    FBgTex: Cardinal;
    procedure KeyConfigClose(Sender: TObject);
    {$IFDEF VSE_CONSOLE}
    function CacheHandler(Sender: TObject; Args: array of const): Boolean;
    function MenuBgHandler(Sender: TObject; Args: array of const): Boolean;
    {$ENDIF}
  protected
    function GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Key: Integer; Event: TKeyEvent); override;
    procedure CharEvent(C: Char); override;
    function  SysNotify(Notify: TSysNotify): Boolean; override;
  end;

implementation

uses VSEInit, VSECore, VSESound{$IFDEF VSE_CONSOLE}, VSEConsole{$ENDIF}{$IFDEF VSE_LOG}, VSELog{$ENDIF};

{TMainMenu}

var
  MainMenuItems: array[0..5] of TMenuItem = (
    (Caption: '����� ����'; Tag: 1),
    (Caption: '����������'; Tag: 0),
    (Caption: '���������'; Tag: 0),
    (Caption: '������'; Tag: 0),
    (Caption: '�������'; Tag: 1),
    (Caption: '�����'; Tag: 0));

constructor TMainMenu.Create(Parent: TStateMenu);
begin
  inherited Create(800, 600, 300, 130, 200, 350, TexMan.FontCreate('Arial', 12, true));
  FParent:=Parent;
  FCaption:='Assault Lite';
  MainMenuItems[0].OnClick:=GameClick;
  MainMenuItems[1].OnClick:=GameClick;
  MainMenuItems[2].OnClick:=OptionsClick;
  MainMenuItems[3].OnClick:=TextClick;
  MainMenuItems[4].OnClick:=TextClick;
  MainMenuItems[5].OnClick:=ExitClick;
  FResumeButton:=CreateMenu(Self, 30, 50, 140, 30, 20, MainMenuItems)[1];
  Button[FResumeButton].Enabled:=false;
end;

procedure TMainMenu.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key=VK_ESCAPE) and (Event=keUp)
    then if Self.Button[FResumeButton].Enabled
      then Core.SwitchState('Game')
      else Core.StopEngine
    else inherited KeyEvent(Key, Event);
end;

procedure TMainMenu.ResumeEnable(Enable: Boolean);
begin
  Button[FResumeButton].Enabled:=Enable;
end;

procedure TMainMenu.GameClick(Btn: PBtn);
begin
  if Btn.Tag=1 then
  begin
    FParent.FLoad.LevelName:='L0';
    Core.SwitchState('Load');
  end
    else Core.SwitchState('Game');
end;

procedure TMainMenu.OptionsClick(Btn: PBtn);
begin
  FParent.FOptions.ReadOptions;
  FParent.FCurFrm:=FParent.FOptions;
end;

procedure TMainMenu.TextClick(Btn: PBtn);
begin
  FParent.FCurFrm:=TTextView.Create(FParent, Btn.Caption, IntToStr(Btn.Tag)+'.txt');
end;

procedure TMainMenu.ExitClick(Btn: PBtn);
begin
  Core.StopEngine;
end;                 

{TOptions}

const
  CacheState: array[Boolean] of string = ('�������� ���', '��������� ���');
  SCacheSize='���: ';

constructor TOptions.Create(Parent: TStateMenu);
var
  Btn: TBtn;
  Lbl: TLbl;
begin
  inherited Create(800, 600, 200, 130, 400, 350, TexMan.FontCreate('Arial', 12, true));
  FParent:=Parent;
  FCaption:='���������';
  FResolutions:=gleGetResolutions;
  FLResolution:=CreateSelect(Self, 10, 60, 190, 20, ResClick, '-', '+');
  FLRefreshRate:=CreateSelect(Self, 10, 110, 190, 20, RefrClick, '-', '+');
  FLColorDepth:=CreateSelect(Self, 10, 160, 190, 20, DepthClick, '-', '+');
  with Btn do
  begin
    Enabled:=true;
    X:=220;
    Y:=60;
    Width:=160;
    Height:=30;
    Tag:=0;
    Type_:=btPush;
    OnClick:=ToggleCache;
    FBToggleCache:=AddButton(Btn);
    Y:=100;
    Caption:='�������� ���';
    OnClick:=ClearCache;
    FBClearCache:=AddButton(Btn);
    Y:=150;
    Caption:='����������';
    OnClick:=KeyConfig;
    AddButton(Btn);
    X:=10;
    Y:=200;
    Width:=200;
    Height:=20;
    Type_:=btCheck;
    OnClick:=nil;
    Caption:='������ �����';
    FCFullscreen:=AddButton(Btn);
    Y:=230;
    Caption:='����. �����.';
    FCVSync:=AddButton(Btn);
    X:=220;
    Y:=200;
    Width:=160;
    Caption:='������';
    FCEnableBGM:=AddButton(Btn);
    X:=140;
    Y:=310;
    Width:=120;
    Height:=30;
    Type_:=btPush;
    Caption:='���������';
    OnClick:=OKClick;
    AddButton(Btn);
    X:=270;
    Caption:='��������';
    OnClick:=CancelClick;
    AddButton(Btn);
  end;
  with Lbl do
  begin
    Align:=laCenter;
    Color:=Integer($FF00B200);
    X:=10;
    Y:=40;
    Width:=190;
    Caption:='����������';
    AddLabel(Lbl);
    Y:=90;
    Caption:='������� ����������';
    AddLabel(Lbl);
    Y:=140;
    Caption:='������� �����';
    AddLabel(Lbl);
    X:=220;
    Y:=40;
    Width:=160;
    Align:=laLeft;
    Caption:='';
    FLCacheSize:=AddLabel(Lbl);
  end;
end;

destructor TOptions.Destroy;
begin
  Finalize(FResolutions);
  inherited Destroy;
end;

procedure TOptions.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  if (Key=VK_ESCAPE) and (Event=keUp)
    then FParent.FCurFrm:=FParent.FMainMenu
    else inherited KeyEvent(Key, Event);
end;

procedure TOptions.ReadOptions;
var
  i: Integer;
begin
  Button[FCFullscreen].Checked:=Core.Fullscreen;
  Button[FCVSync].Checked:=Core.VSync;
  Button[FCEnableBGM].Checked:=Sound.EnableBGM;
  FColorDepth:=Core.ColorDepth;
  FCurrentResolution:=-1;
  for i:=0 to High(FResolutions) do
    if (FResolutions[i].Width=Core.ResolutionX) and (FResolutions[i].Height=Core.ResolutionY) then FCurrentResolution:=i;
  if FCurrentResolution=-1 then
  begin
    FCurrentResolution:=Length(FResolutions);
    SetLength(FResolutions, FCurrentResolution+1);
    with FResolutions[FCurrentResolution] do
    begin
      Width:=Core.ResolutionX;
      Height:=Core.ResolutionY;
      SetLength(RefreshRates, 1);
      RefreshRates[0]:=Core.RefreshRate;
    end;
  end;
  FCurrentRefreshRate:=-1;
  for i:=0 to High(FResolutions[FCurrentResolution].RefreshRates) do
    if FResolutions[FCurrentResolution].RefreshRates[i]=Core.RefreshRate then FCurrentRefreshRate:=i;
  if FCurrentRefreshRate=-1 then
  begin
    FCurrentRefreshRate:=Length(FResolutions[FCurrentResolution].RefreshRates);
    SetLength(FResolutions[FCurrentResolution].RefreshRates, FCurrentRefreshRate+1);
    FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate]:=Core.RefreshRate;
  end;
  Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
  Button[FBToggleCache].Caption:=CacheState[UseCache];
  Button[FBClearCache].Enabled:=UseCache;
end;

procedure TOptions.DrawForm;
begin
  Lbl[FLResolution].Caption:=Format('%dx%d', [FResolutions[FCurrentResolution].Width, FResolutions[FCurrentResolution].Height]);
  Lbl[FLRefreshRate].Caption:=IntToStr(FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate]);
  Lbl[FLColorDepth].Caption:=IntToStr(FColorDepth);
  inherited DrawForm;
end;

procedure TOptions.ResClick(Btn: PBtn);
begin
  FCurrentResolution:=Max(0, Min(FCurrentResolution+Btn.Tag, High(FResolutions)));
  FCurrentRefreshRate:=0;
end;

procedure TOptions.RefrClick(Btn: PBtn);
begin
  FCurrentRefreshRate:=Max(0, Min(FCurrentRefreshRate+Btn.Tag, High(FResolutions[FCurrentResolution].RefreshRates)));
end;

procedure TOptions.DepthClick(Btn: PBtn);
const
  Depth: array[-1..1] of Integer = (16, 0, 32);
begin
  FColorDepth:=Depth[Btn.Tag];
end;

procedure TOptions.ToggleCache(Btn: PBtn);
begin
  if UseCache then
  begin
    UseCache:=false;
    DeleteDir(CacheDir);
  end
  else begin
    UseCache:=true;
    CreateDir(CacheDir);
  end;
  Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
  Button[FBToggleCache].Caption:=CacheState[UseCache];
  Button[FBClearCache].Enabled:=UseCache;
end;

procedure TOptions.ClearCache(Btn: PBtn);
begin
  TexMan.ClearCache;
  Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
end;

procedure TOptions.KeyConfig(Btn: PBtn);
begin
  FParent.FCurFrm:=FParent.FKeyConfig;
  FParent.FKeyConfig.Refresh;
end;

procedure TOptions.OKClick(Btn: PBtn);
begin
  Core.SetResolution(FResolutions[FCurrentResolution].Width,
                     FResolutions[FCurrentResolution].Height,
                     FResolutions[FCurrentResolution].RefreshRates[FCurrentRefreshRate],
                     Button[FCFullscreen].Checked, true);
  Core.VSync:=Button[FCVSync].Checked;
  Core.ColorDepth:=FColorDepth;
  Sound.EnableBGM:=Button[FCEnableBGM].Checked;
  FParent.FCurFrm:=FParent.FMainMenu;
end;

procedure TOptions.CancelClick(Btn: PBtn);
begin
  FParent.FCurFrm:=FParent.FMainMenu;
end;

{TTextView}

const
  SPage='%d/%d';

constructor TTextView.Create(Parent: TStateMenu; const Caption, TextFile: string);
var
  Line: Integer;
  Src, Dst: string;
  Btn: TBtn;
  //Lbl: TLbl;
begin
  inherited Create(800, 600, 80, 60, 640, 480, TexMan.FontCreate('Arial', 12, true));
  FParent:=Parent;
  FCaption:=Caption;
  FText:=GetFileText(TextFile);
  Line:=0;
  while Line<FText.Count do
  begin
    Src:=ProcessKeyTags(FText[Line]);
    Dst:='';
    while (Src<>'') and (TexMan.TextWidth(FFont, Src)>620) do
    begin
      Dst:=Src[Length(Src)]+Dst;
      Delete(Src, Length(Src), 1);
    end;
    FText[Line]:=Src;
    if Dst<>'' then FText.Insert(Line+1, Dst);
    Inc(Line);
  end;
  FPages:=(FText.Count-1) div 25;
  FCurPage:=0;
  with Btn do
  begin
    Type_:=btPush;
    X:=535;
    Y:=445;
    Width:=100;
    Height:=30;
    Enabled:=true;
    Caption:='�������';
    OnClick:=Close;
    AddButton(Btn);
  end;
  if FPages>0 then
    FLPage:=CreateSelect(Self, 5, 445, 315, 30, ChangePage, '<', '>');
end;

destructor TTextView.Destroy;
begin
  FAN(FText);
  FParent.FCurFrm:=FParent.FMainMenu;
  inherited Destroy;
end;

procedure TTextView.KeyEvent(Key: Integer; Event: TKeyEvent);
var
  Btn: TBtn;
begin
  if Event=keDown then
    case Key of
      VK_LEFT:
        begin
          Btn.Tag:=-1;
          ChangePage(@Btn);
        end;
      VK_RIGHT:
        begin
          Btn.Tag:=1;
          ChangePage(@Btn);
        end;
    end
    else if Key=VK_ESCAPE then Close(nil);
  inherited KeyEvent(Key, Event);
end;

procedure TTextView.DrawForm;
var
  i, Left: Integer;
  S: string;
begin
  if FPages>0 then Lbl[FLPage].Caption:=Format(SPage, [FCurPage+1, FPages+1]);
  inherited DrawForm;
  glColor3f(0, 0.7, 0);
  for i:=0 to 24 do
    if 25*FCurPage+i<FText.Count then
    begin
      S:=FText[25*FCurPage+i];
      Left:=FX+10;
      if (S<>'') and (S[1]=#9) then
      begin
        S:=Copy(S, 2, MaxInt);
        Inc(Left, 310-TexMan.TextWidth(FFont, S) div 2);
      end;
      TexMan.TextOut(FFont, Left, FY+35+16*i, S);
    end;
end;

procedure TTextView.ChangePage(Btn: PBtn);
begin
  FCurPage:=Max(0, Min(FCurPage+Btn.Tag, FPages));
end;

procedure TTextView.Close(Btn: PBtn);
begin
  Free;
end;

{TStateMenu}

constructor TStateMenu.Create;
begin
  inherited Create;
  {$IFDEF VSE_CONSOLE}
  Console.OnCommand['cache ?val=eoff:on']:=CacheHandler;
  Console.OnCommand['menubg file=s']:=MenuBgHandler;
  {$ENDIF}
  FMainMenu:=TMainMenu.Create(Self);
  FOptions:=TOptions.Create(Self);
  FKeyConfig:=TBindManCfgForm.Create(800, 600, 200, 130, 400, 350,
    TexMan.FontCreate('Arial', 12, true), '�����������', '�������');
  FKeyConfig.Caption:='����������';
  FKeyConfig.OnClose:=KeyConfigClose;
  FCurFrm:=FMainMenu;
  FGame:=TStateGame(Core.GetState(Core.FindState('Game')));
  FLoad:=TStateLoad(Core.GetState(Core.FindState('Load')));
end;

destructor TStateMenu.Destroy;
begin
  FAN(FMainMenu);
  FAN(FOptions);
  FAN(FKeyConfig);
  inherited Destroy;
end;

procedure TStateMenu.Draw;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  if FGame.CanResumeGame then FGame.Draw
    else if FBgTex<>0 then
    begin
      TexMan.Bind(FBgTex);
      gleOrthoMatrix(800, 600);
      glColor(1.0, 1.0, 1.0);
      glBegin(GL_QUADS);
        glTexCoord(0.0, 0.0); glVertex(0.0, 0.0);
        glTexCoord(1.0, 0.0); glVertex(800.0, 0.0);
        glTexCoord(1.0, 1.0); glVertex(800.0, 600.0);
        glTexCoord(0.0, 1.0); glVertex(0.0, 600.0);
      glEnd;
      TexMan.Unbind;
    end;
  FCurFrm.Draw;
end;

procedure TStateMenu.Update;
begin
  inherited;
  FCurFrm.Update;
end;

function TStateMenu.Activate: Cardinal;
begin
  Result:=inherited Activate;
  glClearColor(0, 0, 0, 1);
  FMainMenu.ResumeEnable(FGame.CanResumeGame);
end;

procedure TStateMenu.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  inherited;
  FCurFrm.MouseEvent(Button, Event, X, Y);
end;

procedure TStateMenu.KeyEvent(Key: Integer; Event: TKeyEvent);
begin
  inherited;
  FCurFrm.KeyEvent(Key, Event);
end;

procedure TStateMenu.CharEvent(C: Char);
begin
  inherited;
  FCurFrm.CharEvent(C);
end;

function TStateMenu.GetName: string;
begin
  Result:='Menu';
end;

procedure TStateMenu.KeyConfigClose(Sender: TObject);
begin
  FCurFrm:=FOptions;
end;

function TStateMenu.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result:=inherited SysNotify(Notify);
  if Notify=snConsoleActive then Result:=true;
end;

{$IFDEF VSE_CONSOLE}
const
  BoolState: array[Boolean] of string = ('off', 'on');

function TStateMenu.CacheHandler(Sender: TObject; Args: array of const): Boolean;
begin
  if Length(Args)>0 then
  begin
    UseCache:=Boolean(Args[0].VInteger);
    if UseCache
      then CreateDir(CacheDir)
      else DeleteDir(CacheDir);
  end
    else Console.WriteLn('Cache: '+BoolState[UseCache]);
  Result:=true;
end;

function TStateMenu.MenuBgHandler(Sender: TObject; Args: array of const): Boolean;
var
  Image: TImage;
  FileName: string;
begin
  FileName:=ExePath+string(Args[0].VAnsiString);
  if FileExists(FileName) then
  begin
    Image:=TImage.Create;
    try
      Image.Load(FileName);
      FBgTex:=TexMan.AddTexture('MenuBg', Image, true, false);
    finally
      Image.Free;
    end;
  end
    {$IFDEF VSE_LOG}else LogF(llError, 'StateMenu.MenuBg: file "%s" not found', [string(Args[0].VAnsiString)]){$ENDIF};
end;
{$ENDIF}

end.
