unit StateMenu;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions,
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
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
    procedure ResumeEnable(Enable: Boolean);
  end;
  TOptions=class(TGUIForm)
  protected
    FParent: TStateMenu;
    FLRes, FLRefr, FLDepth, FLCacheSize, FCFullScr, FCVSync, FBToggleCache,
      FBClearCache, FCurRes, FCurRefr, FDepth: Integer;
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
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
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
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
  end;
  TStateMenu=class(TGameState)
  private
    FMainMenu: TMainMenu;
    FOptions: TOptions;
    FKeyConfig: TBindManCfgForm;
    FCurFrm: TGUIForm;
    FGame: TStateGame;
    FLoad: TStateLoad;
    procedure KeyConfigClose(Sender: TObject);
  protected
    function GetName: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
    procedure CharEvent(C: Char); override;
  end;

implementation

uses VSECore, VSEInit;

{TMainMenu}

constructor TMainMenu.Create(Parent: TStateMenu);
var
  Btn: TBtn;
begin
  inherited Create(800, 600, 300, 130, 200, 350, TexMan.FontCreate('Arial', 12, true));
  FParent:=Parent;
  FCaption:='Assault Lite';
  with Btn do
  begin
    Caption:='Новая игра';
    Tag:=1;
    Typ:=btPush;
    X:=30;
    Y:=50;
    Width:=140;
    Height:=30;
    OnClick:=GameClick;
    Enabled:=true;
    AddButton(Btn);
    Caption:='Продолжить';
    Tag:=0;
    Y:=100;
    Enabled:=false;
    OnClick:=GameClick;
    FResumeButton:=AddButton(Btn);
    Caption:='Настройки';
    Y:=150;
    Enabled:=true;
    OnClick:=OptionsClick;
    AddButton(Btn);
    Caption:='Авторы';
    Tag:=0;
    Y:=200;
    OnClick:=TextClick;
    AddButton(Btn);
    Caption:='Справка';
    Tag:=1;
    Y:=250;
    OnClick:=TextClick;
    AddButton(Btn);
    Caption:='Выход';
    Y:=300;
    OnClick:=ExitClick;
    AddButton(Btn);
  end;
end;

procedure TMainMenu.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if (Button=VK_ESCAPE) and (Event=keDown)
    then Core.StopEngine
    else inherited KeyEvent(Button, Event);
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
  CacheState: array[Boolean] of string = ('Включить кэш', 'Выключить кэш');
  SCacheSize='Кэш: ';

constructor TOptions.Create(Parent: TStateMenu);
var
  Btn: TBtn;
  Lbl: TLbl;
begin
  inherited Create(800, 600, 200, 130, 400, 350, TexMan.FontCreate('Arial', 12, true));
  FParent:=Parent;
  FCaption:='Настройки';
  FResolutions:=gleGetResolutions;
  with Btn do
  begin
    Enabled:=true;
    Tag:=0;
    Typ:=btCheck;
    X:=10;
    Y:=200;
    Width:=210;
    Height:=20;
    Caption:='Полный экран';
    FCFullScr:=AddButton(Btn);
    Y:=230;
    Caption:='Верт. синхронизация';
    FCVSync:=AddButton(Btn);
    Typ:=btPush;
    X:=220;
    Y:=60;
    Width:=160;
    Height:=30;
    OnClick:=ToggleCache;
    FBToggleCache:=AddButton(Btn);
    Y:=100;
    Caption:='Очистить кэш';
    OnClick:=ClearCache;
    FBClearCache:=AddButton(Btn);
    Y:=150;
    Caption:='Управление';
    OnClick:=KeyConfig;
    AddButton(Btn);
    X:=140;
    Y:=310;
    Width:=120;
    Height:=30;
    Caption:='Применить';
    OnClick:=OKClick;
    AddButton(Btn);
    X:=270;
    Caption:='Отменить';
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
    Caption:='Разрешение';
    AddLabel(Lbl);
    Y:=90;
    Caption:='Частота обновления';
    AddLabel(Lbl);
    Y:=140;
    Caption:='Глубина цвета';
    AddLabel(Lbl);
    X:=220;
    Y:=40;
    Width:=160;
    Align:=laLeft;
    Caption:='';
    FLCacheSize:=AddLabel(Lbl);
  end;
  FLRes:=CreateSelect(Self, 10, 60, 190, 20, ResClick, '-', '+');
  FLRefr:=CreateSelect(Self, 10, 110, 190, 20, RefrClick, '-', '+');
  FLDepth:=CreateSelect(Self, 10, 160, 190, 20, DepthClick, '-', '+');
end;

destructor TOptions.Destroy;
begin
  Finalize(FResolutions);
  inherited Destroy;
end;

procedure TOptions.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  if (Button=VK_ESCAPE) and (Event=keDown)
    then FParent.FCurFrm:=FParent.FMainMenu
    else inherited KeyEvent(Button, Event);
end;

procedure TOptions.ReadOptions;
var
  i: Integer;
begin
  Button[FCFullScr].Checked:=Core.Fullscreen;
  Button[FCVSync].Checked:=Core.VSync;
  FDepth:=Core.Depth;
  FCurRes:=-1;
  for i:=0 to High(FResolutions) do
    if (FResolutions[i].Width=Core.ResX) and (FResolutions[i].Height=Core.ResY) then FCurRes:=i;
  if FCurRes=-1 then
  begin
    FCurRes:=Length(FResolutions);
    SetLength(FResolutions, FCurRes+1);
    with FResolutions[FCurRes] do
    begin
      Width:=Core.ResX;
      Height:=Core.ResY;
      SetLength(RefreshRates, 1);
      RefreshRates[0]:=Core.Refresh;
    end;
  end;
  FCurRefr:=-1;
  for i:=0 to High(FResolutions[FCurRes].RefreshRates) do
    if FResolutions[FCurRes].RefreshRates[i]=Core.Refresh then FCurRefr:=i;
  if FCurRefr=-1 then
  begin
    FCurRefr:=Length(FResolutions[FCurRes].RefreshRates);
    SetLength(FResolutions[FCurRes].RefreshRates, FCurRefr+1);
    FResolutions[FCurRes].RefreshRates[FCurRefr]:=Core.Refresh;
  end;
  Lbl[FLCacheSize].Caption:=SCacheSize+SizeToStr(DirSize(CacheDir));
  Button[FBToggleCache].Caption:=CacheState[UseCache];
  Button[FBClearCache].Enabled:=UseCache;
end;

procedure TOptions.DrawForm;
begin
  Lbl[FLRes].Caption:=Format('%dx%d', [FResolutions[FCurRes].Width, FResolutions[FCurRes].Height]);
  Lbl[FLRefr].Caption:=IntToStr(FResolutions[FCurRes].RefreshRates[FCurRefr]);
  Lbl[FLDepth].Caption:=IntToStr(FDepth);
  inherited DrawForm;
end;

procedure TOptions.ResClick(Btn: PBtn);
begin
  FCurRes:=Max(0, Min(FCurRes+Btn.Tag, High(FResolutions)));
  FCurRefr:=0;
end;

procedure TOptions.RefrClick(Btn: PBtn);
begin
  FCurRefr:=Max(0, Min(FCurRefr+Btn.Tag, High(FResolutions[FCurRes].RefreshRates)));
end;

procedure TOptions.DepthClick(Btn: PBtn);
const
  Depth: array[-1..1] of Integer = (16, 0, 32);
begin
  FDepth:=Depth[Btn.Tag];
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
end;

procedure TOptions.OKClick(Btn: PBtn);
begin
  Core.SetResolution(FResolutions[FCurres].Width, FResolutions[FCurres].Height, FResolutions[FCurres].RefreshRates[FCurRefr], true);
  Core.Fullscreen:=Button[FCFullScr].Checked;
  Core.VSync:=Button[FCVSync].Checked;
  Core.Depth:=FDepth;
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
    Src:=FText[Line];
    Dst:='';
    while (Src<>'') and (TexMan.TextLen(FFont, Src)>620) do
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
    Typ:=btPush;
    X:=535;
    Y:=445;
    Width:=100;
    Height:=30;
    Enabled:=true;
    Caption:='Закрыть';
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

procedure TTextView.KeyEvent(Button: Integer; Event: TKeyEvent);
var
  Btn: TBtn;
begin
  if Event=keDown then
    case Button of
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
      VK_ESCAPE: Close(nil);
      else inherited KeyEvent(Button, Event);
    end;
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
        Inc(Left, 310-TexMan.TextLen(FFont, S) div 2);
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
  FMainMenu:=TMainMenu.Create(Self);
  FOptions:=TOptions.Create(Self);
  FKeyConfig:=TBindManCfgForm.Create(800, 600, 200, 130, 400, 350, TexMan.FontCreate('Arial', 12, true), 'Закрыть');
  FKeyConfig.Caption:='Управление';
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
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  FCurFrm.Draw;
end;

procedure TStateMenu.Update;
begin
  FCurFrm.Update;
end;

function TStateMenu.Activate: Cardinal;
begin
  glClearColor(0, 0, 0, 1);
  FMainMenu.ResumeEnable(FGame.CanResumeGame);
  Result:=50;
end;

procedure TStateMenu.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  FCurFrm.MouseEvent(Button, Event, X, Y);
end;

procedure TStateMenu.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  FCurFrm.KeyEvent(Button, Event);
end;

procedure TStateMenu.CharEvent(C: Char);
begin
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

end.
