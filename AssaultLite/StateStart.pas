unit StateStart;

interface

uses
  Windows, Messages, AvL, avlUtils, OpenGL, VSEOpenGLExt, oglExtensions, avlVectors,
  VSEGameStates, SynTex, SynTexFilters, VSEMemPak, VSETexMan;

type
  TStateStart=class;
  TLoadThread=class(TThread)
  protected
    procedure Execute; override;
    procedure Store(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string);
    function  Load(Sender: TObject; var Reg: TSynTexRegister; TexSize: Integer; const Name: string): Boolean;
  public
    Parent: TStateStart;
  end;
  TStateStart=class(TGameState)
  private
    FFont: Cardinal;
    FLoadThread: TLoadThread;
  protected
    function  GetName: string; override;
    procedure DrawSegs(X, Y: Single; Segs: Integer);
    procedure SyncLoadCache;
    procedure SyncStore;
    procedure SyncLoad;
    procedure OnLoaded(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
  end;

implementation

uses VSECore {$IFDEF VSE_LOG}, VSELog{$ENDIF};

var
  LoadResult: Boolean=false;
  SReg: PSynTexRegister;
  STexSize: Integer;
  SName: string;
  SSender: TObject;
  LDResult: Boolean;

const
  SLoad='Загрузка...';
  SRVSE='Reduced VS Engine';
  STitle='Assault Lite';

{TLoadThread} //I'm don't sure that is correct... May be a source of AV's

procedure TLoadThread.Execute;
var
  ST: TSynTex;
  STF: TSynTexFilters;
  STCode: TStream;
begin
  STCode:=GetFile('Textures.stc');
  ST:=nil;
  STF:=nil;
  if STCode=nil then
  begin
    {$IFDEF VSE_LOG}Log(llError, 'Textures synthesizing code not found');{$ENDIF}
    Exit;
  end;
  try
    ST:=TSynTex.Create(512);
    STF:=TSynTexFilters.Create(ST);
    ST.Code:=STCode;
    ST.OnStore:=Store;
    ST.OnLoad:=Load;
    Synchronize(Parent.SyncLoadCache);
    if not LoadResult then LoadResult:=ST.Synthesize;
  finally
    FAN(STF);
    FAN(ST);
    FAN(STCode);
  end;
end;

procedure TLoadThread.Store(Sender: TObject; const Reg: TSynTexRegister; TexSize: Integer; const Name: string);
begin
  SReg:=@Reg;
  STexSize:=TexSize;
  SName:=Name;
  SSender:=Sender;
  Synchronize(Parent.SyncStore);
end;

function TLoadThread.Load(Sender: TObject; var Reg: TSynTexRegister; TexSize: Integer; const Name: string): Boolean;
begin
  SReg:=@Reg;
  STexSize:=TexSize;
  SName:=Name;
  SSender:=Sender;
  Synchronize(Parent.SyncLoad);
  Result:=LDResult;
end;

{TStateStart}

constructor TStateStart.Create;
begin
  inherited Create;
  FFont:=TexMan.FontCreate('Arial', 20, true);
end;

destructor TStateStart.Destroy;
begin
  inherited Destroy;
end;

procedure TStateStart.Draw;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  TexMan.TextOut(FFont, 400-TexMan.TextLen(FFont, SLoad)/2, 500, SLoad);
  TexMan.TextOut(FFont, 400-TexMan.TextLen(FFont, STitle)/2, 250, STitle);
  glPushMatrix;
  glTranslate(200, 230, 0);
  glScalef(0.5, 0.5, 1);
  TexMan.TextOut(FFont, 400-TexMan.TextLen(FFont, SRVSE)/2, 430, SRVSE);
  DrawSegs(300, 200, $426);
  DrawSegs(400, 200, $2C0);
  DrawSegs(500, 200, $079);
  glScalef(0.7, 0.7, 1);
  DrawSegs(300, 370, $473);
  glPopMatrix;
end;

procedure TStateStart.Update;
begin

end;

function TStateStart.Activate: Cardinal;
begin
  glClearColor(0, 0, 0, 1);
  glColor3f(0, 1, 0);
  gleOrthoMatrix(800, 600);
  FLoadThread:=TLoadThread.Create(true);
  FLoadThread.OnTerminate:=OnLoaded;
  FLoadThread.Resume;
  ShowCursor(false);
  Result:=100;
end;

procedure TStateStart.Deactivate;
begin
  FLoadThread.WaitFor;
  FAN(FLoadThread);
  ShowCursor(true);
end;

function TStateStart.GetName: string;
begin
  Result:='Start';
end;

procedure TStateStart.DrawSegs(X, Y: Single; Segs: Integer);
const
  SegsCoord: array [0..10, 0..1] of TVector2D=(
    ((X: 0; Y: 0),    (X: 100; Y: 0)),
    ((X: 100; Y: 0),  (X: 100; Y: 100)),
    ((X: 100; Y: 100), (X: 100; Y: 200)),
    ((X: 100; Y: 200),  (X: 0; Y: 200)),
    ((X: 0; Y: 200),   (X: 0; Y: 100)),
    ((X: 0; Y: 100),    (X: 0; Y: 0)),
    ((X: 0; Y: 100),  (X: 100; Y: 100)),
    ((X: 100; Y: 0),   (X: 0; Y: 100)),
    ((X: 0; Y: 0),   (X: 100; Y: 100)),
    ((X: 100; Y: 100),  (X: 0; Y: 200)),
    ((X: 0; Y: 100),  (X: 100; Y: 200)));
var
  i: Integer;
begin
  glPushMatrix;
  glLineWidth(7);
  glEnable(GL_LINE_SMOOTH);
  glPointSize(6);
  glEnable(GL_POINT_SMOOTH);
  glTranslatef(X, Y, 0);
  glBegin(GL_LINES);
    for i:=0 to 10 do
      if Segs and (1 shl i)<>0 then
      begin
        glVertex2fv(@SegsCoord[i, 0]);
        glVertex2fv(@SegsCoord[i, 1]);
      end;
  glEnd;
  glBegin(GL_POINTS);
    for i:=0 to 10 do
      if Segs and (1 shl i)<>0 then
      begin
        glVertex2fv(@SegsCoord[i, 0]);
        glVertex2fv(@SegsCoord[i, 1]);
      end;
  glEnd;
  glPopMatrix;
end;

procedure TStateStart.SyncLoadCache;
begin
  LoadResult:=TexMan.LoadCache;
end;

procedure TStateStart.SyncStore;
begin
  TexMan.Store(SSender, SReg^, STexSize, SName);
end;

procedure TStateStart.SyncLoad;
begin
  LDResult:=TexMan.Load(SSender, SReg^, STexSize, SName);
end;

procedure TStateStart.OnLoaded(Sender: TObject);
begin
  if LoadResult
    then Core.SwitchState('Menu')
    else begin
      {$IFDEF VSE_LOG}Log(llError, 'Loading textures failed');{$ENDIF}
      Core.StopEngine;
    end;
end;

end.
