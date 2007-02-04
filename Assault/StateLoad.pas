unit StateLoad;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures, GameStates;

type
  TProgFunc=procedure(Progress: Integer; const State: string) of object;
  TLoadFunc=function(ProgFunc: TProgFunc): Boolean of object;
  TStateLoad=class;
  TLoadThread=class(TThread)
  protected
    FParent: TStateLoad;
    FLoadFunc: TLoadFunc;
    procedure Execute; override;
    procedure ProgFunc(Progress: Integer; const State: string);
  public
    Done: Boolean;
    constructor Create(CreateSuspended: Boolean; AParent: TStateLoad);
  end;
  TStateLoad=class(TGameState)
  private
    FLoadFunc: TLoadFunc;
    FLoadThread: TLoadThread;
    FProgress: Integer;
    FResult: Boolean;
    FState: string;
    function GetName: string; override;
  public
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
    procedure Resume; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
    property LoadFunc: TLoadFunc read FLoadFunc write FLoadFunc;
    property Progress: Integer write FProgress;
    property State: string write FState;
    property Result: Boolean read FResult write FResult;
  end;

implementation

uses UCore;

constructor TLoadThread.Create(CreateSuspended: Boolean; AParent: TStateLoad);
begin
  inherited Create(CreateSuspended);
  FParent:=AParent;
  FLoadFunc:=FParent.LoadFunc;
  Done:=false;
end;

procedure TLoadThread.Execute;
begin
  FParent.Result:=FLoadFunc(ProgFunc);
  Done:=true;
end;

procedure TLoadThread.ProgFunc(Progress: Integer; const State: string);
begin
  FParent.Progress:=Progress;
  FParent.State:=State;
end;

///

procedure TStateLoad.Draw;
var
  i: Integer;
begin
  glClear(GL_COLOR_BUFFER_BIT);
  glColor3d(0, 1, 0);
  glBegin(GL_LINE_LOOP);
    for i:=0 to 15 do glVertex2d(240*cos(pi*i*0.93333333333333333333333333333333)+400, 240*sin(pi*i*0.93333333333333333333333333333333)+300);
  glEnd;
  glBegin(GL_QUADS);
    glColor3d(0, 1, 1);
    glVertex2d(10, 590);
    glVertex2d(10, 580);
    glColor3d(0, 1, 1-FProgress/100);
    glVertex2d(10+FProgress/0.12820512820512820512820512820513, 580);
    glVertex2d(10+FProgress/0.12820512820512820512820512820513, 590);
  glEnd;
  glColor3d(0, 1, 0);
  glBegin(GL_LINE_LOOP);
    glVertex2d(10, 590);
    glVertex2d(10, 580);
    glVertex2d(790, 580);
    glVertex2d(790, 590);
  glEnd;
  gleWrite(10, 560, FState);
end;

procedure TStateLoad.Update;
begin
  if FLoadThread.Done then Core.SwitchState(Core.FindState('Menu'));
end;

function TStateLoad.Activate: Cardinal;
begin
  FLoadThread:=TLoadThread.Create(false, Self);
  FLoadThread.Priority:=tpHigher;
  glClearColor(0, 0, 0, 1);
  glLineWidth(1);
  glEnable(GL_LINE_SMOOTH);
  glDisable(GL_LIGHTING);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glBindTexture(GL_TEXTURE_2D, 0);
  gleSelectFont('Default');
  gleOrthoMatrix(800, 600);
  Result:=50;
end;

procedure TStateLoad.Deactivate;
begin
  FAN(FLoadThread);
end;

procedure TStateLoad.Resume;
begin

end;

procedure TStateLoad.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin

end;

procedure TStateLoad.KeyEvent(Button: Integer; Event: TKeyEvent);
begin

end;

function TStateLoad.GetName: string;
begin
  Result:='Load';
end;

end.
