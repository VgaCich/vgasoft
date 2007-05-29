unit StateIntro;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures,
  VSEGameStates;

type
  TStateIntro=class(TGameState)
  private
    FU: Cardinal;
  protected
    function GetName: string; override;
  public
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
    procedure Resume; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
  end;

implementation

uses VSECore, VSEInit;

procedure TStateIntro.Draw;
begin
  glClear(GL_COLOR_BUFFER_BIT);
  gleWrite(400-gleTextWidth('VgaSoft '+CaptionVer)/2, 300, 'VgaSoft '+CaptionVer);
  gleWrite(400-gleTextWidth('Updates: '+IntToStr(FU))/2, 320, 'Updates: '+IntToStr(FU));
end;

procedure TStateIntro.Update;
begin
  Inc(FU);
  if FU>50 then Core.SwitchState('Menu');
end;

function TStateIntro.Activate: Cardinal;
begin
  gleSelectFont('Default');
  FU:=0;
  glClearColor(0, 0, 0, 1);
  glColor3d(0, 1, 0);
  glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA);
  glEnable(GL_COLOR_MATERIAL);
  gleOrthoMatrix(800, 600);
  Result:=50;
end;

procedure TStateIntro.Deactivate;
begin

end;

procedure TStateIntro.Resume;
begin

end;

procedure TStateIntro.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin
  Core.SwitchState('Menu');
end;

procedure TStateIntro.KeyEvent(Button: Integer; Event: TKeyEvent);
begin
  Core.SwitchState('Menu');
end;

function TStateIntro.GetName: string;
begin
  Result:='Intro';
end;

end.
