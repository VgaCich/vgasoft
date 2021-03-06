unit Sky;

interface

uses
  AvL, avlUtils, OpenGL, VSECamera, VSEPrimitiveModel;

type
  TSky=class(TObject)
  private
    FDome: TPriModel;
    FShift: Single;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; //Draw sky
    procedure Update; //Update sky
  end;

implementation

constructor TSky.Create;
begin
  inherited Create;
  FDome:=TPriModel.Create('Sky.vpm');
end;

destructor TSky.Destroy;
begin
  FAN(FDome);
  inherited Destroy;
end;

procedure TSky.Draw;
begin
  glPushAttrib(GL_ENABLE_BIT or GL_TRANSFORM_BIT);
  glMatrixMode(GL_TEXTURE);
  glPushMatrix;
  glLoadIdentity;
  glTranslate(FShift, 0, 0);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;
  with Camera.Eye do glTranslate(X, Y, Z);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);
  glColor3f(1.0, 1.0, 1.0);
  FDome.Draw;
  glPopMatrix;
  glMatrixMode(GL_TEXTURE);
  glPopMatrix;
  glPopAttrib;
end;

procedure TSky.Update;
begin
  FShift:=FShift+0.0002;
  if FShift>1 then FShift:=FShift-1;
end;

end.