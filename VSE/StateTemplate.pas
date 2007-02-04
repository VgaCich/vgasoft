unit State<!Name>;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures, GameStates;

type
  TState<!Name>=class(TGameState)
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
    procedure CharEvent(C: Char); override;
    procedure SysNotify(Notify: TSysNotify); override;
  end;

implementation

uses UGame;

procedure TState<!Name>.Draw;
begin

end;

procedure TState<!Name>.Update;
begin

end;

function TState<!Name>.Activate: Cardinal;
begin

end;

procedure TState<!Name>.Deactivate;
begin

end;

procedure TState<!Name>.Resume;
begin

end;

procedure TState<!Name>.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin

end;

procedure TState<!Name>.KeyEvent(Button: Integer; Event: TKeyEvent);
begin

end;

procedure TState<!Name>.CharEvent(C: Char);
begin

end;

procedure TState<!Name>.SysNotify(Notify: TSysNotify);
begin

end;

function TState<!Name>.GetName: string;
begin
  Result:='<!Name>';
end;

end.
