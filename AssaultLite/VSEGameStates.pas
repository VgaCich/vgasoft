unit VSEGameStates;

interface

uses
  Windows, Messages, AvL, avlUtils{$IFDEF VSE_LOG}, VSELog{$ENDIF};

type
  TMouseEvent=(meDown, meUp, meMove, meWheel);
  TKeyEvent=(keDown, keUp);
  TSysNotify=(snUpdateOverload);
  TGameState=class
  protected
    function GetName: string; virtual; abstract;
  public
    procedure Draw; virtual; abstract;
    procedure Update; virtual; abstract;
    function  Activate: Cardinal; virtual; abstract; //returns update interval
    procedure Deactivate; virtual;
    procedure Resume; virtual;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); virtual;
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); virtual;
    procedure CharEvent(C: Char); virtual;
    procedure SysNotify(Notify: TSysNotify); virtual;
    property  Name: string read GetName;
  end;

implementation

uses
  VSEInit, VSECore;

procedure TGameState.Deactivate;
begin

end;

procedure TGameState.Resume;
begin

end;

procedure TGameState.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin

end;

procedure TGameState.KeyEvent(Button: Integer; Event: TKeyEvent);
begin

end;

procedure TGameState.CharEvent(C: Char);
begin

end;

procedure TGameState.SysNotify(Notify: TSysNotify);
begin
  case Notify of
    snUpdateOverload: begin
                        {$IFDEF VSE_LOG}Log(llError, 'Update overload in state "'+Name+'"');
                        {$ELSE}MessageBox(Core.Handle, PChar('Update overload in state "'+Name+'"'), PChar(CaptionVer), MB_ICONERROR);{$ENDIF}
                        VSEStopState:=1;
                        Core.StopEngine;
                      end;
  end;
end;

end.
