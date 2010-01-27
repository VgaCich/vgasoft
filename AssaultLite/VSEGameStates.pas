unit VSEGameStates;

interface

uses
  Windows, Messages, AvL, avlUtils{$IFDEF VSE_LOG}, VSELog{$ENDIF};

const
  mbLeft=1; //Left mouse button
  mbRight=2; //Right mouse button
  mbMiddle=3; //Middle mouse button
  mbX1=4; //Fourth mouse button
  mbX2=5; //Fifth mouse button

type
  TMouseEvent=(meDown, meUp, meMove, meWheel); //Mouse event: button pressed, button release, mouse moving, mouse wheel
  TKeyEvent=(keDown, keUp); //Keyboard event: key pressed, key released
  TSysNotify=(  //System notifies:
    snMinimize, //Application minimized, return false to pause or true to continue work
    snMaximize, //Application maximized
    snUpdateOverload //Update Overload Detection triggered, return true to disable default handler (resets update timer)
  );

const
  MouseEventNames: array[TMouseEvent] of string = ('meDown', 'meUp', 'meMove', 'meWheel');
  KeyEventNames: array[TKeyEvent] of string = ('keDown', 'keUp');
  SysNotifyNames: array[TSysNotify] of string = ('snMinimized', 'snMaximized', 'snUpdateOverload');

type
  TGameState=class //Base state class
  protected
    function GetName: string; virtual; abstract; //Must returns state name
  public
    procedure Draw; virtual; abstract; //Draw state event
    procedure Update; virtual; abstract; //Update state event
    function  Activate: Cardinal; virtual; abstract; //Activate state event (triggered on switching to state), must return updates interval
    procedure Deactivate; virtual; //Deactiovate state event (triggered on switching from state)
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); virtual; //Mouse event; Button - mouse button number or wheel click if Event=meWheel; X, Y - cursor coordinates or cursor coordinates delta if Core.MouseCapture=true
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); virtual; //Keyboard event; Button - VK key code
    procedure CharEvent(C: Char); virtual; //Char event (char of pressed key in current layout)
    function  SysNotify(Notify: TSysNotify): Boolean; virtual; //System notify event
    property  Name: string read GetName; //State name
  end;

implementation

uses
  VSEInit, VSECore;

procedure TGameState.Deactivate;
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

function TGameState.SysNotify(Notify: TSysNotify): Boolean;
begin
  Result:=false;
end;

end.
