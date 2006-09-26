unit StateConsole;

interface

uses
  Windows, Messages, AvL, avlUtils, dglOpenGL, OpenGLExt, avlVectors, Textures,
  GameStates, StateGame, UConsole, avlMath;

type
  TStateConsole=class(TGameState)
  private
    FCurLine, FSubLine, FCurCmdHist, FCursor, FCursorShowTime, FGameState, FConsoleState: Cardinal;
    FCurCmd: string;
    FNotEnd, FCursorShow: Boolean;
    FGame: TStateGame;
    FList: TStringList;
    function  GetName: string; override;
    function  LineLength(const Line: string): Integer;
    procedure GetEnd(var CurLine, SubLine: Cardinal);
    procedure SplitLine(const Line: string);
    procedure Adding(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    procedure Update; override;
    function  Activate: Cardinal; override;
    procedure Deactivate; override;
    procedure Resume; override;
    procedure MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer); override;
    procedure KeyEvent(Button: Integer; Event: TKeyEvent); override;
    procedure CharEvent(C: Char); override;
  end;

implementation

uses UGame;

const
  Prompt='>';
  Tag: Char='^';

constructor TStateConsole.Create;
begin
  inherited Create;
  Console.OnAdding:=Adding;
  FList:=TStringList.Create;
end;

destructor TStateConsole.Destroy;
begin
  FAN(FList);
  inherited Destroy;
end;

procedure TStateConsole.Draw;
const
  DefaultColor: TVector4f=(Red: 0; Green: 1; Blue: 0; Alpha: 1);
var
  i, CurConLine, CurLine, SubLine, CurCmdFrom, Len: Integer;
  CurColor: TVector4f;

  procedure SetColor(R, G, B: TGLfloat);
  begin
    CurColor.Red:=R;
    CurColor.Green:=G;
    CurColor.Blue:=B;
  end;

  procedure SetTag(Tag: Char);
  begin
    case Tag of
      '0': SetColor(0, 0, 0); //BLACK
      '1': SetColor(1, 0, 0); //RED
      '2': SetColor(0, 1, 0); //GREEN
      '3': SetColor(1, 1, 0); //YELLOW
      '4': SetColor(0, 0, 1); //BLUE
      '5': SetColor(1, 0, 1); //FUCHSIA
      '6': SetColor(0, 1, 1); //AQUA
      '7': SetColor(1, 1, 1); //WHITE
      'b': CurColor.Alpha:=Abs(Sin(Game.Time mod 300/300*pi))/2+0.5; //BLINK
      'n': CurColor.Alpha:=1; //NORMAL
    end;
    glColor4fv(@CurColor);
  end;

  function NextLine: Boolean;
  begin
    Inc(CurLine);
    Result:=CurLine<Console.HistoryCount;
    if not Result then Exit;
    SplitLine(Console[CurLine]);
    SubLine:=0;
    CurColor:=DefaultColor;
    glColor4fv(@CurColor);
  end;

  procedure WriteLine(const Line: string);
  var
    i, Pos: Integer;
    S: string;
    IsTag: Boolean;
  begin
    S:='';
    Pos:=0;
    IsTag:=false;
    for i:=1 to Length(Line) do
    begin
      if Line[i]=Tag
        then IsTag:=true
        else
          if IsTag then
          begin
            gleWrite(Pos*10, CurConLine*16+5, S);
            Inc(Pos, Length(S));
            S:='';
            SetTag(Line[i]);
            IsTag:=false;
          end
            else S:=S+Line[i];
    end;
    gleWrite(Pos*10, CurConLine*16+5, S);
  end;

begin
  FGame.Draw;
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  gleOrthoUpdateMatrixProc(0, 0);
  glBindTexture(GL_TEXTURE_2D, 0);
  glColor4f(0.3, 0.3, 0.3, 0.8);
  glBegin(GL_QUADS);
    glVertex2d(0, 0);
    glVertex2d(800, 0);
    glVertex2d(800, 300);
    glVertex2d(0, 300);
  glEnd;
  glLineWidth(1);
  glEnable(GL_LINE_SMOOTH);
  glBegin(GL_LINES);
    glColor4f(0, 1, 0, 1);
    glVertex2d(0, 300);
    glVertex2d(800, 300);
    glColor4f(0.2, 0.2, 0.2, 0.5);
    for i:=0 to 99 do
    begin
      glVertex2d(0, 3*i);
      glVertex2d(800, 3*i);
    end;
  glEnd;
  glColor4f(0, 1, 0, 0.3);
  gleSelectFont('Console');
  gleWrite(650, 280, 'VS '+CaptionVer);
  CurColor:=DefaultColor;
  glColor4fv(@CurColor);
  CurConLine:=0;
  CurLine:=FCurLine-1;
  NextLine;
  SubLine:=FSubLine;
  while CurConLine<16 do
  begin
    if SubLine<FList.Count then
    begin
      WriteLine(FList[SubLine]);
      Inc(SubLine);
      Inc(CurConLine);
    end
      else if not NextLine then Break;
  end;
  glColor4fv(@DefaultColor);
  if FNotEnd then gleWrite(0, 265, '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^');
  CurCmdFrom:=FCursor-79;
  if CurCmdFrom<0 then CurCmdFrom:=0;
  if FCursorShow then gleWrite(10*(FCursor-CurCmdFrom), 282, '_');
  gleWrite(0, 280, Prompt+Copy(FCurCmd, CurCmdFrom+1, 79));
end;

procedure TStateConsole.Update;
var
  CurLine, SubLine: Cardinal;
begin
  GetEnd(CurLine, SubLine);
  FNotEnd:=(FCurLine<CurLine) or ((FCurLine=CurLine) and (FSubLine<SubLine));
  Inc(FCursorShowTime, 50);
  if FCursorShowTime>500 then
  begin
    FCursorShow:=not FCursorShow;
    FCursorShowTime:=0;
  end;
end;

function TStateConsole.Activate: Cardinal;
begin
  FGameState:=Game.FindState('Game');
  FConsoleState:=Game.FindState('Console');
  FGame:=Game.GetState(FGameState) as TStateGame;
  GetEnd(FCurLine, FSubLine);
  FCurCmdHist:=Console.CommandsHistoryCount;
  FCursor:=1;
  FCursorShowTime:=0;
  FCurCmd:='';
  Result:=50;
end;

procedure TStateConsole.Deactivate;
begin

end;

procedure TStateConsole.Resume;
begin

end;

procedure TStateConsole.MouseEvent(Button: Integer; Event: TMouseEvent; X, Y: Integer);
begin

end;

procedure TStateConsole.KeyEvent(Button: Integer; Event: TKeyEvent);

  procedure IncrementalFind;
  var
    CmdList: TStringList;
    i: Integer;
    S: string;
  begin
    CmdList:=TStringList.Create;
    try
      if (FCurCmd<>'') and (FCurCmd[1]=ConsoleCommandPrefix)
        then S:=Copy(FCurCmd, 2, MaxInt)
        else S:=FCurCmd;
      Console.IncrementalFind(S, CmdList);
      case CmdList.Count of
        0: Console.AddToConsole('No commands, starts with "'+S+'"');
        1: begin
             FCurCmd:=ConsoleCommandPrefix+CmdList[0]+' ';
             i:=Pos(' ', S);
             if i>0 then FCurCmd:=FCurCmd+Copy(S, i+1, MaxInt);
             FCursor:=Length(FCurCmd)+1;
           end;
        else
          Console.AddToConsole('Commands, starts with "'+S+'":');
          for i:=0 to CmdList.Count-1 do
            Console.AddToConsole('    '+CmdList[i]);
      end;
    finally
      FAN(CmdList);
    end;
  end;

begin
  if Event=keDown then
    case Button of
      VK_PRIOR: if FSubLine>0
                  then Dec(FSubLine)
                  else if FCurLine>0
                    then begin
                      Dec(FCurLine);
                      FSubLine:=Floor(LineLength(Console[FCurLine])/80);
                    end;
      VK_NEXT: if FSubLine<Floor(LineLength(Console[FCurLine])/80)
                 then Inc(FSubLine)
                 else if FCurLine<Console.HistoryCount-1
                   then begin
                     Inc(FCurLine);
                     FSubLine:=0;
                   end;
      VK_LEFT: if FCursor>1 then Dec(FCursor);
      VK_RIGHT: if FCursor<Length(FCurCmd)+1 then Inc(FCursor);
      VK_DELETE: Delete(FCurCmd, FCursor, 1);
    end
  else
    case Button of
      VK_TAB: IncrementalFind;
      VK_END: FCursor:=Length(FCurCmd)+1;
      VK_HOME: FCursor:=1;
      VK_UP: begin
               if FCurCmdHist>0 then Dec(FCurCmdHist);
               FCurCmd:=Console.CommandsHistory[FCurCmdHist];
               FCursor:=Length(FCurCmd)+1;
             end;
      VK_DOWN: begin
                 if FCurCmdHist<Console.CommandsHistoryCount then Inc(FCurCmdHist);
                 FCurCmd:=Console.CommandsHistory[FCurCmdHist];
                 FCursor:=Length(FCurCmd)+1;
               end;
      192: Game.SwitchState(FGameState); // ~
    end;
end;

procedure TStateConsole.CharEvent(C: Char);
begin
  case Ord(C) of
    VK_BACK: if FCursor>1 then
       begin
         Delete(FCurCmd, FCursor-1, 1);
         Dec(FCursor);
       end;
    VK_RETURN: begin
                 Console.ExecCommand(FCurCmd);
                 FCurCmdHist:=Console.CommandsHistoryCount;
                 FCurCmd:='';
                 FCursor:=1;
               end;
    31..95, 97..255: begin
                       Insert(c, FCurCmd, FCursor);
                       Inc(FCursor);
                     end;
  end;
end;

//Private

function TStateConsole.GetName: string;
begin
  Result:='Console';
end;

function TStateConsole.LineLength(const Line: string): Integer;
var
  i: Integer;
  IsTag: Boolean;
begin
  Result:=0;
  IsTag:=false;
  for i:=1 to Length(Line) do
    if Line[i]=Tag
      then IsTag:=true
      else
        if IsTag
          then IsTag:=false
          else Inc(Result);
end;

procedure TStateConsole.GetEnd(var CurLine, SubLine: Cardinal);
var
  i, StrLines, Accum: Integer;
begin
  i:=Console.HistoryCount-1;
  Accum:=0;
  while Accum<16 do
  begin
    StrLines:=Ceil(LineLength(Console[i])/80);
    if StrLines=0 then StrLines:=1;
    Inc(Accum, StrLines);
    Dec(i);
  end;
  CurLine:=i+1;
  SubLine:=Accum-16;
end;

procedure TStateConsole.SplitLine(const Line: string);
var
  i, Len: Integer;
  S: string;
  IsTag: Boolean;
begin
  FList.Clear;
  Len:=0;
  S:='';
  IsTag:=false;
  for i:=1 to Length(Line) do
  begin
    S:=S+Line[i];
    if Line[i]=Tag
      then IsTag:=true
      else
        if IsTag
          then IsTag:=false
          else Inc(Len);
    if Len=80 then
    begin
      FList.Add(S);
      S:='';
      Len:=0;
    end;
  end;
  FList.Add(S);
end;

procedure TStateConsole.Adding(Sender: TObject);
begin
  if Assigned(Game) and (Game.State=FConsoleState) and not FNotEnd then GetEnd(FCurLine, FSubLine);
end;

end.
