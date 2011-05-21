unit MForm;

interface

uses
  Windows, Messages, AvL, avlUtils;

type
  PBtn=^TBtn;
  TBtn=record
    Rect: TRect;
    Text: string;
    Icon, IconH, IconD: hIcon;
    IconWidth, IconHeight, Data: Integer;
    OnClick: procedure(Btn: PBtn) of object;
  end;
  TGame=record
    CmdLine, DLLs: string;
  end;
  TMainForm=class(TForm)
    procedure IconClick(Btn: PBtn);
    procedure MinimizeClick(Btn: PBtn);
    procedure ExitClick(Btn: PBtn);
  private
    FImage, FDrawBuffer: TBitmap;
    FButtonsTimer: TTimer;
    FActive, FDown: Integer;
    FFont: HFont;
    FButtons: array of TBtn;
    FGames: array of TGame;
  protected
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure FormPaint(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonsTick(Sender: TObject);
  public
    constructor Create;
  end;

const
  FormCaption='Half-Life';

var
  MainForm: TMainForm;

implementation

function PointInRect(X, Y: Integer; Rect: TRect): Boolean;
begin
  Result:=(X>=Rect.Left) and (X<=Rect.Right) and
          (Y>=Rect.Top) and (Y<=Rect.Bottom);
end;

constructor TMainForm.Create;
var
  Games: TIniFile;
  GamesCount, i: Integer;
begin
  inherited Create(nil, FormCaption);
  Games:=TIniFile.Create(ExePath+'Launcher\Games.ini');
  GamesCount:=Games.ReadInteger('Games', 'Count', 0);
  SetSize(480, 275+75*((GamesCount-1) div 4));
  Position:=poScreenCenter;
  BorderStyle:=bsNone;
  BorderIcons:=[biMinimize];
  CanvasInit;
  OnPaint:=FormPaint;
  OnMouseMove:=FormMouseMove;
  OnMouseDown:=FormMouseDown;
  OnMouseUp:=FormMouseUp;
  OnDestroy:=FormDestroy;
  SetLength(FButtons, GamesCount+2);
  SetLength(FGames, GamesCount);
  for i:=0 to GamesCount-1 do
  begin
    with FButtons[i] do
    begin
      Rect.Left:=(i mod 4)*120;
      Rect.Right:=Rect.Left+120;
      Rect.Top:=202+(i div 4)*75;
      Rect.Bottom:=Rect.Top+70;
      Text:=Games.ReadString('Games', IntToStr(i), '');
      Icon:=LoadImage(hInstance, PChar(ExePath+Games.ReadString(IntToStr(i), 'Icon', '')), IMAGE_ICON, 48, 48, LR_LOADFROMFILE);
      IconH:=Icon;
      IconD:=Icon;
      IconWidth:=48;
      IconHeight:=48;
      Data:=i;
      OnClick:=IconClick;
    end;
    with FGames[i] do
    begin
      CmdLine:=Games.ReadString(IntToStr(i), 'CmdLine', '');
      DLLs:=Games.ReadString(IntToStr(i), 'DLLs', '');
    end;
  end;
  with FButtons[GamesCount] do
  begin
    Rect:=AvL.Rect(ClientWidth-38, 0, ClientWidth-19, 19);
    Text:='';
    Icon:=LoadImage(hInstance, 'MIN', IMAGE_ICON, 19, 19, 0);
    IconH:=LoadImage(hInstance, 'MINH', IMAGE_ICON, 19, 19, 0);
    IconD:=LoadImage(hInstance, 'MIND', IMAGE_ICON, 19, 19, 0);
    IconWidth:=19;
    IconHeight:=19;
    OnClick:=MinimizeClick;
  end;
  with FButtons[GamesCount+1] do
  begin
    Rect:=AvL.Rect(ClientWidth-19, 0, ClientWidth, 19);
    Text:='';
    Icon:=LoadImage(hInstance, 'CLOSE', IMAGE_ICON, 19, 19, 0);
    IconH:=LoadImage(hInstance, 'CLOSEH', IMAGE_ICON, 19, 19, 0);
    IconD:=LoadImage(hInstance, 'CLOSED', IMAGE_ICON, 19, 19, 0);
    IconWidth:=19;
    IconHeight:=19;
    OnClick:=ExitClick;
  end;
  FImage:=TBitmap.Create;
  FImage.Handle:=LoadImage(hInstance, 'BG', IMAGE_BITMAP, 0, 0, 0);
  FDrawBuffer:=TBitmap.CreateNew(ClientWidth, ClientHeight);
  FActive:=-1;
  FDown:=-1;
  FFont:=CreateFont(-11, 0, 0, 0, FW_BOLD, 0, 0, 0, 1, 0, 0, 0, FF_DONTCARE, 'Arial');
  FButtonsTimer:=TTimer.CreateEx(50, true);
  FButtonsTimer.OnTimer:=ButtonsTick;
end;

procedure TMainForm.IconClick(Btn: PBtn);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  SR: TSearchRec;
  DLLsDir: string;
begin
  ShowWindow(Handle, SW_HIDE);
  DLLsDir:=AddTrailingBackslash(ExePath+'Launcher\'+FGames[Btn.Data].DLLs);
  if FindFirst(DLLsDir+'*.dll', faAnyFile and not faDirectory, SR)=0 then
    repeat
      CopyFile(PChar(DLLsDir+SR.Name), PChar(ExePath+SR.Name), false);
    until FindNext(SR)<>0;
  FindClose(SR);
  ZeroMemory(@SI, SizeOf(SI));
  SI.cb:=SizeOf(SI);
  if not CreateProcess(nil, PChar(FGames[Btn.Data].CmdLine), nil, nil, false, CREATE_DEFAULT_ERROR_MODE, nil, PChar(ExePath), SI, PI) then
    ShowMessage('CreateProcess error:'#13#10+SysErrorMessage(GetLastError));
  Close;
end;

procedure TMainForm.MinimizeClick(Btn: PBtn);
begin
  ShowWindow(Handle, SW_MINIMIZE);
end;

procedure TMainForm.ExitClick(Btn: PBtn);
begin
  Close;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DeleteObject(FFont);
  FAN(FImage);
  FAN(FDrawBuffer);
  Finalize(FButtons);
  Finalize(FGames);
end;

procedure TMainForm.ButtonsTick(Sender: TObject);
var
  i: Integer;
  P: TPoint;
begin
  GetCursorPos(P);
  Dec(P.X, Left);
  Dec(P.Y, Top);
  for i:=0 to High(FButtons) do
    if PointInRect(P.X, P.Y, FButtons[i].Rect) then
    begin
      FActive:=i;
      InvalidateRect(Handle, nil, false);
      Exit;
    end;
  FActive:=-1;
  InvalidateRect(Handle, nil, false);
end;

procedure TMainForm.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  if ((Msg.YPos-Top)<200) and not PointInRect(Msg.XPos-Left, Msg.YPos-Top,
    Rect(ClientWidth-38, 0, ClientWidth, 19))
    then Msg.Result:=HTCAPTION
    else Msg.Result:=HTCLIENT;
end;

procedure TMainForm.FormPaint(Sender: TObject);
var
  i: Integer;
  OldFont: HFont;
  CurIcon: HICON;
  PS: TPaintStruct;
begin
  if not (Assigned(FDrawBuffer) and Assigned(FImage)) then Exit;
  BeginPaint(Handle, PS);
  with FDrawBuffer.Canvas do
  begin
    Brush.Color:=$000000;
    Pen.Color:=$0094FF;
    SetBkMode(Handle, TRANSPARENT);
    OldFont:=SelectObject(Handle, FFont);
    Brush.Style:=bsSolid;
    FillRect(Rect(0, 0, ClientWidth, ClientHeight));
    Brush.Style:=bsClear;
    FImage.Draw(Handle, 0, 0);
    for i:=0 to High(FButtons) do
      with FButtons[i] do
      begin
        Brush.Style:=bsClear;
        Pen.Width:=1;
        CurIcon:=Icon;
        if FActive=i then
          if (FDown<>FActive)
            then CurIcon:=IconH
            else CurIcon:=IconD;
        DrawIconEx(Handle, Rect.Left+((Rect.Right-Rect.Left-IconWidth) div 2), Rect.Top, CurIcon, IconWidth, IconHeight, 0, Brush.Handle, 0);
        if i=FActive
          then SetTextColor(Handle, $00FFFF)
          else SetTextColor(Handle, $0094FF);
        if Text<>''
          then DrawText(Handle, PChar(Text), -1, Rect, DT_CENTER or DT_BOTTOM or DT_SINGLELINE or DT_PLOTTER);
      end;
    if OldFont<>0 then SelectObject(Handle, OldFont);
  end;
  FDrawBuffer.Draw(Canvas.Handle, 0, 0);
  EndPaint(Handle, PS);
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  for i:=0 to High(FButtons) do
    if PointInRect(X, Y, FButtons[i].Rect) then
    begin
      FActive:=i;
      InvalidateRect(Handle, nil, false);
      Exit;
    end;
  FActive:=-1;
  InvalidateRect(Handle, nil, false);
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button<>mbLeft then Exit;
  FDown:=FActive;
  SetCapture(Handle);
  InvalidateRect(Handle, nil, false);
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button<>mbLeft then Exit;
  if (FDown=FActive) and (FDown>=0) and (FDown<=High(FButtons)) then
    if Assigned(FButtons[FActive].OnClick) then FButtons[FActive].OnClick(@FButtons[FActive]);
  FDown:=-1;
  ReleaseCapture;
  InvalidateRect(Handle, nil, false);
end;

end.
