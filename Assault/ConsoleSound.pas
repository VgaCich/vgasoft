unit ConsoleSound;

interface

uses
  Windows, AvL, avlUtils;

type
  TConsoleSound=class
  private
    function CreateStream(const Args: string): Boolean;
    function ChannelPlay(const Args: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses UGame, ULog, PakMan, UConsole, USound, BASS;

var
  Channels: array[0..7] of TSoundChannel;

constructor TConsoleSound.Create;
begin
  inherited Create;
  Console.RegisterCommand('snd_createstream', '', CreateStream);
  Console.RegisterCommand('snd_playchannel', '', ChannelPlay);
end;

destructor TConsoleSound.Destroy;
begin
  inherited Destroy;
end;

function TConsoleSound.CreateStream(const Args: string): Boolean;
var
  i, Index: Integer;
  Data: TStream;
begin
  Result:=false;
  Index:=-1;
  for i:=0 to 7 do
    if Channels[i]=nil then
    begin
      Index:=i;
      Break;
    end;
  if Index<0 then
  begin
    Log('ConsoleSound: none free streams');
    Exit;
  end;
  Data:=Game.PakMan.OpenFile(Args, ofNoCreate);
  if Data=nil then
  begin
    LogF('ConsoleSound: cannot load sound stream "%s". File not found', [Args]);
    Exit;
  end;
  Channels[Index]:=TSoundStream.Create(Data, 0);
  if Channels[Index].Handle=0 then
  begin
    Log('ConsoleSound: stream creating error');
    Exit;
  end;
  Console.AddToConsole('Stream successfully loaded. Index='+IntToStr(Index));
  Result:=true;
end;

function TConsoleSound.ChannelPlay(const Args: string): Boolean;
var
  Index: Integer;
begin
  Result:=false;
  Index:=StrToInt(Args);
  if (Index<0) or (Index>7) or (Channels[Index]=nil) then begin
    Log('ConsoleSound: incorrect stream index '+IntToStr(Index));
    Exit;
  end;
  Channels[Index].Play(true);
end;

end.