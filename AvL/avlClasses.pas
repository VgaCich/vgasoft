//(c) VgaSoft, 2004-2007
unit avlClasses;

interface

uses
  Windows, AvL;

type
  TDLCListItem=class;
  TDLCListCheckFunc=function(Item: TDLCListItem; Data: Integer): Boolean of object;
  TDLCListItem=class
  private
    FNext, FPrev: TDLCListItem;
    procedure Remove;
  public
    constructor Create(PrevItem: TDLCListItem);
    destructor Destroy; override;
    procedure ClearList;
    procedure ResetList;
    function  FindItem(CheckFunc: TDLCListCheckFunc; Data: Integer): TDLCListItem;
  end;

implementation

constructor TDLCListItem.Create(PrevItem: TDLCListItem);
begin
  inherited Create;
  if Assigned(PrevItem) then
  begin
    FNext:=PrevItem.FNext;
    FPrev:=PrevItem;
    PrevItem.FNext:=Self;
    FNext.FPrev:=Self;
  end
  else begin
    FNext:=Self;
    FPrev:=Self;
  end;
end;

destructor TDLCListItem.Destroy;
begin
  Remove;
  inherited Destroy;
end;

procedure TDLCListItem.Remove;
begin
  if (FPrev<>Self) and (FNext<>Self) and Assigned(FPrev) and Assigned(FNext) then
  begin
    FPrev.FNext:=FNext;
    FNext.FPrev:=FPrev;
  end;
end;

procedure TDLCListItem.ClearList;
begin
  if not Assigned(FNext) then Exit;
  while FNext<>Self do FNext.Free;
end;

procedure TDLCListItem.ResetList;
begin
  Remove;
  FNext:=Self;
  FPrev:=Self;
end;

function TDLCListItem.FindItem(CheckFunc: TDLCListCheckFunc; Data: Integer): TDLCListItem;
begin
  if CheckFunc(Self, Data) then
  begin
    Result:=Self;
    Exit;
  end;
  Result:=Self.FNext;
  while (Result<>Self) do
    if CheckFunc(Result, Data)
      then Exit
      else Result:=Result.FNext;
  Result:=nil;
end;

end.