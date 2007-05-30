unit VSETexLoader;

interface

uses
  AvL, avlUtils, avlClasses, dglOpenGL, VSEManagers;

type
  TTextureData=class(TDLCListItem)
  private

  public
    //Width, Height, Components: Word;
    //Format, Target, ComponentFormat: TGLenum;
    //Data: Pointer;
  end;
  TTexFormatLoader=class(TDLCListItem)
  public
    constructor Create;
    function  CheckFormat(Data: TStream): Boolean; virtual; abstract;
    function  LoadTextureData(Data: TStream): TTextureData; virtual; abstract;
  end;
  TTexLoader=class(TManager)
  private
    FTFLRoot: TTexFormatLoader;
    FTexDataRoot: TTextureData;
    function  CheckFormat(Item: TDLCListItem; Data: Integer): Boolean;
  protected
    procedure Init; override;
    procedure Cleanup; override;
  public
    constructor Create;
    destructor Destroy; override;
    function  LoadTextureData(Data: TStream): TTextureData;
  end;

var
  TexLoader: TTexLoader;

implementation

uses
  VSELog;

{TTexFormatLoader}

constructor TTexFormatLoader.Create;
begin
  if Assigned(TexLoader) then
  begin
    inherited Create(TexLoader.FTFLRoot);
    if not Assigned(TexLoader.FTFLRoot) then TexLoader.FTFLRoot:=Self;
  end
    else raise Exception.Create('Cannot register TextureFormatLoader '+ClassName);
end;

{TTexLoader}

constructor TTexLoader.Create;
begin
  LogNC(llInfo, 'TexLoader: Create');
  inherited Create;
end;

destructor TTexLoader.Destroy;
begin
  LogNC(llInfo, 'TexLoader: Destroy');
  if Assigned(FTexDataRoot) then FTexDataRoot.ClearList;
  FAN(FTexDataRoot);
  if Assigned(FTFLRoot) then FTFLRoot.ClearList;
  FAN(FTFLRoot);
  if TexLoader=Self then TexLoader:=nil;
  inherited Destroy;
end;

procedure TTexLoader.Init;
begin
  Log(llInfo, 'TexLoader: Initialize');
  FTexDataRoot:=nil;
end;

procedure TTexLoader.Cleanup;
begin
  Log(llInfo, 'TexLoader: Cleanup');
  if Assigned(FTexDataRoot) then FTexDataRoot.ClearList;
  FAN(FTexDataRoot);
end;

function TTexLoader.LoadTextureData(Data: TStream): TTextureData;
var
  TexFormatLoader: TTexFormatLoader;
begin
  Result:=nil;
  if (not Assigned(Data)) or (Data.Size=0) then
  begin
    Log(llError, 'TexLoader: LoadTexture failed: Data is void');
    Exit;
  end;
  if not Assigned(FTFLRoot) then
  begin
    Log(llError, 'TexLoader: LoadTexture failed: no format loaders registered');
    Exit;
  end;
  TexFormatLoader:=TTexFormatLoader(FTFLRoot.FindItem(CheckFormat, Integer(Data)));
  if Assigned(TexFormatLoader)
    then Result:=TexFormatLoader.LoadTextureData(Data)
    else begin
      Result:=nil;
      Log(llError, 'TexLoader: LoadTexture failed: no appropriate format loader found');
    end;
end;

function TTexLoader.CheckFormat(Item: TDLCListItem; Data: Integer): Boolean;
begin
  Result:=TTexFormatLoader(Item).CheckFormat(TStream(Data));
end;

initialization

  TexLoader:=TTexLoader.Create;

end.
