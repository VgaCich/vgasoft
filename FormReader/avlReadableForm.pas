unit avlReadableForm;

interface

uses
  AvL, avlUtils, avlFormReader;

type
  TReadableForm=class(TForm)
  private
    function  AddReadableForm(Properties: TStringList; Parent: TWinControl): TWinControl;
    function  GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
  protected
    FFormReader: TFormReader;
  public
    constructor Create(AParent: TWinControl; FormData: TStream);
    destructor Destroy; override;
  end;

implementation

constructor TReadableForm.Create(AParent: TWinControl; FormData: TStream);
var
  Controls: TStringList;
  i: Integer;
  Field: Pointer;
begin
  inherited Create(AParent, '');
  FFormReader:=TFormReader.Create;
  FFormReader.AddControlType(ClassName, AddReadableForm);
  FFormReader.OnEventHandlerRequired:=GetEventHandler;
  FFormReader.FormData:=FormData;
  if not FFormReader.ReadForm
    then raise Exception.Create('Failed to read form '+ClassName);
  Controls:=TStringList.Create;
  try
    FFormReader.GetControlsList(Controls);
    for i:=0 to Controls.Count-1 do
    begin
      if ClassNameIs(FFormReader.ControlType[Controls[i]]) then Continue;
      Field:=FieldAddress(Controls[i]);
      if Field=nil then Continue;
      TObject(Field^):=FFormReader[Controls[i]];
    end;
  finally
    FAN(Controls);
  end;
end;

destructor TReadableForm.Destroy;
begin
  FAN(FFormReader);
  inherited Destroy;
end;

function TReadableForm.AddReadableForm(Properties: TStringList; Parent: TWinControl): TWinControl;
var
  I: Integer;
  S: string;
  B: Boolean;
begin
  Result:=Self;
  FFormReader.SetWinControlProperties(Result, Properties);
  Width:=2*Width-ClientWidth;
  Height:=2*Height-ClientHeight;
  if FFormReader.GetStrProperty('Caption', Properties, S) then Caption:=S;
  if FFormReader.GetIntProperty('AlphaBlendValue', Properties, I) then AlphaBlendValue:=I;
  if FFormReader.GetBoolProperty('AlphaBlend', Properties, B) then AlphaBlend:=B;
  if FFormReader.GetIntProperty('TransparentColorValue', Properties, I) then TransparentColorValue:=I;
  if FFormReader.GetBoolProperty('TransparentColor', Properties, B) then TransparentColor:=B;
  if FFormReader.GetIntProperty('BorderStyle', Properties, I) then BorderStyle:=TFormBorderStyle(I);
  if FFormReader.GetIntProperty('Position', Properties, I) then Position:=I;
  if FFormReader.GetIntProperty('WindowState', Properties, I) then WindowState:=I;
  if FFormReader.GetIntProperty('BorderIcons', Properties, I) then
  begin
    if (I and $1)<>0
      then BorderIcons:=[biSystemMenu]
      else BorderIcons:=[];
    if (I and $2)<>0 then BorderIcons:=BorderIcons+[biMinimize];
    if (I and $4)<>0 then BorderIcons:=BorderIcons+[biMaximize];
    if (I and $8)<>0 then BorderIcons:=BorderIcons+[biHelp];
  end;
end;

function TReadableForm.GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
begin
  Result.Data:=Self;
  Result.Code:=MethodAddress(HandlerName);
end;

end.
