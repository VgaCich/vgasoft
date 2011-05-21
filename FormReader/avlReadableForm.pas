//(c)VgaSoft, 2004-2011
unit avlReadableForm;

interface

uses
  AvL, avlUtils, avlFormReader;

type
  TReadableForm=class(TForm)
  private
    function  GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
  protected
    FFormReader: TFormReader;
  public
    constructor Create(AParent: TWinControl; FormData: TStream);
    destructor Destroy; override;
  end;
  TReadableMDIChildForm=class(TMDIChildForm)
  private
    function  GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
  protected
    FFormReader: TFormReader;
  public
    constructor Create(AParent: TMDIForm; FormData: TStream);
    destructor Destroy; override;
  end;
  TReadableMDIForm = class(TMDIForm)
  private
    function  GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
  protected
    FFormReader: TFormReader;
  public
    constructor Create(AParent: TWinControl; FormData: TStream);
    destructor Destroy; override;
  end;

implementation

type
  TRFReader=class(TFormReader)
  protected
    FForm: TForm;
    function AddReadableForm(Properties: TStringList; Parent: TWinControl): TWinControl;
  public
    constructor Create(Form: TForm; FData: TStream);
    function ReadForm: Boolean; override;
  end;

{ TRFReader }

constructor TRFReader.Create(Form: TForm; FData: TStream);
begin
  inherited Create;
  FormData:=FData;
  FForm:=Form;
  AddControlType(FForm.ClassName, AddReadableForm);
end;

function TRFReader.AddReadableForm(Properties: TStringList; Parent: TWinControl): TWinControl;
begin
  Result:=FForm;
  SetWinControlProperties(Result, Properties);
  SetFormProperties(Result, Properties);
end;

function TRFReader.ReadForm: Boolean;
var
  Controls: TStringList;
  i: Integer;
  Field: Pointer;
begin
  try
    Result:=inherited ReadForm;
    Controls:=TStringList.Create;
    try
      GetControlsList(Controls);
      for i:=0 to Controls.Count-1 do
      begin
        Field:=FForm.FieldAddress(Controls[i]);
        if Field=nil then Continue;
        TObject(Field^):=Control[Controls[i]];
      end;
    finally
      FAN(Controls);
    end;
  finally
    DeleteControlFromList(FForm);
  end;
end;

{ TReadableForm }

constructor TReadableForm.Create(AParent: TWinControl; FormData: TStream);
begin
  inherited Create(AParent, '');
  FFormReader:=TRFReader.Create(Self, FormData);
  FFormReader.OnEventHandlerRequired:=GetEventHandler;
  if not FFormReader.ReadForm
    then raise Exception.Create('Failed to read form '+ClassName);
end;

destructor TReadableForm.Destroy;
begin
  FAN(FFormReader);
  inherited Destroy;
end;

function TReadableForm.GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
begin
  Result.Data:=Self;
  Result.Code:=MethodAddress(HandlerName);
end;

{ TReadableMDIChildForm }

constructor TReadableMDIChildForm.Create(AParent: TMDIForm; FormData: TStream);
begin
  inherited Create(AParent, '');
  FFormReader:=TRFReader.Create(Self, FormData);
  FFormReader.OnEventHandlerRequired:=GetEventHandler;
  if not FFormReader.ReadForm
    then raise Exception.Create('Failed to read form '+ClassName);
end;

destructor TReadableMDIChildForm.Destroy;
begin
  FAN(FFormReader);
  inherited Destroy;
end;

function TReadableMDIChildForm.GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
begin
  Result.Data:=Self;
  Result.Code:=MethodAddress(HandlerName);
end;

{ TReadableMDIForm }

constructor TReadableMDIForm.Create(AParent: TWinControl; FormData: TStream);
begin
  inherited Create(AParent, '');
  FFormReader:=TRFReader.Create(Self, FormData);
  FFormReader.OnEventHandlerRequired:=GetEventHandler;
  if not FFormReader.ReadForm
    then raise Exception.Create('Failed to read form '+ClassName);
end;

destructor TReadableMDIForm.Destroy;
begin
  FAN(FFormReader);
  inherited Destroy;
end;

function TReadableMDIForm.GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
begin
  Result.Data:=Self;
  Result.Code:=MethodAddress(HandlerName);
end;

end.
