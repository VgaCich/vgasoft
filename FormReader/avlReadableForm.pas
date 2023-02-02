unit avlReadableForm;

interface

uses
  AvL, avlUtils, avlFormReader;

type
  TReadableForm = class(TForm)
  private
    function  GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
  protected
    FFormReader: TFormReader;
  public
    constructor Create(AParent: TWinControl; FormData: TStream);
    destructor Destroy; override;
  end;
  TReadableMDIChildForm = class(TMDIChildForm)
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
  TRFReader = class(TFormReader)
  protected
    FForm: TForm;
    function AddReadableForm(Properties: TControlProperties; Parent: TWinControl): TWinControl;
  public
    constructor Create(Form: TForm);
    procedure ReadForm(Data: TStream); override;
  end;

{ TRFReader }

constructor TRFReader.Create(Form: TForm);
begin
  inherited Create;
  FForm := Form;
  AddControlType(FForm.ClassName, AddReadableForm);
end;

function TRFReader.AddReadableForm(Properties: TControlProperties; Parent: TWinControl): TWinControl;
begin
  Result := FForm;
  FForm.Caption := Properties.Str('Caption');
  SetWinControlProperties(Result, Properties);
  SetFormProperties(Result as TForm, Properties);
end;

procedure TRFReader.ReadForm(Data: TStream);
var
  Controls: TStringList;
  i: Integer;
  Field: Pointer;
begin
  try
    inherited ReadForm(Data);
    Controls := TStringList.Create;
    try
      GetControlsList(Controls);
      for i := 0 to Controls.Count - 1 do
      begin
        Field := FForm.FieldAddress(Controls[i]);
        if not Assigned(Field) then Continue;
        TObject(Field^) := Control[Controls[i]];
      end;
    finally
      FAN(Controls);
    end;
  finally
    DeleteControl(FForm);
  end;
end;

{ TReadableForm }

constructor TReadableForm.Create(AParent: TWinControl; FormData: TStream);
begin
  inherited Create(AParent, '');
  FFormReader := TRFReader.Create(Self);
  FFormReader.OnEventHandlerRequired := GetEventHandler;
  FFormReader.ReadForm(FormData);
end;

destructor TReadableForm.Destroy;
begin
  FAN(FFormReader);
  inherited Destroy;
end;

function TReadableForm.GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
begin
  Result.Code := MethodAddress(HandlerName);
  Result.Data := Self;
end;

{ TReadableMDIChildForm }

constructor TReadableMDIChildForm.Create(AParent: TMDIForm; FormData: TStream);
begin
  inherited Create(AParent, '');
  FFormReader := TRFReader.Create(Self);
  FFormReader.OnEventHandlerRequired := GetEventHandler;
  FFormReader.ReadForm(FormData);
end;

destructor TReadableMDIChildForm.Destroy;
begin
  FAN(FFormReader);
  inherited Destroy;
end;

function TReadableMDIChildForm.GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
begin
  Result.Data := Self;
  Result.Code := MethodAddress(HandlerName);
end;

{ TReadableMDIForm }

constructor TReadableMDIForm.Create(AParent: TWinControl; FormData: TStream);
begin
  inherited Create(AParent, '');
  FFormReader := TRFReader.Create(Self);
  FFormReader.OnEventHandlerRequired := GetEventHandler;
  FFormReader.ReadForm(FormData);
end;

destructor TReadableMDIForm.Destroy;
begin
  FAN(FFormReader);
  inherited Destroy;
end;

function TReadableMDIForm.GetEventHandler(Sender: TObject; const HandlerName: string): TMethod;
begin
  Result.Data := Self;
  Result.Code := MethodAddress(HandlerName);
end;

end.
