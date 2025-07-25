unit Debugger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPHttpServer, FPJson, JsonParser, HttpRoute, HttpDefs,
  Hardware, Cpu8088;

type

  { TWebDebugger }

  TWebDebugger = class(TComponent)
  private
    type

      { TServer }

      TServer = class(TThread)
        Cpu: TCpu8088;
        Router: THTTPRouter;
        constructor Create(
          CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
        destructor Destroy; override;
        procedure Execute; override;
        procedure RegisterRoutes;
        procedure RequestHandler(Sender: TObject;
          var ARequest: TFPHTTPConnectionRequest;
          var AResponse : TFPHTTPConnectionResponse);

        procedure GetRegisters(ARequest: TRequest; AResponse: TResponse);
      private
        procedure GetMemory(ARequest: TRequest; AResponse: TResponse);
        procedure NotFound(ARequest: TRequest; AResponse: TResponse);
      end;
  private
    FServer: TServer;
    procedure SetServer(AValue: TServer);
  public
    const Port = 3456;
  private
    FCpu: TCpu8088;
    property Server: TServer read FServer write SetServer;
    procedure SetCpu(AValue: TCpu8088);
  public
    property Cpu: TCpu8088 read FCpu write SetCpu;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TWebDebugger }

procedure TWebDebugger.SetServer(AValue: TServer);
begin
  if FServer = AValue then Exit;
  FServer := AValue;
end;

procedure TWebDebugger.SetCpu(AValue: TCpu8088);
begin
  if FCpu = AValue then Exit;
  FCpu := AValue;
end;

destructor TWebDebugger.Destroy;
begin
  inherited Destroy;
  if Assigned(Server) then Stop;
end;

procedure TWebDebugger.Start;
begin
  Server := TServer.Create(True);
  Server.Cpu := Cpu;
  Server.FreeOnTerminate := True;
  Server.Start;
end;

procedure TWebDebugger.Stop;
begin
  Server.Terminate;
  Server := Nil;
end;

{ TWebDebugger.TServer }

constructor TWebDebugger.TServer.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  Router := THTTPRouter.Create(Nil);
  RegisterRoutes;
end;

destructor TWebDebugger.TServer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Router);
end;

procedure TWebDebugger.TServer.Execute;
var
  Server: TFPHttpServer;
begin
  Server := TFPHttpServer.Create(Nil);
  Server.Port := 3456;
  Server.OnRequest := @RequestHandler;
  Server.Active := True;
  while not Terminated do Sleep(50);

  FreeAndNil(Server);
end;

procedure TWebDebugger.TServer.RegisterRoutes;
begin
  Router.RegisterRoute('/memory', rmGet, @GetMemory);
  Router.RegisterRoute('/registers', rmGet, @GetRegisters);
  Router.RegisterRoute('', @NotFound, True);
end;

procedure TWebDebugger.TServer.RequestHandler(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  Router.RouteRequest(ARequest, AResponse);
end;

procedure TWebDebugger.TServer.NotFound(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.SetStatus(404);
  AResponse.Content := 'Not found';
end;

procedure TWebDebugger.TServer.GetMemory(ARequest: TRequest; AResponse: TResponse);
var
  Address: TPhysicalAddress;
  Length_, I: Integer;
  Result: TJSONArray;
  Data: Byte;
begin
  try
    Address := StrToInt(ARequest.QueryFields.Values['address']);
    Length_ := StrToInt(ARequest.QueryFields.Values['length']);
  except
    AResponse.SetStatus(400);
    Exit;
  end;

  Result := TJSONArray.Create;
  for I := 1 to Length_ do
  begin
    Cpu.MemoryBus.InvokeRead(Nil, Address + I, Data);
    Result.Add(Data);
  end;

  AResponse.Content := Result.AsJSON;
  AResponse.ContentType := 'application/json';
  FreeAndNil(Result);
end;

procedure TWebDebugger.TServer.GetRegisters(ARequest: TRequest; AResponse: TResponse);
var
  Result: TJSONObject;
begin
  Result := TJSONObject.Create([
    'ax', Cpu.Registers.AX,
    'bx', Cpu.Registers.BX,
    'cx', Cpu.Registers.CX,
    'dx', Cpu.Registers.DX,

    'bp', Cpu.Registers.BP,
    'sp', Cpu.Registers.SP,
    'si', Cpu.Registers.SI,
    'di', Cpu.Registers.DI,

    'cs', Cpu.Registers.BP,
    'ds', Cpu.Registers.SP,
    'ss', Cpu.Registers.SI,
    'es', Cpu.Registers.DI,

    'ip', Cpu.Registers.IP,
    'flags', Cpu.Registers.Flags.GetWord
  ]);

  AResponse.Content := Result.FormatJSON;
  AResponse.ContentType := 'application/json';
  FreeAndNil(Result);
end;

end.

