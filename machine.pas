unit Machine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Cpu8088, VideoController, Hardware, Timer, Cassette;

type

  { TMachine }

  TMachine = class(TComponent)
  private
    FCassetteDrive: TCassetteDrive;
    FCpu: TCpu8088;
    FIOBus: IIOBus;
    FMemoryBus: IMemoryBus;
    FVideo: TVideoController;
    FTimer: TPit8253;
    FMemory: specialize TArray<IMemoryBusDevice>;
    procedure SetCassetteDrive(AValue: TCassetteDrive);
    procedure SetCpu(AValue: TCpu8088);
    procedure SetIOBus(AValue: IIOBus);
    procedure SetMemoryBus(AValue: IMemoryBus);
  published
    property Cpu: TCpu8088 read FCpu write SetCpu;
    property MemoryBus: IMemoryBus read FMemoryBus write SetMemoryBus;
    property IOBus: IIOBus read FIOBus write SetIOBus;
    property Video: TVideoController read FVideo;
    property Timer: TPit8253 read FTimer;
    property CassetteDrive: TCassetteDrive read FCassetteDrive write SetCassetteDrive;
    procedure Tick;
    procedure Run(ATicks: Integer=1000);
    procedure Reset;
    procedure Initialize;
    procedure InstallCpu(ACpu: TCpu8088);
    procedure InstallMemoryBus(AMemoryBus: IMemoryBus);
    procedure InstallIOBus(AIOBus: IIOBus);
    procedure InstallMemory(AMemory: IMemoryBusDevice);
    procedure InstallVideo(AVideo: TVideoController);
    procedure InstallTimer(ATimer: TPit8253);
  end;

implementation

{ TMachine }

procedure TMachine.SetCpu(AValue: TCpu8088);
begin
  if FCpu = AValue then Exit;
  FCpu := AValue;
end;

procedure TMachine.SetCassetteDrive(AValue: TCassetteDrive);
begin
  if FCassetteDrive = AValue then Exit;
  FCassetteDrive := AValue;
end;

procedure TMachine.SetIOBus(AValue: IIOBus);
begin
  if FIOBus = AValue then Exit;
  FIOBus := AValue;
end;

procedure TMachine.SetMemoryBus(AValue: IMemoryBus);
begin
  if FMemoryBus = AValue then Exit;
  FMemoryBus := AValue;
end;

procedure TMachine.Tick;
begin
  Cpu.Tick;
  Timer.Tick;
  Video.Tick;
end;

procedure TMachine.Run(ATicks: Integer);
var
  I: Integer;
begin
  for I := 1 to ATicks do Tick;
end;

procedure TMachine.Reset;
begin
  Cpu.Reset;
  Timer.Reset;
end;

procedure TMachine.Initialize;
var
  Errors: TStringList;
  MemoryBlock: IMemoryBusDevice;
begin
  Errors := TStringList.Create;
  Errors.Delimiter := ';';
  try
    if not Assigned(Cpu) then Errors.Add('No Cpu installed');
    if not Assigned(MemoryBus) then Errors.Add('No Memory Bus installed');
    if not Assigned(IOBus) then Errors.Add('No IO bus installed');
    if Errors.Count > 0 then
    begin
      raise Exception.CreateFmt(
        'Invalid confiruration: %s',
        [Errors.DelimitedText]);
    end;
  finally
    FreeAndNil(Errors);
  end;

  IOBus.AttachDevice(Cpu);
  MemoryBus.AttachDevice(Cpu);

  if Assigned(FVideo) then
  begin
    MemoryBus.AttachDevice(FVideo);
    IOBus.AttachDevice(FVideo);
  end;

  if Assigned(FTimer) then
  begin
    { Todo: wire through a i8259 }
    IOBus.AttachDevice(FTimer);
  end;

  if Assigned(FCassetteDrive) then
    IOBus.AttachDevice(FCassetteDrive);

  for MemoryBlock in FMemory do
    MemoryBus.AttachDevice(MemoryBlock);

  Cpu.Reset;
end;

procedure TMachine.InstallCpu(ACpu: TCpu8088);
begin
  Cpu := ACpu;
end;

procedure TMachine.InstallMemoryBus(AMemoryBus: IMemoryBus);
begin
  MemoryBus := AMemoryBus;
end;

procedure TMachine.InstallIOBus(AIOBus: IIOBus);
begin
  IOBus := AIOBus;
end;

procedure TMachine.InstallMemory(AMemory: IMemoryBusDevice);
begin
  Insert(AMemory, FMemory, Integer.MaxValue);
end;

procedure TMachine.InstallVideo(AVideo: TVideoController);
begin
  FVideo := AVideo;
end;

procedure TMachine.InstallTimer(ATimer: TPit8253);
begin
  FTimer := ATimer;
end;

end.

