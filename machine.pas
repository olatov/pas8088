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
    FFloppyDiskController: IFloppyDiskController;
    FInterrptController: IInterruptController;
    FIOBus: IIOBus;
    FMemoryBus: IMemoryBus;
    FVideo: TVideoController;
    FTimer: TPit8253;
    FMemory: specialize TArray<IMemoryBusDevice>;
    procedure SetCassetteDrive(AValue: TCassetteDrive);
    procedure SetCpu(AValue: TCpu8088);
    procedure SetFloppyDiskController(AValue: IFloppyDiskController);
    procedure SetInterrptController(AValue: IInterruptController);
    procedure SetIOBus(AValue: IIOBus);
    procedure SetMemoryBus(AValue: IMemoryBus);
    procedure OnTimerOutputChange(ASender: TObject; AChannel: Integer; AValue: Boolean);
  published
    property Cpu: TCpu8088 read FCpu write SetCpu;
    property MemoryBus: IMemoryBus read FMemoryBus write SetMemoryBus;
    property IOBus: IIOBus read FIOBus write SetIOBus;
    property Video: TVideoController read FVideo write FVideo;
    property Timer: TPit8253 read FTimer write FTimer;
    property InterrptController: IInterruptController read FInterrptController write SetInterrptController;
    property CassetteDrive: TCassetteDrive read FCassetteDrive write SetCassetteDrive;
    property FloppyDiskController: IFloppyDiskController read FFloppyDiskController write SetFloppyDiskController;
    procedure Tick;
    procedure Run(ATicks: Integer=1000);
    procedure Reset;
    procedure Initialize;
    procedure AddMemory(AMemory: IMemoryBusDevice);
  end;

implementation

{ TMachine }

procedure TMachine.SetCpu(AValue: TCpu8088);
begin
  if FCpu = AValue then Exit;
  FCpu := AValue;
end;

procedure TMachine.SetFloppyDiskController(AValue: IFloppyDiskController);
begin
  if FFloppyDiskController = AValue then Exit;
  FFloppyDiskController := AValue;
end;

procedure TMachine.SetInterrptController(AValue: IInterruptController);
begin
  if FInterrptController = AValue then Exit;
  FInterrptController := AValue;
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

procedure TMachine.OnTimerOutputChange(
  ASender: TObject; AChannel: Integer; AValue: Boolean);
begin
  if not AValue then Exit;

  case AChannel of
    0: InterrptController.RaiseIrq(0);
    1: InterrptController.RaiseIrq(6);
  else;
  end;
end;

procedure TMachine.Tick;
begin
  if Assigned(InterrptController) then InterrptController.Tick;
  Cpu.Tick;
  if Assigned(Timer) then Timer.Tick;
  if Assigned(Video) then Video.Tick;

  case CassetteDrive.State of
    csPlaying: Timer.TapeIn := CassetteDrive.TapeIn;
    csRecording: CassetteDrive.TapeOut := Timer.TapeOut;
  else;
  end;
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

  if Assigned(InterrptController) then
  begin
    IOBus.AttachDevice(InterrptController);
    InterrptController.AttachCpu(Cpu);
  end;

  if Assigned(FVideo) then
  begin
    MemoryBus.AttachDevice(FVideo);
    IOBus.AttachDevice(FVideo);
  end;

  if Assigned(FTimer) then
  begin
    { Todo: wire through a i8259 }
    IOBus.AttachDevice(FTimer);

    if Assigned(InterrptController) then
      FTimer.OnChannelOutputChange := @OnTimerOutputChange;
  end;

  if Assigned(FCassetteDrive) then
    IOBus.AttachDevice(FCassetteDrive);

  if Assigned(FloppyDiskController) then
    MemoryBus.AttachDevice(FloppyDiskController);

  for MemoryBlock in FMemory do
    MemoryBus.AttachDevice(MemoryBlock);

  Cpu.Reset;
end;

procedure TMachine.AddMemory(AMemory: IMemoryBusDevice);
begin
  Insert(AMemory, FMemory, Integer.MaxValue);
end;

end.

