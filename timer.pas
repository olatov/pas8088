unit Timer;

{$mode ObjFPC}{$H+}
{$R-}

interface

uses
  Classes, SysUtils, Math, Nullable,
  Hardware;

type

  { TPit8253 }

  TPit8253 = class(TComponent, IIOBusDevice)
  {
    Todo: implemeted as IIOBusDevice for now, must be re-wired properly
    throuh i8255 + i8259 as soon as these are available.
  }
  private
    const
      ChannelsCount = 3;
      BaseFrequency = 1250000;
    type
      { https://wiki.osdev.org/Programmable_Interval_Timer#I/O_Ports }
      TCountMode = (cmBinary = 0, cmBcd);
      TAccessMode = (amLatch = 0, amLoByte, amHiByte, amLoByteHiByte);
      TAccessState = (asLoByte, asHiByte);
      TOperatingMode = (
        omInterruptOnTerminalCount = 0,
        omHardwareReTriggerableOneShot,
        omRateGenerator,
        omSquareWaveGenerator,
        omSoftwareTriggeredStrobe,
        omHardwareTriggeredStrobe,
        omRateGeneratorDuplicate,
        omSquareWaveGeneratorDuplicate
      );

      { TChannel }

      TChannel = class(TComponent)
      public
        CountingMode: TCountMode;
        AccessMode: TAccessMode;
        AccessState: TAccessState;
        OperatingMode: TOperatingMode;
        Value, ReloadValue: Integer;
        Output: Boolean;
        Latch: specialize TNullable<Word>;
        procedure Tick;
        procedure WriteReloadValue(AValue: Byte);
        function ReadValue: Byte;
      end;

      TCommand = bitpacked record
        CountMode: TCountMode;
        OperatingMode: TOperatingMode;
        AccessMode: TAccessMode;
        Channel: 0..2;
      end;

  public
    type
      TTimerOutputNotify = procedure(ASender: TObject; AChannel: Integer; AValue: Boolean) of object;
  private
    FIOBus: IIOBus;
    FChannels: array[0..(ChannelsCount - 1)] of TChannel;
    FLatch: Word;
    FOnChannelOutputChange: TTimerOutputNotify;
    FTickBatchSize: Integer;
    function GetOutput(AChannel: Integer): Boolean;
    procedure SetOnChannelOutputChange(AValue: TTimerOutputNotify);
    procedure WriteChannelPort(AChannel: Integer; AValue: Byte);
    procedure WriteCommandPort(ACommand: TCommand);
  public
    constructor Create(AOwner: TComponent; AActualFrequency: Integer); reintroduce;
    procedure Tick;
    property OnChannelOutputChange: TTimerOutputNotify read FOnChannelOutputChange write SetOnChannelOutputChange;
    property Output[AChannel: Integer]: Boolean read GetOutput; default;
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

implementation

{ TPit8253 }

procedure TPit8253.SetOnChannelOutputChange(AValue: TTimerOutputNotify);
begin
  if FOnChannelOutputChange = AValue then Exit;
  FOnChannelOutputChange := AValue;
end;

procedure TPit8253.WriteChannelPort(AChannel: Integer; AValue: Byte);
begin

end;

function TPit8253.GetOutput(AChannel: Integer): Boolean;
begin
  Result := FChannels[AChannel].Output;
end;

procedure TPit8253.WriteCommandPort(ACommand: TCommand);
var
  Channel: TChannel;
begin
  if ACommand.Channel >= ChannelsCount then Exit;

  Channel := FChannels[ACommand.Channel];

  case ACommand.AccessMode of
    amLatch:
      Channel.Latch := Min(Channel.Value, $FFFF);
  else
    Channel.CountingMode := ACommand.CountMode;
    if Channel.CountingMode = cmBcd then
      raise Exception.Create('BCD counting mode: not implemented');

    Channel.AccessMode := ACommand.AccessMode;
    Channel.AccessState := asLoByte;
    Channel.OperatingMode := ACommand.OperatingMode;
    Channel.Latch := Null;
  end;
end;

constructor TPit8253.Create(AOwner: TComponent; AActualFrequency: Integer);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FTickBatchSize := BaseFrequency div AActualFrequency;

  for I := 0 to High(FChannels) do
    FChannels[I] := TChannel.Create(Self);
end;

procedure TPit8253.Tick;
var
  ChannelNumber, I: Integer;
  Channel: TChannel;
  Old: Boolean;
begin
  for I := 1 to FTickBatchSize do
    for ChannelNumber := 0 to High(FChannels) do
    begin
      Channel := FChannels[ChannelNumber];
      Old := Channel.Output;
      Channel.Tick;

      if (Channel.Output <> Old) and Assigned(OnChannelOutputChange) then
        OnChannelOutputChange(Self, ChannelNumber, Channel.Output);
    end;
end;

function TPit8253.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TPit8253.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TPit8253.WriteIOByte(AAddress: Word; AData: Byte);
begin

end;

function TPit8253.ReadIOByte(AAddress: Word): Byte;
begin

end;

function TPit8253.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
begin
  case AAddress of
    $40..$42:
      begin
        AData := FChannels[AAddress - $40].ReadValue;
        Result := True;
      end;

    $43:
      begin
        { read command port ? }
        Result := False;
      end
  else
    Result := False;
  end;
end;

procedure TPit8253.OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  case AAddress of
    $40..$42: FChannels[AAddress - $40].WriteReloadValue(AData);

    $43: WriteCommandPort(TCommand(AData));
  end;
end;

{ TPit8253.TChannel }

procedure TPit8253.TChannel.Tick;
begin
  Dec(Value);

  case OperatingMode of
    omInterruptOnTerminalCount:
      if Value = 0 then Output := True;

    omRateGenerator, omRateGeneratorDuplicate:
      case Value of
        1: Output := False;
        0: Output := True;
      else
        { no change }
      end;

    omSquareWaveGenerator, omSquareWaveGeneratorDuplicate:
      begin
        Dec(Value);  { This mode decrements 2x }
        if Value <= 1 then Output := not Output;
      end;

    else
      raise Exception.CreateFmt(
        'PIT operating mode %d: not implemented', [Ord(OperatingMode)]);
  end;

  if Value < 0 then
    Value := IfThen(ReloadValue > 0, ReloadValue, 65536) - Abs(Value);
end;

procedure TPit8253.TChannel.WriteReloadValue(AValue: Byte);
begin
  case AccessState of
    asLoByte:
      begin
        ReloadValue := (ReloadValue and $FF00) or AValue;
        if AccessMode = amLoByteHiByte then AccessState := asHiByte;
      end;

    asHiByte:
      begin
        ReloadValue := (ReloadValue and $FF) or (AValue shl 8);
        AccessState := asLoByte;
      end;
  end;

  Output := False; { Todo }
end;

function TPit8253.TChannel.ReadValue: Byte;
begin
  case AccessMode of
    amLoByte:
      begin
        Result := Lo(Latch.ValueOr(Value));
        Latch := Null;
      end;

    amHiByte:
      begin
        Result := Hi(Latch.ValueOr(Value));
        Latch := Null;
      end;

    amLoByteHiByte:
      begin
        case AccessState of
          asLoByte:
            begin
              Result := Lo(Latch.ValueOr(Value));
              AccessState := asHiByte;
            end;

          asHiByte:
            begin
              Result := Hi(Latch.ValueOr(Value));
              Latch := Null;
              AccessState := asLoByte;
            end;
        end;
      end;
  else
    raise Exception.CreateFmt('Invalid access mode: %d', [Ord(AccessMode)]);
  end;
end;

end.

