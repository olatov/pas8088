unit Memory;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}

interface

uses
  Classes, SysUtils, Math,
  Cpu8088;

type

  { IMemoryBlock }

  IMemoryBlock = interface
    ['{73EE5E2E-7345-4032-90F9-78BD84CADA2D}']
    function GetSize: UInt32;
    property Size: UInt32 read GetSize;
    procedure WriteByte(AOffset: UInt32; AData: Byte);
    function ReadByte(AOffset: UInt32): Byte;
  end;

  { TRamMemoryBlock }

  TRamMemoryBlock = class(TComponent, IMemoryBlock)
  private
    FData: array of Byte;
  public
    function GetSize: UInt32;
    property Size: UInt32 read GetSize;
    constructor Create(AOwner: TComponent; ASize: UInt32); reintroduce;
    procedure WriteByte(AOffset: UInt32; AData: Byte); virtual;
    function ReadByte(AOffset: UInt32): Byte; virtual;
    procedure LoadFromStream(AStream: TStream; AOffset, ALength: Integer);
    procedure LoadFromStream(AStream: TStream; AOffset: Integer);
    procedure LoadFromStream(AStream: TStream);
  end;

  { TRomMemoryBlock }

  TRomMemoryBlock = class(TRamMemoryBlock)
  public
    procedure WriteByte(AOffset: UInt32; AData: Byte); override;
  end;

  { TMemoryBus }

  TMemoryBus = class(TComponent, IMemoryBus)
  private
    type
      TMemorySlot = record
        StartAddress, EndAddress: TPhysicalAddress;
        MemoryBlock: IMemoryBlock;
      end;
  private
    FDevices: specialize TArray<IMemoryBusDevice>;
    FMemoryMap: specialize TArray<TMemorySlot>;
    function SegmentToPhysical(ASegment, AOffset: Word): TPhysicalAddress;
    function FindMemorySlot(AAddress: TPhysicalAddress): TMemorySlot;

    procedure WriteByte(AAddress: TPhysicalAddress; AData: Byte);
    function ReadByte(AAddress: TPhysicalAddress): Byte;
    procedure WriteByte(ASegment, AOffset: Word; AData: Byte);
    function ReadByte(ASegment, AOffset: Word): Byte;
  public
    procedure AttachDevice(ADevice: IMemoryBusDevice);
    procedure InvokeWrite(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
    procedure InvokeRead(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte);

    procedure InstallMemoryBlock(
        AAddress: TPhysicalAddress; AMemoryBlock: IMemoryBlock);
  end;

implementation

{ TRamMemoryBlock }

constructor TRamMemoryBlock.Create(AOwner: TComponent; ASize: UInt32);
begin
  inherited Create(AOwner);
  SetLength(FData, ASize);
end;

function TRamMemoryBlock.GetSize: UInt32;
begin
  Result := Length(FData);
end;

procedure TRamMemoryBlock.WriteByte(AOffset: UInt32; AData: Byte);
begin
  FData[AOffset] := AData;
end;

function TRamMemoryBlock.ReadByte(AOffset: UInt32): Byte;
begin
  Result := FData[AOffset];
end;

procedure TRamMemoryBlock.LoadFromStream(AStream: TStream; AOffset,
  ALength: Integer);
var
  DataLength: Integer;
begin
  DataLength := Min(ALength, AStream.Size - AStream.Position);
  if AOffset + DataLength > Size then
    raise Exception.Create('Not enough room in memory block');

  AStream.Read(FData[AOffset], DataLength);
end;

procedure TRamMemoryBlock.LoadFromStream(AStream: TStream; AOffset: Integer);
begin
  LoadFromStream(AStream, AOffset, AStream.Size - AStream.Position);
end;

procedure TRamMemoryBlock.LoadFromStream(AStream: TStream);
begin
  LoadFromStream(AStream, 0, AStream.Size - AStream.Position);
end;

{ TRomMemoryBlock }

procedure TRomMemoryBlock.WriteByte(AOffset: UInt32; AData: Byte);
begin
  { Nothing happens - writes to ROM are ignored }
end;

{ TMemoryBus }

function TMemoryBus.SegmentToPhysical(ASegment, AOffset: Word
  ): TPhysicalAddress;
begin
  {$Push}{$R-}
  Result := (ASegment shl 4) + AOffset;
  {$Pop}
end;

function TMemoryBus.FindMemorySlot(AAddress: TPhysicalAddress): TMemorySlot;
var
  Slot: TMemorySlot;
begin
  Result.MemoryBlock := Nil;
  for Slot in FMemoryMap do
    if InRange(AAddress, Slot.StartAddress, Slot.EndAddress) then
      Exit(Slot);
end;

procedure TMemoryBus.WriteByte(AAddress: TPhysicalAddress; AData: Byte);
var
  Slot: TMemorySlot;
begin
  Slot := FindMemorySlot(AAddress);
  if not Assigned(Slot.MemoryBlock) then Exit;

  Slot.MemoryBlock.WriteByte(AAddress - Slot.StartAddress, AData);
end;

function TMemoryBus.ReadByte(AAddress: TPhysicalAddress): Byte;
var
  Slot: TMemorySlot;
begin
  Slot := FindMemorySlot(AAddress);
  if not Assigned(Slot.MemoryBlock) then Exit($FF);

  Result := Slot.MemoryBlock.ReadByte(AAddress - Slot.StartAddress);
end;

procedure TMemoryBus.WriteByte(ASegment, AOffset: Word; AData: Byte);
begin
  WriteByte(SegmentToPhysical(ASegment, AOffset), AData);
end;

function TMemoryBus.ReadByte(ASegment, AOffset: Word): Byte;
begin
  Result := ReadByte(SegmentToPhysical(ASegment, AOffset));
end;

procedure TMemoryBus.AttachDevice(ADevice: IMemoryBusDevice);
begin
  Insert(ADevice, FDevices, Integer.MaxValue);
  ADevice.MemoryBus := Self;
end;

procedure TMemoryBus.InvokeWrite(
  ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
var
  Device: IMemoryBusDevice;
begin
  for Device in FDevices do
    if (Device <> ADevice) and Assigned(Device.OnMemoryWrite) then
      Device.OnMemoryWrite(ADevice, AAddress, AData);
end;

procedure TMemoryBus.InvokeRead(
  ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte);
var
  Device: IMemoryBusDevice;
begin
  for Device in FDevices do
    if (Device <> ADevice)
        and Assigned(Device.OnMemoryRead)
        and Device.OnMemoryRead(ADevice, AAddress, AData) then Exit;
  AData := $FF;
end;

procedure TMemoryBus.InstallMemoryBlock(AAddress: TPhysicalAddress;
  AMemoryBlock: IMemoryBlock);
var
  Slot, OtherSlot: TMemorySlot;
begin
  Slot.StartAddress := AAddress;
  Slot.EndAddress := AAddress + AMemoryBlock.Size - 1;
  Slot.MemoryBlock := AMemoryBlock;

  if Slot.EndAddress > High(TPhysicalAddress) then
    Exception.Create('Memory block exceeds the address space');

  for OtherSlot in FMemoryMap do
    if (Slot.EndAddress >= OtherSlot.StartAddress)
        and (OtherSlot.EndAddress >= Slot.StartAddress) then
      Exception.Create('Memory block overlaps with an existing block');

  Insert(Slot, FMemoryMap, Integer.MaxValue);
end;

end.

