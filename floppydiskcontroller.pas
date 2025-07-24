unit FloppyDiskController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Hardware, Cpu8088;

type

  { TFloppyDiskController }

  TFloppyDiskController = class(TComponent, IMemoryBusDevice, IFloppyDiskController)
  public
    type
      TDiskGeometry = record
        Cylinders, Heads, Sectors, SectorSize: Integer;
      end;
  public
    const
      DefaultDiskGeometry: TDiskGeometry = (
        { 720 KB floppy, DS / QD }
        Cylinders: 80;
        Heads: 2;
        Sectors: 9;
        SectorSize: 512;
      );
  private
    FBuffer: array of Byte;
    FMemoryBus: IMemoryBus;
    FDiskGeometry: TDiskGeometry;
    FDiskStreams: specialize TArray<TSTream>;
    function ChsToLogical(Cylinder, Head, Sector: Integer): Integer;
    procedure SetDiskGeometry(AValue: TDiskGeometry);
    procedure Seek(ADriveNumber, ALogicalSector: Integer);
    function TryWriteSectorFromBuffer(ADriveNumber: Integer): Boolean;
    function TryReadSectorToBuffer(ADriveNumber: Integer): Boolean;
    procedure LoadBufferFromMemory(ASegment, AOffset: Word);
    procedure SaveBufferToMemory(ASegment, AOffset: Word);
    procedure EnsureDriveNumberIsValid(ADriveNumber: Integer);
    function VerifyChs(ACylinder, AHead, ASector: Integer): Boolean;
  public
    property DiskGeometry: TDiskGeometry read FDiskGeometry write SetDiskGeometry;
    constructor Create(AOwner: TComponent; DriveCount: Integer = 2); reintroduce;

    procedure InsertDisk(ADriveNumber: Integer; DiskStream: TStream);
    procedure EjectDisk(ADriveNumber: Integer);

    function Reset: Boolean;
    function ReadSectors(
      ADriveNumber, ACylinder, AHead, ASector, ASectorCount: Integer;
      ASegment, AOffset: Word): Boolean;

    function WriteSectors(
      ADriveNumber, ACylinder, AHead, ASector, ASectorCount: Integer;
      ASegment, AOffset: Word): Boolean;

    function GetMemoryBus: IMemoryBus;
    procedure SetMemoryBus(AValue: IMemoryBus);
    procedure WriteMemoryByte(AAddress: TPhysicalAddress; AData: Byte);
    function ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
    property MemoryBus: IMemoryBus read GetMemoryBus write SetMemoryBus;
    function OnMemoryRead(Sender: IMemoryBusDevice; AAddress:
      TPhysicalAddress; out AData: Byte): Boolean;
    procedure OnMemoryWrite(Sender: IMemoryBusDevice; AAddress:
      TPhysicalAddress; AData: Byte);
  end;

implementation

{ TFloppyDiskController }

function TFloppyDiskController.ChsToLogical(
  Cylinder, Head, Sector: Integer): Integer;
begin
  Result :=
    (Cylinder * (DiskGeometry.Heads * DiskGeometry.Sectors))
    + (Head * DiskGeometry.Sectors)
    + Sector - 1;
end;

procedure TFloppyDiskController.SetDiskGeometry(AValue: TDiskGeometry);
begin
  FDiskGeometry := AValue;
  SetLength(FBuffer, DiskGeometry.SectorSize);
end;

procedure TFloppyDiskController.Seek(ADriveNumber, ALogicalSector: Integer);
begin
  if not Assigned(FDiskStreams[ADriveNumber]) then Exit;

  FDiskStreams[ADriveNumber].Seek(
    ALogicalSector * (DiskGeometry.SectorSize), soBeginning);
end;

function TFloppyDiskController.TryWriteSectorFromBuffer(
  ADriveNumber: Integer): Boolean;
var
  Disk: TStream;
begin
  Result := False;
  Disk := FDiskStreams[ADriveNumber];
  if not Assigned(Disk)
    or (Disk.Position > (Disk.Size - DiskGeometry.SectorSize)) then Exit;

  Disk.Write(FBuffer[0], DiskGeometry.SectorSize);
  Result := True;
end;

function TFloppyDiskController.TryReadSectorToBuffer(
  ADriveNumber: Integer): Boolean;
var
  Disk: TStream;
begin
  Result := False;
  Disk := FDiskStreams[ADriveNumber];
  if not Assigned(Disk)
    or (Disk.Position > (Disk.Size - DiskGeometry.SectorSize)) then Exit;

  Disk.Read(FBuffer[0], DiskGeometry.SectorSize);
  Result := True;
end;

procedure TFloppyDiskController.LoadBufferFromMemory(ASegment, AOffset: Word);
var
  I: Integer;
begin
  for I := 0 to High(FBuffer) do
    FMemoryBus.InvokeRead(Self,
      GetPhysicalAddress(ASegment, AOffset + I), FBuffer[I]);
end;

procedure TFloppyDiskController.SaveBufferToMemory(ASegment, AOffset: Word);
var
  I: Integer;
begin
  for I := 0 to High(FBuffer) do
    FMemoryBus.InvokeWrite(Self,
      GetPhysicalAddress(ASegment, AOffset + I), FBuffer[I]);
end;

procedure TFloppyDiskController.EnsureDriveNumberIsValid(ADriveNumber: Integer);
begin
  if not InRange(ADriveNumber, 0, High(FDiskStreams)) then
    raise Exception.CreateFmt('Invalid drive number: %d', [ADriveNumber]);
end;

function TFloppyDiskController.VerifyChs(
  ACylinder, AHead, ASector: Integer): Boolean;
begin
  Result := InRange(ACylinder, 0, DiskGeometry.Cylinders - 1)
    and InRange(AHead, 0, DiskGeometry.Heads - 1)
    and InRange(ASector, 1, DiskGeometry.Sectors)
    and InRange(
      ChsToLogical(ACylinder, AHead, ASector),
      0,
      (DiskGeometry.Cylinders * DiskGeometry.Heads * DiskGeometry.Sectors) - 1);
end;

constructor TFloppyDiskController.Create(
  AOwner: TComponent; DriveCount: Integer);
begin
  inherited Create(AOwner);
  SetLength(FDiskStreams, DriveCount);
  DiskGeometry := DefaultDiskGeometry;
end;

procedure TFloppyDiskController.InsertDisk(
  ADriveNumber: Integer; DiskStream: TStream);
begin
  EnsureDriveNumberIsValid(ADriveNumber);
  FDiskStreams[ADriveNumber] := DiskStream;
end;

procedure TFloppyDiskController.EjectDisk(ADriveNumber: Integer);
begin
  EnsureDriveNumberIsValid(ADriveNumber);
  FDiskStreams[ADriveNumber] := Nil;
end;

function TFloppyDiskController.Reset: Boolean;
var
  DriveNumber: Integer;
begin
  if Length(FDiskStreams) = 0 then Exit(False);

  for DriveNumber := 0 to High(FDiskStreams) do Seek(DriveNumber, 0);
  Result := True;
end;

function TFloppyDiskController.ReadSectors(
  ADriveNumber, ACylinder, AHead, ASector, ASectorCount: Integer;
  ASegment, AOffset: Word): Boolean;
var
  I: Integer;
  CurrentOffset: Word;
begin
  Result := False;
  EnsureDriveNumberIsValid(ADriveNumber);
  if not VerifyChs(ACylinder, AHead, ASector) then Exit;

  Seek(ADriveNumber, ChsToLogical(ACylinder, AHead, ASector));
  CurrentOffset := AOffset;

  for I := 1 to ASectorCount do
  begin
    if not TryReadSectorToBuffer(ADriveNumber) then Exit;
    SaveBufferToMemory(ASegment, CurrentOffset);
    Inc(CurrentOffset, DiskGeometry.SectorSize);
  end;

  Result := True;
end;

function TFloppyDiskController.WriteSectors(
  ADriveNumber, ACylinder, AHead, ASector, ASectorCount:Integer;
  ASegment, AOffset: Word): Boolean;
var
  I: Integer;
  CurrentOffset: Word;
begin
  Result := False;
  EnsureDriveNumberIsValid(ADriveNumber);
  if not VerifyChs(ACylinder, AHead, ASector) then Exit;

  Seek(ADriveNumber, ChsToLogical(ACylinder, AHead, ASector));
  CurrentOffset := AOffset;

  for I := 1 to ASectorCount do
  begin
    LoadBufferFromMemory(ASegment, CurrentOffset);
    if not TryWriteSectorFromBuffer(ADriveNumber) then Exit;
    Inc(CurrentOffset, DiskGeometry.SectorSize);
  end;

  Result := True;
end;

function TFloppyDiskController.GetMemoryBus: IMemoryBus;
begin
  Result := FMemoryBus;
end;

procedure TFloppyDiskController.SetMemoryBus(AValue: IMemoryBus);
begin
  FMemoryBus := AValue;
end;

procedure TFloppyDiskController.WriteMemoryByte(
  AAddress: TPhysicalAddress; AData: Byte);
begin

end;

function TFloppyDiskController.ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
begin
  Result := 0;
end;

function TFloppyDiskController.OnMemoryRead(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; out AData: Byte): Boolean;
begin
  Result := False;
end;

procedure TFloppyDiskController.OnMemoryWrite(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; AData: Byte);
begin

end;

end.

