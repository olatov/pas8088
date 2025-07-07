unit Memory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Cpu8088;

type

  { TMemoryBus }

  TMemoryBus = class(TComponent, IMemoryBus)
  private
    function SegmentToPhysical(ASegment, AOffset: Word): TPhysicalAddress;
  public
    Data: array[0..$FFFFF] of Byte;
    procedure WriteByte(AAddress: TPhysicalAddress; AData: Byte);
    function ReadByte(AAddress: TPhysicalAddress): Byte;
    procedure WriteByte(ASegment, AOffset: Word; AData: Byte);
    function ReadByte(ASegment, AOffset: Word): Byte;
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromStream(AStream: TStream; AAddress: TPhysicalAddress);
  end;


implementation

{ TMemoryBus }

function TMemoryBus.SegmentToPhysical(ASegment, AOffset: Word
  ): TPhysicalAddress;
begin
  {$Push}{$R-}
  Result := (ASegment shl 4) + AOffset;
  {$Pop}
end;

procedure TMemoryBus.WriteByte(AAddress: TPhysicalAddress; AData: Byte);
begin
  Data[AAddress] := AData;
end;

function TMemoryBus.ReadByte(AAddress: TPhysicalAddress): Byte;
begin
  Result := Data[AAddress];
end;

procedure TMemoryBus.WriteByte(ASegment, AOffset: Word; AData: Byte);
begin
  WriteByte(SegmentToPhysical(ASegment, AOffset), AData);
end;

function TMemoryBus.ReadByte(ASegment, AOffset: Word): Byte;
begin
  Result := ReadByte(SegmentToPhysical(ASegment, AOffset));
end;

constructor TMemoryBus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FillByte(Data, SizeOf(Data), 0);
end;

procedure TMemoryBus.LoadFromStream(
  AStream: TStream;
  AAddress: TPhysicalAddress);
begin
  if AAddress + (AStream.Size - AStream.Position - 1) > High(TPhysicalAddress) then
    raise Exception.Create('Insufficient address space');

  AStream.ReadBuffer(Data[AAddress], AStream.Size - AStream.Position);
end;

end.

