unit Hardware;

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils;

type
  TPhysicalAddress = 0..$FFFFF;

  IMemoryBusDevice = interface;
  IMemoryBus = interface;

  ICpu = interface
    procedure Tick;
    procedure RaiseNmi;
    function RaiseHardwareInterrupt(ANumber: Byte): Boolean; { True if acknowledged }
  end;

  IMemoryBus = interface
    ['{8A9E54C3-119D-417B-AC06-1B59AF018B4E}']
    procedure AttachDevice(ADevice: IMemoryBusDevice);
    procedure InvokeWrite(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
    procedure InvokeRead(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte);
  end;

  { IMemoryBusDevice }

  IMemoryBusDevice = interface
    ['{330D62BD-4E8F-4C7F-BC64-ACC7BAD5146E}']
    function GetMemoryBus: IMemoryBus;
    procedure SetMemoryBus(AValue: IMemoryBus);
    procedure WriteMemoryByte(AAddress: TPhysicalAddress; AData: Byte);
    function ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
    property MemoryBus: IMemoryBus read GetMemoryBus write SetMemoryBus;
    function OnMemoryRead(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte): Boolean;
    procedure OnMemoryWrite(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
  end;

  IIOBus = interface;
  IIOBusDevice = interface;

  { IIOBusDevice }

  IIOBusDevice = interface
    ['{273E3676-D428-452A-84CA-3EADAC0AC885}']
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

  IIOBus = interface
    ['{364D996D-C306-4750-A24A-E1E84CBB0110}']
    procedure AttachDevice(ADevice: IIOBusDevice);
    procedure InvokeWrite(ADevice: IIOBusDevice; AAddress: Word; AData: Byte);
    procedure InvokeRead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte);
  end;

  INmiTrigger = interface
    ['{7F4DA08F-DA98-4F0A-B45B-B40A53304387}']
    procedure AttachCpu(ACpu: ICpu);
    procedure RaiseNmi;
  end;

  IInterruptController = interface(IIOBusDevice)
    ['{6DC53945-9F2B-4758-9324-B6B6040ADEF6}']
    procedure AttachCpu(ACpu: ICpu);
    procedure RaiseIrq(ANumber: Byte);
    procedure Tick;
  end;

  IFloppyDiskController = interface(IMemoryBusDevice)
    ['{D35A22B1-05C9-44EF-AAD6-589259DA833A}']
    procedure InsertDisk(ADriveNumber: Integer; DiskStream: TStream);
    procedure EjectDisk(ADriveNumber: Integer);
    function Reset: Boolean;
    function ReadSectors(
      ADriveNumber, ACylinder, AHead, ASector, ASectorCount: Integer;
      ASegment, AOffset: Word): Boolean;
    function WriteSectors(
      ADriveNumber, ACylinder, AHead, ASector, ASectorCount: Integer;
      ASegment, AOffset: Word): Boolean;
  end;

implementation

end.

