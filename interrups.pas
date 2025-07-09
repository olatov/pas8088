unit Interrups;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Hardware;

type

  { TNmiGate }

  TNmiGate = class(TComponent, INmiGate, IIOBusDevice)
  private
    FCpu: ICpu;
    FIOBus: IIOBus;
    FOutputEnabled: Boolean;
    procedure SetCpu(AValue: ICpu);
  private
    property Cpu: ICpu read FCpu write SetCpu;
  public
    procedure AttachCpu(ACpu: ICpu);
    procedure RaiseNmi;

    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

implementation

{ TNmiGate }

procedure TNmiGate.SetCpu(AValue: ICpu);
begin
  FCpu := AValue;
end;

procedure TNmiGate.AttachCpu(ACpu: ICpu);
begin
  Cpu := ACpu;
end;

procedure TNmiGate.RaiseNmi;
begin
  if Assigned(Cpu) and FOutputEnabled then
    Cpu.RaiseNmi;
end;

function TNmiGate.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TNmiGate.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TNmiGate.WriteIOByte(AAddress: Word; AData: Byte);
begin

end;

function TNmiGate.ReadIOByte(AAddress: Word): Byte;
begin

end;

function TNmiGate.OnIORead(ADevice: IIOBusDevice; AAddress: Word; out
  AData: Byte): Boolean;
begin
  Result := False;
end;

procedure TNmiGate.OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  if AAddress = $68 then
    FOutputEnabled := (AData and (1 shl 3)) = 0;
end;

end.

