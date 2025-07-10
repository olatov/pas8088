unit Interrups;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Hardware;

type

  { TNmiTrigger }

  TNmiTrigger = class(TComponent, INmiTrigger, IIOBusDevice)
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

{ TNmiTrigger }

procedure TNmiTrigger.SetCpu(AValue: ICpu);
begin
  FCpu := AValue;
end;

procedure TNmiTrigger.AttachCpu(ACpu: ICpu);
begin
  Cpu := ACpu;
end;

procedure TNmiTrigger.RaiseNmi;
begin
  if Assigned(Cpu) and FOutputEnabled then
    Cpu.RaiseNmi;
end;

function TNmiTrigger.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TNmiTrigger.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TNmiTrigger.WriteIOByte(AAddress: Word; AData: Byte);
begin

end;

function TNmiTrigger.ReadIOByte(AAddress: Word): Byte;
begin

end;

function TNmiTrigger.OnIORead(ADevice: IIOBusDevice; AAddress: Word; out
  AData: Byte): Boolean;
begin
  Result := False;
end;

procedure TNmiTrigger.OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  if AAddress = $68 then
    { TODO: double check }
    FOutputEnabled := (AData and (1 shl 3)) <> 0;
end;

end.

