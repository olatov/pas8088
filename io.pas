unit IO;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Cpu8088;

type

  { TIOBus }

  TIOBus = class(TComponent, IIOBus)
  private
    FDevices: specialize TArray<IIODevice>;
  public
    procedure AttachDevice(ADevice: IIODevice);
    procedure InvokeWrite(ADevice: IIODevice; AAddress: Word; AData: Byte);
    procedure InvokeRead(ADevice: IIODevice; AAddress: Word; out AData: Byte);
  end;

implementation

{ TIOBus }

procedure TIOBus.AttachDevice(ADevice: IIODevice);
begin
  Insert(ADevice, FDevices, Integer.MaxValue);
  ADevice.IOBus := Self;
end;

procedure TIOBus.InvokeWrite(ADevice: IIODevice; AAddress: Word; AData: Byte);
var
  Device: IIODevice;
begin
  for Device in FDevices do
    if (Device <> ADevice) and Assigned(Device.OnIOWrite) then
      Device.OnIOWrite(ADevice, AAddress, AData);
end;

procedure TIOBus.InvokeRead(ADevice: IIODevice; AAddress: Word; out AData: Byte);
var
  Device: IIODevice;
begin
  for Device in FDevices do
    if (Device <> ADevice)
        and Assigned(Device.OnIOWrite)
        and Device.OnIORead(ADevice, AAddress, AData) then Exit;
  AData := $FF;
end;

end.

