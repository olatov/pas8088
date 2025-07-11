unit Machine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Cpu8088;

type

  { TMachine }

  TMachine = class(TComponent)
  private
    FCpu: TCpu8088;
    FIOBus: IIOBus;
    FMemoryBus: IMemoryBus;
    procedure SetCpu(AValue: TCpu8088);
    procedure SetIOBus(AValue: IIOBus);
    procedure SetMemoryBus(AValue: IMemoryBus);
  published
    property Cpu: TCpu8088 read FCpu write SetCpu;
    property MemoryBus: IMemoryBus read FMemoryBus write SetMemoryBus;
    property IOBus: IIOBus read FIOBus write SetIOBus;
    procedure Tick;
    procedure Run(ATicks: Integer=1000);
    procedure Initialize;
    procedure InstallCpu(ACpu: TCpu8088);
    procedure InstallMemoryBus(AMemoryBus: IMemoryBus);
    procedure InstallIOBus(AIOBus: IIOBus);
  end;

implementation

{ TMachine }

procedure TMachine.SetCpu(AValue: TCpu8088);
begin
  if FCpu = AValue then Exit;
  FCpu := AValue;
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
end;

procedure TMachine.Run(ATicks: Integer);
var
  I: Integer;
begin
  for I := 1 to ATicks do Tick;
end;

procedure TMachine.Initialize;
var
  Errors: TStringList;
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

  Cpu.Reset;
end;

procedure TMachine.InstallCpu(ACpu: TCpu8088);
begin
  Cpu := ACpu;
end;

procedure TMachine.InstallMemoryBus(AMemoryBus: IMemoryBus);
begin
  MemoryBus := AMemoryBus;;
end;

procedure TMachine.InstallIOBus(AIOBus: IIOBus);
begin
  IOBus := AIOBus;
end;

end.

