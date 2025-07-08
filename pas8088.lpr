program pas8088;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Classes, SysUtils,
  Cpu8088, Memory, IO, Machine;

type

  { TApp }

  TApp = class
  public
    procedure Run;
    function BuildMachine: TMachine;
    function InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
    procedure OnInstruction(ASender: TObject; AInstruction: TInstruction);
  end;

function TApp.BuildMachine: TMachine;
var
  MemoryBus: TMemoryBus;
  BiosRom: TRomMemoryBlock;
  BiosStream: TStream;
  Ram: TRamMemoryBlock;
  TestProgramStream: TStream;
begin
  Result := TMachine.Create(Nil);

  { Cpu }
  Result.InstallCpu(TCpu8088.Create(Result));

  { I/O }
  Result.InstallIOBus(TIOBus.Create(Result));

  { BIOS ROM }
  MemoryBus := TMemoryBus.Create(Result);

  BiosRom := TRomMemoryBlock.Create(MemoryBus, 8 * 1024);

  BiosStream := TBytesStream.Create([
    $EA, $00, $00, $60, $00   { jmp 0x0060:0x0000 }
  ]);

  try
    BiosRom.LoadFromStream(BiosStream, $2000 - $10);
  finally
    FreeAndNil(BiosStream);
  end;
  MemoryBus.InstallMemoryBlock($FE000, BiosRom);

  { RAM, test program }
  Ram := TRamMemoryBlock.Create(MemoryBus, 128 * 1024);
  TestProgramStream := TFileStream.Create('test.bin', fmOpenRead);
  try
    Ram.LoadFromStream(TestProgramStream, $00600);
  finally
    FreeAndNil(TestProgramStream);
  end;

  MemoryBus.InstallMemoryBlock($00000, Ram);

  Result.InstallMemoryBus(MemoryBus);

  Result.Initialize;
end;

procedure TApp.Run;
var
  Computer: TMachine;

begin
  Computer := BuildMachine;

  Computer.Cpu.InterruptHook := @InterruptHook;
  Computer.Cpu.OnAfterInstruction := @OnInstruction;

  try
    while not (Computer.Cpu.Halted
        or (Computer.Cpu.Ticks > 50000000)) do
      Computer.Tick;
  except
    on E: Exception do
    begin
      Writeln('*** ERROR: ', E.Message, ' ***');
      Computer.Cpu.DumpCurrentInstruction;
    end;
  end;

  Writeln;
  Computer.Cpu.Registers.Log;

  FreeAndNil(Computer);
end;

function TApp.InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
var
  Cpu: TCpu8088 absolute ASender;
begin
  Writeln(Format('INT 0x%.x', [ANumber]));

  case ANumber of
    $10:
      begin
        case Cpu.Registers.AH of
          $0E: { teletype }
            Write(Char(Cpu.Registers.AL));
        else
          Writeln(Format('BIOS function 0x%.x', [Cpu.Registers.AH]));
        end;
      end;
  end;
  Result := False;
end;

procedure TApp.OnInstruction(ASender: TObject; AInstruction: TInstruction);
begin
  (ASender as TCpu8088).DumpCurrentInstruction;
end;

var
  App: TApp;

begin
  App := TApp.Create;
  App.Run;
  App.Free;
end.

