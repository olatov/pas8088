program pas8088;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Classes, SysUtils,
  Cpu8088, Memory, IO, Machine;

function BuildMachine: TMachine;
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
    BiosRom.LoadFromStream(BiosStream, 8192 - 16);
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

var
  Computer: TMachine;

begin
  Computer := BuildMachine;
  try
    while not (Computer.Cpu.Halted or (Computer.Cpu.Ticks > 1000)) do
      Computer.Tick;
  except
    on E: Exception do
      Writeln('*** ERROR: ', E.Message, ' ***');
  end;

  Writeln;
  Computer.Cpu.Registers.Log;

  FreeAndNil(Computer);
end.

