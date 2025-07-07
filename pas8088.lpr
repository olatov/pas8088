program pas8088;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  Cpu8088, Memory, IO;

var
  Cpu: TCpu8088;
  MemoryBus: TMemoryBus;
  Prog: String;

  BootstrapStream: TBytesStream;
  ProgramStream: TFileStream;

begin
  Cpu := TCpu8088.Create(Nil);
  MemoryBus := TMemoryBus.Create(Cpu);
  Cpu.MemoryBus := MemoryBus;
  Cpu.IOBus := TIOBus.Create(Cpu);

  BootstrapStream := TBytesStream.Create([
    $EA, $00, $00, $60, $00   { jmp 0x0060:0x0000 }
  ]);
  MemoryBus.LoadFromStream(BootstrapStream, $FFFF0);
  FreeAndNil(BootstrapStream);

  Prog := 'test';

  ProgramStream := TFileStream.Create(Prog, fmOpenRead);
  MemoryBus.LoadFromStream(ProgramStream, $00600);
  FreeAndNil(ProgramStream);

  try
    while not Cpu.Halted do Cpu.Tick;
  except
    on E: Exception do
      Writeln('*** ERROR: ', E.Message, ' ***');
  end;

  Writeln;
  Cpu.Registers.Log;

  FreeAndNil(Cpu);
end.

