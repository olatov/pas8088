unit Benchmark;

{ Performance benchmark for the emulator core.

  Part 1 measures what a single memory read costs when it goes through
  different kinds of dispatch (inline array access, static call, virtual
  call, CORBA interface call).

  Part 2 runs a synthetic 8086 workload on TCpu8088 attached to a flat RAM
  bus. It reports emulated instructions per second, bus accesses per
  instruction, and an estimate of the share of run time spent on interface
  dispatch. }

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, Math, Hardware, Cpu8088;

procedure RunBenchmark(AInstructions: QWord);

implementation

type
  TMemory = array[0..$FFFFF] of Byte;
  PMemory = ^TMemory;

  { TFlatMemoryBus }

  TFlatMemoryBus = class(TObject, IMemoryBus)
  public
    Memory: TMemory;
    procedure AttachDevice(ADevice: IMemoryBusDevice);
    procedure InvokeWrite(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
    procedure InvokeRead(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte);
    { Same work as InvokeRead, reached through other dispatch kinds }
    function ReadStatic(AAddress: TPhysicalAddress): Byte;
    function ReadVirtual(AAddress: TPhysicalAddress): Byte; virtual;
  end;

  { TCountingMemoryBus }

  TCountingMemoryBus = class(TFlatMemoryBus, IMemoryBus)
  public
    Reads, Writes: QWord;
    procedure InvokeWrite(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
    procedure InvokeRead(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte);
  end;

  { TNullIOBus }

  TNullIOBus = class(TObject, IIOBus)
  public
    procedure AttachDevice(ADevice: IIOBusDevice);
    procedure InvokeWrite(ADevice: IIOBusDevice; AAddress: Word; AData: Byte);
    procedure InvokeRead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte);
  end;

const
  { Loaded at 1000:0000; DS = ES = SS = 1000h. Source (nasm, bits 16, org 0):

      start:  mov ax, 0x1000 / mov ds, ax / mov es, ax / mov ss, ax
              mov sp, 0xFFFE / xor bx, bx / xor dx, dx
      outer:  mov cx, 256 / xor si, si / mov di, 0x8000
      inner:  lodsw / add ax, bx / xor dx, ax / mov [di], ax / add di, 2
              push cx / call sub1 / pop cx / cmp ax, dx / jne .skip / inc dx
      .skip:  loop inner
              xor si, si / mov di, 0x4000 / mov cx, 512 / cld / rep movsb
              mov ax, dx / mov cx, 7 / mul cx / jmp outer
      sub1:   mov bp, sp / mov ax, [bp+2] / inc bx / shl ax, 1
              and ax, 0x7FFF / ret

    The mix covers register ALU, ModRM memory operands, string ops, the
    stack, near calls, loops, branches and a MUL. }
  WorkloadSegment = $1000;
  Workload: array[0..77] of Byte = (
    $B8, $00, $10, $8E, $D8, $8E, $C0, $8E, $D0, $BC, $FE, $FF,
    $31, $DB, $31, $D2, $B9, $00, $01, $31, $F6, $BF, $00, $80,
    $AD, $01, $D8, $31, $C2, $89, $05, $83, $C7, $02, $51, $E8,
    $1C, $00, $59, $39, $D0, $75, $01, $42, $E2, $EA, $31, $F6,
    $BF, $00, $40, $B9, $00, $02, $FC, $F3, $A4, $89, $D0, $B9,
    $07, $00, $F7, $E1, $EB, $CE, $89, $E5, $8B, $46, $02, $43,
    $D1, $E0, $25, $FF, $7F, $C3);

  { Rough average throughput of a 4.77 MHz 8088 on typical code }
  RealMips = 0.33;

{ TFlatMemoryBus }

procedure TFlatMemoryBus.AttachDevice(ADevice: IMemoryBusDevice);
begin
end;

procedure TFlatMemoryBus.InvokeWrite(ADevice: IMemoryBusDevice;
  AAddress: TPhysicalAddress; AData: Byte);
begin
  Memory[AAddress] := AData;
end;

procedure TFlatMemoryBus.InvokeRead(ADevice: IMemoryBusDevice;
  AAddress: TPhysicalAddress; out AData: Byte);
begin
  AData := Memory[AAddress];
end;

function TFlatMemoryBus.ReadStatic(AAddress: TPhysicalAddress): Byte;
begin
  Result := Memory[AAddress];
end;

function TFlatMemoryBus.ReadVirtual(AAddress: TPhysicalAddress): Byte;
begin
  Result := Memory[AAddress];
end;

{ TCountingMemoryBus }

procedure TCountingMemoryBus.InvokeWrite(ADevice: IMemoryBusDevice;
  AAddress: TPhysicalAddress; AData: Byte);
begin
  Inc(Writes);
  Memory[AAddress] := AData;
end;

procedure TCountingMemoryBus.InvokeRead(ADevice: IMemoryBusDevice;
  AAddress: TPhysicalAddress; out AData: Byte);
begin
  Inc(Reads);
  AData := Memory[AAddress];
end;

{ TNullIOBus }

procedure TNullIOBus.AttachDevice(ADevice: IIOBusDevice);
begin
end;

procedure TNullIOBus.InvokeWrite(ADevice: IIOBusDevice; AAddress: Word;
  AData: Byte);
begin
end;

procedure TNullIOBus.InvokeRead(ADevice: IIOBusDevice; AAddress: Word;
  out AData: Byte);
begin
  AData := $FF;
end;

var
  { Checksums land here so the measured loops can't be optimised away }
  Sink: QWord = 0;

{ Timing }

function NowSeconds: Double;
begin
  Result := GetTickCount64 / 1000;
end;

{ Part 1: dispatch micro-benchmark }

type
  TDispatchKind = (dkInline, dkStatic, dkVirtual, dkInterface);

const
  DispatchNames: array[TDispatchKind] of String = (
    'inline array access', 'static method call',
    'virtual method call', 'CORBA interface call');

{ Every variant walks the same pseudo-random address sequence inside one
  64 KB segment. Generating the address costs the same in all variants, so
  the difference from dkInline is the dispatch overhead. }
function RunDispatch(AKind: TDispatchKind; ABus: TFlatMemoryBus;
  AIterations: QWord; out AChecksum: QWord): Double;
var
  Mem: PMemory;
  Bus: IMemoryBus;
  Addr: LongWord;
  B: Byte;
  I: QWord;
  Sum: QWord;
  Start: Double;
begin
  Mem := @ABus.Memory;
  Bus := ABus;
  Addr := 0;
  Sum := 0;
  Start := NowSeconds;
  {$push}{$R-}{$Q-}
  case AKind of
    dkInline:
      for I := 1 to AIterations do
      begin
        Addr := (Addr * 5 + 1) and $FFFF;
        Inc(Sum, Mem^[Addr]);
      end;
    dkStatic:
      for I := 1 to AIterations do
      begin
        Addr := (Addr * 5 + 1) and $FFFF;
        Inc(Sum, ABus.ReadStatic(Addr));
      end;
    dkVirtual:
      for I := 1 to AIterations do
      begin
        Addr := (Addr * 5 + 1) and $FFFF;
        Inc(Sum, ABus.ReadVirtual(Addr));
      end;
    dkInterface:
      for I := 1 to AIterations do
      begin
        Addr := (Addr * 5 + 1) and $FFFF;
        Bus.InvokeRead(nil, Addr, B);
        Inc(Sum, B);
      end;
  end;
  {$pop}
  Result := NowSeconds - Start;
  AChecksum := Sum;
end;

{ Part 2: whole-CPU throughput }

procedure LoadWorkload(ABus: TFlatMemoryBus; ACpu: TCpu8088);
begin
  FillByte(ABus.Memory, SizeOf(ABus.Memory), 0);
  Move(Workload, ABus.Memory[WorkloadSegment * 16], SizeOf(Workload));
  ACpu.Reset;
  ACpu.Registers.CS := WorkloadSegment;
  ACpu.Registers.IP := 0;
end;

{ ABusIntf must come from the bus's most derived class: a class-to-interface
  conversion uses the static type, so passing a TCountingMemoryBus as
  TFlatMemoryBus would hand the CPU the uncounted methods. }
function RunCpu(ABus: TFlatMemoryBus; ABusIntf: IMemoryBus;
  ATicks: QWord): Double;
var
  Cpu: TCpu8088;
  IOBus: TNullIOBus;
  I: QWord;
  Start: Double;
begin
  IOBus := TNullIOBus.Create;
  Cpu := TCpu8088.Create(nil);
  try
    Cpu.MemoryBus := ABusIntf;
    Cpu.IOBus := IOBus;
    LoadWorkload(ABus, Cpu);

    Start := NowSeconds;
    for I := 1 to ATicks do
      Cpu.Tick;
    Result := NowSeconds - Start;

    { Make sure the workload ran as intended and did not go off the rails }
    if (Cpu.Registers.CS <> WorkloadSegment)
        or (Cpu.Registers.IP >= SizeOf(Workload))
        or Cpu.Halted then
      raise Exception.CreateFmt(
        'Workload went off the rails: CS:IP = %.4X:%.4X, halted = %s',
        [Cpu.Registers.CS, Cpu.Registers.IP, BoolToStr(Cpu.Halted, True)]);
  finally
    Cpu.Free;
    IOBus.Free;
  end;
end;

procedure RunBenchmark(AInstructions: QWord);
const
  DispatchIterations = 200000000;
var
  Bus: TFlatMemoryBus;
  CountingBus: TCountingMemoryBus;
  Kind: TDispatchKind;
  I, Run: Integer;
  Times: array[TDispatchKind] of Double;
  Checksum, CountTicks: QWord;
  NsPerAccess, InlineNs, Elapsed, NsPerTick, AccessesPerTick,
    DispatchNsPerTick: Double;
begin
  Writeln('== Memory read dispatch (', DispatchIterations div 1000000,
    'M reads) ==');
  Bus := TFlatMemoryBus.Create;
  try
    for I := 0 to High(Bus.Memory) do
      Bus.Memory[I] := Byte(I * 31);

    { Best of three runs per variant to filter out scheduler noise }
    for Kind in TDispatchKind do
    begin
      Times[Kind] := MaxDouble;
      for Run := 1 to 3 do
      begin
        Times[Kind] := Min(Times[Kind],
          RunDispatch(Kind, Bus, DispatchIterations, Checksum));
        Inc(Sink, Checksum);
      end;
      NsPerAccess := Times[Kind] * 1e9 / DispatchIterations;
      Writeln(Format('  %-22s %5.2f ns/read  (delta %5.2f ns)',
        [DispatchNames[Kind], NsPerAccess,
         NsPerAccess - Times[dkInline] * 1e9 / DispatchIterations]));
    end;
    InlineNs := Times[dkInline] * 1e9 / DispatchIterations;
    NsPerAccess := Times[dkInterface] * 1e9 / DispatchIterations;
    Writeln;

    Writeln('== CPU throughput (', AInstructions, ' ticks) ==');
    CountingBus := TCountingMemoryBus.Create;
    try
      CountTicks := Min(AInstructions, QWord(2000000));
      RunCpu(CountingBus, CountingBus, CountTicks);
      AccessesPerTick := (CountingBus.Reads + CountingBus.Writes) / CountTicks;
      Writeln(Format('  bus accesses/tick      %6.2f  (%.2f rd, %.2f wr)',
        [AccessesPerTick, CountingBus.Reads / CountTicks,
         CountingBus.Writes / CountTicks]));
    finally
      CountingBus.Free;
    end;

    Elapsed := RunCpu(Bus, Bus, AInstructions);
    NsPerTick := Elapsed * 1e9 / AInstructions;
    Writeln(Format('  elapsed                %6.2f s', [Elapsed]));
    Writeln(Format('  ns/tick                %6.2f', [NsPerTick]));
    Writeln(Format('  emulated MIPS          %6.2f  (~%.0fx a 4.77 MHz 8088)',
      [1000 / NsPerTick, 1000 / NsPerTick / RealMips]));

    { Overhead of the interface call over an inline access, multiplied by
      the access count. Ignores secondary effects: register pressure,
      inlining it would enable, and the CPU-side wrapper calls. }
    DispatchNsPerTick := AccessesPerTick * (NsPerAccess - InlineNs);
    Writeln(Format('  est. dispatch overhead %6.2f ns/tick  (%.1f%% of time)',
      [DispatchNsPerTick, DispatchNsPerTick / NsPerTick * 100]));
  finally
    Bus.Free;
  end;
end;

end.
