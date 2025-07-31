unit TestMachine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPjson, Generics.Collections, ZDeflate, Nullable,
  Hardware, Cpu8088;

type

  { TTest }

  TTest = class(TComponent)
  public
    type
      TRegs = specialize TDictionary<String, Word>;
      TRamItem = specialize TPair<TPhysicalAddress, Byte>;
      TRam = specialize TList<TRamItem>;

      { TState }

      TState = class(TComponent)
      public
        Regs: TRegs;
        Ram: TRam;
        Queue: TBytes;
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
      end;

  public
    TestName: String;
    Bytes: TBytes;
    Initial: TState;
    Final: TState;
    Idx: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadTest(TestData: TJsonData);
    function ToString: RTLString; override;
  end;

  { TTestMachine }

  TTestMachine = class(TComponent)
  private
    type

      { TMemoryBus }

      TMemoryBus = class(TComponent, IMemoryBus)
      public
        type
          TMemory = array[0..$FFFFF] of Byte;
      private
        FMemory: TMemory;
        procedure SetMemory(AValue: TMemory);
      public
        property Memory: TMemory read FMemory write SetMemory;
        procedure AttachDevice(ADevice: IMemoryBusDevice);
        procedure InvokeWrite(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
        procedure InvokeRead(ADevice: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte);
      end;

      { TIOBus }

      TIOBus = class(TComponent, IIOBus)
        procedure AttachDevice(ADevice: IIOBusDevice);
        procedure InvokeWrite(ADevice: IIOBusDevice; AAddress: Word; AData: Byte);
        procedure InvokeRead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte);
      end;

  private
    procedure LoadTest(ATest: TTest);
    function VerifyTest(ATest: TTest; out AErrors: TStringArray): Boolean;

  public
    Cpu: TCpu8088;
    MemoryBus: IMemoryBus;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RunTest(ATest: TTest; out AErrors: TStringArray): Boolean;
  end;

implementation

{ TTest }

procedure TTest.LoadTest(TestData: TJsonData);
  procedure ParseState(AData: TJsonObject; var AState: TState);
  var
    Item: TJSONEnum;
    RamItem: TRamItem;
  begin
    AState.Regs.Clear;
    for Item in AData.GetPath('regs') do
      AState.Regs.Add(Item.Key, Item.Value.AsInteger);

    AState.Ram.Clear;
    for Item in AData.GetPath('ram') do
    begin
      RamItem.Key := Item.Value.Items[0].AsInteger;
      RamItem.Value := Item.Value.Items[1].AsInteger;
      AState.Ram.Add(RamItem);
    end;

    SetLength(AState.Queue, AData.GetPath('queue').Count);
    for Item in AData.GetPath('queue') do
      AState.Queue[Item.KeyNum] := Item.Value.AsInteger;
  end;

var
  I: Integer;
begin
  TestName := TestData.GetPath('name').AsString;
  Idx := TestData.GetPath('idx').AsInteger;

  SetLength(Bytes, TestData.GetPath('bytes').Count);
  for I := 0 to High(Bytes) do
    Bytes[I] := TestData.GetPath('bytes').Items[I].AsInteger;

  ParseState(TestData.GetPath('initial') as TJSONObject, Initial);
  ParseState(TestData.GetPath('final') as TJSONObject, Final);
end;

function TTest.ToString: RTLString;
var
  Builder: TStringBuilder;
  B: Byte;
begin
  Builder := TStringBuilder.Create;
  Builder
    .Append('Test #')
    .Append(Idx)
    .Append(' | ');

  for B in Bytes do
    Builder
      .Append(IntToHex(B, 2))
      .Append(' ');

  Builder
    .Append('| ')
    .Append(TestName);

  Result := Builder.ToString;
  FreeAndNil(Builder);
end;

constructor TTest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Initial := TState.Create(Self);
  Final := TState.Create(Self);
end;

destructor TTest.Destroy;
begin
  inherited Destroy;
end;

{ TTest.TState }

constructor TTest.TState.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Regs := TRegs.Create;
  Ram := TRam.Create;
  Queue := [];
end;

destructor TTest.TState.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Regs);
  FreeAndNil(Ram);
end;

{ TTestMachine }

procedure TTestMachine.LoadTest(ATest: TTest);
var
  Value: Word;
  RamItem: TTest.TRamItem;
begin
  if ATest.Initial.Regs.TryGetValue('ax', Value) then Cpu.Registers.AX := Value;
  if ATest.Initial.Regs.TryGetValue('bx', Value) then Cpu.Registers.BX := Value;
  if ATest.Initial.Regs.TryGetValue('cx', Value) then Cpu.Registers.CX := Value;
  if ATest.Initial.Regs.TryGetValue('dx', Value) then Cpu.Registers.DX := Value;

  if ATest.Initial.Regs.TryGetValue('bp', Value) then Cpu.Registers.BP := Value;
  if ATest.Initial.Regs.TryGetValue('sp', Value) then Cpu.Registers.SP := Value;
  if ATest.Initial.Regs.TryGetValue('si', Value) then Cpu.Registers.SI := Value;
  if ATest.Initial.Regs.TryGetValue('di', Value) then Cpu.Registers.DI := Value;

  if ATest.Initial.Regs.TryGetValue('cs', Value) then Cpu.Registers.CS := Value;
  if ATest.Initial.Regs.TryGetValue('ds', Value) then Cpu.Registers.DS := Value;
  if ATest.Initial.Regs.TryGetValue('es', Value) then Cpu.Registers.ES := Value;
  if ATest.Initial.Regs.TryGetValue('ss', Value) then Cpu.Registers.SS := Value;

  if ATest.Initial.Regs.TryGetValue('ip', Value) then Cpu.Registers.IP := Value;
  if ATest.Initial.Regs.TryGetValue('flags', Value) then
    Cpu.Registers.Flags.SetWord(Value);

  for RamItem in ATest.Initial.Ram do
    MemoryBus.InvokeWrite(Nil, RamItem.Key, RamItem.Value);
end;

function TTestMachine.VerifyTest(
  ATest: TTest; out AErrors: TStringArray): Boolean;

  function VerifyRegister(ARegister: String; AExpected: Word; out AActual: Word): Boolean;
  begin
    case ARegister of
      'ax': AActual := Cpu.Registers.AX;
      'bx': AActual := Cpu.Registers.BX;
      'cx': AActual := Cpu.Registers.CX;
      'dx': AActual := Cpu.Registers.DX;
      'sp': AActual := Cpu.Registers.SP;
      'bp': AActual := Cpu.Registers.BP;
      'si': AActual := Cpu.Registers.SI;
      'di': AActual := Cpu.Registers.DI;
      'cs': AActual := Cpu.Registers.CS;
      'ds': AActual := Cpu.Registers.DS;
      'es': AActual := Cpu.Registers.ES;
      'ss': AActual := Cpu.Registers.SS;
      'ip': AActual := Cpu.Registers.IP;
      'flags': AActual := Cpu.Registers.Flags.GetWord;
    end;

    Result := AExpected = AActual;
  end;

  function VerifyFlag(AFlag: TFlagRegister.TFlags; AExpectedFlags, AActualFlags: Word): Boolean;
  begin
    Result := AExpectedFlags.Bits[Ord(AFlag)] = AActualFlags.Bits[Ord(AFlag)];
  end;

var
  ActualWord, ExpectedWord: Word;
  ActualByte, ExpectedByte: Byte;
  RamItem: TTest.TRamItem;
  Check: Boolean;
  WrongFlags: TStringArray;
  Reg: String;

begin
  Result := True;

  for Reg in ATest.Final.Regs.Keys do
  begin
    ExpectedWord := ATest.Final.Regs[Reg];
    if not VerifyRegister(Reg, ExpectedWord, ActualWord) then
    begin
      Insert(
        Format('%s: expected: %.4X, actual: %.4X',
          [UpCase(Reg), ExpectedWord, ActualWord]),
        AErrors,
        Integer.MaxValue);

      if Reg = 'flags' then
      begin
        WrongFlags := [];
        if not VerifyFlag(TFlagRegister.TFlags.flagC, ExpectedWord, ActualWord) then Insert('CF', WrongFlags, Integer.MaxValue);
        if not VerifyFlag(TFlagRegister.TFlags.flagP, ExpectedWord, ActualWord) then Insert('PF', WrongFlags, Integer.MaxValue);
        if not VerifyFlag(TFlagRegister.TFlags.flagA, ExpectedWord, ActualWord) then Insert('AF', WrongFlags, Integer.MaxValue);
        if not VerifyFlag(TFlagRegister.TFlags.flagZ, ExpectedWord, ActualWord) then Insert('ZF', WrongFlags, Integer.MaxValue);
        if not VerifyFlag(TFlagRegister.TFlags.flagS, ExpectedWord, ActualWord) then Insert('SF', WrongFlags, Integer.MaxValue);
        if not VerifyFlag(TFlagRegister.TFlags.flagT, ExpectedWord, ActualWord) then Insert('TF', WrongFlags, Integer.MaxValue);
        if not VerifyFlag(TFlagRegister.TFlags.flagI, ExpectedWord, ActualWord) then Insert('IF', WrongFlags, Integer.MaxValue);
        if not VerifyFlag(TFlagRegister.TFlags.flagD, ExpectedWord, ActualWord) then Insert('DF', WrongFlags, Integer.MaxValue);
        if not VerifyFlag(TFlagRegister.TFlags.flagO, ExpectedWord, ActualWord) then Insert('OF', WrongFlags, Integer.MaxValue);
        Insert(String.Join(', ', WrongFlags), AErrors, Integer.MaxValue);
      end;

      Result := False;
    end;
  end;

  for RamItem in ATest.Final.Ram do
  begin
    ExpectedByte := RamItem.Value;
    MemoryBus.InvokeRead(Nil, RamItem.Key, ActualByte);
    if ExpectedByte <> ActualByte then
    begin
      Insert(
        Format('RAM[%.5X]: expected: %.2X, actual: %.2X',
          [RamItem.Key, ExpectedByte, ActualByte]),
        AErrors,
        Integer.MaxValue);
      Result := False;
    end;
  end;
end;

constructor TTestMachine.Create(AOwner: TComponent);
var
  IOBus: TIOBus;
begin
  inherited Create(AOwner);
  MemoryBus := TMemoryBus.Create(Self);
  IOBus := TIOBus.Create(Self);

  Cpu := TCpu8088.Create(Self);
  Cpu.MemoryBus := MemoryBus;
  Cpu.IOBus := IOBus;
end;

destructor TTestMachine.Destroy;
begin
  inherited Destroy;
end;

function TTestMachine.RunTest(ATest: TTest; out AErrors: TStringArray): Boolean;
var
  Errors: TStringArray;
  Counter: Integer;
  Value: Word;
  FinalIP: specialize TNullable<Word>;
begin
  LoadTest(ATest);

  if ATest.Final.Regs.TryGetValue('ip', Value) then
    FinalIP := Value
  else
    FinalIP.Clear;

  Counter := 0;
  repeat
    if FinalIP.HasValue and (Cpu.Registers.IP = FinalIP.Value)
      and (not Cpu.CurrentInstruction.Repeating) then Break;

    Inc(Counter);
    Cpu.Tick;
    { Break; }
  until Counter > 100000;

  Result := VerifyTest(ATest, Errors);
  AErrors := Errors;
end;

{ TTestMachine.TMemoryBus }

procedure TTestMachine.TMemoryBus.SetMemory(AValue: TMemory);
begin
  FMemory := AValue;
end;

procedure TTestMachine.TMemoryBus.AttachDevice(ADevice: IMemoryBusDevice);
begin

end;

procedure TTestMachine.TMemoryBus.InvokeWrite(ADevice: IMemoryBusDevice;
  AAddress: TPhysicalAddress; AData: Byte);
begin
  FMemory[AAddress mod $100000] := AData;
end;

procedure TTestMachine.TMemoryBus.InvokeRead(ADevice: IMemoryBusDevice;
  AAddress: TPhysicalAddress; out AData: Byte);
begin
  AData := Memory[AAddress mod $100000];
end;

{ TTestMachine.TIOBus }

procedure TTestMachine.TIOBus.AttachDevice(ADevice: IIOBusDevice);
begin

end;

procedure TTestMachine.TIOBus.InvokeWrite(ADevice: IIOBusDevice;
  AAddress: Word; AData: Byte);
begin

end;

procedure TTestMachine.TIOBus.InvokeRead(ADevice: IIOBusDevice; AAddress: Word;
  out AData: Byte);
begin
  AData := $FF;
end;

end.

