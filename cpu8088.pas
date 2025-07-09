unit Cpu8088;

{$mode ObjFPC}{$H+}
{$interfaces CORBA}
{$R-}
{$Inline on}

interface

uses
  Classes, SysUtils, Math;

type
  TPhysicalAddress = 0..$FFFFF;

  IMemoryBusDevice = interface;
  IMemoryBus = interface;

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

  TInstruction = record
    CS, IP: Word;
    OpCode: Byte;
    Lock: Boolean;
    Repetition: (repNone, repRep, repRepE, repRepNE);
    SegmentOverride: (soNone, soCS, soDS, soES, soSS);
    Code: array[0..5] of Byte;
    Length: Integer;
    Repeating: Boolean;
  end;

  TInteruptHook = function(ASender: TObject; ANumber: Byte): Boolean of object;
  TInstructionNotifyEvent = procedure(ASender: TObject; AInstruction: TInstruction) of object;

  TInstructionHandler = procedure of object;

  { TFlagRegister }

  TFlagRegister = class(TComponent)
  private
    FBits: TBits;
    FParityTable: TBits;
    function GetAF: Boolean;
    function GetCF: Boolean;
    function GetDF: Boolean;
    function GetIF: Boolean;
    function GetOF: Boolean;
    function GetPF: Boolean;
    function GetSF: Boolean;
    function GetTF: Boolean;
    function GetZF: Boolean;
    procedure InitParityTable;
    procedure SetAF(AValue: Boolean);
    procedure SetCF(AValue: Boolean);
    procedure SetDF(AValue: Boolean);
    procedure SetIF(AValue: Boolean);
    procedure SetOF(AValue: Boolean);
    procedure SetPF(AValue: Boolean);
    procedure SetSF(AValue: Boolean);
    procedure SetTF(AValue: Boolean);
    procedure SetZF(AValue: Boolean);
    procedure UpdateAfterAdd8(AOld, AChange: Byte; AResult: Int16);
    procedure UpdateAfterAdd16(AOld, AChange: Word; AResult: Int32);
    procedure UpdateAfterSar16(AResult: Word; ALastShiftedOut: Boolean);
    procedure UpdateAfterSub16(AOld, AChange: Word; AResult: Int32);
    procedure UpdateAfterSub8(AOld, AChange: Byte; AResult: Int16);
    procedure UpdateAfterAnd8(AResult: Byte);
    procedure UpdateAfterAnd16(AResult: Word);
    procedure UpdateAfterOr8(AResult: Byte);
    procedure UpdateAfterOr16(AResult: Word);
    procedure UpdateAfterNeg8(AResult: Byte);
    procedure UpdateAfterNeg16(AResult: Word);
    procedure UpdateAfterXor8(AResult: Byte);
    procedure UpdateAfterXor16(AResult: Word);
    procedure UpdateAfterDec8(AOld: Byte; AResult: Int16);
    procedure UpdateAfterDec16(AOld: Word; AResult: Int32);
    procedure UpdateAfterInc8(AOld: Byte; AResult: Int16);
    procedure UpdateAfterInc16(AOld: Word; AResult: Int32);
    procedure UpdateAfterShl8(AOld: Byte; ACount: Byte; AResult: Byte);
    procedure UpdateAfterShl16(AOld: Word; ACount: Byte; AResult: Word);
    procedure UpdateAfterShr8(AOld: Byte; ACount: Byte; AResult: Byte);
    procedure UpdateAfterShr16(AOld: Word; ACount: Byte; AResult: Word);
    procedure UpdateAfterSar8(AResult: Byte; ALastShiftedOut: Boolean);
    procedure UpdateAfterMul8(AResult: Word);
    procedure UpdateAfterMul16(AResult: DWord);
  public
    type
      TFlags = (
        flagC = 0, flagP = 2, flagA = 4, flagZ = 5, flagS = 6,
        flagT = 7, flagI = 8, flagD = 9, flagO = 10);
      TFlagSet = set of TFlags;
  public
    property AF: Boolean read GetAF write SetAF;
    property PF: Boolean read GetPF write SetPF;
    property CF: Boolean read GetCF write SetCF;
    property ZF: Boolean read GetZF write SetZF;
    property SF: Boolean read GetSF write SetSF;
    property OF_: Boolean read GetOF write SetOF;
    property TF: Boolean read GetTF write SetTF;
    property IF_: Boolean read GetIF write SetIF;
    property DF: Boolean read GetDF write SetDF;
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    function GetWord: Word;
    procedure SetWord(AValue: Word);
    procedure Clear(AFlag: TFlags);
    procedure Set_(AFlag: TFlags);
    procedure SetValue(AFlag: TFlags; AValue: Boolean);
    function Get(AFlag: TFlags): Boolean;

    procedure UpdatePF8(AValue: Byte); inline;
    procedure UpdateZF8(AValue: Byte); inline;
    procedure UpdateSF8(AValue: Byte); inline;
    procedure UpdateAFAdd8(AOp1, AOp2: Byte; AResult: Int16); inline;
    procedure UpdateOFAdd8(AOp1, AOp2: Byte; AResult: Int16); inline;
    procedure UpdateCFAdd8(AOp1, AOp2: Byte; AResult: Int16); inline;
    procedure UpdateAFSub8(AOp1, AOp2: Byte; AResult: Int16); inline;
    procedure UpdateOFSub8(AOp1, AOp2: Byte; AResult: Int16); inline;
    procedure UpdateCFSub8(AOp1, AOp2: Byte; AResult: Int16); inline;

    procedure UpdatePF16(AValue: Word); inline;
    procedure UpdateZF16(AValue: Word); inline;
    procedure UpdateSF16(AValue: Word); inline;
    procedure UpdateAFAdd16(AOp1, AOp2: Word; AResult: Int32); inline;
    procedure UpdateOFAdd16(AOp1, AOp2: Word; AResult: Int32); inline;
    procedure UpdateCFAdd16(AOp1, AOp2: Word; AResult: Int32); inline;
    procedure UpdateAFSub16(AOp1, AOp2: Word; AResult: Int32); inline;
    procedure UpdateOFSub16(AOp1, AOp2: Word; AResult: Int32); inline;
    procedure UpdateCFSub16(AOp1, AOp2: Word; AResult: Int32); inline;
  end;

  { TRegisters }

  TRegisters = class(TComponent)
  protected
    function GetAH: Byte;
    function GetAL: Byte;
    function GetBH: Byte;
    function GetBL: Byte;
    function GetCH: Byte;
    function GetCL: Byte;
    function GetDH: Byte;
    function GetDL: Byte;
    procedure SetAH(AValue: Byte);
    procedure SetAL(AValue: Byte);
    procedure SetBH(AValue: Byte);
    procedure SetBL(AValue: Byte);
    procedure SetCH(AValue: Byte);
    procedure SetCL(AValue: Byte);
    procedure SetDH(AValue: Byte);
    procedure SetDL(AValue: Byte);
  public
    type
      TRegIndex8 = (riAL = 0, riCL, riDL, riBL, riAH, riCH, riDH, riBH);
      TRegIndex16 = (
        riAX = 0, riCX, riDX, riBX, riSP, riBP, riSI, riDI,
        riES, riCS, riSS, riDS,
        riIP);
    const
      // FLAGS e X:X:X:X:(OF):(DF):(IF):(TF):(SF):(ZF):X:(AF):X:(PF):X:(CF)
  public
    AX, BX, CX, DX,
    SI, DI, BP, SP, IP,
    CS, DS, ES, SS: Word;
    Flags: TFlagRegister;

    property AH: Byte read GetAH write SetAH;
    property AL: Byte read GetAL write SetAL;
    property BH: Byte read GetBH write SetBH;
    property BL: Byte read GetBL write SetBL;
    property CH: Byte read GetCH write SetCH;
    property CL: Byte read GetCL write SetCL;
    property DH: Byte read GetDH write SetDH;
    property DL: Byte read GetDL write SetDL;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetByIndex8(AIndex: TRegIndex8): Byte; overload;
    function GetByIndex8(AIndex: Byte): Byte; overload;
    procedure SetByIndex8(AIndex: TRegIndex8; AValue: Byte); overload;
    procedure SetByIndex8(AIndex: Byte; AValue: Byte); overload;

    function GetByIndex16(AIndex: TRegIndex16): Word;
    function GetByIndex16(AIndex: Byte): Word; overload;
    procedure SetByIndex16(AIndex: TRegIndex16; AValue: Word);
    procedure SetByIndex16(AIndex: Byte; AValue: Word); overload;

    procedure Log;

    class Function RegToStr(ARegIndex: TRegIndex8): String;
    class Function RegToStr(ARegIndex: TRegIndex16): String;
  end;

  { TCpu8088 }

  TCpu8088 = class(TComponent, IIOBusDevice, IMemoryBusDevice)
  private
    type
      TModRM = bitpacked record
        Rm: 0..%111;
        Reg: 0..%111;
        Mod_: (modMemNoDisp, modMem8, modMem16, modRegister);
        Segment: Word;
        EffectiveAddr: Word;
      end;
    procedure DecRM16(AModRM: TModRM);
    procedure EnterISR(ANumber: Byte);
    procedure ExecuteCurrentInstruction;
    procedure RaiseSoftwareInterrupt(ANumber: Byte);
    function ReadMemoryByte(ASegment, AOffset: Word): Byte;
    procedure WriteMemoryWord(ASegment, AOffset: Word; AData: Word);
    procedure WriteMemoryByte(ASegment, AOffset: Word; AData: Byte);
  private
    FHalted: Boolean;
    FOnAfterInstruction: TInstructionNotifyEvent;
    FOnBeforeInstruction: TNotifyEvent;
    FInterruptHook: TInteruptHook;
    FTicks: QWord;
    FIOBus: IIOBus;
    FMemoryBus: IMemoryBus;
    FRegisters: TRegisters;
    FInstructionHandlers: array[0..$FF] of TInstructionHandler;
    FHardwareInterrupt: record
      Pending: Boolean;
      Number: Byte;
    end;
    FCurrentInstruction: TInstruction;

    function CodeSegment: Word;
    procedure SetInterruptHook(AValue: TInteruptHook);
    procedure SetOnAfterInstruction(AValue: TInstructionNotifyEvent);
    procedure SetOnBeforeInstruction(AValue: TNotifyEvent);
    function StackSegment: Word;
    function DataSegment: Word;
    function ExtraSegment: Word;

    procedure RaiseHardwareInterrupt(ANumber: Byte);

    function ReadMemoryWord(ASegment, AOffset: Word): Word;
    function FetchCodeByte: Byte;
    function FetchCodeWord: Word;
    function FetchModRM: TModRM;

    function ReadRM8(AModRM: TModRM): Byte;
    function ReadRM16(AModRM: TModRM): Word;
    procedure WriteRM8(AModRM: TModRM; AValue: Byte);
    procedure WriteRM16(AModRM: TModRM; AValue: Word);
    procedure FillEffectiveAddress(var AModRM: TModRM);

    procedure Push(AValue: Word);
    function Pop: Word;

    procedure InitInstructionHandlers;
    procedure HandleInvalidInstruction;
    procedure HandleHardwareInterrupt;

    procedure HandleAddRM8Reg8;  { $ 00 }
    procedure HandleAddRM16Reg16;  { $ 01 }
    procedure HandleAddReg8RM8;  { $ 02 }
    procedure HandleAddReg16RM16;  { $ 03 }
    procedure HandleAddALImm8;  { $04 }
    procedure HandleAddAXImm16;  { $05 }
    procedure HandlePushES;  { $06 }
    procedure HandlePopES;  { $07 }
    procedure HandleOrReg8RM8;  { $0A }
    procedure HandleOrReg16RM16;  { $0A }
    procedure HandleOrALImm8;  { $0C }
    procedure HandleOrAXImm16;  { $0D }
    procedure HandlePushCS;  { $0E }

    procedure HandleAdcRM8Reg8;  { $10 }
    procedure HandleAdcRM16Reg16;  { $11 }
    procedure HandleAdcALImm8;  { $14 }
    procedure HandleAdcAXImm16;  { $15 }
    procedure HandlePushSS;  { $16 }
    procedure HandlePopSS;  { $17 }
    procedure HandleSbbRM8Reg8;  { $ 18 }
    procedure HandleSbbRM16Reg16;  { $ 19 }
    procedure HandleSbbALImm8;  { $1C }
    procedure HandleSbbAXImm16;  { $1D }
    procedure HandlePushDS;  { $1E }
    procedure HandlePopDS;  { $1F }

    procedure HandleAndReg8RM8;  { $22 }
    procedure HandleAndReg16RM16;  { $23 }
    procedure HandleAndALImm8;  { 24 }
    procedure HandleAndAXImm16;  { 24 }
    procedure HandleSubALImm8;  { $2C }
    procedure HandleSubAXImm16;  { $2D }
    procedure HandleSubRM8Reg8;  { $28 }
    procedure HandleSubRM16Reg16;  { $29 }
    procedure HandleSubReg8RM8;  { $2A }
    procedure HandleSubReg16RM16;  { $2B }

    procedure HandleXorReg8RM8;  { $32 }
    procedure HandleXorReg16RM16;  { $33 }
    procedure HandleCmpRM8Reg8;  { $38 }
    procedure HandleCmpRM16Reg16;  { $39 }
    procedure HandleCmpReg8RM8;  { $3A }
    procedure HandleCmpReg16RM16;  { $3B }
    procedure HandleCmpALImm8;  { $3C }
    procedure HandleCmpAXImm16;  { $3D }

    procedure HandleIncReg16;  { $40..47 }
    procedure HandleDecReg16;  { $48..4F }

    procedure HandlePushReg16;  { $50..$57 }
    procedure HandlePopReg16;  { $58..$5F }

    procedure HandleJbShort;  { $72 }
    procedure HandleJaeShort;  { $73 }
    procedure HandleJeShort;  { $74 }
    procedure HandleJneShort;  { $74 }
    procedure HandleJbeShort;  { $76 }
    procedure HandleJaShort;  { $77 }
    procedure HandleJsShort;  { $78 }
    procedure HandleJnsShort;  { $79 }
    procedure HandleJpeShort;  { $7A }
    procedure HandleJpoShort;  { $7B }
    procedure HandleJngeShort;  { $7C }
    procedure HandleJnlShort;  { $7D }
    procedure HandleJngShort;  { $7E }
    procedure HandleJgShort;  { $7F }

    procedure HandleGRP1RM8Imm8;  { $80, $82 }
    procedure HandleGRP1RM16Imm16;  { $81 }
    procedure HandleGRP1RM16Imm8;  { 83 }
    procedure HandleMovRM8Reg8;  { $88 }
    procedure HandleMovRM16Reg16;  { $89 }
    procedure HandleMovReg8RM8;  { $8A }
    procedure HandleMovReg16RM16;  { $8B }
    procedure HandleMovSRegRM16;  { $8E }

    procedure HandleNop;  { $90 }
    procedure HandleXchgReg16;  { $91..$97 }
    procedure HandleCbw;  { $98 }
    procedure HandleCwd;  { $99 }
    procedure HandleCallFar;  { $9A }
    procedure HandleWait;  { $9B }
    procedure HandlePushf;  { $9C }
    procedure HandlePopf;  { $9D }
    procedure HandleSahf;  { $9E }
    procedure HandleLahf;  { $9F }

    procedure HandleMovALDisp16;  { $A0 }
    procedure HandleMovAXDisp16;  { $A1 }
    procedure HandleMovDisp16AL;  { $A2 }
    procedure HandleMovDisp16AX;  { $A3 }
    procedure HandleMovsb;  { $A4 }
    procedure HandleMovsw;  { $A5 }
    procedure HandleTestALImm8;  { $A8 }
    procedure HandleTestAXImm16;  { $A9 }
    procedure HandleStosb;  { $AA }
    procedure HandleStosw;  { $AB }
    procedure HandleLodsb;  { $AC }
    procedure HandleLodsw;  { $AD }

    procedure HandleMovReg8Imm8;  { $B0..$B7 }
    procedure HandleMovReg16Imm16;  { $B8..$BF }

    procedure HandleRetiNear;  { $C2 }
    procedure HandleRetNear;  { $C3 }
    procedure HandleLes;  { $C4 }
    procedure HandleLds;  { $C5 }
    procedure HandleMovRM8Imm8;  { $C6 }
    procedure HandleMovRM16Imm16;  { $C7 }
    procedure HandleRetiFar;  { $CA }
    procedure HandleRetFar;  { $CB }
    procedure HandleInt3;  { $CC }
    procedure HandleIntImm8;  { $CD }
    procedure HandleInto;  { $CE }
    procedure HandleIret;  { $CF }

    procedure HandleGRP2RM8Const1; { D0 }
    procedure HandleGRP2RM16Const1; { D1 }
    procedure HandleGRP2RM8CL; { D2 }
    procedure HandleGRP2RM16CL; { D3 }
    procedure HandleXlat;  { D7 }

    procedure HandleLoop;  { $E2 }
    procedure HandleJcxz;  { $E3 }
    procedure HandleInALImm8;  { $E4 }
    procedure HandleInAXImm8;  { $E5 }
    procedure HandleOutImm8AL;  { $E6 }
    procedure HandleOutImm8AX;  { $E7 }
    procedure HandleCallNear;  { $E8 }
    procedure HandleJmpNear;  { $E9 }
    procedure HandleJmpFar;  { $EA }
    procedure HandleJmpShort;  { $EB }
    procedure HandleInALDX;  { $EC }
    procedure HandleInAXDX;  { $ED }
    procedure HandleOutDXAL;  { $EE }
    procedure HandleOutDXAX;  { $EF }

    procedure HandleInt1;  { $F1 }
    procedure HandleHlt;  { $F4 }
    procedure HandleCmc;  { $F5 }
    procedure HandleGRP3A;  { $F6 }
    procedure HandleGRP3B;  { $F7 }
    procedure HandleClc;  { $F8 }
    procedure HandleStc;  { $F9 }
    procedure HandleCli;  { $FA }
    procedure HandleSti;  { $FB }
    procedure HandleCld;  { $FC }
    procedure HandleStd;  { $FD }
    procedure HandleGRP4;  { $FE }
    procedure HandleGRP5;  { $FF }

    procedure HandleDiv8;   { F6 }
    procedure HandleDiv16;   { F7 }

    procedure IncReg8(ARegIndex: TRegisters.TRegIndex8);

    procedure PushReg16(ARegIndex: TRegisters.TRegIndex16);
    procedure PopReg16(ARegIndex: TRegisters.TRegIndex16);
    procedure IncReg16(ARegIndex: TRegisters.TRegIndex16);
    procedure DecReg16(ARegIndex: TRegisters.TRegIndex16);
    procedure DecRM8(AModRM: TModRM);
    procedure XchgReg16Reg16(ARegIndex1, ARegIndex2: TRegisters.TRegIndex16);
    procedure AddRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure AddRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure AndRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure AndRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure Cmp8(AFirst, ASecond: Byte);
    procedure Cmp16(AFirst, ASecond: Word);
    procedure JumpShort(ADisplacement: Int8); inline;
    procedure JumpNear(ADisplacement: Int16); inline;
    procedure JumpFar(ASegment, AOffset: Word); inline;
    procedure NotRM8(AModRM: TModRM);
    procedure NotRM16(AModRM: TModRM);
    procedure ShlRM8Const1(AModRM: TModRM);
    procedure ShlRM16Const1(AModRM: TModRM);
    procedure ShlRM8CL(AModRM: TModRM);
    procedure ShlRM16CL(AModRM: TModRM);
    procedure ShrRM8Const1(AModRM: TModRM);
    procedure ShrRM16Const1(AModRM: TModRM);
    procedure ShrRM8CL(AModRM: TModRM);
    procedure ShrRM16CL(AModRM: TModRM);
    procedure SarRM8Const1(AModRM: TModRM);
    procedure SarRM16Const1(AModRM: TModRM);
    procedure SarRM8CL(AModRM: TModRM);
    procedure SarRM16CL(AModRM: TModRM);
    procedure TestRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure TestRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure MulALRM8(AModRM: TModRM);
    procedure MulAXRM16(AModRM: TModRM);
    procedure IncRM8(AModRM: TModRM);
    procedure IncRM16(AModRM: TModRM);
    procedure HandleRepetition;
  public
    procedure DumpCurrentInstruction;
    property Ticks: QWord read FTicks;
    property Halted: Boolean read FHalted;
    property Registers: TRegisters read FRegisters write FRegisters;
    property OnBeforeInstruction: TNotifyEvent read FOnBeforeInstruction write SetOnBeforeInstruction;
    property OnAfterInstruction: TInstructionNotifyEvent read FOnAfterInstruction write SetOnAfterInstruction;
    property InterruptHook: TInteruptHook read FInterruptHook write SetInterruptHook;
    constructor Create(AOwner: TComponent); override;
    procedure Reset;
    procedure Tick;
    procedure FetchInstruction;

    { IO bus device API }
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);

    { Memory bus device API }
    function GetMemoryBus: IMemoryBus;
    procedure SetMemoryBus(AValue: IMemoryBus);
    procedure WriteMemoryByte(AAddress: TPhysicalAddress; AData: Byte);
    function ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
    property MemoryBus: IMemoryBus read GetMemoryBus write SetMemoryBus;
    function OnMemoryRead(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte): Boolean;
    procedure OnMemoryWrite(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);
  end;

implementation

{ TFlagRegister }

procedure TFlagRegister.InitParityTable;
var
  I, J: Integer;
  Value: Boolean;
begin
  for I := 0 To FParityTable.Size - 1 do
  begin
    Value := True;
    for J := 0 to 7 do
      if (I and (1 shl J)) <> 0 then
        Value := not Value;
    FParityTable[I] := Value;
  end;
end;

function TFlagRegister.GetAF: Boolean;
begin
  Result := FBits[Ord(flagA)];
end;

function TFlagRegister.GetCF: Boolean;
begin
  Result := FBits[Ord(flagC)];
end;

function TFlagRegister.GetDF: Boolean;
begin
  Result := FBits[Ord(flagD)];
end;

function TFlagRegister.GetIF: Boolean;
begin
  Result := FBits[Ord(flagI)];
end;

function TFlagRegister.GetOF: Boolean;
begin
  Result := FBits[Ord(flagO)];
end;

function TFlagRegister.GetPF: Boolean;
begin
  Result := FBits[Ord(flagP)];
end;

function TFlagRegister.GetSF: Boolean;
begin
  Result := FBits[Ord(flagS)];
end;

function TFlagRegister.GetTF: Boolean;
begin
  Result := FBits[Ord(flagT)];
end;

function TFlagRegister.GetZF: Boolean;
begin
  Result := FBits[Ord(flagZ)];
end;

procedure TFlagRegister.SetAF(AValue: Boolean);
begin
  FBits[Ord(flagA)] := AValue;
end;

procedure TFlagRegister.SetCF(AValue: Boolean);
begin
  FBits[Ord(flagC)] := AValue;
end;

procedure TFlagRegister.SetDF(AValue: Boolean);
begin
  FBits[Ord(flagD)] := AValue;
end;

procedure TFlagRegister.SetIF(AValue: Boolean);
begin
  FBits[Ord(flagI)] := AValue;
end;

procedure TFlagRegister.SetOF(AValue: Boolean);
begin
  FBits[Ord(flagO)] := AValue;
end;

procedure TFlagRegister.SetPF(AValue: Boolean);
begin
  FBits[Ord(flagP)] := AValue;
end;

procedure TFlagRegister.SetSF(AValue: Boolean);
begin
  FBits[Ord(flagS)] := AValue;
end;

procedure TFlagRegister.SetTF(AValue: Boolean);
begin
  FBits[Ord(flagT)] := AValue;
end;

procedure TFlagRegister.SetZF(AValue: Boolean);
begin
  FBits[Ord(flagZ)] := AValue;
end;

constructor TFlagRegister.Create(AOwner: TComponent);
begin
  inherited Create(AOWner);
  FBits := TBits.Create(16);
  FParityTable := TBits.Create(256);
  InitParityTable;
  Reset;
end;

destructor TFlagRegister.Destroy;
begin
  FreeAndNil(FBits);
  FreeAndNil(FParityTable);
  inherited Destroy;
end;

procedure TFlagRegister.Reset;
begin
  FBits.Clearall;
  FBits.SetOn(1);
end;

function TFlagRegister.GetWord: Word;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 15 do
    if FBits[I] then
      Result := Result or (1 shl I);
end;

procedure TFlagRegister.SetWord(AValue: Word);
var
  I: Integer;
  MaskedValue: Word;
begin
  MaskedValue := AValue or $02;
  for I := 0 to 15 do
    if (MaskedValue and (1 shl I)) <> 0 then
      FBits.SetOn(I)
    else
      FBits.Clear(I);
end;

procedure TFlagRegister.Clear(AFlag: TFlags);
begin
  FBits.Clear(Ord(AFlag));
end;

procedure TFlagRegister.Set_(AFlag: TFlags);
begin
  FBits.SetOn(Ord(AFlag));
end;

procedure TFlagRegister.SetValue(AFlag: TFlags; AValue: Boolean);
begin
  if AValue then
    FBits.SetOn(Ord(AFlag))
  else
    FBits.Clear(Ord(AFlag));
end;

function TFlagRegister.Get(AFlag: TFlags): Boolean;
begin
  Result := FBits[Ord(AFlag)];
end;

procedure TFlagRegister.UpdatePF8(AValue: Byte);
begin
  PF := FParityTable[AValue];
end;

procedure TFlagRegister.UpdateZF8(AValue: Byte);
begin
  FBits[Ord(flagZ)] := AValue = 0;
end;

procedure TFlagRegister.UpdateSF8(AValue: Byte);
begin
  FBits[Ord(flagS)] := Int8(AValue) < 0;
end;

procedure TFlagRegister.UpdateAFAdd8(AOp1, AOp2: Byte; AResult: Int16);
begin
  AF := ((AOp1 xor AOp2 xor AResult) and $10) <> 0;
end;

procedure TFlagRegister.UpdateOFAdd8(AOp1, AOp2: Byte; AResult: Int16);
begin
  FBits[Ord(flagO)] := not InRange(AResult, Int8.MinValue, Int8.MaxValue);
end;

procedure TFlagRegister.UpdateCFAdd8(AOp1, AOp2: Byte; AResult: Int16);
begin
  CF := not InRange(AResult, Byte.MinValue, Byte.MaxValue);
end;

procedure TFlagRegister.UpdateAFSub8(AOp1, AOp2: Byte; AResult: Int16);
begin
  AF := ((AOp1 xor AOp2 xor AResult) and $10) <> 0;
end;

procedure TFlagRegister.UpdateOFSub8(AOp1, AOp2: Byte; AResult: Int16);
begin
  OF_ := ((AOp1 xor AOp2) and (AOp1 xor AResult) and $80) <> 0;
end;

procedure TFlagRegister.UpdateCFSub8(AOp1, AOp2: Byte; AResult: Int16);
begin
  CF := AOp1 < AOp2;
end;

procedure TFlagRegister.UpdatePF16(AValue: Word);
begin
  PF := FParityTable[Lo(AValue)];
end;

procedure TFlagRegister.UpdateZF16(AValue: Word);
begin
  ZF := AValue = 0;
end;

procedure TFlagRegister.UpdateSF16(AValue: Word);
begin
  SF := Int16(AValue) < 0;
end;

procedure TFlagRegister.UpdateAFAdd16(AOp1, AOp2: Word; AResult: Int32);
begin
  AF := ((AOp1 xor AOp2 xor AResult) and $10) <> 0;
end;

procedure TFlagRegister.UpdateOFAdd16(AOp1, AOp2: Word; AResult: Int32);
begin
  OF_ := not InRange(AResult, Int16.MinValue, Int16.MaxValue);
end;

procedure TFlagRegister.UpdateCFAdd16(AOp1, AOp2: Word; AResult: Int32);
begin
  CF := not InRange(AResult, Word.MinValue, Word.MaxValue);
end;

procedure TFlagRegister.UpdateAFSub16(AOp1, AOp2: Word; AResult: Int32);
begin
  AF := ((AOp1 xor AOp2 xor AResult) and $10) <> 0;
end;

procedure TFlagRegister.UpdateOFSub16(AOp1, AOp2: Word; AResult: Int32);
begin
  OF_ := ((AOp1 xor AOp2) and (AOp1 xor AResult) and $8000) <> 0;
end;

procedure TFlagRegister.UpdateCFSub16(AOp1, AOp2: Word; AResult: Int32);
begin
  CF := AOp1 < AOp2;
end;

procedure TFlagRegister.UpdateAfterAdd8(AOld, AChange: Byte; AResult: Int16);
begin
  UpdateZF8(AResult);
  UpdatePF8(AResult);
  UpdateSF8(AResult);
  UpdateAFAdd8(AOld, AChange, AResult);
  UpdateCFAdd8(AOld, AChange, AResult);
  UpdateOFAdd8(AOld, AChange, AResult);
end;

procedure TFlagRegister.UpdateAfterAdd16(AOld, AChange: Word; AResult: Int32);
begin
  UpdateZF16(AResult);
  UpdatePF16(AResult);
  UpdateSF16(AResult);
  UpdateAFAdd16(AOld, AChange, AResult);
  UpdateCFAdd16(AOld, AChange, AResult);
  UpdateOFAdd16(AOld, AChange, AResult);
end;

procedure TFlagRegister.UpdateAfterSub8(AOld, AChange: Byte; AResult: Int16);
begin
  UpdateZF8(Byte(AResult));
  UpdatePF8(Byte(AResult));
  UpdateSF8(Byte(AResult));
  UpdateAFSub8(AOld, AChange, AResult);
  UpdateCFSub8(AOld, AChange, AResult);
  UpdateOFSub8(AOld, AChange, AResult);
end;

procedure TFlagRegister.UpdateAfterAnd8(AResult: Byte);
begin
  OF_ := False;
  CF := False;
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);
end;

procedure TFlagRegister.UpdateAfterAnd16(AResult: Word);
begin
  OF_ := False;
  CF := False;
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);
end;

procedure TFlagRegister.UpdateAfterOr8(AResult: Byte);
begin
  OF_ := False;
  CF := False;
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);
end;

procedure TFlagRegister.UpdateAfterOr16(AResult: Word);
begin
  OF_ := False;
  CF := False;
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);
end;

procedure TFlagRegister.UpdateAfterNeg8(AResult: Byte);
begin

end;

procedure TFlagRegister.UpdateAfterNeg16(AResult: Word);
begin

end;

procedure TFlagRegister.UpdateAfterXor8(AResult: Byte);
begin
  OF_ := False;
  CF := False;
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);
end;

procedure TFlagRegister.UpdateAfterXor16(AResult: Word);
begin
  OF_ := False;
  CF := False;
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);
end;

procedure TFlagRegister.UpdateAfterDec8(AOld: Byte; AResult: Int16);
begin
  UpdateZF8(AResult);
  UpdatePF8(AResult);
  UpdateSF8(AResult);
  UpdateAFSub8(AOld, 1, AResult);
  UpdateOFSub8(AOld, 1, AResult);
end;

procedure TFlagRegister.UpdateAfterDec16(AOld: Word; AResult: Int32);
begin
  UpdateZF16(AResult);
  UpdatePF16(AResult);
  UpdateSF16(AResult);
  UpdateAFSub16(AOld, 1, AResult);
  UpdateOFSub16(AOld, 1, AResult);
end;

procedure TFlagRegister.UpdateAfterInc8(AOld: Byte; AResult: Int16);
begin
  UpdateZF8(Word(AResult));
  UpdatePF8(Word(AResult));
  UpdateSF8(Word(AResult));
  UpdateAFAdd8(AOld, 1, AResult);
  UpdateOFAdd8(AOld, 1, AResult);
end;

procedure TFlagRegister.UpdateAfterInc16(AOld: Word; AResult: Int32);
begin
  UpdateZF16(Word(AResult));
  UpdatePF16(Word(AResult));
  UpdateSF16(Word(AResult));
  UpdateAFAdd16(AOld, 1, AResult);
  UpdateOFAdd16(AOld, 1, AResult);
end;

procedure TFlagRegister.UpdateAfterShl8(
  AOld: Byte; ACount: Byte; AResult: Byte);
begin
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);
  if ACount < 8 then
    CF := (AOld and (1 shl (8 - ACount)) <> 0)
  else
    CF := False;

  if ACount = 1 then OF_ := not (((AResult and $80) <> 0) xor CF);
end;

procedure TFlagRegister.UpdateAfterShl16(
  AOld: Word; ACount: Byte; AResult: Word);
begin
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);
  if ACount < 8 then
    CF := (AOld and (1 shl (16 - ACount)) <> 0)
  else
    CF := False;

  if ACount = 1 then OF_ := not (((AResult and $8000) <> 0) xor CF);
end;

procedure TFlagRegister.UpdateAfterShr8(
  AOld: Byte; ACount: Byte; AResult: Byte);
begin
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);
  CF := (AOld and (1 shl ACount)) <> 0;
  if ACount = 1 then OF_ := (AResult and $80) <> 0;
end;

procedure TFlagRegister.UpdateAfterShr16(
  AOld: Word; ACount: Byte; AResult: Word);
begin
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);
  CF := (AOld and (1 shl ACount)) <> 0;
  if ACount = 1 then OF_ := (AResult and $8000) <> 0;
end;

procedure TFlagRegister.UpdateAfterSar8(AResult: Byte; ALastShiftedOut: Boolean);
begin
  CF := ALastShiftedOut;
  OF_ := False;
  UpdateSF8(AResult);
  UpdateZF8(AResult);
  UpdatePF8(AResult);
end;

procedure TFlagRegister.UpdateAfterMul8(AResult: Word);
begin
  CF := Hi(AResult) = 0;
  OF_ := CF;
end;

procedure TFlagRegister.UpdateAfterMul16(AResult: DWord);
begin
  CF := Hi(AResult) = 0;
  OF_ := CF;
end;

procedure TFlagRegister.UpdateAfterSar16(AResult: Word; ALastShiftedOut: Boolean);
begin
  CF := ALastShiftedOut;
  OF_ := False;
  UpdateSF16(AResult);
  UpdateZF16(AResult);
  UpdatePF16(AResult);
end;

procedure TFlagRegister.UpdateAfterSub16(AOld, AChange: Word; AResult: Int32);
begin
  UpdateZF16(Word(AResult));
  UpdatePF16(Word(AResult));
  UpdateSF16(Word(AResult));
  UpdateAFSub16(AOld, AChange, AResult);
  UpdateCFSub16(AOld, AChange, AResult);
  UpdateOFSub16(AOld, AChange, AResult);
end;

{ TRegisters }

function TRegisters.GetAH: Byte; inline;
begin
  Result := Hi(AX);
end;

function TRegisters.GetAL: Byte; inline;
begin
  Result := Lo(AX);
end;

function TRegisters.GetBH: Byte; inline;
begin
  Result := Hi(BX);
end;

function TRegisters.GetBL: Byte; inline;
begin
  Result := Lo(BX);
end;

function TRegisters.GetCH: Byte; inline;
begin
  Result := Hi(CX);
end;

function TRegisters.GetCL: Byte; inline;
begin
  Result := Lo(CX);
end;

function TRegisters.GetDH: Byte; inline;
begin
  Result := Hi(DX);
end;

function TRegisters.GetDL: Byte; inline;
begin
  Result := Lo(DX);
end;

procedure TRegisters.SetAH(AValue: Byte); inline;
begin
  AX := (AValue shl 8) or AL;
end;

procedure TRegisters.SetAL(AValue: Byte); inline;
begin
  AX := (AX and $FF00) or AValue;
end;

procedure TRegisters.SetBH(AValue: Byte); inline;
begin
  BX := (AValue shl 8) or BL;
end;

procedure TRegisters.SetBL(AValue: Byte); inline;
begin
  BX := (BX and $FF00) or AValue;
end;

procedure TRegisters.SetCH(AValue: Byte); inline;
begin
  CX := (AValue shl 8) or CL;
end;

procedure TRegisters.SetCL(AValue: Byte); inline;
begin
  CX := (CX and $FF00) or AValue;
end;

procedure TRegisters.SetDH(AValue: Byte);
begin
  DX := (AValue shl 8) or DL;
end;

procedure TRegisters.SetDL(AValue: Byte);
begin
  DX := (DX and $FF00) or AValue;
end;

constructor TRegisters.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Flags := TFlagRegister.Create(Self);
end;

destructor TRegisters.Destroy;
begin
  inherited Destroy;
end;

function TRegisters.GetByIndex8(AIndex: TRegIndex8): Byte;
begin
  case AIndex of
    riAL: Result := AL;
    riCL: Result := CL;
    riDL: Result := DL;
    riBL: Result := BL;
    riAH: Result := AH;
    riCH: Result := CH;
    riDH: Result := DH;
    riBH: Result := BH;
  end;
end;

function TRegisters.GetByIndex8(AIndex: Byte): Byte;
begin
  Result := GetByIndex8(TRegIndex8(AIndex));
end;

procedure TRegisters.SetByIndex8(AIndex: TRegIndex8; AValue: Byte);
begin
  case AIndex of
    riAL: AL := AValue;
    riCL: CL := AValue;
    riDL: DL := AValue;
    riBL: BL := AValue;
    riAH: AH := AValue;
    riCH: CH := AValue;
    riDH: DH := AValue;
    riBH: BH := AValue;
  end;
end;

procedure TRegisters.SetByIndex8(AIndex: Byte; AValue: Byte);
begin
  SetByIndex8(TRegIndex8(AIndex), AValue);
end;

function TRegisters.GetByIndex16(AIndex: TRegIndex16): Word;
begin
  case AIndex of
    riAX: Result := AX;
    riBX: Result := BX;
    riCX: Result := CX;
    riDX: Result := DX;
    riSI: Result := SI;
    riDI: Result := DI;
    riBP: Result := BP;
    riSP: Result := SP;
    riCS: Result := CS;
    riDS: Result := DS;
    riES: Result := ES;
    riSS: Result := SS;
    riIP: Result := IP;
  end;
end;

function TRegisters.GetByIndex16(AIndex: Byte): Word;
begin
  Result := GetByIndex16(TRegIndex16(AIndex));
end;

procedure TRegisters.SetByIndex16(AIndex: TRegIndex16; AValue: Word);
begin
  case AIndex of
    riAX: AX := AValue;
    riBX: BX := AValue;
    riCX: CX := AValue;
    riDX: DX := AValue;
    riSI: SI := AValue;
    riDI: DI := AValue;
    riBP: BP := AValue;
    riSP: SP := AValue;
    riCS: CS := AValue;
    riDS: DS := AValue;
    riES: ES := AValue;
    riSS: SS := AValue;
    riIP: IP := AValue;
  end;
end;

procedure TRegisters.SetByIndex16(AIndex: Byte; AValue: Word);
begin
  SetByIndex16(TRegIndex16(AIndex), AValue);
end;

procedure TRegisters.Log;
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;

  Builder
    .Append('AX [%.4x %6d %6d] :: ', [AX, AX, Int16(AX)])
    .Append('AH [%.2x %6d %6d] :: ', [AH, AH, Int8(AL)])
    .Append('AL [%.2x %6d %6d] ', [AL, AL, Int8(AL)])
    .Append(LineEnding);

  Builder
    .Append('BX [%.4x %6d %6d] :: ', [BX, BX, Int16(BX)])
    .Append('BH [%.2x %6d %6d] :: ', [BH, BH, Int8(BL)])
    .Append('BL [%.2x %6d %6d] ', [BL, BL, Int8(BL)])
    .Append(LineEnding);

  Builder
    .Append('CX [%.4x %6d %6d] :: ', [CX, CX, Int16(CX)])
    .Append('CH [%.2x %6d %6d] :: ', [CH, CH, Int8(CL)])
    .Append('CL [%.2x %6d %6d] ', [CL, CL, Int8(CL)])
    .Append(LineEnding);
  
  Builder
    .Append('DX [%.4x %6d %6d] :: ', [DX, DX, Int16(DX)])
    .Append('DH [%.2x %6d %6d] :: ', [DH, DH, Int8(DL)])
    .Append('DL [%.2x %6d %6d] ', [DL, DL, Int8(DL)])
    .Append(LineEnding);

  Builder
    .Append('SI [%.4x %6d %6d] ', [SI, SI, Int16(SI)]).Append(LineEnding)
    .Append('DI [%.4x %6d %6d] ', [DI, DI, Int16(DI)]).Append(LineEnding)
    .Append('BP [%.4x %6d %6d] ', [BP, BP, Int16(BP)]).Append(LineEnding)
    .Append('SP [%.4x %6d %6d] ', [SP, SP, Int16(SP)]).Append(LineEnding)
    .Append('CS [%.4x %6d %6d] ', [CS, CS, Int16(CS)]).Append(LineEnding)
    .Append('DS [%.4x %6d %6d] ', [DS, DS, Int16(DS)]).Append(LineEnding)
    .Append('ES [%.4x %6d %6d] ', [ES, ES, Int16(ES)]).Append(LineEnding)
    .Append('SS [%.4x %6d %6d] ', [SS, SS, Int16(SS)]).Append(LineEnding)
    .Append('IP [%.4x %6d %6d] ', [IP, IP, Int16(IP)]).Append(LineEnding);

  // FLAGS e X:X:X:X:(OF):(DF):(IF):(TF):(SF):(ZF):X:(AF):X:(PF):X:(CF)
  Builder
    .Append('Flags [%.4x %s]', [Flags.GetWord, BinStr(Flags.GetWord, 16)]).Append(LineEnding)
    .Append('O(%d) D(%d) I(%d) T(%d) S(%d) Z(%d) A(%d) P(%d) C(%d)',
    [
      IfThen(Flags.Get(Flags.TFlags.flagO), 1, 0),
      IfThen(Flags.Get(Flags.TFlags.flagD), 1, 0),
      IfThen(Flags.Get(Flags.TFlags.flagI), 1, 0),
      IfThen(Flags.Get(Flags.TFlags.flagT), 1, 0),
      IfThen(Flags.Get(Flags.TFlags.flagS), 1, 0),
      IfThen(Flags.Get(Flags.TFlags.flagZ), 1, 0),
      IfThen(Flags.Get(Flags.TFlags.flagA), 1, 0),
      IfThen(Flags.Get(Flags.TFlags.flagP), 1, 0),
      IfThen(Flags.Get(Flags.TFlags.flagC), 1, 0)
    ]);
  Writeln(Builder.ToString);
  FreeAndNil(Builder);
end;

class function TRegisters.RegToStr(ARegIndex: TRegIndex8): String;
begin
  case ARegIndex of
    riAL: Result := 'al';
    riCL: Result := 'cl';
    riDL: Result := 'dl';
    riBL: Result := 'bl';
    riAH: Result := 'ah';
    riCH: Result := 'ch';
    riDH: Result := 'dh';
    riBH: Result := 'bh';
  end;
end;

class function TRegisters.RegToStr(ARegIndex: TRegIndex16): String;
begin
  case ARegIndex of
    riAX: Result := 'ax';
    riBX: Result := 'bx';
    riCX: Result := 'cx';
    riDX: Result := 'dx';
    riSI: Result := 'si';
    riDI: Result := 'di';
    riBP: Result := 'bp';
    riSP: Result := 'sp';
    riCS: Result := 'cs';
    riDS: Result := 'ds';
    riES: Result := 'es';
    riSS: Result := 'ss';
    riIP: Result := 'ip';
  end;
end;

{ TCpu8088 }

procedure TCpu8088.ExecuteCurrentInstruction;
begin
  FInstructionHandlers[FCurrentInstruction.OpCode]
end;

function TCpu8088.CodeSegment: Word;
begin
  case FCurrentInstruction.SegmentOverride of
    soDS: Result := Registers.DS;
    soES: Result := Registers.ES;
    soSS: Result := Registers.SS;
  else
    Result := Registers.CS;
  end;
end;

procedure TCpu8088.SetInterruptHook(AValue: TInteruptHook);
begin
  if FInterruptHook = AValue then Exit;
  FInterruptHook := AValue;
end;

procedure TCpu8088.SetOnAfterInstruction(AValue: TInstructionNotifyEvent);
begin
  if FOnAfterInstruction = AValue then Exit;
  FOnAfterInstruction := AValue;
end;

procedure TCpu8088.SetOnBeforeInstruction(AValue: TNotifyEvent);
begin
  if FOnBeforeInstruction = AValue then Exit;
  FOnBeforeInstruction := AValue;
end;

function TCpu8088.StackSegment: Word;
begin
  case FCurrentInstruction.SegmentOverride of
    soCS: Result := Registers.CS;
    soDS: Result := Registers.DS;
    soES: Result := Registers.ES;
  else
    Result := Registers.SS;
  end; 
end;

function TCpu8088.DataSegment: Word;
begin
  case FCurrentInstruction.SegmentOverride of
    soCS: Result := Registers.CS;
    soES: Result := Registers.ES;
    soSS: Result := Registers.SS;
  else
    Result := Registers.DS;
  end;
end;

function TCpu8088.ExtraSegment: Word;
begin
  case FCurrentInstruction.SegmentOverride of
    soCS: Result := Registers.CS;
    soDS: Result := Registers.DS;
    soSS: Result := Registers.SS;
  else
    Result := Registers.ES;
  end;
end;

procedure TCpu8088.RaiseHardwareInterrupt(ANumber: Byte);
begin
  FHardwareInterrupt.Pending := True;
  FHardwareInterrupt.Number := ANumber;
end;

procedure TCpu8088.RaiseSoftwareInterrupt(ANumber: Byte);
begin
  EnterISR(ANumber);
end;

function TCpu8088.FetchCodeByte: Byte;
begin
  Result := ReadMemoryByte(Registers.CS, Registers.IP);
  Registers.IP := Registers.IP + 1;

  FCurrentInstruction.Code[FCurrentInstruction.Length] := Result;
  Inc(FCurrentInstruction.Length);
end;

function TCpu8088.FetchCodeWord: Word;
begin
  Result := FetchCodeByte;
  Result := Result or (FetchCodeByte shl 8);
end;

function TCpu8088.FetchModRM: TModRM;
var
  Data: Byte;
begin
  Data := FetchCodeByte;
  Move(Data, Result, 1);

  if Result.Mod_ <> modRegister then FillEffectiveAddress(Result);
end;

function TCpu8088.ReadRM8(AModRM: TModRM): Byte;
begin
  case AModRM.Mod_ of
    modRegister:
      Result := Registers.GetByIndex8(Registers.TRegIndex8(AModRM.Rm));
  else
    Result := ReadMemoryByte(AModRM.Segment, AModRM.EffectiveAddr);
  end;
end;

function TCpu8088.ReadRM16(AModRM: TModRM): Word;
begin
  case AModRM.Mod_ of
    modRegister:
      Result := Registers.GetByIndex16(Registers.TRegIndex16(AModRM.Rm));
  else
    Result := ReadMemoryWord(AModRM.Segment, AModRM.EffectiveAddr);
  end;
end;

procedure TCpu8088.WriteRM8(AModRM: TModRM; AValue: Byte);
begin
  case AModRM.Mod_ of
    modRegister:
      Registers.SetByIndex8(Registers.TRegIndex8(AModRM.Rm), AValue);
  else
    WriteMemoryByte(AModRM.Segment, AModRM.EffectiveAddr, AValue);
  end;
end;

procedure TCpu8088.WriteRM16(AModRM: TModRM; AValue: Word);
begin
  case AModRM.Mod_ of
    modRegister:
      Registers.SetByIndex16(Registers.TRegIndex16(AModRM.Rm), AValue);
  else
    WriteMemoryByte(AModRM.Segment, AModRM.EffectiveAddr, Lo(AValue));
    WriteMemoryByte(AModRM.Segment, AModRM.EffectiveAddr + 1, Hi(AValue));
  end;
end;

procedure TCpu8088.FillEffectiveAddress(var AModRM: TModRM);
begin
  Assert(AModRM.Mod_ <> modRegister);

  case AModRM.Rm of
    %000: AModRM.EffectiveAddr := Registers.BX + Registers.SI;
    %001: AModRM.EffectiveAddr := Registers.BX + Registers.DI;
    %010: AModRM.EffectiveAddr := Registers.BP + Registers.SI;
    %011: AModRM.EffectiveAddr := Registers.BP + Registers.DI;
    %100: AModRM.EffectiveAddr := Registers.SI;
    %101: AModRM.EffectiveAddr := Registers.DI;
    %110:
      if AModRM.Mod_ = modMemNoDisp then
        AModRM.EffectiveAddr := FetchCodeWord
      else
        AModRM.EffectiveAddr := Registers.BP;
    %111: AModRM.EffectiveAddr := Registers.BX;
  end;

  case AModRM.Mod_ of
    modMem8:
      AModRM.EffectiveAddr := AModRM.EffectiveAddr + Int8(FetchCodeByte);

    modMem16:
      AModRM.EffectiveAddr := AModRM.EffectiveAddr + Int16(FetchCodeWord);
  else;
  end;

  AModRm.Segment := IfThen(
    AModRM.Rm in [%010, %011], StackSegment, DataSegment);
end;

procedure TCpu8088.SetIOBus(AValue: IIOBus);
begin
  if FIOBus = AValue then Exit;
  FIOBus := AValue;
end;

procedure TCpu8088.WriteIOByte(AAddress: Word; AData: Byte);
begin
  IOBus.InvokeWrite(Self, AAddress, AData);
end;

function TCpu8088.ReadIOByte(AAddress: Word): Byte;
begin
  IOBus.InvokeRead(Self, AAddress, Result);
end;

function TCpu8088.OnIORead(ADevice: IIOBusDevice; AAddress: Word; out
  AData: Byte): Boolean;
begin
  { Nothing reads from the CPU }
end;

procedure TCpu8088.OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  { Nothing writes to the CPU }
end;

function TCpu8088.GetMemoryBus: IMemoryBus;
begin
  Result := FMemoryBus;
end;

procedure TCpu8088.SetMemoryBus(AValue: IMemoryBus);
begin
  if FMemoryBus = AValue then Exit;
  FMemoryBus := AValue;
end;

procedure TCpu8088.WriteMemoryByte(AAddress: TPhysicalAddress; AData: Byte);
begin
  if not Assigned(MemoryBus) then Exit;
  MemoryBus.InvokeWrite(Self, AAddress, AData);
end;

function TCpu8088.ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
begin
  if not Assigned(MemoryBus) then Exit;
  MemoryBus.InvokeRead(Self, AAddress, Result);
end;

function TCpu8088.OnMemoryRead(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; out AData: Byte): Boolean;
begin
  { Nothing reads from the CPU }
  Result := False;
end;

procedure TCpu8088.OnMemoryWrite(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; AData: Byte);
begin
  { Nothin writes to the CPU }
end;

function TCpu8088.ReadMemoryByte(ASegment, AOffset: Word): Byte;
begin
  Result := ReadMemoryByte((ASegment shl 4) + AOffset);
end;

function TCpu8088.ReadMemoryWord(ASegment, AOffset: Word): Word;
var
  Offset: Word;
begin
  Offset := AOffset;
  Result := ReadMemoryByte((ASegment shl 4) + Offset);
  Inc(Offset);
  Result := Result or (ReadMemoryByte((ASegment shl 4) + Offset) shl 8);
end;

procedure TCpu8088.WriteMemoryByte(ASegment, AOffset: Word; AData: Byte);
begin
  WriteMemoryByte((ASegment shl 4) + AOffset, AData);
end;

procedure TCpu8088.WriteMemoryWord(ASegment, AOffset: Word; AData: Word);
var
  Offset: Word;
begin
  Offset := AOffset;
  WriteMemoryByte((ASegment shl 4) + Offset, Lo(AData));
  Inc(Offset);
  WriteMemoryByte((ASegment shl 4) + Offset, Hi(AData));
end;

procedure TCpu8088.Push(AValue: Word);
begin
  Registers.SP := Registers.SP - 2;
  WriteMemoryByte(StackSegment, Word(Registers.SP + 1), Lo(AValue));
  WriteMemoryByte(StackSegment, Word(Registers.SP + 2), Hi(AValue));
end;

function TCpu8088.Pop: Word;
begin
  Result := ReadMemoryWord(StackSegment, Registers.SP + 1);
  Registers.SP := Registers.SP + 2;
end;

procedure TCpu8088.InitInstructionHandlers;
var
  I: Integer;
begin
  for I := Low(FInstructionHandlers) to High(FInstructionHandlers) do
    case I of
      $00:      FInstructionHandlers[I] := @HandleAddRM8Reg8;
      $01:      FInstructionHandlers[I] := @HandleAddRM16Reg16;
      $02:      FInstructionHandlers[I] := @HandleAddReg8RM8;
      $03:      FInstructionHandlers[I] := @HandleAddReg16RM16;
      $04:      FInstructionHandlers[I] := @HandleAddALImm8;
      $05:      FInstructionHandlers[I] := @HandleAddAXImm16;
      $06:      FInstructionHandlers[I] := @HandlePushES;
      $07:      FInstructionHandlers[I] := @HandlePopES;
      $0A:      FInstructionHandlers[I] := @HandleOrReg8RM8;
      $0B:      FInstructionHandlers[I] := @HandleOrReg16RM16;
      $0C:      FInstructionHandlers[I] := @HandleOrALImm8;
      $0D:      FInstructionHandlers[I] := @HandleOrAXImm16;
      $0E:      FInstructionHandlers[I] := @HandlePushCS;
      $10:      FInstructionHandlers[I] := @HandleAdcRM8Reg8;
      $11:      FInstructionHandlers[I] := @HandleAdcRM16Reg16;
      $14:      FInstructionHandlers[I] := @HandleAdcALImm8;
      $15:      FInstructionHandlers[I] := @HandleAdcAXImm16;
      $16:      FInstructionHandlers[I] := @HandlePushSS;
      $17:      FInstructionHandlers[I] := @HandlePopSS;
      $18:      FInstructionHandlers[I] := @HandleSbbRM8Reg8;
      $19:      FInstructionHandlers[I] := @HandleSbbRM16Reg16;
      $1C:      FInstructionHandlers[I] := @HandleSbbALImm8;
      $1D:      FInstructionHandlers[I] := @HandleSbbAXImm16;
      $1E:      FInstructionHandlers[I] := @HandlePushDS;
      $1F:      FInstructionHandlers[I] := @HandlePopDS;
      $22:      FInstructionHandlers[I] := @HandleAndReg8RM8;
      $23:      FInstructionHandlers[I] := @HandleAndReg16RM16;
      $24:      FInstructionHandlers[I] := @HandleAndALImm8;
      $25:      FInstructionHandlers[I] := @HandleAndAXImm16;
      $28:      FInstructionHandlers[I] := @HandleSubRM8Reg8;
      $29:      FInstructionHandlers[I] := @HandleSubRM16Reg16;
      $2A:      FInstructionHandlers[I] := @HandleSubReg8RM8;
      $2B:      FInstructionHandlers[I] := @HandleSubReg16RM16;
      $2C:      FInstructionHandlers[I] := @HandleSubALImm8;
      $2D:      FInstructionHandlers[I] := @HandleSubAXImm16;
      $32:      FInstructionHandlers[I] := @HandleXorReg8RM8;
      $33:      FInstructionHandlers[I] := @HandleXorReg16RM16;
      $38:      FInstructionHandlers[I] := @HandleCmpRM8Reg8;
      $39:      FInstructionHandlers[I] := @HandleCmpRM16Reg16;
      $3A:      FInstructionHandlers[I] := @HandleCmpReg8RM8;
      $3B:      FInstructionHandlers[I] := @HandleCmpReg16RM16;
      $3C:      FInstructionHandlers[I] := @HandleCmpALImm8;
      $3D:      FInstructionHandlers[I] := @HandleCmpAXImm16;
      $40..$47: FInstructionHandlers[I] := @HandleIncReg16;
      $48..$4F: FInstructionHandlers[I] := @HandleDecReg16;
      $50..$57: FInstructionHandlers[I] := @HandlePushReg16;
      $58..$5F: FInstructionHandlers[I] := @HandlePopReg16;
      $72:      FInstructionHandlers[I] := @HandleJbShort;
      $73:      FInstructionHandlers[I] := @HandleJaeShort;
      $74:      FInstructionHandlers[I] := @HandleJeShort;
      $75:      FInstructionHandlers[I] := @HandleJneShort;
      $76:      FInstructionHandlers[I] := @HandleJbeShort;
      $77:      FInstructionHandlers[I] := @HandleJaShort;
      $78:      FInstructionHandlers[I] := @HandleJsShort;
      $79:      FInstructionHandlers[I] := @HandleJnsShort;
      $7A:      FInstructionHandlers[I] := @HandleJpeShort;
      $7B:      FInstructionHandlers[I] := @HandleJpoShort;
      $7C:      FInstructionHandlers[I] := @HandleJngeShort;
      $7D:      FInstructionHandlers[I] := @HandleJnlShort;
      $7E:      FInstructionHandlers[I] := @HandleJngShort;
      $7F:      FInstructionHandlers[I] := @HandleJgShort;
      $80, $82: FInstructionHandlers[I] := @HandleGRP1RM8Imm8;
      $81:      FInstructionHandlers[I] := @HandleGRP1RM16Imm16;
      $83:      FInstructionHandlers[I] := @HandleGRP1RM16Imm8;
      $88:      FInstructionHandlers[I] := @HandleMovRM8Reg8;
      $89:      FInstructionHandlers[I] := @HandleMovRM16Reg16;
      $8A:      FInstructionHandlers[I] := @HandleMovReg8RM8;
      $8B:      FInstructionHandlers[I] := @HandleMovReg16RM16;
      $8E:      FInstructionHandlers[I] := @HandleMovSRegRM16;
      $90:      FInstructionHandlers[I] := @HandleNop;
      $91..$97: FInstructionHandlers[I] := @HandleXchgReg16;
      $98:      FInstructionHandlers[I] := @HandleCbw;
      $99:      FInstructionHandlers[I] := @HandleCwd;
      $9A:      FInstructionHandlers[I] := @HandleCallFar;
      $9C:      FInstructionHandlers[I] := @HandlePushf;
      $9D:      FInstructionHandlers[I] := @HandlePopf;
      $9E:      FInstructionHandlers[I] := @HandleSahf;
      $9F:      FInstructionHandlers[I] := @HandleLahf;
      $A0:      FInstructionHandlers[I] := @HandleMovALDisp16;
      $A1:      FInstructionHandlers[I] := @HandleMovAXDisp16;
      $A2:      FInstructionHandlers[I] := @HandleMovDisp16AL;
      $A3:      FInstructionHandlers[I] := @HandleMovDisp16AX;
      $A4:      FInstructionHandlers[I] := @HandleMovsb;
      $A5:      FInstructionHandlers[I] := @HandleMovsw;
      $A8:      FInstructionHandlers[I] := @HandleTestALImm8;
      $A9:      FInstructionHandlers[I] := @HandleTestAXImm16;
      $AA:      FInstructionHandlers[I] := @HandleStosb;
      $AB:      FInstructionHandlers[I] := @HandleStosw;
      $AC:      FInstructionHandlers[I] := @HandleLodsb;
      $AD:      FInstructionHandlers[I] := @HandleLodsw;
      $B0..$B7: FInstructionHandlers[I] := @HandleMovReg8Imm8;
      $B8..$BF: FInstructionHandlers[I] := @HandleMovReg16Imm16;
      $C2:      FInstructionHandlers[I] := @HandleRetiNear;
      $C3:      FInstructionHandlers[I] := @HandleRetNear;
      $C4:      FInstructionHandlers[I] := @HandleLes;
      $C5:      FInstructionHandlers[I] := @HandleLds;
      $C6:      FInstructionHandlers[I] := @HandleMovRM8Imm8;
      $C7:      FInstructionHandlers[I] := @HandleMovRM16Imm16;
      $CA:      FInstructionHandlers[I] := @HandleRetiFar;
      $CB:      FInstructionHandlers[I] := @HandleRetFar;
      $CC:      FInstructionHandlers[I] := @HandleInt3;
      $CD:      FInstructionHandlers[I] := @HandleIntImm8;
      $CE:      FInstructionHandlers[I] := @HandleInto;
      $CF:      FInstructionHandlers[I] := @HandleIret;
      $D0:      FInstructionHandlers[I] := @HandleGRP2RM8Const1;
      $D1:      FInstructionHandlers[I] := @HandleGRP2RM16Const1;
      $D2:      FInstructionHandlers[I] := @HandleGRP2RM8CL;
      $D3:      FInstructionHandlers[I] := @HandleGRP2RM16CL;
      $D7:      FInstructionHandlers[I] := @HandleXlat;
      $E2:      FInstructionHandlers[I] := @HandleLoop;
      $E3:      FInstructionHandlers[I] := @HandleJcxz;
      $E4:      FInstructionHandlers[I] := @HandleInALImm8;
      $E5:      FInstructionHandlers[I] := @HandleInAXImm8;
      $E6:      FInstructionHandlers[I] := @HandleOutImm8AL;
      $E7:      FInstructionHandlers[I] := @HandleOutImm8AX;
      $E8:      FInstructionHandlers[I] := @HandleCallNear;
      $E9:      FInstructionHandlers[I] := @HandleJmpNear;
      $EA:      FInstructionHandlers[I] := @HandleJmpFar;
      $EB:      FInstructionHandlers[I] := @HandleJmpShort;
      $EC:      FInstructionHandlers[I] := @HandleInALDX;
      $ED:      FInstructionHandlers[I] := @HandleInAXDX;
      $EE:      FInstructionHandlers[I] := @HandleOutDXAL;
      $EF:      FInstructionHandlers[I] := @HandleOutDXAX;
      $F1:      FInstructionHandlers[I] := @HandleInt1;
      $F4:      FInstructionHandlers[I] := @HandleHlt;
      $F5:      FInstructionHandlers[I] := @HandleCmc;
      $F6:      FInstructionHandlers[I] := @HandleGRP3A;
      $F7:      FInstructionHandlers[I] := @HandleGRP3B;
      $F8:      FInstructionHandlers[I] := @HandleClc;
      $F9:      FInstructionHandlers[I] := @HandleStc;
      $FA:      FInstructionHandlers[I] := @HandleCli;
      $FB:      FInstructionHandlers[I] := @HandleSti;
      $FC:      FInstructionHandlers[I] := @HandleCld;
      $FD:      FInstructionHandlers[I] := @HandleStd;
      $FE:      FInstructionHandlers[I] := @HandleGRP4;
      $FF:      FInstructionHandlers[I] := @HandleGRP5;
    else
      FInstructionHandlers[I] := @HandleInvalidInstruction;
    end;
end;

procedure TCpu8088.HandleInvalidInstruction;
begin
  raise Exception.CreateFmt(
    'Opcode not supported: %.2x at %.4x:%.4x', [
      FCurrentInstruction.OpCode, FCurrentInstruction.CS, FCurrentInstruction.IP
    ]);
end;

procedure TCpu8088.HandleHardwareInterrupt;
begin
  FHardwareInterrupt.Pending := False;
  EnterISR(FHardwareInterrupt.Number);
end;

procedure TCpu8088.EnterISR(ANumber: Byte);
var
  Vector: Word;
begin
  if Assigned(InterruptHook) and InterruptHook(Self, ANumber) then Exit;

  Push(Registers.Flags.GetWord, Registers.SS);

  if not FCurrentInstruction.Repeating then
  begin
    Push(Registers.CS, Registers.SS);
    Push(Registers.IP, Registers.SS);
  end else
  begin
    Push(FCurrentInstruction.CS, Registers.SS);
    Push(FCurrentInstruction.IP, Registers.SS);
  end;

  Registers.Flags.IF_ := False;
  Registers.Flags.TF := False;

  Vector := ANumber * 4;
  Registers.CS := ReadMemoryWord(0, Vector + 2);
  Registers.IP := ReadMemoryWord(0, Vector);
end;

procedure TCpu8088.HandleAddRM8Reg8;
var
  ModRM: TModRM;
  Old, Change: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  Old := ReadRM8(ModRM);
  Change := Registers.GetByIndex8(ModRM.Reg);
  Result := Old + Change;
  WriteRM8(ModRM, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(Old, Change, Result);
end;

procedure TCpu8088.HandleAddRM16Reg16;
var
  ModRM: TModRM;
  Old, Change: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  Old := ReadRM16(ModRM);
  Change := Registers.GetByIndex16(ModRM.Reg);
  Result := Old + Change;
  WriteRM16(ModRM, Word(Result));
  Registers.Flags.UpdateAfterAdd16(Old, Change, Result);
end;

procedure TCpu8088.HandleAddReg8RM8;
var
  ModRM: TModRM;
  Old, Change: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex8(ModRM.Reg);
  Change := ReadRM8(ModRM);
  Result := Old + Change;
  Registers.SetByIndex8(ModRM.Reg, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(Old, Change, Result);
end;

procedure TCpu8088.HandleAddReg16RM16;
var
  ModRM: TModRM;
  Old, Change: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex16(ModRM.Reg);
  Change := ReadRM16(ModRM);
  Result := Old + Change;
  Registers.SetByIndex16(ModRM.Reg, Word(Result));
  Registers.Flags.UpdateAfterAdd16(Old, Change, Result);
end;

procedure TCpu8088.HandleAddALImm8;
var
  Old, Change: Byte;
  Result: Int16;
begin
  Old := Registers.AL;
  Change := FetchCodeByte;
  Result := Registers.AL + Change;
  Registers.AL := Byte(Result);
  Registers.Flags.UpdateAfterAdd8(Old, Change, Result);
end;

procedure TCpu8088.HandleAddAXImm16;
var
  Old, Change: Word;
  Result: Int32;
begin
  Old := Registers.AX;
  Change := FetchCodeWord;
  Result := Registers.AX + Change;
  Registers.AX := Word(Result);
  Registers.Flags.UpdateAfterAdd16(Old, Change, Result);
end;

procedure TCpu8088.HandlePushES;
begin
  Push(Registers.ES);
end;

procedure TCpu8088.HandlePopES;
begin
  Registers.ES := Pop;
end;

procedure TCpu8088.HandleOrReg8RM8;
var
  ModRM: TModRM;
  Old, Change, Result: Byte;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex8(ModRM.Reg);
  Change := ReadRM8(ModRM);
  Result := Old or Change;
  Registers.SetByIndex8(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterOr8(Result);
end;

procedure TCpu8088.HandleOrReg16RM16;
var
  ModRM: TModRM;
  Old, Change, Result: Word;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex16(ModRM.Reg);
  Change := ReadRM16(ModRM);
  Result := Old or Change;
  Registers.SetByIndex16(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterOr16(Result);
end;

procedure TCpu8088.HandleOrALImm8;
begin
  Registers.AL := Registers.AL or FetchCodeByte;
  Registers.Flags.UpdateAfterOr8(Registers.AL);
end;

procedure TCpu8088.HandleOrAXImm16;
begin
  Registers.AX := Registers.AX or FetchCodeWord;
  Registers.Flags.UpdateAfterOr16(Registers.AX);
end;

procedure TCpu8088.HandlePushCS;
begin
  Push(Registers.CS);
end;

procedure TCpu8088.HandleAdcRM8Reg8;
var
  ModRM: TModRM;
  Old, Change, CarryIn: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  Old := ReadRM8(ModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Change := Registers.GetByIndex8(ModRM.Reg);
  Result := Old + Change + CarryIn;
  WriteRM8(ModRM, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(Old, Change + CarryIn, Result);
end;

procedure TCpu8088.HandleAdcRM16Reg16;
var
  ModRM: TModRM;
  Old, Change, CarryIn: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  Old := ReadRM16(ModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Change := Registers.GetByIndex16(ModRM.Reg);
  Result := Old + Change + CarryIn;
  WriteRM16(ModRM, Word(Result));
  Registers.Flags.UpdateAfterAdd16(Old, Change + CarryIn, Result);
end;

procedure TCpu8088.HandleAdcALImm8;
var
  Old, Change, CarryIn: Byte;
  Result: Int16;
begin
  Old := Registers.AL;
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Change := FetchCodeByte;
  Result := Old + Change + CarryIn;
  Registers.AL := Byte(Result);
  Registers.Flags.UpdateAfterAdd8(Old, Change + CarryIn, Result);
end;

procedure TCpu8088.HandleAdcAXImm16;
var
  Old, Change, CarryIn: Word;
  Result: Int32;
begin
  Old := Registers.AX;
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Change := FetchCodeWord;
  Result := Registers.AX + Change + CarryIn;
  Registers.AX := Word(Result);
  Registers.Flags.UpdateAfterAdd16(Old, Change + CarryIn, Result);
end;

procedure TCpu8088.HandlePushSS;
begin
  Push(Registers.SS);
end;

procedure TCpu8088.HandlePopSS;
begin
  Registers.SS := Pop;
end;

procedure TCpu8088.HandleSbbALImm8;
var
  Old, Change, CarryIn: Byte;
  Result: Int16;
begin
  Old := Registers.AL;
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Change := FetchCodeByte;
  Result := Old - (Change + CarryIn);
  Registers.AL := Byte(Result);
  Registers.Flags.UpdateAfterSub8(Old, Change + CarryIn, Result);
end;

procedure TCpu8088.HandleSbbAXImm16;
var
  Old, Change, CarryIn: Word;
  Result: Int32;
begin
  Old := Registers.AX;
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Change := FetchCodeWord;
  Result := Old - (Change + CarryIn);
  Registers.AX := Word(Result);
  Registers.Flags.UpdateAfterSub16(Old, Change + CarryIn, Result);
end;

procedure TCpu8088.HandlePushDS;
begin
  Push(Registers.DS);
end;

procedure TCpu8088.HandlePopDS;
begin
  Registers.DS := Pop;
end;

procedure TCpu8088.HandleAndReg8RM8;
var
  ModRM: TModRM;
  Old, Change, Result: Byte;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex8(ModRM.Reg);
  Change := ReadRM8(ModRM);
  Result := Old and Change;
  Registers.SetByIndex8(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterAnd8(Result);
end;

procedure TCpu8088.HandleAndReg16RM16;
var
  ModRM: TModRM;
  Old, Change, Result: Word;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex16(ModRM.Reg);
  Change := ReadRM16(ModRM);
  Result := Old and Change;
  Registers.SetByIndex16(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterAnd16(Result);
end;

procedure TCpu8088.HandleAndALImm8;
begin
  Registers.AL := Registers.AL and FetchCodeByte;
  Registers.Flags.UpdateAfterAnd8(Registers.AL);
end;

procedure TCpu8088.HandleAndAXImm16;
begin
  Registers.AX := Registers.AX and FetchCodeWord;
  Registers.Flags.UpdateAfterAnd16(Registers.AX);
end;

procedure TCpu8088.HandleSubALImm8;
var
  Old, Change: Byte;
  Result: Int16;
begin
  Old := Registers.AL;
  Change := FetchCodeByte;
  Result := Old - Change;
  Registers.AL := Byte(Result);
  Registers.Flags.UpdateAfterSub8(Old, Change, Result);
end;

procedure TCpu8088.HandleSubAXImm16;
var
  Old, Change: Word;
  Result: Int32;
begin
  Old := Registers.AX;
  Change := FetchCodeWord;
  Result := Old - Change;
  Registers.AX := Word(Result);
  Registers.Flags.UpdateAfterSub16(Old, Change, Result);
end;

procedure TCpu8088.HandleSubRM8Reg8;
var
  ModRM: TModRM;
  Old, Change: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  Old := ReadRM8(ModRM);
  Change := Registers.GetByIndex8(ModRM.Reg);
  Result := Old - Change;
  WriteRM8(ModRM, Byte(Result));
  Registers.Flags.UpdateAfterSub8(Old, Change, Result);
end;

procedure TCpu8088.HandleSubRM16Reg16;
var
  ModRM: TModRM;
  Old, Change: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  Old := ReadRM16(ModRM);
  Change := Registers.GetByIndex16(ModRM.Reg);
  Result := Old - Change;
  WriteRM16(ModRM, Word(Result));
  Registers.Flags.UpdateAfterSub16(Old, Change, Result);
end;

procedure TCpu8088.HandleSubReg8RM8;
var
  ModRM: TModRM;
  Old, Change: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex8(ModRM.Reg);
  Change := ReadRM8(ModRM);
  Result := Old - Change;
  Registers.SetByIndex8(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterSub8(Old, Change, Result);
end;

procedure TCpu8088.HandleSubReg16RM16;
var
  ModRM: TModRM;
  Old, Change: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex16(ModRM.Reg);
  Change := ReadRM16(ModRM);
  Result := Old - Change;
  Registers.SetByIndex16(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterSub16(Old, Change, Result);
end;

procedure TCpu8088.HandleXorReg8RM8;
var
  ModRM: TModRM;
  Old, Change, Result: Byte;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex8(ModRM.Reg);
  Change := ReadRM8(ModRM);
  Result := Old xor Change;
  Registers.SetByIndex8(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterXor8(Result);
end;

procedure TCpu8088.HandleXorReg16RM16;
var
  ModRM: TModRM;
  Old, Change, Result: Word;
begin
  ModRM := FetchModRM;
  Old := Registers.GetByIndex16(ModRM.Reg);
  Change := ReadRM16(ModRM);
  Result := Old xor Change;
  Registers.SetByIndex16(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterXor16(Result);
end;

procedure TCpu8088.HandleCmpRM8Reg8;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Cmp16(ReadRM8(ModRM), Registers.GetByIndex8(ModRM.Reg));
end;

procedure TCpu8088.HandleCmpRM16Reg16;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Cmp16(ReadRM16(ModRM), Registers.GetByIndex16(ModRM.Reg));
end;

procedure TCpu8088.HandleCmpReg8RM8;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Cmp16(Registers.GetByIndex8(ModRM.Reg), ReadRM8(ModRM));
end;

procedure TCpu8088.HandleCmpReg16RM16;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Cmp16(Registers.GetByIndex16(ModRM.Reg), ReadRM16(ModRM));
end;

procedure TCpu8088.HandleCmpALImm8;
begin
  Cmp8(Registers.AL, FetchCodeByte);
end;

procedure TCpu8088.HandleCmpAXImm16;
begin
  Cmp16(Registers.AX, FetchCodeWord);
end;

procedure TCpu8088.HandleNop;
begin
  { NOP: Nothing }
end;

procedure TCpu8088.HandleXchgReg16;
begin
  XchgReg16Reg16(
    TRegisters.TRegIndex16(FCurrentInstruction.OpCode and $07),
    riAX);
end;

procedure TCpu8088.HandleCbw;
begin
  Registers.AH := IfThen(Registers.AL and $80 <> 0, $FF, $00);
end;

procedure TCpu8088.HandleCwd;
begin
  Registers.DX := IfThen(Registers.AX and $8000 <> 0, $FFFF, $0000);
end;

procedure TCpu8088.HandleCallFar;
var
  NewCS, NewIP: Word;
begin
  NewIP := FetchCodeWord;
  NewCS := FetchCodeWord;
  Push(Registers.CS);
  Push(Registers.IP);
  Registers.CS := NewCS;
  Registers.IP := NewIP;
end;

procedure TCpu8088.HandleIncReg16;
begin
  IncReg16(TRegisters.TRegIndex16(FCurrentInstruction.OpCode and $07));
end;

procedure TCpu8088.HandleDecReg16;
begin
  DecReg16(TRegisters.TRegIndex16(FCurrentInstruction.OpCode and $07));
end;

procedure TCpu8088.HandlePushReg16;
begin
  PushReg16(TRegisters.TRegIndex16(FCurrentInstruction.OpCode and $07));
end;

procedure TCpu8088.HandlePopReg16;
begin
  PopReg16(TRegisters.TRegIndex16(FCurrentInstruction.OpCode and $07));
end;

procedure TCpu8088.HandleJbShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if Registers.Flags.CF then JumpShort(Param);
end;

procedure TCpu8088.HandleJaeShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if not Registers.Flags.CF then JumpShort(Param);
end;

procedure TCpu8088.HandleJeShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if Registers.Flags.ZF then JumpShort(Param);
end;

procedure TCpu8088.HandleJneShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if not Registers.Flags.ZF then JumpShort(Param);
end;

procedure TCpu8088.HandleJbeShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if Registers.Flags.CF or Registers.Flags.ZF then JumpShort(Param);
end;

procedure TCpu8088.HandleJaShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if not (Registers.Flags.CF or Registers.Flags.ZF) then JumpShort(Param);
end;

procedure TCpu8088.HandleJsShort;
var
  Param: Int8;
  NewIP: Word;
begin
  Param := Int8(FetchCodeByte);
  if Registers.Flags.SF then JumpShort(Param);
end;

procedure TCpu8088.HandleJnsShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if not Registers.Flags.SF then JumpShort(Param);
end;

procedure TCpu8088.HandleJpeShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if Registers.Flags.PF then JumpShort(Param);
end;

procedure TCpu8088.HandleJpoShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if not Registers.Flags.PF then JumpShort(Param);
end;

procedure TCpu8088.HandleJngeShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
   if Registers.Flags.SF <> Registers.Flags.OF_ then JumpShort(Param);
end;

procedure TCpu8088.HandleJnlShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if Registers.Flags.SF = Registers.Flags.OF_ then JumpShort(Param);
end;

procedure TCpu8088.HandleJngShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if Registers.Flags.ZF or (Registers.Flags.SF <> Registers.Flags.OF_) then
    JumpShort(Param);
end;

procedure TCpu8088.HandleJgShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if not Registers.Flags.ZF and (Registers.Flags.SF = Registers.Flags.OF_) then
    JumpShort(Param);
end;

procedure TCpu8088.HandleGRP1RM8Imm8;
var
  ModRM: TModRM;
  Imm: Byte;
begin
  ModRM := FetchModRM;
  Imm := FetchCodeByte;

  case ModRM.Reg of
    0: AddRM8Imm8(ModRM, Imm);
    4: AndRM8Imm8(ModRM, Imm);
    7: Cmp8(ReadRM8(ModRm), Imm);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleGRP1RM16Imm16;
var
  ModRM: TModRM;
  Imm: Word;
begin
  ModRM := FetchModRM;
  Imm := FetchCodeWord;

  case ModRM.Reg of
    0: AddRM16Imm16(ModRM, Imm);
    4: AndRM16Imm16(ModRM, Imm);
    7: Cmp8(ReadRM16(ModRm), Imm);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleGRP1RM16Imm8;
var
  ModRM: TModRM;
  Imm: Byte;
begin
  ModRM := FetchModRM;
  Imm := FetchCodeByte;

  case ModRM.Reg of
    0: AddRM16Imm16(ModRM, Imm);
    4: AndRM16Imm16(ModRM, Imm);
    7: Cmp8(ReadRM16(ModRm), Imm);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleSbbRM8Reg8;
var
  ModRM: TModRM;
  Old, Change, CarryIn: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  Old := ReadRM8(ModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Change := Registers.GetByIndex8(ModRM.Reg);
  Result := Old - (Change + CarryIn);
  WriteRM8(ModRM, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(Old, Change + CarryIn, Result);
end;

procedure TCpu8088.HandleSbbRM16Reg16;
var
  ModRM: TModRM;
  Old, Change, CarryIn: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  Old := ReadRM16(ModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Change := Registers.GetByIndex16(ModRM.Reg);
  Result := Old - (Change + CarryIn);
  WriteRM16(ModRM, Word(Result));
  Registers.Flags.UpdateAfterSub16(Old, Change + CarryIn, Result);
end;

procedure TCpu8088.HandleMovRM8Reg8;
var
  Param: TModRM;
  SourceIndex: TRegisters.TRegIndex8;
begin
  Param := FetchModRM;
  SourceIndex := TRegisters.TRegIndex8(Param.Reg);
  WriteRM8(Param, Registers.GetByIndex8(SourceIndex));
end;

procedure TCpu8088.HandleMovRM16Reg16;
var
  Param: TModRM;
  SourceIndex: TRegisters.TRegIndex16;
begin
  Param := FetchModRM;
  SourceIndex := TRegisters.TRegIndex16(Param.Reg);
  WriteRM16(Param, Registers.GetByIndex16(SourceIndex));
end;

procedure TCpu8088.HandleMovReg8RM8;
var
  Param: TModRM;
  DestIndex: TRegisters.TRegIndex8;
  Data: Byte;
begin
  Param := FetchModRM;
  Data := ReadRM8(Param);
  DestIndex := TRegisters.TRegIndex8(Param.Reg);
  Registers.SetByIndex8(DestIndex, Data);
end;

procedure TCpu8088.HandleMovReg16RM16;
var
  Param: TModRM;
  DestIndex: TRegisters.TRegIndex16;
  Data: Word;
begin
  Param := FetchModRM;
  Data := ReadRM16(Param);
  DestIndex := TRegisters.TRegIndex16(Param.Reg);
  Registers.SetByIndex16(DestIndex, Data);
end;

procedure TCpu8088.HandleMovSRegRM16;
var
  Param: TModRM;
  DestIndex: TRegisters.TRegIndex16;
  Data: Word;
begin
  Param := FetchModRM;
  Data := ReadRM16(Param);
  DestIndex := TRegisters.TRegIndex16(Param.Reg + 8);
  Registers.SetByIndex16(DestIndex, Data);
end;

procedure TCpu8088.HandleWait;
begin
  { Wait }
end;

procedure TCpu8088.HandlePushf;
begin
  Push(Registers.Flags.GetWord);
end;

procedure TCpu8088.HandlePopf;
begin
  Registers.Flags.SetWord(Pop);
end;

procedure TCpu8088.HandleSahf;
begin
  Registers.Flags.SetWord((Registers.Flags.GetWord and $FF00) or Registers.AH);
end;

procedure TCpu8088.HandleLahf;
begin
  Registers.AH := Byte(Registers.Flags.GetWord);
end;

procedure TCpu8088.HandleMovALDisp16;
begin
  Registers.AL := ReadMemoryByte(DataSegment, FetchCodeWord);
end;

procedure TCpu8088.HandleMovAXDisp16;
begin
  Registers.AX := ReadMemoryWord(DataSegment, FetchCodeWord);
end;

procedure TCpu8088.HandleMovDisp16AL;
begin
  WriteMemoryByte(DataSegment, FetchCodeWord, Registers.AL);
end;

procedure TCpu8088.HandleMovDisp16AX;
begin
  WriteMemoryWord(DataSegment, FetchCodeWord, Registers.AX);
end;

procedure TCpu8088.HandleMovsb;
begin
  WriteMemoryByte(
    Registers.ES, Registers.DI,
    ReadMemoryByte(DataSegment, Registers.SI));

  if Registers.Flags.DF then
  begin
    Registers.SI := Registers.SI + 1;
    Registers.DI := Registers.DI + 1;
  end else
  begin
    Registers.SI := Registers.SI - 1;
    Registers.DI := Registers.DI - 1;
  end;
end;

procedure TCpu8088.HandleMovsw;
begin
  WriteMemoryByte(
    Registers.ES, Registers.DI,
    ReadMemoryByte(DataSegment, Registers.SI));
  WriteMemoryByte(
    Registers.ES, Registers.DI + 1,
    ReadMemoryByte(DataSegment, Registers.SI + 1));

  if Registers.Flags.DF then
  begin
    Registers.SI := Registers.SI + 2;
    Registers.DI := Registers.DI + 2;
  end else
  begin
    Registers.SI := Registers.SI - 2;
    Registers.DI := Registers.DI - 2;
  end;
end;

procedure TCpu8088.HandleTestALImm8;
begin
  Registers.Flags.UpdateAfterAnd8(Registers.AL and FetchCodeByte);
end;

procedure TCpu8088.HandleTestAXImm16;
begin
  Registers.Flags.UpdateAfterAnd16(Registers.AX and FetchCodeWord);
end;

procedure TCpu8088.HandleStosb;
begin
  WriteMemoryByte(ExtraSegment, Registers.DI, Registers.AL);
  Registers.DI := Registers.DI + IfThen(Registers.Flags.DF, -1, 1);
end;

procedure TCpu8088.HandleStosw;
var
  Segment: Word;
begin
  Segment := ExtraSegment;
  WriteMemoryByte(Segment, Registers.DI, Registers.AL);
  WriteMemoryByte(Segment, Registers.DI + 1, Registers.AH);
  Registers.DI := Registers.DI + IfThen(Registers.Flags.DF, -2, 2);
end;

procedure TCpu8088.HandleLodsb;
begin
  Registers.AL := ReadMemoryByte(DataSegment, Registers.SI);
  Registers.SI := Registers.SI + IfThen(Registers.Flags.DF, -1, 1);
end;

procedure TCpu8088.HandleLodsw;
var
  Segment: Word;
begin
  Segment := DataSegment;
  Registers.AL := ReadMemoryByte(Segment, Registers.SI);
  Registers.AH := ReadMemoryByte(Segment, Registers.SI + 1);
  Registers.SI := Registers.SI + IfThen(Registers.Flags.DF, -2, 2);
end;

procedure TCpu8088.HandleMovReg8Imm8;
begin
  Registers.SetByIndex8(FCurrentInstruction.OpCode and $07, FetchCodeByte);
end;

procedure TCpu8088.HandleJmpShort;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  JumpShort(Param);
end;

procedure TCpu8088.HandleInALDX;
begin
  Registers.AL := ReadIOByte(Registers.DX);
end;

procedure TCpu8088.HandleInAXDX;
begin
  Registers.AL := ReadIOByte(Registers.DX);
  Registers.AH := ReadIOByte(Registers.DX + 1);
end;

procedure TCpu8088.HandleOutDXAL;
begin
  WriteIOByte(Registers.DX, Registers.AL);
end;

procedure TCpu8088.HandleOutDXAX;
begin
  WriteIOByte(Registers.DX, Registers.AL);
  WriteIOByte(Registers.DX + 1, Registers.AH);
end;

procedure TCpu8088.HandleInt1;
begin
  RaiseHardwareInterrupt(1);
end;

procedure TCpu8088.HandleHlt;
begin
  FHalted := True;
end;

procedure TCpu8088.HandleCmc;
begin
  Registers.Flags.CF := not Registers.Flags.CF;
end;

procedure TCpu8088.HandleGRP3A;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;

  case ModRM.Reg of
    0: TestRM8Imm8(ModRM, FetchCodeByte);
    2: DecRM8(ModRM);
    4: MulALRM8(ModRM);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleGRP3B;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;

  case ModRM.Reg of
    0: TestRM16Imm16(ModRM, FetchCodeWord);
    2: DecRM16(ModRM);
    4: MulAXRM16(ModRM);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleDiv8;
begin

end;

procedure TCpu8088.HandleDiv16;
begin

end;

procedure TCpu8088.HandleMovReg16Imm16;
begin
  Registers.SetByIndex16(FCurrentInstruction.OpCode and $07, FetchCodeWord);
end;

procedure TCpu8088.HandleRetiNear;
var
  Param: Word;
begin
  Param := FetchCodeWord;
  Registers.IP := Pop;
  Registers.SP := Registers.SP + (2 * Param);
end;

procedure TCpu8088.HandleRetNear;
begin
  Registers.IP := Pop;
end;

procedure TCpu8088.HandleLes;
var
  ModRM: TModRM;
  Addr: Word;
begin
  ModRM := FetchModRM;
  Addr := FetchCodeWord;
  Registers.SetByIndex16(ModRM.Reg, ReadMemoryWord(DataSegment, Addr));
  Registers.ES := ReadMemoryWord(DataSegment, Addr + 2);
end;

procedure TCpu8088.HandleLds;
var
  ModRM: TModRM;
  Addr: Word;
begin
  ModRM := FetchModRM;
  Addr := FetchCodeWord;
  Registers.SetByIndex16(ModRM.Reg, ReadMemoryWord(DataSegment, Addr));
  Registers.DS := ReadMemoryWord(DataSegment, Addr + 2);
end;

procedure TCpu8088.HandleMovRM8Imm8;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  WriteRM8(ModRM, FetchCodeByte);
end;

procedure TCpu8088.HandleMovRM16Imm16;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  WriteRM16(ModRM, FetchCodeWord);
end;

procedure TCpu8088.HandleRetiFar;
var
  Param: Word;
begin
  Param := FetchCodeWord;
  Registers.IP := Pop;
  Registers.CS := Pop;  
  Registers.SP := Registers.SP + (2 * Param);
end;

procedure TCpu8088.HandleRetFar;
begin
  Registers.IP := Pop;
  Registers.CS := Pop;
end;

procedure TCpu8088.HandleInt3;
begin
  RaiseSoftwareInterrupt(3);
end;

procedure TCpu8088.HandleIntImm8;
var
  InterruptNumber: Byte;
begin
  InterruptNumber := FetchCodeByte;
  RaiseSoftwareInterrupt(InterruptNumber);
end;

procedure TCpu8088.HandleInto;
begin
  if Registers.Flags.OF_ then RaiseSoftwareInterrupt(4);
end;

procedure TCpu8088.HandleIret;
begin
  Registers.IP := Pop;
  Registers.CS := Pop;
  Registers.Flags.SetWord(Pop);
end;

procedure TCpu8088.HandleGRP2RM8Const1;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  case ModRM.Reg of
    4: ShlRM8Const1(ModRM);
    5: ShrRM8Const1(ModRM);
    7: SarRM8Const1(ModRM);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleGRP2RM16Const1;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  case ModRM.Reg of
    4: ShlRM16Const1(ModRM);
    5: ShrRM16Const1(ModRm);
    7: SarRM16Const1(ModRM);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleGRP2RM8CL;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  case ModRM.Reg of
    4: ShlRM8CL(ModRM);
    5: ShrRM8CL(ModRM);
    7: SarRM8CL(ModRM);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleGRP2RM16CL;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  case ModRM.Reg of
    4: ShlRM16CL(ModRM);
    5: ShrRM16CL(ModRM);
    7: SarRM16CL(ModRM);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleXlat;
begin
  Registers.AL := ReadMemoryByte(DataSegment, Registers.BX + Registers.AL);
end;

procedure TCpu8088.HandleLoop;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  Registers.CX := Registers.CX - 1;
  if Registers.CX <> 0 then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJcxz;
var
  Param: Int8;
begin
  Param := Int8(FetchCodeByte);
  if Registers.CX = 0 then JumpShort(Param);
end;

procedure TCpu8088.HandleInALImm8;
begin
  Registers.AL := ReadIOByte(FetchCodeByte);
end;

procedure TCpu8088.HandleInAXImm8;
var
  Address: Byte;
begin
  Address := FetchCodeByte;
  Registers.AL := ReadIOByte(Address);
  Registers.AH := ReadIOByte(Address + 1);
end;

procedure TCpu8088.HandleOutImm8AL;
var
  Address: Byte;
begin
  Address := FetchCodeByte;
  WriteIOByte(Address, Registers.AL);
end;

procedure TCpu8088.HandleOutImm8AX;
var
  Address: Byte;
begin
  Address := FetchCodeByte;
  WriteIOByte(Address, Registers.AL);
  WriteIOByte(Address + 1, Registers.AH);
end;

procedure TCpu8088.HandleCallNear;
var
  Displacement: Word;
begin
  Displacement := FetchCodeWord;
  Push(Registers.IP);
  Registers.IP := Word(Registers.IP + Displacement);
end;

procedure TCpu8088.HandleJmpNear;
begin
  JumpNear(FetchCodeWord);
end;

procedure TCpu8088.HandleJmpFar;
var
  NewCS, NewIP: Word;
begin
  NewIP := FetchCodeWord;
  NewCS := FetchCodeWord;
  JumpFar(NewCS, NewIP);
end;

procedure TCpu8088.HandleClc;
begin
  Registers.Flags.CF := False;
end;

procedure TCpu8088.HandleStc;
begin
  Registers.Flags.CF := True;
end;

procedure TCpu8088.HandleCli;
begin
  Registers.Flags.IF_ := False;
end;

procedure TCpu8088.HandleSti;
begin
  Registers.Flags.IF_ := True;
end;

procedure TCpu8088.HandleCld;
begin
  Registers.Flags.DF := False;
end;

procedure TCpu8088.HandleStd;
begin
  Registers.Flags.DF := True;
end;

procedure TCpu8088.HandleGRP4;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;

  case ModRM.Reg of
    0: IncRM8(ModRM);
    1: DecRM8(ModRM);
  else
    raise Exception.CreateFmt('Invalid GRP4 extension: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleGRP5;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;

  case ModRM.Reg of
    0: IncRM16(ModRM);
    1: DecRM16(ModRM);
    4: Registers.IP := ReadMemoryWord(ModRm.Segment, ModRm.EffectiveAddr);
  else
    raise Exception.CreateFmt('Not implemented: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.PushReg16(ARegIndex: TRegisters.TRegIndex16);
begin
  Push(Registers.GetByIndex16(ARegIndex));
end;

procedure TCpu8088.PopReg16(ARegIndex: TRegisters.TRegIndex16);
begin
  Registers.SetByIndex16(ARegIndex, Pop);
end;

procedure TCpu8088.IncReg16(ARegIndex: TRegisters.TRegIndex16);
var
  Old: Word;
  Result: Int32;
begin
  Old := Registers.GetByIndex16(ARegIndex);
  Result := Old + 1;
  Registers.SetByIndex16(ARegIndex, Word(Result));
  Registers.Flags.UpdateAfterInc16(Old, Result);
end;

procedure TCpu8088.AddRM16Imm16(AModRM: TModRM; AImm: Word);
var
  Old: Word;
  Result: Int32;
begin
  Old := ReadRM16(AModRM);
  Result := Old + AImm;
  WriteRM16(AModRM, Word(Result));
  Registers.Flags.UpdateAfterAdd16(Old, AImm, Result);
end;

procedure TCpu8088.AndRM8Imm8(AModRM: TModRM; AImm: Byte);
var
  Result: Byte;
begin
  Result := ReadRM8(AModRM) and AImm;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterAnd8(Result);
end;

procedure TCpu8088.AndRM16Imm16(AModRM: TModRM; AImm: Word);
var
  Result: Word;
begin
  Result := ReadRM16(AModRM) and AImm;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterAnd16(Result);
end;

procedure TCpu8088.DecReg16(ARegIndex: TRegisters.TRegIndex16);
var
  Old: Word;
  Result: Int32;
begin
  Old := Registers.GetByIndex16(ARegIndex);
  Result := Old - 1;
  Registers.SetByIndex16(ARegIndex, Word(Result));
  Registers.Flags.UpdateAfterDec16(Old, Result);
end;

procedure TCpu8088.IncReg8(ARegIndex: TRegisters.TRegIndex8);
var
  Old: Byte;
  Result: Int16;
begin
  Old := Registers.GetByIndex8(ARegIndex);
  Result := Old + 1;
  Registers.SetByIndex8(ARegIndex, Byte(Result));
  Registers.Flags.UpdateAfterInc8(Old, Result);
end;

procedure TCpu8088.DecRM8(AModRM: TModRM);
var
  Old: Byte;
  Result: Int16;
begin
  Old := ReadRM8(AModRM);
  Result := Old - 1;
  WriteRM8(AModRM, Byte(Result));
  Registers.Flags.UpdateAfterDec8(Old, Result);
end;

procedure TCpu8088.DecRM16(AModRM: TModRM);
var
  Old: Word;
  Result: Int32;
begin
  Old := ReadRM16(AModRM);
  Result := Old - 1;
  WriteRM16(AModRM, Word(Result));
  Registers.Flags.UpdateAfterDec16(Old, Result);
end;

procedure TCpu8088.XchgReg16Reg16(
  ARegIndex1, ARegIndex2: TRegisters.TRegIndex16);
var
  Value: Word;
begin
  Value := Registers.GetByIndex16(ARegIndex1);
  Registers.SetByIndex16(ARegIndex1, Registers.GetByIndex16(ARegIndex2));
  Registers.SetByIndex16(ARegIndex2, Value);
end;

procedure TCpu8088.AddRM8Imm8(AModRM: TModRM; AImm: Byte);
var
  Old: Byte;
  Result: Int16;
begin
  Old := ReadRM8(AModRM);
  Result := Old + AImm;
  WriteRM8(AModRM, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(Old, AImm, Result);
end;

procedure TCpu8088.Cmp8(AFirst, ASecond: Byte);
begin
  Registers.Flags.UpdateAfterSub8(AFirst, ASecond, AFirst - ASecond);
end;

procedure TCpu8088.Cmp16(AFirst, ASecond: Word);
begin
  Registers.Flags.UpdateAfterSub16(AFirst, ASecond, AFirst - ASecond);
end;

procedure TCpu8088.JumpShort(ADisplacement: Int8);
begin
  Registers.IP := Registers.IP + ADisplacement;
end;

procedure TCpu8088.JumpNear(ADisplacement: Int16);
begin
  Registers.IP := Registers.IP + ADisplacement;
end;

procedure TCpu8088.JumpFar(ASegment, AOffset: Word);
begin
  Registers.CS := ASegment;
  Registers.IP := AOffset;
end;

procedure TCpu8088.NotRM8(AModRM: TModRM);
begin
  WriteRM8(AModRM, not ReadRM8(AModRM));
end;

procedure TCpu8088.NotRM16(AModRM: TModRM);
begin
  WriteRM16(AModRM, not ReadRM16(AModRM));
end;

procedure TCpu8088.ShlRM8Const1(AModRM: TModRM);
var
  Old, Result: Byte;
begin
  Old := ReadRM8(AModRm);
  Result := Old shl 1;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterShl8(Old, 1, Result);
end;

procedure TCpu8088.ShlRM16Const1(AModRM: TModRM);
var
  Old, Result: Word;
begin
  Old := ReadRM16(AModRm);
  Result := Old shl 1;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterShl16(Old, 1, Result);
end;

procedure TCpu8088.ShlRM8CL(AModRM: TModRM);
var
  Old, Result: Byte;
begin
  Old := ReadRM8(AModRm);
  if Registers.CL = 0 Then Exit;
  Result := Old shl Registers.CL;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterShl8(Old, Registers.CL, Result);
end;

procedure TCpu8088.ShlRM16CL(AModRM: TModRM);
var
  Old, Result: Word;
begin
  Old := ReadRM16(AModRm);
  if Registers.CL = 0 Then Exit;
  Result := Old shl Registers.CL;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterShl16(Old, Registers.CL, Result);
end;

procedure TCpu8088.ShrRM8Const1(AModRM: TModRM);
var
  Old, Result: Byte;
begin
  Old := ReadRM16(AModRm);
  Result := Old shr 1;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterShr8(Old, 1, Result);
end;

procedure TCpu8088.ShrRM16Const1(AModRM: TModRM);
var
  Old, Result: Word;
begin
  Old := ReadRM16(AModRm);
  Result := Old shr 1;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterShr16(Old, 1, Result);
end;

procedure TCpu8088.ShrRM8CL(AModRM: TModRM);
var
  Old, Result: Byte;
begin
  Old := ReadRM8(AModRm);
  if Registers.CL = 0 Then Exit;
  Result := Old shr Registers.CL;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterShr8(Old, Registers.CL, Result);
end;

procedure TCpu8088.ShrRM16CL(AModRM: TModRM);
var
  Old, Result: Word;
begin
  Old := ReadRM16(AModRm);
  if Registers.CL = 0 Then Exit;
  Result := Old shr Registers.CL;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterShr16(Old, Registers.CL, Result);
end;

procedure TCpu8088.SarRM8Const1(AModRM: TModRM);
var
  OrigSign: Byte;
  LastShiftedOut, Value: Byte;
begin
  Value := ReadRM8(AModRM);
  OrigSign := Value and $80;

  LastShiftedOut := Value and 1;
  Value := (Value shr 1) or OrigSign;

  WriteRM8(AModRM, Value);
  Registers.Flags.UpdateAfterSar8(Value, LastShiftedOut <> 0);
end;

procedure TCpu8088.SarRM16Const1(AModRM: TModRM);
var
  OrigSign: Word;
  LastShiftedOut, Value: Word;
begin
  Value := ReadRM16(AModRM);
  OrigSign := Value and $8000;

  LastShiftedOut := Value and 1;
  Value := (Value shr 1) or OrigSign;

  WriteRM16(AModRM, Value);
  Registers.Flags.UpdateAfterSar16(Value, LastShiftedOut <> 0);
end;

procedure TCpu8088.SarRM8CL(AModRM: TModRM);
var
  OrigSign, LastShiftedOut, Value: Byte;
  I: Integer;
begin
  Value := ReadRM8(AModRM);
  if Registers.CL = 0 then exit;

  OrigSign := Value and $80;

  for I := 1 to Registers.CL do
  begin
    LastShiftedOut := Value and 1;
    Value := (Value shr 1) or OrigSign;
  end;

  WriteRM8(AModRM, Value);
  Registers.Flags.UpdateAfterSar8(Value, LastShiftedOut <> 0);
end;

procedure TCpu8088.SarRM16CL(AModRM: TModRM);
var
  OrigSign, LastShiftedOut, Value: Word;
  I: Integer;
begin
  Value := ReadRM16(AModRM);
  if Registers.CL = 0 then exit;

  OrigSign := Value and $8000;

  for I := 1 to Registers.CL do
  begin
    LastShiftedOut := Value and 1;
    Value := (Value shr 1) or OrigSign;
  end;

  WriteRM16(AModRM, Value);
  Registers.Flags.UpdateAfterSar16(Value, LastShiftedOut <> 0);
end;

procedure TCpu8088.TestRM8Imm8(AModRM: TModRM; AImm: Byte);
begin
  Registers.Flags.UpdateAfterAnd8(ReadRM8(AModRm) and AImm);
end;

procedure TCpu8088.TestRM16Imm16(AModRM: TModRM; AImm: Word);
begin
  Registers.Flags.UpdateAfterAnd16(ReadRM16(AModRm) and AImm);
end;

procedure TCpu8088.MulALRM8(AModRM: TModRM);
begin
  Registers.AX := Registers.AL * ReadRM8(AModRM);
  Registers.Flags.UpdateAfterMul8(Registers.AX);
end;

procedure TCpu8088.MulAXRM16(AModRM: TModRM);
var
  Result: QWord;
begin
  Result := Registers.AX * ReadRM16(AModRM);
  Registers.DX := Hi(Result);
  Registers.AX := Lo(Result);
  Registers.Flags.UpdateAfterMul16(Result);
end;

procedure TCpu8088.IncRM8(AModRM: TModRM);
var
  Old: Byte;
  Result: Int16;
begin
  Old := ReadRM8(AModRM);
  Result := Old + 1;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterInc8(Old, Result);
end;

procedure TCpu8088.IncRM16(AModRM: TModRM);
var
  Old: Word;
  Result: Int32;
begin
  Old := ReadRM16(AModRM);
  Result := Old + 1;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterInc16(Old, Result);
end;

procedure TCpu8088.HandleRepetition;
begin
  if Registers.CX = 0 then
  begin
    FCurrentInstruction.Repeating := False;
    Exit;
  end;

  Registers.CX := Registers.CX - 1;

  case FCurrentInstruction.Repetition of
    repRepE: FCurrentInstruction.Repeating := Registers.Flags.ZF;
    repRepNE: FCurrentInstruction.Repeating := not Registers.Flags.ZF;
  else;
  end;
end;

procedure TCpu8088.DumpCurrentInstruction;
var
  I: Integer;
  Buf: String = '';
begin
  Write(
    Format('%.4x:%.4x | ',
    [FCurrentInstruction.CS, FCurrentInstruction.IP]));

  for I := 0 to FCurrentInstruction.Length - 1 do
    Buf := Buf + IntToHex(FCurrentInstruction.Code[I], 2);

  Write(Format('%-10s | AX:%.4x | BX:%.4x | CX:%.4x | DX:%.4x | SI:%.4x | DI:%.4x',
    [Buf, Registers.AX, Registers.BX, Registers.CX, Registers.DX, Registers.SI, Registers.DI]));

  Writeln;
end;

constructor TCpu8088.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Registers := TRegisters.Create(Self);
  InitInstructionHandlers;
  Reset;
end;

procedure TCpu8088.Reset;
begin
  Registers.AX := 0;
  Registers.BX := 0;
  Registers.CX := 0;
  Registers.DX := 0;
  Registers.SI := 0;
  Registers.DI := 0;
  Registers.BP := 0;
  Registers.SP := 0;
  Registers.DS := 0;
  Registers.ES := 0;
  Registers.SS := 0;
  Registers.IP := 0;
  Registers.CS := $FFFF;

  Registers.Flags.Reset;

  FTicks := 0;
  FHalted := False;
end;

procedure TCpu8088.Tick;
begin
  Inc(FTicks);

  if FHardwareInterrupt.Pending and Registers.Flags.IF_ then HandleHardwareInterrupt;

  if Halted then Exit;

  if Assigned(OnBeforeInstruction) then OnBeforeInstruction(Self);

  if not FCurrentInstruction.Repeating then FetchInstruction;

  if FCurrentInstruction.Repeating then
  begin
    HandleRepetition;
    if not FCurrentInstruction.Repeating then Exit;
  end;

  ExecuteCurrentInstruction;

  if Assigned(OnAfterInstruction) then
    OnAfterInstruction(Self, FCurrentInstruction);
end;

procedure TCpu8088.FetchInstruction;
var
  Data: Byte;
begin
  FillByte(FCurrentInstruction, SizeOf(FCurrentInstruction), 0);
  FCurrentInstruction.CS := Registers.CS;
  FCurrentInstruction.IP := Registers.IP;

  repeat
    Data := FetchCodeByte;
    case Data of
      $F0: FCurrentInstruction.Lock := True;
      $F2: FCurrentInstruction.Repetition := repRepE;
      $F3: FCurrentInstruction.Repetition := repRep;
      $26: FCurrentInstruction.SegmentOverride := soES;
      $2E: FCurrentInstruction.SegmentOverride := soCS;
      $36: FCurrentInstruction.SegmentOverride := soSS;
      $3E: FCurrentInstruction.SegmentOverride := soDS;
      else
        begin
          FCurrentInstruction.OpCode := Data;
          break;
        end;
    end;
  until False;
  FCurrentInstruction.Repeating := FCurrentInstruction.Repetition <> repNone;
end;

function TCpu8088.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

end.

