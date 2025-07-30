unit Cpu8088;

{$mode ObjFPC}{$H+}
{$R-}
{$Inline on}

interface

uses
  Classes, SysUtils, Math,
  Hardware;

type
  TCpu8088 = class;

  TInstruction = record
    CS, IP: Word;
    OpCode: Byte;
    Lock: Boolean;
    Repetition: (repNone, repRep, repRepE, repRepNE);
    SegmentOverride: (soNone, soCS, soDS, soES, soSS);
    Code: array[0..7] of Byte;
    Length: Byte;
    Repeating: Boolean;
  end;

  TInteruptHook = function(ASender: TObject; ANumber: Byte): Boolean of object;
  TInstructionHook = function(ASender: TObject; AAddress: TPhysicalAddress): Boolean of object;
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
    procedure UpdateAfterAdd8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16);
    procedure UpdateAfterAdd16(AAOp, ABOp, ACarryIn: Word; AResult: Int32);
    procedure UpdateAfterRol16(AOld, ACount: Word; ALastShifted: Boolean; AResult: Word);
    procedure UpdateAfterRol8(AOld, ACount: Word; ALastShifted: Boolean; AResult: Byte);
    procedure UpdateAfterSar16(AResult: Word; ALastShiftedOut: Boolean);
    procedure UpdateAfterSub16(AOld, AChange, ACarryIn: Word; AResult: Int32);
    procedure UpdateAfterSub8(AOld, AChange, ACarryIn: Byte; AResult: Int16);
    procedure UpdateAfterAnd8(AResult: Byte);
    procedure UpdateAfterAnd16(AResult: Word);
    procedure UpdateAfterOr8(AResult: Byte);
    procedure UpdateAfterOr16(AResult: Word);
    procedure UpdateAfterNeg8(AOld, AResult: Byte);
    procedure UpdateAfterNeg16(AOld, AResult: Word);
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
    procedure UpdateAfterRor8(AOld, ACount: Byte; LastShifted: Boolean; AResult: Byte);
    procedure UpdateAfterRor16(AOld, ACount: Word; LastShifted: Boolean; AResult: Word);
    procedure UpdateAfterMul8(AResult: Word);
    procedure UpdateAfterMul16(AResult: DWord);
    procedure UpdateAfterImul8(AResult: Int16);
    procedure UpdateAfterImul16(AResult: Int32);
  public
    type
      TFlags = (
        flagC = 0, flagP = 2, flagA = 4, flagZ = 6, flagS = 7,
        flagT = 8, flagI = 9, flagD = 10, flagO = 11);
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
    procedure UpdateAFAdd8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16); inline;
    procedure UpdateOFAdd8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16); inline;
    procedure UpdateCFAdd8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16); inline;
    procedure UpdateAFSub8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16); inline;
    procedure UpdateOFSub8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16); inline;
    procedure UpdateCFSub8(AAOp, ABOp, CarryIn: Byte; AResult: Int16); inline;

    procedure UpdatePF16(AValue: Word); inline;
    procedure UpdateZF16(AValue: Word); inline;
    procedure UpdateSF16(AValue: Word); inline;
    procedure UpdateAFAdd16(AAOp, ABOp, ACarryIn: Word; AResult: Int32); inline;
    procedure UpdateOFAdd16(AAOp, ABOp, ACarryIn: Word; AResult: Int32); inline;
    procedure UpdateCFAdd16(AOp, ABOp, ACarryIn: Word; AResult: Int32); inline;
    procedure UpdateAFSub16(AAOp, ABOp, ACarryIn: Word; AResult: Int32); inline;
    procedure UpdateOFSub16(AAOp, ABOp, ACaryIn: Word; AResult: Int32); inline;
    procedure UpdateCFSub16(AAOp, ABOp, ACarryIn: Word; AResult: Int32); inline;
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

  TCpu8088 = class(TComponent, ICpu, IIOBusDevice, IMemoryBusDevice)
  private
    type
      TModRM = bitpacked record
        Rm: 0..%111;
        Reg: 0..%111;
        Mod_: (modMemNoDisp, modMem8, modMem16, modRegister);
        Segment: Word;
        EffectiveAddr: Word;
      end;
    function CheckRepetition: Boolean;
    procedure DecRM16(AModRM: TModRM);
    procedure EnterISR(ANumber: Byte);
    procedure ExecuteCurrentInstruction;
    function Pop(ASegment: Word): Word;
    procedure Push(AValue: Word; ASegment: Word);
    procedure RaiseSoftwareInterrupt(ANumber: Byte);
    function ReadMemoryByte(ASegment, AOffset: Word): Byte;
    procedure WriteMemoryWord(ASegment, AOffset: Word; AData: Word);
    procedure WriteMemoryByte(ASegment, AOffset: Word; AData: Byte);
    function DFDelta(Size: Integer): Integer;
  private
    FHalted: Boolean;
    FOnAfterInstruction: TInstructionNotifyEvent;
    FOnBeforeInstruction: TInstructionHook;
    FOnBeforeExecution: TInstructionNotifyEvent;
    FOnAfterExecution: TInstructionNotifyEvent;
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
    FNmiPending: Boolean;
    FCurrentInstruction: TInstruction;
    FWaitStates: Integer;

    function CodeSegment: Word;
    function GetCurrentAddress: TPhysicalAddress;
    procedure SetInterruptHook(AValue: TInteruptHook);
    procedure SetOnAfterInstruction(AValue: TInstructionNotifyEvent);
    procedure SetOnBeforeInstruction(AValue: TInstructionHook);
    procedure SetWaitStates(AValue: Integer);
    function StackSegment: Word;
    function DataSegment: Word;
    function ExtraSegment: Word;

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
    procedure HandleOrRM8Reg8;  { $08 }
    procedure HandleOrRM16Reg16;  { $09 }
    procedure HandleOrReg8RM8;  { $0A }
    procedure HandleOrReg16RM16;  { $0A }
    procedure HandleOrALImm8;  { $0C }
    procedure HandleOrAXImm16;  { $0D }
    procedure HandlePushCS;  { $0E }
    procedure HandlePopCS;  { $0F }

    procedure HandleAdcRM8Reg8;  { $10 }
    procedure HandleAdcRM16Reg16;  { $11 }
    procedure HandleAdcReg8RM8;  { $ 12 }
    procedure HandleAdcReg16RM16;  { $ 13 }
    procedure HandleAdcALImm8;  { $14 }
    procedure HandleAdcAXImm16;  { $15 }
    procedure HandlePushSS;  { $16 }
    procedure HandlePopSS;  { $17 }
    procedure HandleSbbRM8Reg8;  { $ 18 }
    procedure HandleSbbRM16Reg16;  { $ 19 }
    procedure HandleSbbReg8RM8;  { $ 1A }
    procedure HandleSbbReg16RM16;  { $ 1B }
    procedure HandleSbbALImm8;  { $1C }
    procedure HandleSbbAXImm16;  { $1D }
    procedure HandlePushDS;  { $1E }
    procedure HandlePopDS;  { $1F }

    procedure HandleAndRM8Reg8;  { $20 }
    procedure HandleAndRM16Reg16;  { $21 }
    procedure HandleAndReg8RM8;  { $22 }
    procedure HandleAndReg16RM16;  { $23 }
    procedure HandleAndALImm8;  { $24 }
    procedure HandleAndAXImm16;  { $25 }
    procedure HandleDaa;  { $27 }
    procedure HandleSubRM8Reg8;  { $28 }
    procedure HandleSubRM16Reg16;  { $29 }
    procedure HandleSubReg8RM8;  { $2A }
    procedure HandleSubReg16RM16;  { $2B }
    procedure HandleSubALImm8;  { $2C }
    procedure HandleSubAXImm16;  { $2D }
    procedure HandleDas;  { $2F }

    procedure HandleXorRM8Reg8;  { $30 }
    procedure HandleXorRM16Reg16;  { $31 }
    procedure HandleXorReg8RM8;  { $32 }
    procedure HandleXorReg16RM16;  { $33 }
    procedure HandleXorALImm8;   { $34 }
    procedure HandleXorAXImm16;  { $35 }
    procedure HandleAaa;  { $37; }
    procedure HandleCmpRM8Reg8;  { $38 }
    procedure HandleCmpRM16Reg16;  { $39 }
    procedure HandleCmpReg8RM8;  { $3A }
    procedure HandleCmpReg16RM16;  { $3B }
    procedure HandleCmpALImm8;  { $3C }
    procedure HandleCmpAXImm16;  { $3D }
    procedure HandleAas;  { $3F }

    procedure HandleIncReg16;  { $40..47 }
    procedure HandleDecReg16;  { $48..4F }

    procedure HandlePushReg16;  { $50..$57 }
    procedure HandlePopReg16;  { $58..$5F }

    procedure HandleJoShort;  { $70 }
    procedure HandleJnoShort;  { $71 }
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
    procedure HandleTestReg8RM8;  { 84 }
    procedure HandleTestReg16RM16;  { 84 }
    procedure HandleXchgReg8RM8;  { 86 }
    procedure HandleXchgReg16RM16;  { 87 }
    procedure HandleMovRM8Reg8;  { $88 }
    procedure HandleMovRM16Reg16;  { $89 }
    procedure HandleMovReg8RM8;  { $8A }
    procedure HandleMovReg16RM16;  { $8B }
    procedure HandleMovRM16Sreg;  { $8C }
    procedure HandleLea;  { $8D }
    procedure HandleMovSRegRM16;  { $8E }
    procedure HandlePopRM16;  { $8F }

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
    procedure HandleCmpsb;  { $A6 }
    procedure HandleCmpsw;  { $A7 }
    procedure HandleTestALImm8;  { $A8 }
    procedure HandleTestAXImm16;  { $A9 }
    procedure HandleStosb;  { $AA }
    procedure HandleStosw;  { $AB }
    procedure HandleLodsb;  { $AC }
    procedure HandleLodsw;  { $AD }
    procedure HandleScasb;  { $AE }
    procedure HandleScasw;  { $AF }

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

    procedure HandleGRP2RM8Const1; { $D0 }
    procedure HandleGRP2RM16Const1; { $D1 }
    procedure HandleGRP2RM8CL; { $D2 }
    procedure HandleGRP2RM16CL; { $D3 }
    procedure HandleAam;  { $D4 }
    procedure HandleAad;  { $D5 }
    procedure HandleXlat;  { $D7 }

    procedure HandleLoope;  { $E0 }
    procedure HandleLoopne;  { $E1 }
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

    procedure HandleFpuInstruction; { $D7..$DF }

    procedure IncReg8(ARegIndex: TRegisters.TRegIndex8);

    procedure PushReg16(ARegIndex: TRegisters.TRegIndex16);
    procedure PopReg16(ARegIndex: TRegisters.TRegIndex16);
    procedure IncReg16(ARegIndex: TRegisters.TRegIndex16);
    procedure DecReg16(ARegIndex: TRegisters.TRegIndex16);
    procedure DecRM8(AModRM: TModRM);
    procedure XchgReg16Reg16(ARegIndex1, ARegIndex2: TRegisters.TRegIndex16);
    procedure AddRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure AddRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure AdcRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure AdcRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure AndRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure AndRM8Reg8(AModRM: TModRM);
    procedure AndRM16Reg16(AModRM: TModRM);
    procedure OrRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure OrRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure OrRM8Reg8(AModRM: TModRM);
    procedure XorRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure XorRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure SubRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure SubRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure SbbRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure SbbRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure AndRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure Cmp8(AFirst, ASecond: Byte);
    procedure Cmp16(AFirst, ASecond: Word);
    procedure CallFar(ASegment, AOffset: Word);
    procedure CallNearRelative(ADisplacement: Int16);
    procedure CallNearAbsolute(AOffset: Word);
    procedure JumpShort(ADisplacement: Int8);
    procedure JumpNearRelative(ADisplacement: Int16);
    procedure JumpNearAbsolute(AOffset: Word);
    procedure JumpFar(ASegment, AOffset: Word);
    procedure NotRM8(AModRM: TModRM);
    procedure NotRM16(AModRM: TModRM);
    procedure NegRM8(AModRM: TModRM);
    procedure NegRM16(AModRM: TModRM);
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
    procedure RorRM8Const1(AModRM: TModRM);
    procedure RorRM16Const1(AModRM: TModRM);
    procedure RorRM8CL(AModRM: TModRM);
    procedure RorRM16CL(AModRM: TModRM);
    procedure RolRM8Const1(AModRM: TModRM);
    procedure RolRM16Const1(AModRM: TModRM);
    procedure RolRM8CL(AModRM: TModRM);
    procedure RolRM16CL(AModRM: TModRM);
    procedure RclRM8Const1(AModRM: TModRM);
    procedure RclRM16Const1(AModRM: TModRM);
    procedure RclRM8CL(AModRM: TModRM);
    procedure RclRM16CL(AModRM: TModRM);
    procedure RcrRM8Const1(AModRM: TModRM);
    procedure RcrRM16Const1(AModRM: TModRM);
    procedure RcrRM8CL(AModRM: TModRM);
    procedure RcrRM16CL(AModRM: TModRM);
    procedure SarRM8CL(AModRM: TModRM);
    procedure SarRM16CL(AModRM: TModRM);
    procedure TestRM8Imm8(AModRM: TModRM; AImm: Byte);
    procedure TestRM16Imm16(AModRM: TModRM; AImm: Word);
    procedure MulALRM8(AModRM: TModRM);
    procedure MulAXRM16(AModRM: TModRM);
    procedure ImulALRM8(AModRM: TModRM);
    procedure ImulAXRM16(AModRM: TModRM);
    procedure DivALRM8(AModRM: TModRM);
    procedure DivAXRM16(AModRM: TModRM);
    procedure IdivALRM8(AModRM: TModRM);
    procedure IdivAXRM16(AModRM: TModRM);
    procedure IncRM8(AModRM: TModRM);
    procedure IncRM16(AModRM: TModRM);
    procedure HandleRepetition;
  public
    function DumpCurrentInstruction: String;
    property Ticks: QWord read FTicks;
    property WaitStates: Integer read FWaitStates write SetWaitStates;
    property Halted: Boolean read FHalted;
    property Registers: TRegisters read FRegisters write FRegisters;
    property OnBeforeInstruction: TInstructionHook read FOnBeforeInstruction write SetOnBeforeInstruction;
    property OnAfterInstruction: TInstructionNotifyEvent read FOnAfterInstruction write SetOnAfterInstruction;
    property OnBeforeExecution: TInstructionNotifyEvent read FOnBeforeExecution write FOnBeforeExecution;
    property OnAfterExecution: TInstructionNotifyEvent read FOnAfterExecution write FOnAfterExecution;
    property InterruptHook: TInteruptHook read FInterruptHook write SetInterruptHook;
    procedure RaiseNmi;
    function RaiseHardwareInterrupt(ANumber: Byte): Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Reset;
    procedure Tick;
    procedure FetchInstruction;
    property CurrentAddress: TPhysicalAddress read GetCurrentAddress;
    property CurrentInstruction: TInstruction read FCurrentInstruction;

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

  function GetPhysicalAddress(ASegment, AOffset: Word): TPhysicalAddress;

implementation

function GetPhysicalAddress(ASegment, AOffset: Word): TPhysicalAddress;
var
  Addr: LongInt;
begin
  Addr := ((ASegment shl 4) + AOffset);
  if Addr > High(TPhysicalAddress) then
    Addr := Addr mod (High(TPhysicalAddress) + 1);
  Result := Addr;
end;

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
  inherited Create(AOwner);
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
  {
    Possibly on the 8086/88 (and 80186/88) CPUs,
    the flags register's bits 12-15 will always be set to 1
  }

  Result := $F000;
  for I := 0 to 11 do
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
  ZF := AValue = 0;
end;

procedure TFlagRegister.UpdateSF8(AValue: Byte);
begin
  SF := (AValue and $80) <> 0;
end;

procedure TFlagRegister.UpdateAFAdd8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16);
begin
  AF := ((AAOp xor ABOp xor ACarryIn xor AResult) and $10) <> 0;
end;

procedure TFlagRegister.UpdateOFAdd8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16);
var
  SignedResult: Integer;
begin
  { Todo }
  SignedResult := Int8(AAop) + Int8(ABop) + ACarryIn;
  OF_ := not InRange(SignedResult, Int8.MinValue, Int8.MaxValue);
end;

procedure TFlagRegister.UpdateCFAdd8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16);
begin
  CF := (AResult and $100) <> 0;
end;

procedure TFlagRegister.UpdateAFSub8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16);
begin
  AF := (AAOp and $0F) < ((ABOp and $0F) + ACarryIn);
end;

procedure TFlagRegister.UpdateOFSub8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16);
begin
  OF_ := ((AAOp xor ABOp) and (AAOp xor AResult) and $80) <> 0;
end;

procedure TFlagRegister.UpdateCFSub8(AAOp, ABOp, CarryIn: Byte; AResult: Int16);
begin
  CF := AAOp < (ABOp + CarryIn);
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
  SF := (AValue and $8000) <> 0;
end;

procedure TFlagRegister.UpdateAFAdd16(AAOp, ABOp, ACarryIn: Word; AResult: Int32);
begin
  AF := (((AAOp xor ABOp xor ACarryIn xor AResult) and $10) <> 0);
end;

procedure TFlagRegister.UpdateOFAdd16(AAOp, ABOp, ACarryIn: Word; AResult: Int32);
var
  SignedResult: Integer;
begin
  { Todo }
  SignedResult := Int16(AAop) + Int16(ABop) + ACarryIn;
  OF_ := not InRange(SignedResult, Int16.MinValue, Int16.MaxValue);
end;

procedure TFlagRegister.UpdateCFAdd16(AOp, ABOp, ACarryIn: Word; AResult: Int32);
begin
  CF := (AResult and $10000) <> 0;
end;

procedure TFlagRegister.UpdateAFSub16(AAOp, ABOp, ACarryIn: Word; AResult: Int32);
begin
  AF := (AAOp and $0F) < ((ABOp and $0F) + ACarryIn);
end;

procedure TFlagRegister.UpdateOFSub16(AAOp, ABOp, ACaryIn: Word; AResult: Int32);
begin
  OF_ := ((AAOp xor ABOp) and (AAOp xor AResult) and $8000) <> 0;
end;

procedure TFlagRegister.UpdateCFSub16(AAOp, ABOp, ACarryIn: Word; AResult: Int32);
begin
  CF := AAOp < (ABOp + ACarryIn);
end;

procedure TFlagRegister.UpdateAfterAdd8(AAOp, ABOp, ACarryIn: Byte; AResult: Int16);
begin
  UpdateZF8(AResult);
  UpdatePF8(AResult);
  UpdateSF8(AResult);
  UpdateAFAdd8(AAOp, ABOp, ACarryIn, AResult);
  UpdateCFAdd8(AAOp, ABOp, ACarryIn, AResult);
  UpdateOFAdd8(AAOp, ABOp, ACarryIn, AResult);
end;

procedure TFlagRegister.UpdateAfterAdd16(AAOp, ABOp, ACarryIn: Word; AResult: Int32);
begin
  UpdateZF16(AResult);
  UpdatePF16(AResult);
  UpdateSF16(AResult);
  UpdateAFAdd16(AAOp, ABOp, ACarryIn, AResult);
  UpdateCFAdd16(AAOp, ABOp, ACarryIn, AResult);
  UpdateOFAdd16(AAOp, ABOp, ACarryIn, AResult);
end;

procedure TFlagRegister.UpdateAfterSub8(AOld, AChange, ACarryIn: Byte; AResult: Int16);
begin
  UpdateZF8(Byte(AResult));
  UpdatePF8(Byte(AResult));
  UpdateSF8(Byte(AResult));
  UpdateAFSub8(AOld, AChange, ACarryIn, AResult);
  UpdateCFSub8(AOld, AChange, ACarryIn, AResult);
  UpdateOFSub8(AOld, AChange, ACarryIn, AResult);
end;

procedure TFlagRegister.UpdateAfterAnd8(AResult: Byte);
begin
  OF_ := False;
  CF := False;
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterAnd16(AResult: Word);
begin
  OF_ := False;
  CF := False;
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterOr8(AResult: Byte);
begin
  OF_ := False;
  CF := False;
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterOr16(AResult: Word);
begin
  OF_ := False;
  CF := False;
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterNeg8(AOld, AResult: Byte);
begin
  CF := AResult <> 0;
  OF_ := AOld = $80;
  AF := (AOld and $0F) <> 0;
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);
end;

procedure TFlagRegister.UpdateAfterNeg16(AOld, AResult: Word);
begin
  CF := AResult <> 0;
  OF_ :=AOld = $8000;
  AF := (AOld and $000F) <> 0;
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);
end;

procedure TFlagRegister.UpdateAfterXor8(AResult: Byte);
begin
  OF_ := False;
  CF := False;
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterXor16(AResult: Word);
begin
  OF_ := False;
  CF := False;
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterDec8(AOld: Byte; AResult: Int16);
begin
  UpdateZF8(AResult);
  UpdatePF8(AResult);
  UpdateSF8(AResult);
  UpdateAFSub8(AOld, 1, 0, AResult);
  UpdateOFSub8(AOld, 1, 0, AResult);
end;

procedure TFlagRegister.UpdateAfterDec16(AOld: Word; AResult: Int32);
begin
  UpdateZF16(AResult);
  UpdatePF16(AResult);
  UpdateSF16(AResult);
  UpdateAFSub16(AOld, 1, 0, AResult);
  UpdateOFSub16(AOld, 1, 0, AResult);
end;

procedure TFlagRegister.UpdateAfterInc8(AOld: Byte; AResult: Int16);
begin
  UpdateZF8(Word(AResult));
  UpdatePF8(Word(AResult));
  UpdateSF8(Word(AResult));
  UpdateAFAdd8(AOld, 1, 0, AResult);
  UpdateOFAdd8(AOld, 1, 0, AResult);
end;

procedure TFlagRegister.UpdateAfterInc16(AOld: Word; AResult: Int32);
begin
  UpdateZF16(Word(AResult));
  UpdatePF16(Word(AResult));
  UpdateSF16(Word(AResult));
  UpdateAFAdd16(AOld, 1, 0, AResult);
  UpdateOFAdd16(AOld, 1, 0, AResult);
end;

procedure TFlagRegister.UpdateAfterShl8(
  AOld: Byte; ACount: Byte; AResult: Byte);
var
  CFMask: array[False..True] of Byte = (0, $80);
begin
  OF_ := False;  { debug }
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);
  if ACount < 8 then
    CF := (AOld and (1 shl (8 - ACount)) <> 0)
  else
    CF := False;

  if ACount = 1 then OF_ := ((AResult and $80) xor CFMask[CF]) <> 0;

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterShl16(
  AOld: Word; ACount: Byte; AResult: Word);
var
  CFMask: array[False..True] of Word = (0, $8000);
begin
  OF_ := False;  { debug }
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);
  if ACount < 16 then
    CF := (AOld and (1 shl (16 - ACount)) <> 0)
  else
    CF := False;

  if ACount = 1 then OF_ := ((AResult and $8000) xor CFMask[CF]) <> 0;

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterShr8(
  AOld: Byte; ACount: Byte; AResult: Byte);
begin
  OF_ := False;  { debug }
  UpdateZF8(AResult);
  UpdateSF8(AResult);
  UpdatePF8(AResult);
  CF := ((AOld shr (ACount - 1)) and 1) <> 0;

  if ACount = 1 then OF_ := (AResult and $80) <> 0;

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterShr16(
  AOld: Word; ACount: Byte; AResult: Word);
begin
  OF_ := False;  { debug }
  UpdateZF16(AResult);
  UpdateSF16(AResult);
  UpdatePF16(AResult);
  CF := ((AOld shr (ACount - 1)) and 1) <> 0;
  if ACount = 1 then OF_ := (AResult and $8000) <> 0;

  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterSar8(AResult: Byte; ALastShiftedOut: Boolean);
begin
  CF := ALastShiftedOut;
  OF_ := False;
  UpdateSF8(AResult);
  UpdateZF8(AResult);
  UpdatePF8(AResult);
  AF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterRor8(AOld, ACount: Byte;
  LastShifted: Boolean; AResult: Byte);
begin
  if ACount = 0 then Exit;
  CF := LastShifted;
  if ACount = 1 then OF_ := (AResult and $C0) in [$40, $80];
end;

procedure TFlagRegister.UpdateAfterRor16(AOld, ACount: Word;
  LastShifted: Boolean; AResult: Word);
begin
  if ACount = 0 then Exit;
  CF := LastShifted;
  if ACount = 1 then OF_ := (Hi(AResult) and $C0) in [$40, $80];
end;

procedure TFlagRegister.UpdateAfterRol8(AOld, ACount: Word;
  ALastShifted: Boolean; AResult: Byte);
begin
  if ACount = 0 then Exit;
  CF := ALastShifted;
  if ACount = 1 then OF_ := (AResult and $C0) in [$40, $80];
end;

procedure TFlagRegister.UpdateAfterRol16(AOld, ACount: Word;
  ALastShifted: Boolean; AResult: Word);
begin
  if ACount = 0 then Exit;
  CF := ALastShifted;
  if ACount = 1 then OF_ := (Hi(AResult) and $C0) in [$40, $80];
end;

procedure TFlagRegister.UpdateAfterMul8(AResult: Word);
begin
  CF := Hi(AResult) <> 0;
  OF_ := CF;

  SF := False; { for debug }
  ZF := False; { for debug }
  AF := False; { for debug }
  PF := False; { for debug }
end;

procedure TFlagRegister.UpdateAfterMul16(AResult: DWord);
begin
  CF := Hi(AResult) <> 0;
  OF_ := CF;

  SF := Int32(AResult) < 0; { for debug }
  ZF := False; { for debug }
  AF := False; { for debug }
  PF := FParityTable[AResult and $FF] ; { for debug }
end;

procedure TFlagRegister.UpdateAfterImul8(AResult: Int16);
begin
  CF := InRange(AResult, Int8.MinValue, Int8.MaxValue);
  OF_ := CF;
end;

procedure TFlagRegister.UpdateAfterImul16(AResult: Int32);
begin
  CF := InRange(AResult, Int16.MinValue, Int16.MaxValue);
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

procedure TFlagRegister.UpdateAfterSub16(AOld, AChange, ACarryIn: Word; AResult: Int32);
begin
  UpdateZF16(Word(AResult));
  UpdatePF16(Word(AResult));
  UpdateSF16(Word(AResult));
  UpdateAFSub16(AOld, AChange, ACarryIn, AResult);
  UpdateCFSub16(AOld, AChange, ACarryIn, AResult);
  UpdateOFSub16(AOld, AChange, ACarryIn, AResult);
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
  if Assigned(FOnBeforeExecution) then
    FOnBeforeExecution(Self, FCurrentInstruction);;
  {
    case FCurrentInstruction.Repeating of
      True: if CheckRepetition then
        FOnBeforeExecution(Self, FCurrentInstruction);
    else
      FOnBeforeExecution(Self, FCurrentInstruction);;
    end;
  }

  if FCurrentInstruction.Repeating and (Registers.CX = 0) then
  begin
    FCurrentInstruction.Repeating := False;
    Exit;
  end;

  FInstructionHandlers[FCurrentInstruction.OpCode];

  if FCurrentInstruction.Repeating then
  begin
    Registers.CX := Registers.CX - 1;
    if Registers.CX <> 0 then
      case FCurrentInstruction.Repetition of
        repRepE: FCurrentInstruction.Repeating := Registers.Flags.ZF;
        repRepNE: FCurrentInstruction.Repeating := not Registers.Flags.ZF;
      else;
      end
    else
      FCurrentInstruction.Repeating := False;
  end;

  if Assigned(FOnAfterExecution) then
    FOnAfterExecution(Self, FCurrentInstruction);
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

function TCpu8088.GetCurrentAddress: TPhysicalAddress;
begin
  Result := GetPhysicalAddress(Registers.CS, Registers.IP);
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

procedure TCpu8088.SetOnBeforeInstruction(AValue: TInstructionHook);
begin
  if FOnBeforeInstruction = AValue then Exit;
  FOnBeforeInstruction := AValue;
end;


procedure TCpu8088.SetWaitStates(AValue: Integer);
begin
  if FWaitStates = AValue then Exit;
  FWaitStates := Max(AValue, 0);
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

function TCpu8088.RaiseHardwareInterrupt(ANumber: Byte): Boolean;
begin
  Result := False;
  if (not Registers.Flags.IF_) or Registers.Flags.TF then Exit;

  FHardwareInterrupt.Pending := True;
  FHardwareInterrupt.Number := ANumber;
  Result := True;
end;

procedure TCpu8088.RaiseNmi;
begin
  FNmiPending := True;
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

  if FCurrentInstruction.Length > Length(FCurrentInstruction.Code) then
    raise Exception.Create('Instruction too long');
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

  AModRm.Segment := DataSegment;
  case AModRM.Rm of
    %000: AModRM.EffectiveAddr := Registers.BX + Registers.SI;
    %001: AModRM.EffectiveAddr := Registers.BX + Registers.DI;
    %010:
      begin
        AModRM.EffectiveAddr := Registers.BP + Registers.SI;
        AModRM.Segment := StackSegment;
      end;
    %011:
      begin
        AModRM.EffectiveAddr := Registers.BP + Registers.DI;
        AModRM.Segment := StackSegment;
      end;
    %100: AModRM.EffectiveAddr := Registers.SI;
    %101: AModRM.EffectiveAddr := Registers.DI;
    %110:
      if AModRM.Mod_ = modMemNoDisp then
        AModRM.EffectiveAddr := FetchCodeWord
      else
      begin
        AModRM.EffectiveAddr := Registers.BP;
        AModRM.Segment := StackSegment;
      end;
    %111: AModRM.EffectiveAddr := Registers.BX;
  end;

  case AModRM.Mod_ of
    modMem8:
      AModRM.EffectiveAddr := AModRM.EffectiveAddr + Int8(FetchCodeByte);

    modMem16:
      AModRM.EffectiveAddr := AModRM.EffectiveAddr + Int16(FetchCodeWord);
  else;
  end;
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

function TCpu8088.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
begin
  { Nothing reads from the CPU }
  Result := False;
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
  Result := ReadMemoryByte(GetPhysicalAddress(ASegment, AOffset));
end;

function TCpu8088.ReadMemoryWord(ASegment, AOffset: Word): Word;
begin
  Result := ReadMemoryByte(ASegment, AOffset);
  Result := Result or (ReadMemoryByte(ASegment, Word(AOffset + 1)) shl 8);
end;

procedure TCpu8088.WriteMemoryByte(ASegment, AOffset: Word; AData: Byte);
begin
  WriteMemoryByte(GetPhysicalAddress(ASegment, AOffset), AData);
end;

function TCpu8088.DFDelta(Size: Integer): Integer;
const
  Delta: array[1..2] of array[False..True] of Integer = ((1, -1), (2, -2));
begin
  Result := Delta[Size][Registers.Flags.DF];
end;

procedure TCpu8088.WriteMemoryWord(ASegment, AOffset: Word; AData: Word);
begin
  WriteMemoryByte(GetPhysicalAddress(ASegment, AOffset), Lo(AData));
  WriteMemoryByte(GetPhysicalAddress(ASegment, Word(AOffset + 1)), Hi(AData));
end;

procedure TCpu8088.Push(AValue: Word);
begin
  Push(AValue, Registers.SS);
end;

procedure TCpu8088.Push(AValue: Word; ASegment: Word);
begin
  Registers.SP := Registers.SP - 2;
  WriteMemoryByte(ASegment, Word(Registers.SP), Lo(AValue));
  WriteMemoryByte(ASegment, Word(Registers.SP + 1), Hi(AValue));
end;

function TCpu8088.Pop: Word;
begin
  {Result := Pop(StackSegment);}
  Result := Pop(Registers.SS);
end;

function TCpu8088.Pop(ASegment: Word): Word;
begin
  Result := ReadMemoryWord(ASegment, Registers.SP);
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
      $08:      FInstructionHandlers[I] := @HandleOrRM8Reg8;
      $09:      FInstructionHandlers[I] := @HandleOrRM16Reg16;
      $0A:      FInstructionHandlers[I] := @HandleOrReg8RM8;
      $0B:      FInstructionHandlers[I] := @HandleOrReg16RM16;
      $0C:      FInstructionHandlers[I] := @HandleOrALImm8;
      $0D:      FInstructionHandlers[I] := @HandleOrAXImm16;
      $0E:      FInstructionHandlers[I] := @HandlePushCS;
      $0F:      FInstructionHandlers[I] := @HandlePopCS;
      { $0F : PopCS / [n/a] }
      $10:      FInstructionHandlers[I] := @HandleAdcRM8Reg8;
      $11:      FInstructionHandlers[I] := @HandleAdcRM16Reg16;
      $12:      FInstructionHandlers[I] := @HandleAdcReg8RM8;
      $13:      FInstructionHandlers[I] := @HandleAdcReg16RM16;
      $14:      FInstructionHandlers[I] := @HandleAdcALImm8;
      $15:      FInstructionHandlers[I] := @HandleAdcAXImm16;
      $16:      FInstructionHandlers[I] := @HandlePushSS;
      $17:      FInstructionHandlers[I] := @HandlePopSS;
      $18:      FInstructionHandlers[I] := @HandleSbbRM8Reg8;
      $19:      FInstructionHandlers[I] := @HandleSbbRM16Reg16;
      $1A:      FInstructionHandlers[I] := @HandleSbbReg8RM8;
      $1B:      FInstructionHandlers[I] := @HandleSbbReg16RM16;
      $1C:      FInstructionHandlers[I] := @HandleSbbALImm8;
      $1D:      FInstructionHandlers[I] := @HandleSbbAXImm16;
      $1E:      FInstructionHandlers[I] := @HandlePushDS;
      $1F:      FInstructionHandlers[I] := @HandlePopDS;
      $20:      FInstructionHandlers[I] := @HandleAndRM8Reg8;
      $21:      FInstructionHandlers[I] := @HandleAndRM16Reg16;
      $22:      FInstructionHandlers[I] := @HandleAndReg8RM8;
      $23:      FInstructionHandlers[I] := @HandleAndReg16RM16;
      $24:      FInstructionHandlers[I] := @HandleAndALImm8;
      $25:      FInstructionHandlers[I] := @HandleAndAXImm16;
      { $26 ES: [prefix] }
      $27:      FInstructionHandlers[I] := @HandleDaa;
      $28:      FInstructionHandlers[I] := @HandleSubRM8Reg8;
      $29:      FInstructionHandlers[I] := @HandleSubRM16Reg16;
      $2A:      FInstructionHandlers[I] := @HandleSubReg8RM8;
      $2B:      FInstructionHandlers[I] := @HandleSubReg16RM16;
      $2C:      FInstructionHandlers[I] := @HandleSubALImm8;
      $2D:      FInstructionHandlers[I] := @HandleSubAXImm16;
      { $2E CS: [prefix] }
      $2F:      FInstructionHandlers[I] := @HandleDas;
      $30:      FInstructionHandlers[I] := @HandleXorRM8Reg8;
      $31:      FInstructionHandlers[I] := @HandleXorRM16Reg16;
      $32:      FInstructionHandlers[I] := @HandleXorReg8RM8;
      $33:      FInstructionHandlers[I] := @HandleXorReg16RM16;
      $34:      FInstructionHandlers[I] := @HandleXorALImm8;
      $35:      FInstructionHandlers[I] := @HandleXorAXImm16;
      { $36 SS: [prefix] }
      $37:      FInstructionHandlers[I] := @HandleAaa;
      $38:      FInstructionHandlers[I] := @HandleCmpRM8Reg8;
      $39:      FInstructionHandlers[I] := @HandleCmpRM16Reg16;
      $3A:      FInstructionHandlers[I] := @HandleCmpReg8RM8;
      $3B:      FInstructionHandlers[I] := @HandleCmpReg16RM16;
      $3C:      FInstructionHandlers[I] := @HandleCmpALImm8;
      $3D:      FInstructionHandlers[I] := @HandleCmpAXImm16;
      { $3E DS: [prefix] }
      $3F:      FInstructionHandlers[I] := @HandleAas;
      $40..$47: FInstructionHandlers[I] := @HandleIncReg16;
      $48..$4F: FInstructionHandlers[I] := @HandleDecReg16;
      $50..$57: FInstructionHandlers[I] := @HandlePushReg16;
      $58..$5F: FInstructionHandlers[I] := @HandlePopReg16;
      { $60..$6F [n/a] }
      $70, $60: FInstructionHandlers[I] := @HandleJoShort;
      $71, $61: FInstructionHandlers[I] := @HandleJnoShort;
      $72, $62: FInstructionHandlers[I] := @HandleJbShort;
      $73, $63: FInstructionHandlers[I] := @HandleJaeShort;
      $74, $64: FInstructionHandlers[I] := @HandleJeShort;
      $75, $65: FInstructionHandlers[I] := @HandleJneShort;
      $76, $66: FInstructionHandlers[I] := @HandleJbeShort;
      $77, $67: FInstructionHandlers[I] := @HandleJaShort;
      $78, $68: FInstructionHandlers[I] := @HandleJsShort;
      $79, $69: FInstructionHandlers[I] := @HandleJnsShort;
      $7A, $6A: FInstructionHandlers[I] := @HandleJpeShort;
      $7B, $6B: FInstructionHandlers[I] := @HandleJpoShort;
      $7C, $6C: FInstructionHandlers[I] := @HandleJngeShort;
      $7D, $6D: FInstructionHandlers[I] := @HandleJnlShort;
      $7E, $6E: FInstructionHandlers[I] := @HandleJngShort;
      $7F, $6F: FInstructionHandlers[I] := @HandleJgShort;
      $80, $82: FInstructionHandlers[I] := @HandleGRP1RM8Imm8;
      $81:      FInstructionHandlers[I] := @HandleGRP1RM16Imm16;
      $83:      FInstructionHandlers[I] := @HandleGRP1RM16Imm8;
      $84:      FInstructionHandlers[I] := @HandleTestReg8RM8;
      $85:      FInstructionHandlers[I] := @HandleTestReg16RM16;
      $86:      FInstructionHandlers[I] := @HandleXchgReg8RM8;
      $87:      FInstructionHandlers[I] := @HandleXchgReg16RM16;
      $88:      FInstructionHandlers[I] := @HandleMovRM8Reg8;
      $89:      FInstructionHandlers[I] := @HandleMovRM16Reg16;
      $8A:      FInstructionHandlers[I] := @HandleMovReg8RM8;
      $8B:      FInstructionHandlers[I] := @HandleMovReg16RM16;
      $8C:      FInstructionHandlers[I] := @HandleMovRM16Sreg;
      $8D:      FInstructionHandlers[I] := @HandleLea;
      $8E:      FInstructionHandlers[I] := @HandleMovSRegRM16;
      $8F:      FInstructionHandlers[I] := @HandlePopRM16;
      $90:      FInstructionHandlers[I] := @HandleNop;
      $91..$97: FInstructionHandlers[I] := @HandleXchgReg16;
      $98:      FInstructionHandlers[I] := @HandleCbw;
      $99:      FInstructionHandlers[I] := @HandleCwd;
      $9A:      FInstructionHandlers[I] := @HandleCallFar;
      $9B:      FInstructionHandlers[I] := @HandleWait;
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
      $A6:      FInstructionHandlers[I] := @HandleCmpsb;
      $A7:      FInstructionHandlers[I] := @HandleCmpsw;
      $A8:      FInstructionHandlers[I] := @HandleTestALImm8;
      $A9:      FInstructionHandlers[I] := @HandleTestAXImm16;
      $AA:      FInstructionHandlers[I] := @HandleStosb;
      $AB:      FInstructionHandlers[I] := @HandleStosw;
      $AC:      FInstructionHandlers[I] := @HandleLodsb;
      $AD:      FInstructionHandlers[I] := @HandleLodsw;
      $AE:      FInstructionHandlers[I] := @HandleScasb;
      $AF:      FInstructionHandlers[I] := @HandleScasw;
      $B0..$B7: FInstructionHandlers[I] := @HandleMovReg8Imm8;
      $B8..$BF: FInstructionHandlers[I] := @HandleMovReg16Imm16;
      { $C0, $C1 [n/a] }
      $C2, $C0: FInstructionHandlers[I] := @HandleRetiNear;
      $C3, $C1: FInstructionHandlers[I] := @HandleRetNear;
      $C4:      FInstructionHandlers[I] := @HandleLes;
      $C5:      FInstructionHandlers[I] := @HandleLds;
      $C6:      FInstructionHandlers[I] := @HandleMovRM8Imm8;
      $C7:      FInstructionHandlers[I] := @HandleMovRM16Imm16;
      { $C8, $C9 [n/a] }
      $CA, $C8: FInstructionHandlers[I] := @HandleRetiFar;
      $CB, $C9: FInstructionHandlers[I] := @HandleRetFar;
      $CC:      FInstructionHandlers[I] := @HandleInt3;
      $CD:      FInstructionHandlers[I] := @HandleIntImm8;
      $CE:      FInstructionHandlers[I] := @HandleInto;
      $CF:      FInstructionHandlers[I] := @HandleIret;
      $D0:      FInstructionHandlers[I] := @HandleGRP2RM8Const1;
      $D1:      FInstructionHandlers[I] := @HandleGRP2RM16Const1;
      $D2:      FInstructionHandlers[I] := @HandleGRP2RM8CL;
      $D3:      FInstructionHandlers[I] := @HandleGRP2RM16CL;
      $D4:      FInstructionHandlers[I] := @HandleAam;
      $D5:      FInstructionHandlers[I] := @HandleAad;
      { $D6 [n/a] }
      $D7:      FInstructionHandlers[I] := @HandleXlat;
      $D8..$DF: FInstructionHandlers[I] := @HandleFpuInstruction; { fpu, fetch but do nothing }
      $E0:      FInstructionHandlers[I] := @HandleLoopne;
      $E1:      FInstructionHandlers[I] := @HandleLoope;
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
      { $F0 Lock [perfix] }
      $F1:      FInstructionHandlers[I] := @HandleInt1;
      { $F2 Repne [prefix] }
      { $F3 Repe [prefix] }
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
    'Invalid opcode: %.2x at %.4x:%.4x', [
      FCurrentInstruction.OpCode, FCurrentInstruction.CS, FCurrentInstruction.IP
    ]);
end;

procedure TCpu8088.HandleHardwareInterrupt;
begin
  FHalted := False;
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
  JumpFar(ReadMemoryWord(0, Vector + 2), ReadMemoryWord(0, Vector));
  FCurrentInstruction.Repeating := False;
end;

procedure TCpu8088.HandleAddRM8Reg8;
var
  ModRM: TModRM;
  AOp, BOp: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  AOp := ReadRM8(ModRM);
  BOp := Registers.GetByIndex8(ModRM.Reg);
  Result := AOp + BOp;
  WriteRM8(ModRM, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleAddRM16Reg16;
var
  ModRM: TModRM;
  AOp, BOp: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  AOp := ReadRM16(ModRM);
  BOp := Registers.GetByIndex16(ModRM.Reg);
  Result := AOp + BOp;
  WriteRM16(ModRM, Word(Result));
  Registers.Flags.UpdateAfterAdd16(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleAddReg8RM8;
var
  ModRM: TModRM;
  AOp, BOp: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex8(ModRM.Reg);
  BOp := ReadRM8(ModRM);
  Result := AOp + BOp;
  Registers.SetByIndex8(ModRM.Reg, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleAddReg16RM16;
var
  ModRM: TModRM;
  AOp, BOp: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex16(ModRM.Reg);
  BOp := ReadRM16(ModRM);
  Result := AOp + BOp;
  Registers.SetByIndex16(ModRM.Reg, Word(Result));
  Registers.Flags.UpdateAfterAdd16(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleAddALImm8;
var
  AOp, BOp: Byte;
  Result: Int16;
begin
  AOp := Registers.AL;
  BOp := FetchCodeByte;
  Result := AOp + BOp;
  Registers.AL := Lo(Result);
  Registers.Flags.UpdateAfterAdd8(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleAddAXImm16;
var
  AOp, BOp: Word;
  Result: Int32;
begin
  AOp := Registers.AX;
  BOp := FetchCodeWord;
  Result := AOp + BOp;
  Registers.AX := Lo(Result);
  Registers.Flags.UpdateAfterAdd16(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandlePushES;
begin
  Push(Registers.ES);
end;

procedure TCpu8088.HandlePopES;
begin
  Registers.ES := Pop;
end;

procedure TCpu8088.HandleOrRM8Reg8;
var
  ModRM: TModRM;
  Result: Byte;
begin
  ModRM := FetchModRM;
  Result := ReadRM8(ModRM) or Registers.GetByIndex8(ModRM.Reg);;
  WriteRM8(ModRM, Result);
  Registers.Flags.UpdateAfterOr8(Result);
end;

procedure TCpu8088.HandleOrRM16Reg16;
var
  ModRM: TModRM;
  Result: Word;
begin
  ModRM := FetchModRM;
  Result := ReadRM16(ModRM) or Registers.GetByIndex16(ModRM.Reg);;
  WriteRM16(ModRM, Result);
  Registers.Flags.UpdateAfterOr16(Result);
end;

procedure TCpu8088.HandleOrReg8RM8;
var
  ModRM: TModRM;
  Result: Byte;
begin
  ModRM := FetchModRM;
  Result := Registers.GetByIndex8(ModRM.Reg) or ReadRM8(ModRM);
  Registers.SetByIndex8(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterOr8(Result);
end;

procedure TCpu8088.HandleOrReg16RM16;
var
  ModRM: TModRM;
  Result: Word;
begin
  ModRM := FetchModRM;
  Result := Registers.GetByIndex16(ModRM.Reg) or ReadRM16(ModRM);
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

procedure TCpu8088.HandlePopCS;
begin
  Registers.CS := Pop;
end;

procedure TCpu8088.HandleAdcRM8Reg8;
var
  ModRM: TModRM;
  AOp, BOp, CarryIn: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  AOp := ReadRM8(ModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  BOp := Registers.GetByIndex8(ModRM.Reg);
  Result := AOp;
  Inc(Result, BOp);
  Inc(Result, CarryIn);
  WriteRM8(ModRM, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleAdcRM16Reg16;
var
  ModRM: TModRM;
  AOp, BOp, CarryIn: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  AOp := ReadRM16(ModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  BOp := Registers.GetByIndex16(ModRM.Reg);
  Result := AOp;
  Inc(Result, BOp);
  Inc(Result, CarryIn);
  WriteRM16(ModRM, Word(Result));
  Registers.Flags.UpdateAfterAdd16(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleAdcReg8RM8;
var
  ModRM: TModRM;
  AOp, BOp: Byte;
  Result: Int16;
  CarryIn: Byte;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex8(ModRM.Reg);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  BOp := ReadRM8(ModRM);
  Result := AOp;
  Inc(Result, BOp);
  Inc(Result, CarryIn);
  Registers.SetByIndex8(ModRM.Reg, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleAdcReg16RM16;
var
  ModRM: TModRM;
  AOp, BOp: Word;
  Result: Int32;
  CarryIn: Byte;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex16(ModRM.Reg);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  BOp := ReadRM16(ModRM);
  Result := AOp;
  Inc(Result, BOp);
  Inc(Result, CarryIn);
  Registers.SetByIndex16(ModRM.Reg, Word(Result));
  Registers.Flags.UpdateAfterAdd16(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleAdcALImm8;
var
  AOp, BOp, CarryIn: Byte;
  Result: Int16;
begin
  AOp := Registers.AL;
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  BOp := FetchCodeByte;
  Result := AOp;
  Inc(Result, BOp);
  Inc(Result, CarryIn);
  Registers.AL := Byte(Result);
  Registers.Flags.UpdateAfterAdd8(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleAdcAXImm16;
var
  AOp, BOp, CarryIn: Word;
  Result: Int32;
begin
  AOp := Registers.AX;
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  BOp := FetchCodeWord;
  Result := Registers.AX;
  Inc(Result, BOp);
  Inc(Result, CarryIn);
  Registers.AX := Word(Result);
  Registers.Flags.UpdateAfterAdd16(AOp, BOp, CarryIn, Result);
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
  AOp, BOp, CarryIn: Byte;
  Result: Int16;
begin
  AOp := Registers.AL;
  BOp := FetchCodeByte;
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Result := AOp;
  Dec(Result, BOp);
  Dec(Result, CarryIn);
  Registers.AL := Lo(Result);
  Registers.Flags.UpdateAfterSub8(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleSbbAXImm16;
var
  AOp, BOp, CarryIn: Word;
  Result: Int32;
begin
  AOp := Registers.AX;
  BOp := FetchCodeWord;
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Result := AOp;
  Dec(Result, BOp);
  Dec(Result, CarryIn);
  Registers.AX := Lo(Result);
  Registers.Flags.UpdateAfterSub16(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandlePushDS;
begin
  Push(Registers.DS);
end;

procedure TCpu8088.HandlePopDS;
begin
  Registers.DS := Pop;
end;

procedure TCpu8088.HandleAndRM8Reg8;
var
  ModRM: TModRM;
  Result: Byte;
begin
  ModRM := FetchModRM;
  Result := ReadRM8(ModRM) and Registers.GetByIndex8(ModRM.Reg);
  WriteRM8(ModRM, Result);
  Registers.Flags.UpdateAfterAnd8(Result);
end;

procedure TCpu8088.HandleAndRM16Reg16;
var
  ModRM: TModRM;
  Result: Word;
begin
  ModRM := FetchModRM;
  Result := ReadRM16(ModRM) and Registers.GetByIndex16(ModRM.Reg);
  WriteRM16(ModRM, Result);
  Registers.Flags.UpdateAfterAnd16(Result);
end;

procedure TCpu8088.HandleAndReg8RM8;
var
  ModRM: TModRM;
  AOp, BOp, Result: Byte;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex8(ModRM.Reg);
  BOp := ReadRM8(ModRM);
  Result := AOp and BOp;
  Registers.SetByIndex8(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterAnd8(Result);
end;

procedure TCpu8088.HandleAndReg16RM16;
var
  ModRM: TModRM;
  AOp, BOp, Result: Word;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex16(ModRM.Reg);
  BOp := ReadRM16(ModRM);
  Result := AOp and BOp;
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

procedure TCpu8088.HandleDaa;
var
  Adjustment: Byte = 0;
  OriginalAL: Byte;
begin
  OriginalAL := Registers.AL;

  Registers.Flags.CF := Registers.Flags.CF
    or (Registers.AL > IfThen(Registers.Flags.AF, $9F, $99));

  Registers.Flags.AF := Registers.Flags.AF or ((Registers.AL and $0F) > 9);

  if Registers.Flags.CF then Adjustment := 6;
  if Registers.Flags.AF then Inc(Adjustment, $60);

  Registers.AL := OriginalAL + Adjustment;

  Registers.Flags.UpdateSF8(Registers.AL);
  Registers.Flags.UpdatePF8(Registers.AL);
  Registers.Flags.UpdateZF8(Registers.AL);
  Registers.Flags.UpdateOFAdd8(OriginalAL, Adjustment, 0, Registers.AL);
end;

procedure TCpu8088.HandleSubALImm8;
var
  AOp, BOp: Byte;
  Result: Int16;
begin
  AOp := Registers.AL;
  BOp := FetchCodeByte;
  Result := AOp - BOp;
  Registers.AL := Lo(Result);
  Registers.Flags.UpdateAfterSub8(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleSubAXImm16;
var
  AOp, BOp: Word;
  Result: Int32;
begin
  AOp := Registers.AX;
  BOp := FetchCodeWord;
  Result := AOp - BOp;
  Registers.AX := Lo(Result);
  Registers.Flags.UpdateAfterSub16(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleDas;
var
  OldAL: Byte;
  Adjustment: Byte = 0;
begin
  OldAL := Registers.AL;

  Registers.Flags.CF := Registers.Flags.CF or
    (Registers.AL > IfThen(Registers.Flags.AF, $9F, $99));

  Registers.Flags.AF := Registers.Flags.AF or ((Registers.AL and $0F) > 9);

  if Registers.Flags.AF then Adjustment := 6;
  if Registers.Flags.CF then Inc(Adjustment, $60);

  Registers.AL := Registers.AL - Adjustment;

  Registers.Flags.UpdatePF8(Registers.AL);
  Registers.Flags.UpdateSF8(Registers.AL);
  Registers.Flags.UpdateZF8(Registers.AL);
  Registers.Flags.UpdateOFSub8(OldAL, Adjustment, 0, Registers.AL);
end;

procedure TCpu8088.HandleSubRM8Reg8;
var
  ModRM: TModRM;
  AOp, BOp: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  AOp := ReadRM8(ModRM);
  BOp := Registers.GetByIndex8(ModRM.Reg);
  Result := AOp - BOp;
  WriteRM8(ModRM, Lo(Result));
  Registers.Flags.UpdateAfterSub8(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleSubRM16Reg16;
var
  ModRM: TModRM;
  AOp, BOp: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  AOp := ReadRM16(ModRM);
  BOp := Registers.GetByIndex16(ModRM.Reg);
  Result := AOp - BOp;
  WriteRM16(ModRM, Lo(Result));
  Registers.Flags.UpdateAfterSub16(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleSubReg8RM8;
var
  ModRM: TModRM;
  AOp, BOp: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex8(ModRM.Reg);
  BOp := ReadRM8(ModRM);
  Result := AOp - BOp;
  Registers.SetByIndex8(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterSub8(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleSubReg16RM16;
var
  ModRM: TModRM;
  AOp, BOp: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex16(ModRM.Reg);
  BOp := ReadRM16(ModRM);
  Result := AOp - BOp;
  Registers.SetByIndex16(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterSub16(AOp, BOp, 0, Result);
end;

procedure TCpu8088.HandleXorRM8Reg8;
var
  ModRM: TModRM;
  Result: Byte;
begin
  ModRM := FetchModRM;
  Result := ReadRM8(ModRM) xor Registers.GetByIndex8(ModRM.Reg);
  WriteRM8(ModRM, Result);
  Registers.Flags.UpdateAfterXor8(Result);
end;

procedure TCpu8088.HandleXorRM16Reg16;
var
  ModRM: TModRM;
  Result: Word;
begin
  ModRM := FetchModRM;
  Result := ReadRM16(ModRM) xor Registers.GetByIndex16(ModRM.Reg);
  WriteRM16(ModRM, Result);
  Registers.Flags.UpdateAfterXor16(Result);
end;

procedure TCpu8088.HandleXorReg8RM8;
var
  ModRM: TModRM;
  Result: Byte;
begin
  ModRM := FetchModRM;
  Result := Registers.GetByIndex8(ModRM.Reg) xor ReadRM8(ModRM);
  Registers.SetByIndex8(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterXor8(Result);
end;

procedure TCpu8088.HandleXorReg16RM16;
var
  ModRM: TModRM;
  Result: Word;
begin
  ModRM := FetchModRM;
  Result := Registers.GetByIndex16(ModRM.Reg) xor ReadRM16(ModRM);
  Registers.SetByIndex16(ModRM.Reg, Result);
  Registers.Flags.UpdateAfterXor16(Result);
end;

procedure TCpu8088.HandleXorALImm8;
begin
  Registers.AL := Registers.AL xor FetchCodeByte;
  Registers.Flags.UpdateAfterXor8(Registers.AL);
end;

procedure TCpu8088.HandleXorAXImm16;
begin
  Registers.AX := Registers.AX xor FetchCodeWord;
  Registers.Flags.UpdateAfterXor16(Registers.AX);
end;

procedure TCpu8088.HandleAaa;
var
  OldAL: Byte;
begin
  OldAL := Registers.AL;

  Registers.Flags.AF := Registers.Flags.AF or ((Registers.AL and $0F) > 9);
  if Registers.Flags.AF then
  begin
    Registers.AL := Registers.AL + 6;
    Registers.AH := Registers.AH + 1;
    Registers.Flags.AF := True;
    Registers.Flags.CF := True;
  end;

  Registers.Flags.CF := Registers.Flags.AF;
  Registers.Flags.UpdatePF8(Registers.AL);
  Registers.Flags.UpdateSF8(Registers.AL);
  Registers.Flags.UpdateZF8(Registers.AL);
  Registers.Flags.UpdateOFAdd8(OldAL, Registers.AL - OldAL, 0, Registers.AL);

  Registers.AL := Registers.AL and $0F;
end;

procedure TCpu8088.HandleCmpRM8Reg8;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Cmp8(ReadRM8(ModRM), Registers.GetByIndex8(ModRM.Reg));
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
  Cmp8(Registers.GetByIndex8(ModRM.Reg), ReadRM8(ModRM));
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

procedure TCpu8088.HandleAas;
var
  OldAL: Byte;
begin
  OldAL := Registers.AL;

  Registers.Flags.AF := Registers.Flags.AF or ((Registers.AL and $0F) > 9);
  if Registers.Flags.AF then
  begin
    Registers.AL := Registers.AL - 6;
    Registers.AH := Registers.AH - 1;
  end;

  Registers.Flags.CF := Registers.Flags.AF;
  Registers.Flags.UpdatePF8(Registers.AL);
  Registers.Flags.UpdateZF8(Registers.AL);
  Registers.Flags.UpdateSF8(Registers.AL);
  Registers.Flags.UpdateOFAdd8(OldAL, Registers.AL - OldAL, 0, Registers.AL);

  Registers.AL := Registers.AL and $0F;
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
  CallFar(NewCS, NewIP);
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

procedure TCpu8088.HandleJoShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if Registers.Flags.OF_ then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJnoShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if not Registers.Flags.OF_ then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJbShort;
var
  displacement: Int8;
begin
  displacement := Int8(FetchCodeByte);
  if Registers.Flags.CF then JumpShort(displacement);
end;

procedure TCpu8088.HandleJaeShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if not Registers.Flags.CF then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJeShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if Registers.Flags.ZF then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJneShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if not Registers.Flags.ZF then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJbeShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if Registers.Flags.CF or Registers.Flags.ZF then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJaShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if not (Registers.Flags.CF or Registers.Flags.ZF) then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJsShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if Registers.Flags.SF then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJnsShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if not Registers.Flags.SF then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJpeShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if Registers.Flags.PF then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJpoShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if not Registers.Flags.PF then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJngeShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
   if Registers.Flags.SF <> Registers.Flags.OF_ then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJnlShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if Registers.Flags.SF = Registers.Flags.OF_ then JumpShort(Displacement);
end;

procedure TCpu8088.HandleJngShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if Registers.Flags.ZF or (Registers.Flags.SF <> Registers.Flags.OF_) then
    JumpShort(Displacement);
end;

procedure TCpu8088.HandleJgShort;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if not Registers.Flags.ZF and (Registers.Flags.SF = Registers.Flags.OF_) then
    JumpShort(Displacement);
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
    1: OrRM8Imm8(ModRM, Imm);
    2: AdcRM8Imm8(ModRM, Imm);
    3: SbbRM8Imm8(ModRM, Imm);
    4: AndRM8Imm8(ModRM, Imm);
    5: SubRM8Imm8(ModRM, Imm);
    6: XorRM8Imm8(ModRM, Imm);
    7: Cmp8(ReadRM8(ModRM), Imm);
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
    1: OrRM16Imm16(ModRM, Imm);
    2: AdcRM16Imm16(ModRM, Imm);
    3: SbbRM16Imm16(ModRm, Imm);
    4: AndRM16Imm16(ModRM, Imm);
    5: SubRM16Imm16(ModRM, Imm);
    6: XorRM16Imm16(ModRM, Imm);
    7: Cmp16(ReadRM16(ModRM), Imm);
  end;
end;

procedure TCpu8088.HandleGRP1RM16Imm8;
var
  ModRM: TModRM;
  Imm: Int8;
begin
  ModRM := FetchModRM;
  Imm := FetchCodeByte;

  case ModRM.Reg of
    0: AddRM16Imm16(ModRM, Imm);
    1: OrRM16Imm16(ModRM, Imm);
    2: AdcRM16Imm16(ModRM, Imm);
    3: SbbRM16Imm16(ModRm, Imm);
    4: AndRM16Imm16(ModRM, Imm);
    5: SubRM16Imm16(ModRM, Imm);
    6: XorRM16Imm16(ModRM, Imm);
    7: Cmp16(ReadRM16(ModRM), Imm);
  end;
end;

procedure TCpu8088.HandleTestReg8RM8;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Registers.Flags.UpdateAfterAnd8(
    Registers.GetByIndex8(ModRM.Reg) and ReadRM8(ModRM));
end;

procedure TCpu8088.HandleTestReg16RM16;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Registers.Flags.UpdateAfterAnd16(
    Registers.GetByIndex16(ModRM.Reg) and ReadRM16(ModRM));
end;

procedure TCpu8088.HandleXchgReg8RM8;
var
  ModRM: TModRM;
  Temp: Byte;
begin
  ModRM := FetchModRM;
  Temp := Registers.GetByIndex8(ModRM.Reg);
  Registers.SetByIndex8(ModRM.Reg, ReadRM8(ModRM));
  WriteRM8(ModRM, Temp);
end;

procedure TCpu8088.HandleXchgReg16RM16;
var
  ModRM: TModRM;
  Temp: Word;
begin
  ModRM := FetchModRM;
  Temp := Registers.GetByIndex16(ModRM.Reg);
  Registers.SetByIndex16(ModRM.Reg, ReadRM16(ModRM));
  WriteRM16(ModRM, Temp);
end;

procedure TCpu8088.HandleSbbRM8Reg8;
var
  ModRM: TModRM;
  AOp, BOp, CarryIn: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  AOp := ReadRM8(ModRM);
  BOp := Registers.GetByIndex8(ModRM.Reg);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Result := AOp;
  Dec(Result, BOp);
  Dec(Result, CarryIn);
  WriteRM8(ModRM, Byte(Result));
  Registers.Flags.UpdateAfterSub8(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleSbbRM16Reg16;
var
  ModRM: TModRM;
  AOp, BOp, CarryIn: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  AOp := ReadRM16(ModRM);
  BOp := Registers.GetByIndex16(ModRM.Reg);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Result := AOp;
  Dec(Result, BOp);
  Dec(Result, CarryIn);
  WriteRM16(ModRM, Word(Result));
  Registers.Flags.UpdateAfterSub16(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleSbbReg8RM8;
var
  ModRM: TModRM;
  AOp, BOp, CarryIn: Byte;
  Result: Int16;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex8(ModRM.Reg);
  BOp := ReadRM8(ModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Result := AOp;
  Dec(Result, BOp);
  Dec(Result, CarryIn);
  Registers.SetByIndex8(ModRM.Reg, Word(Result));
  Registers.Flags.UpdateAfterSub8(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleSbbReg16RM16;
var
  ModRM: TModRM;
  AOp, BOp, CarryIn: Word;
  Result: Int32;
begin
  ModRM := FetchModRM;
  AOp := Registers.GetByIndex16(ModRM.Reg);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  BOp := ReadRM16(ModRM);
  Result := AOp;
  Dec(Result, BOp);
  Dec(Result, CarryIn);
  Registers.SetByIndex16(ModRM.Reg, Word(Result));
  Registers.Flags.UpdateAfterSub16(AOp, BOp, CarryIn, Result);
end;

procedure TCpu8088.HandleMovRM8Reg8;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  WriteRM8(ModRM, Registers.GetByIndex8(ModRM.Reg));
end;

procedure TCpu8088.HandleMovRM16Reg16;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  WriteRM16(ModRM, Registers.GetByIndex16(ModRM.Reg));
end;

procedure TCpu8088.HandleMovReg8RM8;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Registers.SetByIndex8(ModRM.Reg, ReadRM8(ModRM));
end;

procedure TCpu8088.HandleMovReg16RM16;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Registers.SetByIndex16(ModRM.Reg, ReadRM16(ModRM));
end;

procedure TCpu8088.HandleMovRM16Sreg;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  WriteRM16(ModRM, Registers.GetByIndex16(Ord(riES) + ModRM.Reg));
end;

procedure TCpu8088.HandleLea;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Registers.SetByIndex16(ModRM.Reg, ModRM.EffectiveAddr);
end;

procedure TCpu8088.HandleMovSRegRM16;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Registers.SetByIndex16(Ord(riES) + ModRM.Reg, ReadRM16(ModRM));
end;

procedure TCpu8088.HandlePopRM16;
begin
  WriteRM16(FetchModRM, Pop);
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
  Registers.AH := Lo(Registers.Flags.GetWord);
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
  Registers.SI := Registers.SI + DFDelta(SizeOf(Byte));
  Registers.DI := Registers.DI + DFDelta(SizeOf(Byte));
end;

procedure TCpu8088.HandleMovsw;
begin
  WriteMemoryByte(
    Registers.ES, Registers.DI,
    ReadMemoryByte(DataSegment, Registers.SI));
  WriteMemoryByte(
    Registers.ES, Registers.DI + 1,
    ReadMemoryByte(DataSegment, Registers.SI + 1));
  Registers.SI := Registers.SI + DFDelta(SizeOf(Word));
  Registers.DI := Registers.DI + DFDelta(SizeOf(Word));
end;

procedure TCpu8088.HandleCmpsb;
begin
  Cmp8(
    ReadMemoryByte(DataSegment, Registers.SI),
    ReadMemoryByte(Registers.ES, Registers.DI));
  Registers.SI := Registers.SI + DFDelta(SizeOf(Byte));
  Registers.DI := Registers.DI + DFDelta(SizeOf(Byte));
end;

procedure TCpu8088.HandleCmpsw;
begin
  Cmp16(
    ReadMemoryWord(DataSegment, Registers.SI),
    ReadMemoryWord(Registers.ES, Registers.DI));
  Registers.SI := Registers.SI + DFDelta(SizeOf(Word));
  Registers.DI := Registers.DI + DFDelta(SizeOf(Word));
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
  Registers.DI := Registers.DI + DFDelta(SizeOf(Registers.AL));
end;

procedure TCpu8088.HandleStosw;
begin
  WriteMemoryWord(ExtraSegment, Registers.DI, Registers.AX);
  Registers.DI := Registers.DI + DFDelta(SizeOf(Registers.AX));
end;

procedure TCpu8088.HandleLodsb;
begin
  Registers.AL := ReadMemoryByte(DataSegment, Registers.SI);
  Registers.SI := Registers.SI + DFDelta(SizeOf(Registers.AL));
end;

procedure TCpu8088.HandleLodsw;
begin
  Registers.AX := ReadMemoryWord(DataSegment, Registers.SI);
  Registers.SI := Registers.SI + DFDelta(SizeOf(Registers.AX));
end;

procedure TCpu8088.HandleScasb;
var
  Result: Int16;
  Change: Byte;
begin
  Change := ReadMemoryByte(Registers.ES, Registers.DI);
  Result := Registers.AL - Change;
  Registers.DI := Registers.DI + DFDelta(SizeOf(Registers.AL));
  Registers.Flags.UpdateAfterSub8(Registers.AL, Change, 0, Result);
end;

procedure TCpu8088.HandleScasw;
var
  Result: Int32;
  Change: Word;
begin
  Change := ReadMemoryWord(Registers.ES, Registers.DI);
  Result := Registers.AX - Change;
  Registers.DI := Registers.DI + DFDelta(SizeOf(Registers.AX));
  Registers.Flags.UpdateAfterSub16(Registers.AX, Change, 0, Result);
end;

procedure TCpu8088.HandleMovReg8Imm8;
begin
  Registers.SetByIndex8(FCurrentInstruction.OpCode and $07, FetchCodeByte);
end;

procedure TCpu8088.HandleJmpShort;
begin
  JumpShort(FetchCodeByte);
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
    1: raise Exception.CreateFmt('Invalid GRP3 extension: %d', [ModRM.Reg]);
    2: NotRM8(ModRM);
    3: NegRM8(ModRM);
    4: MulALRM8(ModRM);
    5: ImulALRM8(ModRM);
    6: DivALRM8(ModRM);
    7: IdivALRM8(ModRM);
  end;
end;

procedure TCpu8088.HandleGRP3B;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  case ModRM.Reg of
    0: TestRM16Imm16(ModRM, FetchCodeWord);
    1: Exception.CreateFmt('Invalid GRP3 extension: %d', [ModRM.Reg]);
    2: NotRM16(ModRM);
    3: NegRM16(ModRM);
    4: MulAXRM16(ModRM);
    5: ImulAXRM16(ModRM);
    6: DivAXRM16(ModRM);
    7: IdivAXRM16(ModRM);
  end;
end;

procedure TCpu8088.HandleMovReg16Imm16;
begin
  Registers.SetByIndex16(FCurrentInstruction.OpCode and $07, FetchCodeWord);
end;

procedure TCpu8088.HandleRetiNear;
var
  Displacement: Word;
begin
  Displacement := FetchCodeWord;
  Registers.IP := Pop;
  Registers.SP := Registers.SP + Displacement;
end;

procedure TCpu8088.HandleRetNear;
begin
  Registers.IP := Pop;
end;

procedure TCpu8088.HandleLes;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Registers.SetByIndex16(ModRM.Reg,
    ReadMemoryWord(ModRM.Segment, ModRm.EffectiveAddr));
  Registers.ES := ReadMemoryWord(ModRM.Segment, ModRM.EffectiveAddr + 2);
end;

procedure TCpu8088.HandleLds;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  Registers.SetByIndex16(
    ModRM.Reg, ReadMemoryWord(ModRM.Segment, ModRM.EffectiveAddr));
  Registers.DS := ReadMemoryWord(ModRM.Segment, ModRM.EffectiveAddr + 2);
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
  Displacement: Word;
begin
  Displacement := FetchCodeWord;
  Registers.IP := Pop;
  Registers.CS := Pop;
  Registers.SP := Registers.SP + Displacement;
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
    0: RolRM8Const1(ModRM);
    1: RorRM8Const1(ModRM);
    2: RclRM8Const1(ModRM);
    3: RcrRM8Const1(ModRM);
    4: ShlRM8Const1(ModRM);
    5: ShrRM8Const1(ModRM);
    6: raise Exception.CreateFmt('Invalid GRP2 extension: %d', [ModRM.Reg]);
    7: SarRM8Const1(ModRM);
  end;
end;

procedure TCpu8088.HandleGRP2RM16Const1;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  case ModRM.Reg of
    0: RolRM16Const1(ModRM);
    1: RorRM16Const1(ModRM);
    2: RclRM16Const1(ModRM);
    3: RcrRM16Const1(ModRM);
    4: ShlRM16Const1(ModRM);
    5: ShrRM16Const1(ModRM);
    6: raise Exception.CreateFmt('Invalid GRP2 extension: %d', [ModRM.Reg]);
    7: SarRM16Const1(ModRM);
  end;
end;

procedure TCpu8088.HandleGRP2RM8CL;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  case ModRM.Reg of
    0: RolRM8CL(ModRM);
    1: RorRM8CL(ModRM);
    2: RclRM8CL(ModRM);
    3: RcrRM8CL(ModRM);
    4: ShlRM8CL(ModRM);
    5: ShrRM8CL(ModRM);
    6: raise Exception.CreateFmt('Invalid GRP2 extension: %d', [ModRM.Reg]);
    7: SarRM8CL(ModRM);
  end;
end;

procedure TCpu8088.HandleGRP2RM16CL;
var
  ModRM: TModRM;
begin
  ModRM := FetchModRM;
  case ModRM.Reg of
    0: RolRM16CL(ModRM);
    1: RorRM16CL(ModRM);
    2: RclRM16CL(ModRM);
    3: RcrRM16CL(ModRM);
    4: ShlRM16CL(ModRM);
    5: ShrRM16CL(ModRM);
    6: raise Exception.CreateFmt('Invalid GRP2 extension: %d', [ModRM.Reg]);
    7: SarRM16CL(ModRM);
  end;
end;

procedure TCpu8088.HandleAam;
var
  Result: Word = 0;
  Remainder: Word = 0;
  Base: Byte;
begin
  Base := FetchCodeByte;
  if Base = 0 then
  begin
    RaiseHardwareInterrupt(0);
    Exit;
  end;

  DivMod(Registers.AL, Base, Result, Remainder);
  Registers.AH := Result;
  Registers.AL := Remainder;
  Registers.Flags.UpdateSF8(Remainder);
  Registers.Flags.UpdatePF8(Remainder);
  Registers.Flags.UpdateZF8(Remainder);
end;

procedure TCpu8088.HandleAad;
var
  Result: Byte;
begin
  Result := ((Registers.AL * 10) + Registers.AX) and $FF;
  Registers.AL := Result;
  Registers.AH := 0;
  Registers.Flags.UpdateSF8(Result);
  Registers.Flags.UpdatePF8(Result);
  Registers.Flags.UpdateZF8(Result);
end;

procedure TCpu8088.HandleXlat;
begin
  Registers.AL := ReadMemoryByte(DataSegment, Registers.BX + Registers.AL);
end;

procedure TCpu8088.HandleLoope;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  Registers.CX := Registers.CX - 1;
  if (Registers.CX <> 0) and Registers.Flags.ZF then JumpShort(Displacement);
end;

procedure TCpu8088.HandleLoopne;
var
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  Registers.CX := Registers.CX - 1;
  if (Registers.CX <> 0) and
    not Registers.Flags.ZF then JumpShort(Displacement);
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
  Displacement: Int8;
begin
  Displacement := Int8(FetchCodeByte);
  if Registers.CX = 0 then JumpShort(Displacement);
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
begin
  CallNearRelative(FetchCodeWord);
end;

procedure TCpu8088.HandleJmpNear;
begin
  JumpNearRelative(FetchCodeWord);
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
    2: CallNearAbsolute(ReadRM16(ModRM));
    3: CallFar(
         ReadMemoryWord(ModRM.Segment, ModRM.EffectiveAddr + 2),
         ReadMemoryWord(ModRM.Segment, ModRM.EffectiveAddr));
    4: JumpNearAbsolute(ReadRM16(ModRM));
    5: JumpFar(
         ReadMemoryWord(ModRM.Segment, ModRM.EffectiveAddr + 2),
         ReadMemoryWord(ModRM.Segment, ModRM.EffectiveAddr));
    6: Push(ReadRM16(ModRM));
    7: Exception.CreateFmt('Invalid GRP5 extension: %d', [ModRM.Reg]);
  end;
end;

procedure TCpu8088.HandleFpuInstruction;
begin
  FetchModRM;
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
  AOp: Word;
  Result: Int32;
begin
  AOp := Registers.GetByIndex16(ARegIndex);
  Result := AOp + 1;
  Registers.SetByIndex16(ARegIndex, Word(Result));
  Registers.Flags.UpdateAfterInc16(AOp, Result);
end;

procedure TCpu8088.AddRM16Imm16(AModRM: TModRM; AImm: Word);
var
  AOp: Word;
  Result: Int32;
begin
  AOp := ReadRM16(AModRM);
  Result := AOp + AImm; { Todo: check if negative? }
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterAdd16(AOp, AImm, 0, Result);
end;

procedure TCpu8088.AdcRM8Imm8(AModRM: TModRM; AImm: Byte);
var
  AOp, CarryIn: Byte;
  Result: Int16;
begin
  AOp := ReadRM8(AModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Result := AOp;
  Inc(Result, AImm);
  Inc(Result, CarryIn);
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterAdd8(AOp, AImm, CarryIn, Result);
end;

procedure TCpu8088.AdcRM16Imm16(AModRM: TModRM; AImm: Word);
var
  AOp, CarryIn: Word;
  Result: Int32;
begin
  AOp := ReadRM16(AModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Result := AOp;
  Inc(Result, AImm);
  Inc(Result, CarryIn);
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterAdd16(AOp, AImm, CarryIn, Result);
end;

procedure TCpu8088.AndRM8Imm8(AModRM: TModRM; AImm: Byte);
var
  Result: Byte;
begin
  Result := ReadRM8(AModRM) and AImm;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterAnd8(Result);
end;

procedure TCpu8088.AndRM8Reg8(AModRM: TModRM);
var
  Result: Byte;
begin
  Result := ReadRM8(AModRM) and Registers.GetByIndex8(AModRM.Reg);
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterAnd8(Result);
end;

procedure TCpu8088.AndRM16Reg16(AModRM: TModRM);
var
  Result: Word;
begin
  Result := ReadRM16(AModRM) and Registers.GetByIndex16(AModRM.Reg);
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterAnd16(Result);
end;

procedure TCpu8088.OrRM8Imm8(AModRM: TModRM; AImm: Byte);
var
  Result: Byte;
begin
  Result := ReadRM8(AModRM) or AImm;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterOr8(Result);
end;

procedure TCpu8088.OrRM16Imm16(AModRM: TModRM; AImm: Word);
var
  Result: Word;
begin
  Result := ReadRM16(AModRM) or AImm;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterOr16(Result);
end;

procedure TCpu8088.OrRM8Reg8(AModRM: TModRM);
var
  Result: Byte;
begin
  Result := ReadRM8(AModRM) or Registers.GetByIndex8(AModRM.Reg);
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterOr8(Result);
end;

procedure TCpu8088.XorRM8Imm8(AModRM: TModRM; AImm: Byte);
var
  Result : Byte;
begin
  Result := ReadRM8(AModRM) xor AImm;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterXor8(Result);
end;

procedure TCpu8088.XorRM16Imm16(AModRM: TModRM; AImm: Word);
var
  Result : Word;
begin
  Result := ReadRM16(AModRM) xor AImm;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterXor16(Result);
end;

procedure TCpu8088.SubRM16Imm16(AModRM: TModRM; AImm: Word);
var
  AOp: Word;
  Result: Int32;
begin
  AOp := ReadRM16(AModRM);
  Result := AOp;
  Dec(Result, AImm);
  WriteRM16(AModRM, Word(Result));
  Registers.Flags.UpdateAfterSub16(AOp, AImm, 0, Result);
end;

procedure TCpu8088.SbbRM8Imm8(AModRM: TModRM; AImm: Byte);
var
  AOp, CarryIn: Byte;
  Result: Int16;
begin
  AOp := ReadRM8(AModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Result := AOp;
  Dec(Result, AImm);
  Dec(Result, CarryIn);
  WriteRM8(AModRM, Byte(Result));
  Registers.Flags.UpdateAfterSub8(AOp, AImm, CarryIn, Result);
end;

procedure TCpu8088.SbbRM16Imm16(AModRM: TModRM; AImm: Word);
var
  AOp, CarryIn: Word;
  Result: Int32;
begin
  AOp := ReadRM16(AModRM);
  CarryIn := IfThen(Registers.Flags.CF, 1, 0);
  Result := AOp;
  Dec(Result, AImm);
  Dec(Result, CarryIn);
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterSub16(AOp, AImm, CarryIn, Result);
end;

procedure TCpu8088.SubRM8Imm8(AModRM: TModRM; AImm: Byte);
var
  AOp: Byte;
  Result: Int16;
begin
  AOp := ReadRM8(AModRM);
  Result := AOp;
  Dec(Result, AImm);
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterSub8(AOp, AImm, 0, Result);
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
  AOp: Word;
  Result: Int32;
begin
  AOp := Registers.GetByIndex16(ARegIndex);
  Result := AOp - 1;
  Registers.SetByIndex16(ARegIndex, Word(Result));
  Registers.Flags.UpdateAfterDec16(AOp, Result);
end;

procedure TCpu8088.IncReg8(ARegIndex: TRegisters.TRegIndex8);
var
  AOp: Byte;
  Result: Int16;
begin
  AOp := Registers.GetByIndex8(ARegIndex);
  Result := AOp + 1;
  Registers.SetByIndex8(ARegIndex, Byte(Result));
  Registers.Flags.UpdateAfterInc8(AOp, Result);
end;

procedure TCpu8088.DecRM8(AModRM: TModRM);
var
  AOp: Byte;
  Result: Int16;
begin
  AOp := ReadRM8(AModRM);
  Result := AOp - 1;
  WriteRM8(AModRM, Lo(Result));
  Registers.Flags.UpdateAfterDec8(AOp, Result);
end;

procedure TCpu8088.DecRM16(AModRM: TModRM);
var
  AOp: Word;
  Result: Int32;
begin
  AOp := ReadRM16(AModRM);
  Result := AOp - 1;
  WriteRM16(AModRM, Lo(Result));
  Registers.Flags.UpdateAfterDec16(AOp, Result);
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
  AOp: Byte;
  Result: Int16;
begin
  AOp := ReadRM8(AModRM);
  Result := AOp + AImm;
  WriteRM8(AModRM, Byte(Result));
  Registers.Flags.UpdateAfterAdd8(AOp, AImm, 0, Result);
end;

procedure TCpu8088.Cmp8(AFirst, ASecond: Byte);
begin
  Registers.Flags.UpdateAfterSub8(AFirst, ASecond, 0, AFirst - ASecond);
end;

procedure TCpu8088.Cmp16(AFirst, ASecond: Word);
begin
  Registers.Flags.UpdateAfterSub16(AFirst, ASecond, 0, AFirst - ASecond);
end;

procedure TCpu8088.CallFar(ASegment, AOffset: Word);
begin
  Push(Registers.CS);
  Push(Registers.IP);
  Registers.CS := ASegment;
  Registers.IP := AOffset;
end;

procedure TCpu8088.CallNearRelative(ADisplacement: Int16);
begin
  CallNearAbsolute(Registers.IP + ADisplacement);
end;

procedure TCpu8088.CallNearAbsolute(AOffset: Word);
begin
  Push(Registers.IP);
  Registers.IP := AOffset;
end;

procedure TCpu8088.JumpShort(ADisplacement: Int8);
begin
  Registers.IP := Registers.IP + ADisplacement;
end;

procedure TCpu8088.JumpNearRelative(ADisplacement: Int16);
begin
  Registers.IP := Registers.IP + ADisplacement;
end;

procedure TCpu8088.JumpNearAbsolute(AOffset: Word);
begin
  Registers.IP := AOffset;
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

procedure TCpu8088.NegRM8(AModRM: TModRM);
var
  AOp: Byte;
  Result: Int8;
begin
  AOp := ReadRM8(AModRM);
  Result := Int8(-AOp);
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterNeg8(AOp, Result);
end;

procedure TCpu8088.NegRM16(AModRM: TModRM);
var
  AOp: Word;
  Result: Int16;
begin
  AOp := ReadRM16(AModRM);
  Result := Int16(-AOp);
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterNeg16(AOp, Result);
end;

procedure TCpu8088.ShlRM8Const1(AModRM: TModRM);
var
  AOp, Result: Byte;
begin
  AOp := ReadRM8(AModRm);
  Result := AOp shl 1;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterShl8(AOp, 1, Result);
end;

procedure TCpu8088.ShlRM16Const1(AModRM: TModRM);
var
  AOp, Result: Word;
begin
  AOp := ReadRM16(AModRM);
  Result := AOp shl 1;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterShl16(AOp, 1, Result);
end;

procedure TCpu8088.ShlRM8CL(AModRM: TModRM);
var
  AOp, Result: Byte;
begin
  AOp := ReadRM8(AModRM);
  if Registers.CL = 0 then Exit;
  Result := AOp shl Registers.CL;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterShl8(AOp, Registers.CL, Result);
end;

procedure TCpu8088.ShlRM16CL(AModRM: TModRM);
var
  AOp, Result: Word;
begin
  AOp := ReadRM16(AModRM);
  if Registers.CL = 0 then Exit;
  Result := AOp shl Registers.CL;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterShl16(AOp, Registers.CL, Result);
end;

procedure TCpu8088.ShrRM8Const1(AModRM: TModRM);
var
  AOp, Result: Byte;
begin
  AOp := ReadRM8(AModRM);
  Result := AOp shr 1;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterShr8(AOp, 1, Result);
end;

procedure TCpu8088.ShrRM16Const1(AModRM: TModRM);
var
  AOp, Result: Word;
begin
  AOp := ReadRM16(AModRM);
  Result := AOp shr 1;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterShr16(AOp, 1, Result);
end;

procedure TCpu8088.ShrRM8CL(AModRM: TModRM);
var
  AOp, Result: Byte;
begin
  AOp := ReadRM8(AModRM);
  if Registers.CL = 0 then Exit;
  Result := AOp shr Registers.CL;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterShr8(AOp, Registers.CL, Result);
end;

procedure TCpu8088.ShrRM16CL(AModRM: TModRM);
var
  AOp, Result: Word;
begin
  AOp := ReadRM16(AModRM);
  if Registers.CL = 0 Then Exit;
  Result := AOp shr Registers.CL;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterShr16(AOp, Registers.CL, Result);
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

procedure TCpu8088.RorRM8Const1(AModRM: TModRM);
var
  AOp, Result: Byte;
  Shifted: Boolean;
begin
  AOp := ReadRM8(AModRM);

  Shifted := AOp.Bits[0];
  Result := AOp shr 1;
  Result.Bits[7] := Shifted;

  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterRor8(AOp, 1, Shifted, Result);
end;

procedure TCpu8088.RorRM16Const1(AModRM: TModRM);
var
  AOp, Result: Word;
  Shifted: Boolean;
begin
  AOp := ReadRM16(AModRM);

  Shifted := AOp.Bits[0];
  Result := AOp shr 1;
  Result.Bits[15] := Shifted;

  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterRor16(AOp, 1, Shifted, Result);
end;

procedure TCpu8088.RorRM8CL(AModRM: TModRM);
var
  AOp, Result: Byte;
  I: Integer;
  LastShifted: Boolean;
begin
  AOp := ReadRM8(AModRM);
  Result := AOp;
  for I := 1 to Registers.CL do
  begin
    LastShifted := Result.Bits[0];
    Result := Result shr 1;
    Result.Bits[7] := LastShifted;
  end;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterRor8(AOp, Registers.CL, LastShifted, Result);
end;

procedure TCpu8088.RorRM16CL(AModRM: TModRM);
var
  AOp, Result: Word;
  I: Integer;
  LastShifted: Boolean;
begin
  AOp := ReadRM16(AModRM);
  Result := AOp;
  for I := 1 to Registers.CL do
  begin
    LastShifted := Result.Bits[0];
    Result := Result shr 1;
    Result.Bits[15] := LastShifted;
  end;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterRor16(AOp, Registers.CL, LastShifted, Result);
end;

procedure TCpu8088.RolRM8Const1(AModRM: TModRM);
var
  AOp, Result: Byte;
  Shifted: Boolean;
begin
  AOp := ReadRM8(AModRM);

  Shifted := AOp.Bits[7];
  Result := AOp shl 1;
  Result.Bits[0] := Shifted;

  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterRol8(AOp, Registers.CL, Shifted, Result);
end;

procedure TCpu8088.RolRM16Const1(AModRM: TModRM);
var
  AOp, Result: Word;
  Shifted: Boolean;
begin
  AOp := ReadRM16(AModRM);

  Shifted := AOp.Bits[15];
  Result := AOp shl 1;
  Result.Bits[0] := Shifted;

  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterRol16(AOp, 1, Shifted, Result);
end;

procedure TCpu8088.RolRM8CL(AModRM: TModRM);
var
  AOp, Result: Byte;
  I: Integer;
  LastShifted: Boolean;
begin
  AOp := ReadRM8(AModRM);
  Result := AOp;
  for I := 1 to Registers.CL do
  begin
    LastShifted := Result.Bits[7];
    Result := Result shl 1;
    Result.Bits[0] := LastShifted;
  end;

  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterRol8(AOp, Registers.CL, LastShifted, Result);
end;

procedure TCpu8088.RolRM16CL(AModRM: TModRM);
var
  AOp, Result: Word;
  I: Integer;
  LastShifted: Boolean;
begin
  AOp := ReadRM16(AModRM);
  Result := AOp;
  for I := 1 to Registers.CL do
  begin
    LastShifted := Result.Bits[15];
    Result := Result shl 1;
    Result.Bits[0] := LastShifted;
  end;

  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterRol16(AOp, Registers.CL, LastShifted, Result);
end;

procedure TCpu8088.RclRM8Const1(AModRM: TModRM);
var
  AOp, OldCF, Result: Byte;
const
  CFMask: array[False..True] of Byte = (0, $80);
begin
  AOp := ReadRM8(AModRM);
  OldCF := IfThen(Registers.Flags.CF, 1, 0);
  Result := (AOp shl 1) or OldCF;
  WriteRM8(AModRM, Result);

  Registers.Flags.CF := (AOp and $80) <> 0;
  Registers.Flags.OF_ := ((Result xor CFMask[Registers.Flags.CF]) and $80) <> 0;
end;

procedure TCpu8088.RclRM16Const1(AModRM: TModRM);
var
  OldCF: Byte;
  AOp, Result: Word;
const
  CFMask: array[False..True] of Word = (0, $8000);
begin
  AOp := ReadRM16(AModRM);
  OldCF := IfThen(Registers.Flags.CF, 1, 0);
  Result := (AOp shl 1) or OldCF;
  WriteRM16(AModRM, Result);

  Registers.Flags.CF := (AOp and $8000) <> 0;
  Registers.Flags.OF_ :=
    ((Result xor CFMask[Registers.Flags.CF]) and $8000) <> 0;
end;

procedure TCpu8088.RclRM8CL(AModRM: TModRM);
var
  AOp, OldCF, Result: Byte;
  I: Integer;
const
  CFMask: array[False..True] of Byte = (0, $80);
begin
  AOp := ReadRM8(AModRM);
  Result := AOp;
  for I := 1 to Registers.CL do
  begin
    OldCF := IfThen(Registers.Flags.CF, 1, 0);
    Registers.Flags.CF := (Result and $80) <> 0;
    Result := (Result shl 1) or OldCF;
  end;
  WriteRM8(AModRM, Result);

  if Registers.CL = 1 then
    Registers.Flags.OF_ :=
      ((Result xor CFMask[Registers.Flags.CF]) and $80) <> 0;
end;

procedure TCpu8088.RclRM16CL(AModRM: TModRM);
var
  AOp, OldCF, Result: Word;
  I: Integer;
const
  CFMask: array[False..True] of Word = (0, $8000);
begin
  AOp := ReadRM16(AModRM);
  Result := AOp;
  for I := 1 to Registers.CL do
  begin
    OldCF := IfThen(Registers.Flags.CF, 1, 0);
    Registers.Flags.CF := (Result and $8000) <> 0;
    Result := (Result shl 1) or OldCF;
  end;
  WriteRM16(AModRM, Result);

  if Registers.CL = 1 then
    Registers.Flags.OF_ :=
      ((Result xor CFMask[Registers.Flags.CF]) and $8000) <> 0;
end;

procedure TCpu8088.RcrRM8Const1(AModRM: TModRM);
var
  AOp, OldCF, Result: Byte;
const
  CFMask: array[False..True] of Byte = (0, $80);
begin
  AOp := ReadRM8(AModRM);
  OldCF := IfThen(Registers.Flags.CF, 1, 0);
  Result := (AOp shr 1) or (OldCF shl 7);
  WriteRM8(AModRM, Result);

  Registers.Flags.CF := (AOp and 1) <> 0;
  Registers.Flags.OF_ := ((AOp xor CFMask[Registers.Flags.CF]) and $8000) = 0;
end;

procedure TCpu8088.RcrRM16Const1(AModRM: TModRM);
var
  AOp, OldCF, Result: Word;
const
  CFMask: array[False..True] of Word = (0, $8000);
begin
  AOp := ReadRM16(AModRM);
  OldCF := IfThen(Registers.Flags.CF, 1, 0);
  Result := (AOp shr 1) or (OldCF shl 15);
  WriteRM16(AModRM, Result);

  Registers.Flags.CF := (AOp and 1) <> 0;
  Registers.Flags.OF_ := ((AOp xor CFMask[Registers.Flags.CF]) and $8000) = 0;
end;

procedure TCpu8088.RcrRM8CL(AModRM: TModRM);
var
  AOp, OldCF, Result: Byte;
  I: Integer;
const
  CFMask: array[False..True] of Byte = (0, $80);
begin
  AOp := ReadRM8(AModRM);
  Result := AOp;
  for I := 1 to Registers.CL do
  begin
    OldCF := IfThen(Registers.Flags.CF, 1, 0);
    Registers.Flags.CF := (Result and 1) <> 0;
    Result := (Result shr 1) or (OldCF shl 7);
  end;
  WriteRM8(AModRM, Result);

  if Registers.CL = 1 then
    Registers.Flags.OF_ := ((AOp xor CFMask[Registers.Flags.CF]) and $8000) = 0;
end;

procedure TCpu8088.RcrRM16CL(AModRM: TModRM);
var
  AOp, OldCF, Result: Word;
  I: Integer;
const
  CFMask: array[False..True] of Word = (0, $8000);
begin
  AOp := ReadRM16(AModRM);
  Result := AOp;
  for I := 1 to Registers.CL do
  begin
    OldCF := IfThen(Registers.Flags.CF, 1, 0);
    Registers.Flags.CF := (Result and 1) <> 0;
    Result := (Result shr 1) or (OldCF shl 15);
  end;
  WriteRM16(AModRM, Result);

  if Registers.CL = 1 then
    Registers.Flags.OF_ := ((AOp xor CFMask[Registers.Flags.CF]) and $8000) = 0;
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
  Result: DWord;
begin
  Result := Registers.AX * ReadRM16(AModRM);
  Registers.DX := Hi(Result);
  Registers.AX := Lo(Result);
  Registers.Flags.UpdateAfterMul16(Result);
end;

procedure TCpu8088.ImulALRM8(AModRM: TModRM);
var
  Result: Int16;
begin
  Result := Int8(Registers.AL) * Int8(ReadRM8(AModRM));
  Registers.AX := Result;
  Registers.Flags.UpdateAfterImul8(Result);
end;

procedure TCpu8088.ImulAXRM16(AModRM: TModRM);
var
  Result: Int32;
begin
  Result := Int16(Registers.AX) * Int16(ReadRM16(AModRM));
  Registers.DX := Hi(Result);
  Registers.AX := Lo(Result);
  Registers.Flags.UpdateAfterImul16(Result);
end;

procedure TCpu8088.DivALRM8(AModRM: TModRM);
var
  Divisor: Byte;
  Result: Word = 0;
  Remainder: Word = 0;
begin
  Divisor := ReadRM8(AModRM);
  if Divisor = 0 then
  begin
    RaiseHardwareInterrupt(0);
    Exit;
  end;

  DivMod(Registers.AX, Divisor, Result, Remainder);
  if Result > Byte.MaxValue then
  begin
    RaiseHardwareInterrupt(0);
    Exit;
  end;

  Registers.AL := Lo(Result);
  Registers.AH := Lo(Remainder);
end;

procedure TCpu8088.DivAXRM16(AModRM: TModRM);
var
  Divisor: Word;
  Result: DWord = 0;
  Remainder: DWord = 0;
  Dividend: DWord;
begin
  Divisor := ReadRM16(AModRM);
  if Divisor = 0 then
  begin
    RaiseHardwareInterrupt(0);
    Exit;
  end;

  Dividend := (Registers.DX shl 16) or Registers.AX;
  DivMod(Dividend, Divisor, Result, Remainder);
  if Result > Word.MaxValue then
  begin
    RaiseHardwareInterrupt(0);
    Exit;
  end;

  Registers.AX := Lo(Result);
  Registers.DX := Lo(Remainder);
end;

procedure TCpu8088.IdivALRM8(AModRM: TModRM);
var
  Divisor: Int8;
  Result: Int16 = 0;
  Remainder: Int16 = 0;
begin
  Divisor := ReadRM8(AModRM);
  if Divisor = 0 then
  begin
    RaiseHardwareInterrupt(0);
    Exit;
  end;

  DivMod(Int16(Registers.AX), Divisor, Result, Remainder);
  if not InRange(Result, Int8.MinValue, Int8.MaxValue) then
  begin
    RaiseHardwareInterrupt(0);
    Exit;
  end;

  Registers.AL := Lo(Result);
  Registers.AH := Lo(Remainder);
end;

procedure TCpu8088.IdivAXRM16(AModRM: TModRM);
var
  Divisor: Int16;
  Result: Int32 = 0;
  Remainder: Int32 = 0;
  Dividend: Int32;
begin
  Divisor := ReadRM16(AModRM);
  if Divisor = 0 then
  begin
    RaiseHardwareInterrupt(0);
    Exit;
  end;

  Dividend := Int32((Registers.DX shl 16) or Registers.AX);
  DivMod(Dividend, Divisor, Result, Remainder);
  if not InRange(Result, Int16.MinValue, Int16.MaxValue) then
  begin
    RaiseHardwareInterrupt(0);
    Exit;
  end;

  Registers.AX := Lo(Result);
  Registers.DX := Lo(Remainder);
end;

procedure TCpu8088.IncRM8(AModRM: TModRM);
var
  AOp: Byte;
  Result: Int16;
begin
  AOp := ReadRM8(AModRM);
  Result := AOp + 1;
  WriteRM8(AModRM, Result);
  Registers.Flags.UpdateAfterInc8(AOp, Result);
end;

procedure TCpu8088.IncRM16(AModRM: TModRM);
var
  AOp: Word;
  Result: Int32;
begin
  AOp := ReadRM16(AModRM);
  Result := AOp + 1;
  WriteRM16(AModRM, Result);
  Registers.Flags.UpdateAfterInc16(AOp, Result);
end;

function TCpu8088.CheckRepetition: Boolean; { for debug }
begin
  Result := FCurrentInstruction.Repeating;
  if Registers.CX = 0 then Exit(False);

  case FCurrentInstruction.Repetition of
    repRepE: Result := Registers.Flags.ZF;
    repRepNE: Result := not Registers.Flags.ZF;
  else;
  end;
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

function TCpu8088.DumpCurrentInstruction: String;
var
  I: Integer;
  Buf: String = '';
begin
  WriteStr(Result,
    Format('%.4x:%.4x | ',
    [FCurrentInstruction.CS, FCurrentInstruction.IP]));

  for I := 0 to FCurrentInstruction.Length - 1 do
    Buf := Buf + IntToHex(FCurrentInstruction.Code[I], 2);

  WriteStr(Result,
    Format(
      '%-10s | AX:%.4x | BX:%.4x | CX:%.4x | DX:%.4x | ' +
      'SI:%.4x | DI:%.4x | DS:%.4x | ES:%.4x | SS:%.4x',
      [
        Buf, Registers.AX, Registers.BX, Registers.CX, Registers.DX,
        Registers.SI, Registers.DI, Registers.DS, Registers.ES, Registers.SS
      ]));
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

  FNmiPending := False;
  FHardwareInterrupt.Pending := False;

  Registers.Flags.Reset;

  FTicks := 0;
  FHalted := False;
end;

procedure TCpu8088.Tick;
begin
  Inc(FTicks);
  Dec(FWaitStates);

  if (WaitStates > 0) then Exit;

  if FNmiPending then
  begin
    FNmiPending := False;
    EnterISR(2);
  end;

  if FHardwareInterrupt.Pending and Registers.Flags.IF_ then
    HandleHardwareInterrupt;

  if Halted then Exit;

  if Assigned(OnBeforeInstruction)
      and OnBeforeInstruction(Self, CurrentAddress) then
    Exit;

  if not FCurrentInstruction.Repeating then FetchInstruction;

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
      $F2: FCurrentInstruction.Repetition := repRepNE;
      $F3: FCurrentInstruction.Repetition := repRep;
      $26: FCurrentInstruction.SegmentOverride := soES;
      $2E: FCurrentInstruction.SegmentOverride := soCS;
      $36: FCurrentInstruction.SegmentOverride := soSS;
      $3E: FCurrentInstruction.SegmentOverride := soDS;
    else
      begin
        FCurrentInstruction.OpCode := Data;
        Break;
      end;
    end;
  until False;

  if not (FCurrentInstruction.OpCode in
      [$6C, $6D, $6E, $6F, $A4, $A5, $A6, $A7, $AA, $AB, $AC, $AD, $AE, $AF]) then
    FCurrentInstruction.Repetition := repNone;

  case FCurrentInstruction.Repetition of
    repRep:
      if FCurrentInstruction.OpCode in [$A6, $A7, $AE, $AF] then
        { CMPS and SCAS only }
        FCurrentInstruction.Repetition := repRepE;

    repRepE, repRepNE:
      if not (FCurrentInstruction.OpCode in [$A6, $A7, $AE, $AF]) then
        FCurrentInstruction.Repetition := repRep;
  else;
  end;

  FCurrentInstruction.Repeating := (FCurrentInstruction.Repetition <> repNone)
end;

function TCpu8088.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

end.
