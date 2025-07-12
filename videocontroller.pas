unit VideoController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, Hardware,
  Cpu8088;

const
  CGABlack = $000000;
  CGABlue = $aa0000;
  CGAGreen = $00aa00;
  CGACyan = $aaaa00;
  CGARed = $0000aa;
  CGAMagenta = $aa00aa;
  CGABrown = $0055aa;
  CGALightGray = $aaaaaa;
  CGADarkGray = $282828; { $555555 }
  CGABrightBlue = $ff0000;
  CGABrightGreen = $00ff00;
  CGABrightCyan = $ffff00;
  CGABrightRed = $0000ff;
  CGABrightMagenta = $ff00ff;
  CGAYellow = $55ffff;
  CGAWhite = $ffffff;

type
  TColor = UInt32;
  TVideoCols = 0..639;
  TVideoRows = 0..199;
  TScanLine = array[0..High(TVideoCols)] of TColor;
  TCGAPallette = array[0..3] of TColor;

var
  CGAPallettes: array[0..4] of TCGAPallette = (
    { Palette 0, low intensity [00] }
    (CGABlack, CGAGreen, CGARed, CGABrown),

    { Palette 1, low intensity [01] }
    (CGABlack, CGACyan, CGAMagenta, CGAWhite),

    { Palette 1, high intensity [10] }
    (CGADarkGray, CGABrightCyan, CGABrightMagenta, CGAWhite),

    { Palette 0, high intensity [11], possibly incorrect on Poisk }
    { (CGADarkGray, CGABrightGreen, CGABrightRed, CGAYellow), }
    (CGADarkGray, CGABrightCyan, CGABrightMagenta, CGAWhite),

    { Hi res BW pallette }
    (CGABlack, CGAWhite, CGABlack, CGABlack)
  );

type
  { TVideoController }

  TVideoController = class(TComponent, IMemoryBusDevice, IIOBusDevice)
  private
    type
      TVideoMode = (vmText40, vmText80, vmGraphics320, vmGraphics640);
    const
      BaseSegment = $B800;
    procedure ActivateTrap(const AOffset: Word; const AData: Byte = 0);
  private
    Tmp: Byte;
    FBackgroundColor: TColor;
    FGetActivePallette: TCGAPallette;
    FActivePalletteIndex: Byte;
    FLatch: array[$28..$2A] of Byte;
    FVideoModeSelector: Byte;

    {
      0  BGColor
      1  BGColor
      2  BGColor
      3  NMI Disable
      4  Intensity
      5  Pal 0/1
      6  Page
      7  Hires
    }

    FPort68Register: bitpacked record
      BackgroundColor: 0..%111;
      NmiDisable: 0..%1;
      Intensity: 0..%1;
      Pallette: 0..%1;
      TextMode: 0..%1;
      HiRes: 0..%1;
    end;

    {
      0
      1
      2
      3
      4
      5
      6  VideoMode LoNibble
      7  VideoMode LoNibble
    }

    FPort6ARegister: bitpacked record
      Unused: 0..%111111;
      VideoModeLoNibble: 0..%11;
    end;
    function GetActiveMode: TVideoMode;
    function GetActivePallette: TCGAPallette;
    function GetBitsPerPixel: Byte;
    function GetSegment: Word;
    procedure SetBackgroundColor(AValue: TColor);
  private
    FIOBus: IIOBus;
    FMemoryBus: IMemoryBus;
    FNmiTrigger: INmiTrigger;
    function GetScanlines(ANumber: TVideoRows): TScanline;
    property ActivePallette: TCGAPallette read GetActivePallette;
    property ActiveMode: TVideoMode read GetActiveMode;
    property Segment: Word read GetSegment;
    property BitsPerPixel: Byte read GetBitsPerPixel;
  public
    property NmiTrigger: INmiTrigger read FNmiTrigger write FNmiTrigger;
    property ScanLines[ANumber: TVideoRows]: TScanLine read GetScanLines;
    property BackgroundColor: TColor read FBackgroundColor;

    { Memory bus device API }
    function GetMemoryBus: IMemoryBus;
    procedure SetMemoryBus(AValue: IMemoryBus);
    procedure WriteMemoryByte(AAddress: TPhysicalAddress; AData: Byte);
    function ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
    property MemoryBus: IMemoryBus read GetMemoryBus write SetMemoryBus;
    function OnMemoryRead(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; out AData: Byte): Boolean;
    procedure OnMemoryWrite(Sender: IMemoryBusDevice; AAddress: TPhysicalAddress; AData: Byte);

    { IO bus device API }
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

implementation

{ TVideoController }

function TVideoController.GetSegment: Word;
begin
  case ActiveMode of
    vmText40, vmText80: Result := BaseSegment + (FPort68Register.TextMode shl 10);
    vmGraphics320, vmGraphics640: Result := BaseSegment;
  end;
end;

function TVideoController.GetBitsPerPixel: Byte;
begin
  case ActiveMode of
    vmText40, vmGraphics320: Result := 2;
    vmText80, vmGraphics640: Result := 1;
  end;
end;

function TVideoController.GetActivePallette: TCGAPallette;
var
  Index: bitpacked record
    Intensity: 0..%1;
    Palette: 0..%1;
    Unused: 0..%111111;
  end;
begin
  Index.Palette := FPort68Register.Pallette;
  Index.Intensity := FPort68Register.Intensity;
  Index.Unused := 0;

  case BitsPerPixel of
    2: Result := CGAPallettes[Byte(Index)];
  else
    Result := CGAPallettes[4];
  end;
end;

procedure TVideoController.ActivateTrap(
  const AOffset: Word; const AData: Byte = 0);
begin
  FLatch[$28] := Lo(AOffset);
  FLatch[$29] := Hi(AOffset);
  FLatch[$2A] := AData;
  if (FPort68Register.NmiDisable = 0) and Assigned(NmiTrigger) then
    NmiTrigger.RaiseNmi;
end;

function TVideoController.GetActiveMode: TVideoMode;
var
  Index: bitpacked record
    TextMode: 0..%1;
    HiRes: 0..%1;
    Unused: 0..%111111;
  end;
begin
  Index.TextMode := FPort68Register.TextMode;
  Index.HiRes := FPort68Register.HiRes;
  Index.Unused := 0;

  case Byte(Index) of
    %01: Result := vmText40;
    %11: Result := vmText80;
    %00: Result := vmGraphics320;
    %10: Result := vmGraphics640;
  else
    Result := vmText40;
  end;
end;

procedure TVideoController.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then Exit;
  FBackgroundColor := AValue;
end;

function TVideoController.GetScanlines(ANumber: TVideoRows): TScanline;
var
  I, J, K: Integer;
  Addr: Word;
  Data: Byte;
  Pallette: TCGAPallette;
  ColorIndex, ColorMask: Byte;
begin
  Addr := ((ANumber shr 1) * 80);
  if (Odd(ANumber)) then Inc(Addr, $2000);

  Pallette := ActivePallette;
  Pallette[0] := BackgroundColor;

  ColorMask := (1 shl BitsPerPixel) - 1;

  I := Low(Result);
  while I <= High(Result) do
  begin
    Data := ReadMemoryByte((Segment shl 4) + Addr);

    for J := ((8 Div BitsPerPixel) - 1) downto 0 do
    begin
      ColorIndex := (Data shr (J * BitsPerPixel) and ColorMask);
      for K := 0 to BitsPerPixel - 1 do
        Result[I + K] := Pallette[ColorIndex];
      Inc(I, BitsPerPixel);
    end;
    Inc(Addr);
  end;
end;

function TVideoController.GetMemoryBus: IMemoryBus;
begin
  Result := FMemoryBus;
end;

procedure TVideoController.SetMemoryBus(AValue: IMemoryBus);
begin
  if FMemoryBus = AValue then Exit;
  FMemoryBus := AValue;
end;

procedure TVideoController.WriteMemoryByte(
  AAddress: TPhysicalAddress; AData: Byte);
begin

end;

function TVideoController.ReadMemoryByte(AAddress: TPhysicalAddress): Byte;
begin
  if not Assigned(MemoryBus) then Exit;
  MemoryBus.InvokeRead(Self, AAddress, Result);
end;

function TVideoController.OnMemoryRead(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; out AData: Byte): Boolean;
begin
  Result := False;
end;

procedure TVideoController.OnMemoryWrite(Sender: IMemoryBusDevice;
  AAddress: TPhysicalAddress; AData: Byte);
var
  Offset: Word;
begin
  if InRange(AAddress, $B8000, $BFFFF) then
  begin
    { NMI trap }

    Offset := AAddress - $B8000;
    if Offset < $4000 then ActivateTrap(Offset, AData);
  end;
end;

function TVideoController.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TVideoController.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TVideoController.WriteIOByte(AAddress: Word; AData: Byte);
begin
  { n / a }
end;

function TVideoController.ReadIOByte(AAddress: Word): Byte;
begin

end;

function TVideoController.OnIORead(ADevice: IIOBusDevice; AAddress: Word; out
  AData: Byte): Boolean;
begin
  case AAddress of
    $28..$2A:
      begin
        AData := FLatch[AAddress];
        Result := True;
      end;

    $68:
      begin
        AData := Byte(FPort68Register);
        Result := True;
      end;

    $6A:
      begin
        AData := Byte(FPort6ARegister);
        Result := True;
      end;

    $3D4, $3D5, $3D8, $3D9:
      begin
        { Writeln(Format('CGA read %.x', [AAddress])); }
        ActivateTrap(Lo(AAddress) or $4000);
      end;

    $3DA:
      begin
        { Todo }
        { Writeln(Format('CGA read %.x', [AAddress])); }
        Tmp := Tmp xor $08;
        AData := Tmp;
        Result := Result;
      end;

  else
    Result := False;
  end;
end;

procedure TVideoController.OnIOWrite(Sender: IIOBusDevice; AAddress: Word;
  AData: Byte);
begin
  case AAddress of
    $68:  Move(AData, FPort68Register, 1);
    $6A:  Move(AData, FPort6ARegister, 1);

    $3D4, $3D5, $3D8, $3D9, $3DA:
      Writeln(Format('CGA write %.x: %.2x', [AAddress, AData]));
  end;
end;

end.

