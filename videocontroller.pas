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

    { Palette 0, high intensity [11] }
    (CGADarkGray, CGABrightGreen, CGABrightRed, CGAYellow),

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
  private
    FBackgroundColor: TColor;
    FGetActivePallette: TCGAPallette;
    FLatch: array[$28..$2A] of Byte;
    FVideoModeSelector: Byte;
    function GetActiveMode: TVideoMode;
    function GetActivePallette: TCGAPallette;
    function GetBitsPerPixel: Byte;
    function GetSegment: Word;
    procedure SetBackgroundColor(AValue: TColor);
  private
    FIOBus: IIOBus;
    FMemoryBus: IMemoryBus;
    FNmiGate: INmiGate;
    function GetScanlines(ANumber: TVideoRows): TScanline;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property ActivePallette: TCGAPallette read GetActivePallette;
    property ActiveMode: TVideoMode read GetActiveMode;
    property Segment: Word read GetSegment;
    property BitsPerPixel: Byte read GetBitsPerPixel;
  public
    property NmiGate: INmiGate read FNmiGate write FNmiGate;
    property ScanLines[ANumber: TVideoRows]: TScanLine read GetScanLines;

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
    vmText40, vmText80: Result := BaseSegment + $400;
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
begin
  if BitsPerPixel = 2 then
    Result := CGAPallettes[2]
  else
    Result := CGAPallettes[4];
end;

function TVideoController.GetActiveMode: TVideoMode;
begin
  case FVideoModeSelector of
    %0110: Result := vmText40;
    %1101: Result := vmText80;
    %0010: Result := vmGraphics320;
    %1010: Result := vmGraphics640;
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
    if Offset < $4000 then
    begin
      FLatch[$28] := Lo(Offset);
      FLatch[$29] := Hi(Offset);
      FLatch[$2A] := AData;
      if Assigned(NmiGate) then NmiGate.RaiseNmi;
    end;
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
  else
    Result := False;
  end;
end;

procedure TVideoController.OnIOWrite(Sender: IIOBusDevice; AAddress: Word;
  AData: Byte);
begin
  case AAddress of
    $68:
      FVideoModeSelector :=
        (FVideoModeSelector and $F3) or ((AData shr 4) and $0C);
    $6A:
      FVideoModeSelector :=
        (FVideoModeSelector and $FC) or ((AData shr 6) and $03);
  end;
  {
                  ((_ports[0x68] >> 4) & 0b1100) |
                  ((_ports[0x6a] >> 6) & 0b0011);
  }
end;

end.

