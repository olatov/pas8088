unit VideoController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
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

  TVideoController = class(TComponent)
  private
    type
      TVideoMode = (vmText40, vmText80, vmGraphics320, vmGraphics640);
    const
      BaseSegment = $B800;
  private
    FActiveMode: TVideoMode;
    FBackgroundColor: TColor;
    FGetActivePallette: TCGAPallette;
    function GetActivePallette: TCGAPallette;
    function GetBitsPerPixel: Byte;
    function GetSegment: Word;
    procedure SetActiveMode(AValue: TVideoMode);
    procedure SetBackgroundColor(AValue: TColor);
  private
    FIOBus: IIOBus;
    FMemoryBus: IMemoryBus;
    function GetScanlines(ANumber: TVideoRows): TScanline;
    procedure SetIOBus(AValue: IIOBus);
    procedure SetMemoryBus(AValue: IMemoryBus);
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property ActivePallette: TCGAPallette read GetActivePallette;
    property ActiveMode: TVideoMode read FActiveMode write SetActiveMode;
    property Segment: Word read GetSegment;
    property BitsPerPixel: Byte read GetBitsPerPixel;
  public
    property MemoryBus: IMemoryBus read FMemoryBus write SetMemoryBus;
    property IOBus: IIOBus read FIOBus write SetIOBus;
    property ScanLines[ANumber: TVideoRows]: TScanLine read GetScanLines;
  end;

implementation

{ TVideoController }

procedure TVideoController.SetIOBus(AValue: IIOBus);
begin
  if FIOBus = AValue then Exit;
  FIOBus := AValue;
end;

procedure TVideoController.SetActiveMode(AValue: TVideoMode);
begin
  if FActiveMode = AValue then Exit;
  FActiveMode := AValue;
end;

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
  ActiveMode := vmText40;

  Addr := ((ANumber shr 1) * 80);
  if (Odd(ANumber)) then Inc(Addr, $2000);

  Pallette := ActivePallette;

  ColorMask := (1 shl BitsPerPixel) - 1;

  I := Low(Result);
  while I <= High(Result) do
  begin
    Data := MemoryBus.ReadByte(Segment, Addr);
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

procedure TVideoController.SetMemoryBus(AValue: IMemoryBus);
begin
  if FMemoryBus = AValue then Exit;
  FMemoryBus := AValue;
end;

end.

