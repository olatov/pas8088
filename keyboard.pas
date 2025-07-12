unit Keyboard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Hardware;

type

  { TKeyboard }

  TKeyboard = class(TComponent, IIOBusDevice)
  public
    type TKey = (
      keyNone, keyEsc, keyEnter, keySpace, keyBackspace,
      keyLeftShift, keyRightShift, keyLeftControl, keyRightControl,
      keyLeftAlt, keyRightAlt, keyTilde, keyTab, keyCapsLock, keyNumLock,
      keyPrintScreen, keyPauseBreak, keyScrollLock, keySemiColon, keyPipe,
      keyF1, KeyF2, keyF3, KeyF4, keyF5, KeyF6, keyF7, keyF8, keyF9, keyF10,
      keyQ, keyW, keyE, keyR, keyT, keyY, keyU, keyI, keyO, keyP,
      keyA, keyS, keyD, keyF, keyG, keyH, keyJ, keyK, keyL,
      keyZ, keyX, keyC, keyV, keyB, keyN, keyM,
      keyD1, keyD2, keyD3, keyD4, keyD5,
      keyD6, keyD7, keyD8, keyD9, keyD0,
      keyNumPad0, keyNumPad1, keyNumPad2, keyNumPad3, keyNumPad4, keyNumPad5,
      keyNumPad6, keyNumPad7, keyNumPad8, keyNumPad9, keyNumPad10, keyDecimal
    );
  private
    FIOBus: IIOBus;
    FKeyState: array[Low(TKey)..High(TKey)] of Boolean;

    FScanCode: Byte;
    function GetActiveRow: Byte;
    property ScanCode: Byte read FScanCode write FScanCode;
    function GetKeyState(AKey: TKey): Boolean;
    procedure SetKeyState(AKey: TKey; AValue: Boolean);
  public
    property KeyState[AKey: TKey]: Boolean read GetKeyState write SetKeyState; default;
    property ActiveRow: Byte read GetActiveRow;

    { IO bus device API }
    function GetIOBus: IIOBus;
    procedure SetIOBus(AValue: IIOBus);
    procedure WriteIOByte(AAddress: Word; AData: Byte);
    function ReadIOByte(AAddress: Word): Byte;
    property IOBus: IIOBus read GetIOBus write SetIOBus;
    function OnIORead(ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
    procedure OnIOWrite(Sender: IIOBusDevice; AAddress: Word; AData: Byte);
  end;

var
  Port68Map: array[0..7, 0..7] of TKeyboard.TKey = (
    (keyNone { closing bracket }, keyNumPad5, keyNone, keySemiColon, keyNone, keyEnter, keyNumPad8, keyNumPad2),
    (keyNone { plus }, keyNumPad4, keyNumPad0, KeyF6, keyPipe, keyBackspace, keyNumPad7, keyNumPad1),
    (keyF, keyNumPad9, keyNumpad3, keyN, keyD, keyS, keyZ, keyNone),
    (keyF2, keyNone, keyNone, keyF5, keyF1, keyEsc, keyTilde, keyPauseBreak),
    (keyE, keyNone, keyCapsLock, keyH, keyW, keyQ, keyA, keyNone),
    (keyLeftShift, keyRightControl, keyLeftControl, keyC, keyX, keyLeftAlt, keyRightAlt, keyRightShift),
    (keyNone, keyNumLock, keyDecimal, keyNone, keyNone, keyF9, keyScrollLock, keyNumPad6),
    (keyNone { D3 }, keyPrintScreen, keyTab, keyY, keyNone { D2 }, keyF10, keyNone { D1 }, keyNone { UKR i })
  );

  Port6AMap: array[0..7, 0..3] of TKeyboard.TKey = (
    (keyD0, keyO, keyNone { minus }, keyNone { quote }),
    (keyD8, keyD9, keyP, keyNone { closing bracket }),
    (keyM, KeyK, keyB, keyG),
    (keyD6, keyD7, keyF4, keyF3),
    (keyJ, keyI, keyT, keyR),
    (keyNone { period }, keyNone { comma }, keyV, keySpace),
    (keyL, keyF7, keyNone { question }, keyF8),
    (keyNone, keyU, keyD5, keyD4)
  );

implementation

{ TKeyboard }

function TKeyboard.GetActiveRow: Byte;
var
  I: Integer;
begin
  for I := 0 to 7 do
    if (ScanCode and (1 shl I) <> 0) then Exit(I);
  Result := 0;
end;

function TKeyboard.GetKeyState(AKey: TKey): Boolean;
begin
  Result := FKeyState[AKey];
end;

procedure TKeyboard.SetKeyState(AKey: TKey; AValue: Boolean);
begin
  FKeyState[AKey] := AValue;
end;

function TKeyboard.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TKeyboard.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TKeyboard.WriteIOByte(AAddress: Word; AData: Byte);
begin
  { n / a }
end;

function TKeyboard.ReadIOByte(AAddress: Word): Byte;
begin
  { n / a }
end;

function TKeyboard.OnIORead(ADevice: IIOBusDevice; AAddress: Word; out
  AData: Byte): Boolean;
var
  I: Integer;
begin
  case AAddress of
    $60: AData := ScanCode;
    $69:
      begin
        Result := True;
        AData := 0;
        if ScanCode = $FF then Exit;

        for I := 0 to High(Port68Map) do
          if Self[Port68Map[ActiveRow, I]] then
            AData := AData or (1 shl I);

        AData := not AData;
      end;
    $6A:
      begin
        Result := True;
        if ScanCode = $FF then
        begin
          AData := 0;
          Exit;
        end;
        AData := 0;

        for I := 0 to High(Port6AMap) do
          if not Self[Port6AMap[ActiveRow, I]] then
            AData := AData or (1 shl I);

        AData := $F0 or (not AData);
        AData := $FF;
      end;
  else
    Result := False;
  end;
end;

procedure TKeyboard.OnIOWrite(
  Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin
  case AAddress of
    $60: ScanCode := AData;
  end;
end;

end.

