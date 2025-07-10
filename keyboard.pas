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
      keyEsc, keyEnter, keySpace,
      keyF1, KeyF2, keyF3, KeyF4, keyF5, KeyF6, keyF7, keyF8, keyF9, keyF10,
      keyE, keyW, keyS, keyA, keyD
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
begin
  case AAddress of
    $60: AData := ScanCode;
    $69:
      begin
        Result := True;
        AData := 0;
        if ScanCode = $FF then Exit;

        if Self[keyEnter] and (ActiveRow = 0) then AData := AData or (1 shl 5);
        if Self[keyEsc] and (ActiveRow = 4) then  AData := AData or (1 shl 5);
        if Self[keyF1] and (ActiveRow = 3) then AData := AData or (1 shl 4);
        if Self[keyF2] and (ActiveRow = 3) then AData := AData or (1 shl 0);

        if Self[keyE] and (ActiveRow = 4) then AData := AData or (1 shl 0);

        if Self[keyW] and (ActiveRow = 4) then AData := AData or (1 shl 4);
        if Self[keyA] and (ActiveRow = 4) then AData := AData or (1 shl 6);
        if Self[keyS] and (ActiveRow = 2) then AData := AData or (1 shl 5);
        if Self[keyD] and (ActiveRow = 2) then AData := AData or (1 shl 4);

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

