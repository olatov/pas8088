unit IO;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Cpu8088;

type

  { TIOBus }

  TIOBus = class(TComponent, IIOBus)
  public
    procedure WriteByte(AAddress: Word; AData: Byte);
    function ReadByte(AAddress: Word): Byte;
  end;

implementation

{ TIOBus }

procedure TIOBus.WriteByte(AAddress: Word; AData: Byte);
begin
  Writeln(Format('IO: write 0x%.2x -> port 0x%x', [AData, AAddress]));
end;

function TIOBus.ReadByte(AAddress: Word): Byte;
begin
  Result := $FF;
  Writeln(Format('IO: read 0x%.2x <- port 0x%x ', [Result, AAddress]));
end;

end.

