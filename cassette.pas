unit Cassette;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, fpwavformat, fpwavreader, fpwavwriter,
  Hardware;

type

  { TCassetteDrive }

  TCassetteDrive = class(TComponent, IIOBusDevice)
  private
    FIOBus: IIOBus;
    FTapeStream: TStream;
    FWavReader: TWavReader;
    FTapeIn, FTapeOut: Boolean;
  public
    State: (csStopped, csPlaying, csRecording);
    property TapeOut: Boolean read FTapeOut;
    property TapeIn: Boolean read FTapeIn;
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure CreateTape;
    procedure LoadTape(ATapeStream: TStream);
    procedure Play;
    procedure Record_;
    procedure Stop;
    procedure Rewind;
    procedure Eject;
    procedure Tick(const ADeltaTime: Double);

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

{ TCassetteDrive }

procedure TCassetteDrive.LoadTape(ATapeStream: TStream);
begin
  FTapeStream := ATapeStream;
  FWavReader.LoadFromStream(FTapeStream);
end;

procedure TCassetteDrive.Play;
begin
  if not Assigned(FTapeStream) then Exit;
  State := csPlaying;
end;

procedure TCassetteDrive.Record_;
begin

end;

procedure TCassetteDrive.Stop;
begin
  State := csStopped;
end;

procedure TCassetteDrive.Rewind;
begin
  if not Assigned(FTapeStream) then Exit;
  FTapeStream.Seek(0, soBeginning);
end;

procedure TCassetteDrive.Eject;
begin
  FTapeStream := Nil;
end;

procedure TCassetteDrive.Tick(const ADeltaTime: Double);
var
  Buffer: array[1..4096] of Byte;
  DesiredCount, ActualCount: Int64;
begin
  DesiredCount := Trunc(
    FWavReader.Fmt.SampleRate * FWavReader.Fmt.Channels * ADeltaTime);
  { Assert(InRange(DesiredCount, 1, Length(Buffer))); }
  if DesiredCount = 0 then Exit;

  case State of
    csPlaying:
      begin
        ActualCount := FWavReader.ReadBuf(Buffer, DesiredCount);
        if ActualCount < DesiredCount then
        begin
          { End of tape }
          State := csStopped;
        end;

        { Todo: Assume 8-bit unsigned for now }
        if ActualCount > 0 then
          FTapeIn := Buffer[(ActualCount div 2) + 1] > 127;
      end;
  end;
end;

constructor TCassetteDrive.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWavReader := TWavReader.Create;
end;

destructor TCassetteDrive.Destroy;
begin
  FreeAndNil(FWavReader);
  inherited Destroy;
end;

procedure TCassetteDrive.CreateTape;
begin
  FTapeStream := TMemoryStream.Create;
end;

function TCassetteDrive.GetIOBus: IIOBus;
begin
  Result := FIOBus;
end;

procedure TCassetteDrive.SetIOBus(AValue: IIOBus);
begin
  FIOBus := AValue;
end;

procedure TCassetteDrive.WriteIOByte(AAddress: Word; AData: Byte);
begin

end;

function TCassetteDrive.ReadIOByte(AAddress: Word): Byte;
begin

end;

function TCassetteDrive.OnIORead(
  ADevice: IIOBusDevice; AAddress: Word; out AData: Byte): Boolean;
begin
  Result := False;
  if AAddress = $62 then
  begin
    AData := IfThen(FTapeIn, $10, $00);
      Result := True;
  end;
end;

procedure TCassetteDrive.OnIOWrite(
  Sender: IIOBusDevice; AAddress: Word; AData: Byte);
begin

end;

end.

