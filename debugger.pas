unit Debugger;

{$mode ObjFPC}{$H+}
{$R-}

interface

uses
  Classes, SysUtils, StrUtils, Generics.Collections, Process, Math,
  Hardware, System.IOUtils;

type
  { TDebugger }

  TDebugger = class
  type
    TProgramLines = specialize TDictionary<TPhysicalAddress, String>;
    TSourceLine = record
      Address: TPhysicalAddress;
      Contents: String;
    end;
    TSourceLines = array of TSourceLine;
  private
    FProgramLines: TProgramLines;
    FOpcodeAddresses: array of TPhysicalAddress;
    procedure RefreshOpcodeAddresses;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(AFileName: String; AOrigin, ASync: TPhysicalAddress);
    procedure LoadFromStream(AStream: TStream; AOrigin, ASync: TPhysicalAddress);
    procedure LoadBytes(ABytes: TBytes; AOrigin, ASync: TPhysicalAddress);
    function DisassembleBytes(ABytes: TBytes; AOrigin: TPhysicalAddress = 0): TSourceLines;
    function FindLine(AAddress: TPhysicalAddress): String;
    function FindLine(ASegment, AOffset: Word): String;
    function GetListing(AAddress: TPhysicalAddress; ABefore: Integer = 2; AAfter: Integer = 8): TSourceLines;
  end;

implementation

{ TDebugger }

procedure TDebugger.RefreshOpcodeAddresses;
begin
  FOpcodeAddresses := FProgramLines.Keys.ToArray;
  specialize TArrayHelper<TPhysicalAddress>.Sort(FOpcodeAddresses);
end;

constructor TDebugger.Create;
begin
 FProgramLines := TProgramLines.Create;
end;

destructor TDebugger.Destroy;
begin
 FreeAndNil(FProgramLines);
 inherited Destroy;
end;

procedure TDebugger.Clear;
begin
  FOpcodeAddresses := [];
  FProgramLines.Clear;
end;

procedure TDebugger.LoadFromFile(AFileName: String; AOrigin, ASync: TPhysicalAddress);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(Stream, AOrigin, ASync);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TDebugger.LoadFromStream(AStream: TStream; AOrigin, ASync: TPhysicalAddress
  );
var
  Cmd, Contents, Line: String;
  SourceLines: TStringList;
  LineAddr: TPhysicalAddress;
  LineCode: String;
  LineAssembly: String;
  TmpStream: TFileStream;
  S: String;
  NDisasmProcess: TProcess;
const
  TmpFile = '/tmp/bytes.tmp';
begin

  TmpStream := TFileStream.Create(TmpFile, fmCreate);
  try
    TmpStream.CopyFrom(AStream, AStream.Size);
    TmpStream.Flush;
    Cmd := Format('/usr/bin/ndisasm -o0x%.x -s0x%.x %s', [AOrigin, ASync, TmpFile]);
    RunCommand(Cmd, Contents);
  finally
    FreeAndNil(TmpStream);
    if TFile.Exists(TmpFile) Then TFile.Delete(TmpFile);
  end;
  //Writeln(Cmd);

  SourceLines := TStringList.Create;
  SourceLines.Text := Contents;
  for Line in SourceLines do
  begin
    try
      S := Trim(Copy(Line, 0, 8));
      if S.IsEmpty then Continue;

      LineAddr := Hex2Dec(S);
      LineCode := Trim(Copy(Line, 10, 17));
      LineAssembly := Trim(Copy(Line, 28, 32));
      FProgramLines.AddOrSetValue(LineAddr, LineAssembly);
    except
      on E: Exception do
        Writeln('Debugger: could not parse line');
    end;
  end;
  FreeAndNil(SourceLines);
  RefreshOpcodeAddresses;
end;

procedure TDebugger.LoadBytes(ABytes: TBytes; AOrigin, ASync: TPhysicalAddress);
var
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create(ABytes);
  try
    LoadFromStream(Stream, AOrigin, ASync);
  finally
    FreeAndNil(Stream);
  end;
end;

function TDebugger.DisassembleBytes(ABytes: TBytes; AOrigin: TPhysicalAddress
  ): TSourceLines;
begin

end;

function TDebugger.FindLine(AAddress: TPhysicalAddress): String;
begin
  if not FProgramLines.TryGetValue(AAddress, Result) then Result := '<No data>'
end;

function TDebugger.FindLine(ASegment, AOffset: Word): String;
begin
  Result := FindLine((ASegment shl 4) + AOffset);
end;

function TDebugger.GetListing(AAddress: TPhysicalAddress; ABefore: Integer;
  AAfter: Integer): TSourceLines;
var
  Res: TBinarySearchResult;
  I: SizeInt;
  Line: TSourceLine;
begin
  if not specialize TArrayHelper<TPhysicalAddress>.BinarySearch(
      FOpcodeAddresses, AAddress, Res) then Exit;

  for I := Max(Res.FoundIndex - ABefore, Low(FOpcodeAddresses)) to Min(Res.FoundIndex + AAfter, High(FOpcodeAddresses)) do
  begin
    Line.Address := FOpcodeAddresses[I];
    Line.Contents := FindLine(Line.Address);
    Insert(Line, Result, Integer.MaxValue);
  end;
end;

end.

