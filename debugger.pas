unit Debugger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Generics.Collections, Process,
  Hardware;

type
  { TDebugger }

  TDebugger = class
  type
    TProgramLines = specialize TDictionary<TPhysicalAddress, String>;
  private
    FProgramLines: TProgramLines;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadProgram(AFileName: String; AAddress: TPhysicalAddress);
    function FindLine(AAddress: TPhysicalAddress): String;
    function FindLine(ASegment, AOffset: Word): String;
  end;

implementation

{ TDebugger }

constructor TDebugger.Create;
begin
 FProgramLines := TProgramLines.Create;
end;

destructor TDebugger.Destroy;
begin
 FreeAndNil(FProgramLines);
 inherited Destroy;
end;

procedure TDebugger.LoadProgram(AFileName: String; AAddress: TPhysicalAddress);
var
  Cmd, Contents, Line: String;
  SourceLines: TStringList;
  LineAddr: TPhysicalAddress;
  LineCode: String;
  LineAssembly: String;
  S: String;
begin
  Cmd := Format('/usr/bin/ndisasm -o0x%.x %s', [AAddress, AFileName]);
  Writeln(Cmd);
  RunCommand(Cmd, Contents);

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
      FProgramLines.Add(LineAddr, LineAssembly);
    except
      on E: Exception do
        Writeln('Debugger: could not parse line');
    end;
  end;
  FreeAndNil(SourceLines);
end;

function TDebugger.FindLine(AAddress: TPhysicalAddress): String;
begin
  if not FProgramLines.TryGetValue(AAddress, Result) then Result := '<No data>'
end;

function TDebugger.FindLine(ASegment, AOffset: Word): String;
begin
  Result := FindLine((ASegment shl 4) + AOffset);
end;

end.

