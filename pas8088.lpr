program pas8088;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, Math, StreamEx, bufstream,
  RayLib, RayMath,
  Cpu8088, Memory, IO, Machine, VideoController, Interrups, Hardware, Debugger;

const
  FPS = 25;
  Cycles = 750000 div FPS;
  BiosFile = 'poisk_1991.bin';
  //BiosFile = 'test.bin';

  RamAddress   = $00000;
  BiosAddress  = $FE000;
  VideoAddress = $B8000;

type
  TDumpFrame = record
    PhysicalAddress: UInt32;
    CodeLenth: Byte;
    CodeBytes: array[0..5] of Byte;
    AX, BX, CX, DX, BP, SP, SI, DI,
    CS, DS, SS, ES, IP, Flags: Word;
  end;

  { TApp }

  TApp = class
  public
    Target: TRenderTexture;
    FDebugger: TDebugger;
    FLogWriter: TStreamWriter;
    FDumpStream: TStream;
    Speed: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    procedure RenderDisplay(AVideo: TVideoController);
    function BuildMachine: TMachine;
    function InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
    procedure OnInstruction(ASender: TObject; AInstruction: TInstruction);
  end;

function TApp.BuildMachine: TMachine;
var
  BiosRom: TRomMemoryBlock;
  BiosStream: TStream;
  Ram, VideoRam: TRamMemoryBlock;
  NmiTrigger: TNmiTrigger;
begin
  Result := TMachine.Create(Nil);

  { Cpu }
  Result.InstallCpu(TCpu8088.Create(Result));

  { I/O }
  Result.InstallIOBus(TIOBus.Create(Result));

  { RAM / ROM }
  Result.InstallMemoryBus(TMemoryBus.Create(Result));

  Ram := TRamMemoryBlock.Create(Result, 1024 * 608, RamAddress);
  Result.InstallMemory(Ram);

  BiosRom := TRomMemoryBlock.Create(Result, 1024 * 8, BiosAddress);
  Result.InstallMemory(BiosRom);

  VideoRam := TRamMemoryBlock.Create(Result, 1024 * 32, VideoAddress);
  Result.InstallMemory(VideoRam);

  { Video }
  Result.InstallVideo(TVideoController.Create(Result));
  NmiTrigger := TNmiTrigger.Create(Result);
  Result.Video.NmiGate := NmiTrigger;
  Result.Video.NmiGate.AttachCpu(Result.Cpu);
  Result.IOBus.AttachDevice(NmiTrigger);

  Result.Initialize;

  //BiosStream := TFileStream.Create('poisk_1991.bin', fmOpenRead);
  BiosStream := TFileStream.Create(BiosFile, fmOpenRead);
  try
    BiosRom.LoadFromStream(BiosStream);
  finally
    FreeAndNil(BiosStream);
  end;
end;

constructor TApp.Create;
begin
  FDebugger := TDebugger.Create;
  FLogWriter := TStreamWriter.Create('/tmp/comp.log', False, TEncoding.UTF8, 16384);
  FDumpStream := TWriteBufStream.Create(
    TFileStream.Create('/tmp/comp.dump', fmCreate));
  TWriteBufStream(FDumpStream).SourceOwner := True;
end;

destructor TApp.Destroy;
begin
  FreeAndNil(FDebugger);
  FreeAndNil(FLogWriter);
  FreeAndNil(FDumpStream);
  inherited Destroy;
end;

procedure TApp.Run;
var
  Computer: TMachine;
  ScanlineShader: TShader;
  LinesLoc: Integer;
  Tmp: Single = 400.0;

begin
  SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(800, 600, 'Poisk');
  SetTargetFPS(FPS);

  Target := LoadRenderTexture(640, 400);
  SetTextureFilter(Target.texture, TEXTURE_FILTER_BILINEAR);

  Computer := BuildMachine;
  Speed := Cycles;

  Computer.Cpu.InterruptHook := @InterruptHook;
  Computer.Cpu.OnAfterInstruction := @OnInstruction;

  FDebugger.LoadProgram(BiosFile, BiosAddress);

  ScanlineShader := LoadShader(Nil, TextFormat('scanlines.fs'));

  LinesLoc := GetShaderLocation(ScanlineShader, 'lines');
  SetShaderValue(ScanlineShader, LinesLoc, @Tmp, SHADER_UNIFORM_FLOAT);

  while not WindowShouldClose do
  begin
    if IsKeyPressed(KEY_F) then ToggleBorderlessWindowed;

    try
      Computer.Run(Cycles);
      Computer.Cpu.RaiseHardwareInterrupt(8);  { Todo: invoke by TIMER0 }
    except
      on E: Exception do
        begin
          Writeln('*** ERROR: ', E.Message, ' ***');
          Computer.Cpu.DumpCurrentInstruction;
          break;
        end;
    end;

    BeginTextureMode(Target);
      { ClearBackground(BLANK); }
      RenderDisplay(Computer.Video);
      DrawFPS(550, 5);
    EndTextureMode;

    BeginDrawing;
      BeginShaderMode(ScanlineShader);
        DrawTexturePro(
          Target.texture,
          RectangleCreate(0, 0, Target.texture.width, -Target.texture.height),
          RectangleCreate(0, 0, GetScreenHeight * 1.333, GetScreenHeight),
          Vector2Zero, 0, WHITE);
      EndShaderMode;
    EndDrawing;
  end;

  UnloadShader(ScanlineShader);
  UnloadRenderTexture(Target);

  CloseWindow;

  Writeln;
  Computer.Cpu.Registers.Log;
  Writeln;

  FreeAndNil(Computer);
end;

procedure TApp.RenderDisplay(AVideo: TVideoController);
var
  Pixel: TColorB;
  Row, Col: Integer;
  Line: TScanLine;
begin
  ClearBackground(BLACK);

  { Todo: Too slow, redo }

  for Row := 0 to High(TVideoRows) do
  begin
    Line := AVideo.ScanLines[Row];
    for Col := 0 to High(Line) do
    begin
      Pixel := TColorB(Line[Col] or $FF000000);
      DrawLine(Col, Row * 2, Col, (Row * 2) + 2, Pixel);
    end;
  end;
end;

function TApp.InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
var
  Cpu: TCpu8088 absolute ASender;
  Message: String = '';
  TestProgram: TBytesStream;
  I: Integer;
begin
  //Writeln(Format('INT %.x', [ANumber]));
  Result := False;
end;

procedure TApp.OnInstruction(ASender: TObject; AInstruction: TInstruction);
var
  Line: String;
  Data: Byte;
  Frame: TDumpFrame;
  Cpu: TCpu8088 absolute ASender;
begin
  Frame.PhysicalAddress := (AInstruction.CS shl 4) and AInstruction.IP;
  Frame.CodeLenth := AInstruction.Length;
  Frame.CodeBytes := AInstruction.Code;

  Frame.AX := Cpu.Registers.AX;
  Frame.BX := Cpu.Registers.BX;
  Frame.CX := Cpu.Registers.CX;
  Frame.DX := Cpu.Registers.DX;
  Frame.BP := Cpu.Registers.BP;
  Frame.SP := Cpu.Registers.SP;
  Frame.SI := Cpu.Registers.SI;
  Frame.DI := Cpu.Registers.DI;
  Frame.CS := Cpu.Registers.CS;
  Frame.DS := Cpu.Registers.DS;
  Frame.SS := Cpu.Registers.SS;
  Frame.ES := Cpu.Registers.ES;
  Frame.IP := Cpu.Registers.IP;
  Frame.Flags := Cpu.Registers.Flags.GetWord;

  //FDumpStream.Write(Frame, SizeOf(Frame));

{
  FLogWriter.WriteLine('%.4x:%.4x | %s',
    [
      AInstruction.CS, AInstruction.IP,
      FDebugger.FindLine(AInstruction.CS, AInstruction.IP)
    ]);
}
end;

var
  App: TApp;

begin
  SetExceptionMask(GetExceptionMask + [exInvalidOp, exOverflow, exZeroDivide]);

  App := TApp.Create;
  App.Run;
  App.Free;
end.

