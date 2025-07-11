program pas8088;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, Math, StreamEx, bufstream, RayLib, RayMath,
  Cpu8088, Memory, IO, Machine, VideoController, Interrupts, Hardware, Debugger,
  Keyboard;

const
  FPS = 50;
  Cycles = 1000000 div FPS;

  BiosFile = 'poisk_1991.bin';
  CartFile = '';
  BootstrapFile = '';

  DBG = False;

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
    FDebug: Boolean;
    FFrames: UInt64;
    FNmiCounter: Integer;
    Speed: Integer;
    Keyboard: TKeyboard;
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    procedure RenderDisplay(AVideo: TVideoController);
    procedure RenderDebugger(ACpu: TCpu8088);
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
  CartRom: TRomMemoryBlock;
  TmpStream: TFileStream;
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
  Result.Video.NmiTrigger := NmiTrigger;
  Result.Video.NmiTrigger.AttachCpu(Result.Cpu);

  { Keyboard }
  Keyboard := TKeyboard.Create(Result);
  Result.IOBus.AttachDevice(Keyboard);

  //TmpStream := TFileStream.Create('BASICC11.BIN', fmOpenRead)

  if not CartFile.IsEmpty then
  begin
    CartRom := TRomMemoryBlock.Create(Result, 1024 * 64, $C0000);
    Result.InstallMemory(CartRom);
    TmpStream := TFileStream.Create(CartFile, fmOpenRead);
    CartRom.LoadFromStream(TmpStream);
    TmpStream.Free;
  end;

  Result.Initialize;

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
  FDebug := DBG;

  SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(1600, 900, 'Poisk');
  SetTargetFPS(FPS);

  Target := LoadRenderTexture(640, 400);
  SetTextureFilter(Target.texture, TEXTURE_FILTER_BILINEAR);

  Computer := BuildMachine;
  Speed := Cycles;

  Computer.Cpu.InterruptHook := @InterruptHook;
  Computer.Cpu.OnAfterInstruction := @OnInstruction;

  FDebugger.LoadProgram(BiosFile, BiosAddress);

  if not CartFile.IsEmpty then
    FDebugger.LoadProgram(CartFile, $C0000);

  ScanlineShader := LoadShader(Nil, TextFormat('scanlines.fs'));

  LinesLoc := GetShaderLocation(ScanlineShader, 'lines');
  SetShaderValue(ScanlineShader, LinesLoc, @Tmp, SHADER_UNIFORM_FLOAT);

  while not WindowShouldClose do
  begin
    Inc(FFrames);

    Keyboard[keyEsc] := IsKeyDown(KEY_ESCAPE);
    Keyboard[keyEnter] := IsKeyDown(KEY_ENTER);
    Keyboard[keyF1] := IsKeyDown(KEY_F1);
    Keyboard[keyF2] := IsKeyDown(KEY_F2);
    Keyboard[keyE] := IsKeyDown(KEY_E);

    Keyboard[keyW] := IsKeyDown(KEY_W);
    Keyboard[keyA] := IsKeyDown(KEY_A);
    Keyboard[keyS] := IsKeyDown(KEY_S);
    Keyboard[keyD] := IsKeyDown(KEY_D);

    try
      Computer.Run(Cycles);
      if (Computer.Cpu.Ticks > 2000000) then
      begin
        if Odd(FFrames) then
          Computer.Cpu.RaiseHardwareInterrupt($08)  { Todo: TIMER0 }
        else
          Computer.Cpu.RaiseHardwareInterrupt($0E);  { Todo: TIMER1 }
      end;
      //Computer.Run(Cycles div 2);
    except
      on E: Exception do
        begin
          Writeln('*** ERROR: ', E.Message, ' ***');
          Writeln(Computer.Cpu.DumpCurrentInstruction);
          Break;
        end;
    end;

    BeginTextureMode(Target);
      RenderDisplay(Computer.Video);
      DrawFPS(0, 380);
    EndTextureMode;

    BeginDrawing;
      ClearBackground(BLANK);
      BeginShaderMode(ScanlineShader);
        DrawTexturePro(
          Target.texture,
          RectangleCreate(0, 0, Target.texture.width, -Target.texture.height),
          RectangleCreate(0, 0, GetScreenHeight * 1.333, GetScreenHeight),
          Vector2Zero, 0, WHITE);
      EndShaderMode;
      RenderDebugger(Computer.Cpu);
    EndDrawing;
  end;

  UnloadShader(ScanlineShader);
  UnloadRenderTexture(Target);

  CloseWindow;

  //Writeln;
  //Computer.Cpu.Registers.Log;
  Writeln;

  FreeAndNil(Computer);
end;

procedure TApp.RenderDisplay(AVideo: TVideoController);
var
  Pixel: TColorB;
  Row, Col: Integer;
  Line: TScanLine;
begin
  ClearBackground(TColorB(AVideo.BackgroundColor or $FF000000));

  { Todo: Too slow, redo }

  for Row := 0 to High(TVideoRows) do
  begin
    Line := AVideo.ScanLines[Row];
    for Col := 0 to High(Line) do
    begin
      if Line[Col] = AVideo.BackgroundColor then Continue;

      Pixel := TColorB(Line[Col] or $FF000000);
      DrawLine(Col, Row * 2, Col, (Row * 2) + 2, Pixel);
    end;
  end;
end;

procedure TApp.RenderDebugger(ACpu: TCpu8088);
var
  Left: Integer;
  Color: TColorB;
const
  FontSize = 48;
  VertSpacing = 12;
begin
  Left := GetScreenWidth - 280;
  Color := ColorAlpha(YELLOW, 0.7);

  DrawText(
    PChar(Format('%.4x:%.4x %s',
      [
        ACpu.Registers.CS, ACpu.Registers.IP,
        FDebugger.FindLine(ACpu.Registers.CS, ACpu.Registers.IP)
      ]
    )),
    Left - 480, 0 * (FontSize + VertSpacing) + 0, FontSize, Color);

  DrawText(
    PChar(Format('AX: %.4x', [ACpu.Registers.AX])),
    Left, 1 * (FontSize + VertSpacing) + 30, FontSize, Color);
  DrawText(
    PChar(Format('BX: %.4x', [ACpu.Registers.BX])),
    Left, 2 * (FontSize + VertSpacing) + 30, FontSize, Color);
  DrawText(
    PChar(Format('CX: %.4x', [ACpu.Registers.CX])),
    Left, 3 * (FontSize + VertSpacing) + 30, FontSize, Color);
  DrawText(
    PChar(Format('DX: %.4x', [ACpu.Registers.DX])),
    Left, 4 * (FontSize + VertSpacing) + 30, FontSize, Color);

  DrawText(
    PChar(Format('BP: %.4x', [ACpu.Registers.BP])),
    Left, 5 * (FontSize + VertSpacing) + 60, FontSize, Color);
  DrawText(
    PChar(Format('SP: %.4x', [ACpu.Registers.SP])),
    Left, 6 * (FontSize + VertSpacing) + 60, FontSize, Color);
  DrawText(
    PChar(Format('SI: %.4x', [ACpu.Registers.SI])),
    Left, 7 * (FontSize + VertSpacing) + 60, FontSize, Color);
  DrawText(
    PChar(Format('DI: %.4x', [ACpu.Registers.DI])),
    Left, 8 * (FontSize + VertSpacing) + 60, FontSize, Color);

  DrawText(
    PChar(Format('CS: %.4x', [ACpu.Registers.CS])),
    Left, 9 * (FontSize + VertSpacing) + 90, FontSize, Color);
  DrawText(
    PChar(Format('DS: %.4x', [ACpu.Registers.DS])),
    Left, 10 * (FontSize + VertSpacing) + 90, FontSize, Color);
  DrawText(
    PChar(Format('ES: %.4x', [ACpu.Registers.ES])),
    Left, 11 * (FontSize + VertSpacing) + 90, FontSize, Color);
  DrawText(
    PChar(Format('SS: %.4x', [ACpu.Registers.SS])),
    Left, 12 * (FontSize + VertSpacing) + 90, FontSize, Color);

  DrawText(
    PChar(Format('NMIs: %d', [FNmiCounter])),
    Left - 64, 13 * (FontSize + VertSpacing) + 120, FontSize, Color);
end;

function TApp.InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
var
  Cpu: TCpu8088 absolute ASender;
  Message: String = '';
  TestProgram: TMemoryStream;
  I: Integer;
begin
  //Writeln(Format('INT %.x', [ANumber]));

  Result := False;
  case ANumber of
    $02:
      begin
        Inc(FNmiCounter);
      end;
    $10:
      begin
        //if Cpu.Registers.AH = $0E then Writeln('Prints');
      end;

    $19:
      if not BootstrapFile.IsEmpty then
      begin
        TestProgram := TMemoryStream.Create;
        TestProgram.LoadFromFile(BootstrapFile);
        //TestProgram.LoadFromFile('test.bin');
        I := 0;
        while TestProgram.Position < TestProgram.Size do
        begin
          Cpu.MemoryBus.InvokeWrite(Cpu, $600 + I, TestProgram.ReadByte);
          Inc(I);
        end;
        Cpu.Registers.CS := $60;
        Cpu.Registers.DS := $60;
        Cpu.Registers.ES := $60;
        Cpu.Registers.SS := $60;
        Cpu.Registers.IP := $0;
        Cpu.Registers.SP := $FFFE;
        TestProgram.Free;
        //Cpu.MemoryBus.InvokeWrite(Nil, $41C, $20);
        Result := True
      end;
  end;

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

  //if (AInstruction.CS = $C000) then FDebug := True;

  //FDebug := AInstruction.CS = $60;

  if FDebug then
    WriteLn(Format('%.4x:%.4x | %-24s | %s ',
    [
      AInstruction.CS, AInstruction.IP,
      FDebugger.FindLine(AInstruction.CS, AInstruction.IP),
      TCpu8088(ASender).DumpCurrentInstruction
    ]));

end;

var
  App: TApp;

begin
  SetExceptionMask(GetExceptionMask + [exInvalidOp, exOverflow, exZeroDivide]);

  App := TApp.Create;
  App.Run;
  App.Free;
end.

