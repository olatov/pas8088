program pas8088;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, Math, IConvEnc,
  RayLib, RayMath,
  Cpu8088, Memory, IO, Machine, VideoController, Interrups, Hardware;

const
  FPS = 50;
  Cycles = 10000;

type

  { TApp }

  TApp = class
  public
    Target: TRenderTexture;
    Speed: Integer;
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

  Ram := TRamMemoryBlock.Create(Result, 1024 * 608, $0);
  Result.InstallMemory(Ram);

  BiosRom := TRomMemoryBlock.Create(Result, 1024 * 8, $FE000);
  Result.InstallMemory(BiosRom);

  VideoRam := TRamMemoryBlock.Create(Result, 1024 * 32, $B8000);
  Result.InstallMemory(VideoRam);

  { Video }
  Result.InstallVideo(TVideoController.Create(Result));
  NmiTrigger := TNmiTrigger.Create(Result);
  Result.Video.NmiGate := NmiTrigger;
  Result.Video.NmiGate.AttachCpu(Result.Cpu);
  Result.IOBus.AttachDevice(NmiTrigger);

  Result.Initialize;

  BiosStream := TFileStream.Create('poisk_1991.bin', fmOpenRead);
  try
    BiosRom.LoadFromStream(BiosStream);
  finally
    FreeAndNil(BiosStream);
  end;
end;

procedure TApp.Run;
var
  Computer: TMachine;

begin
  SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(800, 600, 'Poisk');
  SetTargetFPS(FPS);

  Target := LoadRenderTexture(640, 400);

  Computer := BuildMachine;
  Speed := Cycles;

  Computer.Cpu.InterruptHook := @InterruptHook;
  Computer.Cpu.OnAfterInstruction := @OnInstruction;

  while not WindowShouldClose do
  begin
    if IsKeyPressed(KEY_F) then ToggleBorderlessWindowed;

    try
      Computer.Run(Cycles);
      Computer.Cpu.RaiseHardwareInterrupt(8);
    except
      on E: Exception do
        begin
          Writeln('*** ERROR: ', E.Message, ' ***');
          Computer.Cpu.DumpCurrentInstruction;
          break;
        end;
    end;

    BeginTextureMode(Target);
      ClearBackground(BLANK);

      RenderDisplay(Computer.Video);

    EndTextureMode;

    BeginDrawing;
      DrawTexturePro(
        Target.texture,
        RectangleCreate(0, 0, 640, -400),
        RectangleCreate(0, 0, GetScreenHeight * 1.333, GetScreenHeight),
        Vector2Zero, 0, WHITE);
    EndDrawing;
  end;

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
      DrawLine(Col, Row * 2, Col + 1, (Row * 2) + 3, Pixel);
      //DrawLine(Col, (Row * 2) + 1, Col + 1, (Row * 2) + 2, ColorBrightness(Pixel, -0.15));
    end;
  end;

  DrawFPS(550, 5);
end;

function TApp.InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
var
  Cpu: TCpu8088 absolute ASender;
  Message: String = '';
  TestProgram: TBytesStream;
  I: Integer;
begin
  //Writeln(Format('INT %.x', [ANumber]));
  case ANumber of
    $10:
      begin
        case Cpu.Registers.AH of
          $0E: { teletype }
            begin
              Iconvert(Char(Cpu.Registers.AL), Message, 'cp866', 'utf-8');
              //Write(Message);
            end;
        end;
      end;
    $19:
      begin
        TestProgram := TBytesStream.Create;
        TestProgram.LoadFromFile('test.bin');
        for I := 0 To TestProgram.Size - 1 do
          Cpu.MemoryBus.InvokeWrite(
            Nil, $10000 + I, TestProgram.ReadByte);
        TestProgram.Free;

        Cpu.Registers.CS := $1000;
        Cpu.Registers.IP := $0000;
        Speed := 1;
        Result := True;
      end;
  end;
  Result := False;
end;

procedure TApp.OnInstruction(ASender: TObject; AInstruction: TInstruction);
begin
  if Speed < 100 then
    (ASender as TCpu8088).DumpCurrentInstruction;
end;

var
  App: TApp;

begin
  SetExceptionMask(GetExceptionMask + [exInvalidOp, exOverflow, exZeroDivide]);

  App := TApp.Create;
  App.Run;
  App.Free;
end.

