program pas8088;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, Math, IConvEnc,
  RayLib, RayMath,
  Cpu8088, Memory, IO, Machine;

type

  { TApp }

  TApp = class
  public
    Target: TRenderTexture;
    procedure Run;
    function BuildMachine: TMachine;
    function InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
    procedure OnInstruction(ASender: TObject; AInstruction: TInstruction);
  end;

function TApp.BuildMachine: TMachine;
var
  MemoryBus: TMemoryBus;
  BiosRom: TRomMemoryBlock;
  BiosStream: TStream;
  Ram, VideoRam: TRamMemoryBlock;
  TestProgramStream: TStream;
  IOBus: TIOBus;
  Cpu: TCpu8088;
begin
  Result := TMachine.Create(Nil);

  { I/O }
  IOBus := TIOBus.Create(Result);
  Result.InstallIOBus(IOBus);

  { RAM / ROM }
  MemoryBus := TMemoryBus.Create(Result);

  Ram := TRamMemoryBlock.Create(MemoryBus, 1024 * 128, $0);
  MemoryBus.AttachDevice(Ram);

  BiosRom := TRomMemoryBlock.Create(MemoryBus, 1024 * 8, $FE000);
  MemoryBus.AttachDevice(BiosRom);

  { Video }
  VideoRam := TRamMemoryBlock.Create(MemoryBus, 1024 * 32, $BF000);
  MemoryBus.AttachDevice(VideoRam);

  Result.InstallMemoryBus(MemoryBus);

  { Cpu }
  Cpu := TCpu8088.Create(Result);
  Result.InstallCpu(Cpu);
  IOBus.AttachDevice(Cpu);
  MemoryBus.AttachDevice(Cpu);

  Result.Initialize;

  BiosStream := TBytesStream.Create([
    $EA, $00, $00, $60, $00   { jmp 0x0060:0x0000 }
  ]);

  try
    BiosRom.LoadFromStream(BiosStream, $2000 - $10);
  finally
    FreeAndNil(BiosStream);
  end;

  { Test program }
  TestProgramStream := TFileStream.Create('test.bin', fmOpenRead);
  try
    Ram.LoadFromStream(TestProgramStream, $00600);
  finally
    FreeAndNil(TestProgramStream);
  end;
end;

procedure TApp.Run;
var
  Computer: TMachine;

begin
  SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(800, 600, 'TEST');
  SetTargetFPS(50);

  Target := LoadRenderTexture(640, 400);

  Computer := BuildMachine;

  Computer.Cpu.InterruptHook := @InterruptHook;
  Computer.Cpu.OnAfterInstruction := @OnInstruction;

  while not WindowShouldClose do
  begin
    try
      Computer.Run(10000);
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

      { Todo: Render display }

    EndTextureMode;

    BeginDrawing;
      DrawTexturePro(
        Target.texture,
        RectangleCreate(0, 0, 640, -400),
        RectangleCreate(0, 0, GetScreenWidth, GetScreenHeight),
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

function TApp.InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
var
  Cpu: TCpu8088 absolute ASender;
  Message: String = '';
begin
  case ANumber of
    $10:
      begin
        case Cpu.Registers.AH of
          $0E: { teletype }
            begin
              Iconvert(Char(Cpu.Registers.AL), Message, 'cp866', 'utf-8');
              Write(Message);
            end;
        end;
      end;
  end;
  Result := False;
end;

procedure TApp.OnInstruction(ASender: TObject; AInstruction: TInstruction);
begin
  //(ASender as TCpu8088).DumpCurrentInstruction;
end;

var
  App: TApp;

begin
  SetExceptionMask(GetExceptionMask + [exInvalidOp, exOverflow, exZeroDivide]);

  App := TApp.Create;
  App.Run;
  App.Free;
end.

