program pas8088;

{$mode objfpc}{$H+}

{$ifdef darwin}
  {$linklib raylib}
{$endif}
{$R-}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, Math, StreamEx, bufstream, Generics.Collections,
  RayLib, RayMath,
  Cpu8088, Memory, IO, Machine, VideoController, Interrupts, Hardware, Debugger,
  Keyboard, Dump;

const
  ClockSpeed = 1000 * 500;
  FPS = 50;
  CyclesPerFrame = ClockSpeed div FPS;

  DBG = False;

  FontFile = 'LiberationMono-Regular.ttf';

  RamAddress   = $00000;
  BiosAddress  = $FE000;
  VideoAddress = $B8000;

var
  BiosFile: String = 'poisk_1991.rom';
  CartFile: String = 'basicc11.cart';
  TapeFile: String = 'cassette.img';
  BootstrapFile: String = '';
  DumpFile: String = '';

  Breakpoints: array of TPhysicalAddress = ($00000);

type
  { TApp }

  TApp = class
  private
    procedure DumpMemory(AFileName: String; AMemoryBus: IMemoryBus;
      AAddress: TPhysicalAddress; ALength: Integer);
    procedure OnBeforeExecution(ASender: TObject; AInstruction: TInstruction);
  public
    type
      TKeyboardMap = specialize TDictionary<TKeyboard.TKey, specialize TArray<RayLib.TKeyboardKey>>;
  public
    Target: TRenderTexture;
    FFont: TFont;
    FDebugger: TDebugger;
    FLogWriter: TStreamWriter;
    FDumpStream: TStream;
    FDebug: Boolean;
    FStepBreakpoint: TPhysicalAddress;
    FFrames: UInt64;
    FNmiCounter: Integer;
    FStepByStep: Boolean;
    FKeybEnabled: Boolean;
    FKeyboardMap: TKeyboardMap;
    FKeyboard: TKeyboard;
    FTapeStream: TStream;
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    procedure RenderDisplay(AVideo: TVideoController);
    procedure RenderDebugger(ACpu: TCpu8088);
    procedure BuildKeyboardMap;
    function HandleBootstrap(const Cpu: TCpu8088): Boolean;
    procedure HandleLoadFromTape(ACpu: TCpu8088);
    procedure UpdateKeyboard(AKeyboard: TKeyboard);
    function BuildMachine: TMachine;
    function InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
    function OnBeforeInstruction(ASender: TObject; AAddress: TPhysicalAddress
      ): Boolean;
    procedure OnAfterInstruction(ASender: TObject; AInstruction: TInstruction);
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
  Result.InstallVideo(TVideoController.Create(Result, CyclesPerFrame));
  NmiTrigger := TNmiTrigger.Create(Result);
  Result.Video.NmiTrigger := NmiTrigger;
  Result.Video.NmiTrigger.AttachCpu(Result.Cpu);

  { FKeyboard }
  FKeyboard := TKeyboard.Create(Result);
  Result.IOBus.AttachDevice(FKeyboard);

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

  if not DumpFile.IsEmpty then
  begin
    FDumpStream := TWriteBufStream.Create(
      TFileStream.Create(DumpFile, fmCreate));
    TWriteBufStream(FDumpStream).SourceOwner := True;
  end;

  if not TapeFile.IsEmpty then
    FTapeStream := TFileStream.Create(TapeFile, fmOpenRead);

  FKeyboardMap := TKeyboardMap.Create;
  BuildKeyboardMap;
end;

destructor TApp.Destroy;
begin
  FreeAndNil(FDebugger);
  FreeAndNil(FLogWriter);
  FreeAndNil(FDumpStream);
  FreeAndNil(FTapeStream);
  FreeAndNil(FKeyboardMap);
  inherited Destroy;
end;

procedure TApp.Run;
var
  Computer: TMachine;
  ScanlineShader: TShader;
  LinesLoc, I: Integer;
  Tmp: Single = 400.0;
  Listing: array of TDebugger.TSourceLine;
  PrevTicks: QWord = 0;

  procedure LoadBytesToDebugger(AMemoryBus: IMemoryBus; AAddress: TPhysicalAddress);
  var
    Data: Byte;
    Code: TBytes = ();
    I: Integer;
  begin
    SetLength(Code, 64);
    for I := 0 to High(Code) do
    begin
      AMemoryBus.InvokeRead(Nil, AAddress + I - 8, Data);
      Code[I] := Data;
    end;
    FDebugger.Clear;
    FDebugger.LoadBytes(Code, AAddress - 8, AAddress);
  end;

begin
  FDebug := DBG;

  if ParamCount >= 1 then BootstrapFile := ParamStr(1);

  SetConfigFlags(FLAG_WINDOW_RESIZABLE);
  InitWindow(800, 600, 'Poisk');
  SetTargetFPS(FPS);
  SetExitKey(KEY_NULL);

  FFont := LoadFont(FontFile);

  Target := LoadRenderTexture(640, 400);
  SetTextureFilter(Target.texture, TEXTURE_FILTER_BILINEAR);

  Computer := BuildMachine;

  Computer.Cpu.InterruptHook := @InterruptHook;
  Computer.Cpu.OnBeforeInstruction := @OnBeforeInstruction;
  Computer.Cpu.OnAfterInstruction := @OnAfterInstruction;
  Computer.Cpu.OnBeforeExecution := @OnBeforeExecution;

  ScanlineShader := LoadShader(Nil, TextFormat('scanlines.fs'));

  LinesLoc := GetShaderLocation(ScanlineShader, 'lines');
  SetShaderValue(ScanlineShader, LinesLoc, @Tmp, SHADER_UNIFORM_FLOAT);

  FKeybEnabled := True;

  while not WindowShouldClose do
  begin
    if IsKeyPressed(KEY_F12) then FStepByStep := True;

    Inc(FFrames);

    UpdateKeyboard(FKeyboard);

    if FStepByStep and (PrevTicks <> Computer.Cpu.Ticks) then
      LoadBytesToDebugger(Computer.MemoryBus, Computer.Cpu.CurrentAddress);

    PrevTicks := Computer.Cpu.Ticks;

    Computer.Video.BeginFrame;

    try
      if (not FStepByStep) or (Computer.Cpu.Registers.CS >= $F000) then
      begin
        for I := 1 to CyclesPerFrame do
        begin
          //if Computer.Cpu.Ticks = Trunc(ClockSpeed * 2) then FKeyboard[KeyF2] := True;
          //if Computer.Cpu.Ticks = Trunc(ClockSpeed * 2.2) then FKeyboard[KeyF2] := False;

          Computer.Run(1);
          if FStepByStep then Break;
        end;

        if (FFrames > 20) then
        begin
          if (Computer.Cpu.Ticks mod 5000) = 0 then
            if not Odd(FFrames) then
              Computer.Cpu.RaiseHardwareInterrupt($08)  { Todo: TIMER0 }
            else
            if FKeybEnabled then
              Computer.Cpu.RaiseHardwareInterrupt($0E);  { Todo: TIMER1 }
        end;

      end else
      begin
        if IsKeyPressed(KEY_F7) or IsKeyPressedRepeat(KEY_F7) then
        begin
          Computer.Run(1); { Step into }
        end;

        if IsKeyPressed(KEY_F8) or IsKeyPressedRepeat(KEY_F8) then
        begin
          { Step over }
          Listing := FDebugger.GetListing(Computer.Cpu.CurrentAddress, 0, 1);
          if Length(Listing) > 0 then
            FStepBreakpoint := Listing[High(Listing)].Address;
          FStepByStep := False;
          Computer.Run(1);
        end;
        if IsKeyPressed(KEY_F9) then
        begin
          { Run }
          FStepByStep := False;
          Computer.Run(1);
        end;
      end;

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
      //DrawFPS(0, 380);
    EndTextureMode;

    BeginDrawing;
      ClearBackground(BLANK);
      BeginShaderMode(ScanlineShader);
        DrawTexturePro(
          Target.texture,
          RectangleCreate(0, 0, Target.texture.width, -Target.texture.height),
          RectangleCreate(0, 0, GetScreenHeight * 1.333, GetScreenHeight),
          { RectangleCreate(64, 64, GetScreenHeight * 1.333 * 0.85, GetScreenHeight * 0.85), }
          Vector2Zero, 0, WHITE);
      EndShaderMode;
      DrawRectangleLinesEx(
        RectangleCreate(0, 0, GetScreenHeight * 1.333, GetScreenHeight), 1, DARKGRAY);

      if FStepByStep then
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

procedure TApp.DumpMemory(AFileName: String;
  AMemoryBus: IMemoryBus; AAddress: TPhysicalAddress; ALength: Integer);
var
  Data: Byte;
  Stream: TMemoryStream;
  I: TPhysicalAddress;
begin
  Stream := TMemoryStream.Create;
  for I := AAddress to AAddress + ALength do
  begin
    AMemoryBus.InvokeRead(Nil, I, Data);
    Stream.WriteByte(Data);
  end;
  Stream.SaveToFile(AFileName);
  Stream.Free;
end;

function TApp.HandleBootstrap(const Cpu: TCpu8088): Boolean;
var
  I: Integer;
  TestProgram: TMemoryStream;
begin
  Result := False;
  if BootstrapFile.IsEmpty then Exit;

  TestProgram := TMemoryStream.Create;
  TestProgram.LoadFromFile(BootstrapFile);
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
  Result := True
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
  Listing: array of TDebugger.TSourceLine;
  Line: TDebugger.TSourceLine;
  I: Integer;
  Addr: TPhysicalAddress;
  DataByte: Byte;
  DataWord: Word;
const
  FontSize = 48;
  CodeFontSize = 48;
  VertSpacing = 1;
begin
  Left := GetScreenWidth - 360;
  Color := ColorAlpha(YELLOW, 0.9);

  DrawTextEx(
    FFont,
    PChar(Format('%.4x:%.4x %s',
      [
        ACpu.Registers.CS, ACpu.Registers.IP,
        FDebugger.FindLine(ACpu.CurrentAddress)
      ]
    )),
    Vector2Create(Left - 800, 0 * (FontSize + VertSpacing) + 0), CodeFontSize, 1, Color);

  DrawTextEx(
    FFont,
    PChar(Format('AX %.4x', [ACpu.Registers.AX])),
    Vector2Create(Left, 0 * (FontSize + VertSpacing) + 0), FontSize, 1, Color);
  DrawTextEx(
    FFont,
    PChar(Format('BX %.4x', [ACpu.Registers.BX])),
    Vector2Create(Left, 1 * (FontSize + VertSpacing) + 0), FontSize, 1, Color);
  DrawTextEx(
    FFont,
    PChar(Format('CX %.4x', [ACpu.Registers.CX])),
    Vector2Create(Left, 2 * (FontSize + VertSpacing) + 0), FontSize, 1, Color);
  DrawTextEx(
    FFont,
    PChar(Format('DX %.4x', [ACpu.Registers.DX])),
    Vector2Create(Left, 3 * (FontSize + VertSpacing) + 0), FontSize, 1, Color);

  DrawTextEx(
    FFont,
    PChar(Format('BP %.4x', [ACpu.Registers.BP])),
    Vector2Create(Left, 4 * (FontSize + VertSpacing) + 48), FontSize, 1, Color);
  DrawTextEx(
    FFont,
    PChar(Format('SP %.4x', [ACpu.Registers.SP])),
    Vector2Create(Left, 5 * (FontSize + VertSpacing) + 48), FontSize, 1, Color);
  DrawTextEx(
    FFont,
    PChar(Format('SI %.4x', [ACpu.Registers.SI])),
    Vector2Create(Left, 6 * (FontSize + VertSpacing) + 48), FontSize, 1, Color);
  DrawTextEx(
    FFont,
    PChar(Format('DI %.4x', [ACpu.Registers.DI])),
    Vector2Create(Left, 7 * (FontSize + VertSpacing) + 48), FontSize, 1, Color);

  DrawTextEx(
    FFont,
    PChar(Format('CS %.4x', [ACpu.Registers.CS])),
    Vector2Create(Left, 8 * (FontSize + VertSpacing) + 96), FontSize, 1, Color);
  DrawTextEx(
    FFont,
    PChar(Format('DS %.4x', [ACpu.Registers.DS])),
    Vector2Create(Left, 9 * (FontSize + VertSpacing) + 96), FontSize, 1, Color);
  DrawTextEx(
    FFont,
    PChar(Format('ES %.4x', [ACpu.Registers.ES])),
    Vector2Create(Left, 10 * (FontSize + VertSpacing) + 96), FontSize, 1, Color);
  DrawTextEx(
    FFont,
    PChar(Format('SS %.4x', [ACpu.Registers.SS])),
    Vector2Create(Left, 11 * (FontSize + VertSpacing) + 96), FontSize, 1, Color);

  DrawTextEx(
    FFont,
    PChar(Format('FL %.4x', [ACpu.Registers.Flags.GetWord])),
    Vector2Create(Left, 12 * (FontSize + VertSpacing) + 128), FontSize, 1, Color);

  for I := 0 to 12 do
  begin
    Addr := (ACpu.Registers.SS shl 4) + Word(ACpu.Registers.SP + (2 * I));
    ACpu.MemoryBus.InvokeRead(Nil, Addr, DataByte);
    DataWord := DataByte;
    Addr := (ACpu.Registers.SS shl 4) + Word(ACpu.Registers.SP + (2 * I) + 1);
    ACpu.MemoryBus.InvokeRead(Nil, Addr, DataByte);
    DataWord := DataWord or (DataByte shl 8);

    DrawTextEx(
      FFont,
      PChar(Format('ST(%d) %.4x', [I, DataWord])),
      Vector2Create(Left, (11 + I) * (FontSize + VertSpacing) + 240), FontSize, 1, Color);
  end;


  DrawTextEx(
    FFont,
    PChar(Format('C(%d)', [IfThen(ACpu.Registers.Flags.CF, 1, 0)])),
    Vector2Create(Left + 240, 0 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.CF, YELLOW, GRAY));

  DrawTextEx(
    FFont,
    PChar(Format('Z(%d)', [IfThen(ACpu.Registers.Flags.ZF, 1, 0)])),
    Vector2Create(Left + 240, 1 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.ZF, YELLOW, GRAY));

  DrawTextEx(
    FFont,
    PChar(Format('S(%d)', [IfThen(ACpu.Registers.Flags.SF, 1, 0)])),
    Vector2Create(Left + 240, 2 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.SF, YELLOW, GRAY));

  DrawTextEx(
    FFont,
    PChar(Format('O(%d)', [IfThen(ACpu.Registers.Flags.OF_, 1, 0)])),
    Vector2Create(Left + 240, 3 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.OF_, YELLOW, GRAY));

  DrawTextEx(
    FFont,
    PChar(Format('P(%d)', [IfThen(ACpu.Registers.Flags.PF, 1, 0)])),
    Vector2Create(Left + 240, 4 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.PF, YELLOW, GRAY));

  DrawTextEx(
    FFont,
    PChar(Format('A(%d)', [IfThen(ACpu.Registers.Flags.AF, 1, 0)])),
    Vector2Create(Left + 240, 5 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.AF, YELLOW, GRAY));

  DrawTextEx(
    FFont,
    PChar(Format('D(%d)', [IfThen(ACpu.Registers.Flags.DF, 1, 0)])),
    Vector2Create(Left + 240, 6 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.DF, YELLOW, GRAY));

  DrawTextEx(
    FFont,
    PChar(Format('I(%d)', [IfThen(ACpu.Registers.Flags.IF_, 1, 0)])),
    Vector2Create(Left + 240, 7 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.IF_, YELLOW, GRAY));

  DrawTextEx(
    FFont,
    PChar(Format('T(%d)', [IfThen(ACpu.Registers.Flags.TF, 1, 0)])),
    Vector2Create(Left + 240, 8 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.TF, YELLOW, GRAY));

  if FStepByStep then
  begin
    { DrawText('[STEP]', Left - 480, 70, FontSize, RED); }

    Addr := ACpu.CurrentAddress;
    //DrawTextEx(FFont, PChar(Format('%.5x', [Addr])), Vector2Create(0, 64) , 72, 1, WHITE);

    Listing := FDebugger.GetListing(Addr, 6, 30);
    for I := 0 to Min(25, High(Listing)) do
      DrawTextEx(
        FFont,
        PChar(Format('%.5x | %s', [Listing[I].Address, Listing[I].Contents])),
        Vector2Create(Left - 800, I * (FontSize + 8) + 120), CodeFontSize, 1,
        specialize IfThen<TColorB>(Listing[I].Address = Addr, YELLOW, GRAY));
  end;
end;

procedure TApp.BuildKeyboardMap;
begin
  with FKeyboardMap do
  begin
    Clear;
    Add(keyEnter, [KEY_ENTER, KEY_KP_ENTER]);
    Add(keySpace, [KEY_SPACE]);
    Add(keyEsc, [KEY_ESCAPE]);
    Add(keyBackspace, [KEY_BACKSPACE]);
    Add(keyLeftShift, [KEY_LEFT_SHIFT]);
    Add(keyRightShift, [KEY_RIGHT_SHIFT, KEY_KP_ADD]);
    Add(keyLeftControl, [KEY_LEFT_CONTROL]);
    Add(keyRightControl, [KEY_RIGHT_CONTROL]);
    Add(keyLeftAlt, [KEY_LEFT_ALT]);
    Add(keyRightAlt, [KEY_RIGHT_ALT]);
    Add(keyF1, [KEY_F1]);
    Add(keyF2, [KEY_F2]);
    Add(keyF3, [KEY_F3]);
    Add(keyF4, [KEY_F4]);
    Add(keyF5, [KEY_F5]);
    Add(keyF6, [KEY_F6]);
    Add(keyF7, [KEY_F7]);
    Add(keyF8, [KEY_F8]);
    Add(keyF9, [KEY_F9]);
    Add(keyF10, [KEY_F10]);
    Add(keyD1, [KEY_ONE]);
    Add(keyD2, [KEY_TWO]);
    Add(keyD3, [KEY_THREE]);
    Add(keyD4, [KEY_FOUR]);
    Add(keyD5, [KEY_FIVE]);
    Add(keyD6, [KEY_SIX]);
    Add(keyD7, [KEY_SEVEN]);
    Add(keyD8, [KEY_EIGHT, KEY_KP_ADD]);
    Add(keyD9, [KEY_NINE]);
    Add(keyD0, [KEY_ZERO]);
    Add(keyA, [KEY_A]);
    Add(keyB, [KEY_B]);
    Add(keyC, [KEY_C]);
    Add(keyD, [KEY_D]);
    Add(keyE, [KEY_E]);
    Add(keyF, [KEY_F]);
    Add(keyG, [KEY_G]);
    Add(keyH, [KEY_H]);
    Add(keyI, [KEY_I]);
    Add(keyJ, [KEY_J]);
    Add(keyK, [KEY_K]);
    Add(keyL, [KEY_L]);
    Add(keyM, [KEY_M]);
    Add(keyN, [KEY_N]);
    Add(keyO, [KEY_O]);
    Add(keyP, [KEY_P]);
    Add(keyQ, [KEY_Q]);
    Add(keyR, [KEY_R]);
    Add(TKeyboard.TKey.keyS, [KEY_S]);
    Add(keyT, [KEY_T]);
    Add(keyU, [KEY_U]);
    Add(keyV, [KEY_V]);
    Add(keyW, [KEY_W]);
    Add(keyX, [KEY_X]);
    Add(keyY, [KEY_Y]);
    Add(keyZ, [KEY_Z]);
    Add(keyNumPad1, [KEY_KP_1, KEY_END]);
    Add(keyNumPad2, [KEY_KP_2, KEY_DOWN]);
    Add(keyNumPad3, [KEY_KP_3, KEY_PAGE_DOWN]);
    Add(keyNumPad4, [KEY_KP_4, KEY_LEFT]);
    Add(keyNumPad5, [KEY_KP_5]);
    Add(keyNumPad6, [KEY_KP_6, KEY_RIGHT]);
    Add(keyNumPad7, [KEY_KP_7, KEY_HOME]);
    Add(keyNumPad8, [KEY_KP_8, KEY_UP]);
    Add(keyNumPad9, [KEY_KP_9, KEY_PAGE_UP]);
    Add(keyNumPad0, [KEY_KP_0, KEY_INSERT]);
    Add(keyDecimal, [KEY_KP_DECIMAL, KEY_DELETE]);
    Add(keyPeriod, [KEY_PERIOD]);
    Add(keyComma, [KEY_COMMA]);
    Add(keyMinus, [KEY_MINUS, KEY_KP_SUBTRACT]);
    Add(keyEqual, [KEY_EQUAL, KEY_KP_ADD, KEY_KP_MULTIPLY, KEY_KP_DIVIDE]);
    Add(keyApostrophe, [KEY_APOSTROPHE]);
    Add(keyLeftBracket, [KEY_LEFT_BRACKET]);
    Add(keyRightBracket, [KEY_RIGHT_BRACKET]);
    Add(keyGrave, [KEY_GRAVE]);
    Add(keySlash, [KEY_SLASH]);
    Add(keySemiColon, [KEY_SEMICOLON]);
    Add(keyBackSlash, [KEY_BACKSLASH]);
    Add(keyNumLock, [KEY_NUM_LOCK]);
    Add(keyTab, [KEY_TAB]);
  end;
end;

procedure TApp.HandleLoadFromTape(ACpu: TCpu8088);
var
  Segment, Offset, BytesToRead: Word;
  Addr: TPhysicalAddress;
  Counter: Word;
  Data: Byte;
begin
  if not Assigned(FTapeStream) then
  begin
    ACpu.Registers.AH := 4;  { Error: timeout }
    ACpu.Registers.Flags.CF := True;
    Exit;
  end;

  Offset := ACpu.Registers.BX;
  BytesToRead := ACpu.Registers.CX;
  if (BytesToRead mod 256) <> 0 then
    BytesToRead := ((BytesToRead div 256) + 1) * 256;

  for Counter := 1 to BytesToRead do
  begin
    if (FTapeStream.Position >= FTapeStream.Size) then
    begin
      ACpu.Registers.AH := 2;  { Error: data loss }
      ACpu.Registers.DX := Counter - 1;
      ACpu.Registers.Flags.CF := True;
      Exit;
    end;

    Data := FTapeStream.ReadByte;
    if Counter > ACpu.Registers.CX then Continue;

    Addr := (ACpu.Registers.ES shl 4) + Offset;
    ACpu.MemoryBus.InvokeWrite(ACpu, Addr, Data);
    Inc(Offset);
  end;

  ACpu.Registers.BX := Offset;
  ACpu.Registers.DX := ACpu.Registers.CX;
  ACPu.Registers.Flags.CF := False;
end;

procedure TApp.UpdateKeyboard(AKeyboard: TKeyboard);
var
  Item: specialize TPair<TKeyboard.TKey, specialize TArray<RayLib.TKeyboardKey>>;
  RaylibKey: RayLib.TKeyboardKey;
begin
  for Item in FKeyboardMap do
  begin
    AKeyboard[Item.Key] := False;
    for RayLibKey in Item.Value do
      AKeyboard[Item.Key] := AKeyboard[Item.Key] or IsKeyDown(RayLibKey);
  end;
end;

function TApp.InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
var
  Cpu: TCpu8088 absolute ASender;
begin
  //Writeln(Format('INT %.x', [ANumber]));

  Result := False;
  case ANumber of
    $10:
      begin
        if Cpu.Registers.AH = $0E then
        begin
          // Writeln('Print char: ', Cpu.Registers.AL);
        end;
        {
        Writeln(Format('INT %.x :: %.4X %.4X :: AH:%.2x AL:%.2x', [
          ANumber,
          Cpu.Registers.CS, Cpu.Registers.IP,
          Cpu.Registers.AH, Cpu.Registers.AL
        ]));
        //if Cpu.Registers.AH = $0E then Writeln('Prints');
        }
      end;

    $15:
      case Cpu.Registers.AH of
        2:
          begin
            HandleLoadFromTape(Cpu);
            Result := True;
          end;
      else;
      end;

    $19: Result := HandleBootstrap(Cpu);
  end;
end;

function TApp.OnBeforeInstruction(ASender: TObject; AAddress: TPhysicalAddress): Boolean;
var
  Cpu: TCpu8088 absolute ASender;
  Res: TBinarySearchResult;
  BreakAddr: TPhysicalAddress;

begin
  Result := False;
  if FStepByStep then
  begin
    //LoadBytesToDebugger;
    Exit;
  end;

  if FStepBreakpoint <> 0 then
    FStepByStep := (AAddress = FStepBreakpoint)
  else
    for BreakAddr in Breakpoints do
      if BreakAddr = AAddress then
      begin
        FStepByStep := True;
        Break;
      end;

    { FStepByStep := specialize TArrayHelper<TPhysicalAddress>.BinarySearch(Breakpoints, AAddress, Res); }

  //if FStepByStep then LoadBytesToDebugger;
  Result := FStepByStep;
end;

procedure TApp.OnBeforeExecution(ASender: TObject; AInstruction: TInstruction);
var
  Cpu: TCpu8088 absolute ASender;
  DumpFrame: Dump.TDumpFrame;
begin
  if not Assigned(FDumpStream) then Exit;
  if AInstruction.CS = $F000 then Exit;

  DumpFrame := BuldDumpFrame(Cpu, (AInstruction.CS shl 4) + AInstruction.IP);
  DumpFrame.CS := AInstruction.CS;
  DumpFrame.IP := AInstruction.IP;
  FDumpStream.Write(DumpFrame, SizeOf(DumpFrame));
end;

procedure TApp.OnAfterInstruction(ASender: TObject; AInstruction: TInstruction);
var
  Line: String;
  Data: Byte;
  Frame: TDumpFrame;
  Cpu: TCpu8088 absolute ASender;
begin
  //if (AInstruction.CS = $C000) then FKeybEnabled := False;

  if (FDebug or FStepByStep) then
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

