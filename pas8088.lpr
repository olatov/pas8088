program pas8088;

{$mode objfpc}{$H+}

{$ifdef darwin}
  {$linklib raylib}
{$endif}
{$R-}

uses
  {$IFDEF UNIX}
  //CThreads,
  {$ENDIF}
  {$IfDef MSWINDOWS}
  Windows,
  {$EndIf}
  Classes, SysUtils, StrUtils, Math, StreamEx, BufStream, Generics.Collections,
  CustApp, RayLib, RayMath,
  Cpu8088, Memory, IO, Machine, VideoController, Interrupts, Hardware, Debugger,
  Keyboard, Dump, Timer, AppSettings, Cassette;

const
  FPS = 50;

  SpeakerSampleRate = 44100;
  SpeakerSamplesPerFrame = SpeakerSampleRate div FPS;

  DBG = False;

  RamAddress   = $00000;
  BiosAddress  = $FE000;
  VideoAddress = $B8000;
  CartAddress  = $C0000;

  DumpFile = '';
  DumpBios = False;
type

  { TRingBuffer }

  TRingBuffer = class
    Data: array[0..((44100 div 5) - 1)] of Byte;
    ReadIndex, WriteIndex: Integer;
    procedure Write(AValue: Byte);
    function Read(AFallback: Byte = 0): Byte;
  end;

var
  AudioBuffer: TRingBuffer;
  Breakpoints: array of TPhysicalAddress = ($00000);

type
  { TApplication }

  TApplication = class(TCustomApplication)
  private
    FFont: TFont;
    FGfxShader: TShader;
    FSpeakerStream: TAudioStream;
    procedure DumpMemory(AFileName: String; AMemoryBus: IMemoryBus;
      AAddress: TPhysicalAddress; ALength: Integer);
    procedure HandleCommandLine;
    procedure HandleReadFromTape;
    procedure HandleWriteToTape;
    function LoadFontFromResource(const AResourceName: String;
      const AFileType: String): TFont;
    function LoadFragmentShaderFromResource(const AResourceName: String): TShader;
    procedure OnBeforeExecution(ASender: TObject; AInstruction: TInstruction);
  public
    type
      TKeyboardMap = specialize TDictionary<TKeyboard.TKey, specialize TArray<RayLib.TKeyboardKey>>;
  public
    Target: TRenderTexture;
    FDebugger: TDebugger;
    FLogWriter: TStreamWriter;
    FDumpStream: TStream;
    FDebug: Boolean;
    FStepBreakpoint: TPhysicalAddress;
    FFrames: UInt64;
    FWorkingFileName: String;
    FNmiCounter: Integer;
    FStepByStep: Boolean;
    FKeybEnabled: Boolean;
    FKeyboardMap: TKeyboardMap;
    FKeyboard: TKeyboard;
    FBiosRomStream, FTapeStream, FBinStream, FCartridgeStream: TStream;
    FComputer: TMachine;
    FAudioCount: Integer;
    FOsdText: record
      Text: String;
      Color: TColorB;
      Lifetime: Integer; { In frames }
    end;
    property Font: TFont read FFont write FFont;
    property GfxShader: TShader read FGfxShader write FGfxShader;
    property SpeakerAudioStream: TAudioStream read FSpeakerStream write FSpeakerStream;
    property Computer: TMachine read FComputer write FComputer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Run;
    procedure RenderDisplay(AVideo: TVideoController);
    procedure RenderDebugger(ACpu: TCpu8088);
    procedure BuildKeyboardMap;
    function HandleBootstrap(const Cpu: TCpu8088): Boolean;
    { procedure HandleLoadFromBinaryTape(ACpu: TCpu8088); }
    procedure UpdateKeyboard(AKeyboard: TKeyboard);
    function BuildMachine: TMachine;
    procedure ToggleFullscreen;
    procedure PrintOsd(AText: String; AColor: TColorB);
    procedure PrintOsd(AText: String);
    function InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
    function OnBeforeInstruction(
      ASender: TObject; AAddress: TPhysicalAddress): Boolean;
    procedure OnAfterInstruction(ASender: TObject; AInstruction: TInstruction);
  end;

procedure AudioCallback(buffer: Pointer; frames: LongWord); cdecl; forward;

{ TRingBuffer }

procedure TRingBuffer.Write(AValue: Byte);
var
  NewIndex: Integer;
begin
  NewIndex := WriteIndex + 1;
  if NewIndex > High(Data) then NewIndex := 0;
  if NewIndex = ReadIndex then
  begin
    //Writeln('Buffer full');
    Exit;
  end;

  Data[WriteIndex] := AValue;
  WriteIndex := NewIndex;
end;

function TRingBuffer.Read(AFallback: Byte = 0): Byte;
begin
  if ReadIndex = WriteIndex then
    { Buffer is empty }
    Exit(AFallback);

  Result := Data[ReadIndex];
  Inc(ReadIndex);
  if ReadIndex > High(Data) then ReadIndex := 0;
end;

function TApplication.BuildMachine: TMachine;
var
  BiosRomBlock: TRomMemoryBlock;
  SystemRam, VideoRam: TRamMemoryBlock;
  CartRomBlock: TRomMemoryBlock;
begin
  Result := TMachine.Create(Nil);

  { Cpu }
  Result.Cpu := TCpu8088.Create(Result);

  { I/O }
  Result.IOBus := TIOBus.Create(Result);

  { Interrupt controller }
  Result.InterrptController := TPic8259.Create(Result, 8);

  { Timer }
  Result.Timer := TPit8253.Create(Result, Settings.Machine.ClockSpeed);

  { SystemRam / ROM }
  Result.MemoryBus := TMemoryBus.Create(Result);

  { System SystemRam = Total - 32 KB video }
  SystemRam := TRamMemoryBlock.Create(Result, 1024 * (Settings.Machine.Ram - 32), RamAddress);
  Result.AddMemory(SystemRam);

  BiosRomBlock := TRomMemoryBlock.Create(Result, 1024 * 8, BiosAddress);
  Result.AddMemory(BiosRomBlock);

  VideoRam := TRamMemoryBlock.Create(Result, 1024 * 32, VideoAddress);
  Result.AddMemory(VideoRam);

  { Video }
  Result.Video := TVideoController.Create(
    Result, Settings.Machine.ClockSpeed div FPS);

  Result.Video.NmiTrigger := TNmiTrigger.Create(Result);
  Result.Video.NmiTrigger.AttachCpu(Result.Cpu);

  { Keyboard }
  FKeyboard := TKeyboard.Create(Result);
  Result.IOBus.AttachDevice(FKeyboard);

  { Cassette drive }
  Result.CassetteDrive := TCassetteDrive.Create(Result);

  if Assigned(FCartridgeStream) then
  begin
    CartRomBlock := TRomMemoryBlock.Create(
      Result, FCartridgeStream.Size, CartAddress);
    Result.AddMemory(CartRomBlock);
    try
      CartRomBlock.LoadFromStream(FCartridgeStream);
    finally
      FreeAndNil(FCartridgeStream);
    end;
  end;

  Result.Initialize;

  if Assigned(FBiosRomStream) then
    try
      BiosRomBlock.LoadFromStream(FBiosRomStream);
    finally
      FreeAndNil(FBiosRomStream);
    end;
end;

procedure TApplication.ToggleFullscreen;
begin
  Settings.Window.FullScreen := not Settings.Window.FullScreen;

  if Settings.Window.FullScreen then
  begin
    ClearWindowState(FLAG_WINDOW_RESIZABLE);
    //HideCursor;
  end else
  begin
    SetWindowState(FLAG_WINDOW_RESIZABLE);
    //ShowCursor;
  end;

  ToggleBorderlessWindowed;
end;

procedure TApplication.PrintOsd(AText: String; AColor: TColorB);
begin
  FOsdText.Text := AText;
  FOsdText.Color := Fade(AColor, 0.5);
  FOsdText.Lifetime := FPS * 3;
end;

procedure TApplication.PrintOsd(AText: String);
begin
  PrintOsd(AText, YELLOW);
end;

constructor TApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDebugger := TDebugger.Create;
  { FLogWriter := TStreamWriter.Create('/tmp/comp.log', False, TEncoding.UTF8, 16384); }

  if not DumpFile.IsEmpty then
  begin
    FDumpStream := TWriteBufStream.Create(
      TFileStream.Create(DumpFile, fmCreate));
    TWriteBufStream(FDumpStream).SourceOwner := True;
  end;
end;

destructor TApplication.Destroy;
begin
  if not Settings.Window.FullScreen then
  begin
    Settings.Window.Width := GetScreenWidth;
    Settings.Window.Height := GetScreenHeight;
  end;

  UnloadShader(GfxShader);
  UnloadRenderTexture(Target);
  UnloadFont(Font);

  CloseWindow;

  UnloadAudioStream(SpeakerAudioStream);
  CloseAudioDevice;

  FreeAndNil(FComputer);

  Settings.Save;

  FreeAndNil(FDebugger);
  FreeAndNil(FLogWriter);
  FreeAndNil(FDumpStream);
  FreeAndNil(FTapeStream);
  FreeAndNil(FKeyboardMap);

  inherited Destroy;
end;

procedure TApplication.Initialize;
begin
  inherited Initialize;

  HandleCommandLine;

  if not Settings.Machine.BiosRom.IsEmpty then
  begin
    FBiosRomStream := TMemoryStream.Create;
    TMemoryStream(FBiosRomStream).LoadFromFile(Settings.Machine.BiosRom);
  end;

  InitAudioDevice;

  if not Settings.Window.FullScreen then SetConfigFlags(FLAG_WINDOW_RESIZABLE);

  {$IfNDef darwin}
    SetConfigFlags(FLAG_WINDOW_HIGHDPI);
  {$EndIf}

  InitWindow(640, 400, PChar(Title));
  SetTargetFPS(FPS);
  SetExitKey(KEY_NULL);

  SetWindowSize(Settings.Window.Width, Settings.Window.Height);

  Target := LoadRenderTexture(640, 400);
  SetTextureFilter(Target.texture, TEXTURE_FILTER_BILINEAR);

  GfxShader := LoadFragmentShaderFromResource('GFX_SHADER');

  Font := LoadFontFromResource('FONT', '.ttf');

  FKeyboardMap := TKeyboardMap.Create;
  BuildKeyboardMap;

  Computer := BuildMachine;
end;

procedure TApplication.HandleCommandLine;
var
  Option, Extension: String;
begin
  for Option in GetNonOptions('', []) do
  begin
    Extension := LowerCase(ExtractFileExt(Option));
    case Extension of
      '.wav':
        begin
          try
            FTapeStream := TFileStream.Create(Option, fmOpenRead);
            FWorkingFileName := Option;
            PrintOsd('[Tape] ' + FWorkingFileName);
          except
            on E: Exception do
            begin
              PrintOsd('[Tape] ' + E.Message);
              FWorkingFileName := String.Empty;
              FreeAndNil(FTapeStream);
            end;
          end;
        end;

      '.cart':
        begin
          FCartridgeStream := TMemoryStream.Create;
          try
            TMemoryStream(FCartridgeStream).LoadFromFile(Option);
            FWorkingFileName := Option;
            PrintOsd('[Cartridge] ' + FWorkingFileName);
          except
            on E: Exception do
            begin
              PrintOsd('[Cartridge] ' + E.Message);
              FWorkingFileName := String.Empty;
              FreeAndNil(FCartridgeStream);
            end;
          end;
        end;

      '.bin':
        begin
          FBinStream := TMemoryStream.Create;
          try
            TMemoryStream(FBinStream).LoadFromFile(Option);
            FWorkingFileName := Option;
          except
            on E: Exception do
            begin
              PrintOsd('[BIN] ' + E.Message);
              FWorkingFileName := String.Empty;
              FreeAndNil(FBinStream);
            end;
          end;
        end;
    else
      PrintOsd(Format('[Error] Unknown file: %s', [Option]));
    end;
  end;
end;

procedure TApplication.Run;
const
  TapeFrequency = 8000;
var
  LinesLoc, I, Underruns, CyclesPerSpeakerSample: Integer;
  LinesCount: Single = 400.0;
  Listing: array of TDebugger.TSourceLine;
  PrevTicks: QWord = 0;
  ScanlinesEnabled, GrayscaleEnabled, CyclesPerFrame: Integer;
  TargetRectangle: TRectangle;
  CyclesPerCassetteSample: Integer;
  TapeDelta: Double;
  WaitStates, InitialWaitStates: Integer;

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

  function TapePositionAsText: String;
  begin
    Result := Format(
      '%d:%.2d / %d:%.2d',
      [
        Trunc(Computer.CassetteDrive.Position) div 60,
        Trunc(Computer.CassetteDrive.Position) mod 60,
        Trunc(Computer.CassetteDrive.Length) div 60,
        Trunc(Computer.CassetteDrive.Length) mod 60
      ]);
  end;

begin
  FDebug := DBG;

  Computer.Cpu.InterruptHook := @InterruptHook;
  Computer.Cpu.OnBeforeInstruction := @OnBeforeInstruction;
  Computer.Cpu.OnAfterInstruction := @OnAfterInstruction;
  Computer.Cpu.OnBeforeExecution := @OnBeforeExecution;

  LinesLoc := GetShaderLocation(GfxShader, 'lines');
  SetShaderValue(GfxShader, LinesLoc, @LinesCount, SHADER_UNIFORM_FLOAT);

  Title := 'Poisk';
  if not FWorkingFileName.IsEmpty then
  begin
    Title := Title + ' - ' + FWorkingFileName;
    if Assigned(FTapeStream) then
      Title := Title + Format(' [%s]', [TapePositionAsText]);
  end;

  SetWindowTitle(PChar(Title));

  { Audio stream }
  SetAudioStreamBufferSizeDefault(SpeakerSamplesPerFrame);
  SpeakerAudioStream := LoadAudioStream(SpeakerSampleRate, 8, 1);
  SetAudioStreamCallback(SpeakerAudioStream, @AudioCallback);

  SetMasterVolume(Settings.Audio.Volume);

  PlayAudioStream(SpeakerAudioStream);
  if Settings.Audio.Mute then PauseAudioStream(SpeakerAudioStream);

  FKeybEnabled := True;

  CyclesPerFrame := Settings.Machine.ClockSpeed div FPS;

  if Assigned(FTapeStream) then
    Computer.CassetteDrive.LoadTape(FTapeStream);

  CyclesPerCassetteSample := Settings.Machine.ClockSpeed div TapeFrequency - 4;
  TapeDelta := CyclesPerCassetteSample / Settings.Machine.ClockSpeed;

  HideCursor;

  if Settings.Window.FullScreen then
  begin
    Sleep(200);
    ToggleBorderlessWindowed;
  end;

  CyclesPerSpeakerSample := (CyclesPerFrame div SpeakerSamplesPerFrame);

  DivMod(
    Settings.Machine.ClockSpeed, Settings.Machine.CpuSpeed,
    WaitStates, InitialWaitStates);
  InitialWaitStates := InitialWaitStates div FPS;

  while not WindowShouldClose do
  begin
    FOsdText.Lifetime := Max(0, FOsdText.Lifetime - 1);

    if IsKeyDown(KEY_F11) then
    begin
      if IsKeyPressed(KEY_ESCAPE) then Break;
      if IsKeyPressed(KEY_F1) then Computer.Reset;

      if IsKeyPressed(KEY_F5) then
        if Computer.CassetteDrive.State = csStopped then
          Computer.CassetteDrive.Play
        else
          Computer.CassetteDrive.Stop;

      if (IsKeyPressed(KEY_F6) or IsKeyPressedRepeat(KEY_F6)) then
      begin
        Computer.CassetteDrive.Position := Computer.CassetteDrive.Position - 10;
        PrintOsd('Tape: ' + TapePositionAsText);
      end;

      if (IsKeyPressed(KEY_F7) or IsKeyPressedRepeat(KEY_F7)) then
      begin
        Computer.CassetteDrive.Position := Computer.CassetteDrive.Position + 10;
        PrintOsd('Tape: ' + TapePositionAsText);
      end;

      if IsKeyPressed(KEY_F8) then
        PrintOsd('Tape: ' + TapePositionAsText);

      if IsKeyPressed(KEY_G) then Settings.Video.Grayscale := not Settings.Video.GrayScale;
      if IsKeyPressed(KEY_S) then Settings.Video.ScanLines := not Settings.Video.ScanLines;
      if IsKeyPressed(KEY_A) then
        Settings.Window.AspectRatio := IfThen(IsZero(Settings.Window.AspectRatio), 4/3, 0);
      if IsKeyPressed(KEY_F) then ToggleFullscreen;

      if IsKeyPressed(KEY_NINE) or IsKeyPressedRepeat(KEY_NINE) then
      begin
        Settings.Audio.Volume := Settings.Audio.Volume - 0.05;
        SetMasterVolume(Settings.Audio.Volume);
        PrintOsd(Format('Vol: %d', [Round(Settings.Audio.Volume * 100)]));
      end;

      if IsKeyPressed(KEY_ZERO) or IsKeyPressedRepeat(KEY_ZERO) then
      begin
        Settings.Audio.Volume := Settings.Audio.Volume + 0.05;
        SetMasterVolume(Settings.Audio.Volume);
        PrintOsd(Format('Vol: %d', [Round(Settings.Audio.Volume * 100)]));
      end;

      if IsKeyPressed(KEY_M) then
      begin
        Settings.Audio.Mute := not Settings.Audio.Mute;
        if Settings.Audio.Mute then
        begin
          PauseAudioStream(SpeakerAudioStream);
          PrintOsd('Sound off');
        end else
        begin
          ResumeAudioStream(SpeakerAudioStream);
          PrintOsd('Sound on');
        end;
      end;
    end else
      UpdateKeyboard(FKeyboard);

    GrayscaleEnabled := IfThen(Settings.Video.Grayscale, 1, 0);
    ScanlinesEnabled := IfThen(Settings.Video.ScanLines, 1, 0);

    SetShaderValue(GfxShader,
      GetShaderLocation(GfxShader, 'enableGrayscale'),
      @GrayscaleEnabled, SHADER_UNIFORM_INT);

    SetShaderValue(GfxShader,
      GetShaderLocation(GfxShader, 'enableScanlines'),
      @ScanlinesEnabled, SHADER_UNIFORM_INT);

    Inc(FFrames);

    if FStepByStep and (PrevTicks <> Computer.Cpu.Ticks) then
      LoadBytesToDebugger(Computer.MemoryBus, Computer.Cpu.CurrentAddress);

    PrevTicks := Computer.Cpu.Ticks;

    for I := 0 to Computer.Timer.Speaker.SampleCount - 1 do
      AudioBuffer.Write(Computer.Timer.Speaker.Samples[I]);

    Computer.Video.BeginFrame;
    Computer.Timer.Speaker.BeginAudioChunk;

    if Computer.CassetteDrive.State in [csPlaying, csRecording] then
    begin
      CyclesPerCassetteSample := Settings.Machine.ClockSpeed div Computer.CassetteDrive.SampleRate;
      TapeDelta := CyclesPerCassetteSample / Settings.Machine.ClockSpeed;

      SetWindowTitle(PChar(
        Format('Poisk - %s [%s]',
        [
          FWorkingFileName,
          TapePositionAsText
        ])));
    end;

    try
      if (not FStepByStep) or (Computer.Cpu.Registers.CS >= $F000) then
      begin
        Computer.Cpu.WaitStates := InitialWaitStates;
        for I := 0 to CyclesPerFrame - 1 do
        begin
          Computer.Tick;
          if Computer.Cpu.WaitStates <= 0 then Computer.Cpu.WaitStates := WaitStates;

          if FStepByStep then Break;

          if (Computer.Cpu.Ticks mod CyclesPerSpeakerSample) = 0 then
            { Time to play a sample from tape noise }
            case Computer.CassetteDrive.State of
              csPlaying:
                Computer.Timer.Speaker.CaptureSample(Computer.CassetteDrive.TapeIn, 0.4);
              csRecording:
                Computer.Timer.Speaker.CaptureSample(Computer.CassetteDrive.TapeOut, 0.4);
            else
              Computer.Timer.Speaker.CaptureSample;
            end;

          if (Computer.CassetteDrive.State in [csPlaying, csRecording]) and
              ((Computer.Cpu.Ticks mod CyclesPerCassetteSample) = 0) then
            { Time to update the state of tape }
            Computer.CassetteDrive.Tick(TapeDelta);
        end;
      end else
      begin
        { Debugger mode keys }
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

      TargetRectangle := RectangleCreate(0, 0, GetRenderWidth, GetRenderHeight);
      if not IsZero(Settings.Window.AspectRatio) then
      begin
        if (GetRenderHeight * Settings.Window.AspectRatio) < GetRenderWidth then
        begin
          TargetRectangle.Width := TargetRectangle.Height * Settings.Window.AspectRatio;
          TargetRectangle.X := (GetRenderWidth - TargetRectangle.width) * 0.5;
        end else
        begin
          TargetRectangle.Height := TargetRectangle.Width / Settings.Window.AspectRatio;
          TargetRectangle.Y := (GetRenderHeight - TargetRectangle.height) * 0.5;
        end;
      end;

      TargetRectangle.width := TargetRectangle.width / GetWindowScaleDPI.x;
      TargetRectangle.height := TargetRectangle.height / GetWindowScaleDPI.y;
      TargetRectangle.x := TargetRectangle.x / GetWindowScaleDPI.x;
      TargetRectangle.y := TargetRectangle.y / GetWindowScaleDPI.y;

      BeginShaderMode(GfxShader);

      DrawTexturePro(
        Target.texture,
        RectangleCreate(0, 0, Target.texture.width, -Target.texture.height),
        TargetRectangle,
        Vector2Zero, 0, WHITE);

      EndShaderMode;

      if (FOsdText.Lifetime > 0) then
      begin
        DrawRectangle(0, 0, 640, 56, ColorAlpha(BLACK, Min(FOsdText.Lifetime / FPS, 0.8)));
        DrawTextEx(Font,
          PChar(FOsdText.Text),
          Vector2Create(0, 0), 48, 0,
          ColorAlpha(FOsdText.Color, Min(FOsdText.Lifetime / FPS, 1)));
      end;

      if FStepByStep then
        RenderDebugger(Computer.Cpu);
    EndDrawing;
  end;
end;

procedure TApplication.DumpMemory(AFileName: String;
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

procedure TApplication.HandleReadFromTape;
begin
  FComputer.CassetteDrive.Play;
end;

procedure TApplication.HandleWriteToTape;
var
  FileName: String = '';
  Data: Byte;
  I: Integer;
begin
  if Computer.CassetteDrive.State <> csStopped then Exit;

  { Release current tape if any, new tape will be created }
  FreeAndNil(FTapeStream);

  { See if there's tape file header by checking the magic byte }
  Computer.MemoryBus.InvokeRead(Nil,
    GetPhysicalAddress(Computer.Cpu.Registers.ES, Computer.Cpu.Registers.BX),
    Data);

  if Data = $A5 then
  begin
    { Yes - read file name from the header }
    SetLength(FileName, 8);
    for I := 1 to 8 do
    begin
      Computer.MemoryBus.InvokeRead(Nil,
        GetPhysicalAddress(
          Computer.Cpu.Registers.ES,
          Computer.Cpu.Registers.BX + I),
        Data);
      FileName[I] := AnsiChar(Data);
    end;
    FileName := LowerCase(Trim(FileName));
  end else
    { No - use default one }
    FileName := 'tapeout';

  FileName := FileName + '.wav';
  FTapeStream := TFileStream.Create(FileName, fmCreate);
  Computer.CassetteDrive.LoadTape(FTapeStream);
  FComputer.CassetteDrive.Record_;
end;

function TApplication.HandleBootstrap(const Cpu: TCpu8088): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not Assigned(FBinStream) then Exit;

  I := 0;
  while FBinStream.Position < FBinStream.Size do
  begin
    Cpu.MemoryBus.InvokeWrite(Cpu, $600 + I, FBinStream.ReadByte);
    Inc(I);
  end;
  FreeAndNil(FBinStream);

  Cpu.Registers.CS := $60;
  Cpu.Registers.DS := $60;
  Cpu.Registers.ES := $60;
  Cpu.Registers.SS := $60;
  Cpu.Registers.IP := $0;
  Cpu.Registers.SP := $FFFE;
  Result := True
end;

procedure TApplication.RenderDisplay(AVideo: TVideoController);
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

procedure TApplication.RenderDebugger(ACpu: TCpu8088);
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
  Left := 0;
  Color := ColorAlpha(YELLOW, 0.9);

  DrawTextEx(
    Font,
    PChar(Format('%.4x:%.4x %s',
      [
        ACpu.Registers.CS, ACpu.Registers.IP,
        FDebugger.FindLine(ACpu.CurrentAddress)
      ]
    )),
    Vector2Create(Left - 800, 0 * (FontSize + VertSpacing) + 0), CodeFontSize, 1, Color);

  DrawTextEx(
    Font,
    PChar(Format('AX %.4x', [ACpu.Registers.AX])),
    Vector2Create(Left, 0 * (FontSize + VertSpacing) + 0), FontSize, 1, Color);
  DrawTextEx(
    Font,
    PChar(Format('BX %.4x', [ACpu.Registers.BX])),
    Vector2Create(Left, 1 * (FontSize + VertSpacing) + 0), FontSize, 1, Color);
  DrawTextEx(
    Font,
    PChar(Format('CX %.4x', [ACpu.Registers.CX])),
    Vector2Create(Left, 2 * (FontSize + VertSpacing) + 0), FontSize, 1, Color);
  DrawTextEx(
    Font,
    PChar(Format('DX %.4x', [ACpu.Registers.DX])),
    Vector2Create(Left, 3 * (FontSize + VertSpacing) + 0), FontSize, 1, Color);

  DrawTextEx(
    Font,
    PChar(Format('BP %.4x', [ACpu.Registers.BP])),
    Vector2Create(Left, 4 * (FontSize + VertSpacing) + 48), FontSize, 1, Color);
  DrawTextEx(
    Font,
    PChar(Format('SP %.4x', [ACpu.Registers.SP])),
    Vector2Create(Left, 5 * (FontSize + VertSpacing) + 48), FontSize, 1, Color);
  DrawTextEx(
    Font,
    PChar(Format('SI %.4x', [ACpu.Registers.SI])),
    Vector2Create(Left, 6 * (FontSize + VertSpacing) + 48), FontSize, 1, Color);
  DrawTextEx(
    Font,
    PChar(Format('DI %.4x', [ACpu.Registers.DI])),
    Vector2Create(Left, 7 * (FontSize + VertSpacing) + 48), FontSize, 1, Color);

  DrawTextEx(
    Font,
    PChar(Format('CS %.4x', [ACpu.Registers.CS])),
    Vector2Create(Left, 8 * (FontSize + VertSpacing) + 96), FontSize, 1, Color);
  DrawTextEx(
    Font,
    PChar(Format('DS %.4x', [ACpu.Registers.DS])),
    Vector2Create(Left, 9 * (FontSize + VertSpacing) + 96), FontSize, 1, Color);
  DrawTextEx(
    Font,
    PChar(Format('ES %.4x', [ACpu.Registers.ES])),
    Vector2Create(Left, 10 * (FontSize + VertSpacing) + 96), FontSize, 1, Color);
  DrawTextEx(
    Font,
    PChar(Format('SS %.4x', [ACpu.Registers.SS])),
    Vector2Create(Left, 11 * (FontSize + VertSpacing) + 96), FontSize, 1, Color);

  DrawTextEx(
    Font,
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
      Font,
      PChar(Format('ST(%d) %.4x', [I, DataWord])),
      Vector2Create(Left, (11 + I) * (FontSize + VertSpacing) + 240), FontSize, 1, Color);
  end;

  DrawTextEx(
    Font,
    PChar(Format('C(%d)', [IfThen(ACpu.Registers.Flags.CF, 1, 0)])),
    Vector2Create(Left + 240, 0 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.CF, YELLOW, GRAY));

  DrawTextEx(
    Font,
    PChar(Format('Z(%d)', [IfThen(ACpu.Registers.Flags.ZF, 1, 0)])),
    Vector2Create(Left + 240, 1 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.ZF, YELLOW, GRAY));

  DrawTextEx(
    Font,
    PChar(Format('S(%d)', [IfThen(ACpu.Registers.Flags.SF, 1, 0)])),
    Vector2Create(Left + 240, 2 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.SF, YELLOW, GRAY));

  DrawTextEx(
    Font,
    PChar(Format('O(%d)', [IfThen(ACpu.Registers.Flags.OF_, 1, 0)])),
    Vector2Create(Left + 240, 3 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.OF_, YELLOW, GRAY));

  DrawTextEx(
    Font,
    PChar(Format('P(%d)', [IfThen(ACpu.Registers.Flags.PF, 1, 0)])),
    Vector2Create(Left + 240, 4 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.PF, YELLOW, GRAY));

  DrawTextEx(
    Font,
    PChar(Format('A(%d)', [IfThen(ACpu.Registers.Flags.AF, 1, 0)])),
    Vector2Create(Left + 240, 5 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.AF, YELLOW, GRAY));

  DrawTextEx(
    Font,
    PChar(Format('D(%d)', [IfThen(ACpu.Registers.Flags.DF, 1, 0)])),
    Vector2Create(Left + 240, 6 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.DF, YELLOW, GRAY));

  DrawTextEx(
    Font,
    PChar(Format('I(%d)', [IfThen(ACpu.Registers.Flags.IF_, 1, 0)])),
    Vector2Create(Left + 240, 7 * (FontSize + VertSpacing) + 0), FontSize, 1,
    specialize IfThen<TColorB>(ACpu.Registers.Flags.IF_, YELLOW, GRAY));

  DrawTextEx(
    Font,
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
        Font,
        PChar(Format('%.5x | %s', [Listing[I].Address, Listing[I].Contents])),
        Vector2Create(Left + 400, I * (FontSize + 8) + 120), CodeFontSize, 1,
        specialize IfThen<TColorB>(Listing[I].Address = Addr, YELLOW, GRAY));
  end;
end;

procedure TApplication.BuildKeyboardMap;
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
    {$ifdef DARWIN}
      Add(keyRightControl, [KEY_RIGHT_SUPER]);
    {$else}
      Add(keyRightControl, [KEY_RIGHT_CONTROL]);
    {$endif}
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

{
procedure TApp.HandleLoadFromBinaryTape(ACpu: TCpu8088);
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
}

procedure TApplication.UpdateKeyboard(AKeyboard: TKeyboard);
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

function TApplication.InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
var
  Cpu: TCpu8088 absolute ASender;
begin
  //Writeln(Format('INT %.x', [ANumber]));

  Result := False;
  case ANumber of
    {
    $08:
      Writeln((Time*86400):12:9, ' :: INT 8');
    }
    $10:
      begin
        { BIOS Video function }
      end;

    $15:
      begin
        case Cpu.Registers.AH of
          1: Computer.CassetteDrive.Stop;
          2: HandleReadFromTape;
          3: HandleWriteToTape;
        else;
        end;
      end;

    $19:
      try
        Result := HandleBootstrap(Cpu);
      except
        on E: Exception do
          PrintOsd('[BIN] ' + E.Message);
      end;
  end;
end;

function TApplication.OnBeforeInstruction(ASender: TObject; AAddress: TPhysicalAddress): Boolean;
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

procedure TApplication.OnBeforeExecution(ASender: TObject; AInstruction: TInstruction);
var
  Cpu: TCpu8088 absolute ASender;
  DumpFrame: Dump.TDumpFrame;
  Data: Byte;
  Crc: Word;
begin
  if not Assigned(FDumpStream) then Exit;
  if not DumpBios and (AInstruction.CS = $F000) then Exit;

  DumpFrame := BuldDumpFrame(Cpu, (AInstruction.CS shl 4) + AInstruction.IP);
  DumpFrame.CS := AInstruction.CS;
  DumpFrame.IP := AInstruction.IP;
  FDumpStream.Write(DumpFrame, SizeOf(DumpFrame));
end;

procedure TApplication.OnAfterInstruction(ASender: TObject; AInstruction: TInstruction);
var
  Line: String;
  Data: Byte;
  Frame: TDumpFrame;
  Cpu: TCpu8088 absolute ASender;
begin
{
  if (Cpu.Registers.AX = 63435) then
    WriteLn(Format('%.4x:%.4x !!!',
    [
      AInstruction.CS, AInstruction.IP
    ]));
}

  //if (AInstruction.CS = $070) then FStepByStep := True;
  if (AInstruction.CS = $F000) then Exit;
  //if (AInstruction.CS = $70) then FDebug := True;

  if (FDebug or FStepByStep) then
    WriteLn(Format('%.4x:%.4x | %-24s | %s ',
    [
      AInstruction.CS, AInstruction.IP,
      FDebugger.FindLine(AInstruction.CS, AInstruction.IP),
      TCpu8088(ASender).DumpCurrentInstruction
    ]));
end;

function TApplication.LoadFontFromResource(
  const AResourceName: String; const AFileType: String): TFont;
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HINSTANCE, AResourceName, RT_RCDATA);
  try
    Result := LoadFontFromMemory(
      PChar(AFileType), Stream.Memory, Stream.Size, 48, Nil, 0);
  finally
    FreeAndNil(Stream);
  end;
end;

function TApplication.LoadFragmentShaderFromResource(const AResourceName: String): TShader;
var
  Stream: TResourceStream;
  Content: String;
begin
  Stream := TResourceStream.Create(HINSTANCE, AResourceName, RT_RCDATA);
  try
    SetLength(Content, Stream.Size);
    Stream.Read(Content[1], Stream.Size);
    Result := LoadShaderFromMemory(Nil, PChar(Content));
  finally
    FreeAndNil(Stream);
  end;
end;


procedure AudioCallback(buffer: Pointer; Frames: LongWord); cdecl;
var
  I: Integer;
  OutBuf: PByte;
  Sample: Byte;
begin
  OutBuf := PByte(Buffer);
  Sample := 0;
  for I := 0 to Frames - 1 do
  begin
    Sample := AudioBuffer.Read(127);
    OutBuf[I] := Sample;
  end;
end;

var
  Application: TApplication;

{$R *.res}

begin
  SetExceptionMask(GetExceptionMask + [exInvalidOp, exOverflow, exZeroDivide]);

  AudioBuffer := TRingBuffer.Create;

  Application := TApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;

  AudioBuffer.Free;
end.

