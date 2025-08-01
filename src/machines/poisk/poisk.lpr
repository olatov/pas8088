program Poisk;

{$mode objfpc}{$H+}

{$ifdef darwin}
  {$linklib raylib}
{$endif}
{$R-}

uses
  {$IFDEF UNIX}
  CThreads,
  {$ENDIF}
  {$IfDef MSWINDOWS}
  Windows,
  {$EndIf}
  Classes, SysUtils, StrUtils, Math, StreamEx, BufStream, Generics.Collections,
  CustApp, RayLib, RayMath, Cpu8088, Memory, IO, Machine, VideoController,
  Interrupts, Hardware, Keyboard, Dump, Timer, AppSettings, Cassette,
  FloppyDiskController, Buffers, Debugger;

const
  Version = '0.1';
  FPS = 50;

  SpeakerSampleRate = 44100;
  SpeakerSamplesPerFrame = SpeakerSampleRate div FPS;

  DBG = False;

  RamAddress    = $00000;
  BiosAddress   = $FE000;
  VideoAddress  = $B8000;
  CartAddress   = $C0000;
  FdcRomAddress = $E0000;

  DumpFile = '';
  DumpBios = False;

  DiskSectorSize = 512;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  private
    FDebugger: TWebDebugger;
    type
      TOsdText = class
        Text: String;
        Color: TColorB;
        Lifetime: Double; { In seconds }
      end;
      TOsdTextList = specialize TObjectList<TOsdText>;
    procedure SetDebugger(AValue: TWebDebugger);
  private
    FFont: TFont;
    FGfxShader: TShader;
    FPaused: Boolean;
    FSpeakerStream: TAudioStream;
    property Debugger: TWebDebugger read FDebugger write SetDebugger;
    procedure DumpMemory(AFileName: String; AMemoryBus: IMemoryBus;
      AAddress: TPhysicalAddress; ALength: Integer);
    procedure HandleCommandLine;
    procedure HandleReadFromTape;
    procedure HandleWriteToTape;
    function LoadFontFromResource(const AResourceName: String;
      const AFileType: String): TFont;
    function LoadFragmentShaderFromResource(const AResourceName: String): TShader;
    procedure OnBeforeExecution(ASender: TObject; AInstruction: TInstruction);
    procedure SetPaused(AValue: Boolean);
  public
    type
      TKeyboardMap = specialize TDictionary<TKeyboard.TKey, specialize TArray<RayLib.TKeyboardKey>>;
  public
    FAudioBuffer: TRingBuffer;
    FTarget: TRenderTexture;
    FLogWriter: TStreamWriter;
    FDumpStream: TStream;
    FDebug: Boolean;
    FStepBreakpoint: TPhysicalAddress;
    FFrames: UInt64;
    FWorkingFileName: String;
    FKeyboardMap: TKeyboardMap;
    FKeyboard: TKeyboard;
    FBiosRomStream, FTapeStream, FBinStream, FCartridgeStream: TStream;
    FFloppyDiskStreams: specialize TArray<TStream>;
    FComputer: TMachine;
    FLastAudioSampleTime: Double;
    FOsdTextItems: TOsdTextList;
    property Font: TFont read FFont write FFont;
    property GfxShader: TShader read FGfxShader write FGfxShader;
    property SpeakerAudioStream: TAudioStream read FSpeakerStream write FSpeakerStream;
    property Computer: TMachine read FComputer write FComputer;
    property Paused: Boolean read FPaused write SetPaused;
    procedure RenderDisplay(AVideo: TVideoController);
    procedure RunMachine;
    procedure RenderOsd;
    procedure BuildKeyboardMap;
    function HandleBootstrap(const Cpu: TCpu8088): Boolean;
    function HandleDiskIO: Boolean;
    procedure UpdateKeyboard(AKeyboard: TKeyboard);
    function BuildMachine: TMachine;
    procedure ToggleFullscreen;
    procedure PrintOsd(AText: String; AColor: TColorB);
    procedure PrintOsd(AText: String);
    function InterruptHook(ASender: TObject; ANumber: Byte): Boolean;
    function OnBeforeInstruction(
      ASender: TObject; AAddress: TPhysicalAddress): Boolean;
    procedure OnAfterInstruction(ASender: TObject; AInstruction: TInstruction);
  protected
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure WriteHelp; virtual;
  end;

  procedure AudioCallback(Buffer: Pointer; Frames: LongWord); cdecl; forward;

{ TApplication }

procedure TApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  RunMachine;

  // stop program loop
  Terminate;
end;

function TApplication.BuildMachine: TMachine;
var
  BiosRomBlock, FdcRomBlock: TRomMemoryBlock;
  SystemRam, VideoRam: TRamMemoryBlock;
  CartRomBlock: TRomMemoryBlock;
  FdcRomStream: TFileStream;
  VideoController: TVideoController;
  I: Integer;
begin
  Result := TMachine.Create(Self);

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
  VideoController := TVideoController.Create(
    Result, Settings.Machine.ClockSpeed div FPS);
  VideoController.PoiskPalletteBug := Settings.Video.PalletteBug;
  Result.Video := VideoController;

  Result.Video.NmiTrigger := TNmiTrigger.Create(Result);
  Result.Video.NmiTrigger.AttachCpu(Result.Cpu);

  { Keyboard }
  FKeyboard := TKeyboard.Create(Result);
  Result.IOBus.AttachDevice(FKeyboard);

  { Cassette drive }
  Result.CassetteDrive := TCassetteDrive.Create(Result);

  { ROM cartridge disk }
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

  { Floppy disk }
  if Settings.FloppyDisk.Enabled and (Length(FFloppyDiskStreams) > 0) then
  begin
    FdcRomStream := TFileStream.Create(Settings.FloppyDisk.ControllerRom, fmOpenRead);
    try
      FdcRomBlock := TRomMemoryBlock.Create(Result, FdcRomStream.Size, FdcRomAddress);
      FdcRomBlock.LoadFromStream(FdcRomStream);
      Result.AddMemory(FdcRomBlock);
    finally
      FreeAndNil(FdcRomStream);
    end;

    Result.FloppyDiskController := TFloppyDiskController.Create(Result);
    for I := 0 to High(FFloppyDiskStreams) do
      Result.FloppyDiskController.InsertDisk(I, FFloppyDiskStreams[I]);
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
var
  Item: TOsdText;
begin
  Item := TOsdText.Create;
  Item.Text := AText;
  Item.Color := Fade(AColor, 0.5);
  Item.Lifetime := 4;

  FOsdTextItems.Add(Item);
  while FOsdTextItems.Count > 10 do
    FOsdTextItems.Remove(FOsdTextItems.First);
end;

procedure TApplication.PrintOsd(AText: String);
begin
  PrintOsd(AText, YELLOW);
end;

constructor TApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Debugger := TWebDebugger.Create(Self);

  if not DumpFile.IsEmpty then
  begin
    FDumpStream := TWriteBufStream.Create(
      TFileStream.Create(DumpFile, fmCreate));
    TWriteBufStream(FDumpStream).SourceOwner := True;
  end;
end;

destructor TApplication.Destroy;
var
  DiskStream: TStream;
begin
  if not Settings.Window.FullScreen then
  begin
    Settings.Window.Width := GetScreenWidth;
    Settings.Window.Height := GetScreenHeight;
  end;

  SetAudioStreamCallback(SpeakerAudioStream, Nil);

  UnloadShader(GfxShader);
  UnloadRenderTexture(FTarget);
  UnloadFont(Font);

  CloseWindow;

  UnloadAudioStream(SpeakerAudioStream);
  CloseAudioDevice;

  FreeAndNil(FComputer);

  Settings.Save;

  FreeAndNil(FOsdTextItems);
  FreeAndNil(FLogWriter);
  FreeAndNil(FDumpStream);
  FreeAndNil(FTapeStream);

  for DiskStream in FFloppyDiskStreams do
    FreeAndNil(DiskStream);

  FreeAndNil(FKeyboardMap);

  inherited Destroy;
end;

procedure TApplication.Initialize;
begin
  inherited Initialize;

  FOsdTextItems := TOsdTextList.Create;
  PrintOsd(Format('POISK v%s', [Version]));

  HandleCommandLine;

  FAudioBuffer := TRingBuffer.Create(Self, (SpeakerSampleRate div 4) - 1);

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

  FTarget := LoadRenderTexture(640, 400);

  if Settings.Video.TextureFilter then
    SetTextureFilter(FTarget.Texture, TEXTURE_FILTER_BILINEAR);

  GfxShader := LoadFragmentShaderFromResource('GFX_SHADER');

  Font := LoadFontFromResource('FONT', '.ttf');

  FKeyboardMap := TKeyboardMap.Create;
  BuildKeyboardMap;

  Computer := BuildMachine;
end;

procedure TApplication.HandleCommandLine;
var
  Option, Extension: String;
  DiskStream: TStream;
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
            PrintOsd('[BIN] ' + FWorkingFileName);
          except
            on E: Exception do
            begin
              PrintOsd('[BIN] ' + E.Message);
              FWorkingFileName := String.Empty;
              FreeAndNil(FBinStream);
            end;
          end;
        end;

      '.img':
        begin
          DiskStream := TFileStream.Create(Option, fmOpenReadWrite);
          try
            Insert(DiskStream, FFloppyDiskStreams, Integer.MaxValue);
            FWorkingFileName := Option;
            PrintOsd('[FDD] ' + FWorkingFileName);
          except
            on E: Exception do
            begin
              PrintOsd('[FDD] ' + E.Message);
              FWorkingFileName := String.Empty;
              FreeAndNil(DiskStream);
            end;
          end;
        end;

    else
      PrintOsd(Format('[Error] Unknown file: %s', [Option]));
    end;
  end;
end;

procedure TApplication.RunMachine;
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

  procedure HandleEmulatorKeys;
  begin
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

    if IsKeyPressed(KEY_T) then
    begin
      Settings.Machine.Turbo := not Settings.Machine.Turbo;
      PrintOsd('[POISK] Turbo '
        + BoolToStr(Settings.Machine.Turbo, 'on', 'off'));
    end;

    if IsKeyPressed(KEY_B) then
    begin
      Settings.Video.PalletteBug := not Settings.Video.PalletteBug;
      Computer.Video.PoiskPalletteBug := Settings.Video.PalletteBug;
      PrintOsd('[Video] Pallette bug '
        + BoolToStr(Settings.Video.PalletteBug, 'on', 'off'));
    end;

    if IsKeyPressed(KEY_Y) then
    begin
      Settings.Video.TextureFilter := not Settings.Video.TextureFilter;
      SetTextureFilter(FTarget.Texture, specialize IfThen<TTextureFilter>(
        Settings.Video.TextureFilter, TEXTURE_FILTER_BILINEAR, TEXTURE_FILTER_POINT));
      PrintOsd('[Video] Texture filter '
        + BoolToStr(Settings.Video.PalletteBug, 'on', 'off'));
    end;

    if IsKeyPressed(KEY_P) then Paused := not Paused;

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
  end;

const
  TapeFrequency = 8000;
var
  LinesLoc, I, Underruns, CyclesPerSpeakerSample: Integer;
  LinesCount: Single = 400.0;
  PrevTicks: QWord = 0;
  ScanlinesEnabled, GrayscaleEnabled, CyclesPerFrame: Integer;
  TargetRectangle: TRectangle;
  CyclesPerCassetteSample: Integer;
  TapeDelta: Double;
  WaitStates, InitialWaitStates: Integer;
  VirtualTime, TimePerCycle, BaseTimePerSpeakerSample, TimePerSpeakerSample: Double;
  AudioTimeAdjustment: Double;

  procedure UpdateOsd;
  var
    I: Integer;
  begin
    for I := (FOsdTextItems.Count - 1) downto 0 do
    begin
      FOsdTextItems[I].Lifetime := FOsdTextItems[I].Lifetime - GetFrameTime;
      if FOsdTextItems[I].Lifetime <= 0 then
        FOsdTextItems.Remove(FOsdTextItems[I]);
    end;
  end;

  procedure UpdateAudio;
  begin
    case Computer.CassetteDrive.State of
      csPlaying:
        FAudioBuffer.Write(IfThen(Computer.CassetteDrive.TapeIn, 63));

      csRecording:
        FAudioBuffer.Write(IfThen(Computer.CassetteDrive.TapeOut, 63));
    else
      FAudioBuffer.Write(IfThen(Computer.Timer.Speaker.Output, 0, 255));
    end;
  end;

begin
  VirtualTime := 0;
  TimePerCycle := 1 / Settings.Machine.ClockSpeed;
  TimePerSpeakerSample := 1 / SpeakerSampleRate;
  BaseTimePerSpeakerSample := TimePerSpeakerSample;

  AudioTimeAdjustment := 1;

  Computer.Cpu.InterruptHook := @InterruptHook;
  Computer.Cpu.OnBeforeInstruction := @OnBeforeInstruction;
  Computer.Cpu.OnAfterInstruction := @OnAfterInstruction;
  Computer.Cpu.OnBeforeExecution := @OnBeforeExecution;

  Debugger.Machine := Computer;
  Debugger.Start;

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
    UpdateOsd;

    if not IsKeyDown(KEY_F11) then
      UpdateKeyboard(FKeyboard)
    else
    begin
      if IsKeyPressed(KEY_ESCAPE) then Break;
      HandleEmulatorKeys;
    end;

    GrayscaleEnabled := IfThen(Settings.Video.Grayscale, 1, 0);
    ScanlinesEnabled := IfThen(Settings.Video.ScanLines, 1, 0);

    SetShaderValue(GfxShader,
      GetShaderLocation(GfxShader, 'enableGrayscale'),
      @GrayscaleEnabled, SHADER_UNIFORM_INT);

    SetShaderValue(GfxShader,
      GetShaderLocation(GfxShader, 'enableScanlines'),
      @ScanlinesEnabled, SHADER_UNIFORM_INT);

    Inc(FFrames);

    PrevTicks := Computer.Cpu.Ticks;

    if FAudioBuffer.FilledPercentage < 0.01 then
    begin
      while FAudioBuffer.FilledPercentage < 0.2 do FAudioBuffer.Write(0);
      AudioTimeAdjustment := AudioTimeAdjustment * 0.9965;
    end else
    if FAudioBuffer.FilledPercentage > 0.99 then
    begin
      while FAudioBuffer.FilledPercentage > 0.4 do FAudioBuffer.Read;
      AudioTimeAdjustment := AudioTimeAdjustment / 0.996;
    end;

    TimePerSpeakerSample := EnsureRange(
      BaseTimePerSpeakerSample * AudioTimeAdjustment,
      BaseTimePerSpeakerSample * 0.5,
      BaseTimePerSpeakerSample * 2.0);

    Computer.Video.BeginFrame;

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

    Computer.Cpu.WaitStates := IfThen(Settings.Machine.Turbo, 0, InitialWaitStates);
    try
      for I := 0 to CyclesPerFrame - 1 do
      begin
        if Paused or (Computer.ExecutionMode <> emNormal) then Break;

        VirtualTime := VirtualTime + TimePerCycle;
        Computer.Tick;
        if (Computer.Cpu.WaitStates <= 0) and not Settings.Machine.Turbo then
          Computer.Cpu.WaitStates := WaitStates;

        if (VirtualTime - FLastAudioSampleTime) >= TimePerSpeakerSample then
        begin
          UpdateAudio;
          FLastAudioSampleTime := VirtualTime;
        end;

        if (Computer.CassetteDrive.State in [csPlaying, csRecording]) and
            ((Computer.Cpu.Ticks mod CyclesPerCassetteSample) = 0) then
          { Time to update the state of tape }
          Computer.CassetteDrive.Tick(TapeDelta);
      end;

    except
      on E: Exception do
        begin
          Writeln('*** ERROR: ', E.Message, ' ***');
          Writeln(Computer.Cpu.DumpCurrentInstruction);
          Break;
        end;
    end;

    BeginTextureMode(FTarget);
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
        FTarget.texture,
        RectangleCreate(0, 0, FTarget.texture.width, -FTarget.texture.height),
        TargetRectangle,
        Vector2Zero, 0, WHITE);

      EndShaderMode;

      RenderOsd;

      {
      DrawTextEx(Font,
        PChar(Format('Audio buf: %.0f', [FAudioBuffer.FilledPercentage * 100])),
        Vector2Create(500, 0),
        24, 0, YELLOW);
      }

    EndDrawing;
  end;
end;

procedure TApplication.SetDebugger(AValue: TWebDebugger);
begin
  if FDebugger = AValue then Exit;
  FDebugger := AValue;
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
  if Assigned(FBinStream) then
  begin
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
    Result := True;
  end;
end;

function TApplication.HandleDiskIO: Boolean;
var
  DriveNumber, Cylinder, Head, Sector, SectorCount: Integer;
const
  Verbose = True;
begin
  Result := False;

  if not Assigned(Computer.FloppyDiskController) then Exit;

  DriveNumber := Computer.Cpu.Registers.DL;
  Cylinder := Computer.Cpu.Registers.CH
    or ((Computer.Cpu.Registers.CL and $C0) shl 2);
  Head := Computer.Cpu.Registers.DH;
  Sector := Computer.Cpu.Registers.CL and $3F;
  SectorCount := Computer.Cpu.Registers.AL;

  if Verbose then
    Write(Format('[%.4x:%.4x] INT 0x13 (AH=0x%.X): ', [
      Computer.Cpu.Registers.CS,
      Computer.Cpu.Registers.IP,
      Computer.Cpu.Registers.AH
    ]));

  case Computer.Cpu.Registers.AH of
    $02:
      { Read sectors }
      begin
        if Verbose then
          Write(
            Format(
              'Drive %d: READ %d sector(s) from ' +
              'CHS %d:%d:%d, load to %.4x:%.4x',
            [
              DriveNumber, SectorCount, Cylinder, Head, Sector,
              Computer.Cpu.Registers.ES, Computer.Cpu.Registers.BX
            ]));
      end;

    $03:
      begin
        { Disk write }
        if Verbose then
          Write(
            Format(
              'Drive %d: WRITE %d sector(s) to ' +
              'CHS %d:%d:%d, load from %.4x:%.4x',
            [
              DriveNumber, SectorCount, Cylinder, Head, Sector,
              Computer.Cpu.Registers.ES, Computer.Cpu.Registers.BX
            ]));

      end;
  else;
  end;

  if Verbose then Writeln;
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

procedure TApplication.RenderOsd;
var
  I: Integer;
  Item: TOsdText;
begin
  for I := 0 to FOsdTextItems.Count - 1 do
  begin
    Item := FOsdTextItems[I];

    { DrawRectangle(0, I*40, 400, 40, ColorAlpha(BLACK, Min(Item.Lifetime / 3, 0.8))); }
    DrawTextEx(Font,
      PChar(Item.Text),
      Vector2Create(0, I * 40), 36, 0,
      ColorAlpha(Item.Color, Min(Item.Lifetime / 3, 1)));
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
    Add(keyScrollLock, [KEY_SCROLL_LOCK]);
    Add(keyPauseBreak, [KEY_PAUSE]);
    Add(keyTab, [KEY_TAB]);
  end;
end;

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
  I: Word;
  Data: Byte;
begin
  Result := False;
  case ANumber of
    $13:
      Result := HandleDiskIO;

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

  if AAddress < 1024 then
    raise Exception.CreateFmt(
      'Execution at IVT area: %.5x. This can''t be right, terminating.',
      [AAddress]);
end;

procedure TApplication.OnBeforeExecution(ASender: TObject; AInstruction: TInstruction);
var
  Cpu: TCpu8088 absolute ASender;
  DumpFrame: Dump.TDumpFrame;
  Data: Byte;
  Crc: Word;
{$J+}
const
  InsRep: Boolean = False;
{$J-}
begin
  {
  if (AInstruction.DS = $1000) and (AInstruction.IP = $0) and (AInstruction.Code[0] = $AC) then
  begin
    DumpMemory('/tmp/run.dump', Computer.MemoryBus, 0, (256*1024)-1);
    Halt;
  end;
  }

  if not Assigned(FDumpStream) then Exit;
  if not DumpBios and (AInstruction.CS > $A000) then Exit;

  //if AInstruction.CS <> $0070 then Exit;

  if (AInstruction.Repeating) and InsRep then Exit;

  InsRep := AInstruction.Repeating;

  DumpFrame := BuildDumpFrame(Cpu, AInstruction.CS, AInstruction.IP);
  DumpFrame.CS := AInstruction.CS;
  DumpFrame.IP := AInstruction.IP;
  FDumpStream.Write(DumpFrame, SizeOf(DumpFrame));
end;

procedure TApplication.SetPaused(AValue: Boolean);
begin
  if FPaused = AValue then Exit;
  FPaused := AValue;
end;

procedure TApplication.OnAfterInstruction(ASender: TObject; AInstruction: TInstruction);
var
  Line: String;
  Data: Byte;
  Frame: TDumpFrame;
  Cpu: TCpu8088 absolute ASender;
begin
  //if (AInstruction.CS = $70) then FDebug := True;

  if (FDebug) then
    WriteLn(Format('%.4x:%.4x | %s ',
    [
      AInstruction.CS, AInstruction.IP,
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

procedure TApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TApplication;

procedure AudioCallback(Buffer: Pointer; Frames: LongWord); cdecl;
var
  I: Integer;
  OutBuf: PByte;
  Sample: Byte;
begin
  OutBuf := PByte(Buffer);
  Sample := 0;
  for I := 0 to Frames - 1 do
  begin
    Sample := Application.FAudioBuffer.Read(127);
    OutBuf[I] := Sample;
  end;
end;

{$R *.res}

begin
  SetExceptionMask(GetExceptionMask + [exInvalidOp, exOverflow, exZeroDivide]);

  Application := TApplication.Create(Nil);
  Application.Title := 'Poisk';

  Application.Initialize;
  Application.Run;
  FreeAndNil(Application);
end.

