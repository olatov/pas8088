unit AppSettings;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, IniFiles;

type

  { TSettings }

  TSettings = class
  private
    type

      { TAudioSettings }

      TAudioSettings = record
        private
          FVolume: Double;
          procedure SetVolume(AValue: Double);
        public
          Mute: Boolean;
          property Volume: Double read FVolume write SetVolume;
        end;
      public
        Window: record
          Width, Height: Integer;
          AspectRatio: Double;
          FullScreen: Boolean;
        end;

    Machine: record
      ClockSpeed: Integer;
      CpuSpeed: Integer;
      Ram: Integer;
      BiosRom: String;
    end;

    Video: record
      ScanLines: Boolean;
      GrayScale: Boolean;
    end;

    Audio: TAudioSettings;

    procedure SaveToFile(AFileName: String);
    procedure LoadFromFile(AFileName: String);
    procedure Load;
    procedure Save;
  end;

var
  Settings: TSettings;

implementation

{ TSettings }

procedure TSettings.SaveToFile(AFileName: String);
var
  Config: TIniFile;
begin
  Config := TIniFile.Create(AFileName);
  try
    Config.WriteInteger('Machine', 'ClockSpeed', Machine.ClockSpeed);
    Config.WriteInteger('Machine', 'CpuSpeed', Machine.CpuSpeed);
    Config.WriteInteger('Machine', 'RAM', Machine.Ram);
    Config.WriteString('Machine', 'BiosRom', Machine.BiosRom);

    Config.WriteInteger('Window', 'Width', Window.Width);
    Config.WriteInteger('Window', 'Height', Window.Height);
    Config.WriteFloat('Window', 'AspectRatio', Window.AspectRatio);
    Config.WriteBool('Window', 'FullScreen', Window.FullScreen);

    Config.WriteBool('Video', 'ScanLines', Video.ScanLines);
    Config.WriteBool('Video', 'GrayScale', Video.GrayScale);

    Config.WriteBool('Audio', 'Mute', Audio.Mute);
    Config.WriteFloat('Audio', 'Volume', Audio.Volume);
  finally
    FreeAndNil(Config);
  end;
end;

procedure TSettings.LoadFromFile(AFileName: String);
var
  Config: TIniFile;
begin
  Config := TIniFile.Create(AFileName);
  try
    Machine.ClockSpeed := EnsureRange(
      Config.ReadInteger('Machine', 'ClockSpeed', 1250000),
      125000, 2500000);

    Machine.CpuSpeed := EnsureRange(
      Config.ReadInteger('Machine', 'CpuSpeed', 250000),
      125000, 2500000);
    Machine.CpuSpeed := Min(Machine.ClockSpeed, Machine.CpuSpeed);

    Machine.Ram := EnsureRange(
      Config.ReadInteger('Machine', 'Ram', 640), 128, 640);
    Machine.BiosRom := 'poisk_1991.rom';

    Window.Width := EnsureRange(
      Config.ReadInteger('Window', 'Width', 640),
      640, 3840);

    Window.Height := EnsureRange(
      Config.ReadInteger('Window', 'Height', 400),
      400, 2160);

    Window.AspectRatio := Config.ReadFloat('Window', 'AspectRatio', 0);
    Window.FullScreen := Config.ReadBool('Window', 'FullScreen', False);

    Video.ScanLines := Config.ReadBool('Video', 'ScanLines', True);
    Video.GrayScale := Config.ReadBool('Video', 'GrayScale', False);

    Audio.Mute := Config.ReadBool('Audio', 'Mute', False);
    Audio.Volume := Config.ReadFloat('Audio', 'Volume', 0.25);
  finally
    FreeAndNil(Config);
  end;
end;

procedure TSettings.Load;
begin
  LoadFromFile(GetAppConfigFile(False));
end;

procedure TSettings.Save;
begin
  SaveToFile(GetAppConfigFile(False));
end;

{ TSettings.TAudioSettings }

procedure TSettings.TAudioSettings.SetVolume(AValue: Double);
begin
  if FVolume = AValue then Exit;
  FVolume := EnsureRange(AValue, 0, 1);
end;

initialization
  Settings := TSettings.Create;
  Settings.Load;

finalization
  Settings.Save;
  FreeAndNil(Settings);

end.

