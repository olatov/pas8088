program TestRunner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, FPJson, JsonParser, ZStream, PTCCrt,
  Cpu8088, TestMachine;

type

  { TApplication }

  TApplication = class(TCustomApplication)
  private
    FFlagMask: Word;
    procedure SetFlagMask(AValue: Word);
  protected
    property FlagMask: Word read FFlagMask write SetFlagMask;
    function BuildFlagMask(AFlags: String): Word;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure RunTestFile(AFileName: String);
  end;

{ TApplication }

procedure TApplication.SetFlagMask(AValue: Word);
begin
  if FFlagMask = AValue then Exit;
  FFlagMask := AValue;
end;

function TApplication.BuildFlagMask(AFlags: String): Word;
var
  Flag: Char;
begin
  Result := 0;
  for Flag in AFlags.ToLower.Trim do
    case Flag of
      's': Result.SetBit(Ord(TFlagRegister.TFlags.flagS));
      'z': Result.SetBit(Ord(TFlagRegister.TFlags.flagZ));
      'p': Result.SetBit(Ord(TFlagRegister.TFlags.flagP));
      'c': Result.SetBit(Ord(TFlagRegister.TFlags.flagC));
      'o': Result.SetBit(Ord(TFlagRegister.TFlags.flagO));
      'a': Result.SetBit(Ord(TFlagRegister.TFlags.flagA));
      'd': Result.SetBit(Ord(TFlagRegister.TFlags.flagD));
      'i': Result.SetBit(Ord(TFlagRegister.TFlags.flagI));
      't': Result.SetBit(Ord(TFlagRegister.TFlags.flagT));
    else
      raise Exception.CreateFmt('Invalid flag: "%s"', [Flag]);
    end;
end;

procedure TApplication.DoRun;
var
  ErrorMsg: String;
  TestFile: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('f:h', ['flags:', 'help']);
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('f', 'flags') then
    FlagMask := BuildFlagMask(GetOptionValue('f', 'flags'))
  else
    FlagMask := $FFFF;

  for TestFile in GetNonOptions('f:h', ['flags:', 'help']) do
    RunTestFile(TestFile);

  // stop program loop
  Terminate;
end;

constructor TApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

procedure TApplication.RunTestFile(AFileName: String);
var
  Machine: TTestMachine;
  TestJson: TJsonData = Nil;
  TestFileStream: TGZFileStream;
  TestStream: TMemoryStream;
  TestsArray: TJSONArray;
  Item: TJSONEnum;
  Test: TTest;
  Errors: TStringArray;
  TestError: String;
begin
  try
    Machine := TTestMachine.Create(Self);
    try
      Writeln('Test file: ', AFileName);

      TestFileStream := TGZFileStream.Create(AFileName, gzopenread);
      TestStream := TMemoryStream.Create;
      TestStream.CopyFrom(TestFileStream, 0);
      FreeAndNil(TestFileStream);
      TestStream.Seek(0, soBeginning);
      try
        TestJson := GetJSON(TestStream);
        TestsArray := TestJson as TJSONArray;
        if not Assigned(TestsArray) then
          raise Exception.Create('Test file is not valid');

        for Item in TestsArray do
        begin
          Test := TTest.Create(Self);
          try
            Test.LoadTest(Item.Value);

            TextColor(7);
            Write(Test.ToString, ' | ');

            if Machine.RunTest(Test, Errors, FlagMask) then
            begin
              TextColor(2);
              Writeln('OK');
              TextColor(7);
            end
            else
            begin
              TextColor(4);
              Writeln('ERROR');
              TextColor(14);
              for TestError in Errors do
                Writeln('| --> ', TestError);
              TextColor(7);
              Halt(1);
            end;
          finally
            FreeAndNil(Test);
          end;
        end;

      finally
        FreeAndNil(TestStream);
      end;
    finally
      FreeAndNil(Machine);
    end;
  except
    on E: Exception do
      begin
        Writeln('ERROR: ', E.Message);
        Halt(1);
      end;
  end;
end;

var
  Application: TApplication;
begin
  Application := TApplication.Create(nil);
  Application.Title := 'Test Runner';
  try
    Application.Run;
  finally
    FreeAndNil(Application);
  end;
end.

