#!/usr/bin/env ifpc

program RunBatch;

{$mode objfpc}{$H+}

uses
  SysUtils, StrUtils, Process, CSVDocument;

function RunTest(ABaseDir, AFileName, AOptions: String): Integer;
var
  OutputString: String;
begin
  RunCommandInDir(
    '.', './testrunner', [ConcatPaths([ABaseDir, AFileName]), AOptions], OutputString, Result);
  if Result <> 0 then Writeln(OutputString);
end;

function RunBatch(AFileName, ABaseDir: String): Integer;
var
  Batch: TCSVDocument;
  I, ExitCode: Integer;
  FileName: String;
begin
  Batch := TCSVDocument.Create;
  try
    Batch.LoadFromFile(AFileName);
    
    for I := 1 to Batch.RowCount - 1 do
    begin
      if not MatchStr(Batch.Cells[1, I].Trim.ToUpper, ['1', 'Y']) then Continue;

      FileName := Batch.Cells[0, I].Trim;
      Writeln('Testing ', FileName, '... ');      
      ExitCode := RunTest(ABaseDir, FileName, Batch.Cells[2, I].Trim);

      if ExitCode <> 0 then
      begin
        Writeln('^^ Failed on ', FileName);
        Halt(ExitCode);
      end;
    end;
      
  finally
    FreeAndNil(Batch);
  end;
  
end;

begin
  RunBatch(ParamStr(1), ParamStr(2));  
end.