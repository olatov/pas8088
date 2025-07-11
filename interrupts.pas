unit Interrupts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Hardware;

type

  { TNmiTrigger }

  TNmiTrigger = class(TComponent, INmiTrigger)
  private
    FCpu: ICpu;
    procedure SetCpu(AValue: ICpu);
  private
    property Cpu: ICpu read FCpu write SetCpu;
  public
    procedure AttachCpu(ACpu: ICpu);
    procedure RaiseNmi;
  end;

implementation

{ TNmiTrigger }

procedure TNmiTrigger.SetCpu(AValue: ICpu);
begin
  FCpu := AValue;
end;

procedure TNmiTrigger.AttachCpu(ACpu: ICpu);
begin
  Cpu := ACpu;
end;

procedure TNmiTrigger.RaiseNmi;
begin
  if Assigned(Cpu) then Cpu.RaiseNmi;
end;

end.

