{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pas8088;

{$warn 5023 off : no warning about unused units}
interface

uses
  Cpu8088, Hardware, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pas8088', @Register);
end.
