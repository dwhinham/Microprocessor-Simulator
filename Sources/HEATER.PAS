(*
    This file is part of the Microprocessor Simulator Version 5.

    Copyright : C N Bauers 2009
    Author    : C N Bauers
    Contact   : nbauers@samphire.demon.co.uk

    This Microprocessor Simulator is free software: you can redistribute
    it and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This Microprocessor Simulator is distributed in the hope that it will
    be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this Microprocessor Simulator.

    If not, see <http://www.gnu.org/licenses/>.
*)
unit Heater;

interface

uses
  Forms, ExtCtrls, StdCtrls, Classes, Controls, Graphics, sysUtils;

type
  TformHeat = class(TForm)
    Image1: TImage;
    labelIO: TLabel;
    labelOnOff: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    temperature : real;
    heaterOn : Boolean;

    procedure drawHooter;
    procedure hooter;
    function  thermostatOn : Boolean;
  end;

var
  formHeat: TformHeat;

implementation


Uses mainForm, assem;

{$R *.DFM}

{ **** thermostat ****************************************************** }

function TformHeat.thermostatOn : Boolean;
begin
  if (temperature < 22) then
    result := false
  else
    result := true
end;

{ **** draw hooter ***************************************************** }

procedure TformHeat.drawHooter;
var strTemp : String;
begin
  if (temperature <  0) then
    temperature := 0;

  if (temperature > 40) then
    temperature := 40;

  Image1.canvas.pen.color := clWhite;
  Image1.canvas.pen.width := 2;
  Image1.canvas.moveto(170, 150);
  Image1.canvas.lineto(170, 30);

  Image1.canvas.pen.color := clRed;
  Image1.canvas.pen.width := 2;
  Image1.canvas.moveto(170, 150);
  Image1.canvas.lineto(170, 150 - round((130 div 40) * temperature));

  str(temperature:0:2, strTemp);
  Image1.canvas.textOut(80, 172, '   ' + strTemp + ' C   ');

  if heaterOn AND thermostatOn then
  begin
    labelIO.Caption := aaa.h2b($81)
  end
  else if heaterOn then
  begin
    labelIO.Caption := aaa.h2b($80)
  end
  else if thermostatOn then
  begin
    labelIO.Caption := aaa.h2b(1)
  end
  else
  begin
    labelIO.Caption := aaa.h2b(0)
  end
end;

{ **** hooter ********************************************************** }

procedure TformHeat.hooter;
begin
  if aaa.regs[0] AND $80 <> 0 then
  begin
    heaterOn := True;
    labelOnOff.color := clRed;
    labelOnOff.font.color := clYellow;
    labelOnOff.caption := '  ON';
    if thermostatOn then
    begin
      labelIO.Caption := aaa.h2b(aaa.regs[0] OR 1)
    end
    else
    begin
      labelIO.Caption := aaa.h2b(aaa.regs[0])
    end
  end
  else
  begin
    heaterOn := false;
    labelOnOff.color := clBtnFace;
    labelOnOff.font.color := clBlack;
    labelOnOff.caption := '  OFF';
    if thermostatOn then
    begin
      labelIO.Caption := aaa.h2b(aaa.regs[0] OR 1)
    end
    else
    begin
      labelIO.Caption := aaa.h2b(aaa.regs[0])
    end
  end
end;

procedure TformHeat.formCreate;
begin
  temperature := 0;
  heaterOn    := false;
end;

procedure TformHeat.Timer1Timer(Sender: TObject);
begin
  if heaterOn then
    temperature := temperature + (40 - temperature) / 40
  else
    temperature := temperature - temperature / 40;

  drawHooter
end;

procedure TformHeat.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  temperature := round((148 - Y) / (118 / 40));
  drawHooter
end;

end.

