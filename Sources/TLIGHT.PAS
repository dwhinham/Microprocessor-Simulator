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
unit Tlight;

interface

uses
  Forms, StdCtrls, Classes, Controls, ExtCtrls, Graphics;

type
  TformTlight = class(TForm)                   
    theLights: TImage;
    tlightLabel: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }

    procedure doLights;
  end;

var
  formTlight: TformTlight;

implementation

Uses Assem, MainForm;

{$R *.DFM}

{ ---------------------------------------------------------------------- }

procedure TformTlight.doLights;

const lxRed = 41;
      lyRed = 24;
      lxAmb = 41;
      lyAmb = 45;
      lxGrn = 41;
      lyGrn = 66;

      rxRed = 132;
      ryRed = 24;
      rxAmb = 132;
      ryAmb = 45;
      rxGrn = 132;
      ryGrn = 66;

begin
  tlightLabel.Caption := aaa.h2b(aaa.regs[0]);

  if aaa.regs[0] AND $80 <> 0 then
  begin
    theLights.canvas.brush.color := clRed
  end
  else
  begin
    theLights.canvas.brush.color := clBlack
  end;

  theLights.canvas.Ellipse(lxRed,       lyRed,
                           lxRed + 20,  lyRed + 20);

  if aaa.regs[0] AND $40 <> 0 then
  begin
    theLights.canvas.brush.color := clYellow
  end
  else
  begin
    theLights.canvas.brush.color := clBlack
  end;

  theLights.canvas.Ellipse(lxAmb,       lyAmb,
                           lxAmb + 20,  lyAmb + 20);

  if aaa.regs[0] AND $20 <> 0 then
  begin
    theLights.canvas.brush.color := clLime
  end
  else
  begin
    theLights.canvas.brush.color := clBlack
  end;

  theLights.canvas.Ellipse(lxGrn,       lyGrn,
                           lxGrn + 20,  lyGrn + 20);

  if aaa.regs[0] AND $10 <> 0 then
  begin
    theLights.canvas.brush.color := clRed
  end
  else
  begin
    theLights.canvas.brush.color := clBlack
  end;

  theLights.canvas.Ellipse(rxRed,       ryRed,
                           rxRed + 20,  ryRed + 20);

  if aaa.regs[0] AND $8 <> 0 then
  begin
    theLights.canvas.brush.color := clYellow
  end
  else
  begin
    theLights.canvas.brush.color := clBlack
  end;

  theLights.canvas.Ellipse(rxAmb,       ryAmb,
                           rxAmb + 20,  ryAmb + 20);

  if aaa.regs[0] AND $4 <> 0 then
  begin
    theLights.canvas.brush.color := clLime
  end
  else
  begin
    theLights.canvas.brush.color := clBlack
  end;

  theLights.canvas.Ellipse(rxGrn,       ryGrn,
                           rxGrn + 20,  ryGrn + 20)
end;

end.

