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
unit Stepper;

interface

uses
  Forms, Graphics, Classes, Controls, ExtCtrls, WinProcs;

type
  TformStep = class(TForm)
    stepImage: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pos, oldPos      : SmallInt;
    centreX, centreY : SmallInt;
    radius           : SmallInt;

    procedure step;
  end;

var
  formStep: TformStep;

implementation

Uses mainForm, assem;

{$R *.DFM}

procedure TformStep.FormCreate(Sender: TObject);
Var i       : SmallInt;
    x, y, j : Real;
begin
  pos    := 1;
  oldPos := 1;

  centreX := stepImage.Width div 2;
  centreY := stepImage.Width div 2;
  radius  := stepImage.Width div 3;

  stepImage.canvas.brush.color := clBlack;
  stepImage.canvas.pen.color := clBlack;
  stepImage.canvas.rectangle(0, 0, stepImage.width, stepImage.height);

  stepImage.canvas.brush.color := clTeal;
  stepImage.canvas.pen.color := clTeal;
  stepImage.canvas.ellipse(centreX - Radius, centreY - Radius,
                           centreX + Radius, centreY + Radius);

  stepImage.canvas.brush.color := clLime;
  stepImage.canvas.pen.color := clLime;

  for i := 0 to 35 do
  begin
    x := centreX + Radius * sin((2 * PI) * i / 36);
    y := centreX + Radius * cos((2 * PI) * i / 36);

    stepImage.canvas.ellipse(round(x + 6), round(y + 6),
                             round(x - 6), round(y - 6))
  end;

  stepImage.canvas.pen.width := 10;
  stepImage.canvas.pen.color := clSilver;

  for i := 0 to 8 do
  begin
    x := centreX + (Radius - 12) * sin((2 * PI) * i / 9);
    y := centreY + (Radius - 12) * cos((2 * PI) * i / 9);

    stepImage.canvas.moveto(centreX, centreY);
    stepImage.canvas.lineto(round(x),  round(y))
  end;

  stepImage.canvas.pen.color := clWhite;
  stepImage.canvas.pen.width := 1;

  j := centreX + (Radius + 6) * cos((2 * PI) * 0 / 36) + 20;

  for i := 0 downto -3 do
  begin
    x := centreX + (Radius + 6) * sin((2 * PI) * i / 36);
    y := centreX + (Radius + 6) * cos((2 * PI) * i / 36);

    stepImage.canvas.moveTo(round(x), round(y));
    stepImage.canvas.lineTo(round(x), round(j))
  end
end;

{ ---------------------------------------------------------------------- }

procedure TformStep.step;

Var x, y       : Real;
    i, j, port : SmallInt;
    offset     : Real;

begin
  port := aaa.regs[0];

  stepImage.canvas.pen.width := 1;

  for i := 0 to 35 do
  begin
    x := centreX + Radius * sin((2 * PI) * i / 36);
    y := centreX + Radius * cos((2 * PI) * i / 36);

    if (((i + 0) mod 4) = 0) and ((port AND 1) <> 0) then
    begin
      stepImage.canvas.brush.color := clRed;
      stepImage.canvas.pen.color := clRed;
      stepImage.canvas.ellipse(round(x + 6), round(y + 6),
                               round(x - 6), round(y - 6))
    end
    else if (((i + 0) mod 4) = 0) then
    begin
      stepImage.canvas.brush.color := clLime;
      stepImage.canvas.pen.color := clLime;
      stepImage.canvas.ellipse(round(x + 6), round(y + 6),
                               round(x - 6), round(y - 6))
    end;

    if (((i + 1) mod 4) = 0) AND ((port AND 2) <> 0) then
    begin
      stepImage.canvas.brush.color := clRed;
      stepImage.canvas.pen.color := clRed;
      stepImage.canvas.ellipse(round(x + 6), round(y + 6),
                               round(x - 6), round(y - 6))
    end
    else if (((i + 1) mod 4) = 0) then
    begin
      stepImage.canvas.brush.color := clLime;
      stepImage.canvas.pen.color := clLime;
      stepImage.canvas.ellipse(round(x + 6), round(y + 6),
                               round(x - 6), round(y - 6))
    end;

    if (((i + 2) mod 4) = 0) and ((port AND 4) <> 0) then
    begin
      stepImage.canvas.brush.color := clRed;
      stepImage.canvas.pen.color := clRed;
      stepImage.canvas.ellipse(round(x + 6), round(y + 6),
                               round(x - 6), round(y - 6))
    end
    else if (((i + 2) mod 4) = 0) then
    begin
      stepImage.canvas.brush.color := clLime;
      stepImage.canvas.pen.color := clLime;
      stepImage.canvas.ellipse(round(x + 6), round(y + 6),
                               round(x - 6), round(y - 6))
    end;

    if (((i + 3) mod 4) = 0) and ((port AND 8) <> 0) then
    begin
      stepImage.canvas.brush.color := clRed;
      stepImage.canvas.pen.color := clRed;
      stepImage.canvas.ellipse(round(x + 6), round(y + 6),
                               round(x - 6), round(y - 6))
    end
    else if (((i + 3) mod 4) = 0) then
    begin
      stepImage.canvas.brush.color := clLime;
      stepImage.canvas.pen.color := clLime;
      stepImage.canvas.ellipse(round(x + 6), round(y + 6),
                               round(x - 6), round(y - 6))
    end
  end;

  port := port AND $F;

  case port of
    $1 : begin
           case oldPos of
             $8 : pos := $1;
             $9 : pos := $1;
             $1 : pos := $1;
             $3 : pos := $1;
             $2 : pos := $1
           else
             pos := oldPos
           end
         end;

    $3 : begin
           case oldPos of
             $8 : pos := $3;
             $9 : pos := $3;
             $1 : pos := $3;
             $3 : pos := $3;
             $2 : pos := $3;
             $6 : pos := $3;
             $4 : pos := $3
           else
             pos := oldPos
           end
         end;

    $2 : begin
           case oldPos of
             $1 : pos := $2;
             $3 : pos := $2;
             $2 : pos := $2;
             $6 : pos := $2;
             $4 : pos := $2
           else
             pos := oldPos
           end
         end;

    $6 : begin
           case oldPos of
             $1 : pos := $6;
             $3 : pos := $6;
             $2 : pos := $6;
             $6 : pos := $6;
             $4 : pos := $6;
             $C : pos := $6;
             $8 : pos := $6
           else
             pos := oldPos
           end
         end;

    $4 : begin
           case oldPos of
             $2 : pos := $4;
             $6 : pos := $4;
             $4 : pos := $4;
             $C : pos := $4;
             $8 : pos := $4
           else
             pos := oldPos
           end
         end;

    $C : begin
           case oldPos of
             $2 : pos := $C;
             $6 : pos := $C;
             $4 : pos := $C;
             $C : pos := $C;
             $8 : pos := $C;
             $9 : pos := $C;
             $1 : pos := $C
           else
             pos := oldPos
           end
         end;

    $8 : begin
           case oldPos of
             $4 : pos := $8;
             $C : pos := $8;
             $8 : pos := $8;
             $9 : pos := $8;
             $1 : pos := $8
           else
             pos := oldPos
           end
         end;

    $9 : begin
           case oldPos of
             $4 : pos := $9;
             $C : pos := $9;
             $8 : pos := $9;
             $9 : pos := $9;
             $1 : pos := $9;
             $3 : pos := $9;
             $2 : pos := $9
           else
             pos := oldPos
           end
         end
    else begin
             pos := oldPos;
             messageBeep(0)
         end
  end;

  stepImage.canvas.pen.width := 10;

  case oldPos of
    $1 : offset := 0;
    $3 : offset := -5 / 360;
    $2 : offset := -10 / 360;
    $6 : offset := -15 / 360;
    $4 : offset := -20 / 360;
    $C : offset := -25 / 360;
    $8 : offset := -30 / 360;
    $9 : offset := -35 / 360
  else
         offset := 0
  end;

  stepImage.canvas.pen.color := clTeal;

  for i := 0 to 8 do
  begin
    x := centreX + (Radius - 12) * sin(2 * PI * (offset + i / 9));
    y := centreY + (Radius - 12) * cos(2 * PI * (offset + i / 9));

    stepImage.canvas.moveto(centreX, centreY);
    stepImage.canvas.lineto(round(x),  round(y))
  end;

  case pos of
    $1 : offset := 0;
    $3 : offset := -5 / 360;
    $2 : offset := -10 / 360;
    $6 : offset := -15 / 360;
    $4 : offset := -20 / 360;
    $C : offset := -25 / 360;
    $8 : offset := -30 / 360;
    $9 : offset := -35 / 360
  end;

  stepImage.canvas.pen.color := clSilver;

  for i := 0 to 8 do
  begin
    x := centreX + (Radius - 12) * sin(2 * PI * (offset + i / 9));
    y := centreY + (Radius - 12) * cos(2 * PI * (offset + i / 9));

    stepImage.canvas.moveto(centreX, centreY);
    stepImage.canvas.lineto(round(x),  round(y))
  end;

  i := 1;
  j := 0;
  y := centreX + (Radius + 6) * cos((2 * PI) * j / 36) + 22;

  while i <= $80 do
  begin
    if j >= -3 then
    begin
      x := centreX + (Radius + 6) * sin((2 * PI) * j / 36) - 4
    end
    else
    begin
      x := x - 10;
    end;

    if i = 1 then
    begin
      stepImage.canvas.brush.Color := clLime;
      stepImage.canvas.font.Color := clBlack;
      stepImage.canvas.TextOut(round(x) + 10, round(y), 'LSB')
    end;

    if (aaa.regs[AL] AND i) <> 0 then
    begin
      stepImage.canvas.brush.Color := clRed;
      stepImage.canvas.font.Color := clYellow;
      stepImage.canvas.TextOut(round(x), round(y), '1')
    end
    else
    begin
      stepImage.canvas.brush.Color := clLime;
      stepImage.canvas.font.Color := clBlack;
      stepImage.canvas.TextOut(round(x), round(y), '0')
    end;

    i := i * 2;
    j := j - 1;
  end;

  oldPos := pos
end;

end.
