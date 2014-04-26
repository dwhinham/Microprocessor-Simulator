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
unit Sevseg;

interface

uses
  Forms, StdCtrls, Classes, Controls, ExtCtrls, Graphics;

type
  TformSevenSeg = class(TForm)
    sevenSegImage: TImage;
    sevSegLabel: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }

    procedure sevenseg;
  end;

var
  formSevenSeg: TformSevenSeg;
  segLeft, segRight : Integer;

implementation

Uses mainForm, assem;

{$R *.DFM}

{ **** sevenseg displays *********************************************** }

procedure TformSevenSeg.sevenseg;

begin
  sevSegLabel.Caption := aaa.h2b(aaa.regs[0]);

  sevenSegImage.canvas.pen.width := 1;

  if aaa.regs[0] AND 1 <> 0 then
  begin
    sevSeg.segRight := aaa.regs[0]
  end
  else
  begin
    sevSeg.segLeft := aaa.regs[0]
  end;

  if sevSeg.segRight AND $80 <> 0 then       { Right top horizontal }
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(124, 47);
    sevenSegImage.canvas.lineto(156, 47);
    sevenSegImage.canvas.moveto(125, 48);
    sevenSegImage.canvas.lineto(155, 48);
    sevenSegImage.canvas.moveto(126, 49);
    sevenSegImage.canvas.lineto(154, 49);
    sevenSegImage.canvas.moveto(127, 50);
    sevenSegImage.canvas.lineto(153, 50);
    sevenSegImage.canvas.moveto(128, 51);
    sevenSegImage.canvas.lineto(152, 51)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(124, 47);
    sevenSegImage.canvas.lineto(156, 47);
    sevenSegImage.canvas.moveto(125, 48);
    sevenSegImage.canvas.lineto(155, 48);
    sevenSegImage.canvas.moveto(126, 49);
    sevenSegImage.canvas.lineto(154, 49);
    sevenSegImage.canvas.moveto(127, 50);
    sevenSegImage.canvas.lineto(153, 50);
    sevenSegImage.canvas.moveto(128, 51);
    sevenSegImage.canvas.lineto(152, 51)
  end;

  if sevSeg.segRight AND $40 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(122, 49);
    sevenSegImage.canvas.lineto(122, 79);
    sevenSegImage.canvas.moveto(123, 50);
    sevenSegImage.canvas.lineto(123, 78);
    sevenSegImage.canvas.moveto(124, 51);
    sevenSegImage.canvas.lineto(124, 77);
    sevenSegImage.canvas.moveto(125, 52);
    sevenSegImage.canvas.lineto(125, 76);
    sevenSegImage.canvas.moveto(126, 53);
    sevenSegImage.canvas.lineto(126, 75)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(122, 49);
    sevenSegImage.canvas.lineto(122, 79);
    sevenSegImage.canvas.moveto(123, 50);
    sevenSegImage.canvas.lineto(123, 78);
    sevenSegImage.canvas.moveto(124, 51);
    sevenSegImage.canvas.lineto(124, 77);
    sevenSegImage.canvas.moveto(125, 52);
    sevenSegImage.canvas.lineto(125, 76);
    sevenSegImage.canvas.moveto(126, 53);
    sevenSegImage.canvas.lineto(126, 75)
  end;

  if sevSeg.segRight AND $20 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(122, 84);
    sevenSegImage.canvas.lineto(122, 113);
    sevenSegImage.canvas.moveto(123, 85);
    sevenSegImage.canvas.lineto(123, 112);
    sevenSegImage.canvas.moveto(124, 86);
    sevenSegImage.canvas.lineto(124, 111);
    sevenSegImage.canvas.moveto(125, 87);
    sevenSegImage.canvas.lineto(125, 110);
    sevenSegImage.canvas.moveto(126, 88);
    sevenSegImage.canvas.lineto(126, 109)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(122, 84);
    sevenSegImage.canvas.lineto(122, 113);
    sevenSegImage.canvas.moveto(123, 85);
    sevenSegImage.canvas.lineto(123, 112);
    sevenSegImage.canvas.moveto(124, 86);
    sevenSegImage.canvas.lineto(124, 111);
    sevenSegImage.canvas.moveto(125, 87);
    sevenSegImage.canvas.lineto(125, 110);
    sevenSegImage.canvas.moveto(126, 88);
    sevenSegImage.canvas.lineto(126, 109)
  end;

  if sevSeg.segRight AND $10 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(124, 114);
    sevenSegImage.canvas.lineto(156, 114);
    sevenSegImage.canvas.moveto(125, 113);
    sevenSegImage.canvas.lineto(155, 113);
    sevenSegImage.canvas.moveto(126, 112);
    sevenSegImage.canvas.lineto(154, 112);
    sevenSegImage.canvas.moveto(127, 111);
    sevenSegImage.canvas.lineto(153, 111);
    sevenSegImage.canvas.moveto(128, 110);
    sevenSegImage.canvas.lineto(152, 110)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(124, 114);
    sevenSegImage.canvas.lineto(156, 114);
    sevenSegImage.canvas.moveto(125, 113);
    sevenSegImage.canvas.lineto(155, 113);
    sevenSegImage.canvas.moveto(126, 112);
    sevenSegImage.canvas.lineto(154, 112);
    sevenSegImage.canvas.moveto(127, 111);
    sevenSegImage.canvas.lineto(153, 111);
    sevenSegImage.canvas.moveto(128, 110);
    sevenSegImage.canvas.lineto(152, 110)
  end;

  if sevSeg.segRight AND $8 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(157, 84);
    sevenSegImage.canvas.lineto(157, 113);
    sevenSegImage.canvas.moveto(156, 85);
    sevenSegImage.canvas.lineto(156, 112);
    sevenSegImage.canvas.moveto(155, 86);
    sevenSegImage.canvas.lineto(155, 111);
    sevenSegImage.canvas.moveto(154, 87);
    sevenSegImage.canvas.lineto(154, 110);
    sevenSegImage.canvas.moveto(153, 88);
    sevenSegImage.canvas.lineto(153, 109)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(157, 84);
    sevenSegImage.canvas.lineto(157, 113);
    sevenSegImage.canvas.moveto(156, 85);
    sevenSegImage.canvas.lineto(156, 112);
    sevenSegImage.canvas.moveto(155, 86);
    sevenSegImage.canvas.lineto(155, 111);
    sevenSegImage.canvas.moveto(154, 87);
    sevenSegImage.canvas.lineto(154, 110);
    sevenSegImage.canvas.moveto(153, 88);
    sevenSegImage.canvas.lineto(153, 109)
  end;

  if sevSeg.segRight AND $4 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(125, 79);
    sevenSegImage.canvas.lineto(155, 79);
    sevenSegImage.canvas.moveto(124, 80);
    sevenSegImage.canvas.lineto(156, 80);

    sevenSegImage.canvas.moveto(124, 82);
    sevenSegImage.canvas.lineto(156, 82);
    sevenSegImage.canvas.moveto(125, 83);
    sevenSegImage.canvas.lineto(155, 83)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(125, 79);
    sevenSegImage.canvas.lineto(155, 79);
    sevenSegImage.canvas.moveto(124, 80);
    sevenSegImage.canvas.lineto(156, 80);

    sevenSegImage.canvas.moveto(124, 82);
    sevenSegImage.canvas.lineto(156, 82);
    sevenSegImage.canvas.moveto(125, 83);
    sevenSegImage.canvas.lineto(155, 83)
  end;

  if sevSeg.segRight AND $2 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(157, 49);
    sevenSegImage.canvas.lineto(157, 79);
    sevenSegImage.canvas.moveto(156, 50);
    sevenSegImage.canvas.lineto(156, 78);
    sevenSegImage.canvas.moveto(155, 51);
    sevenSegImage.canvas.lineto(155, 77);
    sevenSegImage.canvas.moveto(154, 52);
    sevenSegImage.canvas.lineto(154, 76);
    sevenSegImage.canvas.moveto(153, 53);
    sevenSegImage.canvas.lineto(153, 75)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(157, 49);
    sevenSegImage.canvas.lineto(157, 79);
    sevenSegImage.canvas.moveto(156, 50);
    sevenSegImage.canvas.lineto(156, 78);
    sevenSegImage.canvas.moveto(155, 51);
    sevenSegImage.canvas.lineto(155, 77);
    sevenSegImage.canvas.moveto(154, 52);
    sevenSegImage.canvas.lineto(154, 76);
    sevenSegImage.canvas.moveto(153, 53);
    sevenSegImage.canvas.lineto(153, 75)
  end;

  if sevSeg.segLeft AND $80 <> 0 then       { Right top horizontal }
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(40, 47);
    sevenSegImage.canvas.lineto(72, 47);
    sevenSegImage.canvas.moveto(41, 48);
    sevenSegImage.canvas.lineto(71, 48);
    sevenSegImage.canvas.moveto(42, 49);
    sevenSegImage.canvas.lineto(70, 49);
    sevenSegImage.canvas.moveto(43, 50);
    sevenSegImage.canvas.lineto(69, 50);
    sevenSegImage.canvas.moveto(44, 51);
    sevenSegImage.canvas.lineto(68, 51)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(40, 47);
    sevenSegImage.canvas.lineto(72, 47);
    sevenSegImage.canvas.moveto(41, 48);
    sevenSegImage.canvas.lineto(71, 48);
    sevenSegImage.canvas.moveto(42, 49);
    sevenSegImage.canvas.lineto(70, 49);
    sevenSegImage.canvas.moveto(43, 50);
    sevenSegImage.canvas.lineto(69, 50);
    sevenSegImage.canvas.moveto(44, 51);
    sevenSegImage.canvas.lineto(68, 51)
  end;

  if sevSeg.segLeft AND $40 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(38, 49);
    sevenSegImage.canvas.lineto(38, 79);
    sevenSegImage.canvas.moveto(39, 50);
    sevenSegImage.canvas.lineto(39, 78);
    sevenSegImage.canvas.moveto(40, 51);
    sevenSegImage.canvas.lineto(40, 77);
    sevenSegImage.canvas.moveto(41, 52);
    sevenSegImage.canvas.lineto(41, 76);
    sevenSegImage.canvas.moveto(42, 53);
    sevenSegImage.canvas.lineto(42, 75)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(38, 49);
    sevenSegImage.canvas.lineto(38, 79);
    sevenSegImage.canvas.moveto(39, 50);
    sevenSegImage.canvas.lineto(39, 78);
    sevenSegImage.canvas.moveto(40, 51);
    sevenSegImage.canvas.lineto(40, 77);
    sevenSegImage.canvas.moveto(41, 52);
    sevenSegImage.canvas.lineto(41, 76);
    sevenSegImage.canvas.moveto(42, 53);
    sevenSegImage.canvas.lineto(42, 75)
  end;

  if sevSeg.segLeft AND $20 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(38, 84);
    sevenSegImage.canvas.lineto(38, 113);
    sevenSegImage.canvas.moveto(39, 85);
    sevenSegImage.canvas.lineto(39, 112);
    sevenSegImage.canvas.moveto(40, 86);
    sevenSegImage.canvas.lineto(40, 111);
    sevenSegImage.canvas.moveto(41, 87);
    sevenSegImage.canvas.lineto(41, 110);
    sevenSegImage.canvas.moveto(42, 88);
    sevenSegImage.canvas.lineto(42, 109)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(38, 84);
    sevenSegImage.canvas.lineto(38, 113);
    sevenSegImage.canvas.moveto(39, 85);
    sevenSegImage.canvas.lineto(39, 112);
    sevenSegImage.canvas.moveto(40, 86);
    sevenSegImage.canvas.lineto(40, 111);
    sevenSegImage.canvas.moveto(41, 87);
    sevenSegImage.canvas.lineto(41, 110);
    sevenSegImage.canvas.moveto(42, 88);
    sevenSegImage.canvas.lineto(42, 109)
  end;

  if sevSeg.segLeft AND $10 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(40, 114);
    sevenSegImage.canvas.lineto(72, 114);
    sevenSegImage.canvas.moveto(41, 113);
    sevenSegImage.canvas.lineto(71, 113);
    sevenSegImage.canvas.moveto(42, 112);
    sevenSegImage.canvas.lineto(70, 112);
    sevenSegImage.canvas.moveto(43, 111);
    sevenSegImage.canvas.lineto(69, 111);
    sevenSegImage.canvas.moveto(44, 110);
    sevenSegImage.canvas.lineto(68, 110)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(40, 114);
    sevenSegImage.canvas.lineto(72, 114);
    sevenSegImage.canvas.moveto(41, 113);
    sevenSegImage.canvas.lineto(71, 113);
    sevenSegImage.canvas.moveto(42, 112);
    sevenSegImage.canvas.lineto(70, 112);
    sevenSegImage.canvas.moveto(43, 111);
    sevenSegImage.canvas.lineto(69, 111);
    sevenSegImage.canvas.moveto(44, 110);
    sevenSegImage.canvas.lineto(68, 110)
  end;

  if sevSeg.segLeft AND $8 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(73, 84);
    sevenSegImage.canvas.lineto(73, 113);
    sevenSegImage.canvas.moveto(72, 85);
    sevenSegImage.canvas.lineto(72, 112);
    sevenSegImage.canvas.moveto(71, 86);
    sevenSegImage.canvas.lineto(71, 111);
    sevenSegImage.canvas.moveto(70, 87);
    sevenSegImage.canvas.lineto(70, 110);
    sevenSegImage.canvas.moveto(69, 88);
    sevenSegImage.canvas.lineto(69, 109)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(73, 84);
    sevenSegImage.canvas.lineto(73, 113);
    sevenSegImage.canvas.moveto(72, 85);
    sevenSegImage.canvas.lineto(72, 112);
    sevenSegImage.canvas.moveto(71, 86);
    sevenSegImage.canvas.lineto(71, 111);
    sevenSegImage.canvas.moveto(70, 87);
    sevenSegImage.canvas.lineto(70, 110);
    sevenSegImage.canvas.moveto(69, 88);
    sevenSegImage.canvas.lineto(69, 109)
  end;

  if sevSeg.segLeft AND $4 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(41, 79);
    sevenSegImage.canvas.lineto(71, 79);
    sevenSegImage.canvas.moveto(40, 80);
    sevenSegImage.canvas.lineto(72, 80);

    sevenSegImage.canvas.moveto(40, 82);
    sevenSegImage.canvas.lineto(72, 82);
    sevenSegImage.canvas.moveto(41, 83);
    sevenSegImage.canvas.lineto(71, 83)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(41, 79);
    sevenSegImage.canvas.lineto(71, 79);
    sevenSegImage.canvas.moveto(40, 80);
    sevenSegImage.canvas.lineto(72, 80);

    sevenSegImage.canvas.moveto(40, 82);
    sevenSegImage.canvas.lineto(72, 82);
    sevenSegImage.canvas.moveto(41, 83);
    sevenSegImage.canvas.lineto(71, 83)
  end;

  if sevSeg.segLeft AND $2 <> 0 then
  begin
    sevenSegImage.canvas.pen.color := clYellow;
    sevenSegImage.canvas.moveto(73, 49);
    sevenSegImage.canvas.lineto(73, 79);
    sevenSegImage.canvas.moveto(72, 50);
    sevenSegImage.canvas.lineto(72, 78);
    sevenSegImage.canvas.moveto(71, 51);
    sevenSegImage.canvas.lineto(71, 77);
    sevenSegImage.canvas.moveto(70, 52);
    sevenSegImage.canvas.lineto(70, 76);
    sevenSegImage.canvas.moveto(69, 53);
    sevenSegImage.canvas.lineto(69, 75)
  end
  else
  begin
    sevenSegImage.canvas.pen.color := clBlack;
    sevenSegImage.canvas.moveto(73, 49);
    sevenSegImage.canvas.lineto(73, 79);
    sevenSegImage.canvas.moveto(72, 50);
    sevenSegImage.canvas.lineto(72, 78);
    sevenSegImage.canvas.moveto(71, 51);
    sevenSegImage.canvas.lineto(71, 77);
    sevenSegImage.canvas.moveto(70, 52);
    sevenSegImage.canvas.lineto(70, 76);
    sevenSegImage.canvas.moveto(69, 53);
    sevenSegImage.canvas.lineto(69, 75)
  end
end;

end.
