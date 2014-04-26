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
unit Maze;

interface

uses
  Forms, Classes, Controls, ExtCtrls, Graphics;

type
  TformMaze = class(TForm)
    mazeImage: TImage;
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }

    Procedure resetMaze;
    Procedure doMaze;
  end;

var
  formMaze: TformMaze;
  x, y    : SmallInt;
  s       : String;

implementation

{$R *.DFM}

Uses mainForm, Assem;


Procedure TformMaze.resetMaze;

begin
  x := 20;
  y := 20;

  mazeImage.canvas.pen.width := 4;

  mazeImage.canvas.pen.color := clBlack;
  mazeImage.canvas.brush.color := clLime;
  mazeImage.canvas.rectangle(0, 0, mazeImage.width, mazeImage.height);
  mazeImage.canvas.moveto(0, 196);
  mazeImage.canvas.lineto(mazeImage.width, 196);
  mazeImage.canvas.moveto(196, 0);
  mazeImage.canvas.lineto(196, 196);

  mazeImage.canvas.font.Name  := 'Courier New';
  mazeImage.canvas.font.size  := 8;
  mazeImage.canvas.font.style := [fsBold];
  mazeImage.canvas.textOut(5, 205, 'Port 04         FF to Reset');
  mazeImage.canvas.textOut(5, 221, 'Up Down Left Right Distance');
  mazeImage.canvas.textOut(5, 237, ' 0    0    0    0   0 0 0 0');
  mazeImage.canvas.textOut(5, 253, '80   40   20   10   8 4 2 1');

  mazeImage.canvas.pen.color := clOlive;
  mazeImage.canvas.moveto(40, 4);
  mazeImage.canvas.lineto(40, 159);

  mazeImage.canvas.moveto(80, 196);
  mazeImage.canvas.lineto(80, 120);

  mazeImage.canvas.moveto(40,  80);
  mazeImage.canvas.lineto(120, 80);
  mazeImage.canvas.lineto(120, 160);

  mazeImage.canvas.moveto(80,  40);
  mazeImage.canvas.lineto(160, 40);
  mazeImage.canvas.lineto(160, 196);

  mazeImage.canvas.pen.color := clRed;
  mazeImage.canvas.pen.width := 1;
  mazeImage.canvas.brush.color := clYellow;
  mazeImage.canvas.Ellipse(x-5, y-5, x+5, y+5)
end;


Procedure TformMaze.doMaze;

Var dist  : SmallInt;
    index : SmallInt;

Begin
  s := ' ';
  if aaa.regs[0] AND $80 <> 0 then s := s + '1' else s := s + '0';
  s := s + '    ';
  if aaa.regs[0] AND $40 <> 0 then s := s + '1' else s := s + '0';
  s := s + '    ';
  if aaa.regs[0] AND $20 <> 0 then s := s + '1' else s := s + '0';
  s := s + '    ';
  if aaa.regs[0] AND $10 <> 0 then s := s + '1' else s := s + '0';
  s := s + '   ';
  if aaa.regs[0] AND  $8 <> 0 then s := s + '1' else s := s + '0';
  s := s + ' ';
  if aaa.regs[0] AND  $4 <> 0 then s := s + '1' else s := s + '0';
  s := s + ' ';
  if aaa.regs[0] AND  $2 <> 0 then s := s + '1' else s := s + '0';
  s := s + ' ';
  if aaa.regs[0] AND  $1 <> 0 then s := s + '1' else s := s + '0';

  if aaa.regs[0] = -1 then
  begin
    resetMaze
  end;

  mazeImage.canvas.font.Name  := 'Courier New';
  mazeImage.canvas.font.size  := 8;
  mazeImage.canvas.font.style := [fsBold];
  mazeImage.canvas.textOut(5, 237, s);

  dist := aaa.regs[0] AND $0F;
  mazeImage.canvas.pen.color := clRed;
  mazeImage.canvas.pen.width := 1;
  mazeImage.canvas.brush.color := clYellow;

  if aaa.regs[0] AND $F0 = $80 then                               { UP }
  begin
    for index := y downto y - dist do
    begin
      if index < 7 then
      begin
        y := 7;
        aaa.continuous := false;
        exit
      end
      else
      begin
        y := index;
      end;

      mazeImage.canvas.Ellipse(x-5, y-5, x+5, y+5)
    end
  end;

  if aaa.regs[0] AND $F0 = $40 then                               { DOWN }
  begin
    for index := y to y + dist do
    begin
      if index > 194 then
      begin
        y := 194;
        aaa.continuous := false;
        exit
      end
      else
      begin
        y := index;
      end;

      mazeImage.canvas.Ellipse(x-5, y-5, x+5, y+5)
    end
  end;

  if aaa.regs[0] AND $F0 = $20 then                               { LEFT }
  begin
    for index := x downto x - dist do
    begin
      if index < 7 then
      begin
        x := 7;
        aaa.continuous := false;
        exit
      end
      else
      begin
        x := index
      end;

      mazeImage.canvas.Ellipse(x-5, y-5, x+5, y+5)
    end
  end;

  if aaa.regs[0] AND $F0 = $10 then                               { RIGHT }
  begin
    for index := x to x + dist do
    begin
      if index > 193 then
      begin
        x := 193;
        aaa.continuous := false;
        exit
      end
      else
      begin
        x := index
      end;

      mazeImage.canvas.Ellipse(x-5, y-5, x+5, y+5)
    end
  end
End;

procedure TformMaze.FormCreate(Sender: TObject);
begin
  resetMaze
end;

end.
