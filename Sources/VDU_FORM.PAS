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
unit Vdu_form;

interface

uses
  Forms, Graphics;

type
  TformVdu = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations  }
    xx : Integer;
    yy : Integer;
    zz : Integer;

    procedure drawForm;
    procedure reScale;
  end;

var
  formVdu: TformVdu;

implementation

uses Assem;

{$R *.DFM}

procedure TFormVdu.reScale;
begin
  canvas.pen.color := clBlack;
  canvas.brush.color := clBlack;
  canvas.rectangle(0, 0, formVdu.ClientWidth, formVdu.ClientHeight);
  xx := formVdu.ClientWidth  div 16;
  yy := formVdu.ClientHeight div 4;
end;

procedure TFormVdu.drawForm;
var x, y : Integer;
begin
  canvas.font.color := clWhite;
  canvas.pen.color := clBlack;

  for x := 0 to 15 do
  begin
    for y := 0 to 3 do
    begin
      if aaa.ram[16 * ($C + y) + x] = Nil then
      begin
        canvas.textOut(xx * x, zz + yy * y, ' ')
      end
      else
      begin
        canvas.textOut(xx * x, zz + yy * y, chr(aaa.ram[16 * ($C + y) + x].opHex))
      end
    end
  end
end;

procedure TformVdu.FormActivate(Sender: TObject);
begin
  reScale;
  invalidate
end;

procedure TformVdu.FormResize(Sender: TObject);
begin
  reScale;
  invalidate
end;

procedure TformVdu.FormPaint(Sender: TObject);
begin
  drawForm
end;

end.
