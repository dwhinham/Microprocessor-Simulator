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
unit ramHex;

interface

uses Forms, StdCtrls, Classes, Controls, ExtCtrls, Graphics, SysUtils;

type
  TFormRamHex = class(TForm)
    Panel1: TPanel;
    RadioButtonHex: TRadioButton;
    RadioButtonASCII: TRadioButton;
    RadioButtonSource: TRadioButton;
    procedure FormResize(Sender: TObject);
    procedure RadioButtonHexClick(Sender: TObject);
    procedure RadioButtonASCIIClick(Sender: TObject);
    procedure RadioButtonSourceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    prevIP       : Integer;
    prevSP       : Integer;
    xSize, ySize : Integer;

    procedure reScale;
    procedure ipsp;

    procedure paintOne(addr : Integer);
    procedure paintAll;

    function  ram(addr : Integer) : String;
  end;

var
  FormRamHex: TFormRamHex;

implementation

uses Assem;

{$R *.DFM}

procedure TFormRamHex.paintAll;
var i : Integer;
begin
  canvas.pen.color := clWhite;
  canvas.rectangle(0, 0, clientWidth, clientHeight);

  ySize := (formRamHex.clientHeight - Panel1.Height) div 17;
  xSize := formRamHex.clientWidth  div 17;

  canvas.font.color := clRed;

  for i := 0 to 15 do
  begin
    canvas.textOut(8 + (i + 1) * xSize,  1, intToHex(i, 1));
    canvas.textOut(3,  (i + 1) * ySize + 1,  intToHex(i, 1) + '0');
  end;

  canvas.font.color := clBlack;

  for i := 0 to 255 do
  begin
    paintOne(i)
  end
end;

procedure TFormRamHex.paintOne(addr : Integer);
var x, y : Integer;
begin
  x := xSize + xSize * (addr MOD 16);
  y := ySize + ySize * (addr DIV 16);

  with FormRamHex.canvas do
  begin
    rectangle(x, y, x + xSize, y + ySize);

    if aaa = Nil then
    begin
      textOut(x + 1, y + 1, '  ')
    end
    else if addr = $FF AND aaa.regs[IP] then
    begin
      brush.color := clRed;
      pen.color   := clRed;
      font.color  := clYellow;
      textOut(x + 1, y + 1, FormRamHex.ram(addr));
      brush.color := clWhite;
      pen.color   := clWhite;
      font.color  := clBlack;
    end
    else if addr = $FF AND aaa.regs[SP] then
    begin
      brush.color := clBlue;
      pen.color   := clBlue;
      font.color  := clYellow;
      textOut(x + 1, y + 1, FormRamHex.ram(addr));
      brush.color := clWhite;
      pen.color   := clWhite;
      font.color  := clBlack;
    end
    else
    begin
      textOut(x + 1, y + 1, FormRamHex.ram(addr))
    end
  end
end;

procedure TFormRamHex.ipsp;  // Hande changes to IP and SP
Var lip, lsp : Integer;
begin
  lip := $FF AND aaa.regs[IP];
  lsp := $FF AND aaa.regs[SP];

  if lip <> prevIP then
  begin
    paintOne(prevIP);
    paintOne(lip);
    prevIP := lip;
  end;

  if lsp <> prevSP then
  begin
    paintOne(prevSP);
    paintOne(lsp);
    prevSP := lsp;
  end
end;

function TFormRamHex.ram(addr : Integer) : String;
begin
  if aaa.ram[addr] <> nil then
  begin
    if  RadioButtonHex.Checked then
    begin
      result := aaa.ram[addr].opAscii
    end
    else if RadioButtonASCII.checked then
    begin
      result := ' ' + chr($FF AND aaa.ram[addr].opHex)
    end
    else if RadioButtonSource.checked then
    begin
      result := aaa.ram[addr].token
    end
  end
  else
  begin
    if  RadioButtonHex.Checked then
    begin
      result := '00'
    end
    else if RadioButtonASCII.checked then
    begin
      result := ' ' + chr(0)
    end
    else if RadioButtonSource.checked then
    begin
      result := 'END'
    end
  end
end;

procedure TFormRamHex.reScale;
begin
  ySize := (formRamHex.clientHeight - Panel1.Height) div 17;
  xSize := formRamHex.clientWidth  div 17;

  paintAll
end;

procedure TFormRamHex.FormResize(Sender: TObject);
begin
  reScale;
  invalidate
end;

procedure TFormRamHex.RadioButtonHexClick(Sender: TObject);
begin
  reScale;
  caption := 'RAM Hexadecimal View';
end;

procedure TFormRamHex.RadioButtonASCIIClick(Sender: TObject);
begin
  reScale;
  caption := 'RAM ASCII View';
end;

procedure TFormRamHex.RadioButtonSourceClick(Sender: TObject);
begin
  reScale;
  caption := 'RAM Source Code View';
end;

procedure TFormRamHex.FormCreate(Sender: TObject);
begin
  prevIP := -1;
  prevSP := -1;

  rescale;
end;

procedure TFormRamHex.FormPaint(Sender: TObject);
begin
  paintAll
end;

procedure TFormRamHex.FormActivate(Sender: TObject);
begin
  paintAll
end;

procedure TFormRamHex.FormShow(Sender: TObject);
begin
  paintAll
end;

end.

