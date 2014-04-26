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
unit KeybForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TFormKeyb = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    kk : Byte;

    function getState : byte;
  end;

var
  FormKeyb: TFormKeyb;

implementation

uses Mainform, Assem;

{$R *.dfm}

{ ========================================================================== }

function TFormKeyb.getState : byte;
begin
  result := kk;
end;

{ ========================================================================== }

procedure TFormKeyb.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Label1.Caption := '';

  if Y < 13 then       // Top Row SHIFT
  begin
    if (X > 35) then
    begin
      if X <  61 then Label1.Caption := '!' else
      if X <  89 then Label1.Caption := '"' else
      if X < 117 then Label1.Caption := '£' else
      if X < 145 then Label1.Caption := '$' else
      if X < 173 then Label1.Caption := '%' else
      if X < 202 then Label1.Caption := '^' else
      if X < 230 then Label1.Caption := '&' else
      if X < 259 then Label1.Caption := '*' else
      if X < 286 then Label1.Caption := '(' else
      if X < 314 then Label1.Caption := ')' else
      if X < 343 then Label1.Caption := '_' else
      if X < 371 then Label1.Caption := '+' else
      if X < 425 then Label1.Caption := 'BkSp'
    end
  end
  else
  if Y < 26 then       // Top Row NORMAL
  begin
    if (X > 35) then
    begin
      if X <  61 then Label1.Caption := '1' else
      if X <  89 then Label1.Caption := '2' else
      if X < 117 then Label1.Caption := '3' else
      if X < 145 then Label1.Caption := '4' else
      if X < 173 then Label1.Caption := '5' else
      if X < 202 then Label1.Caption := '6' else
      if X < 230 then Label1.Caption := '7' else
      if X < 259 then Label1.Caption := '8' else
      if X < 286 then Label1.Caption := '9' else
      if X < 314 then Label1.Caption := '0' else
      if X < 343 then Label1.Caption := '-' else
      if X < 371 then Label1.Caption := '=' else
      if X < 425 then Label1.Caption := 'BkSp'
    end
  end
  else
  if Y < 43 then       // Second Row CTRL / SHIFT
  begin
    if X > 6 then
    begin
      if X <  47 then Label1.Caption := 'Tab' else
      if X <  61 then Label1.Caption := 'Ctrl+Q' else
      if X <  75 then Label1.Caption := 'Q' else
      if X <  89 then Label1.Caption := 'Ctrl+W' else
      if X < 104 then Label1.Caption := 'W' else
      if X < 117 then Label1.Caption := 'Ctrl+E' else
      if X < 132 then Label1.Caption := 'E' else
      if X < 145 then Label1.Caption := 'Ctrl+R' else
      if X < 160 then Label1.Caption := 'R' else
      if X < 173 then Label1.Caption := 'Ctrl+T' else
      if X < 188 then Label1.Caption := 'T' else
      if X < 201 then Label1.Caption := 'Ctrl+Y' else
      if X < 216 then Label1.Caption := 'Y' else
      if X < 230 then Label1.Caption := 'Ctrl+U' else
      if X < 244 then Label1.Caption := 'U' else
      if X < 257 then Label1.Caption := 'Ctrl+I' else
      if X < 272 then Label1.Caption := 'I' else
      if X < 286 then Label1.Caption := 'Ctrl+O' else
      if X < 301 then Label1.Caption := 'O' else
      if X < 314 then Label1.Caption := 'Ctrl+P' else
      if X < 329 then Label1.Caption := 'P' else
      if X < 342 then Label1.Caption := 'Ctrl+(' else
      if X < 356 then Label1.Caption := '{' else
      if X < 370 then Label1.Caption := 'Ctrl+)' else
      if X < 385 then Label1.Caption := '}' else
      if X < 403 then Label1.Caption := 'Ctrl+Enter' else
      if X < 425 then Label1.Caption := 'Enter'
    end
  end
  else
  if Y < 56 then       // Second Row NORMAL
  begin
    if X > 6 then
    begin
      if X <  47 then Label1.Caption := 'Tab' else
      if X <  75 then Label1.Caption := 'q' else
      if X < 104 then Label1.Caption := 'w' else
      if X < 132 then Label1.Caption := 'e' else
      if X < 160 then Label1.Caption := 'r' else
      if X < 188 then Label1.Caption := 't' else
      if X < 216 then Label1.Caption := 'y' else
      if X < 244 then Label1.Caption := 'u' else
      if X < 272 then Label1.Caption := 'i' else
      if X < 301 then Label1.Caption := 'o' else
      if X < 329 then Label1.Caption := 'p' else
      if X < 356 then Label1.Caption := '[' else
      if X < 385 then Label1.Caption := ']' else
      if X < 403 then Label1.Caption := 'Ctrl+Enter' else
      if X < 425 then Label1.Caption := 'Enter'
    end
  end
  else
  if Y < 70 then       // Third Row CTRL / SHIFT
  begin
    if X > 56 then
    begin
      if X <  68 then Label1.Caption := 'Ctrl+A' else
      if X <  82 then Label1.Caption := 'A' else
      if X <  95 then Label1.Caption := 'Ctrl+S' else
      if X < 111 then Label1.Caption := 'S' else
      if X < 123 then Label1.Caption := 'Ctrl+D' else
      if X < 137 then Label1.Caption := 'D' else
      if X < 152 then Label1.Caption := 'Ctrl+F' else
      if X < 166 then Label1.Caption := 'F' else
      if X < 181 then Label1.Caption := 'Ctrl+G' else
      if X < 195 then Label1.Caption := 'G' else
      if X < 208 then Label1.Caption := 'Ctrl+H' else
      if X < 223 then Label1.Caption := 'H' else
      if X < 236 then Label1.Caption := 'Ctrl+J' else
      if X < 251 then Label1.Caption := 'J' else
      if X < 264 then Label1.Caption := 'Ctrl+K' else
      if X < 279 then Label1.Caption := 'K' else
      if X < 292 then Label1.Caption := 'Ctrl+L' else
      if X < 307 then Label1.Caption := 'L' else
      if X < 335 then Label1.Caption := ':' else
      if X < 364 then Label1.Caption := '@' else
      if X < 377 then Label1.Caption := 'Ctrl+~' else
      if X < 391 then Label1.Caption := '~' else
      if X < 425 then Label1.Caption := 'Enter'
    end
  end
  else
  if Y < 84 then       // Third Row NORMAL
  begin
    if X > 56 then
    begin
      if X <  82 then Label1.Caption := 'a' else
      if X < 111 then Label1.Caption := 's' else
      if X < 137 then Label1.Caption := 'd' else
      if X < 166 then Label1.Caption := 'f' else
      if X < 195 then Label1.Caption := 'g' else
      if X < 223 then Label1.Caption := 'h' else
      if X < 251 then Label1.Caption := 'j' else
      if X < 279 then Label1.Caption := 'k' else
      if X < 307 then Label1.Caption := 'l' else
      if X < 335 then Label1.Caption := ';' else
      if X < 364 then Label1.Caption := '''' else
      if X < 391 then Label1.Caption := '#' else
      if X < 425 then Label1.Caption := 'Enter'
    end
  end
  else
  if Y < 98 then       // Fourth Row CTRL / SHIFT
  begin
    if X > 42 then
    begin
      if X <  54 then Label1.Caption := 'Ctrl+|' else
      if X <  67 then Label1.Caption := '|' else
      if X <  83 then Label1.Caption := 'Ctrl+Z' else
      if X <  96 then Label1.Caption := 'Z' else
      if X < 110 then Label1.Caption := 'Ctrl+' else
      if X < 124 then Label1.Caption := 'X' else
      if X < 138 then Label1.Caption := 'Ctrl+C' else
      if X < 153 then Label1.Caption := 'C' else
      if X < 167 then Label1.Caption := 'Ctrl+V' else
      if X < 180 then Label1.Caption := 'V' else
      if X < 195 then Label1.Caption := 'Ctrl+B' else
      if X < 209 then Label1.Caption := 'B' else
      if X < 223 then Label1.Caption := 'Ctrl+N' else
      if X < 237 then Label1.Caption := 'N' else
      if X < 250 then Label1.Caption := 'Ctrl+M' else
      if X < 265 then Label1.Caption := 'M' else
      if X < 293 then Label1.Caption := '<' else
      if X < 321 then Label1.Caption := '>' else
      if X < 350 then Label1.Caption := '?'
    end
  end
  else
  if Y < 112 then      // Fourth Row NORMAL
  begin
    if X > 42 then
    begin
      if X <  67 then Label1.Caption := '\' else
      if X <  96 then Label1.Caption := 'z' else
      if X < 124 then Label1.Caption := 'x' else
      if X < 153 then Label1.Caption := 'c' else
      if X < 180 then Label1.Caption := 'v' else
      if X < 209 then Label1.Caption := 'b' else
      if X < 237 then Label1.Caption := 'n' else
      if X < 265 then Label1.Caption := 'm' else
      if X < 293 then Label1.Caption := ',' else
      if X < 321 then Label1.Caption := '.' else
      if X < 350 then Label1.Caption := '/'
    end
  end
  else
  if Y < 150 then      // Space Bar
  begin
    if X > 113 then
    begin
      if X < 279 then Label1.Caption := 'Space'
    end
  end;
end;

{ ========================================================================== }

procedure TFormKeyb.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  kk := 0;

  if Y < 13 then       // Top Row SHIFT
  begin
    if (X > 35) then
    begin
      if X <  61 then kk := ord('!') else
      if X <  89 then kk := ord('"') else
      if X < 117 then kk := ord('£') else
      if X < 145 then kk := ord('$') else
      if X < 173 then kk := ord('%') else
      if X < 202 then kk := ord('^') else
      if X < 230 then kk := ord('&') else
      if X < 259 then kk := ord('*') else
      if X < 286 then kk := ord('(') else
      if X < 314 then kk := ord(')') else
      if X < 343 then kk := ord('_') else
      if X < 371 then kk := ord('+') else
      if X < 425 then kk := 8;          // Backspace
    end
  end
  else
  if Y < 26 then       // Top Row NORMAL
  begin
    if (X > 35) then
    begin
      if X <  61 then kk := ord('1') else
      if X <  89 then kk := ord('2') else
      if X < 117 then kk := ord('3') else
      if X < 145 then kk := ord('4') else
      if X < 173 then kk := ord('5') else
      if X < 202 then kk := ord('6') else
      if X < 230 then kk := ord('7') else
      if X < 259 then kk := ord('8') else
      if X < 286 then kk := ord('9') else
      if X < 314 then kk := ord('0') else
      if X < 343 then kk := ord('-') else
      if X < 371 then kk := ord('=') else
      if X < 425 then kk := 8;             // Backspace
    end
  end
  else
  if Y < 43 then       // Second Row CTRL / SHIFT
  begin
    if X > 6 then
    begin
      if X <  47 then kk := 9        else
      if X <  61 then kk := 17       else
      if X <  75 then kk := ord('Q') else
      if X <  89 then kk := 23       else
      if X < 104 then kk := ord('W') else
      if X < 117 then kk :=  5       else
      if X < 132 then kk := ord('E') else
      if X < 145 then kk := 18       else
      if X < 160 then kk := ord('R') else
      if X < 173 then kk := 20       else
      if X < 188 then kk := ord('T') else
      if X < 201 then kk := 25       else
      if X < 216 then kk := ord('Y') else
      if X < 230 then kk := 21       else
      if X < 244 then kk := ord('U') else
      if X < 257 then kk :=  9       else
      if X < 272 then kk := ord('I') else
      if X < 286 then kk := 15       else
      if X < 301 then kk := ord('O') else
      if X < 314 then kk := 16       else
      if X < 329 then kk := ord('P') else
      if X < 342 then kk := 27       else
      if X < 356 then kk := ord('{') else
      if X < 370 then kk := 29       else
      if X < 385 then kk := ord('}') else
      if X < 403 then kk := 10       else
      if X < 425 then kk := 13             // Enter
    end
  end
  else
  if Y < 56 then       // Second Row NORMAL
  begin
    if X > 6 then
    begin
      if X <  47 then kk := 9        else
      if X <  75 then kk := ord('q') else
      if X < 104 then kk := ord('w') else
      if X < 132 then kk := ord('e') else
      if X < 160 then kk := ord('r') else
      if X < 188 then kk := ord('t') else
      if X < 216 then kk := ord('y') else
      if X < 244 then kk := ord('u') else
      if X < 272 then kk := ord('i') else
      if X < 301 then kk := ord('o') else
      if X < 329 then kk := ord('p') else
      if X < 356 then kk := ord('[') else
      if X < 385 then kk := ord(']') else
      if X < 403 then kk := 10       else
      if X < 425 then kk := 13             // Enter
    end
  end
  else
  if Y < 70 then       // Third Row CTRL / SHIFT
  begin
    if X > 56 then
    begin
      if X <  68 then kk := 1        else
      if X <  82 then kk := ord('A') else
      if X <  95 then kk := 19       else
      if X < 111 then kk := ord('S') else
      if X < 123 then kk :=  4       else
      if X < 137 then kk := ord('D') else
      if X < 152 then kk :=  6       else
      if X < 166 then kk := ord('F') else
      if X < 181 then kk :=  7       else
      if X < 195 then kk := ord('G') else
      if X < 208 then kk :=  8       else
      if X < 223 then kk := ord('H') else
      if X < 236 then kk := 10       else
      if X < 251 then kk := ord('J') else
      if X < 264 then kk := 11       else
      if X < 279 then kk := ord('K') else
      if X < 292 then kk := 12       else
      if X < 307 then kk := ord('L') else
      if X < 335 then kk := ord(':') else
      if X < 364 then kk := ord('@') else
      if X < 377 then kk := 28       else
      if X < 391 then kk := ord('~') else
      if X < 425 then kk := $0D
    end
  end
  else
  if Y < 84 then       // Third Row NORMAL
  begin
    if X > 56 then
    begin
      if X <  82 then kk := ord('a') else
      if X < 111 then kk := ord('s') else
      if X < 137 then kk := ord('d') else
      if X < 166 then kk := ord('f') else
      if X < 195 then kk := ord('g') else
      if X < 223 then kk := ord('h') else
      if X < 251 then kk := ord('j') else
      if X < 279 then kk := ord('k') else
      if X < 307 then kk := ord('l') else
      if X < 335 then kk := ord(';') else
      if X < 364 then kk := ord('''') else
      if X < 391 then kk := ord('#') else
      if X < 425 then kk := $0D
    end
  end
  else
  if Y < 98 then       // Fourth Row CTRL / SHIFT
  begin
    if X > 42 then
    begin
      if X <  54 then kk := 28       else
      if X <  67 then kk := ord('|') else
      if X <  83 then kk := 26       else
      if X <  96 then kk := ord('Z') else
      if X < 110 then kk := 24       else
      if X < 124 then kk := ord('X') else
      if X < 138 then kk :=  3       else
      if X < 153 then kk := ord('C') else
      if X < 167 then kk := 22       else
      if X < 180 then kk := ord('V') else
      if X < 195 then kk :=  2       else
      if X < 209 then kk := ord('B') else
      if X < 223 then kk := 14       else
      if X < 237 then kk := ord('N') else
      if X < 250 then kk := 13       else
      if X < 265 then kk := ord('M') else
      if X < 293 then kk := ord('<') else
      if X < 321 then kk := ord('>') else
      if X < 350 then kk := ord('?')
    end
  end
  else
  if Y < 112 then      // Fourth Row NORMAL
  begin
    if X > 42 then
    begin
      if X <  67 then kk := ord('\') else
      if X <  96 then kk := ord('z') else
      if X < 124 then kk := ord('x') else
      if X < 153 then kk := ord('c') else
      if X < 180 then kk := ord('v') else
      if X < 209 then kk := ord('b') else
      if X < 237 then kk := ord('n') else
      if X < 265 then kk := ord('m') else
      if X < 293 then kk := ord(',') else
      if X < 321 then kk := ord('.') else
      if X < 350 then kk := ord('/')
    end
  end
  else
  if Y < 150 then      // Space Bar
  begin
    if X > 113 then
    begin
      if X < 279 then kk := ord(' ')
    end
  end;

  if kk <> 0 then
  begin
    with aaa do
    begin
      if Continuous AND ((regs[SR] AND I_I) <> 0) then
      begin
        { set return address }
        ramAssign(aaa.regs[SP], regs[IP]);

        { jump to int 03 address }
        regAssign(IP, HexFF AND ram[03].opHex, dontSetFlags);
        regs[IP] := regs[IP] AND HexFF;   // Why is this needed ?

        { IP within RAM }
        if (regs[IP] < 0) OR (regs[IP] > 255) then
        begin
          error(BeyondRAM, currToken);
          exit
        end;

        { Decrement SP }
        if (HexFF AND regs[SP] > 0) then
        begin
          regAssign(SP, regs[SP] - 1, dontSetFlags)
        end
        else
        begin
          error('Stack overflow.', currToken);
        end;

        formMain.currInstr
      end
    end
  end
end;

{ ========================================================================== }

end.

