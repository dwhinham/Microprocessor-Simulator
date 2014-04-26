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
unit KeyPadFm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TFormKeyPad = class(TForm)
    ImageKeyPad: TImage;
    procedure ImageKeyPadMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }

    kk : Byte;

    function getState : byte;
  end;

var
  FormKeyPad: TFormKeyPad;

implementation

uses Assem, Mainform;

{$R *.dfm}

{ ========================================================================== }

function TFormKeyPad.getState : byte;
begin
  result := kk;
end;

{ ========================================================================== }

procedure TFormKeyPad.ImageKeyPadMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  kk := $FF;

  if Y < 33 then                        // Top Row
  begin
    if X <  32 then kk := ord('1') else
    if X <  60 then kk := ord('2') else
    if X <  89 then kk := ord('3') else
    if X < 118 then kk := ord('A') else
    if X < 146 then kk := 8;            // Backspace
  end
  else
  if Y < 61 then       // Top Row NORMAL
  begin
    if X <  32 then kk := ord('4') else
    if X <  60 then kk := ord('5') else
    if X <  89 then kk := ord('6') else
    if X < 118 then kk := ord('B') else
    if X < 146 then kk := 8;            // Backspace
  end
  else
  if Y < 88 then       // Second Row CTRL / SHIFT
  begin
    if X <  32 then kk := ord('7') else
    if X <  60 then kk := ord('8') else
    if X <  89 then kk := ord('9') else
    if X < 118 then kk := ord('C') else
    if X < 146 then kk := 13;           // Enter
  end
  else
  if Y < 113 then       // Second Row NORMAL
  begin
    if X <  32 then kk := ord('F') else
    if X <  60 then kk := ord('0') else
    if X <  89 then kk := ord('E') else
    if X < 118 then kk := ord('D') else
    if X < 146 then kk := 13;           // Enter
  end;

  if kk <> $FF then
  begin
    with aaa do
    begin
      if Continuous AND ((regs[SR] AND I_I) <> 0) then
      begin
        { set return address }
        ramAssign(aaa.regs[SP], regs[IP]);

        { jump to int 04 address }
        regAssign(IP, HexFF AND ram[04].opHex, dontSetFlags);
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



