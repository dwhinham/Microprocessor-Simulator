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
unit Keybin;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TFormKeybIn = class(TForm)
    Label1: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    gotKey : Boolean;
  end;

var
  FormKeybIn: TFormKeybIn;

implementation

Uses MainForm, Assem;

{$R *.DFM}

procedure TFormKeybIn.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(key) = 27 then
  begin
    aaa.continuous := false
  end
  else
  begin
    aaa.regAssign(AL, ord(key), dontSetFlags);
    aaa.continuous := formMain.wasContinuous;
  end;

  gotKey := true;
  close
end;

procedure TFormKeybIn.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if gotKey then
    canClose := True
  else
    canClose := False
end;

procedure TFormKeybIn.FormActivate(Sender: TObject);
begin
  gotKey := False
end;

end.

