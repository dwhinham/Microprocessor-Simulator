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
unit Lift;    // BevelLift Top = 232   Left = 45   Width = 66   Height = 86

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Buttons, StdCtrls, Assem;

type
  TFormLift = class(TForm)
    Image1: TImage;
    SpeedButtonUp: TSpeedButton;
    SpeedButtonDown: TSpeedButton;
    LabelData: TLabel;
    Timer1: TTimer;
    LabelState: TLabel;
    ButtonEngineer: TButton;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ButtonEngineerClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SpeedButtonUpClick(Sender: TObject);
    procedure SpeedButtonDownClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    crashed      : Boolean;

    state        : Integer;

    procedure Lift;
    procedure setView;

    function  getState : Integer;
  end;

var
  FormLift: TFormLift;

implementation

Uses MainForm;

{$R *.DFM}

procedure TFormLift.setView;
begin
  if crashed then
  begin
    labelState.Font.Color := ClRed;
    labelState.Caption := 'CRASHED';
  end
  else if (state AND 1) <> 0 then
  begin
    labelState.Font.Color := ClBlack;
    labelState.Caption := 'Going Up';
  end
  else if (state AND 2) <> 0 then
  begin
    labelState.Font.Color := ClBlack;
    labelState.Caption := 'Going Down';
  end
  else
  begin
    labelState.Font.Color := ClBlack;
    labelState.caption := 'Stopped';
  end;

  labelData.Caption := 'MSB ' + aaa.h2b(state) + ' LSB';
end;

function TFormLift.getState : Integer;
begin
  result := state
end;

procedure TFormLift.Lift;
var switches : Integer;
begin
  switches := state AND $C;
  state := $F3 AND aaa.regs[0];       { Mask out the switches }
  state := state OR switches;

  if state AND $10 <> 0 then
  begin
    speedButtonDown.Enabled := False;
  end
  else
  begin
    speedButtonDown.Enabled := True;
  end;

  if state AND $20 <> 0 then
  begin
    speedButtonUp.Enabled := False;
  end
  else
  begin
    speedButtonUp.Enabled := True;
  end
end;

procedure TFormLift.FormCreate(Sender: TObject);
begin
  crashed      := False;
  state        := 0;
end;

procedure TFormLift.ButtonEngineerClick(Sender: TObject);
begin
  Image2.top := 231;

  crashed      := False;

  state        := 0;

  buttonEngineer.enabled := false;
  SpeedButtonDown.enabled := true;
  SpeedButtonUp.enabled := true;
  setView;

  formMain.setFocus
end;

procedure TFormLift.Timer1Timer(Sender: TObject);
begin
  if crashed then
  begin
    buttonEngineer.enabled := true
  end
  else if ((state AND 1) <> 0) AND ((state AND 2) <> 0) then
  begin
    Crashed := True;
  end
  else
  begin
    if (state AND 1) <> 0 then
    begin
      if timer1.interval = 100 then
        Image2.top := Image2.top - 4
      else
        Image2.top := Image2.top - 1
    end;

    if Image2.top < 74 then
      crashed := true;

    if Image2.top < 76 then  { TopSwitch }
    begin
      state := state OR 4;      { Set bit 4 }
    end
    else
    begin
      state := state AND $FB;   { Clear bit 4 }
    end;

    if (state AND 2) <> 0 then
    begin
      if timer1.interval = 100 then
        Image2.top := Image2.top + 4
      else
        Image2.top := Image2.top + 1
    end;

    if Image2.top > 232 then
      crashed := true;

    if Image2.top > 230 then { BottomSwitch }
    begin
      state := state OR 8;      { Set bit 8 }
    end
    else
    begin
      state := state AND $F7;   { Clear bit 8 }
    end;

    if (Image2.top < 82) OR (Image2.top > 224) then
      timer1.interval := 500
    else
      timer1.interval := 50
  end;

  setView
end;

procedure TFormLift.SpeedButtonUpClick(Sender: TObject);
begin
  state := state OR $20;
  SpeedButtonUp.enabled := False;
  formMain.setFocus
end;

procedure TFormLift.SpeedButtonDownClick(Sender: TObject);
begin
  state := state OR $10;
  SpeedButtonDown.enabled := False;
  formMain.setFocus
end;

end.
