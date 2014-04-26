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
unit Assem;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls;

const
  ZZZ     : smallInt =  $2;                            { FLAGS }
  OOO     : smallInt =  $4;
  SSS     : smallInt =  $8;
  I_I     : smallInt = $10;

  Hex01   : smallInt = $01;
  HexFF   : smallInt = $FF;
  Hex80   : smallInt = $80;
  Hex7F   : smallInt = $7F;
  HexC0   : smallInt = $C0;
  HexBF   : smallInt = $BF;
  HexEF   : smallInt = $EF;
  HexFF00 : smallInt = -256;                           { $FF00 }
  HexFF80 : smallInt = -128;                           { $FF80 }

  setFlags = True;
  dontSetFlags = False;

  TABChar : Char = #9;
  AL = 0;
  BL = 1;
  CL = 2;
  DL = 3;
  IP = 4;
  SP = 5;
  SR = 6;

  BeyondRAM = 'Can not execute code beyond the end of memory.';
  RegNotExi = 'Invalid register in machine code.';

type
  TBuffer = array[0..32767] of char;

  tokNode = class(TObject)
    isSourceLabel : Boolean;
    isDestlabel   : Boolean;
    address       : smallInt;       { Used in Jump Calculations }
    next          : tokNode;
    opHex         : smallInt;       {  D0        }
    opAscii       : String;         { 'D0'       }
    token         : String;         { 'MOV'      }
    opBin         : string;         { '11010000' }
    selStart      : integer;        {  27        }
    selLength     : integer;        {   3        }
    lsStart       : integer;        { patch list file JMP addresses   }
                                    { points to the start of the line }

    constructor create;
    procedure   setTokenValues(aValue : smallInt);
  end;

  Taaa = class(TForm)
    TimerHW: TTimer;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerHWTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    needToAssemble : Boolean;
    continuous     : Boolean;
    eos            : Boolean; { End Of Source }
    ok             : Boolean; { Becomes false on error }
    prevTokComment : Boolean;

    tokListHead    : tokNode;

    iii            : smallInt;
    ramAddr        : smallInt;

    ram            : Array[0..255] of tokNode;
    regs           : Array[0..  6] of smallInt;

    procedure calcJumps;

    procedure Parse;
    procedure ParseAdd;
    procedure ParseCallInt;
    procedure ParseCmp;
    procedure ParseDB;
    procedure ParseInc;
    procedure ParseInOut;
    procedure ParseJmp;
    procedure ParseMov;
    procedure ParseZerop;

    procedure monop;
    procedure arith2;
    procedure imovMR;
    procedure Zerop;
    procedure imovRM;
    procedure cmp;
    procedure inOut;
    procedure jmp;
    procedure movMR;
    procedure movRM;
    procedure movRN;

    procedure CPUReset;
    procedure error(aMessage : String; aToken : tokNode);
    procedure freeAllTokNodes;
    procedure generate(aNode : tokNode);
    procedure incIII;
    procedure listAdd(s : String; wasComment : Boolean);
    procedure ramAssign(anAddress, aValue : smallInt);
    procedure regAssign(aReg, aValue : smallInt; shouldSetFlags : Boolean);
    procedure regsToGray;
    procedure TokeniseSource;
    procedure Log(s : String);
    procedure LogRegs;
    procedure LogIp(count : String);

    function  charToHex(aChar : char) : smallInt; { 'A' -> 10                 }
    function  currToken  : tokNode;
    function  prevToken  : tokNode;
    function  currTokStr : String;
    function  h2b(n : SmallInt) : String;         { 127 -> '01111111'         }
    function  mightHave(sym : String) : Boolean;
    function  mightHaveComment        : Boolean;
    function  mightHaveDestLabel      : Boolean;
    function  mightHaveHexNum         : Boolean;
    function  mightHaveRegister       : Boolean;
    function  mustHave(sym : String)  : Boolean;
    function  ramOP0 : smallInt;                  { ram[regs[IP]].opHex       }
    function  ramOP1 : smallInt;                  { ram[regs[IP] + 1].opHex   }
    function  ramOP2 : smallInt;                  { ram[regs[IP] + 2].opHex   }
    function  ramTo0 : string;                    { ram[regs[IP]].opToken     }
    function  ramTo1 : string;                    { ram[regs[IP] + 1].opToken }
    function  ramTo2 : string;                    { ram[regs[IP] + 2].opToken }
    function  tokenAt(n : smallInt) : tokNode;
    function  trim(s : string) : string;
    function  validLabel(s, colon : string) : boolean;
    function  validReg(n : smallInt) : Boolean;
    function  ___isoz_ : String;
  end;

var
  aaa: Taaa;

implementation

uses Mainform, Stepper, Maze, Heater, Vdu_form,
     keyBin, tLight, sevSeg, lift, ramHex, KeybForm, KeyPadFm;

{$R *.DFM}

{ ---------------------------------------------------------------------- }

procedure Taaa.Log(s : String);
begin
  if FormMain.CheckBoxRunLog.Checked then
  begin
    while FormMain.RichEdit1.Lines.Count > 1000 do
    begin
      FormMain.RichEdit1.Lines.Delete(0);
    end;

    FormMain.RichEdit1.Lines.Add(s);
    FormMain.RichEdit1.Perform(EM_SCROLL, SB_BOTTOM, 0);
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.LogRegs;
begin
  if FormMain.CheckBoxRunLog.Checked then
  begin
    log(#9 + 'AL = ' + IntToHex($FF AND regs[AL], 2) + #9 +
             'BL = ' + IntToHex($FF AND regs[BL], 2) + #9 +
             'CL = ' + IntToHex($FF AND regs[CL], 2) + #9 +
             'DL = ' + IntToHex($FF AND regs[DL], 2) + #9 +
             'IP = ' + IntToHex($FF AND regs[IP], 2) + #9 +
             'SP = ' + IntToHex($FF AND regs[SP], 2) + #9 +
             'SR = ' + ___isoz_);

    FormMain.RichEdit1.Perform(EM_SCROLL, SB_BOTTOM, 0);
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.LogIp(count : String);
begin
  if FormMain.CheckBoxRunLog.Checked then
  begin
    log('INSTRUCTION POINTER');
    log(#9 + 'Add ' + count + ' to IP');
    logRegs;
    log('-------------------------------------------------------------------------------------------');

    FormMain.RichEdit1.Perform(EM_SCROLL, SB_BOTTOM, 0);
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.regAssign(aReg, aValue : smallInt; shouldSetFlags : Boolean);

                function itoa4d(n : SmallInt) : string;   { 127 -> '+127' }
                Var s : String;                           { -13 -> '-013' }
                begin
                  if n < -99 then
                  begin
                    n := abs(n);
                    str(n, s);
                    s := '-' + s
                  end
                  else if n < -9 then
                  begin
                    n := abs(n);
                    str(n, s);
                    s := '-' + '0' + s
                  end
                  else if n < 0 then
                  begin
                    n := abs(n);
                    str(n, s);
                    s := '-' + '00' + s
                  end
                  else if n < 10 then
                  begin
                    str(n, s);
                    s := '+' + '00' + s
                  end
                  else if n < 100 then
                  begin
                    str(n, s);
                    s := '+' + '0' + s
                  end
                  else
                  begin
                    str(n, s);
                    s := '+' + s
                  end;

                  result := s
                end;

                function intToReg(aValue : smallInt) : String;
                begin
                  result := h2b(aValue AND HexFF) + ' ' +
                            intToHex(aValue AND HexFF, 2) +
                            ' ' +
                            itoa4d(aValue)
                end;

Var soz : smallInt;
begin
  if shouldSetFlags then
  begin
    soz := regs[SR] AND I_I;              { Clear SR but leave I flag alone }

    if aValue = 0 then
    begin
      soz := soz OR ZZZ                   { Set Z Flag }
    end
    else if (aValue > 127) AND ((aValue AND Hex80) <> 0) then
    begin
      aValue := aValue OR HexFF80;
      soz := soz OR OOO;                  { Set O Flag }
      soz := soz OR SSS                   { Set S Flag }
    end
    else if aValue > 127 then
    begin
      aValue := aValue AND Hex7F;
      soz := soz OR OOO
    end
    else if (aValue < -128) AND ((aValue AND Hex80) <> 0) then
    begin
      aValue := aValue OR HexFF80;
      soz := soz OR OOO;
      soz := soz OR SSS
    end
    else if (aValue < -128) then
    begin
      aValue := aValue AND Hex7F;
      soz := soz OR OOO
    end
    else if aValue < 0 then
    begin
      soz := soz OR SSS
    end;

    regs[SR] := soz;

    FormMain.labelSR.Caption := 'SR ' + intToReg(soz);
    FormMain.labelSR.Color := clYellow
  end
  else
  begin
    if (aValue > 127) AND ((aValue AND Hex80) <> 0) then
    begin
      aValue := aValue OR HexFF80
    end
    else if aValue > 127 then
    begin
      aValue := aValue AND Hex7F
    end
    else if (aValue < -128) AND ((aValue AND Hex80) <> 0) then
    begin
      aValue := aValue OR HexFF80
    end
    else if aValue < -128 then
    begin
      aValue := aValue AND Hex7F
    end
  end;

  regs[aReg] := aValue;

  case aReg of
    AL:
    begin
      FormMain.labelAL.Caption := 'AL ' + intToReg(aValue);
      FormMain.labelAL.Color := clYellow
    end;

    BL:
    begin
      FormMain.labelBL.Caption := 'BL ' + intToReg(aValue);
      FormMain.labelBL.Color := clYellow
    end;

    CL:
    begin
      FormMain.labelCL.Caption := 'CL ' + intToReg(aValue);
      FormMain.labelCL.Color := clYellow
    end;

    DL:
    begin
      FormMain.labelDL.Caption := 'DL ' + intToReg(aValue);
      FormMain.labelDL.Color := clYellow
    end;

    IP:
    begin
      FormMain.labelIP.Caption := 'IP ' + intToReg(aValue);
      FormMain.labelIP.Color := clYellow;
      FormRamHex.ipsp;
    end;

    SP:
    begin
      FormMain.labelSP.Caption := 'SP ' + intToReg(aValue);
      FormMain.labelSP.Color := clYellow;
      FormRamHex.ipsp
    end;

    SR:
    begin
      FormMain.labelSR.Caption := 'SR ' + intToReg(aValue);
      FormMain.labelSR.Color := clYellow
    end;
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.FormDestroy(Sender: TObject);
begin
  freeAllTokNodes
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.FormCreate(Sender: TObject);
Var i : smallInt;
begin
  tokListHead    := Nil;
  needToAssemble := true;
  continuous     := false;
  prevTokComment := false;

  for i := 0 to 255 do
  begin
    ram[i] := Nil
  end;

  regAssign(AL, 0,   dontSetFlags);
  regAssign(BL, 0,   dontSetFlags);
  regAssign(CL, 0,   dontSetFlags);
  regAssign(DL, 0,   dontSetFlags);
  regAssign(IP, 0,   dontSetFlags);
  regAssign(SR, 0,   dontSetFlags);
  regAssign(SP, HexBF, dontSetFlags);
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.TimerHWTimer(Sender: TObject);
begin
  if Continuous AND ((regs[SR] AND I_I) <> 0) then
  begin
{ return address       }  ramAssign(regs[SP], regs[IP]);
{ jump to call address }  regAssign(IP, HexFF AND ram[02].opHex, dontSetFlags);
                          regs[IP] := regs[IP] AND HexFF;

{ IP within RAM        }  if (regs[IP] < 0) OR (regs[IP] > 255) then
                          begin
                            error(BeyondRAM, currToken);
                            exit
                          end;

{ Decrement SP }          if (HexFF AND regs[SP] > 0) then
                          begin
                            regAssign(SP, regs[SP] - 1, dontSetFlags)
                          end
                          else
                          begin
                            error('Stack overflow.', currToken);
                          end;

                          formMain.currInstr
    end
end;

{ ---------------------------------------------------------------------- }

constructor tokNode.create;
begin
  next            := aaa.tokListHead;
  aaa.tokListHead := self;

  isSourceLabel := False;
  isDestlabel   := False;
  address       := -1;
  opHex         := 0;
  opAscii       := '00';
  token         := 'END';
  opBin         := '00000000';
  selStart      := 0;
  selLength     := 0;
  lsStart       := 0;
end;

{ ---------------------------------------------------------------------- }
                                           { Works on SmallInt and Bytes }
function Taaa.h2b(n : SmallInt) : String;  { 127 -> '01111111'           }
Var answer : String;
    i      : SmallInt;
begin
  answer := '';
  i := Hex80;

  while i > 0 do
  begin
    if (n AND i) <> 0 then
      answer := answer + '1'
    else
      answer := answer + '0';

    i := i div 2;
  end;

  result := answer
end;

{ ---------------------------------------------------------------------- }

procedure tokNode.setTokenValues(aValue : smallInt);
begin
  if aValue AND Hex80 <> 0 then                { signed byte to smallInt }
    aValue := aValue OR HexFF80
  else
    aValue := aValue AND Hex7F;

  opHex   := aValue;                           {  A0            }
  opAscii := intToHex(HexFF AND aValue, 2);    { 'A0'           }
  opBin   := aaa.h2b(HexFF AND aValue);        { '10100000'     }
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.listAdd(s : String; wasComment : Boolean);
begin
  FormMain.memoList.lines.add(s);
  prevTokComment := wasComment;
end;

{ ---------------------------------------------------------------------- }

function Taaa.validReg(n : smallInt) : Boolean;
begin
  if n in [0, 1, 2, 3] then result := true else result := false
end;

{ ---------------------------------------------------------------------- }

function Taaa.validLabel(s, colon : string) : boolean;
Var i, l  : Integer;
    ok    : Boolean;
begin
  ok := true;

  if colon = ':' then
    l := length(s) - 1
  else
    l := length(s);

  for i := 1 to l do
  begin
    if (i = 1) and (not (s[i] in ['a'..'z', 'A'..'Z', '_'])) then
    begin
      ok := false;
      break
    end
    else if (not (s[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '_'])) then
    begin
      ok := false;
      break
    end
  end;

  result := ok
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.freeAllTokNodes;
Var pDel : tokNode;
    i    : Integer;
begin
  while tokListHead <> Nil do
  begin
    pDel := tokListHead;
    tokListHead := tokListHead.next;
    pDel.free
  end;

  for i := 0 to 255 do
  begin
    ram[i] := Nil
  end
end;

{ ---------------------------------------------------------------------- }

function Taaa.ramTo0 : string;               { ram[regs[IP]].opToken     }
begin
  result := ram[HexFF AND regs[IP]].Token
end;

{ ---------------------------------------------------------------------- }

function Taaa.ramTo1 : string;               { ram[regs[IP] + 1].opToken }
begin
  result := ram[HexFF AND regs[IP] + 1].Token
end;

{ ---------------------------------------------------------------------- }

function Taaa.ramTo2 : string;               { ram[regs[IP] + 2].opToken }
begin
  result := ram[HexFF AND regs[IP] + 2].Token
end;

{ ---------------------------------------------------------------------- }

function Taaa.ramOP0 : smallInt;              { ram[regs[IP]].opHex      }
begin
  result := ram[HexFF AND regs[IP]].opHex
end;

{ ---------------------------------------------------------------------- }

function Taaa.ramOP1 : smallInt;              { ram[regs[IP] + 1].opHex  }
begin
  result := ram[HexFF AND regs[IP] + 1].opHex
end;

{ ---------------------------------------------------------------------- }

function Taaa.ramOP2 : smallInt;              { ram[regs[IP] + 2].opHex  }
begin
  result := ram[HexFF AND regs[IP] + 2].opHex
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.regsToGray;
begin
  FormMain.labelAL.Color := clBtnFace;
  FormMain.labelBL.Color := clBtnFace;
  FormMain.labelCL.Color := clBtnFace;
  FormMain.labelDL.Color := clBtnFace;
  FormMain.labelSR.Color := clBtnFace;
  FormMain.labelSP.Color := clBtnFace;
  FormMain.labelIP.Color := clBtnFace
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.ramAssign(anAddress, aValue : smallInt);
begin
  ram[HexFF AND anAddress].opHex     := aValue;                         {  D0        }
  ram[HexFF AND anAddress].opAscii   := intToHex(aValue AND HexFF, 2);  { 'D0'       }
  ram[HexFF AND anAddress].token     := intToHex(aValue AND HexFF, 2);  { 'D0'       }
  ram[HexFF AND anAddress].opBin     := h2b(aValue);                    { '11010000' }
  ram[HexFF AND anAddress].selStart  := 0;
  ram[HexFF AND anAddress].selLength := 0;

  if (HexFF AND anAddress) >= HexC0 then
  begin
    formMain.myShow(formVDU)
  end;

  FormRamHex.paintOne(HexFF AND anAddress)
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.error(aMessage : String; aToken : tokNode);
begin
  ok := False;
  formMain.doHalt;

  listAdd(';', false);
  listAdd('; ' + aMessage, false);

  FormMain.Status(aMessage, clRed, clYellow);

  FormMain.PageControl1.ActivePage := FormMain.TabSheetSourceCode;
  FormMain.memoSource.readOnly     := false;
  FormMain.memoSource.color        := clWindow;
  FormMain.memoSource.SetFocus;
  FormMain.MemoSource.SelStart     := aToken.selStart;
  FormMain.MemoSource.SelLength    := aToken.selLength;
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.generate(aNode : tokNode);
begin
  if ramAddr > 255 then
  begin
    error('Can not generate code beyond the end of RAM.', currToken)
  end
  else
  begin
    if ram[ramAddr] = Nil then
    begin
      if FormMain.CheckBoxAsmLog.Checked then
      begin
        FormMain.MemoAsmLog.Lines.Add('Generate machine code: ' + aNode.token + #9 + IntToHex($FF AND aNode.opHex, 2));
      end;
      ram[ramAddr] := aNode;
      ramAddr := ramAddr + 1
    end
    else
    begin
      error('Can not overwrite code already generated.', currToken)
    end
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.incIII;
begin
  iii := iii + 1
end;

{ ---------------------------------------------------------------------- }

function Taaa.tokenAt(n : smallInt) : tokNode;
begin
  if (n >= 0) AND
     (n < FormMain.listBoxTokens.items.count) then
  begin
    result := FormMain.listBoxTokens.items.objects[n] as tokNode;
    if FormMain.CheckBoxAsmLog.Checked then
    begin
      FormMain.listBoxTokens.itemIndex := n;
      Application.ProcessMessages;
      sleep(5);
    end
  end
  else
  begin
    result := Nil
  end
end;

{ ---------------------------------------------------------------------- }

function  Taaa.trim(s : string) : string;
var i    : Integer;
    copy : boolean;
    last : Integer;
    dest : string;
begin
  copy := false;
  last := length(s);
  dest := '';

  while (last > 0) AND ((s[last] = ' ') OR (s[last] = #9)) do
  begin
    last := last - 1
  end;

  i := 1;

  while i <= last do
  begin
    if (s[i] <> ' ') AND (s[i] <> #9) then
    begin
      copy := true
    end;

    if copy then
    begin
      dest := dest + s[i]
    end;

    i := i + 1
  end;

  result := dest
end;

{ ---------------------------------------------------------------------- }

function Taaa.currToken : tokNode;
begin
  if iii < FormMain.listBoxTokens.items.count then
  begin
    result := FormMain.listBoxTokens.items.objects[iii] as tokNode;
    if FormMain.CheckBoxAsmLog.Checked then
    begin
      FormMain.listBoxTokens.itemIndex := iii;
      Application.ProcessMessages;
      sleep(5);
    end
  end
  else
  begin
    result := FormMain.listBoxTokens.items.objects[
                FormMain.listBoxTokens.items.count - 1] as tokNode;
    if FormMain.CheckBoxAsmLog.Checked then
    begin
      FormMain.listBoxTokens.itemIndex := FormMain.listBoxTokens.items.count - 1;
      Application.ProcessMessages;
      sleep(5);
    end
  end
end;

{ ---------------------------------------------------------------------- }

function Taaa.prevToken : tokNode;
begin
  if ((iii - 1) >= 0) AND ((iii - 1) < FormMain.listBoxTokens.items.count) then
  begin
    result := FormMain.listBoxTokens.items.objects[iii - 1] as tokNode;
    if FormMain.CheckBoxAsmLog.Checked then
    begin
      FormMain.listBoxTokens.itemIndex := iii;
      Application.ProcessMessages;
      sleep(5);
    end
  end
  else
  begin
    result := FormMain.listBoxTokens.items.objects[
                FormMain.listBoxTokens.items.count - 1] as tokNode;
    if FormMain.CheckBoxAsmLog.Checked then
    begin
      FormMain.listBoxTokens.itemIndex := FormMain.listBoxTokens.items.count - 1;
      Application.ProcessMessages;
      sleep(5);
    end
  end
end;

{ ---------------------------------------------------------------------- }

function  Taaa.currTokStr : String;
begin
  if iii < FormMain.listBoxTokens.items.count then
  begin
    result := (FormMain.listBoxTokens.items.objects[iii] as tokNode).token;
    if FormMain.CheckBoxAsmLog.Checked then
    begin
      FormMain.listBoxTokens.itemIndex := iii;
      Application.ProcessMessages;
      sleep(5);
    end
  end
  else
  begin
    result := (FormMain.listBoxTokens.items.objects[
                 FormMain.listBoxTokens.items.count - 1] as tokNode).token;
    if FormMain.CheckBoxAsmLog.Checked then
    begin
      FormMain.listBoxTokens.itemIndex := FormMain.listBoxTokens.items.count - 1;
      Application.ProcessMessages;
      sleep(5);
    end
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.TokeniseSource;
Var i, j       : smallInt;
    start, len : integer;
    s          : char;
    tok        : string;
    inToken    : Boolean;
    inComment  : Boolean;
    inString   : Boolean;
    atLineEnd  : Boolean;
    f          : file;
    buf        : ^TBuffer;
    numGot     : Integer;

        procedure addToken(s : String; aSelStart, aSelLength : integer);
        Var n : tokNode;
        begin
          if length(s) = 0 then Exit;

          n           := tokNode.Create;
          n.selStart  := aSelStart;
          n.selLength := aSelLength;
          if (s[1] = ';') OR (s[1] = '#') then
            n.token := s
          else
            n.token := upperCase(s);
          len         := 0;
          tok         := '';
          inToken     := false;
          inString    := false;
          inComment   := false;
          FormMain.listBoxTokens.items.AddObject(n.token, n);
          if FormMain.CheckBoxAsmLog.Checked then
          begin
            FormMain.listBoxTokens.itemIndex := FormMain.listBoxTokens.items.Count - 1;
            Application.ProcessMessages;
            sleep(5);
          end
        end;

begin
  if FormMain.CheckBoxAsmLog.Checked then
  begin
    FormMain.MemoAsmLog.Lines.Add('Convert source to tokens.');
  end;

  FormMain.listBoxTokens.items.clear;

  i         := 0;
  j         := 0;
  start     := 0;
  len       := 0;
  inToken   := false;
  inComment := false;
  inString  := false;
  atLineEnd := false;

  tok := '';

  getMem(buf, sizeOf(TBuffer));

  assignFile(f, formMain.saveDialog1.fileName);
  reset(f, 1);
  blockRead(f, buf^, sizeOf(TBuffer), numGot);
  system.closeFile(f);

  if numGot <> FormMain.memoSource.getTextLen then
  begin
    messageDlg('Block read error', mtError, [mbOK], 0)
  end;

  while i < numGot do
  begin
    s := buf^[i];

    case s of
      ',',   '[',   ']' :
      begin
        atLineEnd := false;
        if inComment OR inString then
        begin
          tok := tok + s
        end
        else if inToken then
        begin
          addToken(tok, start, len);

          start := i;
          len   := 1;
          addToken(s, start, len)
        end
        else
        begin
          start := i;
          len   := 1;
          addToken(s, start, len)
        end
      end;

      ' ',   #9 :
      begin
        atLineEnd := false;
        if inComment OR inString then
        begin
          tok := tok + s
        end
        else if inToken then
        begin
          addToken(tok, start, len)
        end
      end;

      #10, #13 :
      begin
        j := -1;
        if inString then
        begin
          addToken(tok, start, len)
        end
        else if inComment OR inToken then
        begin
          addToken(tok, start, len)
        end
        else if atLineEnd then
        begin
          if ord(s) = 13 then
          begin
            tok := '#';
            addToken(tok, start, 0);
          end;

          atLineEnd := true
        end
        else
        begin
          atLineEnd := true
        end
      end;

      ';' :
      begin
        atLineEnd := false;
        if inComment OR inString then
        begin
          tok := tok + s
        end
        else if inToken then
        begin
          addToken(tok, start, len);

          if j = 0 then tok := '#' else tok := ';';

          inComment := true;
          start     := i;
          len       := 0
        end
        else
        begin
          if j = 0 then tok := '#' else tok := ';';

          inComment := true;
          start     := i;
          len       := 0
        end
      end;

      '"' :
      begin
        atLineEnd := false;
        if inComment then
        begin
          tok := tok + s
        end
        else if inString then
        begin
          inString := false;
          tok := tok + '"';
          addToken(tok, start, len)
        end
        else if inToken then { END OF TOKEN - START OF STRING }
        begin
          addToken(tok, start, len);

          inString := true;
          tok := '"';
          start := i;
          len := 0
        end
        else { START OF STRING }
        begin
          inString := true;
          tok := '"';
          start := i;
          len := 0
        end
      end
      else { DEFAULT }
      begin
        if inString or inComment then
        begin
          tok := tok + s
        end
        else
        begin
          tok := tok + s;

          if not inToken then
          begin
            inToken := true;
            start   := i;
            len     := 0
          end
        end
      end;
    end;

    i := i + 1;
    j := j + 1;
    len := len + 1
  end;

  addToken(tok, start, len);
  FormMain.memoSource.selstart  := 0;
  FormMain.memoSource.sellength := 0;

  freeMem(buf, sizeOf(TBuffer));
end;

{ ---------------------------------------------------------------------- }

function Taaa.mightHaveComment : Boolean;
Var s : String;
begin
  if FormMain.CheckBoxAsmLog.Checked then
  begin
    FormMain.MemoAsmLog.Lines.Add('Might have comment');
  end;

  s := currTokStr;

  if (s[1] = '#') OR (s[1] = ';') then
  begin
    result := true;
    incIII
  end
  else
  begin
    result := false
  end
end;

{ ---------------------------------------------------------------------- }

function Taaa.mightHave(sym : String) : Boolean;
begin
  if FormMain.CheckBoxAsmLog.Checked then
  begin
    FormMain.MemoAsmLog.Lines.Add('Might have ' + sym);
  end;

  if currTokStr = sym then
  begin
    result := true;
    incIII
  end
  else
  begin
    result := false
  end
end;

{ ---------------------------------------------------------------------- }

function Taaa.mightHaveDestLabel : Boolean;
Var i : smallInt;
begin
  if FormMain.CheckBoxAsmLog.Checked then
  begin
    FormMain.MemoAsmLog.Lines.Add('Might have dest'' label');
    FormMain.MemoAsmLog.Lines.Add('Search for duplicate label');
  end;

  if currTokStr[length(currTokStr)] = ':' then
  begin
    { Check for invalid ident }
    if not validLabel(currTokStr, ':') then
    begin
      error('Labels should begin with a character and contain A..Z, a..z, 0..9 or _.', currToken);
      result := false;
      exit
    end;

    { Check for duplicate }
    for i := 0 to FormMain.listBoxTokens.items.count - 1 do
    begin
      if (tokenAt(i).token = currTokStr) AND (tokenAt(i).isDestLabel) then
      begin
        error('Duplicate destination labels are not allowed.', currToken);
        result := false;
        exit
      end
    end;

    currToken.address     := ramAddr;
    currToken.isDestlabel := true;

    incIII;
    result := true
  end
  else
  begin
    result := false
  end
end;

{ ---------------------------------------------------------------------- }

function Taaa.mustHave(sym : String) : Boolean;
begin
  if FormMain.CheckBoxAsmLog.Checked then
  begin
    FormMain.MemoAsmLog.Lines.Add('Must have ' + sym);
  end;

  if currTokStr = sym then
  begin
    result := true;
    incIII
  end
  else
  begin
    error('Expected ' + sym + '  Got ' + currTokStr, currToken);
    result := false
  end
end;

{ ---------------------------------------------------------------------- }

function Taaa.mightHaveRegister : Boolean;
begin
  if FormMain.CheckBoxAsmLog.Checked then
  begin
    FormMain.MemoAsmLog.Lines.Add('Might have register');
  end;

  if currTokStr = 'AL' then
  begin
    currToken.opHex   := 0;
    currToken.opAscii := '00';
    currToken.opBin   := '00000000';
    result := true;
    incIII
  end
  else if currTokStr = 'BL' then
  begin
    currToken.opHex   := 1;
    currToken.opAscii := '01';
    currToken.opBin   := '00000001';
    result := true;
    incIII
  end
  else if currTokStr = 'CL' then
  begin
    currToken.opHex   := 2;
    currToken.opAscii := '02';
    currToken.opBin   := '00000010';
    result := true;
    incIII
  end
  else if currTokStr = 'DL' then
  begin
    currToken.opHex   := 3;
    currToken.opAscii := '03';
    currToken.opBin   := '00000011';
    result := true;
    incIII
  end
  else
  begin
    result := false
  end
end;

{ ---------------------------------------------------------------------- }

function Taaa.mightHaveHexNum : Boolean;
Var line : String;
begin
  if FormMain.CheckBoxAsmLog.Checked then
  begin
    FormMain.MemoAsmLog.Lines.Add('Might have hex num');
  end;

  line := currTokStr;

  if (length(line)= 1) and
     (((line[1] >= '0') and (line[1] <= '9')) or
      ((line[1] >= 'A') and (line[1] <= 'F'))) then
  begin
    currToken.setTokenValues(charToHex(line[1]));
    result := true;
    incIII
  end
  else if (length(line) = 2) and
          ((((line[1] >= '0') and (line[1] <= '9')) or
            ((line[1] >= 'A') and (line[1] <= 'F'))) and
           (((line[2] >= '0') and (line[2] <= '9')) or
            ((line[2] >= 'A') and (line[2] <= 'F')))) then
  begin
    currToken.setTokenValues(16 * charToHex(line[1])
                                + charToHex(line[2]));
    result := true;
    incIII
  end
  else
  begin
    result := false
  end
end;

{ ---------------------------------------------------------------------- }

function Taaa.charToHex(aChar : char) : smallInt; { 'A' -> 10 }
begin
  case aChar of
    '0'      : result := 0;
    '1'      : result := 1;
    '2'      : result := 2;
    '3'      : result := 3;
    '4'      : result := 4;
    '5'      : result := 5;
    '6'      : result := 6;
    '7'      : result := 7;
    '8'      : result := 8;
    '9'      : result := 9;
    'A', 'a' : result := 10;
    'B', 'b' : result := 11;
    'C', 'c' : result := 12;
    'D', 'd' : result := 13;
    'E', 'e' : result := 14;
    'F', 'f' : result := 15
  else
    result := -1
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.CPUReset;
begin
  regAssign(AL, 0,   dontSetFlags);
  regAssign(BL, 0,   dontSetFlags);
  regAssign(CL, 0,   dontSetFlags);
  regAssign(DL, 0,   dontSetFlags);
  regAssign(IP, 0,   dontSetFlags);
  regAssign(SR, 0,   dontSetFlags);
  regAssign(SP, HexBF, dontSetFlags);
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.calcJumps;
Var source, dest  : smallInt;
    found : boolean;
begin
  if FormMain.CheckBoxAsmLog.Checked then
  begin
    FormMain.MemoAsmLog.Lines.Add('Calculate jumps');
  end;

  for source := 0 to FormMain.listBoxTokens.items.count - 1 do
  begin
    if tokenAt(source).isSourceLabel then
    begin
      found := False;
      for dest := 0 to FormMain.listBoxTokens.items.count - 1 do
      begin
        if (tokenAt(dest).isDestLabel) AND
           (copy(tokenAt(dest).token, 1, length(tokenAt(dest).token) - 1) =
           tokenAt(source).token) then
        begin
          found := true;
             if (tokenAt(dest).address - tokenAt(source).address > 127) OR
             (tokenAt(dest).address - tokenAt(source).address < -128) then
          begin
            error('Jump can not exceed -128..127  ' + tokenAt(source).token, tokenAt(source));
            exit
          end
          else
          begin
            if FormMain.CheckBoxAsmLog.Checked then
            begin
              FormMain.MemoAsmLog.Lines.Add(tokenAt(source).token + ' distance = ' + IntToStr(tokenAt(dest).address - tokenAt(source).address));
            end;

            tokenAt(source).address := tokenAt(dest).address - tokenAt(source).address;
            tokenAt(source).setTokenValues(tokenAt(source).address);

            { Patch List File }
            formMain.memoList.SelStart := tokenAt(source).lsStart;
            formMain.memoList.SelLength := 1;
            while formMain.memoList.SelText <> '?' do
            begin
              formMain.memoList.SelStart := formMain.memoList.SelStart + 1;
              formMain.memoList.SelLength := 1;
            end;
            formMain.memoList.SelLength := 2;
            formMain.memoList.SelText := tokenAt(source).opAscii;
            { End Patch List File }

            continue
          end
        end
      end;

      if not found then
      begin
        error('Jump without a destination.  ' + tokenAt(source).token, tokenAt(source));
        exit
      end
    end
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.cmp;
Var op, arg1, arg2 : smallint;

        procedure ll;
        begin
          logRegs;
          log('FETCH');
          log(#9 + 'Fetch ' + ramTo0);
          log(#9 + 'Fetch ' + ramTo1);
          log(#9 + 'Fetch ' + ramTo2);
          log('');
          log('EXECUTE');
          log(#9 + ramTo0 + #9 + ramTo1 + ',' + ramTo2);
          log('');
        end;

begin
  ll;

  if regs[IP] > (256 - 4) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
    op   := ramOP0;
    arg1 := ramOP1;
    arg2 := ramOP2;

    case HexFF AND op of
      $DA : begin                              { CMP     AL,BL }
              if validReg(arg1) then
              begin
                if validReg(arg2) then
                begin
                  if regs[arg1] = regs[arg2] then
                    regAssign(SR, (regs[sr] AND I_I) OR 2, dontSetFlags)
                  else if regs[arg1] < regs[arg2] then
                    regAssign(SR, (regs[sr] AND I_I) OR 8, dontSetFlags)
                  else
                    regAssign(SR, (regs[sr] AND I_I) OR 0, dontSetFlags)
                end
                else
                begin
                  error(RegNotExi, currToken);
                end
              end
              else
              begin
                error(RegNotExi, currToken);
              end
            end;

      $DB : begin                              { CMP     AL,13 }
              if validReg(arg1) then
              begin
                if regs[arg1] = arg2 then
                  regAssign(SR, (regs[sr] AND I_I) OR 2, dontSetFlags)
                else if regs[arg1] < arg2 then
                  regAssign(SR, (regs[sr] AND I_I) OR 8, dontSetFlags)
                else
                  regAssign(SR, (regs[sr] AND I_I) OR 0, dontSetFlags)
              end
              else
              begin
                error(RegNotExi, currToken);
              end
            end;

      $DC : begin                              { CMP     AL,[13] }
              if validReg(arg1) then
              begin
                if regs[arg1] = ram[$FF AND arg2].opHex then
                  regAssign(SR, (regs[sr] AND I_I) OR 2, dontSetFlags)
                else if regs[arg1] < ram[$FF AND arg2].opHex then
                  regAssign(SR, (regs[sr] AND I_I) OR 8, dontSetFlags)
                else
                  regAssign(SR, (regs[sr] AND I_I) OR 0, dontSetFlags)
              end
              else
              begin
                error(RegNotExi, currToken);
              end
            end;

    end;

    regAssign(IP, regs[IP] + 3, dontSetFlags);
    logIP('3');
  end
end;

{ ---------------------------------------------------------------------- }

function  Taaa.___isoz_ : String;
var s : string;
    i : SmallInt;
begin
  s := '';

  i := regs[SR] AND HexFF;

  if i AND $80 <> 0 then s := s + '?' else s := s + '.';
  if i AND $40 <> 0 then s := s + '?' else s := s + '.';
  if i AND $20 <> 0 then s := s + '?' else s := s + '.';
  if i AND $10 <> 0 then s := s + 'I' else s := s + '.';
  if i AND  $8 <> 0 then s := s + 'S' else s := s + '.';
  if i AND  $4 <> 0 then s := s + 'O' else s := s + '.';
  if i AND  $2 <> 0 then s := s + 'Z' else s := s + '.';
  if i AND  $1 <> 0 then s := s + '?' else s := s + '.';

  result := s;
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.Parse;
Var i    : smallInt;
    s    : String;
    arg0 : tokNode;
begin
  for i := 0 to 255 do
  begin
    ram[i] := Nil
  end;

  FormMain.MemoList.Clear;
  listAdd('; LIST FILE', false);
  listAdd(';', false);
  listAdd('; This shows the machine code that was generated and', false);
  listAdd('; the addresses at which the codes were stored.', false);
  listAdd(';', false);

  ramAddr := 0;
  ok := True;
  iii := 0;
  eos := false;

  while (iii < FormMain.ListBoxTokens.Items.Count) AND (Not eos) Do
  begin
    arg0 := currToken;
    s    := currTokStr;
    if mightHaveComment then
    begin
      if s[1] = '#' then
      begin
        if length(s) > 1 then
        begin
          s[1] := ';';
          listAdd(s, false)
        end
        else
        begin
          listAdd(TABChar + TABChar + TABChar + ';' + TABChar + TABChar, false)
        end
      end
      else
      begin
        if prevTokComment then
        begin
          listAdd(TABChar + TABChar + TABChar + ';' +
                                      TABChar + TABChar + TABChar + s, false);
          prevTokComment := true
        end
        else
        begin
          FormMain.MemoList.Lines[FormMain.MemoList.Lines.Count - 1] :=
            FormMain.MemoList.Lines[FormMain.MemoList.Lines.Count - 1] +
            TABChar + s;
          prevTokComment := true
        end
      end
    end
    else if mightHave('MOV')   then parseMov

    else if mightHave('ADD')   then parseAdd
    else if mightHave('SUB')   then parseAdd
    else if mightHave('MUL')   then parseAdd
    else if mightHave('DIV')   then parseAdd
    else if mightHave('MOD')   then parseAdd
    else if mightHave('AND')   then parseAdd
    else if mightHave('OR')    then parseAdd
    else if mightHave('XOR')   then parseAdd

    else if mightHave('ROL')   then parseInc
    else if mightHave('ROR')   then parseInc
    else if mightHave('SHL')   then parseInc
    else if mightHave('SHR')   then parseInc
    else if mightHave('INC')   then parseInc
    else if mightHave('DEC')   then parseInc
    else if mightHave('NOT')   then parseInc
    else if mightHave('PUSH')  then parseInc
    else if mightHave('POP')   then parseInc

    else if mightHave('CALL')  then parseCallInt
    else if mightHave('INT')   then parseCallInt
    else if mightHave('ORG')   then parseCallInt
    else if mightHave('DB')    then parseDB
    else if mightHave('JMP')   then parseJmp
    else if mightHave('JZ')    then parseJmp
    else if mightHave('JNZ')   then parseJmp
    else if mightHave('JO')    then parseJmp
    else if mightHave('JNO')   then parseJmp
    else if mightHave('JS')    then parseJmp
    else if mightHave('JNS')   then parseJmp

    else if mightHave('CMP')   then parseCmp

    else if mightHave('RET')   then parseZerop
    else if mightHave('IRET')  then parseZerop
    else if mightHave('PUSHF') then parseZerop
    else if mightHave('POPF')  then parseZerop
    else if mightHave('HALT')  then parseZerop
    else if mightHave('NOP')   then parseZerop
    else if mightHave('CLI')   then parseZerop
    else if mightHave('STI')   then parseZerop
    else if mightHave('CLO')   then parseZerop

    else if mightHave('IN')    then parseInOut
    else if mightHave('OUT')   then parseInOut

    else if mightHave('END') then
    begin
      arg0.setTokenValues(0);
      generate(arg0);
      eos := true;
      listAdd(TABChar + arg0.token + TABChar +
            TABChar +
            '; [' + IntToHex(ramAddr - 1, 2) + ']  ' +
            arg0.opAscii +
            TABChar, false)
    end
    else if mightHaveDestlabel then
    begin
      { Do nothing except write to list file. }
      listAdd(prevToken.Token + TABChar + TABChar +
                                TABChar + ';' + TABChar + TABChar, false)
    end
    else
    begin
      if ok then
      begin
        if (currTokStr = 'CALL') OR
           (currTokStr = 'RET') OR
           (currTokStr = 'INT') OR
           (currTokStr = 'IRET') OR
           (currTokStr = 'STI') OR
           (currTokStr = 'CLI') Then
        begin
          error('Instruction not available in the shareware version : ' + currTokStr, currToken)
        end
        else
        begin
          error('Expected a comment, label or machine instruction.  Got ' + currTokStr + '.', currToken)
        end
      end;
      exit
    end
  end;

  if Not eos then
  begin
    error('Expected  END  at the end of the source code.', currToken);
  end
  else
  begin
    listAdd(';', false);
    listAdd('; SUCCESS : No errors found.', false);
    FormMain.MemoList.Lines[0] := FormMain.MemoList.Lines[0] + ' : SUCCESS : No errors found.';
    FormMain.MemoList.selStart := 0;

    if FormMain.CheckBoxAsmLog.Checked then
    begin
      FormMain.MemoAsmLog.Lines.Add('FINISHED - SUCCESS');
    end;

    for i := 0 to 255 do
    begin
      if ram[i] = Nil then
      begin
        ram[i] := tokNode.create;
        if i >= HexC0 then
        begin
          ram[i].setTokenValues($20);
          ram[i].token := ' '
        end
      end
    end
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.ParseInc;
Var aCmd : TokNode;
    arg1 : TokNode;
begin
  aCmd := prevToken;
  arg1 := currToken;
  if mightHaveRegister then                        { INC    AL      }
  begin
    if      aCmd.token = 'ROL'  then aCmd.setTokenValues($9A)
    else if aCmd.token = 'ROR'  then aCmd.setTokenValues($9B)
    else if aCmd.token = 'SHL'  then aCmd.setTokenValues($9C)
    else if aCmd.token = 'SHR'  then aCmd.setTokenValues($9D)
    else if aCmd.token = 'INC'  then aCmd.setTokenValues($A4)
    else if aCmd.token = 'DEC'  then aCmd.setTokenValues($A5)
    else if aCmd.token = 'NOT'  then aCmd.setTokenValues($AD)
    else if aCmd.token = 'PUSH' then aCmd.setTokenValues($E0)
    else if aCmd.token = 'POP'  then aCmd.setTokenValues($E1);

    generate(aCmd);
    generate(arg1);
    aCmd.selLength := (arg1.selStart + arg1.selLength) - aCmd.selStart;
    listAdd(TABChar + aCmd.token + TABChar +
            arg1.token + TABChar +
            '; [' + IntToHex(ramAddr - 2, 2) + ']  ' +
            aCmd.opAscii + ' ' +
            arg1.opAscii + TABChar, false)
  end
  else
  begin
    error('Expected AL, BL, CL or DL.  Got ' + currTokStr, currToken)
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.ParseInOut;
Var aCmd : TokNode;
    arg1 : TokNode;
begin
  aCmd := prevToken;

  if aCmd.token = 'IN'  then aCmd.setTokenValues($F0) else
  if aCmd.token = 'OUT' then aCmd.setTokenValues($F1);

  arg1 := currToken;
  if mightHaveHexNum then
  begin
    generate(aCmd);
    generate(arg1);
    aCmd.selLength := (arg1.selStart + arg1.selLength) - aCmd.selStart;
    listAdd(TABChar + aCmd.token + TABChar +
            arg1.token + TABChar +
            '; [' + IntToHex(ramAddr - 1, 2) + ']  ' +
            aCmd.opAscii + ' ' +
            arg1.opAscii + TABChar, false)
  end
  else
  begin
    error('Expected a hexadecimal number.  Got ' + currTokStr, currToken)
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.ParseDB;
Var aCmd  : TokNode;
    arg1  : TokNode;
    aNode : TokNode;
    i     : Integer;
begin
  aCmd := prevToken;
  arg1 := currToken;
  if mightHaveHexNum then
  begin
    aCmd.setTokenValues($00);
    generate(arg1);
    listAdd(TABChar + aCmd.token + TABChar +
            arg1.token + TABChar +
            '; [' + IntToHex(ramAddr - 1, 2) + ']  ' +
            arg1.opAscii + TABChar, false)
  end
  else if arg1.token[1] = '"' then
  begin
    if arg1.token[length(arg1.token)] <> '"' then
    begin
      error('Missing closing quote.  ' + currTokStr, currToken)
    end
    else
    begin
      arg1.selLength := arg1.selLength + 1;      { KLUDGE }
      for i := 2 to length(arg1.token) - 1 do
      begin
        aNode       := tokNode.Create;

        aNode.token     := arg1.token[i];
        aNode.selStart  := arg1.selStart;
        aNode.selLength := arg1.selLength;
        aNode.setTokenValues(ord(arg1.token[i]));

        generate(aNode);

        if i = 2 then
        begin
          listAdd(TABChar + aCmd.token + TABChar +
                aNode.token + TABChar +
                '; [' + IntToHex(ramAddr - 1, 2) + ']  ' +
                aNode.opAscii + TABChar, false)
        end
        else
        begin
          listAdd(TABChar + TABChar +
                  aNode.token + TABChar +
                  '; [' + IntToHex(ramAddr - 1, 2) + ']  ' +
                  aNode.opAscii + TABChar, false)

        end;
      end;

      incIII
    end
  end
  else
  begin
    error('Expected a hexadecimal number or "text string".  Got ' + currTokStr, currToken)
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.ParseCallInt;
Var aCmd : TokNode;
    arg1 : TokNode;
begin
  aCmd := prevToken;
  arg1 := currToken;
  if mightHaveHexNum then
  begin
    if aCmd.token = 'ORG'  then                  { ORG     50 }
    begin
      { Nothing to generate here }
      ramAddr := HexFF AND arg1.opHex;
      listAdd(TABChar + aCmd.token + TABChar +
              arg1.token + TABChar +
              ';' + TABChar + TABChar, false)
    end
    else if aCmd.token = 'CALL' then                  { CALL    50 }
    begin
      aCmd.setTokenValues($CA);
      generate(aCmd);
      generate(arg1);
      aCmd.selLength := (arg1.selStart + arg1.selLength) - aCmd.selStart;
      listAdd(TABChar + aCmd.token + TABChar +
              arg1.token + TABChar +
              '; [' + IntToHex(ramAddr - 2, 2) + ']  ' +
              aCmd.opAscii + ' ' +
              arg1.opAscii + TABChar, false)
    end
    else if aCmd.token = 'INT'  then             { INT   02 }
    begin
      aCmd.setTokenValues($CC);
      generate(aCmd);
      generate(arg1);
      aCmd.selLength := (arg1.selStart + arg1.selLength) - aCmd.selStart;
      listAdd(TABChar + aCmd.token + TABChar +
              arg1.token + TABChar +
              '; [' + IntToHex(ramAddr - 2, 2) + ']  ' +
              aCmd.opAscii + ' ' +
              arg1.opAscii + TABChar, false)
    end
  end
  else
  begin
    error('Expected a hexadecimal number.  Got ' + currTokStr, currToken)
  end
end;

{ ---------------------------------------------------------------------- }
{ ADD AL,15
  ADD AL,BL                                                              }

procedure Taaa.ParseAdd;
Var aCmd : TokNode;
    arg1 : TokNode;
    arg2 : TokNode;
begin
  aCmd := prevToken;
  arg1 := currToken;
  if mightHaveRegister then                        { ADD    AL      }
  begin
    if mustHave(',') then                          { ADD    AL,     }
    begin
      arg2 := currToken;
      if mightHaveHexNum then                      { ADD    AL,15   }
      begin
        if      aCmd.token = 'ADD' then aCmd.setTokenValues($B0)
        else if aCmd.token = 'SUB' then aCmd.setTokenValues($B1)
        else if aCmd.token = 'MUL' then aCmd.setTokenValues($B2)
        else if aCmd.token = 'DIV' then aCmd.setTokenValues($B3)
        else if aCmd.token = 'MOD' then aCmd.setTokenValues($B6)
        else if aCmd.token = 'AND' then aCmd.setTokenValues($BA)
        else if aCmd.token = 'OR'  then aCmd.setTokenValues($BB)
        else if aCmd.token = 'XOR' then aCmd.setTokenValues($BC);

        generate(aCmd);
        generate(arg1);
        generate(arg2);
        aCmd.selLength := (arg2.selStart + arg2.selLength) - aCmd.selStart;
        listAdd(TABChar + aCmd.Token + TABChar +
                                    arg1.token + ',' + arg2.token + TABChar +
                                    '; [' + IntToHex(ramAddr - 3, 2) + ']  ' +
                                    aCmd.opAscii + ' ' +
                                    arg1.opAscii + ' ' +
                                    arg2.opAscii, false)
      end
      else if mightHaveRegister then               { ADD    AL,BL   }
      begin
        if      aCmd.token = 'ADD' then aCmd.setTokenValues($A0)
        else if aCmd.token = 'SUB' then aCmd.setTokenValues($A1)
        else if aCmd.token = 'MUL' then aCmd.setTokenValues($A2)
        else if aCmd.token = 'DIV' then aCmd.setTokenValues($A3)
        else if aCmd.token = 'MOD' then aCmd.setTokenValues($A6)
        else if aCmd.token = 'AND' then aCmd.setTokenValues($AA)
        else if aCmd.token = 'OR'  then aCmd.setTokenValues($AB)
        else if aCmd.token = 'XOR' then aCmd.setTokenValues($AC);

        generate(aCmd);
        generate(arg1);
        generate(arg2);
        aCmd.selLength := (arg2.selStart + arg2.selLength) - aCmd.selStart;
        listAdd(TABChar + aCmd.Token + TABChar +
                                    arg1.token + ',' + arg2.token + TABChar +
                                    '; [' + IntToHex(ramAddr - 3, 2) + ']  ' +
                                    aCmd.opAscii + ' ' +
                                    arg1.opAscii + ' ' +
                                    arg2.opAscii, false)
      end
      else
      begin
        error('Expected a hexadecimal number, AL, BL, CL or DL.  Got ' + currTokStr, currToken)
      end
    end
  end
  else
  begin
    error('Expected AL, BL, CL or DL.  Got ' + currTokStr, currToken)
  end
end;

{ ---------------------------------------------------------------------- }

{ MOV AL,15     D0 00 15           MOV
  MOV BL,[15]   D1 01 15           / \
  MOV [15],CL   D2 15 03          /   \
  MOV DL,[AL]   D3 03 00         /     \
  MOV [CL],AL   D4 03 00       AL       [
                              /        / \
                             ,       15   AL
                            / \       |   |
                          15  [       ]   ]
                             / \      |   |
                           15   BL    ,   ,
                            |   |     |   |
                            ]   ]    AL   BL                        }

procedure Taaa.ParseMov;
Var mov  : TokNode;
    arg1 : TokNode;
    arg2 : TokNode;
begin
  mov := prevToken;

  arg1 := currToken;
  if mightHaveRegister then                        { MOV    AL      }
  begin
    if mustHave(',') then                          { MOV    AL,     }
    begin
      arg2 := currToken;
      if mightHaveRegister then
      begin
        error('Command not available.  Use PUSH ' +
              arg2.token + ' POP ' + arg1.token +
              ' instead of MOV ' + arg1.token + ',' +
              arg2.token, prevToken)
      end
      else if mightHaveHexNum then                 { MOV    AL,15   }
      begin
        mov.setTokenValues($D0);
        generate(mov);
        generate(arg1);
        generate(arg2);
        mov.selLength := (arg2.selStart + arg2.selLength) - mov.selStart;
        listAdd(TABChar + mov.Token + TABChar +
                                    arg1.token + ',' + arg2.token + TABChar +
                                    '; [' + IntToHex(ramAddr - 3, 2) + ']  ' +
                                    mov.opAscii + ' ' +
                                    arg1.opAscii + ' ' +
                                    arg2.opAscii, false)
      end
      else if mightHave('[') then                  { MOV    AL,[    }
      begin
        arg2 := currToken;
        if mightHaveHexNum then                    { MOV    AL,[15  }
        begin
          if mustHave(']') then                    { MOV    AL,[15] }
          begin
            mov.setTokenValues($D1);
            generate(mov);
            generate(arg1);
            arg2.token := '[' + arg2.token + ']';
            generate(arg2);
            mov.selLength := 1 + (arg2.selStart + arg2.selLength) - mov.selStart;
            listAdd(TABChar + mov.Token + TABChar +
                                        arg1.token + ',' + arg2.token +
                                        TABChar + '; [' +
                                        IntToHex(ramAddr - 3, 2) + ']  ' +
                                        mov.opAscii + ' ' +
                                        arg1.opAscii + ' ' +
                                        arg2.opAscii, false)
          end
        end
        else if mightHaveRegister then             { MOV    AL,[AL  }
        begin
          if mustHave(']') then                    { MOV    AL,[AL] }
          begin
            mov.setTokenValues($D3);
            generate(mov);
            generate(arg1);
            arg2.token := '[' + arg2.token + ']';
            generate(arg2);
            mov.selLength := 1 + (arg2.selStart + arg2.selLength) - mov.selStart;
            listAdd(TABChar + mov.Token + TABChar +
                                        arg1.token + ',' + arg2.token +
                                        TABChar + '; [' +
                                        IntToHex(ramAddr - 3, 2) + ']  ' +
                                        mov.opAscii + ' ' +
                                        arg1.opAscii + ' ' +
                                        arg2.opAscii, false)
          end
        end
        else
        begin
          error('Expected a hexadecimal number, AL, BL, CL or DL.  Got ' + currTokStr, currToken)
        end
      end
      else
      begin
        error('Expected a hexadecimal number or ''[''  Got ' + currTokStr, currToken)
      end
    end
  end
  else if mightHave('[') then                      { MOV    [       }
  begin
    arg1 := currToken;
    if mightHaveHexNum then                        { MOV    [15     }
    begin
      if mustHave(']') then                       { MOV    [15]    }
      begin
        if mustHave(',') then                     { MOV    [15],   }
        begin
          arg2 := currToken;
          if mightHaveRegister then                { MOV    [15],AL }
          begin
            mov.setTokenValues($D2);
            generate(mov);
            arg1.token := '[' + arg1.token + ']';
            generate(arg1);
            generate(arg2);
            mov.selLength := (arg2.selStart + arg2.selLength) - mov.selStart;
            listAdd(TABChar + mov.Token + TABChar +
                                        arg1.token + ',' + arg2.token +
                                        TABChar + '; [' +
                                        IntToHex(ramAddr - 3, 2) + ']  ' +
                                        mov.opAscii + ' ' +
                                        arg1.opAscii + ' ' +
                                        arg2.opAscii, false)
          end
          else
          begin
            error('Expected AL, BL, CL or DL.  Got ' + currTokStr, currToken)
          end
        end
      end
    end
    else if mightHaveRegister then                 { MOV    [AL     }
    begin
      if mustHave(']') then                        { MOV    [AL]    }
      begin
        if mustHave(',') then                      { MOV    [AL],   }
        begin
          arg2 := currToken;
          if mightHaveRegister then                { MOV    [AL],BL }
          begin
            mov.setTokenValues($D4);
            generate(mov);
            arg1.token := '[' + arg1.token + ']';
            generate(arg1);
            generate(arg2);
            mov.selLength := (arg2.selStart + arg2.selLength) - mov.selStart;
            listAdd(TABChar + mov.Token + TABChar +
                                        arg1.token + ',' + arg2.token +
                                        TABChar + '; [' +
                                        IntToHex(ramAddr - 3, 2) + ']  ' +
                                        mov.opAscii + ' ' +
                                        arg1.opAscii + ' ' +
                                        arg2.opAscii, false)
          end
          else
          begin
            error('Expected AL, BL, CL or DL  Got ' + currTokStr, currToken)
          end
        end
      end
    end
    else
    begin
      error('Expected a hexadecimal number, AL, BL, CL or DL  Got ' + currTokStr, currToken)
    end
  end
  else
  begin
    error('Expected AL, BL, CL, DL or ''[''  Got ' + currTokStr, currToken)
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.ParseZerop;
Var op : tokNode;
begin
  op := prevToken;

       if op.token = 'RET'   then op.setTokenValues($CB)
  else if op.token = 'IRET'  then op.setTokenValues($CD)
  else if op.token = 'PUSHF' then op.setTokenValues($EA)
  else if op.token = 'POPF'  then op.setTokenValues($EB)
  else if op.token = 'NOP'   then op.setTokenValues(HexFF)
  else if op.token = 'CLI'   then op.setTokenValues($FD)
  else if op.token = 'STI'   then op.setTokenValues($FC)
  else if op.token = 'CLO'   then op.setTokenValues($FE)
  else if op.token = 'HALT'  then op.setTokenValues($00);

  generate(op);
  listAdd(TABChar + op.Token + TABChar +
                               TABChar + '; [' +
                               IntToHex(ramAddr - 1, 2) + ']  ' +
                               op.opAscii +
                               TABChar, false);
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.ParseJmp;
Var op : tokNode;
begin
  if not validLabel(currTokStr, ' ') then
  begin
    error('Labels should begin with a character and contain A..Z, a..z, 0..9 or _.', currToken);
    exit
  end;

  currToken.isSourceLabel := true;
  currToken.address := ramAddr;
  currToken.opHex := $0;
  currToken.opAscii := '??';
  currToken.opBin := '00000000';

  op := prevToken;

  if      op.token = 'JMP'  then op.setTokenValues(HexC0)
  else if op.token = 'JZ'   then op.setTokenValues($C1)
  else if op.token = 'JNZ'  then op.setTokenValues($C2)
  else if op.token = 'JS'   then op.setTokenValues($C3)
  else if op.token = 'JNS'  then op.setTokenValues($C4)
  else if op.token = 'JO'   then op.setTokenValues($C5)
  else if op.token = 'JNO'  then op.setTokenValues($C6);

  generate(op);
  generate(currToken);
  op.selLength := (currToken.selStart + currToken.selLength) - op.selStart;
  currToken.lsStart := formMain.memoList.SelStart;
  listAdd(TABChar + op.Token + TABChar +
                               currToken.token +
                               TABChar + '; [' +
                               IntToHex(ramAddr - 2, 2) + ']  ' +
                               op.opAscii + ' ' +
                               currToken.opAscii + TABChar, false);
  incIII
end;

{ ---------------------------------------------------------------------- }
{
                CMP	AL,BL	Comparison Instructions - Register
                CMP	AL,0D	Comparison Instructions - Immediate
                CMP	AL,[13]	Comparison Instructions - Indirect
}
procedure Taaa.parseCmp;
Var op, arg1, arg2 : tokNode;
begin
  op := prevToken;

  arg1 := currToken;
  if mightHaveRegister then
  begin
    if mustHave(',') then
    begin
      arg2 := currToken;
      if mightHaveRegister then
      begin
        op.setTokenValues($DA);
        generate(op);
        generate(arg1);
        generate(arg2);
        op.selLength := (arg2.selStart + arg2.selLength) - op.selStart;
        listAdd(TABChar + op.Token + TABChar +
                arg1.token + ',' + arg2.token +
                TABChar + '; [' +
                IntToHex(ramAddr - 3, 2) + ']  ' +
                op.opAscii + ' ' +
                arg1.opAscii + ' ' +
                arg2.opAscii, false)
      end
      else
      if mightHaveHexNum then
      begin
        op.setTokenValues($DB);
        generate(op);
        generate(arg1);
        generate(arg2);
        op.selLength := (arg2.selStart + arg2.selLength) - op.selStart;
        listAdd(TABChar + op.Token + TABChar +
                arg1.token + ',' + arg2.token +
                TABChar + '; [' +
                IntToHex(ramAddr - 3, 2) + ']  ' +
                op.opAscii + ' ' +
                arg1.opAscii + ' ' +
                arg2.opAscii, false)
      end
      else
      if mightHave('[') then
      begin
        arg2 := currToken;
        if mightHaveHexNum then
        begin
          if mustHave(']') then
          begin
            op.setTokenValues($DC);
            generate(op);
            generate(arg1);
            arg2.token := '[' + arg2.token + ']';
            generate(arg2);
            op.selLength := 1 + (arg2.selStart + arg2.selLength) - op.selStart;
            listAdd(TABChar + op.Token + TABChar +
                    arg1.token + ',' + arg2.token +
                    TABChar + '; [' +
                    IntToHex(ramAddr - 3, 2) + ']  ' +
                    op.opAscii + ' ' +
                    arg1.opAscii + ' ' +
                    arg2.opAscii, false)
          end
        end
        else
        begin
          error('Expected a hexadecimal number.  Got ' + currToken.token, currToken)
        end
      end
      else
      begin
        error('Expected AL, BL, CL or DL.  Got ' + currToken.token, currToken)
      end
    end
  end
  else
  begin
    error('Expected AL, BL, CL or DL.  Got ' + currToken.token, currToken)
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.monop;                              { INC    AL    A4 00 }
                        function ROL(n : smallInt) : smallInt;
                        Var msb : Boolean;
                        begin
                          log(#9 + ramTo0 + #9 + ramTo1 + #9 + '; Rotate the bits in ' + ramTo1 + ' one place to the left.');
                          log(#9 + #9 + #9 + '; The most significant bit is moved to the least significant bit position.');
                          msb := (n AND Hex80) <> 0;
                          n := n * 2;
                          if msb then
                            n := n OR Hex01;

                          if (n AND Hex80) <> 0 then
                            n := n OR HexFF00
                          Else
                            n := n AND Hex7F;

                          result := n
                        end;

                        { ---------------------------------------------- }

                        function ROR(n : smallInt) : smallInt;
                        Var lsb : Boolean;
                        begin
                          log(#9 + ramTo0 + #9 + ramTo1 + #9 + '; Rotate the bits in ' + ramTo1 + ' one place to the right.');
                          log(#9 + #9 + #9 + '; The least significant bit is moved to the most significant bit position.');
                          n := n AND HexFF;
                          lsb := (n AND 1) <> 0;
                          n := n div 2;
                          if lsb then
                            n := n OR Hex80;

                          if (n AND Hex80) <> 0 then
                            n := n OR HexFF00
                          Else
                            n := n AND Hex7F;

                          result := n
                        end;

                        { ---------------------------------------------- }

                        function SSL(n : smallInt) : smallInt;
                        begin
                          log(#9 + ramTo0 + #9 + ramTo1 + #9 + '; Shift the bits in ' + ramTo1 + ' one place to the left.');
                          log(#9 + #9 + #9 + '; The most significant bit is discarded.');
                          n := n * 2;

                          if (n AND Hex80) <> 0 then
                            n := n OR HexFF00
                          Else
                            n := n AND Hex7F;

                          result := n
                        end;

                        { ---------------------------------------------- }

                        function SSR(n : smallInt) : smallInt;
                        begin
                          log(#9 + ramTo0 + #9 + ramTo1 + #9 + '; Shift the bits in ' + ramTo1 + ' one place to the right.');
                          log(#9 + #9 + #9 + '; The least significant bit is discarded.');
                          n := n AND HexFF;
                          n := n div 2;

                          if (n AND Hex80) <> 0 then
                            n := n OR HexFF00
                          Else
                            n := n AND Hex7F;

                          result := n
                        end;

                        { ---------------------------------------------- }

                        function TON(n : smallInt) : smallInt;     { NOT }
                        begin
                          n := NOT n;

                          if (n AND Hex80) <> 0 then
                            n := n OR HexFF00
                          Else
                            n := n AND Hex7F;

                          result := n
                        end;

                        { ---------------------------------------------- }

                        procedure doCall;
                        begin
                          log(#9 + ramTo0 + #9 + ramTo1 + #9 +
                              '; Call the procedure at address ' + ramTo1);
                          log(#9 + #9 + #9 + '; Save IP + 2 onto the stack and decrement SP.');
                          log(#9 + #9 + #9 + '; Set IP to ' + ramTo1);
{ return address       }  ramAssign(regs[SP], regs[IP] + 2);
{ jump to call address }  regAssign(IP, ramOP1, dontSetFlags);
                          regs[IP] := regs[IP] AND HexFF;

{ IP within RAM        }  if (regs[IP] < 0) OR (regs[IP] > 255) then
                          begin
                            error(BeyondRAM, currToken);
                            exit
                          end;

{ Decrement SP }          if (HexFF AND regs[SP] > 0) then
                          begin
                            regAssign(SP, regs[SP] - 1, dontSetFlags)
                          end
                          else
                          begin
                            error('Stack overflow.', currToken);
                          end;
                          
                          logRegs;
                          log('-------------------------------------------------------------------------------------------');
                        end;

                        { ---------------------------------------------- }

                        procedure doInt;
                        begin
                          log(#9 + ramTo0 + #9 + ramTo1 + #9 +
                              '; Call the interrupt routine at the address stored in RAM location ' + ramTo1);
                          log(#9 + #9 + #9 + '; Save IP + 2 onto the stack and decrement SP.');
                          log(#9 + #9 + #9 + '; Set IP to the address fetched from RAM address ' + ramTo1);
{ return address       }  ramAssign(regs[SP], regs[IP] + 2);
{ jump to call address }  regAssign(IP, HexFF AND ram[HexFF AND ramOP1].opHex, dontSetFlags);
                          regs[IP] := regs[IP] AND HexFF;

{ IP within RAM        }  if (regs[IP] < 0) OR (regs[IP] > 255) then
                          begin
                            error(BeyondRAM, currToken);
                            exit
                          end;

{ Decrement SP }          if (HexFF AND regs[SP] > 0) then
                          begin
                            regAssign(SP, regs[SP] - 1, dontSetFlags)
                          end
                          else
                          begin
                            error('Stack overflow.', currToken);
                          end;

                          logRegs;
                          log('-------------------------------------------------------------------------------------------');
                        end;

                        { ---------------------------------------------- }

                        procedure doPush;
                        begin
                          log(#9 + ramTo0 + #9 + ramTo1 + #9 + '; Save ' + ramTo1 + ' onto the stack and decrement SP.');
                          if validReg(ramOP1) then
                          begin
                            ramAssign(regs[SP], regs[ramOP1])
                          end
                          else
                          begin
                            error(RegNotExi, currToken);
                            exit
                          end;

{ Decrement SP }          if (HexFF AND regs[SP] > 0) then
                          begin
                            regAssign(SP, regs[SP] - 1, dontSetFlags)
                          end
                          else
                          begin
                            error('Stack overflow.', currToken);
                          end;
                        end;

                        { ---------------------------------------------- }

                        procedure doPop;
                        begin
                          log(#9 + ramTo0 + #9 + ramTo1 + #9 + '; Increment SP and restore ' + ramTo1 + ' from the stack.');
                          if (HexFF AND regs[SP] < HexBF) then
                          begin
                            regAssign(SP, regs[SP] + 1, dontSetFlags);
                            if validReg(ramOP1) then
                            begin
                              regAssign(ramOP1, ram[HexFF AND regs[SP]].opHex, dontSetFlags);
                            end
                            else
                            begin
                              error(RegNotExi, currToken);
                            end;

                            regs[IP] := regs[IP] AND HexFF;

                            if (regs[IP] < 0) OR (regs[IP] > 255) then
                            begin
                              error(BeyondRAM, currToken);
                            end;
                          end
                          else
                          begin
                            error('Stack underflow.', currToken);
                          end;
                        end;

                        { ---------------------------------------------- }
begin
  logRegs;
  log('FETCH');
  log(#9 + 'Fetch ' + ramTo0);
  log(#9 + 'Fetch ' + ramTo1);
  log('');
            log('EXECUTE');

  if regs[IP] > (256 - 3) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
    case HexFF AND ramOP0 of
      $CA: begin
             doCall;              { CALL  }
             exit                 { Without incrementing IP }
           end;

      $CC: begin
             doInt;               { INT   }
             exit                 { Without incrementing IP }
           end;

      $E0: begin
             doPush;                { PUSH  }
           end;

      $E1: begin
             doPop;                 { POP   }
           end;

      $9A: begin
             regAssign(ramOP1, ROL(regs[ramOP1]), SetFlags); { ROL }
           end;

      $9B: begin
             regAssign(ramOP1, ROR(regs[ramOP1]), SetFlags); { ROR }
           end;

      $9C: begin
             regAssign(ramOP1, SSL(regs[ramOP1]), SetFlags); { SHL }
           end;

      $9D: begin
             regAssign(ramOP1, SSR(regs[ramOP1]), SetFlags); { SHR }
           end;

      $A4: begin
             regAssign(ramOP1, regs[ramOP1] + 1,  SetFlags); { INC }
             log(#9 + ramTo0 + #9 + ramTo1 + #9 + '; Add 1 to ' + ramTo1);
           end;
      $A5: begin
             regAssign(ramOP1, regs[ramOP1] - 1,  SetFlags); { DEC }
             log(#9 + ramTo0 + #9 + ramTo1 + #9 + '; Subtract 1 from ' + ramTo1);
           end;
      $AD: regAssign(ramOP1, TON(regs[ramOP1]), SetFlags); { NOT }
    end;

    regAssign(IP, regs[IP] + 2, dontSetFlags);
    log('');
    logIP('2');
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.arith2;          { ADD    AL,BL    A0 00 01 }
                                { ADD    AL,15    B0 00 15 }
        procedure ll;
        begin
          logRegs;
          log('FETCH');
          log(#9 + 'Fetch ' + ramTo0);
          log(#9 + 'Fetch ' + ramTo1);
          log(#9 + 'Fetch ' + ramTo2);
          log('');
          log('EXECUTE');
          log(#9 + ramTo0 + #9 + ramTo1 + ',' + ramTo2);
          log('');
        end;

begin
  if regs[IP] > (256 - 4) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
    ll;
    case HexFF AND ramOP0 of
      $A0: regAssign(ramOP1, regs[ramOP1]  +  regs[ramOP2], setFlags);
      $A1: regAssign(ramOP1, regs[ramOP1]  -  regs[ramOP2], setFlags);
      $A2: regAssign(ramOP1, regs[ramOP1]  *  regs[ramOP2], setFlags);
      $A3: begin
             if regs[ramOP2] <> 0 then
             begin
               regAssign(ramOP1, regs[ramOP1] DIV regs[ramOP2], setFlags);
             end
             else
             begin
               log(#9 + 'Divide by zero error. Processor halted.');
               log('-------------------------------------------------------------------------------------');
               formMain.doHalt;
               messageDlg('Divide by zero error.', mtError, [mbOK], 0);
               exit
             end
           end;
      $A6: regAssign(ramOP1, regs[ramOP1] mod regs[ramOP2], setFlags);
      $AA: regAssign(ramOP1, regs[ramOP1] AND regs[ramOP2], setFlags);
      $AB: regAssign(ramOP1, regs[ramOP1] OR  regs[ramOP2], setFlags);
      $AC: regAssign(ramOP1, regs[ramOP1] XOR regs[ramOP2], setFlags);

      $B0: regAssign(ramOP1, regs[ramOP1]  +  ramOP2, setFlags);
      $B1: regAssign(ramOP1, regs[ramOP1]  -  ramOP2, setFlags);
      $B2: regAssign(ramOP1, regs[ramOP1]  *  ramOP2, setFlags);
      $B3: begin
             if ramOP2 <> 0 then
             begin
               regAssign(ramOP1, regs[ramOP1] DIV ramOP2, setFlags);
             end
             else
             begin
               log(#9 + 'Divide by zero error. Processor halted.');
               log('-------------------------------------------------------------------------------------');
               formMain.doHalt;
               messageDlg('Divide by zero error.', mtError, [mbOK], 0);
               exit
             end
           end;
      $B6: regAssign(ramOP1, regs[ramOP1] MOD ramOP2, setFlags);
      $BA: regAssign(ramOP1, regs[ramOP1] AND ramOP2, setFlags);
      $BB: regAssign(ramOP1, regs[ramOP1] OR  ramOP2, setFlags);
      $BC: regAssign(ramOP1, regs[ramOP1] XOR ramOP2, setFlags);
    end;

    regAssign(IP, regs[IP] + 3, dontSetFlags);
    logIP('3');
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.imovMR;           { MOV    [BL],AL    D4 01 00 }
var reg1, reg2 : smallInt;
begin
  reg1 := ramOP1;
  reg2 := ramOP2;

  if regs[IP] > (256 - 4) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
    if validReg(reg1) then
    begin
      if validReg(reg2) then
      begin
        logRegs;
        log('FETCH');
        log(#9 + 'Fetch ' + ramTo0);
        log(#9 + 'Fetch ' + ramTo1);
        log(#9 + 'Fetch ' + ramTo2);
        log('');
        log('EXECUTE');
        log(#9 + ramTo0 + #9 + ramTo1 + ',' + ramTo2);
        log('');

        ramAssign(regs[reg1], regs[reg2]);

        regAssign(IP, regs[IP] + 3, dontSetFlags);

        logIP('3');
      end
      else
      begin
        error(RegNotExi, currToken);
      end
    end
    else
    begin
      error(RegNotExi, currToken);
    end
  end;
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.zerop;

        procedure ll;
        begin
          logRegs;
          log('FETCH');
          log(#9 + 'Fetch ' + ramTo0);
          log('');
          log('EXECUTE');
          log(#9 + ramTo0);
          log('');
        end;

begin
  ll;

  if regs[IP] > (256 - 2) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
     case HexFF AND ramOP0 of
      $00 : Begin                                           { HALT }
              formMain.doHalt;
              exit
            End;

      $CD,                                                  { IRET  }
      $CB : Begin                                           { RET   }
              if (HexFF AND regs[SP] < HexBF) then
              begin
                regAssign(SP, regs[SP] + 1, dontSetFlags);
                regAssign(IP, ram[HexFF AND regs[SP]].opHex, dontSetFlags);
                regs[IP] := regs[IP] AND HexFF;

                log('INSTRUCTION POINTER');
                log(#9 + 'Set IP to the value popped off the stack: ' + IntToHex(regs[IP], 2));
                logRegs;
                log('-------------------------------------------------------------------------------------------');

                if (regs[IP] < 0) OR (regs[IP] > 255) then
                begin
                  error(BeyondRAM, currToken);
                  log(#9 + BeyondRAM);
                  exit
                end;

                exit
              end
              else
              begin
                error('Stack underflow.', currToken);
              end;
            End;

      $EA : Begin                                           { PUSHF }
              ramAssign(regs[SP], regs[SR]);

              if (HexFF AND regs[SP] > 0) then
              begin
                regAssign(SP, regs[SP] - 1, dontSetFlags)
              end
              else
              begin
                error('Stack overflow.', currToken);
              end
            End;

      $EB : Begin                                           { POPF  }
              if (HexFF AND regs[SP] < HexBF) then
              begin
                regAssign(SP, regs[SP] + 1, dontSetFlags);
                regAssign(SR, ram[HexFF AND regs[SP]].opHex, dontSetFlags);
              end
              else
              begin
                error('Stack underflow.', currToken);
              end;
            End;

      $FF : Begin                                           { NOP   }
              { Do nothing }
            End;

      $FC : Begin                                           { STI   }
              regAssign(SR, regs[SR] OR I_I, dontSetFlags)
            End;

      $FD : Begin                                           { CLI   }
              regAssign(SR, regs[SR] AND HexEF, dontSetFlags)
            End;

      $FE : Begin                                           { CLO   }
              formTlight.Close;
              formSevenSeg.Close;
              formStep.Close;
              formMaze.Close;
              formHeat.Close;
              formVdu.Close;
              formLift.Close;
              formKeyb.Close;
              formKeyPad.Close;
            End;
    end;

    regAssign(IP, regs[IP] + 1, dontSetFlags);
    logIP('1');
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.imovRM;           { MOV    BL,[AL]    D3 01 00 }
var reg1, reg2 : smallInt;
begin
  reg1 := ramOP1;
  reg2 := ramOP2;

  if regs[IP] > (256 - 4) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
    if validReg(reg1) then
    begin
      if validReg(reg2) then
      begin
        logRegs;
        log('FETCH');
        log(#9 + 'Fetch ' + ramTo0);
        log(#9 + 'Fetch ' + ramTo1);
        log(#9 + 'Fetch ' + ramTo2);
        log('');
        log('EXECUTE');
        log(#9 + ramTo0 + #9 + ramTo1 + ',' + ramTo2);
        log('');

        regAssign(reg1, ram[HexFF AND regs[reg2]].opHex, dontSetFlags);
        regAssign(IP, regs[IP] + 3, dontSetFlags);

        logIP('3');

      end
      else
      begin
        error(RegNotExi, currToken);
      end
    end
    else
    begin
      error(RegNotExi, currToken);
    end
  end;
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.inOut;                              { OUT   01     F1  01 }
begin
  logRegs;
  log('FETCH');
  log(#9 + 'Fetch ' + ramTo0);
  log(#9 + 'Fetch ' + ramTo1);
  log('');
  log('EXECUTE');
  log(#9 + ramTo0 + #9 + ramTo1);
  log('');

  if regs[IP] > (256 - 3) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
    if ramOP1 in [0..15] then
    begin
      case HexFF AND ramOP0 of
        $F0: begin                                   { IN     }
               case ramOP1 of
                 0: begin
                      formMain.wasContinuous := continuous;
                      continuous := false;
                      formKeybIn.ShowModal
                    end;

                 3: begin
                      if formHeat.thermostatOn then  { IN  03 }
                        regAssign(AL, 1, dontSetFlags)
                      else
                        regAssign(AL, 0, dontSetFlags)
                    end;

                 6: begin
                      regAssign(AL, formLift.getState, dontSetFlags)
                    end;

                 7: begin                            { Keyb Sim }
                      regAssign(AL, FormKeyb.getState, dontSetFlags)
                    end;

                 8: begin                            { KeyPad Sim }
                      regAssign(AL, FormKeyPad.getState, dontSetFlags)
                    end;
               end
             end;


        $F1: begin                                   { OUT    }
               case ramOP1 of
                 0: ;
                 1: begin
                      formMain.myShow(formTLight);
                      formTlight.doLights;           { OUT 01 }
                    end;
                 2: begin
                      formMain.myShow(formSevenSeg);
                      formSevenSeg.sevenseg;         { OUT 02 }
                    end;
                 3: begin
                      formMain.myShow(formHeat);
                      formHeat.hooter;               { OUT 03 }
                    end;
                 4: begin
                      formMain.myShow(formMaze);
                      formMaze.doMaze;               { OUT 04 }
                    end;
                 5: begin
                      formMain.myShow(formStep);
                      formStep.step;                 { OUT 05 }
                    end;
                 6: begin
                      formMain.myShow(formLift);
                      formLift.lift;                 { OUT 06 }
                    end;
                 7: begin
                      formMain.myShow(formKeyb);     { OUT 07 }
                    end;
                 8: begin
                      formMain.myShow(formKeyPad);   { OUT 08 }
                    end;
               end
             end;
      end
    end
    else
    begin
      error('I/O Ports 0..F are available.', currToken)
    end;

    regAssign(IP, regs[IP] + 2, dontSetFlags);
    logIP('2');
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.jmp;             { JMP    Foo      C0 0F }
Var jumpTooFar : Boolean;
    ip_before  : smallint;


        procedure ll;
        begin
          logRegs;
          log('FETCH');
          log(#9 + 'Fetch ' + ramTo0);
          log(#9 + 'Fetch ' + ramTo1);
          log('');
          log('EXECUTE');
          log(#9 + ramTo0 + #9 + ramTo1);
          log('');
        end;

        procedure l2;
        begin
          logIP(IntToStr(regs[IP] - ip_before) + '(decimal)');
        end;

begin
  ip_before := regs[IP];

  ll;

  if ((HexFF AND regs[IP] + ramOP1) < 0) OR
     ((HexFF AND regs[IP] + ramOP1) > 255) then
  begin
    jumpTooFar := true
  end
  else
  begin
    jumpTooFar := false
  end;

  case HexFF AND ramOP0 of
    $C0: if not jumpTooFar then
         begin
           regAssign(IP, regs[IP] + ramOP1, dontSetFlags);   { JMP }
           l2;
         end
         else
         begin
           error('Can not jump beyond end of memory.', currToken);
           log(#9 + 'Can not jump beyond end of memory.');
           log('-------------------------------------------------------------------------------------');
         end;

    $C1: begin
           if regs[SR] AND 2 <> 0 then
           begin
             if not jumpTooFar then
             begin
               regAssign(IP, regs[IP] + ramOP1, dontSetFlags);  { JZ  }
               l2;
             end
             else
             begin
               error('Can not jump beyond end of memory.', currToken);
               log(#9 + 'Can not jump beyond end of memory.');
               log('-------------------------------------------------------------------------------------');
             end
           end
           else
           begin
             regAssign(IP, regs[IP] + 2, dontSetFlags);
             l2;
           end
         end;
    $C2: begin
           if regs[SR] AND 2 = 0 then
           begin
             if not jumpTooFar then
             begin
               regAssign(IP, regs[IP] + ramOP1, dontSetFlags);  { JNZ }
               l2;
             end
             else
             begin
               error('Can not jump beyond end of memory.', currToken);
               log(#9 + 'Can not jump beyond end of memory.');
               log('-------------------------------------------------------------------------------------');
             end
           end
           else
           begin
             regAssign(IP, regs[IP] + 2, dontSetFlags);
             l2;
           end
         end;
    $C3: begin
           if regs[SR] AND 8 <> 0 then
           begin
             if not jumpTooFar then
             begin
               regAssign(IP, regs[IP] + ramOP1, dontSetFlags);  { JS  }
               l2;
             end
             else
             begin
               error('Can not jump beyond end of memory.', currToken);
               log(#9 + 'Can not jump beyond end of memory.');
               log('-------------------------------------------------------------------------------------');
             end
           end
           else
           begin
             regAssign(IP, regs[IP] + 2, dontSetFlags);
             l2;
           end
         end;
    $C4: begin
           if regs[SR] AND 8 = 0 then
           begin
             if not jumpTooFar then
             begin
               regAssign(IP, regs[IP] + ramOP1, dontSetFlags);  { JNS }
               l2;
             end
             else
             begin
               error('Can not jump beyond end of memory.', currToken);
               log(#9 + 'Can not jump beyond end of memory.');
               log('-------------------------------------------------------------------------------------');
             end
           end
           else
           begin
             regAssign(IP, regs[IP] + 2, dontSetFlags);
             l2;
           end
         end;
    $C5: begin
           if regs[SR] AND 4 <> 0 then
           begin
             if not jumpTooFar then
             begin
               regAssign(IP, regs[IP] + ramOP1, dontSetFlags);  { JO  }
               l2;
             end
             else
             begin
               error('Can not jump beyond end of memory.', currToken);
               log(#9 + 'Can not jump beyond end of memory.');
               log('-------------------------------------------------------------------------------------');
             end
           end
           else
           begin
             regAssign(IP, regs[IP] + 2, dontSetFlags);
             l2;
           end
         end;
    $C6: begin
           if regs[SR] AND 4 = 0 then
           begin
             if not jumpTooFar then
             begin
               regAssign(IP, regs[IP] + ramOP1, dontSetFlags);  { JNO }
               l2;
             end
             else
             begin
               error('Can not jump beyond end of memory.', currToken);
               log(#9 + 'Can not jump beyond end of memory.');
               log('-------------------------------------------------------------------------------------');
             end
           end
           else
           begin
             regAssign(IP, regs[IP] + 2, dontSetFlags);
             l2;
           end
         end;
  end
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.movMR;           { MOV    [15],AL    D2 15 00 }
var aReg : smallInt;
begin
  aReg := ramOP2;

  if regs[IP] > (256 - 4) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
    if validReg(aReg) then
    begin
      logRegs;
      log('FETCH');
      log(#9 + 'Fetch ' + ramTo0);
      log(#9 + 'Fetch ' + ramTo1);
      log(#9 + 'Fetch ' + ramTo2);
      log('');
      log('EXECUTE');
      log(#9 + ramTo0 + #9 + ramTo1 + ',' + ramTo2);
      log('');

      ramAssign(ramOP1, regs[aReg]);
      regAssign(IP, regs[IP] + 3, dontSetFlags);

      logIP('3');
    end
    else
    begin
      error(RegNotExi, currToken);
    end
  end;
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.movRM;           { MOV    AL,[15]    D1 00 15 }
var aReg : smallInt;
begin
  aReg := ramOP1;

  if regs[IP] > (256 - 4) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
    if validReg(aReg) then
    begin
      logRegs;
      log('FETCH');
      log(#9 + 'Fetch ' + ramTo0);
      log(#9 + 'Fetch ' + ramTo1);
      log(#9 + 'Fetch ' + ramTo2);
      log('');
      log('EXECUTE');
      log(#9 + ramTo0 + #9 + ramTo1 + ',' + ramTo2);
      log('');

      regAssign(aReg, ram[$FF AND ramOP2].opHex, dontSetFlags);
      regAssign(IP, regs[IP] + 3, dontSetFlags);

      logIP('3');
    end
    else
    begin
      error(RegNotExi, currToken);
    end
  end;
end;

{ ---------------------------------------------------------------------- }

procedure Taaa.movRN;           { MOV    AL,15    D0 00 15 }
var aReg : smallInt;
begin
  aReg := ramOP1;

  if regs[IP] > (256 - 4) then
  begin
    error(BeyondRAM, currToken)
  end
  else
  begin
    if validReg(aReg) then
    begin
      logRegs;
      log('FETCH');
      log(#9 + 'Fetch ' + ramTo0);
      log(#9 + 'Fetch ' + ramTo1);
      log(#9 + 'Fetch ' + ramTo2);
      log('');
      log('EXECUTE');
      log(#9 + ramTo0 + #9 + ramTo1 + ',' + ramTo2 + #9 + '; Copy ' + ramTo2 + ' into ' + ramTo1);
      log('');

      regAssign(aReg, ramOP2, dontSetFlags);
      regAssign(IP, regs[IP] + 3, dontSetFlags);

      logIP('3');
    end
    else
    begin
      error(RegNotExi, currToken);
    end
  end;
end;

{ ---------------------------------------------------------------------- }

end.

