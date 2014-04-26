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
unit Mainform;

interface

uses
  SysUtils, WinTypes, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Menus, TabNotBk, Assem,
  Grids, Buttons, iniFiles, TLight, SevSeg, Stepper, Maze, Heater,
  Vdu_form, keyBin, Printers, WinProcs, lift, ComCtrls, ShellAPI;

type
  IndexNode = class(TObject)
    token     : String;
    selStart  : Integer;
    selLength : Integer;
    fileName  : String;

    constructor create;
  end;

  TrtfPos = class(TObject)
    pos : Integer;
  end;

  TFormMain = class(TForm)
    MenuMain: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileSave: TMenuItem;
    MenuFileSaveAs: TMenuItem;
    MenuFileExit: TMenuItem;
    PanelTop: TPanel;
    LabelAL: TLabel;
    LabelBL: TLabel;
    Bevel1: TBevel;
    LabelCL: TLabel;
    LabelDL: TLabel;
    Bevel2: TBevel;
    LabelIP: TLabel;
    LabelSP: TLabel;
    LabelSR: TLabel;
    LabelSOZ: TLabel;
    LabelCurrInstr: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    menuEdit: TMenuItem;
    MenuEditCopy: TMenuItem;
    MenuEditCut: TMenuItem;
    MenuEditPaste: TMenuItem;
    menuFileNew: TMenuItem;
    MenuFileN1: TMenuItem;
    TimerCPU: TTimer;
    MenuView: TMenuItem;
    MenuViewVDU: TMenuItem;
    MenuViewTrafficLights: TMenuItem;
    MenuViewSevenSegmentDisplays: TMenuItem;
    MenuViewHeaterandThermostat: TMenuItem;
    MenuViewMaze: TMenuItem;
    MenuViewStepperMotor: TMenuItem;
    MenuFileN2: TMenuItem;
    MenuFilePrintSource: TMenuItem;
    MenuFilePrintListFile: TMenuItem;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    MenuFileN3: TMenuItem;
    MenuFilePrintSetup: TMenuItem;
    MenuHelp: TMenuItem;
    MenuHelpAbout: TMenuItem;
    MenuHelpContents: TMenuItem;
    MenuFileN5: TMenuItem;
    TimerStatusClear: TTimer;
    GlyphFileOpen: TSpeedButton;
    GlyphFileSave: TSpeedButton;
    GlyphFileNew: TSpeedButton;
    ButtonAssemble: TButton;
    ButtonStep: TButton;
    ButtonRun: TButton;
    ButtonStop: TButton;
    ButtonContinuous: TButton;
    ButtonReset: TButton;
    ButtonShowRam: TButton;
    ButtonSlow: TButton;
    ButtonFast: TButton;
    GlyphShowVdu: TSpeedButton;
    GlyphShowTilght: TSpeedButton;
    GlyphShowSevSeg: TSpeedButton;
    GlyphShowHeater: TSpeedButton;
    GlyphShowMaze: TSpeedButton;
    GlyphShowStepperMotor: TSpeedButton;
    menuViewRam: TMenuItem;
    ButtonShowRamSource: TButton;
    ButtonShowRamASCII: TButton;
    ButtonShowRamHex: TButton;
    menuViewLift: TMenuItem;
    glyphEdit: TSpeedButton;
    glyphList: TSpeedButton;
    glyphLift: TSpeedButton;
    Examples1: TMenuItem;
    MenuExampleArithmetic: TMenuItem;
    MenuExampleLoops: TMenuItem;
    MenuExampleLift: TMenuItem;
    MenuExampleText: TMenuItem;
    MenuExampleDemonstration: TMenuItem;
    N2: TMenuItem;
    MenuEditSelectAll: TMenuItem;
    Find1: TMenuItem;
    FindDialog1: TFindDialog;
    ReplaceDialog1: TReplaceDialog;
    MenuEditReplace: TMenuItem;
    PageControl1: TPageControl;
    TabSheetSourceCode: TTabSheet;
    TabSheetListFile: TTabSheet;
    TabSheetConfiguration: TTabSheet;
    TabSheetTokens: TTabSheet;
    memoSource: TMemo;
    ListBoxTokens: TListBox;
    PanelConfig: TPanel;
    GroupBoxHWT: TGroupBox;
    ButtonIntervalReduce: TButton;
    ButtonIntervalIncrease: TButton;
    CheckBoxCLO: TCheckBox;
    CheckBoxPrintPause: TCheckBox;
    MemoList: TMemo;
    SpeedButton1: TSpeedButton;
    TabSheetLog: TTabSheet;
    RichEdit1: TRichEdit;
    SpeedButtonKeyb: TSpeedButton;
    SpeedButton2: TSpeedButton;
    CheckBoxRunLog: TCheckBox;
    CheckBoxAsmLog: TCheckBox;
    Keyboard1: TMenuItem;
    KeyPad1: TMenuItem;
    HideAll1: TMenuItem;
    MemoAsmLog: TMemo;
    Splitter1: TSplitter;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    procedure MenuFileExitClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuFileOpenClick(Sender: TObject);
    procedure MenuFileSaveClick(Sender: TObject);
    procedure MenuFileSaveAsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuEditCopyClick(Sender: TObject);
    procedure MenuEditCutClick(Sender: TObject);
    procedure MenuEditPasteClick(Sender: TObject);
    procedure menuFileNewClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonStepClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure TimerCPUTimer(Sender: TObject);
    procedure ButtonSlowClick(Sender: TObject);
    procedure ButtonFastClick(Sender: TObject);
    procedure memoSourceChange(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonAssembleClick(Sender: TObject);
    procedure ButtonContinuousClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure GlyphShowTilghtClick(Sender: TObject);
    procedure GlyphShowSevSegClick(Sender: TObject);
    procedure GlyphShowStepperMotorClick(Sender: TObject);
    procedure GlyphShowMazeClick(Sender: TObject);
    procedure GlyphShowHeaterClick(Sender: TObject);
    procedure GlyphShowVduClick(Sender: TObject);
    procedure MenuViewVDUClick(Sender: TObject);
    procedure MenuViewTrafficLightsClick(Sender: TObject);
    procedure MenuViewSevenSegmentDisplaysClick(Sender: TObject);
    procedure MenuViewHeaterandThermostatClick(Sender: TObject);
    procedure MenuViewMazeClick(Sender: TObject);
    procedure MenuViewStepperMotorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBoxCLOClick(Sender: TObject);
    procedure MenuFilePrintSourceClick(Sender: TObject);
    procedure MenuFilePrintSetupClick(Sender: TObject);
    procedure MenuFilePrintListFileClick(Sender: TObject);
    procedure CheckBoxPrintPauseClick(Sender: TObject);
    procedure MenuHelpContentsClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure TimerStatusClearTimer(Sender: TObject);
    procedure ButtonShowRamClick(Sender: TObject);
    procedure ButtonShowRamHexClick(Sender: TObject);
    procedure ButtonShowRamASCIIClick(Sender: TObject);
    procedure ButtonShowRamSourceClick(Sender: TObject);
    procedure ButtonIntervalReduceClick(Sender: TObject);
    procedure ButtonIntervalIncreaseClick(Sender: TObject);
    procedure menuViewLiftClick(Sender: TObject);
    procedure MenuExampleArithmeticClick(Sender: TObject);
    procedure MenuExampleLoopsClick(Sender: TObject);
    procedure MenuExampleLiftClick(Sender: TObject);
    procedure MenuExampleTextClick(Sender: TObject);
    procedure MenuExampleDemonstrationClick(Sender: TObject);
    procedure MenuEditSelectAllClick(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FindDialog1Show(Sender: TObject);
    procedure FindDialog1Close(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure MenuEditReplaceClick(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure ReplaceDialog1Show(Sender: TObject);
    procedure ReplaceDialog1Close(Sender: TObject);
    procedure glyphEditClick(Sender: TObject);
    procedure glyphListClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButtonKeybClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure TabSheetSourceCodeShow(Sender: TObject);
    procedure TabSheetListFileShow(Sender: TObject);
    procedure TabSheetTokensShow(Sender: TObject);
    procedure TabSheetLogShow(Sender: TObject);
    procedure Keyboard1Click(Sender: TObject);
    procedure KeyPad1Click(Sender: TObject);
    procedure HideAll1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    helpFile                  : String;
    iniFile                   : String;
    appPath                   : String;
    version                   : String;
    shouldPauseBeforePrinting : boolean;
    prevIP                    : smallInt;
    firstTime                 : Boolean;
    myIniFile                 : TIniFile;         { TApplication }
    wasContinuous             : Boolean;
    showOnlyOne               : Boolean;
    statusCountDown           : Integer;
    captureFocus              : Boolean;

    function  openFile(p : boolean) : boolean;
    function  saveFile              : boolean;
    function  saveFileAs            : boolean;
    function  getStatus             : string;

    procedure currInstr;
    procedure doHalt;
    procedure myShow(aForm : Tform);
    procedure run;
    procedure saveIniFile;
    procedure loadIniFile;
    procedure status(aMessage : String; bgColor, textColor : TColor);
    procedure error(msg : string);
  end;

var
  FormMain: TFormMain;

implementation

uses formAbt, ramHex, KeybForm, KeyPadFm;

{$R *.DFM}

constructor IndexNode.create;
begin
  token     := '';
  selStart  := 0;
  selLength := 0;
  fileName  := '';
end;

procedure TFormMain.myShow(aForm : Tform);
begin
  if showOnlyOne then
  begin
    if aForm <> formTlight   then formTlight.Close;
    if aForm <> formSevenSeg then formSevenSeg.Close;
    if aForm <> formStep     then formStep.Close;
    if aForm <> formMaze     then formMaze.Close;
    if aForm <> formHeat     then formHeat.Close;
    if aForm <> formVdu      then formVdu.Close;
    if aForm <> formLift     then formLift.Close;
    if aForm <> formKeyb     then formKeyb.Close;
    if aForm <> FormKeyPad   then FormKeyPad.Close;
  end;

  if aForm <> Nil then
  begin
    if not aForm.visible then
    begin
      aForm.show;
    end
  end
end;


function TFormMain.getStatus : string;
begin
  result := labelCurrInstr.Caption
end;

procedure TFormMain.status(aMessage : String; bgColor, textColor : TColor);
Var s : String;
begin
  statusCountDown := 30;

  labelCurrInstr.Color := bgColor;
  labelCurrInstr.Font.Color := textColor;

  if length(aMessage) > 76 then
  begin
    s := copy(aMessage, 1, 37) + ' ... ';
    s := s + copy(aMessage, length(aMessage) - 33, 34);
  end
  else
  begin
    s := aMessage
  end;

  labelCurrInstr.Caption := ' ' + s;
  labelCurrInstr.update;
end;

function TFormMain.openFile(p : boolean) : boolean;
begin
  if p then
  begin
    if openDialog1.execute then
    begin
      memoSource.lines.loadFromFile(openDialog1.fileName);
      saveDialog1.FileName := openDialog1.FileName;
      memoSource.modified := false;
      aaa.needToAssemble := True;
      caption := saveDialog1.FileName;
      result := true
    end
    else
    begin
      result := false
    end
  end
  else
  begin
    if fileExists(openDialog1.fileName) then
    begin
      memoSource.lines.loadFromFile(openDialog1.fileName);
      saveDialog1.FileName := openDialog1.FileName;
      memoSource.modified := false;
      aaa.needToAssemble := True;
      caption := saveDialog1.FileName;
      result := true
    end
    else
    begin
      result := false
    end
  end
end;

function TFormMain.saveFile : Boolean;
begin
  if saveDialog1.FileName = 'Untitled' then
  begin
    result := saveFileAs
  end
  else
  begin
    memoSource.lines.saveToFile(saveDialog1.FileName);
    memoSource.modified := false;
    status('Saved ' + ExtractFileName(saveDialog1.FileName), clGreen, clYellow);
    result := true
  end
end;

function TFormMain.saveFileAs : Boolean;
begin
  if saveDialog1.execute then
  begin
    memoSource.lines.saveToFile(saveDialog1.FileName);
    openDialog1.FileName := saveDialog1.FileName;
    memoSource.modified := false;
    caption := saveDialog1.FileName;
    status('Saved ' + saveDialog1.FileName, clGreen, clYellow);
    result := true
  end
  else
  begin
    result := false
  end
end;

procedure TFormMain.run;
begin
  if (aaa.tokListhead <> Nil) AND (aaa.ok) then
  begin
    memoSource.readOnly := true;
    memoSource.color    := clBtnFace;
    aaa.continuous      := true
  end
end;

procedure TFormMain.doHalt;
begin
  aaa.continuous          := false;
  wasContinuous           := False;
  PageControl1.ActivePage := TabSheetSourceCode;
  memoSource.readOnly     := false;
  memoSource.modified     := false;
  memoSource.color        := clWindow;
  memoSource.SetFocus;
  memoSource.SelLength    := 0;
end;

procedure TFormMain.MenuFileExitClick(Sender: TObject);
begin
  Close
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  labelCurrInstr.Width := panelTop.Width - 2;
end;

procedure TFormMain.MenuFileOpenClick(Sender: TObject);
Var r : Word;
begin
  doHalt;
  PageControl1.ActivePage := TabSheetSourceCode;

  if memoSource.modified then
  begin
    r := messageDlg('Save source code now?', mtWarning,
                    [mbYes, mbNo, mbCancel], 0);

    if r = mrYes then
    begin
      if saveFile then
      begin
        openFile(true);
      end
    end
    else if r = mrCancel then
    begin
      { Do nothing }
    end
    else if r = mrNo then
    begin
      openFile(true);
    end
  end
  else
  begin
    openFile(true);
  end
end;

procedure TFormMain.MenuFileSaveClick(Sender: TObject);
begin
  doHalt;
  saveFile
end;

procedure TFormMain.MenuFileSaveAsClick(Sender: TObject);
begin
  saveFileAs
end;

procedure TFormMain.FormCreate(Sender: TObject);
        function extractVersion(const s : String) : String;
        var answer : String;
            i      : Integer;
        begin
          for i := length(s) downto 1 do
          begin
            if (i > 2) AND (s[i] = '.') then
            begin
              answer := s[i - 2] + '.' + s[i - 1];

              break
            end
            else
            begin
              answer := ' - could not generate version information.'
            end
          end;

          extractVersion := answer
        end;
begin
  captureFocus         := True;
  firstTime            := true;
  prevIP               := 0;
  showOnlyOne          := False;
  appPath              := extractFilePath(application.ExeName);
  helpFile             := changeFileExt(Application.ExeName, '.chm');
  iniFile              := changeFileExt(extractFileName(Application.ExeName), '.INI');
  version              := extractVersion(Application.ExeName);
//application.helpFile := helpFile
end;

procedure TFormMain.MenuEditCopyClick(Sender: TObject);
begin
  if memoSource.focused then
  begin
    memoSource.copyToClipBoard
  end
  else if memoList.focused then
  begin
    memoList.copyToClipBoard
  end
end;

procedure TFormMain.MenuEditCutClick(Sender: TObject);
begin
  if memoSource.focused then
  begin
    memoSource.cutToClipBoard
  end
  else if memoList.focused then
  begin
    memoList.cutToClipBoard
  end
end;

procedure TFormMain.MenuEditPasteClick(Sender: TObject);
begin
  if memoSource.focused then
  begin
    memoSource.pasteFromClipBoard
  end
end;

procedure TFormMain.menuFileNewClick(Sender: TObject);
Var r : Word;
begin
  doHalt;
  aaa.freeAllTokNodes;

  PageControl1.ActivePage := TabSheetSourceCode;
  memoSource.readOnly     := false;
  memoSource.color        := clWindow;
  memoSource.SetFocus;
  memoSource.SelLength    := 0;

  if memoSource.modified then
  begin
    r := messageDlg('Save source code now?', mtWarning,
                    [mbYes, mbNo, mbCancel], 0);
    if r = mrYes then
    begin
      if not saveFile then
      begin
        exit
      end
    end
    else if r = mrCancel then
    begin
      exit
    end
  end;

  memoSource.Clear;
  aaa.needToAssemble := True;
  caption := 'Untitled';
  saveDialog1.FileName := 'Untitled';
  FormRamHex.Rescale
end;

procedure TFormMain.FormActivate(Sender: TObject);
Var i     : Integer;
    s     : string;
begin
  if firstTime then
  begin
    loadIniFile;

    firstTime := false;

    if paramCount >= 1 then
    begin
      s := '';

      for i := 1 to paramcount - 1 do
      begin
        s := s + paramstr(i) + ' ';
      end;

      s := s + paramstr(paramcount);

      if fileExists(s) then
      begin
        memoSource.lines.loadFromFile(s);
        saveDialog1.FileName := s;
        openDialog1.FileName := s;
        memoSource.modified := false;
        aaa.needToAssemble := True;
        caption := saveDialog1.FileName
      end
      else
      begin
        messageDlg(paramStr(1) + ' : File not found.', mtError, [mbOK], 0)
      end
    end
    else
    begin
      caption := 'Untitled';
      saveDialog1.filename := 'Untitled';
      openDialog1.filename := 'Untitled';
    end
  end;

  PageControl1.ActivePage := TabSheetSourceCode;
  memoSource.SetFocus 
end;
(*
procedure TFormMain.context(s : string);
begin
  if (compareText(s, 'AL') = 0) OR
     (compareText(s, 'BL') = 0) OR
     (compareText(s, 'CL') = 0) OR
     (compareText(s, 'DL') = 0) OR
     (compareText(s, 'CPU') = 0) Then
  begin
    Application.HelpJump('CPU')
  end
  else if (compareText(s, ';') = 0) then
  begin
    Application.HelpJump('Semicolon')
  end
  else if (compareText(s, '[') = 0) OR
          (compareText(s, ']') = 0) OR
          (compareText(s, 'RAM') = 0) then
  begin
    Application.HelpJump('RAMADDRESSES')
  end
  else if (compareText(s, 'ADD') = 0) then
  begin
    Application.HelpJump('ADD')
  end
  else if (compareText(s, 'AND') = 0) then
  begin
    Application.HelpJump('AND')
  end
  else if (compareText(s, 'CALL') = 0) OR
          (compareText(s, 'RET') = 0)  then
  begin
    Application.HelpJump('CALL')
  end
  else if (compareText(s, 'CLI') = 0) OR
          (compareText(s, 'STI') = 0) then
  begin
    Application.HelpJump('CLI')
  end
  else if (compareText(s, 'CLO') = 0) then
  begin
    Application.HelpJump('CLO')
  end
  else if (compareText(s, 'CMP') = 0) then
  begin
    Application.HelpJump('CMP')
  end
  else if (compareText(s, 'DB') = 0) then
  begin
    Application.HelpJump('DB')
  end
  else if (compareText(s, 'DEC') = 0) OR (compareText(s, 'INC') = 0) then
  begin
    Application.HelpJump('INC')
  end
  else if (compareText(s, 'DIV') = 0) OR
          (compareText(s, 'MOD') = 0) then
  begin
    Application.HelpJump('DIV')
  end
  else if (compareText(s, 'END') = 0) then
  begin
    Application.HelpJump('END')
  end
  else if (compareText(s, 'HALT') = 0) then
  begin
    Application.HelpJump('HALT')
  end
  else if (compareText(s, 'IN') = 0) OR
          (compareText(s, 'OUT') = 0) then
  begin
    Application.HelpJump('IN')
  end
  else if (compareText(s, 'INT') = 0) OR
          (compareText(s, 'IRET') = 0) then
  begin
    Application.HelpJump('INT')
  end
  else if (compareText(s, 'JMP') = 0) then
  begin
    Application.HelpJump('JMP')
  end
  else if (compareText(s, 'JNO') = 0) then
  begin
    Application.HelpJump('JNO')
  end
  else if (compareText(s, 'JNS') = 0) then
  begin
    Application.HelpJump('JNS')
  end
  else if (compareText(s, 'JNZ') = 0) then
  begin
    Application.HelpJump('JNZ')
  end
  else if (compareText(s, 'JO') = 0) then
  begin
    Application.HelpJump('JO')
  end
  else if (compareText(s, 'JS') = 0) then
  begin
    Application.HelpJump('JS')
  end
  else if (compareText(s, 'JZ') = 0) then
  begin
    Application.HelpJump('JZ')
  end
  else if (compareText(s, 'MOV') = 0) then
  begin
    Application.HelpJump('MOV')
  end
  else if (compareText(s, 'MUL') = 0) then
  begin
    Application.HelpJump('MUL')
  end
  else if (compareText(s, 'NOP') = 0) then
  begin
    Application.HelpJump('NOP')
  end
  else if (compareText(s, 'NOT') = 0) then
  begin
    Application.HelpJump('NOT')
  end
  else if (compareText(s, 'OR') = 0) then
  begin
    Application.HelpJump('OR')
  end
  else if (compareText(s, 'ORG') = 0) then
  begin
    Application.HelpJump('ORG')
  end
  else if (compareText(s, 'POP') = 0) OR
          (compareText(s, 'PUSH') = 0) then
  begin
    Application.HelpJump('PUSH')
  end
  else if (compareText(s, 'POPF') = 0) OR
          (compareText(s, 'PUSHF') = 0) then
  begin
    Application.HelpJump('PUSHF')
  end
  else if (compareText(s, 'ROL') = 0) OR
          (compareText(s, 'ROR') = 0) then
  begin
    Application.HelpJump('ROL')
  end
  else if (compareText(s, 'SHL') = 0) OR
          (compareText(s, 'SHR') = 0) then
  begin
    Application.HelpJump('SHL')
  end
  else if (compareText(s, 'SUB') = 0) then
  begin
    Application.HelpJump('SUB')
  end
  else if (compareText(s, 'XOR') = 0) then
  begin
    Application.HelpJump('XOR')
  end
  else
  begin
    Application.HelpJump('Default')
  end;
end;
*)
procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F9 :
    begin
      ButtonRunClick(Self)
    end;

    VK_ESCAPE :
    begin
      doHalt { continuous run }
    end
  end
end;

procedure TFormMain.ButtonStepClick(Sender: TObject);
begin
  if aaa.needToAssemble then
  begin
    ButtonAssembleClick(Self)
  end;

  if aaa.tokListHead = nil then exit;

  memoSource.readOnly := True;
  aaa.regsToGray;
  memoSource.color := clBtnFace;

  if not formRamHex.visible then
  begin
    myShow(formRamHex)
  end;

  if not aaa.ok then exit;

  case $FF AND aaa.ram[$FF AND aaa.regs[IP]].opHex of
    $A0,                { ADD   }
    $A1,                { SUB   }
    $A2,                { MUL   }
    $A3,                { DIV   }
    $A6,                { MOD   }
    $AA,                { AND   }
    $AB,                { OR    }
    $AC,                { XOR   }

    $B0,                { ADD   }
    $B1,                { SUB   }
    $B2,                { MUL   }
    $B3,                { DIV   }
    $B6,                { MOD   }
    $BA,                { AND   }
    $BB,                { OR    }
    $BC : aaa.arith2;   { XOR   }

    $CA,                { CALL  }
    $CC : aaa.monop;     { INT   }

    $E0,                { PUSH  }
    $E1,                { POP   }
    $9A,                { ROL   }
    $9B,                { ROR   }
    $9C,                { SHL   }
    $9D,                { SHR   }
    $A4,                { INC   }
    $A5,                { DEC   }
    $AD : aaa.monop;    { NOT   }

    $D0 : aaa.movRN;    { MOV  AL,55   }
    $D1 : aaa.movRM;    { MOV  AL,[55] }
    $D2 : aaa.movMR;    { MOV  [55],AL }
    $D3 : aaa.imovRM;   { MOV  AL,[BL] }
    $D4 : aaa.imovMR;   { MOV  [AL],BL }

    $DA,                { CMP  AL,BL   }
    $DB,                { CMP  BL,13   }
    $DC : aaa.cmp;      { CMP  CL,[20] }

    $C0,                { JMP   }
    $C1,                { JZ    }
    $C2,                { JNZ   }
    $C3,                { JS    }
    $C4,                { JNS   }
    $C5,                { JO    }
    $C6 : aaa.jmp;      { JNO   }

    $FC,                { STI   }
    $FD,                { CLI   }
    $CB,                { RET   }
    $CD : aaa.zerop;    { IRET  }

    $00,                { HALT  }
    $EA,                { PUSHF }
    $EB,                { POPF  }
    $FE,                { CLO   }
    $FF : aaa.zerop;    { NOP   }

    $F0,                { IN    }
    $F1 : aaa.inOut;    { OUT   }
  else
    aaa.error('Illegal op code not recognised by CPU.', aaa.currToken)
  end;

  currInstr;
  if formVdu.visible then formVdu.drawForm;
end;

procedure TFormMain.currInstr;
Var reAssemble, wasModified : boolean;
begin
  reAssemble  := aaa.needToAssemble;
  wasModified := memoSource.modified;

  if aaa.ram[$FF AND aaa.regs[IP]] <> Nil then
  begin
    if aaa.ram[$FF AND aaa.regs[IP]].selLength > 0 then
    begin
      memoSource.selStart  := aaa.ram[$FF AND aaa.regs[IP]].selStart;
      memoSource.selLength := aaa.ram[$FF AND aaa.regs[IP]].selLength;
//    SendMessage(MemoSource.Handle, EM_ScrollCaret, 0, 0);
      if (aaa.ram[$FF AND aaa.regs[IP]].opHex = 0) AND
         (aaa.regs[IP] = prevIP) then
      begin
        if aaa.ok then status('END  Program has halted.', clYellow, clRed)
      end
      else
      begin
        if aaa.ok then status(memoSource.selText, clBtnFace, clBlack);
      end;
      prevIP := aaa.regs[IP]
    end
  end;

  aaa.needToAssemble := reAssemble;
  memoSource.modified := wasModified
end;

procedure TFormMain.ButtonResetClick(Sender: TObject);
begin
  doHalt;
  aaa.CPUReset;
  currInstr;
  if formVdu.visible then formVdu.drawForm;
end;

procedure TFormMain.ButtonRunClick(Sender: TObject);
begin
  aaa.CPUReset;

  richEdit1.Clear;
  
  memoSource.readOnly := True;
  memoSource.color := clBtnFace;

  if aaa.needToAssemble then
  begin
    ButtonAssembleClick(Self)
  end;

  run; { continuous mode }
end;

procedure TFormMain.TimerCPUTimer(Sender: TObject);
begin
  if aaa.continuous then
  begin
    ButtonStepClick(Self)
  end;
end;

procedure TFormMain.ButtonSlowClick(Sender: TObject);
begin
  timerCPU.interval := timerCPU.interval * 2;
  if timerCPU.interval > 4096 then
  begin
    timerCPU.interval := 4096;
  end;
end;

procedure TFormMain.ButtonFastClick(Sender: TObject);
begin
  timerCPU.interval := timerCPU.interval div 2;
  
  if timerCPU.interval < 2 then
  begin
    timerCPU.interval := 2
  end;
end;

procedure TFormMain.memoSourceChange(Sender: TObject);
begin
  aaa.needToAssemble := true
end;

procedure TFormMain.ButtonStopClick(Sender: TObject);
begin
  doHalt;
end;

procedure TFormMain.ButtonAssembleClick(Sender: TObject);
begin
  MemoAsmLog.Clear;
  formRamHex.Close;
  doHalt;
  saveFile;
  aaa.freeAllTokNodes;
  aaa.CPUReset;
  PageControl1.ActivePage := TabSheetSourceCode;
  memoSource.readOnly := True;

  if memoSource.lines.count = 0 then
  begin
    status('Nothing to assemble.', clRed, clYellow)
  end
  else
  begin
    if CheckBoxAsmLog.Checked then
    begin
      PageControl1.ActivePage := TabSheetTokens;
    end;

    status(aaa.trim(getStatus) + '  Make tokens.', clGreen, clYellow);
    aaa.tokeniseSource;

    status(aaa.trim(getStatus) + '  Parse tokens.', clGreen, clYellow);
    aaa.parse;

    if aaa.ok then
    begin
      status(aaa.trim(getStatus) + '  Calculate jumps.', clGreen, clYellow);
      aaa.calcJumps;
    end;

    if aaa.ok then
    begin
      status(aaa.trim(getStatus) + '  Success.', clGreen, clYellow);

      if aaa.ram[$FF AND aaa.regs[IP]] <> Nil then
      begin
        PageControl1.ActivePage := TabSheetSourceCode;
        memoSource.selStart     := aaa.ram[$FF AND aaa.regs[IP]].selStart;
        memoSource.selLength    := aaa.ram[$FF AND aaa.regs[IP]].selLength;
      end;

      aaa.needToAssemble := False
    end
  end;

  memoSource.readOnly := False;
  memoSource.modified := false;
  if formVdu.visible then formVdu.drawForm;
  formRamHex.Show;
end;

procedure TFormMain.ButtonContinuousClick(Sender: TObject);
begin
  memoSource.readOnly := True;
  memoSource.color := clBtnFace;

  if aaa.needToAssemble then
  begin
    ButtonAssembleClick(Self)
  end;

  run;      { continuous mode }
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
Var r     : Word;
begin
  saveIniFile;

  if CheckBoxAsmLog.Checked then
  begin
    canClose := false;
    CheckBoxAsmLog.Checked := False;

    messageDlg('Can not quit while assembler logging is on. Assembler logging has been turned off. You can now try to quit again.', mtWarning, [mbOK], 0);
  end
  else
  if memoSource.modified then
  begin
    r := messageDlg('Save source code before quitting?',
                    mtWarning, [mbYes, mbNo, mbCancel], 0);
    if r = mrYes then
    begin
      if saveFile then
      begin
        canClose := true
      end
      else
      begin
        canClose := false
      end
    end
    else if r = mrNo then
    begin
      canClose := true
    end
    else if r = mrCancel then
    begin
      canClose := false
    end
  end
end;

procedure TFormMain.GlyphShowTilghtClick(Sender: TObject);
begin
  myShow(formTlight)
end;

procedure TFormMain.GlyphShowSevSegClick(Sender: TObject);
begin
  myShow(formSevenSeg)
end;

procedure TFormMain.GlyphShowStepperMotorClick(Sender: TObject);
begin
  myShow(formStep)
end;

procedure TFormMain.GlyphShowMazeClick(Sender: TObject);
begin
  myShow(formMaze)
end;

procedure TFormMain.GlyphShowHeaterClick(Sender: TObject);
begin
  myShow(formHeat)
end;

procedure TFormMain.GlyphShowVduClick(Sender: TObject);
begin
  myShow(formVdu)
end;

procedure TFormMain.MenuViewVDUClick(Sender: TObject);
begin
  myShow(formVdu)
end;

procedure TFormMain.MenuViewTrafficLightsClick(Sender: TObject);
begin
  myShow(formTlight)
end;

procedure TFormMain.MenuViewSevenSegmentDisplaysClick(Sender: TObject);
begin
  myShow(formSevenSeg)
end;

procedure TFormMain.MenuViewHeaterandThermostatClick(Sender: TObject);
begin
  myShow(formHeat)
end;

procedure TFormMain.MenuViewMazeClick(Sender: TObject);
begin
  myShow(formMaze)
end;

procedure TFormMain.MenuViewStepperMotorClick(Sender: TObject);
begin
  myShow(formStep)
end;

procedure TFormMain.loadIniFile;
begin
  myIniFile   := TIniFile.Create('.\' + changeFileExt(extractFileName(Application.ExeName), '.INI'));

  Top    := myIniFile.ReadInteger('MainWindow', 'Top', 0);
  Left   := myIniFile.ReadInteger('MainWindow', 'Left', 0);
  Width  := myIniFile.ReadInteger('MainWindow', 'Width', 800);
  Height := myIniFile.ReadInteger('MainWindow', 'Height', 600);

  formKeybIn.Top    := myIniFile.ReadInteger('KeybWindow', 'Top', 30);
  formKeybIn.Left   := myIniFile.ReadInteger('KeybWindow', 'Left', 800 - 436);
  formKeybIn.Width  := myIniFile.ReadInteger('KeybWindow', 'Width', 436);
  formKeybIn.Height := myIniFile.ReadInteger('KeybWindow', 'Height', 72);

  formRamHex.Top    := myIniFile.ReadInteger('RamHexWindow', 'Top', 266);
  formRamHex.Left   := myIniFile.ReadInteger('RamHexWindow', 'Left', 300);
  formRamHex.Width  := myIniFile.ReadInteger('RamHexWindow', 'Width', 500);
  formRamHex.Height := myIniFile.ReadInteger('RamHexWindow', 'Height', 334);

  formVdu.Top    := myIniFile.ReadInteger('Vdu', 'Top',    300 - 95);
  formVdu.Left   := myIniFile.ReadInteger('Vdu', 'Left',   800 - 200);
  formVdu.Width  := myIniFile.ReadInteger('Vdu', 'Width',  200);
  formVdu.Height := myIniFile.ReadInteger('Vdu', 'Height', 95);

  formTlight.Top    := myIniFile.ReadInteger('TLight', 'Top',  30);
  formTlight.Left   := myIniFile.ReadInteger('TLight', 'Left', 800 - formTlight.width);

  formSevenSeg.Top  := myIniFile.ReadInteger('SevSeg', 'Top',  30);
  formSevenSeg.Left := myIniFile.ReadInteger('SevSeg', 'Left', 800 - formSevenSeg.width);

  formStep.Top  := myIniFile.ReadInteger('Stepper', 'Top',  30);
  formStep.Left := myIniFile.ReadInteger('Stepper', 'Left', 800 - formStep.width);

  formMaze.Top  := myIniFile.ReadInteger('Maze', 'Top',  0);
  formMaze.Left := myIniFile.ReadInteger('Maze', 'Left', 775 - formMaze.width);

  formHeat.Top  := myIniFile.ReadInteger('Heat', 'Top',  30);
  formHeat.Left := myIniFile.ReadInteger('Heat', 'Left', 800 - formHeat.width);

  formLift.Top  := myIniFile.ReadInteger('Lift', 'Top',  30);
  formLift.Left := myIniFile.ReadInteger('Lift', 'Left', 800 - formLift.width);

  FormKeyb.Top  := myIniFile.ReadInteger('Keyb', 'Top',  600 - FormKeyb.height);
  FormKeyb.Left := myIniFile.ReadInteger('Keyb', 'Left', 800 - FormKeyb.width);

  FormKeyPad.Top  := myIniFile.ReadInteger('KeyPad', 'Top',  30 );
  FormKeyPad.Left := myIniFile.ReadInteger('KeyPad', 'Left', 800 - FormKeyPad.width);

  timerCPU.interval := myIniFile.ReadInteger('CPU_Clock', 'Interval', 1024);

  showOnlyOne := myIniFile.ReadBool('OneWindowAtATime', 'Show', False);
  checkBoxClo.Checked := showOnlyOne;

  shouldPauseBeforePrinting := myIniFile.ReadBool('PrintPause', 'Pause', False);
  CheckBoxPrintPause.Checked := shouldPauseBeforePrinting;

  aaa.timerHW.interval := myIniFile.ReadInteger('HW_Timer', 'Interval', 5000);

  GroupBoxHWT.Caption := 'Hardware Timer Interval = ' +
    intToStr(aaa.timerHW.interval div 1000) +
    ' Seconds';

  myIniFile.Free
end;

procedure TFormMain.saveIniFile;
                procedure UpdateIniFile;
                begin
                  myIniFile   := TIniFile.Create('.\' + changeFileExt(extractFileName(Application.ExeName), '.INI'));

                  myIniFile.WriteInteger('MainWindow', 'Top',    Top);
                  myIniFile.WriteInteger('MainWindow', 'Left',   Left);
                  myIniFile.WriteInteger('MainWindow', 'Width',  Width);
                  myIniFile.WriteInteger('MainWindow', 'Height', Height);

                  myIniFile.WriteInteger('KeybWindow', 'Top',    formKeybIn.Top);
                  myIniFile.WriteInteger('KeybWindow', 'Left',   formKeybIn.Left);
                  myIniFile.WriteInteger('KeybWindow', 'Width',  formKeybIn.Width);
                  myIniFile.WriteInteger('KeybWindow', 'Height', formKeybIn.Height);

                  myIniFile.WriteInteger('RamHexWindow', 'Top',    formRamHex.Top);
                  myIniFile.WriteInteger('RamHexWindow', 'Left',   formRamHex.Left);
                  myIniFile.WriteInteger('RamHexWindow', 'Width',  formRamHex.Width);
                  myIniFile.WriteInteger('RamHexWindow', 'Height', formRamHex.Height);

                  myIniFile.WriteInteger('Vdu', 'Top',    formVdu.Top);
                  myIniFile.WriteInteger('Vdu', 'Left',   formVdu.Left);
                  myIniFile.WriteInteger('Vdu', 'Width',  formVdu.Width);
                  myIniFile.WriteInteger('Vdu', 'Height', formVdu.Height);

                  myIniFile.WriteInteger('TLight', 'Top',  formTlight.Top);
                  myIniFile.WriteInteger('TLight', 'Left', formTlight.Left);

                  myIniFile.WriteInteger('SevSeg', 'Top',  formSevenSeg.Top);
                  myIniFile.WriteInteger('SevSeg', 'Left', formSevenSeg.Left);

                  myIniFile.WriteInteger('Stepper', 'Top',  formStep.Top);
                  myIniFile.WriteInteger('Stepper', 'Left', formStep.Left);

                  myIniFile.WriteInteger('Maze', 'Top',  formMaze.Top);
                  myIniFile.WriteInteger('Maze', 'Left', formMaze.Left);

                  myIniFile.WriteInteger('Heat', 'Top',  formHeat.Top);
                  myIniFile.WriteInteger('Heat', 'Left', formHeat.Left);

                  myIniFile.WriteInteger('Lift', 'Top',  formLift.Top);
                  myIniFile.WriteInteger('Lift', 'Left', formLift.Left);

                  myIniFile.WriteInteger('Keyb', 'Top',  FormKeyb.Top);
                  myIniFile.WriteInteger('Keyb', 'Left', FormKeyb.Left);

                  myIniFile.WriteInteger('KeyPad', 'Top',  FormKeyPad.Top);
                  myIniFile.WriteInteger('KeyPad', 'Left', FormKeyPad.Left);

                  myIniFile.WriteInteger('CPU_Clock', 'Interval', timerCPU.interval);
                  myIniFile.WriteInteger('HW_Timer', 'Interval', aaa.timerHW.interval);

                  myIniFile.WriteBool('OneWindowAtATime', 'Show', showOnlyOne);

                  myIniFile.WriteBool('PrintPause', 'Pause', shouldPauseBeforePrinting);

                  myIniFile.free
                end;
begin
{$WARNINGS OFF}
  if fileExists(iniFile) AND((FileGetAttr(iniFile) AND faReadOnly) <> 0) then
{$WARNINGS ON}
  begin
    messageDlg(iniFile + ' could not be updated.  It may be read only.',
               mtError, [mbOK], 0)
  end
  else
  begin
    try
      updateIniFile
    except
      messageDlg(iniFile + ' could not be updated.  It may be read only.',
                 mtError, [mbOK], 0)
    end
  end
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  aaa.freeAllTokNodes;           { Belt and braces! }
end;

procedure TFormMain.CheckBoxCLOClick(Sender: TObject);
begin
  if CheckBoxCLO.Checked then
  begin
    showOnlyOne := True
  end
  else
  begin
    showOnlyOne := False
  end
end;

procedure TFormMain.MenuFilePrintSourceClick(Sender: TObject);
var page   : Integer;
                Procedure Foo(StartAt : Integer);
                Var i : Integer;
                    f : textFile;
                begin
                  AssignPrn(f);
                  Rewrite(f);

                  printer.canvas.font := memoSource.font;

                  Writeln(f, 'Page ' + intToStr(startAt + 1) + '    ' + caption);
                  Writeln(f, '');
                  Writeln(f, '');
                  Writeln(f, '');

                  startAt := StartAt * 50;

                  for i := StartAt to StartAt + 49 do
                  begin
                    if i = memoSource.Lines.Count then break;

                    Writeln(f, memoSource.Lines[i]);
                  end;

                  System.CloseFile(f)
                end;
begin
  if memoSource.Lines.Count = 0 then
  begin
    messageDlg('There is nothing to print.', mtInformation, [mbOK], 0)
  end
  else if PrintDialog1.Execute then
  begin
    for page := 0 to memoSource.Lines.Count div 50 do
    begin
      if shouldPauseBeforePrinting then
      begin
        if messageDlg('Print next page?', mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
        begin
          break
        end
      end;

      foo(page)
    end
  end
end;

procedure TFormMain.MenuFilePrintSetupClick(Sender: TObject);
begin
  PrinterSetupDialog1.Execute
end;

procedure TFormMain.MenuFilePrintListFileClick(Sender: TObject);
var page   : Integer;
                Procedure Foo(StartAt : Integer);
                Var i : Integer;
                    f : textFile;
                begin
                  AssignPrn(f);
                  Rewrite(f);

                  printer.canvas.font := memoList.font;

                  Writeln(f, 'Page ' + intToStr(startAt + 1) + '    ' + caption);
                  Writeln(f, '');
                  Writeln(f, '');
                  Writeln(f, '');

                  startAt := StartAt * 50;

                  for i := StartAt to StartAt + 49 do
                  begin
                    if i = memoList.Lines.Count then break;

                    Writeln(f, memoList.Lines[i]);
                  end;

                  System.CloseFile(f)
                end;
begin
  if memoList.Lines.Count = 0 then
  begin
    messageDlg('There is nothing to print.', mtInformation, [mbOK], 0)
  end
  else if PrintDialog1.Execute then
  begin
    for page := 0 to memoList.Lines.Count div 50 do
    begin
      if shouldPauseBeforePrinting then
      begin
        if messageDlg('Print next page?', mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
        begin
          break
        end
      end;

      foo(page)
    end
  end
end;

procedure TFormMain.CheckBoxPrintPauseClick(Sender: TObject);
begin
  if CheckBoxPrintPause.Checked then
  begin
    shouldPauseBeforePrinting := true
  end
  else
  begin
    shouldPauseBeforePrinting := false
  end
end;

procedure TFormMain.MenuHelpContentsClick(Sender: TObject);
var p : PChar;
begin
  p := StrNew(PChar(helpFile));

  shellExecute(handle,
               Nil,
               p,
               Nil,
               Nil,
               SW_SHOWMAXIMIZED);

  StrDispose(p);
end;

procedure TFormMain.MenuHelpAboutClick(Sender: TObject);
begin
  formAboutBox.ShowModal
end;

procedure TFormMain.TimerStatusClearTimer(Sender: TObject);
begin
  if statusCountDown > 0 then
  begin
    statusCountDown := statusCountDown - 1;

    if statusCountDown = 0 then
    begin
      status('', clBtnFace, clBlack)
    end
  end;

  if (CaptureFocus) AND (not FormMain.Focused) then
  begin
    FormMain.SetFocus;
  end
end;

procedure TFormMain.ButtonShowRamClick(Sender: TObject);
begin
  myShow(formRamHex);
end;

procedure TFormMain.ButtonShowRamHexClick(Sender: TObject);
begin
  formRamHex.radioButtonHex.checked := true;

  formRamHex.reScale;
end;

procedure TFormMain.ButtonShowRamASCIIClick(Sender: TObject);
begin
  formRamHex.radioButtonASCII.checked := true;

  formRamHex.reScale;
end;

procedure TFormMain.ButtonShowRamSourceClick(Sender: TObject);
begin
  formRamHex.radioButtonSource.checked := true;

  formRamHex.reScale;
end;

procedure TFormMain.ButtonIntervalReduceClick(Sender: TObject);
begin
  if aaa.timerHW.interval >= 2000 then
  begin
    aaa.timerHW.interval := aaa.timerHW.interval - 1000
  end;

  GroupBoxHWT.caption :=
    'Hardware Timer Interval = ' +
    intToStr(aaa.timerHW.interval div 1000) +
    ' Seconds'
end;

procedure TFormMain.ButtonIntervalIncreaseClick(Sender: TObject);
begin
  if aaa.timerHW.interval <= 9000 then
  begin
    aaa.timerHW.interval := aaa.timerHW.interval + 1000
  end;

  GroupBoxHWT.caption :=
    'Hardware Timer Interval = ' +
    intToStr(aaa.timerHW.interval div 1000) +
    ' Seconds'
end;

procedure TFormMain.menuViewLiftClick(Sender: TObject);
begin
  myShow(formLift)
end;

procedure TFormMain.MenuExampleArithmeticClick(Sender: TObject);
Var r : Word;
begin
  doHalt;

  if memoSource.modified then
  begin
    r := messageDlg('Save source code now?', mtWarning,
                    [mbYes, mbNo, mbCancel], 0);

    if r = mrYes then
    begin
      if saveFile then
      begin
        openDialog1.FileName := 'arithmetic.asm';
        openFile(false);
      end
    end
    else if r = mrCancel then
    begin
      { Do nothing }
    end
    else if r = mrNo then
    begin
      openDialog1.FileName := 'arithmetic.asm';
      openFile(false);
    end
  end
  else
  begin
    openDialog1.FileName := 'arithmetic.asm';
    openFile(false);
  end
end;

procedure TFormMain.MenuExampleLoopsClick(Sender: TObject);
Var r : Word;
begin
  doHalt;

  if memoSource.modified then
  begin
    r := messageDlg('Save source code now?', mtWarning,
                    [mbYes, mbNo, mbCancel], 0);

    if r = mrYes then
    begin
      if saveFile then
      begin
        openDialog1.FileName := 'loops.asm';
        openFile(false);
      end
    end
    else if r = mrCancel then
    begin
      { Do nothing }
    end
    else if r = mrNo then
    begin
      openDialog1.FileName := 'loops.asm';
      openFile(false);
    end
  end
  else
  begin
    openDialog1.FileName := 'loops.asm';
    openFile(false);
  end
end;


procedure TFormMain.MenuExampleLiftClick(Sender: TObject);
Var r : Word;
begin
  doHalt;

  if memoSource.modified then
  begin
    r := messageDlg('Save source code now?', mtWarning,
                    [mbYes, mbNo, mbCancel], 0);

    if r = mrYes then
    begin
      if saveFile then
      begin
        openDialog1.FileName := 'lift.asm';
        openFile(false);
      end
    end
    else if r = mrCancel then
    begin
      { Do nothing }
    end
    else if r = mrNo then
    begin
      openDialog1.FileName := 'lift.asm';
      openFile(false);
    end
  end
  else
  begin
    openDialog1.FileName := 'lift.asm';
    openFile(false);
  end
end;

procedure TFormMain.MenuExampleTextClick(Sender: TObject);
Var r : Word;
begin
  doHalt;

  if memoSource.modified then
  begin
    r := messageDlg('Save source code now?', mtWarning,
                    [mbYes, mbNo, mbCancel], 0);

    if r = mrYes then
    begin
      if saveFile then
      begin
        openDialog1.FileName := 'text.asm';
        openFile(false);
      end
    end
    else if r = mrCancel then
    begin
      { Do nothing }
    end
    else if r = mrNo then
    begin
      openDialog1.FileName := 'text.asm';
      openFile(false);
    end
  end
  else
  begin
    openDialog1.FileName := 'text.asm';
    openFile(false);
  end
end;

procedure TFormMain.MenuExampleDemonstrationClick(Sender: TObject);
Var r : Word;
begin
  doHalt;

  if memoSource.modified then
  begin
    r := messageDlg('Save source code now?', mtWarning,
                    [mbYes, mbNo, mbCancel], 0);

    if r = mrYes then
    begin
      if saveFile then
      begin
        openDialog1.FileName := 'demo.asm';
        openFile(false);
      end
    end
    else if r = mrCancel then
    begin
      { Do nothing }
    end
    else if r = mrNo then
    begin
      openDialog1.FileName := 'demo.asm';
      openFile(false);
    end
  end
  else
  begin
    openDialog1.FileName := 'demo.asm';
    openFile(false);
  end
end;

procedure TFormMain.MenuEditSelectAllClick(Sender: TObject);
begin
  if memoSource.focused then
  begin
    memoSource.SelectAll;
  end
end;

procedure TFormMain.Find1Click(Sender: TObject);
begin
  FindDialog1.FindText := MemoSource.SelText;
  FindDialog1.Execute;
end;

procedure TFormMain.FindDialog1Show(Sender: TObject);
begin
  CaptureFocus := False;
end;

procedure TFormMain.FindDialog1Close(Sender: TObject);
begin
  CaptureFocus := True;
end;

procedure TFormMain.FindDialog1Find(Sender: TObject);
var
 SelPos,
 SPos,
 SLen,
 TextLength    : Integer;
 SearchString  : string;
begin
  with FindDialog1 do
  begin
    TextLength:=Length(MemoSource.Lines.Text);

    SPos:=MemoSource.SelStart;
    SLen:=MemoSource.SelLength;

    SearchString := Copy(MemoSource.Lines.Text,
                         SPos + SLen + 1,
                         TextLength - SLen + 1);

    SelPos := Pos(UpperCase(FindText), UpperCase(SearchString));

    if SelPos > 0 then
    begin
      MemoSource.SelStart := (SelPos - 1) + (SPos + SLen);
      MemoSource.SelLength := Length(FindText);

//      {remove this in the OnFind procedure:}
//      MemoSource.SelText := ReplaceText;
    end
    else
    begin
      MessageDlg('Could not find "' + FindText + '".', mtError, [mbOk], 0);
    end
  end;
end;

procedure TFormMain.MenuEditReplaceClick(Sender: TObject);
begin
  ReplaceDialog1.FindText := MemoSource.SelText;
  ReplaceDialog1.Execute;
end;

procedure TFormMain.ReplaceDialog1Replace(Sender: TObject);
var
  Count,
  SelPos,
  SPos,
  SLen,
  TextLength    : Integer;
  SearchString  : string;
begin
  with ReplaceDialog1 do
  begin
    if frReplaceAll in Options then
    begin
      Count := 0;

      repeat
        if MemoSource.SelLength > 0 then
        begin
          MemoSource.SelText := ReplaceText;
        end;

        TextLength:=Length(MemoSource.Lines.Text);

        SPos:=MemoSource.SelStart;
        SLen:=MemoSource.SelLength;

        SearchString := Copy(MemoSource.Lines.Text,
                             SPos + SLen + 1,
                             TextLength - SLen + 1);

        SelPos := Pos(UpperCase(FindText), UpperCase(SearchString));

        if SelPos > 0 then
        begin
          MemoSource.SelStart := (SelPos - 1) + (SPos + SLen);
          MemoSource.SelLength := Length(FindText);
          Count := Count + 1;
        end
        else
        begin
          MemoSource.SelLength := 0;
        end
      until MemoSource.SelLength = 0;

      MessageDlg('Replaced ' + IntToStr(Count) + ' items.', mtError, [mbOk], 0);
    end
    else
    begin
      if MemoSource.SelLength > 0 then
      begin
        MemoSource.SelText := ReplaceText;
      end;

      TextLength:=Length(MemoSource.Lines.Text);

      SPos:=MemoSource.SelStart;
      SLen:=MemoSource.SelLength;

      SearchString := Copy(MemoSource.Lines.Text,
                           SPos + SLen + 1,
                           TextLength - SLen + 1);

      SelPos := Pos(UpperCase(FindText), UpperCase(SearchString));

      if SelPos > 0 then
      begin
        MemoSource.SelStart := (SelPos - 1) + (SPos + SLen);
        MemoSource.SelLength := Length(FindText);
      end
      else
      begin
        MessageDlg('Could not find "' + FindText + '".', mtError, [mbOk], 0);
      end
    end;
  end;
end;

procedure TFormMain.ReplaceDialog1Find(Sender: TObject);
var
 SelPos,
 SPos,
 SLen,
 TextLength    : Integer;
 SearchString  : string;
begin
  with ReplaceDialog1 do
  begin
    TextLength:=Length(MemoSource.Lines.Text);

    SPos:=MemoSource.SelStart;
    SLen:=MemoSource.SelLength;

    SearchString := Copy(MemoSource.Lines.Text,
                         SPos + SLen + 1,
                         TextLength - SLen + 1);

    SelPos := Pos(UpperCase(FindText), UpperCase(SearchString));

    if SelPos > 0 then
    begin
      MemoSource.SelStart := (SelPos - 1) + (SPos + SLen);
      MemoSource.SelLength := Length(FindText);

//      {remove this in the OnFind procedure:}
//      MemoSource.SelText := ReplaceText;
    end
    else
    begin
      MessageDlg('Could not find "' + FindText + '".', mtError, [mbOk], 0);
    end
  end;
end;

procedure TFormMain.ReplaceDialog1Show(Sender: TObject);
begin
  CaptureFocus := False;
end;

procedure TFormMain.ReplaceDialog1Close(Sender: TObject);
begin
  CaptureFocus := True;
end;

procedure TFormMain.glyphEditClick(Sender: TObject);
begin
  if MemoSource.Font.Size < 72 then
  begin
    MemoSource.Font.Size := MemoSource.Font.Size + 1;
    MemoList.Font.Size := MemoSource.Font.Size;
    PageControl1.Font.Size := MemoSource.Font.Size;
    ListBoxTokens.Font.Size := MemoSource.Font.Size;
  end
end;

procedure TFormMain.glyphListClick(Sender: TObject);
begin
  if MemoSource.Font.Size > 6 then
  begin
    MemoSource.Font.Size := MemoSource.Font.Size - 1;
    MemoList.Font.Size := MemoSource.Font.Size;
    PageControl1.Font.Size := MemoSource.Font.Size;
    ListBoxTokens.Font.Size := MemoSource.Font.Size;
  end
end;

procedure TFormMain.SpeedButton1Click(Sender: TObject);
begin
  if fsBold in MemoSource.Font.Style then
  begin
    MemoSource.Font.Style := [];
    MemoList.Font.Style := [];
    PageControl1.Font.Style := [];
    ListBoxTokens.Font.Style := [];
  end
  else
  begin
    MemoSource.Font.Style := [fsBold];
    MemoList.Font.Style := [fsBold];
    PageControl1.Font.Style := [fsBold];
    ListBoxTokens.Font.Style := [fsBold];
  end
end;

procedure TFormMain.SpeedButtonKeybClick(Sender: TObject);
begin
  myShow(FormKeyb)
end;

procedure TFormMain.SpeedButton2Click(Sender: TObject);
begin
  myShow(FormKeyPad)
end;

procedure TFormMain.TabSheetSourceCodeShow(Sender: TObject);
begin
  MemoSource.SetFocus;
end;

procedure TFormMain.TabSheetListFileShow(Sender: TObject);
begin
  MemoList.SetFocus;
end;

procedure TFormMain.TabSheetTokensShow(Sender: TObject);
begin
  ListBoxTokens.SetFocus;
end;

procedure TFormMain.TabSheetLogShow(Sender: TObject);
begin
  RichEdit1.SetFocus
end;

procedure TFormMain.Keyboard1Click(Sender: TObject);
begin
  myShow(formKeyb);
end;

procedure TFormMain.KeyPad1Click(Sender: TObject);
begin
  myShow(formKeyPad);
end;

procedure TFormMain.HideAll1Click(Sender: TObject);
begin
  formTlight.Close;
  formSevenSeg.Close;
  formStep.Close;
  formMaze.Close;
  formHeat.Close;
  formVdu.Close;
  formLift.Close;
  formKeyb.Close;
  formKeyPad.Close;
end;

procedure TFormMain.SpeedButton3Click(Sender: TObject);
begin
  HideAll1Click(Nil);
  FormRamHex.Close;
end;

procedure TFormMain.SpeedButton4Click(Sender: TObject);
begin
  shellExecute(handle,
               Nil,
               'http://groups.yahoo.com/group/learn-asm/',
               Nil,
               Nil,
               0)
end;

procedure TFormMain.error(msg : string);
begin
  messageDlg(msg, mtError, [mbOK], 0);
end;

end.



