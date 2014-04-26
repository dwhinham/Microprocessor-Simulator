; --------------------------------------------------------------
; A program to read in a string of text and store it in RAM.
; The end of text will be labelled with ASCII code zero/null.
; --------------------------------------------------------------
; THE MAIN PROGRAM
	MOV	BL,70	; [70] is the address where the text will
			; be stored. The procedure uses this.

	CALL	10	; The procedure at [10] reads in text and
			; places it starting from the address
			; in BL.

			; BL should still contain [70] here.

	CALL	40	; This procedure does nothing until you
			; write it.  It should display the text.

	HALT		; DON'T USE END HERE BY MISTAKE.
; --------------------------------------------------------------
; A PROCEDURE TO READ IN THE TEXT
	ORG	10	; Code starts from address [10]

	PUSH	AL	; Save AL onto the stack
	PUSH	BL	; Save BL onto the stack
	PUSHF		; Save the CPU flags onto the stack

Rep:
	IN	00	; Input from port 00 (keyboard)
	CMP	AL,0D	; Was key press the Enter key?
	JZ	Stop	; If yes then jump to Stop
	MOV	[BL],AL	; Copy keypress to RAM at position [BL]
	INC	BL	; BL points to the next location.
	JMP	Rep	; Jump back to get the next character

Stop:
	MOV	AL,0	; This is the NULL end marker
	MOV	[BL],AL	; Copy NULL character to this position.

	POPF		; Restore flags from the stack
	POP	BL	; Restore BL from the stack
	POP	AL	; Restore AL from the stack

	RET		; Return from the procedure.
; --------------------------------------------------------------
; A PROCEDURE TO DISPLAY TEXT ON THE SIMULATED SCREEN
	ORG	40	; Code starts from address [10]
			; **** YOU MUST FILL THIS GAP ****
	RET		; At present this procedure does
			; nothing other than return.

; --------------------------------------------------------------
	END		; It is correct to use END at the end.
; --------------------------------------------------------------

TASK

17)	Write a program using three procedures.  The first should 
	read text from the keyboard and store it in RAM.  The second 
	should convert any upper case characters in the stored text 
	to lower case.  The third should display the text on the 
	VDU screen.

; --------------------------------------------------------------

