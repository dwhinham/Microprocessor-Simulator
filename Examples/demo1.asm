; A demonstration program that exercises many instructions
; and most of the peripherals available in the simulator.
; CALL, RET, INT and IRET are not used because these
; commands are not available in the shareware version.

; ----- DATA TABLE ---------------------------------------------

	JMP	HERE		; Skip past data section

	DB	"HELLO WORLD!"	; Load text string into RAM
	DB	0		; Null terminated

; ----- DISPLAY TEXT -------------------------------------------

HERE:
	CLO			; Close all peripheral windows
	MOV	CL,C0		; Video ram base address
	MOV	BL,02		; Offset of text string

START:				; Text output to vdu
	MOV	AL,[BL]		; Text pointer into AL
	CMP	AL,0		; At end yet
	JZ	END1		; Jump out of loop
	MOV	[CL],AL		; AL into video memory
	INC	CL		; Next video location
	INC	BL		; Next text character
	JMP	START		; Not there yet
END1:

; ----- TRAFFIC LIGHTS -----------------------------------------
	MOV	BL,0C		; Flash the traffic lights
	MOV	AL,AA		; 1010 1010b
REP1:
	OUT	01		; Lights are on port one
	NOT	AL		; 0101 0101b
	DEC	BL		; Count down
	JNZ	REP1		; Jump out of loop on zero

; ----- KLUDGE BECAUSE MAX JUMP IS -128 ------------------------
	JMP	SKIPOVR		; Jump forward past jump back
MIDDLE:
	JMP	HERE		; Jump back rest of the way
SKIPOVR:

; ----- SEVEN SEGMENT DISPLAYS ---------------------------------
	MOV	BL,0C		; Flash seven segment displays
REP2:
	MOV	AL,FF		; 1111 1111b
	OUT	02		; Lights are on port one
	MOV	AL,FE		; 1111 1110b
	OUT	02		; LSB used for multiplexing

	MOV	AL,01		; 0000 0001b
	OUT	02		;
	MOV	AL,0		; 0000 0000b
	OUT	02		;

	DEC	BL		; Count down
	JNZ	REP2		; Jump out of loop on zero

; ----- HEATER AND THERMOSTAT ----------------------------------
	IN	03		; Input from thermostat on port 3
	CMP	AL,01		; Is it too warm
	JZ	OFF		; If no then jump to OFF
	MOV	AL,80		; Use MSB to turn heater on.
	OUT	03		; Send 10000000 to port 3
	JMP	Skip2		; Jump past heater-off code
OFF:	
	MOV	AL,0		; Turn of heater with 00000000
	OUT	03		; Send 00000000 to port 3
Skip2:
	MOV	BL,20		; Time Delay
ON:	
	DEC	BL		; BL counts down for time delay
	JNZ	ON		; Jump out of loop on zero

; ----- SNAKE IN THE MAZE --------------------------------------
	MOV	AL,FF		; Maze reset
	OUT	04		; Snake is on port 4
	MOV	BL,0A		; Count down start value
	MOV	AL,4F		; 4 means down.  F means 15.
REP5:
	OUT	04		; Send data to snake
	DEC	BL		; Count down
	JNZ	REP5		; Jump out of loop on zero

; ----- SPIN THE STEPPER MOTOR ---------------------------------
	MOV	BL,20		; Count down start value
	MOV	AL,11		; 0001 0001b
REP6:
	OUT	05		; Stepper motor is on port 5
	ROL	AL		; Rotate bits left
	DEC	BL		; Count down
	JNZ	REP6		; Jump out of loop on zero

; ----- CLEAR THE VDU SCREEN -----------------------------------
	MOV	CL,C0		; Screen base address
	MOV	BL,0C		; Clear row on screen
	MOV	AL,20		; ASCII space
REP7:
	MOV	[CL],AL		; Data to video RAM
	INC	CL		; Next RAM location
	DEC	BL		; Count down
	JNZ	REP7		; Jump out of loop on zero
	JMP	MIDDLE		; Jump towards start
; --------------------------------------------------------------	
	END
; --------------------------------------------------------------	
