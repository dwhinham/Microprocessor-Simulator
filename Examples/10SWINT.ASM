; --------------------------------------------------------------
; An example of software interrupts.
; --------------------------------------------------------------
	JMP	Start	; Jump past table of interrupt vectors
	DB	51	; Vector at 02 pointing to address 51
	DB	71	; Vector at 03 pointing to address 71
Start:
	INT	02	; Do interrupt 02
	INT	03	; Do interrupt 03
	JMP	Start
; --------------------------------------------------------------

	ORG	50
	DB	E0	; Data byte - could be a whole table here
			; Interrupt code starts here
	MOV	AL,[50]	; Copy bits from RAM into AL
	NOT	AL	; Invert the bits in AL
	MOV	[50],AL	; Copy inverted bits back to RAM
	OUT	01	; Send data to traffic lights
	IRET
; --------------------------------------------------------------
	ORG	70
	DB	0	; Data byte  - could be a table here

; Interrupt code starts here
	MOV	AL,[70]	; Copy bits from RAM into AL
	NOT	AL	; Invert the bits in AL
	AND	AL,FE	; Force right most bit to zero
	MOV	[70],AL	; Copy inverted bits back to RAM
	OUT	02	; Send data to seven segment display
	IRET
; --------------------------------------------------------------
	END
; --------------------------------------------------------------

TASK

26)	Write a new interrupt 02 that fetches a key press from the 
	keyboard and stores it into RAM.  The IBM PC allocates 16 
	bytes for key press storage.  The 16 locations are used in 
	a circular fashion.

27)	Create a new interrupt that puts characters onto the next 
	free screen location.  See if you can get correct behaviour 
	in response to the Enter key being pressed (fairly easy) 
	and if the Back Space key is pressed (harder).

