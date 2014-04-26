; --------------------------------------------------------------
; An example of using hardware interrupts.
; This program spins the stepper motor continuously and 
; steps the traffic lights on each hardware interrupt.

; Uncheck the "Show only one peripheral at a time" box
; to enable both displays to appear simultaneously.

; --------------------------------------------------------------
	JMP	Start	; Jump past table of interrupt vectors
	DB	50	; Vector at 02 pointing to address 50

Start:
	STI		; Set I flag. Enable hardware interrupts
	MOV	AL,11	;
Rep:
	OUT	05	; Stepper motor
	ROR	AL	; Rotate bits in AL right
	JMP	Rep
	JMP	Start
; --------------------------------------------------------------
	ORG	50

	PUSH	al	; Save AL onto the stack.
	PUSH	bl	; Save BL onto the stack.
	PUSHF		; Save flags onto the stack.

	JMP	PastData

	DB	84	; Red		Green
	DB	c8	; Red+Amber	Amber
	DB	30	; Green		Red
	DB	58	; Amber		Red+Amber
	DB	57	; Used to track progress through table
PastData:
	MOV	BL,[5B]	; BL now points to the data table
	MOV	AL,[BL]	; Data from table goes into AL
	OUT	01	; Send AL data to traffic lights
	CMP	AL,58	; Last entry in the table
	JZ	Reset	; If last entry then reset pointer

	INC	BL	; BL points to next table entry
	MOV	[5B],BL	; Save pointer in RAM
	JMP	Stop
Reset:
	MOV	BL,57	; Pointer to data table start address
	MOV	[5B],BL	; Save pointer into RAM location 54
Stop:
	POPF		; Restore flags to their previous value
	POP	bl	; Restore BL to its previous value
	POP	al	; Restore AL to its previous value

	IRET
; --------------------------------------------------------------

END
; --------------------------------------------------------------

TASK

28)	Write a program that controls the heater and thermostat 
	whilst at the same time counting from 0 to 9 repeatedly, 
	displaying the result on one of the seven segment displays.  
	If you want a harder challenge, count from 0 to 99 repeatedly 
	using both displays.  Use the simulated hardware interrupt to 
	control the heater and thermostat.

29)	A fiendish problem.  Solve the Tower of Hanoi problem whilst 
	steering the snake through the maze.  Use the text characters 
	A, B, C Etc. to represent the disks.  Use three of the four 
	rows on the simulated screen to represent the pillars.
	
30)	Use the keyboard on Port 07. Write an interrupt handler 
	(INT 03) to process the key presses. You must also process 
	INT 02 (the hardware timer) but it need not perform any task. 
	For a more advanced task, implement a 16 byte circular buffer. 
	Write code to place the buffered text on the VDU screen when 
	you press Enter. For an even harder task, implement code to 
	process the Backspace key to delete text characters in the buffer. 