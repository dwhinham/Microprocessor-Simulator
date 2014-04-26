; ----- EXAMPLE 8 ------- DATA TABLES --------------------------

	JMP	Start	; Skip past the data table.

	DB	84	; Data table begins.
	DB	C8	; These values control the traffic lights
	DB	31	; This sequence is simplified.
	DB	58	; Last entry is also used as end marker

Start:
	MOV	BL,02	; 02 is start address of data table
Rep:
	MOV	AL,[BL]	; Copy data from table to AL
	OUT	01	; Output from AL register to port 01

	CMP	AL,58	; Last item in data table ???
	JZ	Start	; If yes then jump to Start
	INC	BL	; In no then point BL to the next entry
	JMP	Rep	; Jump back to do next table entry

	END
; --------------------------------------------------------------

TASK

18)	Improve the traffic lights data table so there is an 
	overlap with both sets of lights on red.

19)	Use a data table to navigate the snake through the maze.
	This is on port 04.  Send FF to the snake to reset it.  
	Up, down left and right are controlled by the left four bits.  
	The right four bits control the distance moved.

20)	Write a program to spin the stepper motor. Activate bits 
	1, 2, 4 and 8 in sequence to energise the electromagnets
	in turn. The motor can be half stepped by turning on pairs 
	of magnets followed by a single magnet followed by a pair 
	and so on.

21)	Use a data table to make the motor perform a complex sequence 
	of forward and reverse moves.  This is the type of control 
	needed in robotic systems, printers and plotters.  For this 
	exercise, it does not matter exactly what the motor does.

; --------------------------------------------------------------
