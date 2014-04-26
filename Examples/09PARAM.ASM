; ----- EXAMPLE 9 ------- Passing Parameters -------------------

; ----- Use Registers to pass parameters into a procedure ------

	JMP 	Start	; Skip over bytes used for data storage

	DB	0	; Reserve a byte of RAM at address [02]
	DB	0	; Reserve a byte of RAM at address [03]
Start:
	MOV	AL,5
	MOV	BL,4
	CALL	30	; A procedure to add AL to BL
			; Result returned in AL.

; ----- Use RAM locations to pass parameters into a procedure --

	MOV	AL,3
	MOV	[02],AL	; Store 3 into address [02]
	MOV	BL,1
	MOV	[03],BL	; Store 1 into address [03]
	CALL	40

; ----- Use the Stack to pass parameters into a procedure ------
	MOV	AL,7
	PUSH	AL
	MOV	BL,2
	PUSH	BL
	CALL	60
	POP	BL
	POP	AL	; This one contains the answer

	JMP	Start	; Go back and do it again.

; ----- A procedure to add two numbers -------------------------

;	Parameters passed into procedure using AL and BL
;	Result returned in AL
;	This method is simple but is no good if there are a 
;	lot of parameters to be passed.

	ORG	30	; Code starts at address [30]

	ADD	AL,BL	; Do the addition.  Result goes into AL

	RET		; Return from the procedure

; ----- A procedure to add two numbers -------------------------
;	Parameters passed into procedure using RAM locations.
;	Result returned in RAM location

;	This method is more complex and there is no limit on
;	the number of parameters passed unless RAM runs out.

	ORG	40	; Code starts at address [40]

	PUSH	CL	; Save registers and flags on the stack
	PUSH	DL
	PUSHF

	MOV	CL,[02]	; Fetch a parameter from RAM
	MOV	DL,[03]	; Fetch a parameter from RAM
	ADD	CL,DL	; Do the addition
	MOV	[02],CL	; Store the result in RAM

	POPF		; Restore original register 

	POP	DL	; and flag values
	POP	CL

	RET

; ----- A procedure to add two numbers -------------------------
; 	The numbers to be added are on the stack.
;	POP parameters off the stack
;	Do the addition
;	Push answer back onto the stack
;	The majority of procedure calls in real life make use
;	of the stack for parameter passing.  It is very common
;	for the address of a complex data structure in RAM to 
;	be passed to a procedure using the stack.

	ORG	60	; Code starts at address [60]

	POP	DL	; Return address
	POP	BL	; A parameter
	POP	AL	; A parameter

	ADD	AL,BL

	PUSH	AL	; Answer ; The number of pushes must
	PUSH	AL	; Answer ; match the number of pops.
	PUSH	DL	; Put the stack back as it was before

	RET
; --------------------------------------------------------------
	END

Task

22)	Write a procedure that doubles a number.  Pass the single 
	parameter into the procedure using a register.  Use the 
	same register to return the result.

23)	Write a procedure to invert all the bits in a byte.  All 
	the zeros should become ones.  All the ones should become 
	zeros.  Pass the value to be processed into the procedure 
	using a RAM location.  Return the result in the same RAM 
	location.

24)	Write a procedure that works out Factorial N.  This example 
	shows one method for working out factorial N.  
	Factorial 5 is 5 * 4 * 3 * 2 * 1 = 120.  Your procedure 
	should work properly for factorial 1, 2, 3, 4 or 5.  
	Factorial 6 would cause an overflow.  Use the stack to pass 
	parameters and return the result.  Calculate the result.  
	Using a look up table is cheating!

25)	Write a procedure that works out Factorial N.  Use the 
	stack for parameter passing.  Write a recursive 
	procedure.  Use this definition of Factorial.  

	Factorial ( 0 )  is defined as 1.
	Factorial ( N ) is defined as N * Factorial (N - 1).

	To work out Factorial (N), the procedure first tests to see 
	if N is zero and if not then re-uses itself to work out 
	N * Factorial (N - 1).  This problem is hard to understand 
	in any programming language.  In assembly code it is 
	harder still.
