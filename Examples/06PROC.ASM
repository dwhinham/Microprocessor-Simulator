; ---------------------------------------------------------------

;  A general purpose time delay procedure.

;  The delay is controlled by the value in AL.

;  When the procedure terminates, the CPU registers are
;  restored to the same values that were present before 
;  the procedure was called. Push, Pop, Pushf and Popf
;  are used to achieve this.  In this example one procedure 

;  is re-used three times.  This re-use is one of the main 
;  advantages of using procedures.

;------ The Main Program ----------------------------------------
Start:
	MOV	AL,8	; A short delay.
	CALL	30	; Call the procedure at address [30]

	MOV	AL,10	; A middle sized delay.
	CALL	30	; Call the procedure at address [30]

	MOV	AL,20	; A Longer delay.
	CALL	30	; Call the procedure at address [30]

JMP	Start	; Jump back to the start.

; ----- Time Delay Procedure Stored At Address [30] -------------
	ORG	30	; Generate machine code from address [30]

	PUSH	AL	; Save AL on the stack.
	PUSHF		; Save the CPU flags on the stack.
Rep:
	DEC	AL	; Subtract one from AL.
	JNZ	REP	; Jump back to Rep if AL was not Zero.

	POPF		; Restore the CPU flags from the stack.
	POP	AL	; Restore AL from the stack.

	RET		; Return from the procedure.
; ---------------------------------------------------------------
	END
; ---------------------------------------------------------------

TASK

15)	Re-do the traffic lights program and use this procedure 
	to set up realistic time delays.  02tlight.asm

16)	Re-do the text input and display program with procedures.
	Use one procedure to input the text and one to display it. 

; ---------------------------------------------------------------

