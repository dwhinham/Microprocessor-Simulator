; -------------------------------------------------
; Making Comparisons using CMP
; -------------------------------------------------

; 	COMPARE REGISTERS
;	STOP REPEATING WHEN REGISTERS ARE EQUAL
Start:
	MOV	CL,0	; INITIALISE COUNTER
	MOV	DL,10	; INITIALISE TEST VALUE
rept1:
	INC	CL	; INCREMENT CL
	CMP	CL,DL	; COMPARE CL WITH DL
			; 'Z' FLAG IS SET IF THEY ARE EQUAL
	JNZ	rept1	; REPEAT IF 'Z' FLAG IS NOT SET

; -------------------------------------------------

; 	COMPARE REGISTERS
;	STOP REPEATING WHEN CL < DL

	MOV	CL,20	; INITIALISE COUNTER
	MOV	DL,10	; INITIALISE TEST VALUE
rept2:
	DEC	CL	; DECREMENT CL
	CMP	CL,DL	; COMPARE CL WITH DL
			; 'S' FLAG IS SET IF CL < DL
	JNS	rept2	; REPEAT IF 'S' FLAG IS NOT SET

; -------------------------------------------------

; 	COMPARE IMMEDIATE (Register with a number)
;	STOP REPEATING WHEN DL = 10

	MOV	DL,0	; INITIALISE COUNTER
rept3:
	INC	DL	; INCREMENT DL
	CMP	DL,10	; COMPARE DL WITH 10
			; 'Z' FLAG IS SET IF DL = 10
	JNZ	rept3	; REPEAT IF 'Z' FLAG IS NOT SET

; -------------------------------------------------

; 	COMPARE IMMEDIATE (Register with a number)
;	STOP REPEATING WHEN DL < 10

	MOV	DL,20	; INITIALISE COUNTER
rept4:
	DEC	DL	; DECREMENT DL
	CMP	DL,10	; COMPARE DL WITH 10
			; 'S' FLAG IS SET IF DL < 10
	JNS	rept4	; REPEAT IF 'S' FLAG IS NOT SET

; -------------------------------------------------

; 	COMPARE INDIRECT (Register with a RAM location)
;	STOP REPEATING WHEN DL = RAM LOCATION [50]

	MOV	DL,0	; INITIALISE COUNTER
	MOV	AL,10	; TEST VALUE
	MOV	[50],AL	; PLACE TEST VALUE INTO RAM [50]
rept5:
	INC	DL	; INCREMENT DL
	CMP	DL,[50]	; COMPARE DL WITH RAM LOCATION [50]
			; 'Z' FLAG IS SET IF DL = 10
	JNZ	rept5	; REPEAT IF 'Z' FLAG IS NOT SET

; -------------------------------------------------

; 	COMPARE INDIRECT (Register with a RAM location)
;	STOP REPEATING WHEN DL < RAM LOCATION [50]

	MOV	DL,20	; INITIALISE COUNTER
	MOV	AL,10	; TEST VALUE
	MOV	[50],AL	; PLACE TEST VALUE INTO RAM [50]
rept6:
	DEC	DL	; DECREMENT DL
	CMP	DL,[50]	; COMPARE DL WITH 10
			; 'S' FLAG IS SET IF DL < 10
	JNS	rept6	; REPEAT IF 'S' FLAG IS NOT SET

	JMP	Start

; -------------------------------------------------

	END

; -------------------------------------------------
