; ===== Heater and Thermostst on Port 03 ==========================
; ===== 99Heater.asm ==============================================
; ===== Heater and Thermostst on Port 03 ==========================
	MOV	AL,0	; Code to turn the heater off
	OUT	03	; Send code to the heater

	IN	03	; Input from Port 03
	AND	AL,1	; Mask off left seven bits
	JZ	Cold	; If the result is zero, turn the heater on
	HALT		; Quit

Cold:
	MOV	AL,80	; Code to turn the heater on
	OUT	03	; Send code to the heater
	
	END
; =================================================================

