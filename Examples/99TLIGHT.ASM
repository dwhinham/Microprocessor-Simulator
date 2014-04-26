; ===========================================================================
; ===== 99Tlight.asm ========================================================
; ===== Traffic Lighte on Port 01 ===========================================
Start:
	MOV	AL,55	; 01010101
	OUT	01	; Send the data in AL to Port 01 (the traffic lights)

	MOV	AL,AA	; 10101010
	OUT	01	; Send the data in AL to Port 01 (the traffic lights)

	JMP	Start
	
	END
; ===========================================================================
