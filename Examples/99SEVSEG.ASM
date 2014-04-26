; ======================================================
; ===== 99sevseg.asm ===================================
; ===== Seven Segment Displays Port 02 =================
Start:
	MOV	AL,FA	; 1111 1010
	OUT	02	; Send the data in AL to Port 02

	MOV	AL,0	; 0000 0000
	OUT	02	; Send the data in AL to Port 02
	
	MOV	AL,FB	; 1111 1011
	OUT	02	; Send the data in AL to Port 02

	MOV	AL,1	; 0000 0001
	OUT	02	; Send the data in AL to Port 02

	JMP	Start
	
	END
; ======================================================