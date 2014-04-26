; ===============================================
; =====     EXERCISE MANY INSTRUCTIONS      =====
; ===== Run this program continuously with  =====
; ===== a fast clock rate to allow the      =====
; ===== interrupts to process in due time.  =====
; ===== The program must be registered to   =====
; ===== allow the full instruction set to   =====
; ===== function.                           =====
; ===============================================

; ===== DATA TABLE ==============================
	jmp	start
	db	1C
	db	40
	db	C0	; reserved for keyoard
	db	11	; reserved for motor
	db	"Hello World!"
	db	0
; ===============================================

; ===== INITIALISE SYSTEMS ======================
start:
	clo
	sti
	out	07
; ===============================================

; ===== MAIN PROGRAM LOOP =======================
rep1:
	call	62
	nop
	jmp	rep1
; ===============================================

; ===== INT 02 Handler - Heater and Motor =======
	org	1C
	push	al
	pushf
			; ===== MOTOR =====
	mov	al,[05]
	out	05
	rol	al
	mov	[05],al
			; ===== HEATER =====
	in	03
	and	al,01
	jz	too_hot
	mov	al, 0
	out	03
	jmp	end_heat	
too_hot:
	mov	al,80
	out	03

end_heat:
	popf
	pop	al
	iret
; ===============================================

; ===== INT 03 Handler - Keyboard ===============
	org	40
	CLI		; Clear the I flag
	push	al
	push	bl
	pushf

	in	07	; Fetch ASCII code from keyboard
	mov	bl,[04]
	mov	[bl],al
	inc	bl
	cmp	bl,0
	jnz	end_char
	mov	bl,c0
end_char:
	mov	[04],bl

	popf
	pop	bl
	pop	al
	STI		; Set the I flag.
	iret
; ===============================================

; ===== 62 ======================================
	org	62
	push	al
	pushf
	
	mov	al,84
	out	01
	mov	al,50
	call	91
	mov	al,C8
	out	01
	mov	al,10
	call	91
	mov	al,30
	out	01
	mov	al,50
	call	91
	mov	al,58
	out	01
	mov	al,10
	call	91

	popf
	pop	al
	ret
; ===============================================

; ===== TIME DELAY AND INSTRUCTION EXERCISE =====
	org	91
	push	al
	push	bl
	pushf

rep2:
	dec	al
	out	02
	jnz	rep2

	popf
	pop	bl
	pop	al
	ret
; ===============================================

; ===============================================
	end
; ===============================================


ADD    AL,BL    A0 00 01      AL   =  AL + BL 
SUB    BL,CL    A1 01 02      BL   =  BL - CL
MUL    CL,DL    A2 02 03      CL   =  CL * DL
DIV    DL,AL    A3 03 00      DL   =  DL / AL
MOD    AL,BL    A6 00 01      AL   =  AL mod BL
INC    DL       A4 03         DL   =  DL + 1
DEC    AL       A5 00         AL   =  AL - 1

AND    AL,BL    AA 00 01      AL   =  AL AND BL

OR     CL,BL    AB 02 01      CL   =  CL OR  BL
XOR    AL,BL    AC 00 01      AL   =  AL XOR BL

NOT    BL       AD 01         BL   =   NOT BL

ROL    AL       9A 00   Rotate bits left.   LSB  :=  MSB
ROR    BL       9B 01   Rotate bits right.  MSB  :=  LSB.
SHL    CL       9C 02   Shift bits left.    Discard MSB.
SHR    DL       9D 03   Shift bits right.   Discard LSB.



Immediate Arithmetic and Logic Instructions.  Flags are set.

Assembler	Machine Code  Explanation
ADD    AL,12    B0 00 12      AL  =  AL + 12
SUB    BL,15    B1 01 15      BL  =  BL - 15
MUL    CL,03    B2 02 03      CL  =  CL * 03
DIV    DL,02    B3 03 02      DL  =  DL / 02
MOD    AL,10    B6 00 10      AL  =  AL mod 10

OR     CL,F0    BB 02 F0      CL  =  CL OR  F0
XOR    AL,AA    BC 00 AA      AL  =  AL XOR AA

Compare Instructions.  Flags are set.

Assembler	Machine Code  Explanation
CMP    AL,BL    DA 00 01      Set 'Z' if AL = BL   
CMP    CL,[20]  DC 02 20      Set 'Z' if CL = [20] 
JS     STOP     C3 09         Increase IP by 9 if S flag is set
JNS    START    C4 04         Increase IP by 4 if S flag not set
JO     REPEAT   C5 09         Increase IP by 9 if O flag is set
JNO    AGAIN    C6 04         Increase IP by 4 if O flag not set

INT    01       CC 01         Run code starting at address in 01


HALT            00            Halt the processor clock
