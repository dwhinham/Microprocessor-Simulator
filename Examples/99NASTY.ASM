; ----- Here is how NOT to write a program -----
_: Mov BL,C0 Mov AL,3C Q: Mov [BL],AL CMP AL,7B
JZ Z INC AL INC BL JMP Q Z: MOV CL,40 MOV AL,20
MOV BL,C0 Y: MOV [BL],AL INC BL DEC CL JNZ Y JMP
_ END ; Look at the list file. It comes out OK!
; Press Escape to stop the program running.
; ----------------------------------------------