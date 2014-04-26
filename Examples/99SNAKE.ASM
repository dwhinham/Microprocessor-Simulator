; ================================================================
; ===== 99snake.asm ==============================================
; ===== Snake and Maze ===========================================

Start:
	MOV	AL,FF	; Special code to reset the snake.
	OUT	04	; Send AL to port 04 to control the snake.

	MOV	AL,4F	; 4 means DOWN.  F means 15.
	OUT	04	; Send 4F to the snake
	OUT	04	; Send 4F to the snake
	OUT	04	; Send 4F to the snake
	OUT	04	; Send 4F to the snake

	JMP	Start

	END
; ================================================================