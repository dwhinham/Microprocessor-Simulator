; ===== EXAMPLE PROGRAM =====================
;
;	Controlling the lift
;
;	This program needs to run continuously
;	looping fast enough to detect signals from 
;	the lift in good time. Slower hardware
;	might not run this simulation at all.
;
;	If the program is run too slowly, the lift 
;	will crash into	the top or bottom of the 
;	shaft before the program has time to
;	control it. This example has bugs. Can
;	you find them?
;
;	The lift cage moves slowly at the 
;	top and bottom of the shaft.
;
;	If you are using a network, please 
;	save this example to a folder where
;	you have permission to write files.
;
; ===========================================

	mov	al,0	; reset and dhow lift
	out	06
loop:
	in	06
	and	al,20
	jnz	up

	in	06
	and	al,10
	jnz	down

	jmp	loop

up:
	mov	al,21
	out	06
	mov	[BF],al
	in	06
	and	al,4
	jnz	stop
	mov	al,[BF]
	jmp	loop

down:
	mov	al,12
	out	06
	mov	[BF],al
	in	06
	and	al,8
	jnz	stop
	mov	al,[BF]
	jmp	loop

stop:
	mov	al,[BF]
	mov	al,0
	out	06
	jmp	loop

	end
