; ===== LIFT =====
	mov	al,0	; RESET
	out	06	; RESET
	call	10	; Initialise lift at bottom

Start:
	call	C0	; Procedure to handle button presses
	call	70	; Procedure to start the lift moving
	jmp	Start	; Repeat the main loop

; ===== Going Down ============================================

	org	10

repGoingDown:
	call	C0	; Procedure to handle button presses
	in	06
	and	al,8	; Test for lift arrived
	jnz	endGoingDown

	mov	al,2	; Turn on down motor
	or	al,bl	; Remember button states
	out	06
	jmp	repGoingDown

endGoingDown:
	mov	al,0
	and	bl,20	; Force off down button
	or	al,bl
	out	06

	ret

; ===== Going Up ==============================================

	org	40

repGoingUp:
	call	C0	; Procedure to handle button presses
	in	06
	and	al,4	; Test for lift arrived
	jnz	endGoingUp

	mov	al,1	; Turn on up motor
	or	al,bl	; Remember button states
	out	06
	jmp	repGoingUp

endGoingUp:
	mov	al,0
	and	bl,10	; Force off up button
	or	al,bl
	out	06

	ret

; ===== Pause or go again ==========================================

	org	70

	in	06
	push	al	; Copy AL ...
	pop	cl	; ... into CL
	and	cl,0C	; Mask off bits except lift arrived sensors

	cmp	bl,30	; Test for both buttons
	jz	mightGoEither

	cmp	bl,20	; Test for the up button
	jz	mightGoUp

	cmp	bl,10	; Test for the down button
	jz	mightGoDown

	jmp	skip	; No button presses detected

mightGoEither:
	cmp	cl,8	; Is lift at bottom?
	jz	goUp

	cmp	cl,4	; Is lift at top?
	jz	goDown
	jmp	skip

mightGoUp:
	cmp	cl,8	; Is lift at bottom?
	jz	goUp
	jmp	skip

mightGoDown:
	cmp	cl,4	; Is lift at top?
	jz	goDown
	jmp	skip

goUp:
	call	40	; Go Up
	jmp	skip

goDown:
	call	10	; Go Down

skip:
	ret

; ===== Button Presses
; ===== Test for button presses
; ===== Store the result in BL
	org	C0

	in	06	; Input from lift
	and	al,30	; Mask off all bits except button presses
	or	bl,al	; Copy up/down button state into BL

	ret

; ===== END =====

	end

; =================================================================
