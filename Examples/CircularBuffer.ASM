; ====================================
; ===== CIRCULAR KEYBOARD BUFFER =====
; ====================================
	jmp	start
	db	30	; [02] Interrupt 02 is located at address 30
	db	60	; [03] Interrupt 03 is located at address 60
	db	10	; [04] Use this byte as a pointer to the 
			; head of the keyboard buffer 
	db	10	; [05] Use this byte as a pointer to the 
			; tail of the keyboard buffer 
	db	0	; [06] Use this byte to count how full 
			; the buffer is
			; The keyboard buffer is located at 
			; address 10 to 1F

	org	20
start:
	in	0	; Input a key press
	cmp	al,0d	; Test for the Enter key
	jz	here	; Jump here if Enter key pressed
	int	02	; Simulate the hardware interrupt
	jmp	start	; Get next key press
here:
	int	03	; Clear the screen and empty the 
			; keyboard buffer
	jmp	start

; ===== Input Interrupt handler code goes here =====
	org	30
	push	al
	push	bl
	push	cl
	pushf

	mov	bl,[04]	; BL contains the address of the head 
			; of the buffer
	mov	cl,[06]	; CL contains character count
	cmp	cl,10	; Is the buffer full?
	jz	ignore	; Jump here if full

	inc	cl	; Count the new character
	mov	[06],cl	; Store the character count
	mov	[bl],al	; Store ASCII code in the buffer
	inc	bl	; Increment the buffer head
	mod	bl,10	; Make BL wrap back to 0 when it reaches 20
	add	bl,10	; Add 10 to BL because the buffer 
			; start is offset to address 10
	mov	[04],bl	; Save the buffer head

ignore:
	popf
	pop	cl
	pop	bl
	pop	al
	iret

; ===== Output Interrupt handler code goes here =====
	org	60
	push	al
	push	bl
	push	cl
	push	dl
	pushf
			; Clear the Screen
	mov	bl,10	; Counter
	mov	al,20	; Space ASCII code
	mov	cl,c0	; Start of video ram
rep:
	mov	[cl],al	; Copy space char to screen
	inc	cl
	dec	bl
	jnz	rep

			; Display buffer contents
	mov	bl,[05]	; Buffer tail
	mov	cl,[06]	; Character count
	cmp	cl,0	; Test for empty buffer
	jz	empty
	mov	dl,c0	; Video RAM start
rep2:
	mov	al,[bl]	; Copy buffer tail into al
	mov	[dl],al	; Copy AL to video ram
	inc	dl	; Next screen position
	inc	bl	; Buffer tail
	mod	bl,10	; Wrap BL to 0 when it reaches 20
	add	bl,10	; Add 10 becuase buffer starts at address 10
	mov	[05],bl	; Save buffer tail
	dec	cl	; Count down char count
	mov	[06],cl	; Save char count
	jnz	rep2

empty:
	popf
	pop	dl
	pop	cl
	pop	bl
	pop	al
	iret
; ===================================================
	end
