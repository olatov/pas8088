;org		0x100	; for COM file
org  	0		; for Poisk tape

jmp near start

start:
	; for Poisk tape: set data/stack registers
	mov		ax, es
	mov		ds, ax
	
	; set ES register to video mem
	mov		ax, 0xb800
	mov		es, ax
	
	; sceen mode text, 80x25
	call	set_txt_80x25
	call	hide_cursor

intro:
	mov		si, message
	mov		bh, 24
	mov		bl, 79
	sub		bl, [message_len]
	shr		bl, 1

	call	print_string
	
	call	prep_field
	
main:
	; print "Score" text
	mov		bh, 0
	mov		bl, 0
	mov		si, score_text
	call	print_string
	; convert score to string
	mov		ax, [score]
	mov		di, buf
	mov		dx, 8
	call	word_to_str
	; print score value
	mov		bh, 0
	mov		bl, 5
	mov		si, buf
	call	print_string

	; CS -> hex str
	mov		ax, cs
	mov		di, buf
	mov		dx, 8
	call	word_to_str_hex
	mov		bh, 0
	mov		bl, 16
	mov		si, buf
	call	print_string
	
	; DS -> hex str
	mov		ax, ds
	mov		di, buf
	mov		dx, 8
	call	word_to_str_hex
	mov		bh, 0
	mov		bl, 24
	mov		si, buf
	call	print_string

	; ES -> hex str
	mov		ax, es
	mov		di, buf
	mov		dx, 8
	call	word_to_str_hex
	mov		bh, 0
	mov		bl, 32
	mov		si, buf
	call	print_string

	; SS -> hex str
	mov		ax, ss
	mov		di, buf
	mov		dx, 8
	call	word_to_str_hex
	mov		bh, 0
	mov		bl, 40
	mov		si, buf
	call	print_string

	; SP -> hex str
	mov		ax, sp
	mov		di, buf
	mov		dx, 8
	call	word_to_str_hex
	mov		bh, 0
	mov		bl, 48
	mov		si, buf
	call	print_string	
	
	; set cursor for hero
	mov		dh, [row]
	mov		dl, [col]
	call	set_cursor
	
	; print hero
	mov		al, '&'
	call	print_char
	
kbd:	
	call	readkey
	mov		dl, al
	
	mov		al, '.'
	call	print_char
	
	mov		al, dl

	cmp		al, 27
	je		exit
	
	cmp		al, 'q'
	je		exit
	
	cmp		al, 'a'
	jne		kbd_00
	dec		byte [col]
	
kbd_00:
	cmp		al, 'd'
	jne		kbd_01
	inc		byte [col]
	
kbd_01:
	cmp		al, 'w'
	jne		kbd_02
	dec		byte [row]
	
kbd_02:
	cmp		al, 's'
	jne		cont
	inc		byte [row]
	
cont:
	; set cursor for check
	mov		dh, [row]
	mov		dl, [col]
	call	set_cursor

	call	read_screen_char
	cmp		al, '*'
	jne		noscore
	add		word [score], 10
noscore:
	cmp		al, '#'
	je		exit
	jmp		main

exit:
	mov		si, exit_msg
	mov		bh, 20
	mov		bl, 36
	call	print_string
	call	readkey
	
	int		0x20
	hlt
	

message  	db	'Добрый день. Это работает!', 0
message_len	db	$ - message - 1
exit_msg	db	'Это конец.', 0
row			db	12
col			db	39
attr		db	2
score_text	db	'Очки: ', 0
score		dw	0
buf			db	8 dup (' '), 0
rn			dw	42


prep_field:
		mov		si, [rn]
		
		mov		di, 30
	
	prep_field_00:
		mov		ax, si
		call	generateRandom
		mov		si, ax
		mov		bl, 80
		div		bl
		mov		dl, ah
		mov		ax, si
		call	generateRandom
		mov		si, ax
		mov		bl, 25
		div		bl
		mov		dh, ah
	
		call	set_cursor
		
		cmp		di, 10
		jg		prep_field_01
		mov		al, '#'
		jmp		prep_field_02
	prep_field_01:
		mov		al, '*'
	prep_field_02:
		call	print_char
		
		dec		di
		jnz		prep_field_00		
		
		mov		[rn], si

		ret


set_txt_80x25:
	mov		ax, 3
	int		0x10
	ret


clear_screen:
	mov		ax, 0x0700
	xor		di, di
	mov		cx, 8192
	
	rep		stosw

	ret


; AL -> ASCII code
readkey:
	mov		ah, 0
	int		0x16
	ret


; SI <- str addr
; BH <- row
; BL <- col
print_string:
		; calc position in video memory
		mov		al, bh
		mov		ah, 160		; 80 cols only?
		mul		ah
		xor		bh, bh
		shl		bx, 1
		add		ax, bx
		mov		di, ax		; BX <- offset it vmem

		mov		ah, [attr]	; attr: TODO
	print_string_00:
		lodsb
		test	al, al
		je		print_string_exit
		
		stosw
		jmp		print_string_00
		
	print_string_exit:
		ret


; AL <- char
; DI <- str addr
; CX <- length
fill_buf:
	push 	es
	mov		dx, ds
	mov		es,	dx
	rep 	stosb
	pop		es
	ret


; DH <- row
; DL <- col
; BREAKS AH, BH
set_cursor:
	mov		ah, 2
	xor		bh, bh
	int 	0x10
	ret


; BREAKS AH, CX
hide_cursor:
	mov		ah, 1
	mov		cx, 0x2607
	int		0x10
	ret


; AL <- char
; BREAKS AH, BH
print_char:
	mov		ah, 0x0a
	mov		bh, 0
	mov		cx, 1
	int		0x10
	ret
	

; AH -> color
; AL -> char
read_screen_char:
	mov		ah, 0x08
	xor		bx, bx
	int		0x10
	ret

; AX <- word
; BX <- radix
; DI <- buf
; DX <- buf size
word_to_str_base:
		add		di, dx
		dec		di
		mov		bp, dx
	
	word_to_str_div_loop:
		xor		dx, dx
		div		bx
		cmp		dl, 10
		jl		word_to_str_00
		add		dl, 7
		
	word_to_str_00:
		add		dl, '0'
		
		mov		[di], dl

		dec		di
		dec		bp

		test	ax, ax
		jnz		word_to_str_div_loop
	
		mov		dl, ' '
	word_to_str_01:	
		test	bp, bp
		jz		word_to_str_exit
		mov		[di], dl
		dec		di
		dec		bp
		jmp		word_to_str_01
		
	word_to_str_exit:
		
		ret


; AX <- word
; DI <- buf
; DX <- buf size
word_to_str:
	mov		bx, 10
	call	word_to_str_base
	ret


; AX <- word
; DI <- buf
; DX <- buf size
word_to_str_hex:
	mov		bx, 16
	call	word_to_str_base
	ret


; AX <- word
; DI <- buf
; DX <- buf size
word_to_str_bin:
	mov		bx, 2
	call	word_to_str_base
	ret


; Function to generate a pseudo-random number
generateRandom:
    ; Simple linear congruential generator (LCG)
    ; X(n+1) = (a * X(n) + c) mod m

    ; Constants for the LCG
    ;mov ax, seed     ; Load the seed into AX
    mov cl, 17       ; Multiplier (a)
    mov bp, 5        ; Increment (c)
    mov bx, 32767     ; Modulus (m)

    ; Calculate the new random number
    mul cl		      ; AX = AX * CX
    add ax, bp        ; AX = AX + BP
    and ax, bx        ; AX = AX AND BX (modulus operation)

    ret
