; multi-segment executable file template.

data segment
    
    error db "Wystapil blad!$"
    error_l db "Zla dlugosc, prawidlowa dlugosc: 1-25. $"
    
    Y dw 0 ;punkty startu
    X dw 0               
    
    
    white db 31 ;nr koloru bialego
    black db 0 ;nr koloru czarnego
    current_color db 31
    
    
    buffor dw 24 dup('$')
    buffor_length dw ?
    
    
    bar_length db 200
    
    check_sum_char dw 1
    
    string_length db 11
    stop_length db 13
    
    
    start_codeB dw "11010010000"
    stop_code dw   "1100011101011"
    
    ;#######################################
    ;tablice kodow i offsety tablic
    
    off_numbers dw 16
    off_special2 dw 26
    off_big_letters dw 33
    off_special3 dw 59
    off_small_letters dw 65
    off_special4 dw 91
    ;0-15
    special1 dw "11011001100","11001101100", "11001100110", "10010011000", "10010001100", "10001001100", "10011001000", "10011000100", "10001100100", "11001001000", "11001000100", "11000100100", "10110011100", "10011011100", "10011001110", "10111001100"
    ;16-25
    numbers dw "10011101100", "10011100110", "11001110010", "11001011100", "11001001110", "11011100100", "11001110100", "11101101110", "11101001100", "11100101100"
	;26-32
	special2 dw "11100100110", "11101100100", "11100110100", "11100110010", "11011011000", "11011000110", "11000110110"			
    ;33-58
    big_letters dw "10100011000", "10001011000", "10001000110", "10110001000", "10001101000", "10001100010", "11010001000", "11000101000", "11000100010", "10110111000", "10110001110", "10001101110", "10111011000", "10111000110", "10001110110", "11101110110", "11010001110", "11000101110", "11011101000", "11011100010",  "11011101110", "11101011000", "11101000110", "11100010110", "11101101000", "11101100010"
    ;59-64
    special3 dw "11100011010", "11101111010", "11001000010", "11110001010", "10100110000","10100001100"
    ;65-90 
    small_letters dw "10010110000","10010000110","10000101100","10000100110","10110010000","10110000100","10011010000","10011000010","10000110100","10000110010","11000010010","11001010000", "11110111010","11000010100","10001111010","10100111100","10010111100","10010011110","10111100100","10011110100","10011110010","11110100100","11110010100","11110010010","11011011110","11011110110"
    ;91-95
    special4 dw "11110110110", "10101111000", "10100011110", "10001011110", "10111101000", 

ends

stack segment 
    dw 200 dup(?)
	top dw ?
ends

code segment
    
;##########################################################################
;mamy do dyspoczycji rozdzielczosc 320x200 pikseli
;kazdy piksel reprezentowany jest przez jeden bajt
;licze ze wzoru numer komorki = y*320 + x

init_data proc
    mov ax, data
    mov ds, ax
ret
endp

init_screen proc ;wlaczanie trybu 13h
    mov al, 13h
    mov ah, 00h
    int 10h
ret
endp
close_screen proc ;wylaczenie trybu 13h do tekstowego 
    mov al, 03h
    mov ah, 00h
    int 10h
 
ret
endp    

error_screen proc
    call close_screen
    lea dx, error
    mov ah, 9
    int 21h
    jmp put_ESC
ret
endp

error_length proc
	call init_data
    call close_screen
    lea dx, error_l
    mov ah, 9
    int 21h
    jmp put_ESC
ret
endp

white_screen proc 
    mov ax, 0a000h
    mov es, ax
    mov di, 0
    mov al, white
    mov ah, white
    mov cx, 32000
    rep stosw ;stosw po kazdym przeslaniu z ax do es:di zwieksza di
	
ret
endp

xorxor proc
   xor ax, ax
   xor bx, bx
   xor cx, cx
   xor dx, dx
ret
endp

draw_line proc
    
    call xorxor
    
    mov cl, bar_length
    draw_pixel:
    
    push cx
    
    mov ax, Y ;wspolrzedna piksela Y
    mov bx, 320
    mul bx ; ax = Y*320
    add ax, X ; ax = Y*320 + X
    mov di, ax ; di = ax
    mov ax, 0a000h ; adres segemntu graficznego
    mov es, ax ; adres do rejestru es
    mov cl, current_color ;kolor lini
    mov es:di, cl ; zapalanie na ekranie
    
    inc Y ;przesuniecie w dol
    pop cx
    loop draw_pixel
    inc X ;przesuniecie w prawo
    mov byte ptr ds:[Y], 0 ;czyszczenie Y
ret
endp

draw_char_line proc                      
    xor ah, ah
    
    cmp al, '0'
    je draw_white
    
    draw_black:
    mov al, black
    mov byte ptr ds:[current_color], al
    
    call draw_line
    jmp finish
    
    draw_white:
    mov al, white
    mov byte ptr ds:[current_color], al
    call draw_line
    
    finish:
 
ret
endp


draw_quiet_zone proc
    mov al, white
    mov byte ptr ds:[current_color], al
    mov cx, 10
    draw_qz:
    push cx
    call draw_line
    pop cx
    loop draw_qz 
    
ret
endp    

check_sum_count proc
    
    call xorxor
    
    mov cx, buffor_length
    
    count_csum:
    mov ax, [buffor + bx]
    xor ah, ah
    sub al, 32 ; spacja ma numer 0
    push cx
    push bx
    
    ;wyznaczanie reszty z dzielenia przez 103
    
    inc bx
    mul bx ; wartosc razy pozycja
    
    cwd
    mov bx, 103
    div bx
    
    add [check_sum_char], dx
    mov ax, check_sum_char
    
    cmp check_sum_char, 103 ;porownaj sume reszt z dzielenia przez 103 ze 103
    jb go_on ; jezeli wieksze badz rowne 103 wykonaj operacje modulo
    
    modulo:
    
    call xorxor
    
    mov ax, check_sum_char
    cwd 
    mov bx, 103
    div bx
     
    mov word ptr ds:[check_sum_char], dx
    
    go_on:
    pop bx
    inc bx
    pop cx
    loop count_csum
        
ret
endp

find_array proc
    
    cmp al, 0
    jl error_screen
    
    cmp al, 16
    jb to_special1
    
    cmp al, 26
    jb to_numbers
    
    cmp al, 33
    jb to_special2
    
    cmp al, 59
    jb to_big_letters
    
    cmp al, 65
    jb to_special3
    
    cmp al, 91
    jb to_small_letters
    
    cmp al, 96
    jb to_special4
    jnbe error_screen
    
    
    to_special1:    ;znajdujemy miejsce poczatku lancucha odpowiadajacego danemu znakowi
    
    mov bx, 12      ;dlugosc lancucha
    imul bx
    
    mov bx, ax
    xor ah, ah
    xor cx, cx
    
    mov cl, string_length ;11 razy 
    
    
    special1_loop:
    
    push cx
    mov ax, [special1+bx]   ;idziemy kolejno po znakach z lancucha
    push bx
    
    call draw_char_line
    pop bx
    inc bx
    pop cx
    loop special1_loop
    jmp continue
    
    to_numbers:
    
    sub ax, off_numbers
    mov bx, 12
    imul bx
    
    mov bx, ax
    xor ah, ah
    xor cx, cx
    
    mov cl, string_length 
    
    
    numbers_loop:
    
    push cx
    mov ax, [numbers+bx]
    push bx
    
    call draw_char_line
    pop bx
    inc bx
    pop cx
    loop numbers_loop
    jmp continue
    
    
    to_special2:
    
    sub ax, off_special2
    mov bx, 12
    imul bx
    
    mov bx, ax
    xor ah, ah
    xor cx, cx
    
    mov cl, string_length 
    
    
    special2_loop:
    
    push cx
    mov ax, [special2+bx]
    push bx
    
    call draw_char_line
    pop bx
    inc bx
    pop cx
    loop special2_loop
    jmp continue
    
    to_big_letters:
    
    sub ax, off_big_letters
    mov bx, 12
    imul bx
    
    mov bx, ax
    xor ah, ah
    xor cx, cx
    
    mov cl, string_length 
    
    
    big_letters_loop:
    
    push cx
    mov ax, [big_letters+bx]
    push bx
    
    call draw_char_line
    pop bx
    inc bx
    pop cx
    loop big_letters_loop
    jmp continue
    
    to_special3:
    
    sub ax, off_special3
    
    mov bx, 12
    imul bx
    
    mov bx, ax
    xor ah, ah
    xor cx, cx
    
    mov cl, string_length 
    
    
    special3_loop:
    
    push cx
    mov ax, [special3+bx]
    push bx
    
    call draw_char_line
    pop bx
    inc bx
    pop cx
    loop special3_loop
    jmp continue
    
    to_small_letters:
    
    sub ax, off_small_letters
    
    mov bx, 12
    imul bx
    
    mov bx, ax
    xor ah, ah
    xor cx, cx
    
    mov cl, string_length 
    
    
    small_letters_loop:
    
    push cx
    mov ax, [small_letters+bx]
    push bx
    
    call draw_char_line
    pop bx
    inc bx
    pop cx
    loop small_letters_loop
    jmp continue
    
    to_special4:
    
    sub ax, off_special4
    
    mov bx, 12 ;przesuniecie o 12 znakow
    imul bx
    
    mov bx, ax
    xor ah, ah
    xor cx, cx
    
    mov cl, string_length 
    
    
    special4_loop:
    
    push cx
    mov ax, [special4+bx]
    push bx
    
    call draw_char_line
    pop bx
    inc bx
    pop cx
    loop special4_loop
    jmp continue
    
    continue:
    
ret
endp

start:
    
    ;#################################################
    ;inicjalizacja stosu
    mov ax, seg top
	mov ss, ax
	mov sp, offset top
	
	;#########################################################
	;pobieranie danych

	mov ax, data
	mov	es, ax
	mov	si, 082h
	mov	di, offset es:[buffor]
	xor	cx, cx 					
	mov cl, byte ptr ds:[080h] 	; w cx znajduje sie dlugosc buffora
	sub	cl, 1 ;trzeba go usunac
	mov	es:[buffor_length], cx
			
	cmp cl, 0
	ja check_length ;jezeli nie podano zadnego parametru to koniec programu
	
	call error_length
		
	check_length:
	cmp cl, 25
	jb	save_buffor ;jezeli dlugosc buffora jest dluzsza niz 25 znakow to koniec
	call error_length
			
	save_buffor: ;zapisujemy dane do buffora
	push cx
	mov al, byte ptr ds:[si]
	mov byte ptr es:[di], al
	inc si
	inc di
	pop cx
	loop save_buffor
	
	;inicjalizacja danych
	call init_data

    ;##########################################################################################
    ;wlaczenie okna i rysowanie kodu
    
    call init_screen 
    
    call white_screen
    
    call draw_quiet_zone
    
    ;##########################################################################################
    ;rysujemy znak startu
    
    call xorxor
    
    mov cl, string_length
     
    draw_start:
    mov ax, [start_codeB + bx] ;do al przenosimy kod danego znaku
    push bx
    push cx
    call draw_char_line
    pop cx
    pop bx
    inc bx
    loop draw_start
    
    ;##########################################################################################
    ;string po kolei
    
    call xorxor
    
    mov cx, buffor_length
    
    xor ah, ah
    
    take_one_char:
    
    mov ax, [buffor + bx]   ;pobieranie znaku
    push cx
    push bx
    
    xor ah,ah
    sub al, 32 ; zgodnie z kodowaniem
    
    call find_array
    
    pop bx
    inc bx
    pop cx
    loop take_one_char
   
    
    ;##########################################################################################
    ;suma kontrolna (rysowanie znaku jej odpowiadajacego)
    
    call check_sum_count
    call xorxor
    
    mov ax, check_sum_char
    xor ah, ah
    
    call find_array
    
    ;##########################################################################################
    ;rysujemy znak stopu
    call xorxor
    
    mov cl, stop_length
    draw_stop:
    mov ax, [stop_code + bx]
    push bx
    push cx
    call draw_char_line
    pop cx
    pop bx
    inc bx
    loop draw_stop
    
    call xorxor
    
    call draw_quiet_zone 
    
    ;#########################################################################################
    ;oczekiwanie na wcisniecie klawisza ESC
    
    put_ESC:
    
    mov ah, 8 ;oczekiwanie na klawisz, w al : kod pobranego znaku
    int 21h
    
    cmp al, 27
    jne put_ESC
 
    call close_screen
    
    mov ax, 4c00h ; wyjscie do systemu
    int 21h 
     
ends

end start ; set entry point and stop the assembler.
