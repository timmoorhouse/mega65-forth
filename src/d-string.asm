
; ****************************************************************************
; STRING

; ****************************************************************************
; -TRAILING
; (c-addr u_1 -- c-addr u_2)
; ANSI 17.6.1.0170

; FIG:
;
;
;      -TRAILING     addr  n1  ---  addr  n2
;               Adjusts the character count n1 of a text string beginning 
;               address to suppress the output of trailing blanks.  ie. 
;               the characters at addr+n2 are blanks.
;
;;
;;                                       -TRAILING
;;                                       SCREEN 44 LINE 5

!if ENABLE_STRING {
        +WORD "-trailing", 0
W_DTRAILING
        !word DO_COLON
;          !word DUP
;          !word ZERO
;          !word PDO
;L1663:    !word OVER
;          !word OVER
;          !word PLUS
;          !word ONE
;          !word SUB
;          !word CAT
;          !word BL
;          !word SUB
;          !word ZBRAN
;L1672:    !word 8        ; L1676-L1672
;          !word LEAVE
;          !word BRAN
;L1675:    !word 6        ; L1678-L1675
;L1676:    !word ONE
;          !word SUB
;L1678:    !word PLOOP
;L1679:    !word $FFE0    ; L1663-L1679
        !word W_PSEMI
}

; ****************************************************************************
; /STRING
; (c-addr_1 u_1 n -- c-addr_2 u_2)
; ANSI 17.6.1.0245

!if ENABLE_STRING {
}

; ****************************************************************************
; CMOVE
; (c-addr_1 c-addr_2 u --)
; ANSI 17.6.1.0910

; we need this for CREATE
!if ENABLE_STRING {
        +WORD "cmove", 0
} else {
        +NONAME
}
W_CMOVE
        !word *+2

        lda 0,x
        ora 1,x
        beq +

        lda 0,x
        sta _cmove_count
        lda 1,x
        sta _cmove_count+1

        lda 2,x
        sta _cmove_dst
        lda 3,x
        sta _cmove_dst+1

        lda 4,x
        sta _cmove_src
        lda 5,x
        sta _cmove_src+1

        +dma_inline
        !byte $0b               ; F018B 12-byte format ; TODO 11-byte?   
        +dma_options_end
        !byte dma_cmd_copy
_cmove_count
        !word 0
_cmove_src
        !word 0
        !byte 0                 ; src bank/flags
_cmove_dst
        !word 0
        !byte 0                 ; dst bank/flags
        !byte 0                 ; cmd msb
        !word 0                 ; modulo
+       jmp POP3

; ****************************************************************************
; CMOVE>
; (c-addr_1 c-addr_2 u --)
; ANSI 17.6.1.0920

!if ENABLE_STRING {
        +WORD "cmove>", 0
W_CMOVEG
        !word *+2

        lda 0,x
        ora 1,x
        beq ++

        ; TODO add len - 1 to src, dst
        lda 0,x
        sta _cmoveg_count
        lda 1,x
        sta _cmoveg_count+1

        ; decrement len
        sec
        lda 0,x
        sbc #1
        sta 0,x
        lda 1,x
        sbc #0
        sta 1,x

        ; add (len-1) to dst
        clc
        lda 0,x
        adc 2,x
        sta _cmoveg_dst
        lda 1,x
        adc 3,x
        sta _cmoveg_dst+1

        ; add (len-1) to src
        clc
        lda 0,x
        adc 4,x
        sta _cmoveg_src
        lda 1,x
        adc 5,x
        sta _cmoveg_src+1

        +dma_inline
        !byte $0b               ; F018B 12-byte format        
        +dma_options_end
        !byte dma_cmd_copy | $30 ; dec dst, dec src
_cmoveg_count
        !word 0
_cmoveg_src
        !word 0
        !byte 0 ; $40               ; src bank/flags - backwards
_cmoveg_dst
        !word 0
        !byte 0 ; $40               ; dst bank/flags - backwards
        !byte 0                 ; cmd msb
        !word 0                 ; modulo        
++      jmp POP3
}

; ****************************************************************************
; COMPARE
; (c-addr_1 u_1 c-addr_2 u_2 -- n)
; ANSI 17.6.1.0935

; 0 if identical

; if identical up to shortest length:
; -1 if u_1 < u_2
; 1 otherwise

; if not identical up to shortest length:
; -1 if char from str 1 < char from str 2
; 1 otherwise

; The word itself is required by the implementation (of FIND) but is only visible if SEARCH is enabled

!if ENABLE_STRING {
        +WORD "compare", 0
} else {
        +NONAME
}
W_COMPARE
        !word *+2

        ; ldy #0 ; TODO

        ; TEMP1 - pointer into string 1
        ; TEMP2 - pointer into string 2
        ; TEMP3 - index

        ; TODO this is pretty klunky, but should work ...

        lda 6,x
        sta <TEMP1
        lda 7,x
        sta <TEMP1+1

        lda 2,x
        sta <TEMP2
        lda 3,x
        sta <TEMP2+1

        tya
        sta <TEMP3
        sta <TEMP3+1

_compare_loop
        ; while TEMP3 < u1 && TEMP3 < u2

!if 0 {
        lda #'c'
        jsr put_char

!if 0 {
        lda #' '
        jsr put_char
        lda <TEMP3+1
        jsr put_hex
        lda <TEMP3
        jsr put_hex

        lda #' '
        jsr put_char
        lda 5,x
        jsr put_hex
        lda 4,x
        jsr put_hex

        lda #' '
        jsr put_char
}

        ldy #0
}

        sec
        lda <TEMP3
        sbc 4,x
        lda <TEMP3+1
        sbc 5,x
        bcs _compare_end_loop ; TODO

!if 0 {
        lda #'d'
        jsr put_char
        ldy #0
}

        sec
        lda <TEMP3
        sbc 0,x
        lda <TEMP3+1
        sbc 1,x
        bcs _compare_end_loop ; TODO

        lda (<TEMP1),y
        cmp (<TEMP2),y
        beq +
        bcs _compare_greater
        jmp _compare_less
+

        inc <TEMP1
        bne +
        inc <TEMP1+1
+
        inc <TEMP2
        bne +
        inc <TEMP2+1
+
        inc <TEMP3
        bne +
        inc <TEMP3+1
+

        jmp _compare_loop
_compare_end_loop

        sec
        lda 0,x
        sbc 4,x
        lda 1,x
        sbc 5,x
        bcs _compare_lequal ; u2 >= u1

_compare_greater
        ; u1 > u2 ... return 1
        tya
        iny
        phy
        jmp +

_compare_lequal
        sec
        lda 4,x
        sbc 0,x
        lda 5,x
        sbc 1,x
        bcs _compare_equal

_compare_less
        ; u1 < u2 ... return -1
        dey
        phy
        tya
        jmp +

_compare_equal
        ; return 0
        phy
        tya
+

        inx
        inx
        inx
        inx
        inx
        inx
        jmp PUT

; ****************************************************************************
; SEARCH
; (c-addr_1 u_1 c-addr_2 u_2 -- c-addr_3 u_3 flag)
; ANSI 17.6.1.2191

!if ENABLE_STRING {
}
