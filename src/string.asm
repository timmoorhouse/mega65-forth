
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
        +WORD "-trailing"
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
; BLANK
; (c-addr u --)
; ANSI 17.6.1.0780

; FIG
;      BLANKS        addr  count  ---
;               Fill an area of memory begining at addr with blanks.
;

;;
;;                                       BLANKS
;;                                       SCREEN 46 LINE 7
;;

!if ENABLE_STRING {
!if 0 {
        +WORD "blanks"
W_BLANK
        !word DO_COLON
        !word W_BL
        !word W_FILL
        !word W_PSEMI
}
}

; ****************************************************************************
; CMOVE
; (c-addr_1 c-addr_2 u --)
; ANSI 17.6.1.0910

; FIG:
;
;      CMOVE         from  to  count  ---
;               Move the specified quantity of bytes beginning at address 
;               from to address to.  The contents of address from is moved 
;               first proceeding toward high memory.  Further 
;               specification is necessary on word addressing computers.
;
;;
;;                                       CMOVE
;;                                       SCREEN 22 LINE 1

!if ENABLE_STRING {
        +WORD "cmove"
W_CMOVE
        !word *+2
;          LDA #3
;          JSR SETUP
;L370:     CPY N
;          BNE L375
;          DEC N+1
;          BPL L375
;          JMP NEXT
;L375:     LDA (N+4),Y
;          STA (N+2),Y
;          INY
;          BNE L370
;          INC N+5
;          INC N+3
;          JMP L370
}

; ****************************************************************************
; CMOVE>
; (c-addr_1 c-addr_2 u --)
; ANSI 17.6.1.0920

!if ENABLE_STRING {
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

!if 0 {
W_COMPARE_TEST
        !word DO_COLON
        !word W_PDOTQ
        +STRING "<compare>["
        +LITERAL str1
        !word W_COUNT
        !word W_2DUP
        !word W_TYPE
        !word W_PDOTQ
        +STRING "]["
        +LITERAL str2
        !word W_COUNT
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT
        !word W_COMPARE
        !word W_DOTS
        !word W_PSEMI

str1    +STRING "foocar"
str2    +STRING "foobar"
}

!if ENABLE_STRING {
        +WORD "compare"
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

; ****************************************************************************
; SLITERAL
; (???)
; ANSI 17.6.1.2212

!if ENABLE_STRING {
}
