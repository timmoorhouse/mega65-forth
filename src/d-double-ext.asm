
; ****************************************************************************
; DOUBLE EXT

; ****************************************************************************
; 2ROT
; (x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2)

!if ENABLE_DOUBLE_EXT {
        +CREATE "2rot", 0
W_2ROT
        !word *+2
        ldy 0,x
        lda 8,x
        sta 0,x
        lda 4,x
        sta 8,x
        sty 4,x
        ldy 2,x
        lda 10,x
        sta 2,x
        lda 6,x
        sta 10,x
        sty 6,x
        ldy 1,x
        lda 9,x
        sta 1,x
        lda 5,x
        sta 9,x
        sty 5,x
        ldy 3,x
        lda 11,x
        sta 3,x
        lda 7,x
        sta 11,x
        sty 7,x
        jmp NEXT
}

; ****************************************************************************
; DU<
; (ud1 ud2 -- flag)

!if ENABLE_DOUBLE_EXT {
        +CREATE "du<", 0
W_DULESS
        !word *+2
        lda 5,x
        cmp 1,x
        bne +
        lda 4,x
        cmp 0,x
        bne +
        lda 7,x
        cmp 3,x
        bne +
        lda 6,x
        cmp 2,x
+       bcs +
        dey
+       sty 6,x
        sty 7,x
        jmp POP3    
}
