
; ****************************************************************************
; DOUBLE EXT

; ****************************************************************************
; 2ROT
; (x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2)

!if ENABLE_DOUBLE_EXT {
}

; ****************************************************************************
; 2VALUE

; See reference implementation

!if ENABLE_DOUBLE_EXT {
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
