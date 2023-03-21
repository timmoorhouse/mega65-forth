
; ****************************************************************************
; DOUBLE

; ****************************************************************************
; 2CONSTANT
; (???)

; TODO could move this to forth

!if ENABLE_DOUBLE {
        +CREATE "2constant", 0
W_2CONSTANT
        !word DO_COLON
        !word W_CREATE
        !word W_SWAP
        !word W_COMMA
        !word W_COMMA
        !word W_PSCODE
}
DO_2CONSTANT
        dex
        dex
        ldy #2
        lda (<W),y
        sta 0,x
        iny
        lda (<W),y
        sta 1,x
        iny
        lda (<W),y
        pha
        iny
        lda (<W),y
        jmp PUSH

; ****************************************************************************
; 2LITERAL
; (???)
; ANSI 8.6.1.0390

!if ENABLE_DOUBLE {
}

; FIG

;      DLITERAL      d  ---  d           (executing)
;                    d  ---              (compiling)         P
;               If compiling, compile a stack double number into a 
;               literal.  Later execution of the definition containing 
;               the literal will push it to the stack.  If executing, the 
;               number will remain on the stack.

!if 0 {
        +CREATE "dliteral", 0
W_DLITERAL
        !word DO_COLON
;          !word STATE
;          !word AT
;          !word ZBRAN
;L2238:    !word 8        ; L2242-L2238
;          !word SWAP
;          !word LITER
;          !word LITER
        !word W_PSEMI
}

; ****************************************************************************
; 2VARIABLE
; (???)

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; D+
; (d1 d2 -- d3)

; This is required by >number (core) but will only be visible if DOUBLE is enabled

!if ENABLE_DOUBLE {
        +CREATE "d+", 0
} else {
        +NONAME
}
W_DPLUS
        !word *+2
        clc
        lda 2,x
        adc 6,x
        sta 6,x
        lda 3,x
        adc 7,x
        sta 7,x
        lda 0,x
        adc 4,x
        sta 4,x
        lda 1,x
        adc 5,x
        sta 5,x
        jmp POP2

; ****************************************************************************
; D-
; (d1 d2 -- d3)

!if ENABLE_DOUBLE {
        +CREATE "d-", 0
W_DSUB
        !word *+2
        sec
        lda 6,x
        sbc 2,x
        sta 6,x
        lda 7,x
        sbc 3,x
        sta 7,x
        lda 4,x
        sbc 0,x
        sta 4,x
        lda 5,x
        sbc 1,x
        sta 5,x
        jmp POP2
}

; ****************************************************************************
; D2*
; (xd1 -- xd2)

!if ENABLE_DOUBLE {
        +CREATE "d2*", 0
W_D2STAR
        !word *+2
        asl 2,x
        rol 3,x
        rol 0,x
        rol 1,x
        jmp NEXT        
}

; ****************************************************************************
; D2/
; (xd1 -- xd2)

!if ENABLE_DOUBLE {
        +CREATE "d2/", 0
W_D2SLASH
        !word *+2
        asr 1,x
        ror 0,x
        ror 3,x
        ror 2,x
        jmp NEXT        
}

; ****************************************************************************
; D<
; (d1 d2 -- flag)

!if ENABLE_DOUBLE {
        +CREATE "d<", 0
W_DLESS
        !word *+2
        ; ldy #0 ; TODO
        sec
        lda 6,x
        sbc 2,x
        lda 7,x
        sbc 3,x
        lda 4,x
        sbc 0,x
        lda 5,x
        sbc 1,x

        bvc +
        eor #$80
+       bpl +
        dey
+       sty 6,x
        sty 7,x        
        jmp POP3
}

; ****************************************************************************
; DNEGATE
; (d1 -- d2)

; Required by the implementation of . (core)

        +CREATE "dnegate", 0
W_DNEGATE
        !word *+2
        ; ldy #0 ; TODO
        sec
        tya
        sbc 2,x
        sta 2,x
        tya
        sbc 3,x
        sta 3,x
        ; see also NEGATE (core)
        tya
        sbc 0,x
        sta 0,x
        tya
        sbc 1,x
        sta 1,x
        jmp NEXT

; ****************************************************************************
; M*/
; (d1 n1 +n2 -- d2)

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; M+
; (d1 n -- d2)

!if ENABLE_DOUBLE {
}
