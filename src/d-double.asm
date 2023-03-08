
; ****************************************************************************
; DOUBLE

; ****************************************************************************
; 2CONSTANT
; (???)
; ANSI 8.6.1.0360

!if ENABLE_DOUBLE {
}

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
        +WORD "dliteral", 0
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
; ANSI 8.6.1.0440

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; D+
; (d_1 d_2 -- d_3)
; ANSI 8.6.1.1040

; This is required by >number (core) but will only be visible if DOUBLE is enabled

!if ENABLE_DOUBLE {
        +WORD "d+", 0
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
; (d_1 d_2 -- d_3)
; ANSI 8.6.1.1050

!if ENABLE_DOUBLE {
        +WORD "d-", 0
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
; D0<
; (d -- flag)
; ANSI 8.6.1.1075

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; D0=
; (xd -- flag)
; 8.6.1.1080

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; D2*
; (xd_1 -- xd_2)
; ANSI 8.6.1.1090

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; D2/
; (xd_1 -- xd_2)
; ANSI 8.6.1.1100

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; D<
; (d_1 d_2 -- flag)
; ANSI 8.6.1.1110

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; D=
; (xd_1 xd_2 -- flag)
; ANSI 8.6.1.1120

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; D>S
; (d -- n)
; ANSI 8.6.1.1140

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; DMAX
; (d_1 d_2 -- d_3)
; ANSI 8.6.1.1210

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; DMIN
; (d_1 d_2 -- d_3)
; ANSI 8.6.1.1220

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; DNEGATE
; (d_1 -- d_2)
; ANSI 8.6.1.1230

; Required by the implementation of . (core)

        +WORD "dnegate", 0
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
; (d_1 n_1 +n_2 -- d_2)
; ANSI 8.6.1.1820

!if ENABLE_DOUBLE {
}

; ****************************************************************************
; M+
; (d_1 n -- d_2)
; ANSI 8.6.1.1830


!if ENABLE_DOUBLE {
}
