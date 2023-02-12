
; ****************************************************************************
; DOUBLE

!if ENABLE_DOUBLE {
}

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

; FIG:
;
;
;      D+            d1  d2  ---  dsum
;               Leave the double number sum of two double numbers.

; This is required by >number (core) but will only be visible if DOUBLE is enabled

!if ENABLE_DOUBLE {
        +WORD "d+"
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
        jmp POPTWO

; ****************************************************************************
; D-
; (d_1 d_2 -- d_3)
; ANSI 8.6.1.1050

!if ENABLE_DOUBLE {
        +WORD "d-"
W_DMINUS
        !word *+2
        ; TODO
        jmp POPTWO
}

; ****************************************************************************
; D.
; (d --)
; ANSI 8.6.1.1060

; FIG:
;
;      D.            d  ---                                  L1
;               Print a signed double number from a 32 bit two's 
;               complement value.  The high-order 16 bits are most 
;               accessable on the stack.  Conversion is performed 
;               according to the current BASE.  A blank follows.
;               Pronounced D-dot.
;
;;
;;                                       D.
;;                                       SCREEN 76 LINE 5

; The word itself is required by the implmentation (of .) but is only visible if DOUBLE is enabled

!if ENABLE_DOUBLE {
        +WORD "d."
}
W_DDOT
        !word DO_COLON
        !word W_ZERO
        !word W_DDOTR
        !word W_SPACE
        !word W_PSEMI

; ****************************************************************************
; D.R
; (d n --)
; ANSI 8.6.1.1070

; FIG:
;
;      D.R           d  n  ---
;               Print a signed double number d right aligned in a field n 
;               characters wide.
;
;;
;;                                       D.R
;;                                       SCREEN 76 LINE 1

; The word itself is required by the implmentation (of .) but is only visible if DOUBLE is enabled

!if ENABLE_DOUBLE {
        +WORD "d.r"
}
W_DDOTR
        !word DO_COLON
        !word W_TOR
        !word W_SWAP
        !word W_OVER
        !word W_DABS
        ; !word W_BDIGS ; core
        ; !word W_DIGS  ; core
        ; !word W_SIGN  ; core
        ; !word W_EDIGS ; core
        !word W_RFROM
        !word W_OVER
        !word W_SUB
        !word W_SPACES
        !word W_TYPE
        !word W_PSEMI

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
; DABS
; (d -- ud)
; ANSI 8.6.1.1160

; FIG:
;
;      DABS          d  ---  ud
;               Leave the absolute value ud of a double number.
;
;;
;;                                       DABS
;;                                       SCREEN 56 LINE 10

; The word itself is required by the implmentation (of .) but is only visible if DOUBLE is enabled

!if ENABLE_DOUBLE {
        +WORD "dabs"
}
W_DABS
        !word DO_COLON
        !word W_DUP
        !word W_ZLESS
        +ZBRANCH +
        !word W_DNEGATE
+       !word W_PSEMI

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

; The word itself is required by . (core) but is only visible if DOUBLE is enabled

!if ENABLE_DOUBLE {
        +WORD "dnegate"
}
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
