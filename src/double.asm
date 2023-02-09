
; ****************************************************************************
; DOUBLE

!if ENABLE_DOUBLE {

; ****************************************************************************
; 2CONSTANT
; (???)
; ANSI 8.6.1.0360

; ****************************************************************************
; 2LITERAL
; (???)
; ANSI 8.6.1.0390

; ****************************************************************************
; 2VARIABLE
; (???)
; ANSI 8.6.1.0440

; ****************************************************************************
; D+
; (d_1 d_2 -- d_3)
; ANSI 8.6.1.1040

; FIG:
;
;
;      D+            d1  d2  ---  dsum
;               Leave the double number sum of two double numbers.
;
;;
;;                                       D+
;;                                       SCREEN 29 LINE 4
        +WORD "d+"
W_DPLUS
        !word *+2
;          CLC
;          LDA 2,X
;          ADC 6,X
;          STA 6,X
;          LDA 3,X
;          ADC 7,X
;          STA 7,X
;          LDA 0,X
;          ADC 4,X
;          STA 4,X
;          LDA 1,X
;          ADC 5,X
;          STA 5,X
        jmp POPTWO

; ****************************************************************************
; D-
; (d_1 d_2 -- d_3)
; ANSI 8.6.1.1050

; FIG:
;;
;;                                       DMINUS
;;                                       SCREEN 29 LINE 12
        +WORD "d-"
W_DMINUS
;:    !word *+2
;          SEC
;          TYA
;          SBC 2,X
;          STA 2,X
;          TYA
;          SBC 3,X
;          STA 3,X
;          JMP MINUS+3

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
        +WORD "d."
W_DDOT
        !word DO_COLON
;          !word ZERO
;          !word DDOTR
;          !word SPACE
        !word W_SEMI

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
        +WORD "d.r"
W_DDOTR
        !word DO_COLON
;          !word TOR
;          !word SWAP
;          !word OVER
;          !word DABS
;          !word BDIGS
;          !word DIGS
;          !word SIGN
;          !word EDIGS
;          !word RFROM
;          !word OVER
;          !word SUB
;          !word SPACS
;          !word TYPE
        !word W_SEMI

; ****************************************************************************
; D0<
; (d -- flag)
; ANSI 8.6.1.1075

; ****************************************************************************
; D0=
; (xd -- flag)
; 8.6.1.1080

; ****************************************************************************
; D2*
; (xd_1 -- xd_2)
; ANSI 8.6.1.1090

; ****************************************************************************
; D2/
; (xd_1 -- xd_2)
; ANSI 8.6.1.1100

; ****************************************************************************
; D<
; (d_1 d_2 -- flag)
; ANSI 8.6.1.1110

; ****************************************************************************
; D=
; (xd_1 xd_2 -- flag)
; ANSI 8.6.1.1120

; ****************************************************************************
; D>S
; (d -- n)
; ANSI 8.6.1.1140

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
        +WORD "dabs"
W_DABS
        !word DO_COLON
;          !word DUP
;          !word DPM
        !word W_SEMI

; ****************************************************************************
; DMAX
; (d_1 d_2 -- d_3)
; ANSI 8.6.1.1210

; ****************************************************************************
; DMIN
; (d_1 d_2 -- d_3)
; ANSI 8.6.1.1220

; ****************************************************************************
; DNEGATE
; (d_1 -- d_2)
; ANSI 8.6.1.1230

; ****************************************************************************
; M*/
; (d_1 n_1 +n_2 -- d_2)
; ANSI 8.6.1.1820

; ****************************************************************************
; M+
; (d_1 n -- d_2)
; ANSI 8.6.1.1830

}
