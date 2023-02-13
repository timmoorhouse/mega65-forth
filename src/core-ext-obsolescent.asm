
; ****************************************************************************
; #TIB
; (-- a-addr)
; ANSI 6.2.0060 - marked as obsolescent (see SOURCE in core)
; Not in Forth 2012

!if ENABLE_CORE_EXT_OBSOLESCENT {
}

; ****************************************************************************
; CONVERT
; (ud_1 c-addr_1 -- ud_2 c-addr_2) - marked as obsolescent (see >NUMBER)
; ANSI 6.2.0970
; Not in Forth 2012

!if ENABLE_CORE_EXT_OBSOLESCENT {
}

; ****************************************************************************
; EXPECT
; (c-addr +n --) - marked as obsolescent, superseded by ACCEPT in core
; ANSI 6.2.1390
; Not in Forth 2012?

; FIG:
;
;
;      EXPECT        addr  count  ---                        L0
;               Transfer characters from the terminal to address, until a 
;               "return" or the count of characters have been received.  
;               One or more nulls are added at the end of the text.
;
; TODO reimplement on top of ACCEPT

!if ENABLE_CORE_EXT_OBSOLESCENT {

        +WORD "expect"
W_EXPECT
        !word DO_COLON
!if 1 {
        !word W_ACCEPT
        ; TODO store in SPAN?
        !word W_DROP
} else {        
;          !word OVER
;          !word PLUS
;          !word OVER
;          !word PDO


;L1736:    !word KEY


; Check for and handle backspace

;          !word DUP
;          !word CLITERAL
;          !byte $E
;          !word PORIGIN ; ORIG + $E -> backspace char ($08) ?
;          !word AT
;          !word EQUAL
;          !word ZBRANCH
;L1744:    !word $1F       ; L1760-L1744
;          !word DROP
;          !word CLITERAL
;          !byte 08
;          !word OVER
;          !word I
;          !word EQUAL
;          !word DUP
;          !word RFROM
;          !word TWO
;          !word SUB
;          !word PLUS
;          !word TOR
;          !word SUB
;          !word BRANCH
;L1759:    !word $27       ; L1779-L1759


; Check for and handle linefeed

;L1760:    !word DUP
;          !word CLITERAL
;          !byte $0A	
;          !word EQUAL
;          !word ZBRANCH
;L1765:    !word $0E       ; L1772-L1765
;          !word LEAVE
;          !word DROP
;          !word BL
;          !word ZERO
;          !word BRANCH
;L1771:    !word 04        ; L1773-L1771


;L1772:    !word DUP
;L1773:    !word I
;          !word CSTOR
;          !word ZERO
;          !word I
;          !word 1PLUS
;          !word STORE


;L1779:    !word EMIT
;          !word PLOOP
;L1781:    !word $FFA9
;          !word DROP      ; L1736-L1781
}
        !word W_PSEMI
}

; ****************************************************************************
; QUERY
; (--)
; ANSI 6.2.2040 - marked as obsolescent (see ACCEPT in core, EVALUTATE in core)
; Not in Forth 2012

; FIG:
;
;      QUERY
;               Input 80 characters of text (or until a "return") from the 
;               operators terminal.  Text is positioned at the address 
;               contained in TIB with IN set to zero.

!if ENABLE_CORE_EXT_OBSOLESCENT {
        +WORD "query"
W_QUERY
        !word DO_COLON
        !word W_TIB
        !word W_AT
        !word W_CLITERAL
        !byte 80        ; 80 characters from terminal
        !word W_EXPECT
        !word W_ZERO
        !word W_IN 
        !word W_STORE
        !word W_PSEMI
}

; ****************************************************************************
; SPAN
; (-- a-addr)
; ANSI 6.2.2240 - marked as obsolescent (see ACCEPT)
; Not in Forth 2012

!if ENABLE_CORE_EXT_OBSOLESCENT {
}

; ****************************************************************************
; TIB
; (-- c-addr) 
; ANSI 6.2.2290 - marked as obsolescent (see SOURCE)
; Not in Forth 2012?

; FIG:
;
;      TIB           ---  addr                               U
;               A user variable containing the address of the terminal 
;               input buffer.

; TODO this returns the address of a pointer to the TIB !!!!!!!!!

!if ENABLE_CORE_EXT_OBSOLESCENT {
        +WORD "tib"
W_TIB
        !word DO_USER
        !byte U_TIB
}

; ****************************************************************************
; [COMPILE]
; ("text" --)
; ANSI 6.2.2530
; Forth 2012 - marked as obsolescent

; FIG:
;      [COMPILE]                                             P,C
;               Used in a colon-definition in the form:
;                         : xxx   [COMPILE]   FORTH  ;
;               [COMPILE] will force the compilation of an immediate 
;               definition, that would otherwise execute during 
;               compilation.  The above example will select the FORTH 
;               vocabulary when xxx executes, rather than at compile time.
;
;;
;;                                       [COMPILE]
;;                                       SCREEN 51 LINE 2

!if ENABLE_CORE_EXT_OBSOLESCENT {
!if 0 {
        +WORD_IMM "[compile]"
W_BCOMPILE
        !word DO_COLON
;          !word DFIND
;          !word ZEQU
;          !word ZERO
;          !word QERR
;          !word DROP
;          !word CFA
;          !word COMMA
        !word W_PSEMI
}
}