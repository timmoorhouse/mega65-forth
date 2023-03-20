
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

!if ENABLE_CORE_EXT_OBSOLESCENT {
!if 0 {
        +CREATE_IMM "[compile]"
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