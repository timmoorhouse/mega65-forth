
; ****************************************************************************
; SEARCH EXT

!if ENABLE_SEARCH_EXT {

; ****************************************************************************
; ALSO
; (--)
; ANSI 16.6.2.0715

; See reference implementation

; ****************************************************************************
; FORTH
; (--)
; ANSI 16.6.2.1590

; See reference implementation

; FIG:
;
;      FORTH                                                 P,L1
;               The name of the primary vocabulary.  Execution makes FORTH 
;               the CONTEXT vocabulary.  Until additional user 
;               vocabularies are defined, new user definitions become a 
;               part of FORTH.  FORTH is immediate, so it will execute 
;               during the creation of a colon-definition, to select this 
;               vocabulary at compile-time.
;
;;
;;                                       FORTH
;;                                       SCREEN 53 LINE 9
        +WORD_IMM "forth"
W_FORTH
;      !word DODOE
;          !word DOVOC
;          !word $A081
;XFOR :    !word NTOP     ; points to top name in FORTH
;VL0  :    !word 0        ; last vocab link ends at zero

; ****************************************************************************
; ONLY
; (--)
; ANSI 16.6.2.1965

; See reference implementation

; ****************************************************************************
; ORDER
; (--)
; ANSI 16.6.2.1985

; ****************************************************************************
; PREVIOUS
; (--)
; ANSI 16.6.2.2037

; See reference implementation

}
