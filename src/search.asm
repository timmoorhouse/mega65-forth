
; ****************************************************************************
; SEARCH

!if ENABLE_SEARCH {

; ****************************************************************************
; DEFINITIONS
; (--)
; ANSI 16.6.1.1180

; FIG:
;
;      DEFINITIONS                                           L1
;               Used in the form:
;                         cccc  DEFINITIONS
;               Set the CURRENT vocabulary to the CONTEXT vocabulary.  In 
;               the example, executing vocabulary name cccc made it the 
;               CONTEXT vocabulary and executing DEFINITIONS made both 
;               specify vocabulary cccc.
;
;;
;;                                       DEFINITIONS
;;                                       SCREEN 53 LINE 11
!if 0 {
        +WORD "definitions"
W_DEFINITIONS
        !word DO_COLON
;          !word CON
;          !word AT
;          !word CURR
;          !word STORE
        !word W_SEMIS
}

; ****************************************************************************
; FIND
; (c-addr -- c-addr 0 | xt 1 | xt -1)
; ANSI 16.6.1.1550

; ****************************************************************************
; FORTH-WORDLIST
; (-- wid)
; 16.6.1.1595

; ****************************************************************************
; GET-CURRENT
; (-- wid)
; ANSI 16.6.1.1643

;;
;;                                       CURRENT
;;                                       SCREEN 37 LINE 3
;;
!if 0 {
;        +WORD "current"
W_CURRENT
        !word DO_USER
;          !byte $22
}

; ****************************************************************************
; GET-ORDER
; (-- wid_n ... wid_1 n)
; ANSI 16.6.1.1647

; ****************************************************************************
; SEARCH-WORDLIST
; (c-addr u wid -- 0 | xt 1 | xt -1)
; ANSI 16.6.1.2192

; ****************************************************************************
; SET-CURRENT
; (wid --)
; ANSI 16.6.1.2195

; ****************************************************************************
; SET-ORDER
; (wid_n ... wid_1 n --)
; ANSI 16.6.1.2197

; ****************************************************************************
; WORDLIST
; (-- wid)
; ANSI 16.6.1.2460

;      VOCABULARY                                            E,L
;               A defining word used in the form:
;                         VOCABULARY  cccc
;               to create a vocabulary definition cccc.  Subsequent use of 
;               cccc will make it the CONTEXT vocabulary which is searched 
;               first by INTERPRET.  The sequence "cccc DEFINITIONS" will 
;               also make cccc the CURRENT vocabulary into which new 
;               definitions are placed.
;
;               In fig-FORTH, cccc will be so chained as to include all 
;               definitions of the vocabulary in which cccc is itself 
;               defined.  All vocabularies ultimately chain to Forth.  By 
;               convention, vocabulary names are to be declared IMMEDIATE.  
;               See VOC-LINK.

;;
;;                                       VOCABULARY
;;                                       SCREEN 53 LINE 4
;;
!if 0 {
        +WORD "vocabulary"
W_VOCABULARY
        !word DO_COLON
;          !word BUILD
;          !word LIT,$A081
;          !word COMMA
;          !word CURR
;          !word AT
;          !word CFA
;          !word COMMA
;          !word HERE
;          !word VOCL
;          !word AT
;          !word COMMA
;          !word VOCL
;          !word STORE
;          !word DOES
;DOVOC:    !word TWOP
;          !word CON
;          !word STORE
        !word W_SEMI
}

}
