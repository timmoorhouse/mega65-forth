
; ****************************************************************************
; SEARCH

; ****************************************************************************
; DEFINITIONS
; (--)
; ANSI 16.6.1.1180

; See reference implementation

; FIG:
;
;      DEFINITIONS                                           L1
;               Used in the form:
;                         cccc  DEFINITIONS
;               Set the CURRENT vocabulary to the CONTEXT vocabulary.  In 
;               the example, executing vocabulary name cccc made it the 
;               CONTEXT vocabulary and executing DEFINITIONS made both 
;               specify vocabulary cccc.

!if ENABLE_SEARCH {
!if 0 {
        +WORD "definitions", 0
W_DEFINITIONS
        !word DO_COLON
;          !word CON
;          !word AT
;          !word CURR
;          !word STORE
        !word W_PSEMIS
}
}

; ****************************************************************************
; FORTH-WORDLIST
; (-- wid)
; 16.6.1.1595

; Required by lots of things currently

        +WORD "forth-wordlist", 0
W_FORTH_WORDLIST
        !word DO_VARIABLE
FORTH_WORDLIST
        !word 0        

; ****************************************************************************
; GET-CURRENT
; (-- wid)
; ANSI 16.6.1.1643

;!if ENABLE_SEARCH {
!if 1 {
         +WORD "get-current", 0
W_GET_CURRENT
!if 0 {
        !word DO_USER
;          !byte $22
} else {
        !word DO_COLON    ; TODO change this to a variable
        !word W_FORTH_WORDLIST
        !word W_PSEMI
}
}
;}

; ****************************************************************************
; GET-ORDER
; (-- wid_n ... wid_1 n)
; ANSI 16.6.1.1647

; See reference implementation

!if ENABLE_SEARCH {
}

; ****************************************************************************
; SEARCH-WORDLIST
; (c-addr u wid -- 0 | xt 1 | xt -1)
; ANSI 16.6.1.2192

; 0 if not found
; 1 if immediate
; -1 otherwise

; The word itself is required by the implementation (of FIND) but is only visible if SEARCH is enabled

!if ENABLE_SEARCH {
        +WORD "search-wordlist", 0
} else {
        +NONAME
}
W_SEARCH_WORDLIST
        !word DO_COLON

        !word W_FIND_NAME_IN

        ; (0 | nt)

        !word W_DUP
        +ZBRANCH ++

        ; found
        !word W_DUP
        !word W_NAME_TO_INTERPRET
        !word W_SWAP
        !word W_QIMMEDIATE

        +ZBRANCH +
        !word W_ONE
        !word W_PSEMI       
+
        !word W_TRUE ; -1
++
        !word W_PSEMI

; ****************************************************************************
; SET-CURRENT
; (wid --)
; ANSI 16.6.1.2195

!if ENABLE_SEARCH {
}

; ****************************************************************************
; SET-ORDER
; (wid_n ... wid_1 n --)
; ANSI 16.6.1.2197

; See reference implementation

!if ENABLE_SEARCH {
}

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

!if ENABLE_SEARCH {
!if 0 {
        +WORD "vocabulary", 0
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
        !word W_PSEMI
}
}
