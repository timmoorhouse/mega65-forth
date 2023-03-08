
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
        !word DO_CONSTANT
        !word FORTH_WORDLIST

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
; SET-ORDER
; (wid_n ... wid_1 n --)
; ANSI 16.6.1.2197

; See reference implementation

!if ENABLE_SEARCH {
}

; ****************************************************************************
; WORDLIST
; (-- wid)

        +WORD "wordlist", 0
W_WORDLIST
        !word DO_COLON

        +LITERAL WORDLIST_TABLE + 2*WORDLIST_TABLE_LEN
        +LITERAL WORDLIST_TABLE
        +DO _wordlist_after_loop

_wordlist_loop

        !word W_I
        !word W_AT
        !word W_TRUE            ; -1
        !word W_EQUAL
        +ZBRANCH +
        ; found an unused one
        !word W_I
        !word W_ZERO
        !word W_OVER
        !word W_STORE           ; set entry to 0 to mark as allocated
        !word W_UNLOOP
        !word W_EXIT
+
        !word W_TWO
        !word W_PPLOOP
        !word _wordlist_loop-*
_wordlist_after_loop

        +LITERAL E_WORDLIST_NOT_AVAILABLE
        !word W_THROW

        !word W_PSEMI
