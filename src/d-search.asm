
; ****************************************************************************
; SEARCH

; ****************************************************************************
; FORTH-WORDLIST
; (-- wid)

; Required by lots of things currently

        +WORD "forth-wordlist", 0
W_FORTH_WORDLIST
        !word DO_CONSTANT
        !word FORTH_WORDLIST

; ****************************************************************************
; SEARCH-WORDLIST
; (c-addr u wid -- 0 | xt 1 | xt -1)

; 0 if not found
; 1 if immediate
; -1 otherwise

; The word itself is required by the implementation (of FIND) but is only visible if SEARCH is enabled

; TODO this can be moved to forth (once find uses find-name)

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
; WORDLIST
; (-- wid)

; TODO could move this to forth if the table is exposed?

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
