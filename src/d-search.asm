
; ****************************************************************************
; SEARCH

; TODO move these to forth

        +CREATE_ENV "wordlists"
        !word DO_CONSTANT
        !word WORDLISTS

; ****************************************************************************
; WORDLIST
; (-- wid)

; TODO could move this to forth if the table is exposed?

        +CREATE "wordlist", 0
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
