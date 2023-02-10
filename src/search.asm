
; ****************************************************************************
; SEARCH


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

!if ENABLE_SEARCH {
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
}

; ****************************************************************************
; FIND
; (c-addr -- c-addr 0 | xt 1 | xt -1)
; ANSI 16.6.1.1550

!if ENABLE_SEARCH {
}

; ****************************************************************************
; FORTH-WORDLIST
; (-- wid)
; 16.6.1.1595

; The word itself is required by the implementation but will only be visible if SEARCH is enabled

!if ENABLE_SEARCH {
        +WORD "forth-wordlist"
}
W_FORTH_WORDLIST
        !word DO_VARIABLE
FORTH_WORDLIST
        !word 0        

; ****************************************************************************
; GET-CURRENT
; (-- wid)
; ANSI 16.6.1.1643

;;
;;                                       CURRENT
;;                                       SCREEN 37 LINE 3
;;

!if ENABLE_SEARCH {
!if 0 {
;        +WORD "current"
W_CURRENT
        !word DO_USER
;          !byte $22
}
}

; ****************************************************************************
; GET-ORDER
; (-- wid_n ... wid_1 n)
; ANSI 16.6.1.1647

!if ENABLE_SEARCH {
}

; ****************************************************************************
; SEARCH-WORDLIST
; (c-addr u wid -- 0 | xt 1 | xt -1)
; ANSI 16.6.1.2192

; 0 if not found
; 1 if immediate
; -1 otherwise

; The word itself is required by the implmentation (of FIND) but is only visible if SEARCH is enabled

!if ENABLE_SEARCH {
        +WORD "search-wordlist"
}
W_SEARCH_WORDLIST
        !word DO_COLON

!if 1 {
        !word W_ROT
        !word W_ROT
        !word W_PDOTQ
        +STRING "<search>["
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT
        !word W_CR
        !word W_ROT
}

        +LITERAL W_PSEARCH_WORDLIST
        !word W_SWAP

        
        !word W_TRAVERSE_WORDLIST
        ; TODO implement using
        ;     TRAVERSE-WORDLIST (tools-ext)
        ;     NAME>STRING (tools)
        ;     COMPARE (string)
        ;     EXECUTE (core)
        ;
        ;

        !word W_DROP
        !word W_SEMI

W_PSEARCH_WORDLIST ; (c-addr u nt -- c-addr u (xt 0)|1)
        !word DO_COLON

        +CLITERAL 's'
        !word W_EMIT

        !word W_TOR ; (c-addr u) (R: nt)

        !word W_2DUP
        !word W_RAT ; (c-addr u c-addr u nt) (R: nt)

        !word W_NAME_TO_STRING

!if 1 {
        +CLITERAL '"'
        !word W_EMIT
        !word W_2DUP
        !word W_TYPE
        +CLITERAL '"'
        !word W_EMIT
}

        !word W_COMPARE

        !word W_DOTS
        !word W_DROP
        
        ; Check for a name match
        ; !word W_DUP             ; (c-addr u nt nt)
        ; !word W_NAME_TO_STRING  ; (c-addr1 u nt c-addr2 u )
        ; !word W_COMPARE

        ; push true if iteration should continue, false if done
        ; TODO name>interpret or name>compile

!if 0 {
        !word W_CR
}

        !word W_RFROM,W_DROP
        
!if 1 {        
        !word W_TRUE ; continue
} else {
        !word W_FALSE ; stop
}
        
        !word W_SEMI

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

;;
;;                                       VOCABULARY
;;                                       SCREEN 53 LINE 4
;;

!if ENABLE_SEARCH {
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
