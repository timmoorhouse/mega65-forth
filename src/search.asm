
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
        !word W_PSEMIS
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
} else {
        +NONAME
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

;!if ENABLE_SEARCH {
!if 1 {
         +WORD "get-current"
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
} else {
        +NONAME
}
W_SEARCH_WORDLIST
        !word DO_COLON

!if CASE_INSENSITIVE {
        !word W_TOR
        !word W_2DUP
        !word W_LOWER
        !word W_RFROM
}

        ; this zero is the default return value

        !word W_ZERO
        !word W_SWAP

        ; (c-addr u 0 wid)

        +LITERAL W_PSEARCH_WORDLIST
        !word W_SWAP

        ; (c-addr u 0 xt wid)

        !word W_TRAVERSE_WORDLIST
        ; (c-addr u 0)  if not found
        ; (c-addr u nt) if found

        !word W_NIP
        !word W_NIP

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

; Search wordlist and return name token of a match
; Caller must place (c-addr u 0) on stack before TRAVERSE-WORDLIST, cleanup after
        +NONAME
W_PSEARCH_WORDLIST
        !word DO_COLON
        ; (c-addr u 0 nt -- c-addr u 0 true)   if not found
        ; (c-addr u 0 nt -- c-addr u nt false) if found

        !word W_TOR     ; (c-addr u 0) (R: nt)
        !word W_DROP    ; (c-addr u) (R: nt)
        !word W_2DUP
        !word W_RAT     ; (c-addr u c-addr u nt) (R: nt)

        !word W_NAME_TO_STRING
        !word W_COMPARE ; (c-addr u flag) (R: nt)

        +ZBRANCH +

        ; not found
        !word W_RFROM,W_DROP
        !word W_ZERO
        !word W_TRUE
        !word W_PSEMI

+
        ; found
        !word W_RFROM
        !word W_FALSE
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
        !word W_PSEMI
}
}
