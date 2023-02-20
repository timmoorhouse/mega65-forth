
; ****************************************************************************

; Selected words from gforth

; TODO form (-- urows ucols)

; ****************************************************************************
; LATEST (also in FIG)

;      LATEST        ---  addr
;               Leave the name token of the most recently defined word
;               CURRENT vocabulary.

!if ENABLE_GFORTH {
        +WORD "latest"
} else {
        +NONAME
}
W_LATEST
        !word DO_COLON
        !word W_GET_CURRENT
        !word W_AT
        !word W_PSEMI

; ****************************************************************************
; TODO savesystem ("filename" --) - or just include this at the end of bootstrap sources?
; TODO return ior?

; SAVESYSTEM FOO,P,W

!if ENABLE_GFORTH {
        +WORD_IMM "savesystem"
W_SAVESYSTEM
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<savesystem>"
        !word W_DOTS
}
        !word W_PARSE_NAME
        !word W_WSLO
        !word W_OPEN_FILE
        !word W_DROP ; TODO check status

        !word W_TOR

        +LITERAL $2001 ; start of BASIC

        ; write load address
        !word W_SPAT
        !word W_TWO
        !word W_RAT
        !word W_WRITE_FILE
        !word W_DROP ; TODO check status

        ; TODO what to do about basepage?  does it make sense to move our base page to the top of memory and then
        ; initialize it in COLD?
        ; It would be very nice to have a savesystem on an unmodified dictionary give something identical to the
        ; original prg

        ; TODO inline dmalists will also have this issue

        !word W_DUP
        !word W_HERE
        !word W_SWAP
        !word W_SUB
        !word W_RAT
        !word W_WRITE_FILE
        !word W_DROP ; TODO check status

        !word W_RFROM
        !word W_CLOSE_FILE
        !word W_DROP ; TODO check status

!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; S>NUMBER?

; <anynum>   := { <BASEnum> | <decnum> | <hexnum> | <binnum> | <cnum> }
; <BASEnum>  := [-]<bdigit><bdigit>*
; <decnum>   := #[-]<decdigit><decdigit>*
; <hexnum>   := $[-]<hexdigit><hexdigit>*
; <binnum>   := %[-]<bindigit><bindigit>*
; <cnum>     := ’<char>’
; <bindigit> := { 0 | 1 }
; <decdigit> := { 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 }
; <hexdigit> := { <decdigit> | a | b | c | d | e | f | A | B | C | D | E | F }
;
; <bdigit> represents a digit according to the value of BASE (see 3.2.1.2 Digit conversion). 
; For <hexdigit>, the digits a. . . f have the values 10. . . 15. 
; <char> represents any printable character.
; The radix used for number conversion is:
; <BASEnum> the value in BASE
; <decnum> 10
; <hexnum> 16
; <binnum> 2
; <cnum> the number is the value of <char>

; TODO it might be more convenient if d was not left on the stack on failures

        +WORD "s>number?"
W_STONUMBERQ   ; (c-addr u -- d flag) ; flag indicates success
        !word DO_COLON

        ; Not right, but at least follows the interface ...
        !word W_2TOR
        !word W_ZERO
        !word W_ZERO
        !word W_2RFROM          ; (0 0 c-addr u)
        !word W_TONUMBER        ; (ud c-addr2 u2)
        !word W_NIP             ; (ud u2)
        !word W_ZEQUALS         ; (ud flag)
        !word W_PSEMI

!if 0 {
        ; (c-addr)
        !word W_ZERO
        !word W_ZERO
        !word W_ROT     ; (0 0 c-addr)

        ; check if first char is '-'
        !word W_DUP
        !word W_1PLUS
        !word W_CAT
        +CLITERAL '-'
        !word W_EQUAL   ; (0 0 c-addr is-negative)
        !word W_DUP
        !word W_TOR     ; (0 0 c-addr is-negative) (R: is-negative)


        !word W_PLUS
        +LITERAL $ffff
L2023
;    !word DPL
        !word W_STORE
        ; !word W_TONUMBER
        !word W_DUP    
        !word W_CAT
        !word W_BL
        !word W_SUB
        +ZBRANCH L2042

        !word W_DUP
        !word W_CAT
        +CLITERAL '.'
        !word W_SUB
        !word W_ZERO
;          !word QERR
        !word W_ZERO
        +BRANCH L2023

L2042
        !word W_DROP
        !word W_RFROM
        +ZBRANCH L2047
        ; !word W_DMINUS
L2047
        !word W_PSEMI
}

; ****************************************************************************
; SP@

; Also in FIG:

;      SP@           ---  addr
;               A computer dependent procedure to return the address of 
;               the stack position to the top of the stack, as it was 
;               before SP@ was executed.  (e.g. 1 2 SP@ @ . . . would type 
;               2 2 1 )

!if ENABLE_GFORTH {
        +WORD "sp@"
W_SPAT
        !word *+2
        phx
        lda #>base_page
        jmp PUSH
}

; TODO gforth also has fp@, rp@, etc