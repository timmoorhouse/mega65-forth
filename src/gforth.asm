
; ****************************************************************************

; Selected words from gforth

; ****************************************************************************
; FIND-NAME
; (c-addr u -- nt | 0)

; TODO form (-- urows ucols)

; ****************************************************************************
; LATEST (also in FIG)
; (-- nt | 0)

; TODO gforth has two vars:
;    latest   - name token of the most recently defined word (0 if it has no name)
;    latestxt - execution token of the most recently defined word

!if ENABLE_GFORTH {
        +WORD "latest"
} else {
        +NONAME
}
W_LATEST
        !word DO_COLON
        +LITERAL &LATEST
        !word W_AT
        !word W_PSEMI

!if ENABLE_GFORTH {
        +WORD "latestxt"
} else {
        +NONAME
}
W_LATESTXT
        !word DO_COLON
        +LITERAL &LATEST_XT
        !word W_AT
        !word W_PSEMI

; ****************************************************************************
; TODO savesystem ("filename" --) - or just include this at the end of bootstrap sources?
; TODO return ior?

; SAVESYSTEM FOO,P,W

; TODO do this in forth

!if ENABLE_GFORTH {
        +WORD_IMM "savesystem"
W_SAVESYSTEM
        !word DO_COLON
!if DEBUG {
        +DOTQ "<savesystem>"
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

; TODO if a '.' follows any case other than <cnum>, treat it as a double literal (see 8.3.1)
; TODO floating point stuff (from 12.3.7)
; 

; TODO it might be more convenient if d was not left on the stack on failures

        +WORD "s>number?"
W_STONUMBER   ; (c-addr u -- d flag) ; flag indicates success
        !word DO_COLON

!if 0 {
        +DOTQ "s>number-1["
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT,W_DOTS
}

        !word W_BASE
        !word W_AT
        !word W_TOR     ; (c-addr u) (R: old-base)

        !word W_STONUMBER_CHECK_BASE

        !word W_ZERO
        !word W_TOR     ; (c-addr u) (R: old-base is-negative)
        !word W_DUP
        +ZBRANCH ++

        !word W_OVER
        !word W_CAT
!if 0 {
        +DOTQ "s>number-5"
        !word W_DOTS,W_CR
}
        !word W_DUP
        +CLITERAL '-'
        !word W_EQUAL
        +ZBRANCH +
        !word W_DROP
        ; It's a -, set is-negative flag
        !word W_RFROM
        !word W_INVERT
        !word W_TOR
        ; ... and remove from the string
        !word W_1MINUS
        !word W_SWAP
        !word W_1PLUS
        !word W_SWAP
        +BRANCH ++

+       ; !word W_DUP ; Don't bother DUPing - it's the last one
        +CLITERAL '\''
        !word W_EQUAL
        +ZBRANCH ++
        ; It's a '
        ; TODO this doesn't check for a closing ' or check the length
        !word W_DROP
        !word W_1PLUS
        !word W_CAT
        !word W_ZERO
        !word W_TRUE
        +BRANCH _stonumber_finish_up

++
        ; Now convert the unsigned portion

        !word W_2TOR
        !word W_ZERO
        !word W_ZERO
        !word W_2RFROM          ; (0 0 c-addr u) (R: old-base is-negative)
        !word W_TONUMBER        ; (ud c-addr2 u2) (R: old-base is-negative)
        !word W_NIP
        !word W_ZEQUAL         ; (ud flag) (R: old-base is-negative)

_stonumber_finish_up

        ; (ud flag) (R: old-base is-negative)

        !word W_RFROM           ; (ud flag is-negative) (R: old-base)
        +ZBRANCH +
        !word W_TOR
        !word W_DNEGATE         
        !word W_RFROM
+
        !word W_RFROM           ; (d flag old-base)
        !word W_BASE
        !word W_STORE
!if 0 {
        +DOTQ " s>number-9"
        !word W_DOTS,W_CR
}
        !word W_PSEMI

; Checks for a leading #, $ or %
; If found, adjusts the address and count to remove the leading
; char and sets BASE appropriately
        +NONAME
W_STONUMBER_CHECK_BASE ; (c-addr u -- c-addr u)
        !word DO_COLON

        !word W_DUP
        !word W_ZEQUAL
        +ZBRANCH +
        ; zero length, just return
        !word W_PSEMI
+

        !word W_OVER
        !word W_CAT
        ; (c-addr u char)

        !word W_DUP
        +CLITERAL '#'
        !word W_EQUAL
        +ZBRANCH +
        ; It's a # ... use base 10
        +LITERAL 10
        +BRANCH _stonumber_check_base_set

+       !word W_DUP
        +CLITERAL '$'
        !word W_EQUAL
        +ZBRANCH +
        ; It's a $ ... use base 16
        +LITERAL 16
        +BRANCH _stonumber_check_base_set

+       ; !word W_DUP ; Don't bother DUPing ... it's the last one
        +CLITERAL '%'
        !word W_EQUAL
        +ZBRANCH +
        ; It's a % ... use base 2
        +LITERAL 2
        +BRANCH _stonumber_check_base_set

+
        ; ... nope, none of them
        ;!word W_DROP
        ; (c-addr u)
        !word W_PSEMI

_stonumber_check_base_set
        ; (c-addr u c base)
        !word W_BASE
        !word W_STORE
        !word W_DROP
        !word W_1MINUS
        !word W_SWAP
        !word W_1PLUS
        !word W_SWAP
        !word W_PSEMI

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