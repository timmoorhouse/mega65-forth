
;
; Implementations of internal words to allow us to bootstrap
;
; These will not be visible to the user.
;
;

; TODO things marked 'common usage' in Forth Programmer's Handbook:
; .'                    might want this, but could be tricky
; BEGIN (assembler)
; c+!                   skip?
; CVARIABLE             skip?
; DASM
; ELSE (assembler)
; END-CODE
; IF (assembler)
; INTERRUPT
; L
; LOCATE <name>
; M-
; M/
; NEXT
; NOT
; NOT (assembler)
; T*                    triple precision, for m*/ - might want this
; T/                    triple precision, for m*/ - might want this
; THEN (assembler)
; UNTIL (assembler)
; VOCABULARY <name>
; WH <name>
; WHERE <name>
; 

; Is this a whitespace character?
; TODO - use in PARSE-NAME
isspace
        ; A - character to test
        ; TODO check newline, etc
        cmp #' '
        rts

!if ENABLE_RUNTIME_CHECKS {
fail_runtime_check
        jsr CR
        lda #'w'
        jsr EMIT
        lda <W+1
        jsr put_hex
        lda <W
        jsr put_hex
        lda #' '
        jsr EMIT
        lda #'i'
        jsr EMIT
        lda <I+1
        jsr put_hex
        lda <I
        jsr put_hex
        lda #' '
        jsr EMIT
        jsr DOTS
        lda #' '
        jsr EMIT
        jsr RDUMP
        brk
}

!if 0 {
BRK_ENABLED
        !byte 0

        +CREATE_INTERNAL "enable-brk", 0
W_ENABLE_BRK
        !word *+2
        jsr ENABLE_BRK
        jmp NEXT

ENABLE_BRK
        lda #$ff
        sta BRK_ENABLED
        rts

        +CREATE_INTERNAL "brk", 0
W_BRK
        !word *+2
        jsr MAYBE_BRK
        jmp NEXT

REALLY_BRK
        ; lda #'#'
        ;jsr EMIT
        rts

MAYBE_BRK
        lda BRK_ENABLED
        beq +
        jsr REALLY_BRK
+       rts
}

; ****************************************************************************

; TODO timer
; check Clear_TI, start_timer  CIA2_CRA (dd0e), CIA2_CRB (dd0f), CIA_TALO (dd04), Get_TI_CIA
; see https://www.c64-wiki.com/wiki/CIA
; Something like execute that counts clock ticks?

        ; +CREATE_INTERNAL "timer", 0
        +CREATE "timer", 0
W_TIMER
        !word *+2
        jsr read_timer
        dex     ; TODO check for stack space
        dex
        dex
        dex
        lda <TEMP1
        sta 2,x
        lda <TEMP1+1
        sta 3,x
        lda <TEMP1+2
        sta 0,x
        lda <TEMP1+3
        sta 1,x
        jmp NEXT

read_timer
        stx <XSAVE
        lda #$ff
        tax
        tay
        taz
        sbcq $dd04
        stq  <TEMP1
        ldx <XSAVE
        rts

; ****************************************************************************
; 0

        +CREATE "0", 0
W_ZERO
        !word DO_CONSTANT
        !word 0

; ****************************************************************************
; 1

        +CREATE "1", 0
W_ONE
        !word DO_CONSTANT
        !word 1

; ****************************************************************************
; 2

        +CREATE "2", 0
W_TWO
        !word DO_CONSTANT
        !word 2

; ****************************************************************************
; 2+

; TODO native implementation?

        +CREATE "2+", 0
W_2PLUS
        !word DO_COLON
        !word W_TWO
        !word W_PLUS
        !word W_PSEMI

; ****************************************************************************
; 2-

; TODO native implementation?

        +CREATE "2-", 0
W_2MINUS
        !word DO_COLON
        !word W_TWO
        !word W_SUB
        !word W_PSEMI

; ****************************************************************************
; DPL
; number of digits to right of decimal place in most recent numeric
; conversion (-1 if none)

        +CREATE_INTERNAL "dpl", 0
W_DPL
        !word DO_VARIABLE
        !word 0

; ****************************************************************************
; 0BRANCH

;      0BRANCH       f  ---                                  C2
;               The run-time procedure to conditionally branch.  If f is 
;               false (zero), the following in-line parameter is added to 
;               the interpretive pointer to branch ahead or back.  
;               Compiled by IF, UNTIL, and WHILE.

        +CREATE_INTERNAL "0branch", 0
W_ZBRANCH
        !word *+2
        inx
        inx
        lda $fe,x
        ora $ff,x
        beq BRANCH
BUMP             ; used by (loop) and LEAVE  TODO MESSY !!!!!!!!!!!!!!
        inw <I
        inw <I
        jmp NEXT

; ****************************************************************************
;      BRANCH                                                C2,L0
;               The run-time procedure to unconditionally branch.  An in-
;               line offset is added to the interpretive pointer IP to 
;               branch ahead or back.  BRANCH is compiled by ELSE, AGAIN, 
;               REPEAT.

        +CREATE_INTERNAL "branch", 0
W_BRANCH
        !word *+2
BRANCH  ; used by (loop), 0branch  TODO MESSY !!!!!!!!
        ; ldy #0 ; TODO
        clc
        lda (<I),y
        adc <I
        taz
        iny
        lda (<I),y
        adc <I+1
        sta <I+1
        stz <I
        jmp NEXT

; ****************************************************************************
;
;    CLITERAL pushes the next inline byte to data stack
;
;        +CREATE "cliteral"

        +CREATE_INTERNAL "(cliteral)", 0
W_PCLITERAL
        !word *+2
        ; ldy #0 ; TODO
        lda (<I),y
        pha
        tya
        inw <I
        jmp PUSH

; ****************************************************************************
;
;    LITERAL pushes the next inline word to data stack
;

        +CREATE_INTERNAL "(literal)", 0
W_PLITERAL:
        !word *+2
        ; ldy #0 ; TODO
        lda (<I),y
        pha
        inw <I
        lda (<I),y
_inc_I_PUSH ; TODO
        inw <I
        jmp PUSH

; ****************************************************************************
;
;    2LITERAL pushes the next two inline word to data stack
;

        +CREATE_INTERNAL "(2literal)", 0
W_P2LITERAL:
        !word *+2
        ; ldy #0 ; TODO
        dex
        dex
        lda (<I),y
        sta 0,x
        inw <I
        lda (<I),y
        sta 1,x
        inw <I
        lda (<I),y
        pha
        inw <I
        lda (<I),y
        inw <I
        jmp PUSH

; ****************************************************************************
;
;       SLITERAL pushes an inline counted string to the data stack
;

        +CREATE_INTERNAL "(csliteral)", 0
W_PCSLITERAL:
        !word *+2
        ; ldy #0 ; TODO
        lda <I
        pha
        ldz &I+1
        lda (<I),y
        sec
        adc <I
        sta <I
        bcc +
        inc <I+1
+
        tza
        jmp PUSH

; ****************************************************************************

; TODO
; See https://forth-standard.org/proposals/find-name#contribution-58
; (this is pretty much the proposed FIND-NAME, FIND-NAME-IN)

; Like SEARCH-WORDLIST but returns 0|nt

        +CREATE "find-name", 0
W_FIND_NAME
        !word DO_DEFER
        !word W_SIMPLE_FIND_NAME

        +CREATE "find-name-in", 0
W_FIND_NAME_IN  ; (c-addr u wid -- 0 | nt)
        !word DO_COLON

!if CASE_INSENSITIVE {
        !word W_TOR
        !word W_2DUP
        !word W_LOWER
!if 0 {
        +CLITERAL '['
        !word W_EMIT
        !word W_2DUP,W_TYPE
        +CLITERAL ']'
        !word W_EMIT
        !word W_CR
}

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
        !word W_ZERO
        !word W_PSEMI

; ****************************************************************************
; ?COMPILE-ONLY
; (nt -- flag)

; Check if a name token is compile-only

        +CREATE_INTERNAL "?compile-only", 0
W_QCOMPILE_ONLY
        !word DO_COLON
        !word W_2PLUS
        !word W_CAT
        +CLITERAL F_COMPILE_ONLY
        !word W_AND
        ; !word W_ZNOTEQUAL
        !word W_PSEMI

; ****************************************************************************
; ?IMMEDIATE
; (nt -- flag)
;
; Check if a name token is immediate
; Used by NAME>COMPILE and (soon) POSTPONE

        +CREATE_INTERNAL "?immediate", 0
W_QIMMEDIATE
        !word DO_COLON
        !word W_2PLUS
        !word W_CAT
        +CLITERAL F_IMMEDIATE
        !word W_AND
        ; !word W_ZNOTEQUAL
        !word W_PSEMI

; ****************************************************************************
; LATEST
; (-- nt | 0)
; Also in FIG, gforth

; Will be 0 if the last definition had no name (:NONAME)

        +CREATE_INTERNAL "latest", 0
W_LATEST
        !word DO_CONSTANT
LATEST        
        !word 0

; ****************************************************************************
; LATESTXT
; (-- xt)
; Also in gforth

        +CREATE_INTERNAL "latestxt", 0
W_LATESTXT
        !word DO_CONSTANT
LATESTXT
        !word 0        

; ****************************************************************************
; LOWER
; (c-addr u --)
; Convert to lower case

        ; TODO CLEAN THIS UP!

        +CREATE_INTERNAL "lower", 0
W_LOWER
        !word DO_COLON

        ; TODO remove this check and just do ?DO
        !word W_QDUP
        +ZBRANCH _lower_zero_length


        !word W_OVER
        !word W_PLUS
        !word W_SWAP
        +DO _lower_after_loop

_lower_loop

        !word W_I
        !word W_CAT 
        
        ; Handle c1..da

        !word W_DUP
        +CLITERAL 'A'
        !word W_LESS
        !word W_OVER
        +CLITERAL 'Z'
        !word W_SWAP
        !word W_LESS
        !word W_OR
        !word W_ZEQUAL
        +ZBRANCH +

        !word W_I
        !word W_CAT
        +CLITERAL 'A'-'a'
        !word W_SUB
        !word W_I
        !word W_CSTORE
        +BRANCH _lower_loop_next

+

        ; Handle 61..7a
        !word W_DUP
        +CLITERAL 97
        !word W_LESS
        !word W_OVER
        +CLITERAL 122
        !word W_SWAP
        !word W_LESS
        !word W_OR
        !word W_ZEQUAL
        +ZBRANCH +

        !word W_I
        !word W_CAT
        +CLITERAL 97-'a'
        !word W_SUB
        !word W_I
        !word W_CSTORE
        +BRANCH _lower_loop_next

+

        ; TODO handle other char ranges


_lower_loop_next
        !word W_DROP

        !word W_PLOOP
        !word _lower_loop-*
_lower_after_loop

        !word W_PSEMI

_lower_zero_length
        !word W_DROP
        !word W_PSEMI

; ****************************************************************************
; NAME>XT
; (nt -- xt)
; This is like name>interpret but gives the xt even for compile-only words

        +CREATE_INTERNAL "name>xt", 0
W_NAME_TO_XT
        !word DO_COLON
        !word W_DUP
        !word W_ZEQUAL
        +LITERAL E_UNDEFINED_WORD
        !word W_AND
        !word W_THROW
        !word W_NAME_TO_STRING  ; immediately after the name
        !word W_ONE
        !word W_OR              ; for alignment
        !word W_PLUS
        !word W_PSEMI

; ****************************************************************************
; NOOP
; (--)
; Used to initialize DEFERs that don't need to be set to something

        +NONAME
W_NOOP
        !word DO_COLON
        !word W_PSEMI

; ****************************************************************************
; OUT   (from FIG)

;      OUT           ---  addr                               U
;               A user variable that contains a value incremented by EMIT.  
;               The user may alter and examine OUT to control display 
;               formatting.

        +CREATE_INTERNAL "out", 0
W_OUT
        !word DO_CONSTANT
        !word $00ec ; pntr TODO symbol?

; ****************************************************************************
; RP!
; (addr --)

        +CREATE_INTERNAL "rp!", 0
W_RPSTORE
        !word *+2
        lda 0,x
        ldy 1,x
        jsr RPSTORE
        jmp POP

RPSTORE
        ; A - new SPL, Y - new SPH
        ; We need to save one level of return pointer so the return from RPSTORE
        ; goes to the right place
!if 1 {        
        plz
        stz <TEMP1              ; TODO could use plw / phw here
        plz
        stz <TEMP1+1
} else {
        plw &TEMP1
}
        ; Now reset the SPH & SPL from R0
        stx <XSAVE      
        tax
        txs
        ldx <XSAVE
        tys
        ; And finally put the original return pointer onto the new stack
!if 1 {        
        lda <TEMP1+1
        pha
        lda <TEMP1
        pha
} else {
        phw &TEMP1
}
        rts

; ****************************************************************************
; RP@
; (-- addr)
; From gforth

        +CREATE_INTERNAL "rp@", 0
W_RPAT
        !word *+2
        jsr RPAT
!if 0 {        
        clc    ; TODO should we skip the +1 ????
        adc #1 ; we want the value returned by rp@ to point to the top value on the return stack, not the byte below
}        
        pha
        tya
        jmp PUSH

RPAT
        ; Leaves SPH in Y and SPL in A (actually what was SPL at the time of the call to RPAT)
        ; SP points to the location the next pushed byte will be copied to.  
        ; The byte at the top of the stack is at (SP+1).
        ; We want the value of SP at the time of the call to get_SP, so we need to add 2 to
        ; account for the return address for the call to get_SP.
        stx <XSAVE
        tsx
        txa
        ldx <XSAVE
        clc
        adc #2
        tsy
        rts

        
; TODO use RP@ for J, I, LEAVE, PLOOP, PPLOOP, etc?

; TODO gforth also has fp@, etc

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

        +CREATE_INTERNAL "s>number?", 0
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

        !word W_TRUE
        !word W_DPL
        !word W_STORE

        !word W_2TOR
        !word W_ZERO
        !word W_ZERO
        !word W_2RFROM          ; (0 0 c-addr u) (R: old-base is-negative)
        !word W_PTONUMBER       ; (ud c-addr2 u2) (R: old-base is-negative)
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
        !word W_ZERO ; put a dummy value where c should be (since we didn't dup)
        +LITERAL 2
        +BRANCH _stonumber_check_base_set

+
        ; ... nope, none of them
        ; !word W_DROP
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
; SBUF
; ( -- c-addr )
;
; Return the location of the next string buffer
; TODO return the length also?

NEXT_SBUF       !byte 0

        +CREATE_INTERNAL "sbuf", 0
W_SBUF
        !word *+2
        lda NEXT_SBUF
        beq +
        sty NEXT_SBUF
        lda #<(SBUF + SBUF_LEN)
        pha
        lda #>(SBUF + SBUF_LEN)
        jmp PUSH
+
        inc NEXT_SBUF
        ; TODO swap between two buffers
        lda #<SBUF
        pha
        lda #>SBUF
        jmp PUSH

; ****************************************************************************
; SOURCE-LINE

        +CREATE_INTERNAL "source-line", 0
W_SOURCE_LINE
        !word DO_VARIABLE
        !word 0

; ****************************************************************************
; SP!
; (addr --)

        +CREATE_INTERNAL "sp!", 0
W_SPSTORE
        !word *+2
SPSTORE
        lda 0,x
        tax
        jmp NEXT ; note that we don't POP here since SP is already reset

; ****************************************************************************
; SP@
; (-- addr)

; Also in FIG:

;      SP@           ---  addr
;               A computer dependent procedure to return the address of 
;               the stack position to the top of the stack, as it was 
;               before SP@ was executed.  (e.g. 1 2 SP@ @ . . . would type 
;               2 2 1 )

        +CREATE_INTERNAL "sp@", 0
W_SPAT
        !word *+2
        phx
        lda #>base_page
        jmp PUSH

; ****************************************************************************
; !CSP

;               Save the stack position in CSP.  Used as part of the 
;               compiler security.
;

!if 0 {
        +CREATE_INTERNAL "!csp", 0
W_SCSP
        !word DO_COLON
;          !word SPAT
;          !word CSP
;          !word STORE
        !word W_PSEMI
}

; ****************************************************************************
; ?CSP

;      ?CSP
;               Issue error message if stack position differs from value 
;               saved in CSP.

!if 0 {
        +CREATE_INTERNAL "?csp", 0
W_QCSP
        !word DO_COLON
;          !word SPAT
;          !word CSP
;          !word AT
;          !word SUB
;          !word CLIT
;          !byte $14
;          !word QERR
        !word W_PSEMI
}

; ****************************************************************************
; CSP

;      CSP           ---  addr                               U
;               A user variable temporarily storing the stack pointer 
;               position, for compilation error checking.

!if 0 {
        +CREATE_INTERNAL "csp", 0
W_CSP
        !word DO_USER
;          !byte $2C
}

; ****************************************************************************
; ?EXEC

;      ?EXEC
;               Issue an error message if not executing.

!if 0 {
        +CREATE_INTERNAL "?exec", 0
W_QEXEC
        !word DO_COLON
;          !word STATE
;          !word AT
;          !word CLIT
;          !byte $12
;          !word QERR
        !word W_PSEMI
}

; ****************************************************************************
; ?PAIRS

;      ?PAIRS        n1  n2  ---
;               Issue an error message if n1 does not equal n2.  The 
;               message indicates that compiled conditionals do not match.

!if 0 {
        +CREATE_INTERNAL "?pairs", 0
W_QPAIR
        !word DO_COLON
;          !word SUB
;          !word CLIT
;          !byte $13
;          !word QERR
        !word W_PSEMI
}

; ****************************************************************************
; ?STACK
; ( -- )
;               Check if the stack is out of bounds.  

        +CREATE_INTERNAL "?stack", 0
W_QSTACK
        !word DO_COLON
!if 1 {
        +LITERAL base_page + TOS - 4
        !word W_SPAT
        !word W_ULESS
        +LITERAL E_DATA_STACK_UNDERFLOW
        !word W_AND
        !word W_THROW
        !word W_SPAT
        +LITERAL base_page + BOS
        !word W_ULESS
        +LITERAL E_DATA_STACK_OVERFLOW
        !word W_AND
        !word W_THROW
}
        !word W_PSEMI

; ****************************************************************************
; FIG
; ?TERMINAL

;      ?TERMINAL     ---  f
;               Perform a test of the terminal keyboard for actuation of 
;               the break key.  A true flag indicates actuation.  This 
;               definition is installation dependent.

; TODO kernel call to query stop key

!if 0 {
        +CREATE_INTERNAL "?terminal", 0
W_QTERMINAL
;    !word XQTER    ; Vector to code for ?TERMINAL
}
