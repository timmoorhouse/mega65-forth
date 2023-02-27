
;
; Implementations of internal words to allow us to bootstrap
;
; These will not be visible to the user.
;
;

; TODO things marked 'common usage' in Forth Programmer's Handbook:
; 2+                    OK
; 2-                    OK
; .'
; [DEFINED]             in 2012
; [UNDEFINED]           in 2012
; BEGIN (assembler)
; c+!                   skip?
; CURRENT               skip? also in gforth - should be fine with just get-current/set-current though?
; CVARIABLE             skip?
; DASM
; DEFER <name>          in 2012
; ELSE (assembler)
; END-CODE
; IF (assembler)
; INCLUDE <filename>
; INTERRUPT
; IS <name>
; L
; LOCATE <name>
; M-
; M/
; NEXT
; NOT
; NOT (assembler)
; T*                    triple precision, for m*/
; T/                    triple precision, for m*/
; THEN (assembler)
; UNTIL (assembler)
; VOCABULARY <name>
; WH <name>
; WHERE <name>
; 

; TODO some sort of RPICK (like PICK but for the return stack) for J, I, LEAVE, PLOOP, PPLOOP

; Is this a whitespace character?
; Used by PARSE-NAME
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

; ****************************************************************************
; 0

; TODO remove 0 1 2 3 and just use W_CLITERAL?

;      0 1 2 3        ---  n
;               These small numbers are used so often that it is 
;               attractive to define them by name in the dictionary as 
;               constants.

        +WORD "0"
W_ZERO
        !word DO_CONSTANT
        !word 0

; ****************************************************************************
; 1

        +WORD "1"
W_ONE
        !word DO_CONSTANT
        !word 1

; ****************************************************************************
; 2

        +WORD "2"
W_TWO
        !word DO_CONSTANT
        !word 2

; ****************************************************************************
; 2+

; TODO native implementation?

        +WORD "2+"
W_2PLUS
        !word DO_COLON
        !word W_TWO
        !word W_PLUS
        !word W_PSEMI

; ****************************************************************************
; 2-

; TODO native implementation?

        +WORD "2-"
W_2MINUS
        !word DO_COLON
        !word W_TWO
        !word W_SUB
        !word W_PSEMI

; ****************************************************************************
; 0BRANCH

;      0BRANCH       f  ---                                  C2
;               The run-time procedure to conditionally branch.  If f is 
;               false (zero), the following in-line parameter is added to 
;               the interpretive pointer to branch ahead or back.  
;               Compiled by IF, UNTIL, and WHILE.

        +WORD "0branch"
W_ZBRANCH
        !word *+2
        inx
        inx
        lda $fe,x
        ora $ff,x
        beq W_BRANCH+2
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

        +WORD "branch"
W_BRANCH
        !word *+2
BRANCH  ; used by (loop)  TODO MESSY !!!!!!!!
        ; ldy #0 ; TODO
        clc
        lda (<I),y
        adc <I
        pha
        iny
        lda (<I),y
        adc <I+1
        sta <I+1
        pla
        sta <I
        jmp NEXT ; NEXT +2 ????????

;
;    CLITERAL pushes the next inline byte to data stack
;
;        +WORD "cliteral"

        +NONAME
W_PCLITERAL
        !word *+2
        ; ldy #0 ; TODO
        lda (<I),y
        pha
        tya
        inw <I
        jmp PUSH

;
;    LITERAL pushes the next inline word to data stack
;

        +WORD "(literal)"
W_PLITERAL:
        !word *+2
        ; ldy #0 ; TODO
        lda (<I),y
        pha
        inw <I
        lda (<I),y
_inc_I_PUSH
        inw <I
        jmp PUSH

; ****************************************************************************
; ?HIDDEN
; (nt -- flag)

; Check if a name token is hidden

        +WORD "?hidden"
W_QHIDDEN
        !word DO_COLON
        !word W_2PLUS
        !word W_CAT
        +CLITERAL F_HIDDEN
        !word W_AND
        !word W_PSEMI

; ****************************************************************************
; ?IMMEDIATE
; (nt -- flag)
;
; Check if a name token is immediate
; Used by NAME>COMPILE and (soon) POSTPONE

        +WORD "?immediate"
W_QIMMEDIATE
        !word DO_COLON
        !word W_2PLUS
        !word W_CAT
        +CLITERAL F_IMMEDIATE
        !word W_AND
        !word W_PSEMI

; ****************************************************************************
; LATEST
; (-- nt | 0)
; Also in FIG, gforth

; Will be 0 if the last definition had no name (:NONAME)

        +WORD "latest"
W_LATEST
        !word DO_COLON
        +LITERAL &LATEST
        !word W_AT
        !word W_PSEMI

; ****************************************************************************
; LATESTXT
; (-- xt)
; Also in gforth

        +WORD "latestxt"
W_LATESTXT
        !word DO_COLON
        +LITERAL &LATEST_XT
        !word W_AT
        !word W_PSEMI

; ****************************************************************************
; LOWER
; (c-addr u --)
; Convert to lower case

        +WORD "lower"
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
        +CLITERAL 'A'
        !word W_LESS
        !word W_OVER
        +CLITERAL 'Z'
        !word W_SWAP
        !word W_LESS
        !word W_OR
        !word W_ZEQUAL
        +ZBRANCH +

        ; TODO handle other char ranges
        ; need to change case
        !word W_I
        !word W_CAT
        +CLITERAL 'A'-'a'
        !word W_SUB
        !word W_I
        !word W_CSTORE

+

        !word W_PLOOP
        !word _lower_loop-*
_lower_after_loop

        !word W_PSEMI

_lower_zero_length
        !word W_DROP
        !word W_PSEMI

; ****************************************************************************
; OUT   (from FIG)

;      OUT           ---  addr                               U
;               A user variable that contains a value incremented by EMIT.  
;               The user may alter and examine OUT to control display 
;               formatting.

        +WORD "out"
W_OUT
        !word DO_CONSTANT
        !word $00ec ; pntr TODO symbol?

; TODO change to gforth variants (sp0, sp!, rp0, rp!, etc)


; ****************************************************************************
; RP!
; (--)      TODO (addr --)
;      RP! (from FIG)
;               A computer dependent procedure to initialise the return 
;               stack pointer from user variable R0.
;        +WORD "rp!"

        +NONAME
W_RPSTORE
        !word *+2
        jsr _RPSTORE
        jmp NEXT

RPSTORE
        jsr _RPSTORE
        rts

_RPSTORE
        ; We need to save two levels of return pointer so the return from RPSTORE
        ; goes to the right place
        pla
        sta <TEMP1
        pla
        sta <TEMP1+1
        pla
        sta <TEMP2
        pla
        sta <TEMP2+1
        ; Now reset the SPH & SPL from R0
        stx <XSAVE      
        lda <R0      
        tax
        txs
        lda <R0+1
        tay
        tys
        ldx <XSAVE
        ; And finally put the original return pointer onto the new stack
        lda <TEMP2+1
        pha
        lda <TEMP2
        pha
        lda <TEMP1+1
        pha
        lda <TEMP1
        pha
        rts

; ****************************************************************************
; RP@
; (-- addr)
; From gforth

        +WORD "rp@"
W_RPAT
        !word *+2
        stx <XSAVE
        tsx
        phx
        ldx <XSAVE
        tsy
        tya
        jmp PUSH

; TODO gforth also has fp@, rp@, etc

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
;      SMUDGE (from FIG)
;               Used during word definition to toggle the "smudge bit" in 
;               a definition's name field.  This prevents an un-completed 
;               definition  from being found during dictionary searches, 
;               until compiling is completed without error.

        +NONAME
W_SMUDGE
        !word DO_COLON
        ; see also immediate (core)
        !word W_LATEST
        !word W_2PLUS
        !word W_DUP
        !word W_CAT
        +CLITERAL F_HIDDEN
        !word W_XOR
        !word W_SWAP
        !word W_CSTORE
        !word W_PSEMI

; ****************************************************************************
;      SP! (from FIG)
;               A computer dependent procedure to initialise the stack 
;               pointer from S0.
;        +WORD "sp!"

        +NONAME
W_SPSTORE
        !word *+2
SPSTORE
        lda <S0         ; load data stack pointer (X reg) from
        tax             ; silent user variable S0.
        jmp NEXT

; ****************************************************************************
; SP@
; (-- addr)

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

; ****************************************************************************
; !CSP

;               Save the stack position in CSP.  Used as part of the 
;               compiler security.
;

!if 0 {
        +WORD "!csp"
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
        +WORD "?csp"
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
        +WORD "csp"
W_CSP
        !word DO_USER
;          !byte $2C
}

; ****************************************************************************
; ?COMP

;      ?COMP
;               Issue error message if not compiling.

!if 0 {
        +WORD "?comp"
W_QCOMP
        !word DO_COLON
;          !word STATE
;          !word AT
;          !word ZEQU
;          !word CLIT
;          !byte $11
;          !word QERR
        !word W_PSEMI
}

; ****************************************************************************
; ?EXEC

;      ?EXEC
;               Issue an error message if not executing.

!if 0 {
        +WORD "?exec"
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
        +WORD "?pairs"
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

;      ?STACK
;               Issue an error message if the stack is out of bounds.  
;               This definition may be installation dependent.

!if 0 {
        +WORD "?stack"
W_QSTACK
        !word DO_COLON
;          !word CLIT
;          !byte TOS
;          !word SPAT
;          !word ULESS
;          !word ONE
;          !word QERR
;          !word SPAT
;          !word CLIT
;          !byte BOS
;          !word ULESS
;          !word CLIT
;          !byte 7
;          !word QERR
        !word W_PSEMI
}

; ****************************************************************************
; FIG
; ?TERMINAL

;      ?TERMINAL     ---  f
;               Perform a test of the terminal keyboard for actuation of 
;               the break key.  A true flag indicates actuation.  This 
;               definition is installation dependent.

; TODO kernel call to query stop key

!if 0 {
        +WORD "?terminal"
W_QTERMINAL
;    !word XQTER    ; Vector to code for ?TERMINAL
}

; ****************************************************************************

; FIG:
;      ?ERROR        f  n  ---
;               Issue an error message number n, if the boolean flag is 
;               true.

; TODO remove, replace with ABORT"

!if 0 {
        +WORD "?error"
W_QERROR
        !word DO_COLON
;          !word SWAP
;          !word ZBRANCH
;L1402:    !word 8        ; L1406-L1402
;          !word ERROR
;          !word BRANCH
;L1405:    !word 4        ; L1407-L1405
;L1406:    !word DROP
        !word W_PSEMI
}

;      ERROR         line  ---  in  blk
;               Execute error notification and restart of system.  WARNING 
;               is first examined.  If 1, the text of line n, relative to 
;               screen 4 of drive 0 is printed.  This line number may be 
;               positive or negative, and beyond just screen 4.  If 
;               WARNING=0, n is just printed as a message number (non-disc 
;               installation).  If WARNING is -1, the definition (ABORT) 
;               is executed, which executes the system ABORT.  The user 
;               may cautiously modify this execution by altering (ABORT).  
;               fig-FORTH saves the contents of IN and BLK to assist in 
;               determining the location of the error.  Final action is 
;               execution of QUIT.

; TODO remove, replace with ABORT"

!if 0 {
;        +WORD "error"
W_ERROR
        !word DO_COLON
;          !word WARN
;          !word AT
;          !word ZLESS
;          !word ZBRAN
;L2094:    !word $4       ; L2096-L2094
;          !word W_ABORT
;L2096:    !word HERE
;          !word COUNT
;          !word TYPE
;          !word PDOTQ
;          !byte 4,"  ? "
;          !word MESS
;          !word SPSTO
;          !word DROP,DROP; make room for 2 error values
;          !word IN
;          !word AT
;          !word BLK
;          !word AT
;          !word QUIT
        !word W_PSEMI
}