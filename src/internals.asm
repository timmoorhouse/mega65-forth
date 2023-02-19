
;
; Implementations of internal words to allow us to bootstrap
;
; These will not be visible to the user.
;
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
;        +WORD "0"

        +NONAME
W_ZERO
        !word DO_CONSTANT
        !word 0

; ****************************************************************************
; 1
;        +WORD "1"

        +NONAME
W_ONE
        !word DO_CONSTANT
        !word 1

; ****************************************************************************
; 2
;        +WORD "2"

        +NONAME
W_TWO
        !word DO_CONSTANT
        !word 2

; ****************************************************************************
; 2+

;      2+            n1  ---  n2
;               Leave n1 incremented by 2.

; TODO native implementation?

;        +WORD "2+"
        +NONAME
W_2PLUS
        !word DO_COLON
        !word W_TWO
        !word W_PLUS
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
        clc
        lda <I
        adc #2
        sta <I
        bcc +
        inc <I+1
+       jmp NEXT
!macro ZBRANCH .target {
        !word W_ZBRANCH
        !word .target-*
}

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
        pha ; TODO why not just store? (and skip the push and pull)
        iny
        lda (<I),y
        adc <I+1
        sta <I+1
        pla
        sta <I
        jmp NEXT ; NEXT +2 ????????
!macro BRANCH .target {
        !word W_BRANCH
        !word .target-*
}

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
        inc <I
        bne +
        inc <I+1
+       jmp PUSH
!macro CLITERAL .char {
        !word W_PCLITERAL
        !byte .char
}

;
;    LITERAL pushes the next inline word to data stack
;

        +NONAME
W_PLITERAL:
        !word *+2
        ; ldy #0 ; TODO
        lda (<I),y
        pha
        inc <I
        bne +
        inc <I+1
+       lda (<I),y
_inc_I_PUSH
        inc <I
        bne +
        inc <I+1
+       jmp PUSH
!macro LITERAL .word {
        !word W_PLITERAL
        !word .word
}

; ****************************************************************************
; ?IMMEDIATE
; (nt -- flag)
;
; Check if a name token is immediate
; Used by NAME>COMPILE and (soon) POSTPONE

        ; +WORD "?immediate"
        +NONAME
W_QIMMEDIATE
        !word DO_COLON
        !word W_2PLUS
        !word W_CAT
        +CLITERAL F_IMMEDIATE
        !word W_AND
        !word W_PSEMI

; ****************************************************************************
; LOWER
; (c-addr u --)
; Convert to lower case

        ;+WORD "lower"
        +NONAME
W_LOWER
        !word DO_COLON
        !word W_OVER
        !word W_PLUS
        !word W_SWAP
        !word W_PDO

_lower_loop

        !word W_I
        !word W_CAT
        +CLITERAL 'A'
        !word W_LESS
        !word W_OVER
        +CLITERAL 'Z'
        !word W_GREATER
        !word W_OR
        !word W_ZEQUALS
        +ZBRANCH +

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