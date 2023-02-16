
;
; Implementations of internal words to allow us to bootstrap
;
; These will not be visible to the user.
;
;

; TODO some sort of RPICK (like PICK but for the return stack) for J, I, LEAVE, PLOOP, PPLOOP

; from FIG ...
; used by (find), enclose, cmove
;
;
; Move a number of words from data stack to array N[8] in basepage
; (the idea being to allow lda ($<N+xx),y for scanning strings)
; Can we instead move to return stack and use lda ($xx,SP),y?
; We'd need to clean up the return stack
; In either case, this seems like it will be of limited usefulness
; if we want to allow lengths > 255.
;
;SETUP
        ; A - # of words to move from stack to N (at most 4? 3?)
        ; X - data stack pointer
        ; Y - assumed to be 0
        ; Z -
;        asl
;        sta N-1
;-       lda 0,X
;        sta N,Y
;        inx
;        iny
;        cpy N-1
;        bne -
;        ldy #0
        ; A - trashed
        ; X - data stack pointer (adjusted)
        ; Y - 0 
        ; Z -
;        rts


; Is this a whitespace character?
; Used by PARSE-NAME
isspace
        ; A - character to test
        ; TODO check newline, etc
        cmp #' '
        rts



; ****************************************************************************
; 0

; TODO remove 0 1 2 3 and just use W_CLITERAL?

;      0 1 2 3        ---  n
;               These small numbers are used so often that it is 
;               attractive to define them by name in the dictionary as 
;               constants.
;        +WORD "0"
W_ZERO
        !word DO_CONSTANT
        !word 0

; ****************************************************************************
; 1
;        +WORD "1"
W_ONE
        !word DO_CONSTANT
        !word 1

; ****************************************************************************
; 2
;        +WORD "2"
W_TWO
        !word DO_CONSTANT
        !word 2

; ****************************************************************************
; 2+

;      2+            n1  ---  n2
;               Leave n1 incremented by 2.

; TODO native implementation?

;        +WORD "2+"
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
;        +WORD "0branch"
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
; BRANCH

;      BRANCH                                                C2,L0
;               The run-time procedure to unconditionally branch.  An in-
;               line offset is added to the interpretive pointer IP to 
;               branch ahead or back.  BRANCH is compiled by ELSE, AGAIN, 
;               REPEAT.
;        +WORD "branch"
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
W_CLITERAL
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
        !word W_CLITERAL
        !byte .char
}

;
;    LITERAL pushes the next inline word to data stack
;
W_LITERAL:
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
        !word W_LITERAL
        !word .word
}

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
        ldy #U_R0       
        lda (<U),y      
        tax
        txs
        iny
        lda (<U),y
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
;      SP! (from FIG)
;               A computer dependent procedure to initialise the stack 
;               pointer from S0.
;        +WORD "sp!"
W_SPSTORE
        !word *+2
SPSTORE
        ldy #U_S0
        lda (<U),y      ; load data stack pointer (X reg) from
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