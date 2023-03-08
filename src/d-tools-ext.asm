
; ****************************************************************************
; TOOLS EXT

; ****************************************************************************
; ;CODE
; (???)

; FIG:
;      ;CODE                                                 P,C,L0
;               Used in the form:
;                         : cccc  ...  ;CODE
;                              assembly mnemonics
;               Stop compilation and terminate a new defining word cccc by 
;               compiling (;CODE).  Set the CONTEXT vocabulary to 
;               ASSEMBLER, assembling to machine code the following 
;               mnemonics.
;
;               When cccc later executes in the form:
;                         cccc  nnnn
;               the word nnnn will be created with its execution procedure 
;               given by the machine code following cccc.  That is, when 
;               nnnn is executed, it does so by jumping to the code after 
;               nnnn.  An existing defining word must exist in cccc prior 
;               to ;CODE.

!if ENABLE_TOOLS_EXT {
!if 0 {
        +WORD_IMM ";code"
W_SCODE
        !word DO_COLON
;          !word QCSP
;          !word COMP
;          !word PSCOD
;          !word LBRAC
;          !word SMUDG
        !word W_PSEMI
}
}

; (;CODE)

;               The run-time procedure, compiled by ;CODE, that rewrites 
;               the code field of the most recently defined word to point 
;               to the following machine code sequence.  See ;CODE.
        +NONAME
W_PSCODE
        !word DO_COLON
        !word W_RFROM
        !word W_LATESTXT
        !word W_STORE
        !word W_PSEMI

; ****************************************************************************
; ASSEMBLER
; (--)

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; BYE
; (--)
; Return control to the host operating system

        +WORD "bye", 0
W_BYE
        !word *+2

        ; Restore the hardware stack ... an RTS should then return to basic
        lda <R0
        ldy <R0+1
        jsr RPSTORE        

        ; restore base page
        lda #0
        tab

        ; TODO restore kernel vectors (monexit in particular)

        ; TODO restore memory map?
        rts

; ****************************************************************************
; CODE
; (???)

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; CS-PICK
; (???)

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "cs-pick", 0
} else {
        +NONAME
}
W_CS_PICK
        !word W_PICK+2

; ****************************************************************************
; CS-ROLL
; (???)

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "cs-roll", 0
} else {
        +NONAME
}
W_CS_ROLL
        !word W_ROLL+2

; ****************************************************************************
; EDITOR
; (--)

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; N>R
; Forth 2012 15.6.2.1908
; (i*n +n --) (R: -- j*x +n)

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "n>r", 0
} else {
        +NONAME
}
W_NTOR
        !word *+2
        ; see also >r (core), 2>r (core-ext)
        lda 0,x
        tay
        taz
        beq +

-       lda 3,x
        pha
        lda 2,x
        pha
        inx
        inx

        dey
        bne -

+       phy
        phz
        jmp POP

; ****************************************************************************
; NAME>COMPILE
; (nt -- w xt)

; executing xt consumes w, performing the compilation semantics of nt
; w is the execution token of nt (from NAME>INTERPRET)
; xt is EXECUTE (if nt is immediate) or COMPILE, (if nt is not immediate)

!if ENABLE_TOOLS_EXT {
        +WORD "name>compile", 0
} else {
        +NONAME
}
W_NAME_TO_COMPILE
        !word DO_COLON
        !word W_DUP
        !word W_NAME_TO_XT ; this will check for zero and throw
        !word W_SWAP
        !word W_QIMMEDIATE
        +ZBRANCH +
        +LITERAL W_EXECUTE
        !word W_PSEMI
+
        +LITERAL W_COMMA ; COMPILEC?
        !word W_PSEMI

; ****************************************************************************
; NAME>INTERPRET
; (nt -- xt)

!if ENABLE_TOOLS_EXT {
        +WORD "name>interpret", 0
} else {
        +NONAME
}
W_NAME_TO_INTERPRET
        !word DO_COLON
        !word W_DUP
        !word W_NAME_TO_XT ; this will check for zero and throw
        !word W_SWAP
        !word W_QCOMPILE_ONLY
        !word W_ZEQUAL
        !word W_AND
        !word W_PSEMI

; ****************************************************************************
; NAME>STRING
; (nt -- c-addr u)

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "name>string", 0
} else {
        +NONAME
}
W_NAME_TO_STRING
        !word DO_COLON
        ; nt points to link, then name follows
        !word W_2PLUS
        !word W_COUNT
        +CLITERAL NAME_LEN_MASK ; mask off the control bits
        !word W_AND
        !word W_PSEMI

; ****************************************************************************
; NR>
; (-- i*x +n) (R: j*x +n --)

!if ENABLE_TOOLS_EXT {
        +WORD "nr>", 0
} else {
        +NONAME
}
W_NRFROM
        !word *+2
        ; see also r> (core), 2r> (core-ext)
        ; TODO
        pla
        tay
        taz
        pla

        tya
        beq +

-       dex
        dex
        pla
        sta 0,x
        pla
        sta 1,x

        dez
        bne -

+       phy
        lda #0
        jmp PUSH

; ****************************************************************************
; SYNONYM

; See reference implementation

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; TRAVERSE-WORDLIST
; (i*x xt wid -- j*x)

; The word itself is required by the implementation (of FIND) but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "traverse-wordlist", 0
} else {
        +NONAME
}
W_TRAVERSE_WORDLIST
        !word DO_COLON

        !word W_2TOR

_traverse_loop

        ; (i*x) (R: xt wid)

        !word W_RFROM ; (i*x wid) (R: xt)
        !word W_AT    ; (i*x wid') (R: xt)
        !word W_DUP
        !word W_TOR   ; (i*x wid') (R: xt wid')
        +ZBRANCH _traverse_done

        ; (i*x) (R: xt wid)

        !word W_2RAT    ; (i*x xt wid=nt) (R: xt wid)
        !word W_SWAP    ; (i*x nt xt) (R: xt wid)
        !word W_EXECUTE ; (j*x flag) (R: xt wid)

        !word W_ZEQUAL
        +ZBRANCH _traverse_loop

_traverse_done

        ; (i*x) (R: xt wid)

        !word W_2RFROM,W_2DROP ; (ix) (R:)

        !word W_PSEMI

; ****************************************************************************
; [ELSE]
; (???)

; See reference implementation

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; [IF]
; (???)

; See reference implementation

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; [THEN]
; (--)

; See reference implementation

!if ENABLE_TOOLS_EXT {
}
