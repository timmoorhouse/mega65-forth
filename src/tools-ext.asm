
; ****************************************************************************
; TOOLS EXT

; ****************************************************************************
; ;CODE
; (???)
; ANSI 15.6.2.0470

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
;
;;
;;                                       ;CODE
;;                                       SCREEN 42 LINE 6

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

; (;CODE)

;               The run-time procedure, compiled by ;CODE, that rewrites 
;               the code field of the most recently defined word to point 
;               to the following machine code sequence.  See ;CODE.

;;
;;
;;
;;                                       (;CODE)
;;                                       SCREEN 42 LINE 2
;;
!if 0 {
;        +WORD "(;code)"
W_PSCODE
        !word DO_COLON
;          !word RFROM
;          !word LATES
;          !word PFA
;          !word CFA
;          !word STORE
        !word W_PSEMI
}
}

; ****************************************************************************
; AHEAD
; (???)
; ANSI 15.6.2.0702
;
; Compilation: (C: -- orig)
;       Put the location of a new unresolved forward reference onto the control flow stack.
; Run-time: (--)
;       Continue execution at the location speicifed by the resolution of orig.
;
;
;

; This just uses the data stack for the compilation stack

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD_IMM "ahead"
} else {
        +NONAME
}
W_AHEAD
        !word DO_COLON
        !word W_HERE
        !word W_PSEMI

; ****************************************************************************
; ASSEMBLER
; (--)
; ANSI 15.6.2.0740

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; BYE
; (--)
; ANSI 15.6.2.0830
; Return control to the host operating system

!if ENABLE_TOOLS_EXT {
        !word W_BYE
        +WORD "bye"
W_BYE
        !word *+2

        ; Restore the hardware stack ... an RTS should then return to basic
        jsr RPSTORE

        ; restore base page
        lda #0
        tab

        ; TODO restore memory map?
        rts
}

; ****************************************************************************
; CODE
; (???)
; ANSI 15.6.2.0930

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; CS-PICK
; (???)
; ANSI 15.6.2.1015


; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "cs-pick"
} else {
        +NONAME
}
W_CS_PICK
        !word DO_COLON
        !word W_PICK
        !word W_PSEMI

; ****************************************************************************
; CS-ROLL
; (???)
; ANSI 15.6.2.1020

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "cs-roll"
} else {
        +NONAME
}
W_CS_ROLL
        !word DO_COLON
        !word W_ROLL
        !word W_PSEMI

; ****************************************************************************
; EDITOR
; (--)
; ANSI 15.6.2.1300

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; N>R
; Forth 2012 15.6.2.1908
; (i*n +n --) (R: -- j*x +n)

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "n>r"
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
; Forth 2012 15.6.2.1909.10

!if ENABLE_TOOLS_EXT {
        +WORD "name>compile"
} else {
        +NONAME
}
W_NAME_TO_COMPILE
        !word DO_COLON
        !word W_NAME_TO_STRING  ; immediately after the name
        !word W_ONE
        !word W_OR              ; for alignment
        !word W_PLUS
        !word W_PSEMI

; ****************************************************************************
; NAME>INTERPRET
; Forth 2012 15.6.2.1909.20

!if ENABLE_TOOLS_EXT {
        +WORD "name>interpret"
} else {
        +NONAME
}
W_NAME_TO_INTERPRET
        !word DO_COLON
        !word W_NAME_TO_STRING  ; immediately after the name
        !word W_ONE
        !word W_OR              ; for alignment
        !word W_PLUS
        !word W_PSEMI

; ****************************************************************************
; NAME>STRING
; Forth 2012 15.6.2.1909.40
; (nt -- c-addr u)

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "name>string"
} else {
        +NONAME
}
W_NAME_TO_STRING
        !word DO_COLON
        ; nt points to link, then name follows
        !word W_2PLUS
        !word W_COUNT
        +CLITERAL $1f ; mask off the control bits
        !word W_AND
        !word W_PSEMI

; ****************************************************************************
; NR>
; Forth 2012 15.6.2.1940

; (-- i*x +n) (R: j*x +n --)

!if ENABLE_TOOLS_EXT {
        +WORD "nr>"
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
; STATE
; (-- a-addr)
; ANSI 15.6.2.2250

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; SYNONYM
; Forth 2012 15.6.2.2264

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; TRAVERSE-WORDLIST
; (i*x xt wid -- j*x)
; Forth 2012 15.6.2.2297

; The word itself is required by the implementation (of FIND) but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "traverse-wordlist"
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

        !word W_ZEQUALS
        +ZBRANCH _traverse_loop

_traverse_done

        ; (i*x) (R: xt wid)

        !word W_2RFROM,W_2DROP ; (ix) (R:)

        !word W_PSEMI

; ****************************************************************************
; [DEFINED]
; Forth 2012 15.6.2.2530.30

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; [ELSE]
; (???)
; ANSI 15.6.2.2531

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; [IF]
; (???)
; ANSI 15.6.2.2532

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; [THEN]
; (--)
; ANSI 15.6.2.2533

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; [UNDEFINED]
; Forth 2012 15.6.2.2534

!if ENABLE_TOOLS_EXT {
}
