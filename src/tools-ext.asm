
; ****************************************************************************
; TOOLS EXT

!if ENABLE_TOOLS_EXT {
}

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
        +WORD_IMM ";code"
W_SCODE
        !word DO_COLON
;          !word QCSP
;          !word COMP
;          !word PSCOD
;          !word LBRAC
;          !word SMUDG
        !word W_SEMI

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
        !word W_SEMI
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

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
}

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
!if 0 {
        +WORD "bye"
W_BYE
        !word *+2
        jmp NEXT
}
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
}

; ****************************************************************************
; CS-ROLL
; (???)
; ANSI 15.6.2.1020

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; EDITOR
; (--)
; ANSI 15.6.2.1300

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; FORGET
; ("text" --)
; ANSI 15.6.2.1580   (obsolescent)

; FIG:
;      FORGET                                                E,L0
;               Executed in the form:
;                         FORGET  cccc
;               Deletes definition named cccc from the dictionary with all 
;               entries physically following it.  In fig-FORTH, an error 
;               message will occur if the CURRENT and CONTEXT vocabularies 
;               are not currently the same.
;
;;
;;                                       FORGET
;;                                       Altered from model
;;                                       SCREEN 72 LINE 6

!if ENABLE_TOOLS_EXT {
        +WORD "forget"
W_FORGET
        !word DO_COLON
;          !word TICK,NFA,DUP
;          !word FENCE,AT,ULESS,CLIT
;          !byte $15
;          !word QERR,TOR,VOCL,AT
;L3220:    !word R,OVER,ULESS
;          !word ZBRAN,L3225-*
;          !word FORTH,DEFIN,AT,DUP
;          !word VOCL,STORE
;          !word BRAN,$FFFF-24+1 ; L3220-*
;L3225:    !word DUP,CLIT
;          !byte 4
;          !word SUB
;L3228:    !word PFA,LFA,AT
;          !word DUP,R,ULESS
;          !word ZBRAN,$FFFF-14+1 ; L3228-*
;          !word OVER,TWO,SUB,STORE
;          !word AT,DDUP,ZEQU
;          !word ZBRAN,$FFFF-39+1 ; L3225-*
;          !word RFROM,DP,STORE
        !word W_SEMI
}

; ****************************************************************************
; N>R
; Forth 2012 15.6.2.1908

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
}

; ****************************************************************************
; NAME>COMPILE
; Forth 2012 15.6.2.1909.10

!if ENABLE_TOOLS_EXT {
        +word "name>compile"
}
W_NAME_TO_COMPILE
        !word DO_COLON
        ; TODO move past link and name
        !word W_SEMI

; ****************************************************************************
; NAME>INTERPRET
; Forth 2012 15.6.2.1909.20

!if ENABLE_TOOLS_EXT {
        +word "name>interpret"
}
W_NAME_TO_INTERPRET
        !word DO_COLON
        ; TODO move past link and name
        !word W_SEMI

; ****************************************************************************
; NAME>STRING
; Forth 2012 15.6.2.1909.40
; (nt -- c-addr u)

; The word itself is required by the implementation but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "name>string"
}
W_NAME_TO_STRING
        !word DO_COLON
        ; nt points to link, then name follows
        !word W_2PLUS
        !word W_DUP
        !word W_1PLUS
        +CLITERAL $1f ; mask off the control bits
        !word W_AND
        !word W_SEMI

; ****************************************************************************
; NR>
; Forth 2012 15.6.2.1940

!if ENABLE_TOOLS_EXT {
}

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
; Forth 2012 15.6.2.2297

; The word itself is required by the implementation (of FIND) but will only visible if TOOLS-EXT is enabled

!if ENABLE_TOOLS_EXT {
        +WORD "traverse-wordlist"
}
W_TRAVERSE_WORDLIST
        !word DO_COLON
        ; TODO
        !word W_SEMI

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
