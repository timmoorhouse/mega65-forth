
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

; ****************************************************************************
; ASSEMBLER
; (--)
; ANSI 15.6.2.0740

; ****************************************************************************
; BYE
; (--)
; ANSI 15.6.2.0830
; Return control to the host operating system
!if 0 {
        +WORD "bye"
W_BYE
        !word *+2
        jmp NEXT
}


; ****************************************************************************
; CODE
; (???)
; ANSI 15.6.2.0930

; ****************************************************************************
; CS-PICK
; (???)
; ANSI 15.6.2.1015

; ****************************************************************************
; CS-ROLL
; (???)
; ANSI 15.6.2.1020

; ****************************************************************************
; EDITOR
; (--)
; ANSI 15.6.2.1300

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

; ****************************************************************************
; N>R
; Forth 2012 15.6.2.1908

; ****************************************************************************
; NAME>COMPILE
; Forth 2012 15.6.2.1909.10

; ****************************************************************************
; NAME>INTERPRET
; Forth 2012 15.6.2.1909.20

; ****************************************************************************
; NAME>STRING
; Forth 2012 15.6.2.1909.40

; ****************************************************************************
; NR>
; Forth 2012 15.6.2.1940

; ****************************************************************************
; STATE
; (-- a-addr)
; ANSI 15.6.2.2250

; ****************************************************************************
; SYNONYM
; Forth 2012 15.6.2.2264

; ****************************************************************************
; TRAVERSE-WORDLIST
; Forth 2012 15.6.2.2297

; ****************************************************************************
; [DEFINED]
; Forth 2012 15.6.2.2530.30

; ****************************************************************************
; [ELSE]
; (???)
; ANSI 15.6.2.2531

; ****************************************************************************
; [IF]
; (???)
; ANSI 15.6.2.2532

; ****************************************************************************
; [THEN]
; (--)
; ANSI 15.6.2.2533

; ****************************************************************************
; [UNDEFINED]
; Forth 2012 15.6.2.2534
