
; ****************************************************************************
; CORE

; ****************************************************************************
; ! 
; (x a-addr --)
; ANSI 6.1.0010
;
; FIG:
;               Store 16 bits of n at address.  Pronounced "store".

        +WORD "!"
W_STORE
        !word *+2

!if ENABLE_RUNTIME_CHECKS {
        ; TODO check alignment
        lda 0,x
        ror
        bcc+
        jmp fail_runtime_check
+
}

        lda 2,x
        sta (0,x)

        ; TODO we don't really need to do this since we've got an aligned address ..
        ; just in case address is ??ff ...
!if 1 {        
        inc 0,x
        bne +
        inc 1,x
+
        lda 3,x
        sta (0,x)
} else {
        lda 3,x
        sta (1,x)
}        
        jmp POP2

; ****************************************************************************
; # 
; (ud_1 -- ud_2)
; ANSI 6.1.0030

; FIG:
;      #             d1  ---  d2                             L0
;               Generate from a double number d1, the next ascii character 
;               which is placed in an output string.  Result d2 is the 
;               quotient after division by BASE, and is maintained for 
;               further processing.  Used between <# and #>.  See #S.
;
;;
;;                                       #
;;                                       SCREEN 75 LINE 9
;;

!if 0 {
        +WORD "#"
W_DIG
        !word DO_COLON
;          !word BASE
;          !word AT
;          !word MSMOD
;          !word ROT
;          !word CLITERAL
;          !byte 9
;          !word OVER
;          !word LESS
;          !word ZBRANCH
;L3513:    !word 7        ; L3517-L3513
;          !word CLITERAL
;          !byte 7
;          !word PLUS
;L3517:    !word CLITERAL
;          !byte $30
;          !word PLUS
;          !word HOLD
;          !word SEMIS
        !word W_PSEMI
}

; ****************************************************************************
; #> 
; (xd -- c-addr u)
; ANSI 6.1.0040

; FIG:
;      #>            d  ---  addr count                      L0
;               Terminates numeric output conversion by dropping d, 
;               leaving the text address and character count suitable for 
;               TYPE.
;
;;
;;                                       #>
;;                                       SCREEN 75 LINE 5
;;

!if 0 {
        +WORD "#>"
W_EDIGS
        !word DO_COLON
;          !word DROP
;          !word DROP
;          !word HLD
;          !word AT
;          !word PAD
;          !word OVER
;          !word SUB
        !word W_PSEMI
}

; ****************************************************************************
; #S 
; (ud_1 -- ud_2)
; ANSI 6.1.0050

; FIG:
;
;
;      #S            d1  ---  d2                             L0
;               Generates ascii text in the text output buffer, by the use 
;               of #, until a zero double number n2 results.  Used between 
;               <# and #>.
;
;
;;                                       #S
;;                                       SCREEN 75 LINE 12
;;

!if 0 {
        +WORD "#s"
W_DIGS
        !word DO_COLON
;L3529:    !word DIG
;          !word OVER
;          !word OVER
;          !word OR
;          !word ZEQU
;          !word ZBRANCH
;L3535:    !word $FFF4    ; L3529-L3535
        !word W_PSEMI
}

; ****************************************************************************
; ' 
; ("text" -- xt)
; ANSI 6.1.0070

; FIG:
;
;      '             ---  addr                               P,L0
;               Used in the form:
;                         '  nnnn
;               Leaves the parameter field address of dictionary word 
;               nnnn.  As a compiler directive, executes in a colon 
;               definition to compile the address as a literal.  If the 
;               word is not found after a search of CONTEXT and CURRENT, 
;               an appropriate error message is given.  Pronounced "tick".
;
;
;;
;;                                       '
;;                                       SCREEN 72 LINE 2
;;

!if 0 {
        +WORD "'"
W_TICK
        !word DO_COLON



;          !word DFIND
;          !word ZEQU
;          !word ZERO
;          !word QERR
;          !word DROP
;          !word LITERAL
        !word W_PSEMI
}

; ****************************************************************************
; ( 
; ("text" --)
; ANSI 6.1.0080
; ANSI 11.6.1.0080 (extensions in FILE)

; TODO allow multiline comment blocks when parsing from a file

; FIG:
;      (                                                      P,L0
;               Used in the form:
;                         ( cccc)
;               Ignore a comment that will be delimited by a right 
;               parenthesis on the same line.  May occur during execution 
;               or in a colon-definition.  A blank after the leading 
;               parenthesis is required.

        +WORD_IMM "("
W_PAREN
        !word DO_COLON
        +CLITERAL ')'
        !word W_PARSE
!if DEBUG {
        !word W_PDOTQ
        +STRING "<comment>"
        +CLITERAL '['
        !word W_EMIT
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT
        !word W_DOTS,W_CR
}
        !word W_2DROP
        !word W_PSEMI

; ****************************************************************************
; * 
; (n_1 n_2 -- n_3)
; ANSI 6.1.0090

; FIG:
;      *             n1  n2  ---  prod                       L0
;               Leave the signed product of two signed numbers.

        +WORD "*"
W_STAR
        !word *+2
        ; TODO FIX FOR NEGATIVE VALUES !!!!!!!!!!
        lda 0,x
        sta MULTINA
        lda 2,x
        sta MULTINB
        lda 1,x
        sta MULTINA+1
        lda 3,x
        sta MULTINB+1
        ; lda #0
        ; sta MULTINA+2 ; No need to set these since they'll only affect MULTOUT+2 and MULTOUT+3
        ; sta MULTINB+2
        ; sta MULTINB+3
        ; sta MULTINA+3
        ; No need to check MULBUSY (d70f bit 6)
        lda MULTOUT
        sta 2,x
        lda MULTOUT+1
        sta 3,x
        jmp POP

; ****************************************************************************
; */
; (n_1 n_2 -- n_3)
; ANSI 6.1.0100

; FIG:
;      */            n1  n2  n3  ---  n4                     L0
;               Leave the ratio n4 = n1*n2/n3 where all are signed 
;               numbers.  Retention of an intermediate 31 bit product 
;               permits greater accuracy than would be available with the 
;               sequence:  n1 n2 * n3 /
;
;;
;;                                       */
;;                                       SCREEN 57 LINE 13
;;

!if 0 {
        +WORD "*/"
W_SSLASH
        !word DO_COLON
;          !word SSMOD
;          !word SWAP
;          !word DROP
        !word W_PSEMI
}

; ****************************************************************************
; */MOD 
; (n_1 n_2 n_3 -- n_4 n_5)
; ANSI 6.1.0110

; FIG:
;      */MOD         n1  n2  n3  ---  n4  n5                 L0
;               Leave the quotient n5 and remainder n4 of the operation 
;               n1*n2/n3.  A 31 bit intermediate product is used as for 
;               */.
;
;;
;;                                       */MOD
;;                                       SCREEN 57 LINE 11
;;

!if 0 {
        +WORD "*/mod"
W_SSMOD
        !word DO_COLON
;          !word TOR
;          !word MSTAR
;          !word RFROM
;          !word MSLAS
        !word W_PSEMI
}

; ****************************************************************************
; + 
; (n_1 n_2 -- n_3)
; ANSI 6.1.0120

; FIG:
;      +             n1  n2  ---  sum                        L0
;               Leave the sum of n1+n2.

        +WORD "+"
W_PLUS
        !word *+2
        clc
        lda 0,x
        adc 2,x
        sta 2,x
        lda 1,x
        adc 3,x
        sta 3,x
        jmp POP

; ****************************************************************************
; +! 
; (n a-addr --)
; ANSI 6.1.0130

; FIG:
;      +!            n  addr  ---                            L0
;               Add n to the value at the address.  Pronounced "plus-
;               store".

        +WORD "+!"
W_PSTORE
        !word *+2
        clc
        lda (0,x)
        adc 2,x
        sta (0,x)

!if ENABLE_RUNTIME_CHECKS {
        ; TODO check alignment
}

        ; TODO Like for ! do we need to do this when we have an aligned address? 
        ; in case address is ??ff ...
        inc 0,x
        bne +
        inc 1,x

+       lda (0,x)
        adc 3,x
        sta (0,x)
        jmp POP2

; ****************************************************************************
; +LOOP 
; (n --)
; ANSI 6.1.0140

; FIG:
;      +LOOP               n1  ---       (run)
;                    addr  n2  ---       (compile)           P,C2,L0
;               Used in a colon-definition in the form:
;                         DO  ...  n1  +LOOP
;               At run-time, +LOOP selectively controls branching back to 
;               the corresponding DO based on n1, the loop index and the 
;               loop limit.  The signed increment n1 is added to the index 
;               and the total compared to the limit.  The branch back to 
;               DO occurs until the new index is equal to or greater than 
;               the limit (n1>0), or until the new index is equal to or 
;               less than the limit (n1<0).  Upon exiting the loop, the 
;               parameters are discarded and execution continues ahead.
;
;               At compile time, +LOOP compiles the run-time word (+LOOP) 
;               and the branch offset computed from HERE to the address 
;               left on the stack by DO.  n2 is used for compile-time 
;               error checking.
;
;;
;;                                       +LOOP
;;                                       SCREEN 73 LINE 13

!if 0 {
        +WORD_IMM "+loop"
W_PLUS_LOOP
        !word DO_COLON
;          !word THREE
;          !word QPAIR
;          !word COMPILE
;          !word PPLOO
;          !word BACK
        !word W_PSEMI
}

;      (+LOOP)       n  ---                                  C2
;               The run-time procedure compiled by +LOOP, which increments 
;               the loop index by n and tests for loop completion.  See 
;               +LOOP.

;;
;;                                       (+LOOP)
;;                                       SCREEN 16 LINE 8
;;
!if 0 {
;        +WORD "(+loop)"
W_PPLOOP
        !word *+2
;          INX
;          INX
;          STX XSAVE
;          LDA $FF,X
;          PHA
;          PHA
;          LDA $FE,X
;          TSX
;          INX
;          INX
;          CLC
;          ADC $101,X
;          STA $101,X
;          PLA
;          ADC $102,X
;          STA $102,X
;          PLA
;          BPL PL1
;          CLC
;          LDA $101,X
;          SBC $103,X
;          LDA $102,X
;          SBC $104,X
;          JMP PL2
}

; ****************************************************************************
; , 
; (x --)
; ANSI 6.1.0150

; FIG:
;      ,             n  ---                                  L0
;               Store n into the next available dictionary memory cell, 
;               advancing the dictionary pointer.  (comma)

        +WORD ","
W_COMMA
        !word DO_COLON
        !word W_HERE
        !word W_STORE   ; TODO check if aligned?
        !word W_TWO ; TODO W_ONE,W_CELLS?
        !word W_ALLOT
        !word W_PSEMI

; ****************************************************************************
; - 
; (n_1 n_2 -- n_3)
; ANSI 6.1.0160

; FIG:
;      -             n1  n2  ---  diff                       L0
;               Leave the difference of n1-n2.

        +WORD "-"
W_SUB
        !word *+2
        sec
        lda 2,x
        sbc 0,x
        sta 2,x
        lda 3,x
        sbc 1,x
        sta 3,x
        jmp POP

; ****************************************************************************
; . 
; (n --)
; ANSI 6.1.0180

; FIG:
;      .             n  ---                                  L0
;               Print a number from a signed 16 bit two's complement 
;               value, converted according to the numeric BASE.  A 
;               trailing blank follows.  Pronounced "dot".

        +WORD "."
W_DOT
!if 0 {
        !word DO_COLON
        !word W_STOD
        !word W_DDOT
        !word W_PSEMI
} else {
        !word *+2
        lda 1,x
        jsr put_hex
        lda 0,x
        jsr put_hex
        jmp POP
}

; ****************************************************************************
; ." 
; ("text" --)
; ANSI 6.1.0190

; FIG:
;      ."                                                    P,L0
;               Used in the form:
;                         ." cccc"
;               Compiles an in-line string cccc (delimited by the trailing 
;               ") with an execution procedure to transmit the text to the 
;               selected output device.  If executed outside a definition, 
;               ." will immediately print the text until the final ".  The 
;               maximum number of characters may be an installation 
;               dependent value.  See (.").
;
;;
;;                                       ."
;;                                       SCREEN 44 LINE12
;;

!if 0 {
        +WORD_IMM ".\""
W_DOTQUOTE
        !word DO_COLON
;          !word CLITERAL
;          !byte $22
;          !word STATE
;          !word AT
;          !word ZBRANCH
;L1709:    !word $14      ;L1719-L1709
;          !word COMPILE
;          !word PDOTQ
;          !word WORD
;          !word HERE
;          !word CAT
;          !word 1PLUS
;          !word ALLOT
;          !word BRANCH
;L1718:    !word $A       ;L1723-L1718
;L1719:    !word WORD
;          !word HERE
;          !word COUNT
;          !word TYPE
        !word W_PSEMI
}

; (.")
;               The run-time procedure, compiled by ." which transmits the 
;               following in-line text to the selected output device.  See 
;               ."

;;
;;                                       (.")
;;                                       SCREEN 44 LINE 8
;        +WORD "(.\")"
W_PDOTQ
        !word DO_COLON
        !word W_RAT
        !word W_COUNT
        !word W_DUP
        !word W_1PLUS ; account for length field
        !word W_RFROM
        !word W_PLUS
        !word W_TOR
        !word W_TYPE
        !word W_PSEMI

; ****************************************************************************
; / 
; (n_1 n_2 -- n_3)
; ANSI 6.1.0230

; FIG:
;      /             n1  n2  ---  quot                       L0
;               Leave the signed quotient of n1/n2.

        +WORD "/"
W_SLASH
        !word *+2
        ; TODO does this handle negative values?
        lda 0,x
        sta MULTINB
        lda 2,x
        sta MULTINA
        lda 1,x
        sta MULTINB+1
        lda 3,x
        sta MULTINA+1
        lda #0
        sta MULTINA+2
        sta MULTINB+2
        sta MULTINA+3
        sta MULTINB+3
        ; wait for DIVBUSY to go low
-       bit $d70f
        bmi -
        lda DIVOUT+4 ; want the whole part
        sta 2,x
        lda DIVOUT+5
        sta 3,x
        jmp POP

; ****************************************************************************
; /MOD 
; (n_1 n_2 -- n_3 n_4)
; ANSI 6.1.0240

; FIG:
;      /MOD          n1  n2  ---  rem quot                   L0
;               Leave the remainder and signed quotient of n1/n2.  The 
;               remainder has the sign of the dividend.
;
;;
;;                                       /MOD
;;                                       SCREEN 57 LINE 8
;;

!if 0 {
        +WORD "/mod"
W_SLMOD
        !word DO_COLON
;          !word TOR
;          !word STOD
;          !word RFROM
;          !word MSLAS
        !word W_PSEMI
}

; ****************************************************************************
; 0< 
; (n -- flag)
; ANSI 6.1.0250

; FIG:
;      0<            n  ---  f                               L0
;               Leave a true flag if the number is less than zero 
;               (negative), otherwise leave a false flag.

        +WORD "0<"
W_ZLESS
        !word *+2
        ; ldy #0 ; TODO
        asl 1,x
        tya
        rol
        sty 1,x
        sta 0,x 
        jmp NEXT

; ****************************************************************************
; 0= 
; (x -- flag)
; ANSI 6.1.0270

; FIG:
;      0=            n  ---  f                               L0
;               Leave a true flag if the number is equal to zero, 
;               otherwise leave a false flag.

        +WORD "0="
W_ZEQUALS
        !word *+2
        ; see also 0<> (core-ext)
        ; ldy #0 ; TODO
        lda 1,x
        sty 1,x
        ora 0,x
        bne +
        iny
+       sty 0,x
        jmp NEXT

; ****************************************************************************
; 1+ 
; (n_1 -- n_2)
; ANSI 6.1.0290

; FIG:
;      1+            n1  ---  n2                             L1
;               Increment n1 by 1.

        +WORD "1+"
W_1PLUS
        !word *+2
        inc 0,x
        bne +
        inc 1,x
+       jmp NEXT

; ****************************************************************************
; 1- 
; (n_1 -- n_2)
; ANSI 6.1.0300

        +WORD "1-"
W_1MINUS
        !word *+2
        lda 0,x
        bne +
        dec 1,x
+       dec 0,x
        jmp NEXT

; ****************************************************************************
; 2! 
; (x_1 x_2 a-addr --)
; ANSI 6.1.0310

!if 0 {
        +WORD "2!"
W_2STORE
        !word *+2
        rts
}

; ****************************************************************************
; 2* (x_1 -- x_2)
; ANSI 6.1.0320

        +WORD "2*"
W_2STAR
        !word *+2
        asl 0,x
        rol 1,x
        jmp NEXT

; ****************************************************************************
; 2/ 
; (x_1 -- x_2)
; ANSI 6.1.0330

        +WORD "2/"
W_2SLASH
        !word *+2
        asr 1,x
        ror 0,x
        jmp NEXT

; ****************************************************************************
; 2@ 
; (a-addr -- x_1 x_1)
; ANSI 6.1.0350

!if 0 {
        +WORD "2@"
        !word *+2
        rts
}

; ****************************************************************************
; 2DROP 
; (x_1 x_2 --)
; ANSI 6.1.0370

        +WORD "2drop"
W_2DROP
        !word *+2
        jmp POP2

; ****************************************************************************
; 2DUP 
; (x_1 x_2 -- x_1 x_2 x_1 x_2)
; ANSI 6.1.0380

; TODO native implementation?

        +WORD "2dup"
W_2DUP
        !word DO_COLON
        !word W_OVER
        !word W_OVER
        !word W_PSEMI

; ****************************************************************************
; 2OVER 
; (x_1 x_2 x_3 x_4 -- x_1 x_2 x_3 x_4 x_1 x_2)
; ANSI 6.1.0400

!if 0 {
        +WORD "2over"
W_2OVER
        !word *+2
        rts
}

; ****************************************************************************
; 2SWAP 
; (x_1 x_2 x_3 x_4 -- x_3 x_4 x_1 x_2)
; ANSI 6.1.0430

!if 0 {
        +WORD "2swap"
W_2SWAP
        !word *+2
        rts
}

; ****************************************************************************
; : 
; (???)
; ANSI 6.1.0450

; FIG:
;      :                                                     P,E,L0
;               Used in the form called a colon-definition:
;                         : cccc   ...  ;
;               Creates a dictionary entry defining cccc as equivalent to 
;               the following sequence of Forth word definitions '...' 
;               until the next ';' or ';CODE'.  The compiling process is 
;               done by the text interpreter as long as STATE is non-zero.  
;               Other details are that the CONTEXT vocabulary is set to 
;               the CURRENT vocabulary and that words with the precedence 
;               bit set (P) are executed rather than being compiled.
;
;;
;;                                       :
;;                                       SCREEN 33 LINE 2

        +WORD_IMM ":"
W_COLON
        !word DO_COLON
;          !word QEXEC
;          !word SCSP  ; !csp
;          !word CURRENT
;          !word AT
        ; !word W_GET_CURRENT ; context - change to get-current?
        ; !word W_STORE
        !word W_CREATE
        !word W_RBRACKET
;          !word PSCOD
; !word SEMIS?
        !word W_PSEMI

; ****************************************************************************
; ; 
; (???)
; ANSI 6.1.0460

; FIG:
;      ;                                                     P,C,L0
;               Terminate a colon-definition and stop further compilation.  
;               Compiles the run-time ;S.
;

;      ;S                                                    P,L0
;               Stop interpretation of a screen.  ;S is also the run-time 
;               word compiled at the end of a colon-definition which 
;               returns execution to the calling procedure.

;;
;;                                       ;
;;                                       SCREEN 33 LINE 9

        +WORD_IMM ";"
W_SEMI 
        !word DO_COLON
;          !word QCSP
        !word W_POSTPONE
        !word W_PSEMI
;          !word SMUDG
        !word W_LBRACKET
        !word W_PSEMI

        +WORD ";s" ; TODO
W_PSEMI
        !word *+2
        pla
        sta <I
        pla
        sta <I+1
        jmp NEXT

; ****************************************************************************
; < 
; (n_1 n_2 -- flag)
; ANSI 6.1.0480

; FIG:
;      <             n1  n2  ---  f                          L0
;               Leave a true flag if n1 is less than n2; otherwise leave a 
;               false flag.

        +WORD "<"
W_LESS
        !word *+2
        ; ldy #0 ; TODO
        sec
        lda 2,x
        sbc 0,x
        lda 3,x
        sbc 1,x

        sty 3,x
        bvc +
        eor #$80
+       bpl +
        iny
+       sty 2,x
        jmp POP

; ****************************************************************************
; <# 
; (--)
; ANSI 6.1.0490

; FIG:
;      <#                                                    L0
;               Setup for pictured numeric output formatting using the 
;               words:
;                         <#  #  #S  SIGN  #>
;               The conversion is done on a double number producing text 
;               at PAD.
;
;;
;;                                       <#
;;                                       SCREEN 75 LINE 3
;;

!if 0 {
        +WORD "<#"
W_BDIGS
        !word DO_COLON
;          !word PAD
;          !word HLD
;          !word STORE
        !word W_PSEMI
}

; ****************************************************************************
; = 
; (x_1 x_2 -- flag)
; ANSI 6.1.0530

; FIG:
;      =             n1  n2  ---  f                          L0
;               Leave a true flag if n1=n2; otherwise leave a false flag.

        +WORD "="
W_EQUAL
        !word *+2
        ; see also <> (core-ext)
        ; ldy #0 ; TODO
        lda 0,x
        eor 2,x
        sta 2,x

        lda 1,x
        eor 3,x
        sty 3,x
        ora 2,x

        bne +
        iny
+       sty 2,x
        jmp POP

; ****************************************************************************
; > 
; (n_1 n_2 -- flag)
; ANSI 6.1.0540

; FIG:
;      >             n1  n2  ---  f                          L0
;               Leave a true flag if n1 is greater than n2; otherwise a 
;               false flag.

        +WORD ">"
W_GREATER
        !word DO_COLON
        !word W_SWAP
        !word W_LESS
        !word W_PSEMI

; ****************************************************************************
; >BODY 
; (xt -- a-addr)
; ANSI 6.1.0550

        +WORD ">body"
W_TOBODY
        !word DO_COLON
        !word W_2PLUS   ; skip code field
        !word W_PSEMI

; ****************************************************************************
; >IN 
; (-- a-addr)
; ANSI 6.1.0560

; FIG
;      IN            ---  addr                               L0
;               A user variable containing the byte offset within the 
;               current input text buffer (terminal or disc) from which 
;               the next text will be accepted.  WORD uses and moves the 
;               value of IN.

        +WORD ">in"
W_IN
        !word DO_COLON
        +LITERAL &IN
        !word W_PSEMI

; ****************************************************************************
; >NUMBER 
; (ud_1 c-addr_1 u_1 -- ud_2 c-addr_2 u_2)
; ANSI 6.1.0570

; c-addr_2 u_2 is the unconverted portion of c-addr_1 u_1

W_TONUMBER_TEST
        !word DO_COLON

        !word W_HEX
!if 1 {
        !word W_PDOTQ
        +STRING "<base>"
        !word W_BASE
        !word W_AT
        !word W_DOTS,W_CR
        !word W_DROP
}

        +CLITERAL 'f'
        !word W_PDOTQ
        +STRING "<digit>"
        !word W_DUP
        !word W_EMIT
        !word W_DIGIT
        !word W_DOTS,W_CR
        !word W_2DROP

!if 1 {
        +LITERAL 0
        +LITERAL 0
        +LITERAL _tonumber_test
        !word W_COUNT
        !word W_PDOTQ
        +STRING "<number>"
        +CLITERAL '['
        !word W_EMIT
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT
        !word W_TONUMBER
        +CLITERAL '['
        !word W_EMIT
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'        
        !word W_EMIT
        !word W_DOTS,W_CR
}
        !word W_PSEMI

_tonumber_test
        ;+STRING "1xyz"
        ; +STRING "1234abcxyz"
        +STRING "1234abc"
        !byte 'd'

; FIG
;      (NUMBER)      d1  addr1  ---  d2  addr2
;               Convert the ascii text beginning at addr1+1 with regard to 
;               BASE.  The new value is accumulated into double number d1,  
;               being left as d2.  Addr2 is the address of the first 
;               unconvertable digit.  Used by NUMBER.

        +WORD ">number"
W_TONUMBER
        !word DO_COLON
 
_tonumber_loop
        ; (ud c-addr u) = (ud-low ud-high c-addr u)

        !word W_2DUP
        !word W_2TOR    ; (ud c-addr u) (R: c-addr u)

        !word W_QDUP
        +ZBRANCH _tonumber_done_1drop ; reached end of string

        !word W_DROP
        !word W_CAT    ; (ud c) (R: c-addr u)
        !word W_DIGIT
        +ZBRANCH _tonumber_done_0drop ; reached invalid char

        !word W_SWAP ; (ud-low n ud-high) (R: c-addr)

        ; TODO some function for this?
        ; ud * u -> ud
        !word W_BASE
        !word W_AT
        !word W_UMSTAR
        !word W_DROP
        !word W_ROT
        !word W_BASE
        !word W_AT
        !word W_UMSTAR
        !word W_DPLUS

!if 0 {
        !word DPL ; # digits to right of decimal place
        !word W_AT
        !word W_1PLUS
        +ZBRANCH +
        !word W_ONE
        !word DPL
        !word W_PSTORE
+
}

        ; (ud) (R: c-addr u)

        !word W_2RFROM
        ; (ud c-addr u)
        !word W_SWAP
        !word W_1PLUS
        !word W_SWAP
        !word W_1MINUS

        +BRANCH _tonumber_loop

_tonumber_done_1drop
        ; (ud c-addr) (R: c-addr u)
        !word W_DROP
_tonumber_done_0drop
        ; (ud) (R: c-addr u)
        !word W_2RFROM
        !word W_PSEMI


;      NUMBER_FOO        addr  ---  d
;               Convert a character string left at addr with a preceding 
;               count, to a signed double number, using the current base.  
;               If a decimal point is encountered in the text, its 
;               position will be given in DPL, but no other effect occurs.  
;               If numeric conversion is not possible, an error message 
;               will be given.

W_TONUMBER_FOO
        !word DO_COLON
!if 1 {
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
}
        !word W_PSEMI


;      DIGIT         (c -- n 1)    if ok
;                    (c -- 0)      if bad
;               Converts the ascii character c (using BASE) to its 
;               binary equivalent n, accompanied by a true flag.  If the 
;               conversion is invalid, leaves only a false flag.

        ; +WORD "digit"
W_DIGIT
        !word *+2
        ; ldy #0 ; TODO
        sec
        lda 0,x
        sbc #'0'
        bmi _digit_bad ; < '0'

        cmp #$A
        bmi +
        ; > '9'
        sec
        sbc #('a'-'9'-1)
        cmp #$A
        bmi _digit_bad ; in between '9' and 'a'
+
        cmp <BASE
        bpl _digit_bad ; >= base

        sta 0,x
        sty 1,x
        lda #1
        pha
        tya
        jmp PUSH         ; exit true with converted value

_digit_bad
        tya
        pha
        jmp PUT         ; exit false with bad conversion

; ****************************************************************************
; >R 
; (x --) (R: -- x)
; ANSI 6.1.0580

; FIG
;      >R            n  ---                                  C,L0
;               Remove a number from the computation stack and place as 
;               the most accessable on the return stack.  Use should be 
;               balanced with R> in the same definition.

        +WORD ">r"
W_TOR
        !word *+2
        ; see also 2>r (core-ext), n>r (tools-ext)
        lda 1,x
        pha
        lda 0,x
        pha
        jmp POP

; ****************************************************************************
; ?DUP 
; (x -- 0 | x x)
; ANSI 6.1.0630

; FIG
;      -DUP          n1  ---  n1         (if zero)
;                    n1  ---  n1  n1     (non-zero)          L0
;               Reproduce n1 only if it is non-zero.  This is usually used 
;               to copy a value just before IF, to eliminate the need for 
;               an ELSE part to drop it.

        +WORD "?dup"
W_QDUP
        !word *+2
        lda 0,x
        ora 1,x
        bne +
        jmp NEXT
+       lda 0,x
        pha
        lda 1,x
        jmp PUSH

; ****************************************************************************
; @ 
; (a-addr -- x)
; ANSI 6.1.0650

; FIG:
;      @             addr  ---  n                            L0
;               Leave the 16 bit contents of address.

        +WORD "@"
W_AT
        !word *+2

!if ENABLE_RUNTIME_CHECKS {
        ; TODO check alignment
        lda 0,x
        ror
        bcc+
        jmp fail_runtime_check
+
}

        lda (0,x)
        pha

        ; TODO like for ! can we skip this since we have an aligned address?
        ; in case address is ??ff ...
        inc 0,x
        bne +
        inc 1,x

+       lda (0,x)
        jmp PUT

; ****************************************************************************
; ABORT 
; (???)
; ANSI 6.1.0670
;
; Empties the data stack and performs a QUIT

; FIG
;      ABORT                                                 L0
;               Clear the stacks and enter the execution state.  Return 
;               control to the operators terminal, printing a message 
;               appropriate to the installation.

        +WORD "abort"
W_ABORT
        !word DO_COLON
        !word W_SPSTORE
        !word W_DECIMAL
!if ENABLE_BLOCK {
;          !word DR0 ; from fig
}
;          !word FORTH ; from search-ext
;          !word DEFIN ; from search
        +BRANCH QUIT

; ****************************************************************************
; ABORT" 
; (???)
; ANSI 6.1.0680

!if 0 {
        +WORD "abort\""
        !word *+2
        rts
}

; ****************************************************************************
; ABS 
; (n -- u)
; ANSI 6.1.0690


; FIG:
;      ABS           n  ---  u                               L0
;               Leave the absolute value of n as u.

        +WORD "abs"
W_ABS
        !word DO_COLON
        !word W_DUP
        !word W_ZLESS
        +ZBRANCH +
        !word W_NEGATE
+       !word W_PSEMI

; ****************************************************************************
; ACCEPT 
; (c-addr +n_1 -- +n_2)
; ANSI 6.1.0695

; pretty much replaces the obsolescent EXPECT from core-ext

; input terminates on newline or if we reach the character limit
; characters are displayed as they are received (ie, we can assume keyboard input only)

; TODO who should be checking BLK, SOURCE-ID?

; TODO use REFILL?

        +WORD "accept"
W_ACCEPT
        !word DO_COLON

        ; Loop index is a pointer to the buffer entry
        !word W_OVER
        !word W_PLUS
        !word W_OVER
        !word W_PDO

        !word W_DROP 
        !word W_ZERO
        
        ; (0)

!ifdef COLOUR_INPUT {
        +CLITERAL COLOUR_INPUT
        !word W_FOREGROUND
}

_accept_loop

        ; TODO get rid of index on stack and use OUT?
        ; (index)

        !word W_KEY

        ; (index key)

!if 0 {
        !word W_SPACE,W_DUP,W_DOT,W_SPACE
        !word W_DOTS,W_CR
}

!if 0 { ; don't need any of this if using BASIN (line buffered)...

        ; Check for and handle delete
        !word W_DUP
        !word W_K_DELETE
        !word W_EQUAL

        +ZBRANCH _accept_not_delete
        ; It's a delete ...

!if 0 {
        ; TODO subtract 2 from loop index
        ; TODO change put_char to handle backspace
        ; TODO subtract 1 from index
}

;          !word DROP
;          !word CLITERAL
;          !byte 08
;          !word OVER
;          !word I
;          !word EQUAL
;          !word DUP
;          !word RFROM
;          !word TWO
;          !word SUB
;          !word PLUS
;          !word TOR
;          !word SUB

        +BRANCH _accept_do_emit
_accept_not_delete
}

        ; Check for and handle return
        !word W_DUP
        !word W_K_RETURN
        !word W_EQUAL
        +ZBRANCH _accept_not_return

        !word W_DROP ; drop the CR
        !word W_LEAVE
        !word _accept_after_loop-*
_accept_not_return

        ; (index key)

        ; A normal character, add it to the buffer

        !word W_DUP
        !word W_I
        !word W_CSTORE

        ; (index key)

_accept_do_emit

!if 0 { ; will already have been printed
        !word W_EMIT 
} else {
        !word W_DROP
}

        ; (index)

        !word W_1PLUS

        !word W_PLOOP
        !word _accept_loop-*
_accept_after_loop ; TODO remove        

!if 0 { ; will already have been printed
        !word W_CR
}

!ifdef COLOUR_OUTPUT {
        +CLITERAL COLOUR_OUTPUT
        !word W_FOREGROUND
}

!if 1 {
        ; TODO this is a hack - why do we need to do this???
        +CLITERAL 29
        !word W_EMIT
        +CLITERAL 29
        !word W_EMIT
}

        ; left with index (ie final count)

        !word W_PSEMI

; ****************************************************************************
; ALIGN 
; (--)
; ANSI 6.1.0705

        +WORD "align"
W_ALIGN
        !word *+2
        bbr0 <HERE, +
        inc <HERE
        bne +
        inc <HERE+1
+
        jmp NEXT

; ****************************************************************************
; ALIGNED 
; (addr -- a-addr)
; ANSI 6.1.0706

        +WORD "aligned"
W_ALIGNED
        !word *+2
        lda 0,x
        and #1
        beq +
        inc 0,x
        beq +
        inc 1,x
+
        jmp NEXT

; ****************************************************************************
; ALLOT 
; (n --)
; ANSI 6.1.0710

; FIG:
;      ALLOT         n  ---                                  L0
;               Add the signed number to the dictionary pointer DP.  May 
;               be used to reserve dictionary space or re-origin memory.  
;               n is with regard to computer address type (byte or word).

        +WORD "allot"
W_ALLOT
        !word DO_COLON
        +LITERAL &HERE
        !word W_PSTORE
        !word W_PSEMI

; ****************************************************************************
; AND 
; (x_1 x_2 -- x_3)
; ANSI 6.1.0720

; FIG:
;      AND           n1  n2  ---  n3                         L0
;               Leave the bitwise logical and of n1 and n2 as n3.

        +WORD "and"
W_AND
        !word *+2
        lda 0,x
        and 2,x
        sta 2,x
        lda 1,x
        and 3,x
        sta 3,x
        jmp POP

; ****************************************************************************
; BASE 
; (-- a-addr)
; ANSI 6.1.0750

; FIG:
;
      ;BASE          ---  addr                               U,L0
      ;         A user variable containing the current number base used 
      ;         for input and output conversion.

        +WORD "base"
W_BASE
        !word DO_CONSTANT
        !word &BASE

; ****************************************************************************
; BEGIN 
; (??)
; ANSI 6.1.0760

; FIG:
;
;
;      BEGIN         ---  addr  n        (compiling)         P,L0
;               Occurs in a colon-definition in form:
;                         BEGIN  ...  UNTIL
;                         BEGIN  ...  AGAIN
;                         BEGIN  ...  WHILE  ...  REPEAT
;               At run-time, BEGIN marks the start of a sequence that may 
;               be repetitively executed.  It serves as a return point 
;               from the corresponding UNTIL, AGAIN or REPEAT.  When 
;               executing UNTIL, a return to BEGIN will occur if the top 
;               of the stack is false; for AGAIN and REPEAT a return to 
;               BEGIN always occurs.
;
;               At compile time BEGIN leaves its return address and n for 
;               compiler error checking.
;
;;
;;                                       BEGIN
;;                                       SCREEN 73 LINE 3

!if 0 {
        +WORD_IMM "begin"
W_BEGIN
        !word DO_COLON
;          !word QCOMP
;          !word HERE
;          !word ONE
        !word W_PSEMI
}

; ****************************************************************************
; BL 
; (-- char)
; ANSI 6.1.0770

; FIG:
;
;
;      BL            ---  c
;               A constant that leaves the ascii value for "blank".

        +WORD "bl"
W_BL
        !word DO_CONSTANT
        !word ' '

; ****************************************************************************
; C! 
; (char c-addr --)
; ANSI 6.1.0850

; FIG:
;
;      C!            b  addr  ---
;               Store 8 bits at address.  On word addressing computers, 
;               further specification is necessary regarding byte 
;               addressing.

        +WORD "c!"
W_CSTORE
        !word *+2
        lda 2,x
        sta (0,x)
        jmp POP2

; ****************************************************************************
; C, 
; (char --)
; ANSI 6.1.0860

; FIG:
;
;      C,            b  ---
;               Store 8 bits of b into the next available dictionary byte, 
;               advancing the dictionary pointer.  This is only available 
;               on byte addressing computers, and should be used with 
;               caution on byte addressing minicomputers.

        +WORD "c,"
W_CCOMM
        !word DO_COLON
        !word W_HERE
        !word W_CSTORE
        !word W_ONE
        !word W_ALLOT
        !word W_PSEMI

; ****************************************************************************
; C@ 
; (c-addr -- char)
; ANSI 6.1.0870

; FIG:
;
;      C@            addr  ---  b
;               Leave the 8 bit contents of memory address.  On word 
;               addressing computers, further specification is needed 
;               regarding byte addressing.

        +WORD "c@"
W_CAT
        !word *+2
        ; ldy #0 ; TODO
        lda (0,x)
        sta 0,x
        sty 1,x
        jmp NEXT

; ****************************************************************************
; CELL+ 
; (a-addr_1 -- a-addr_2)
; ANSI 6.1.0880

        +WORD "cell+"
W_CELLP
        !word DO_COLON
        !word W_2PLUS
        !word W_PSEMI

; ****************************************************************************
; CELLS 
; (n_1 -- n_2)
; ANSI 6.1.0890

        +WORD "cells"
W_CELLS        
        !word DO_COLON
        !word W_2STAR
        !word W_PSEMI

; ****************************************************************************
; CHAR 
; ("text" -- char)
; ANSI 6.1.0895

        +WORD_IMM "char"
W_CHAR
        !word DO_COLON
        !word W_PARSE_NAME
        ; TODO what if length is 0?
        !word W_DROP
        !word W_CAT
        !word W_PSEMI

; ****************************************************************************
; CHAR+ 
; (c-addr_1 -- c-addr_2)
; ANSI 6.1.0897

        +WORD "char+"
W_CHARP
        !word DO_COLON
        !word W_1PLUS
        !word W_PSEMI

; ****************************************************************************
; CHARS 
; (n_1 -- n_2)
; ANSI 6.1.0898

        +WORD "chars"
W_CHARS
        !word DO_COLON  ; no-op
        !word W_PSEMI

; ****************************************************************************
; CONSTANT 
; (??)
; ANSI 6.1.0950

; FIG:
;
;      CONSTANT      n  ---                                  L0
;               A defining word used in the form:
;                         n  CONSTANT  cccc
;               to create word cccc, with its parameter field containing 
;               n.  When cccc is later executed, it will push the value of 
;               n to the stack.
;
;;
;;                                       CONSTANT
;;                                       SCREEN 34 LINE 1
;;

!if 0 {
        +WORD "constant"
W_CONSTANT
;   !word DOCOL
;          !word CREATE
;          !word SMUDGE
;          !word COMMA
;          !word PSCOD
; SEMIS?
        !word *+2
        rts
}

; ****************************************************************************
; COUNT 
; (c-addr_1 -- c-addr_2 u)
; ANSI 6.1.0980

; FIG:
;
;
;      COUNT         addr1  ---  addr2  n                    L0
;               Leave the byte address addr2 and byte count n of a message 
;               text beginning at addr1.  It is presumed that the first 
;               byte at addr1 contains the text byte count and the actual 
;               text starts with the second byte.  Typically COUNT is 
;               followed by TYPE.

        +WORD "count"
W_COUNT
        !word DO_COLON
        !word W_DUP
        !word W_1PLUS
        !word W_SWAP
        !word W_CAT
        !word W_PSEMI

; ****************************************************************************
; CR 
; (--)
; ANSI 6.1.0990

; FIG:
;
;      CR                                                    L0
;               Transmit a carriage return and line feed to the selected 
;               output device.

        +WORD "cr"
W_CR
        !word *+2
        jsr CR
        jmp NEXT

CR
        lda #K_RETURN
        jmp EMIT

; ****************************************************************************
; CREATE 
; (???)
; ANSI 6.1.1000

; FIG:
;
;      CREATE
;               A defining word used in the form:
;                         CREATE  cccc
;               by such words as CODE and CONSTANT to create a dictionary 
;               header for a Forth definition.  The code field contains 
;               the address of the word's parameter field.  The new word 
;               is created in the CURRENT vocabulary.
;
;;
;;                                       CREATE
;;                                       SCREEN 50 LINE 2
;;

        +WORD "create"
W_CREATE
        !word DO_COLON
;          !word TIB      ;)
;          !word HERE     ;|
;          !word CLITERAL ;|  6502 only, assures
;          !byte $A0      ;|  room exists in dict.
;          !word PLUS     ;|
;          !word ULESS    ;|
;          !word TWO      ;|
;          !word QERR     ;)

!if 1 {
        !word W_PARSE_NAME

        ; TODO look for an existing definition

        ; TODO ALIGN

        ; TODO link into wordlist of GET-CURRENT

        +CLITERAL 31            ; limit the length
        !word W_MIN
        ; TODO adjust length for alignment of data field?

        ; (c-addr u)

        !word W_ALIGN           ; TODO THIS SHOULD BE DONE BEFORE STORING THE LINK

        !word W_DUP             ; store name len | flags
        +CLITERAL F_END_MARKER | F_HIDDEN
        !word W_OR
        !word W_HERE
        !word W_CSTORE
        !word W_ONE
        !word W_ALLOT

        ; (c-addr u)

        !word W_HERE            ; store the name itself
        !word W_SWAP            
        !word W_DUP            
        !word W_ALLOT
        !word W_CMOVE

        !word W_ALIGN           ; need to realign after name

        ; ()

} else {
;          !word DFIND  ; store next word at HERE, search for match in dictionary
}

;          +ZBRANCH L2163
;          !word DROP
;          !word NFA
;          !word IDDOT
;          +CLITERAL 4
;          !word MESSAGE
;          !word SPACE

;L2163:    !word HERE
;          !word DUP
;          !word CAT    ; get string len
;          !word WIDTH
;          !word AT
;          !word MIN    ; limit symbol length to at most WIDTH
;          !word 1PLUS  ; add 1 for the length
;          !word ALLOT  ; allocate space for the counted string symbol name

                ; TODO can we just align to take care of this?
;          !word DP       ;)
;          !word CAT      ;| 6502 only. The code field
;          !word CLIT     ;| must not straddle page
;          !byte $FD      ;| boundaries
;          !word EQUAL    ;|
;          !word ALLOT    ;)

;          !word DUP
;          +CLITERAL $a0 ; F_END_MARKER | F_HIDDEN
;          !word TOGGLE
;          !word HERE
;          !word ONE
;          !word SUB
;          +CLITERAL $80
;          !word TOGGLE  ; set $80 on end of string - we can skip this?
;          !word LATEST
;          !word COMMA
;          !word CURRENT
;          !word AT
;          !word STORE
;          !word HERE
;          !word TWOP
;          !word COMMA
        !word W_PSEMI

; FIG
;      -FIND         ---  pfa  b  true     (found)
;                    ---  0                (not found)
;               Accepts the next text word (delimited by blanks) in the 
;               input stream to HERE, and searches the CONTEXT and then 
;               CURRENT vocabularies for a matching entry.  If found, the 
;               dictionary entry's parameter field address, its length 
;               byte, and a boolean true is left.  Otherwise, only a 
;               boolean false is left.

;;
;;                                       -FIND
;;                                       SCREEN 48 LINE 12
;;
!if 0 {
        +WORD "-find"
W_DFIND
        !word DO_COLON
;          !word BL
;          !word WORD
;          !word HERE     ; )
;          !word COUNT    ; |- Optional allowing free use of low
;          !word UPPER    ; )  case from terminal
;          !word HERE
;          !word CON
;          !word AT
;          !word AT
;          !word PFIND
;          !word DUP
;          !word ZEQU
;          !word ZBRANCH
;L2068:    !word $A       ; L2073-L2068
;          !word DROP
;          !word HERE
;          !word LATES
;          !word PFIND
        !word W_PSEMI
}

; ****************************************************************************
; DECIMAL 
; (--)
; ANSI 6.1.1170

; FIG:
;
;      DECIMAL                                               L0
;               Set the numeric conversion BASE for decimal input-output.

        +WORD "decimal"
W_DECIMAL
        !word DO_COLON
        +CLITERAL 10
        !word W_BASE
        !word W_STORE
        !word W_PSEMI

; ****************************************************************************
; DEPTH 
; (-- +n)
; ANSI 6.1.1200

        +WORD "depth"
W_DEPTH
        !word *+2
        ; ldy #0 ; TODO
        lda #TOS
        stx <XSAVE
        sec
        sbc XSAVE
        lsr
        pha
        tya
        jmp PUSH

; ****************************************************************************
; DO 
; (???)
; ANSI 6.1.1240


;    <limit> <initial> DO ... LOOP
;    <limit> <initial> DO ... <increment> +LOOP

; FIG:
;      DO             n1  n2  ---        (execute)
;                    addr  n  ---        (compile)           P,C2,L0
;               Occurs in a colon-definition in the form:
;                         DO  ...  LOOP
;                         DO  ...  +LOOP
;               At run-time, DO begins a sequence with repetitive 
;               execution controlled by a loop limit n1 and an index with 
;               initial value n2.  DO removes these from the stack.  Upon 
;               reaching LOOP the index is incremented by one.  Until the 
;               new index equals or exceeds the limit, execution loops 
;               back to just after DO; otherwise the loop parameters are 
;               discarded and execution continues ahead.  Both n1 and n2 
;               are determined at run-time and may be the result of other 
;               operations.  Within a loop 'I' will copy the current value 
;               of the index to the stack.  See I, LOOP, +LOOP, LEAVE.
;
;               When compiling within the colon-definition, DO compiles 
;               (DO), leaves the following address addr and n for later 
;               error checking.
;
;;
;;                                       DO
;;                                       SCREEN 73 LINE 9

!if 0 {
        +WORD_IMM "do"
W_DO
        !word DO_COLON
;          !word COMPILE
;          !word PDO
;          !word HERE
;          !word THREE
        !word W_PSEMI
}

;      (DO)                                                   C
;               The run-time procedure compiled by DO which moves the loop 
;               control parameters to the return stack.  See DO.
;        +WORD "(do)"
W_PDO
        !word *+2
        lda 3,x
        pha
        lda 2,x
        pha
        lda 1,x
        pha
        lda 0,x
        pha
        jmp POP2

; ****************************************************************************
; DOES> 
; (???)
; ANSI 6.1.1250

; FIG:
;
;      DOES>                                                 L0
;               A word which defines the run-time action within a high-
;               level defining word.  DOES> alters the code field and 
;               first parameter of the new word to execute the sequence of 
;               compiled word addresses following DOES>.  Used in 
;               combination with <BUILDS.  When the DOES> part executes it 
;               begins with the address of the first parameter of the new 
;               word on the stack.  This allows interpretation using this 
;               area or its contents.  Typical uses include the Forth 
;               assembler, multi-dimensional arrays, and compiler 
;               generation.
;
;;
;;                                       DOES>
;;                                       SCREEN 43 LINE 4
;;

!if 0 {
        +WORD "does>"
W_DOES
;:     !word DOCOL
;          !word RFROM
;          !word LATES
;          !word PFA
;          !word STORE
;          !word PSCOD
; SEMIS?
        !word *+2
        rts
}

; ****************************************************************************
; DROP 
; (x --)
; ANSI 6.1.1260

; FIG:
;
;      DROP          n  ---                                  L0
;               Drop the number from the stack.

        +WORD "drop"
W_DROP
        !word *+2
        jmp POP

; ****************************************************************************
; DUP 
; (x -- x x)
; ANSI 6.1.1290

; FIG:
;
;      DUP           n  ---  n  n                            L0
;               Duplicate the value on the stack.

        +WORD "dup"
W_DUP
        !word *+2
        lda 0,x
        pha
        lda 1,x
        jmp PUSH

; ****************************************************************************
; ELSE 
; (???)
; ANSI 6.1.1310

; Compilation: (C: orig_1 -- orig_2)
;
;
;
; Run-time: (--)
;
;
;
;

; From discussion in ANSI A.3.2.3.2:
;
;     : ELSE POSTPONE AHEAD 1 CS-ROLL POSTPONE THEN ; IMMEDIATE


; FIG:
;      ELSE          addr1  n1  ---  addr2  n2  (compiling)  P,C2,L0
;               Occurs within a colon-definition in the form:
;
;                         IF  ...  ELSE  ...  ENDIF
;               At run-time, ELSE executes after the true part following 
;               IF.  ELSE forces execution to skip over the following 
;               false part and resumes execution after the ENDIF.  It has 
;               no stack effect.
;               At compile-time ELSE emplaces BRANCH reserving a branch 
;               offset, leaves the address addr2 and n2 for error testing.  
;
;               ELSE also resolves the pending forward branch from IF by 
;               calculating the offset from addr1 to HERE and storing at 
;               addr1.
;
;
;;
;;                                       ELSE
;;                                       SCREEN 74 LINE 10

!if 0 {
        +WORD_IMM "else"
W_ELSE
        !word DO_COLON
;          !word TWO
;          !word QPAIR
;          !word COMPILE
;          !word BRANCH
;          !word HERE
;          !word ZERO
;          !word COMMA
;          !word SWAP
;          !word TWO
;          !word ENDIF
;          !word TWO
        !word W_PSEMI
}

; ****************************************************************************
; EMIT 
; (x --)
; ANSI 6.1.1320

; FIG:
;
;      EMIT          c  ---                                  L0
;               Transmit ascii character c to the selected output device.  
;               OUT is incremented for each character output.

        +WORD "emit"
W_EMIT
        !word *+2
        lda 0,x
        jsr BASOUT
        jmp POP

; ****************************************************************************
; ENVIRONMENT? 
; (c-addr u -- false | i*x true)
; ANSI 6.1.1345

!if 0 {
        +WORD "environment?"
        !word *+2
        rts
}

; ****************************************************************************
; EVALUTATE 
; (i*x c-addr u -- j*x)
; ANSI 6.1.1360

        +WORD "evaluate"
W_EVALUATE        
        !word DO_COLON

!if DEBUG {
        !word W_PDOTQ
        +STRING "<evaluate>"
        +CLITERAL '['
        !word W_EMIT
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT        
        !word W_DOTS,W_CR
}

        ; TODO setup SOURCE
        +LITERAL &INPUT_LEN
        !word W_STORE
        +LITERAL &INPUT_BUFFER
        !word W_STORE

        !word W_ZERO
        !word W_IN
        !word W_STORE

_evaluate_loop
        !word W_PARSE_NAME

        ; (c-addr u)

        !word W_QDUP
        +ZBRANCH _evaluate_done_loop

!if DEBUG {
        !word W_PDOTQ
        +STRING "evaluate-name"
        +CLITERAL '['
        !word W_EMIT
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT
        ; !word W_DOTS,W_CR
}

        !word W_2TOR    
        
        ; () (R: c-addr u)

        !word W_2RAT
        !word W_FORTH_WORDLIST
        !word W_SEARCH_WORDLIST

        ; (0)     (R: c-addr u) - not found
        ; (xt 1)  (R: c-addr u) - immediate
        ; (xt -1) (R: c-addr u) - non-immediate

        !word W_QDUP
        +ZBRANCH _evaluate_word_not_found
        
        ; TODO clean up the immediate/non-immediate handling

        !word W_1MINUS
        +ZBRANCH _evaluate_immediate

        ; non-immediate
        ; TODO execute if interpreting, move to definition if compiling
!if DEBUG {
        !word W_PDOTQ
        +STRING "<non-immediate>"
        !word W_DOTS,W_CR
}
        !word W_EXECUTE
        +BRANCH _evaluate_done_word

_evaluate_immediate
        ; TODO always execute
!if DEBUG {
        !word W_PDOTQ
        +STRING "<immediate>"
        !word W_DOTS,W_CR
}
        !word W_EXECUTE
        +BRANCH _evaluate_done_word

_evaluate_number
        ; (ud_2 c-addr_2) (R: c-addr u)
!if DEBUG {
        !word W_PDOTQ
        +STRING "<number>"
        !word W_DOTS,W_CR
}
        ; TODO if compiling postpone a pliteral, then the number
        !word W_2DROP
        +BRANCH _evaluate_done_word

_evaluate_word_not_found

        ; (R: c-addr u)

        ; TODO this just does unsigned single precision so far!!!!!
        !word W_ZERO
        !word W_ZERO
        !word W_2RAT
        !word W_TONUMBER
        +ZBRANCH _evaluate_number
        !word W_2DROP
        !word W_DROP

        ; TODO error
        ; TODO change colour to red?
!if DEBUG {
        !word W_PDOTQ
        +STRING "<not found>"
        !word W_DOTS,W_CR
}

!ifdef COLOUR_ERROR {
        +CLITERAL COLOUR_ERROR
        !word W_FOREGROUND
}
        !word W_SPACE
        !word W_2RAT
        !word W_TYPE
        !word W_PDOTQ
        +STRING "? "
!ifdef COLOUR_ERROR {
        +CLITERAL COLOUR_OUTPUT
        !word W_FOREGROUND
}
        ; jmp _evaluate_done_word

_evaluate_done_word
        !word W_2RFROM,W_2DROP
        +BRANCH _evaluate_loop

_evaluate_done_loop
        !word W_DROP ; (c-addr) was left on stack
        !word W_PSEMI

; FIG
;      INTERPRET
;               The outer text interpreter which sequentially executes or 
;               compiles text from the input stream (terminal or disc) 
;               depending on STATE.  If the word name cannot be found 
;               after a search of CONTEXT and then CURRENT it is converted 
;               to a number according to the current base.  That also 
;               failing, an error message echoing the name with a " ?" 
;               will be given.
;
;               Text input will be taken according to the convention for 
;               WORD.  If a decimal point is found as part of a number, a 
;               double number value will be left.  The decimal point has 
;               no other purpose than to force this action.  See NUMBER.

;;
;;                                       INTERPRET
;;                                       SCREEN 52 LINE 2
;;
!if 0 {
;        +WORD "interpret"
W_INTERPRET
        !word DO_COLON
;L2272:    !word DFIND
;          !word ZBRANCH
;L2274:    !word $1E      ; L2289-L2274
;          !word STATE
;          !word AT
;          !word LESS
;          !word ZBRANCH
;L2279:    !word $A       ; L2284-L2279
;          !word CFA
;          !word COMMA
;          !word BRANCH
;L2283:    !word $6       ; L2286-L2283
;L2284:    !word CFA
;          !word EXEC
;L2286:    !word QSTAC
;          !word BRANCH
;L2288:    !word $1C      ; L2302-L2288
;L2289:    !word HERE
;          !word NUMBER
;          !word DPL
;          !word AT
;          !word 1PLUS
;          !word ZBRANCH
;L2295:    !word 8        ; L2299-L2295
;          !word DLIT
;          !word BRANCH
;L2298:    !word $6       ; L2301-L2298
;L2299:    !word DROP
;          !word LITER
;L2301:    !word QSTAC
;L2302:    !word BRANCH
;L2303:    !word $FFC2    ; L2272-L2303
}

; ****************************************************************************
; EXECUTE 
; (i*x xt -- j*x)
; ANSI 6.1.1370

; FIG:
;
;      EXECUTE       addr  ---
;               Execute the definition whose code field address is on the 
;               stack.  The code field address is also called the 
;               compilation address.

        +WORD "execute"
W_EXECUTE
        !word *+2

!if ENABLE_RUNTIME_CHECKS {
        ; TODO check alignment
        lda 0,x
        ror
        bcc+
        jmp fail_runtime_check
+
}

        lda 0,x
        sta <W
        lda 1,x
        sta <W+1
        inx
        inx
        jmp &DO_JUMP_W

; ****************************************************************************
; EXIT 
; (???)
; ANSI 6.1.1380

!if 0 {
        +WORD "exit"
        !word *+2
        rts
}

; ****************************************************************************
; FILL 
; (c-addr u char --)
; ANSI 6.1.1540

; FIG:
;
;      FILL          addr  quan  b  ---
;               Fill memory at the address with the specified quantity of 
;               bytes b.

        +WORD "fill"
W_FILL
        !word *+2
        lda 0,x
        sta _fill_value
        lda 2,x
        sta _fill_count
        lda 3,x
        sta _fill_count+1
        lda 4,x
        sta _fill_dst
        lda 5,x
        sta _fill_dst+1
        +dma_inline
        +dma_options_end
        !byte dma_cmd_fill      ; cmd
_fill_count
        !word 0                 ; count
_fill_value
        !byte 0                 ; src
        !word 0                 ; 
_fill_dst
        !word 0                 ; dst
        !byte 0                 ; dst bank/flags
        !byte 0                 ; cmd msb
        !word 0                 ; modulo
        jmp POP3

; ****************************************************************************
; FIND 
; (c-addr -- c-addr 0 | xt 1 | xt -1)
; ANSI 6.1.1550

        +WORD "find"
W_FIND
        !word DO_COLON
        !word W_DUP
        !word W_COUNT
!if 0 {
        ; TODO iterate through
        ; !word W_GET_ORDER
        ; ...
} else {
        !word W_FORTH_WORDLIST 
}
        !word W_SEARCH_WORDLIST
        !word W_DUP
        +ZBRANCH +
        !word W_NIP
+
        !word W_PSEMI

; FIG
;      (FIND)        addr1  addr2  ---  pfa  b  true    (ok)
;                    addr1  addr2  ---  0               (bad)
;               Searches the dictionary starting at the name field address 
;               addr2, matching to the text at addr1.  Returns parameter 
;               field address, length byte of name field and boolean true 
;               for a good match.  If no match is found, only a boolean 
;               false is left.


;
; TODO like FIND but for a (c-addr u) string
;          ... using:
;                GET-ORDER (search)
;                TRAVERSE-WORDLIST (tools)? SEARCH-WORDLIST? (search)
;                NAME>STRING (tools),
;                COMPARE (string)
;
;        +WORD "(find)"
W_PFIND
        !word *+2
;          LDA #2
;          JSR SETUP
;          STX XSAVE
;L249:     LDY #0
;          LDA (N),Y
;          EOR (N+2),Y
;;
;;
;          AND #$3F
;          BNE L281
;L254:     INY
;          LDA (N),Y
;          EOR (N+2),Y
;          ASL A
;          BNE L280
;          BCC L254
;          LDX XSAVE
;          DEX
;          DEX
;          DEX
;          DEX
;          CLC
;          TYA
;          ADC #5
;          ADC N
;          STA 2,X
;          LDY #0
;          TYA
;          ADC N+1
;          STA 3,X
;          STY 1,X
;          LDA (N),Y
;          STA 0,X
;          LDA #1
;          PHA
;          JMP PUSH
;L280:     BCS L284
;L281:     INY
;          LDA (N),Y
;          BPL L281
;L284:     INY
;          LDA (N),Y
;          TAX
;          INY
;          LDA (N),Y
;          STA N+1
;          STX N
;          ORA N
;          BNE L249
;          LDX XSAVE
;          LDA #0
;          PHA
        ; jmp PUSH ; exit false upon reading null link
        jmp NEXT

; ****************************************************************************
; FM/MOD 
; (d_1 n_1 -- n_2 n_3)
; ANSI 6.1.1561

!if 0 {
        +WORD "fm/mod"
        !word *+2
        rts
}

; FIG
;      M/            d  n1  ---  n2  n3
;               A mixed magnitude math operator which leaves the signed 
;               remainder n2 and signed quotient n3, from a double number 
;               dividend and divisor n1.  The remainder takes its sign 
;               from the dividend.

;;
;;                                       M/
;;                                       SCREEN 57 LINE 3
;;
!if 0 {
        +WORD "m/"
W_MSLASH
        !word DO_COLON
;          !word OVER
;          !word TOR
;          !word TOR
;          !word DABS
;          !word R
;          !word ABS
;          !word USLAS
;          !word RFROM
;          !word R
;          !word XOR
;          !word PM
;          !word SWAP
;          !word RFROM
;          !word PM
;          !word SWAP
        !word W_PSEMI
}

; ****************************************************************************
; HERE 
; (-- addr)
; ANSI 6.1.1650

        +WORD "here"
W_HERE
        !word *+2
        ; TODO phw HERE
        lda <HERE
        pha
        lda <HERE+1
        jmp PUSH

; ****************************************************************************
; HOLD 
; (char --)
; ANSI 6.1.1670

; FIG:
;
;      HOLD          c  ---                                  L0
;               Used between <# and #> to insert an ascii character into a 
;               pictured numeric output string.  e.g. 2E HOLD will place a 
;               decimal point.
;
;;
;;                                       HOLD
;;                                       SCREEN 46 LINE 10
;;

!if 0 {
        +WORD "hold"
W_HOLD
        !word DO_COLON
;          !word LITERAL,$FFFF
;          !word HLD
;          !word PSTOR
;          !word HLD
;          !word AT
;          !word CSTOR
        !word W_PSEMI
}

; ****************************************************************************
; I 
; (???)
; ANSI 6.1.1680

; FIG:
;
;      I             ---  n                                  C,L0
;               Used within a DO-LOOP to copy the loop index to the stack.  
;               Other use is implementation dependent.  See R.

        +WORD "i"
W_I
!if 1 { 
        ; TODO does this work ???
        !word W_RAT+2      ; share the code for R
} else {
        !word DO_COLON
        !word W_RAT ; 2RAT,DROP?
        !word W_PSEMI
}

; ****************************************************************************
; IF 
; (???)
; ANSI 6.1.1700

; Compilation: (C: -- orig)
;       Put the location of a new unresolved forward reference orig onto the control flow stack.
;       Append the run-time semantics below to the current definition.  The semantics are incomplete
;       until orig is resolved, e.g. by THEN or ELSE.
;
; Run-time: (x --)
;       
;



; FIG:
;
;      IF            f  ---              (run-time)
;                       ---  addr  n     (compile)           P,C2,L0
;               Occurs in a colon definition in the form:
;                         IF  (tp)  ...  ENDIF
;                         IF  (tp)  ...  ELSE  (fp)  ...  ENDIF
;               At run-time, IF selects execution based on a boolean flag.  
;               If f is true (non-zero), execution continues ahead thru 
;               the true part.  If f is false (zero), execution skips till 
;               just after ELSE to execute the false part.  After either 
;               part, execution resumes after ENDIF.  ELSE and its false 
;               part are optional; if missing, false execution skips to 
;               just after ENDIF.
;
;               At compile-time IF compiles 0BRANCH and reserves space for 
;               an offset at addr.  addr and n are used later for 
;               resolution of the offset and error testing.
;
;;
;;                                       IF
;;                                       SCREEN 74 LINE 8

!if 1 {
        +WORD_IMM "if"
W_IF
        !word DO_COLON
;          !word COMPILE
;          !word ZBRANCH
;          !word HERE
;          !word ZERO
;          !word COMMA
;          !word TWO
        !word W_PSEMI
}

; ****************************************************************************
; IMMEDIATE 
; (--)
; ANSI 6.1.1710

; FIG:
;
;
;      IMMEDIATE
;               Mark the most recently made definition so that when 
;               encountered at compile time, it will be executed rather 
;               than being compiled. i.e. the precedence bit in its header 
;               is set.  This method allows definitions to handle unusual 
;               compiling situations, rather than build them into the 
;               fundamental compiler.  The user may force compilation of 
;               an immediate definition by preceding it with [COMPILE].
;
;
;;
;;                                       IMMEDIATE
;;                                       SCREEN 53 LINE 1
;;

!if 0 {
        +WORD "immediate"
W_IMMEDIATE
        !word DO_COLON
;          !word LATES
        +CLITERAL F_IMMEDIATE
;          !word TOGGL
        !word W_PSEMI
}

; ****************************************************************************
; INVERT 
; (x_1 -- x_2)
; ANSI 6.1.1720
; flip all bits

        +WORD "invert"
        !word *+2
        lda 0,x
        eor #$ff ; TODO neg
        sta 0,x
        lda 1,x
        eor #$ff ; TODO neg
        sta 1,x
        jmp NEXT

; ****************************************************************************
; J 
; (???)
; ANSI 6.1.1730

!if 0 {
        +WORD "j"
        !word *+2
        rts
}

; ****************************************************************************
; KEY 
; (-- char)
; ANSI 6.1.1750

; FIG:
;      KEY           ---  c                                  L0
;               Leave the ascii value of the next terminal key struck.

        +WORD "key"
W_KEY
        !word *+2
        ; sei ; TODO
        jsr BASIN ; TODO switch to GETIN for getting an individual keypress?
        pha
        lda #0
        jmp PUSH

; ****************************************************************************
; LEAVE 
; (???)
; ANSI 6.1.1760

; The FIG behaviour is to ensure that the loop is exited when we reach the
; next LOOP or +LOOP.  Execution continues on the current loop iteration until
; that time.

; In ANS, execution of the loop is terminated immediately.

; TODO this currently implements the FIG behaviour - we should switch to ANS.

; FIG:
;
;      LEAVE                                                 C,L0
;               Force termination of a DO-LOOP at the next opportunity by 
;               setting the loop limit to the current value of the index.  
;               The index itself remains unchanged, and execution proceeds 
;               normally until LOOP or +LOOP is encountered.

        +WORD "leave"
W_LEAVE
        !word *+2
        ; TODO
        pla ; TODO 
        pla
        pla
        pla
        jmp BRANCH

; ****************************************************************************
; LITERAL 
; (???)
; ANSI 6.1.1780

; FIG:
;
;      LITERAL       n  ---    (compiling)                   P,C2,L0
;               If compiling, then compile the stack value n as a 16 bit 
;               literal.  This definition is immediate so that it will 
;               execute during a colon definition.  The intended use is:
;                         : xxx   [ calculate ]  LITERAL  ;
;               Compilation is suspended for the compile time calculation 
;               of a value.  Compilation is resumed and LITERAL compiles 
;               this value.
;
;;
;;                                       LITERAL
;;                                       SCREEN 51 LINE 2

!if 0 {
        +WORD_IMM "literal"
W_LITERAL
        !word DO_COLON
;          !word STATE
;          !word AT
;          !word ZBRANCH
;L2222:    !word 8        ; L2226-L2222
;          !word COMPILE
;          !word LIT
;          !word COMMA
        !word W_PSEMI
}

; ****************************************************************************
; LOOP 
; (???)
; ANSI 6.1.1800

; FIG:
;
;      LOOP          addr  n  ---        (compiling)         P,C2,L0
;               Occurs in a colon-definition in the form:
;                         DO  ...  LOOP
;               At run-time, LOOP selectively controls branching back to 
;               the corresponding DO based on the loop index and limit.  
;               The  loop index is incremented by one and compared to the 
;               limit.  The branch back to DO occurs until the index 
;               equals or exceeds the limit; at that time, the parameters 
;               are discarded and execution continues ahead.
;
;               At compile-time, LOOP compiles (LOOP) and uses addr to 
;               calculate an offset to DO.  n is used for error testing.
;
;;
;;                                       LOOP
;;                                       SCREEN 73 LINE 11

!if 0 {
        +WORD_IMM "loop"
W_LOOP
        !word DO_COLON
;          !word THREE
;          !word QPAIR
;          !word COMPILE
;          !word PLOOP
;          !word BACK
        !word W_PSEMI
}

;      (LOOP)                                                 C2
;               The run-time procedure compiled by LOOP which increments 
;               the loop index and tests for loop completion.  See LOOP.
;
;;
;;                                       (LOOP)
;;                                       SCREEN 16 LINE 1
;;
;        +WORD "(loop)"
W_PLOOP
        !word *+2
        stx <XSAVE
        tsx
        inc $101,x
        bne +
        inc $102,x
+
        ; check for termination     TODO WHY DOES THIS WORK????
        clc
        lda $103,x
        sbc $101,x
        lda $104,x
        sbc $102,x
; PL2 ????   used by (+loop)
        ldx <XSAVE
        asl
!if 0 {
        bcc BRANCH
} else {        
        bcs +
        jmp BRANCH
+
}
        ; yup, terminating ...
        pla
        pla
        pla
        pla
        jmp BUMP

; ****************************************************************************
; LSHIFT 
; (x_1 u -- x_2)
; ANSI 6.1.1805

!if 0 {
        +WORD "lshift"
        !word *+2
        rts
}

; ****************************************************************************
; M* 
; (n_1 n_2 -- d)
; ANSI 6.1.1810

; FIG:
;
;      M*            n1  n2  ---  d
;               A mixed magnitude math operation which leaves the double 
;               number signed product of two signed numbers.

        +WORD "m*"
W_MSTAR
        !word DO_COLON
        !word W_2DUP
        !word W_XOR
        !word W_TOR
        !word W_ABS
        !word W_SWAP
        !word W_ABS
        !word W_UMSTAR
        !word W_RFROM
        !word W_ZLESS
        +ZBRANCH +
        !word W_DNEGATE
+       !word W_PSEMI

; ****************************************************************************
; MAX 
; (n_1 n_2 -- n_3)
; ANSI 6.1.1870

; FIG:
;
;      MAX           n1  n2  ---  max                        L0
;               Leave the greater of two numbers.

        +WORD "max"
W_MAX
        !word DO_COLON
        !word W_2DUP
        !word W_LESS
        +ZBRANCH +
        !word W_SWAP
+       !word W_DROP
        !word W_PSEMI

; ****************************************************************************
; MIN 
; (n_1 n_2 -- n_3)
; ANSI 6.1.1880

; FIG:
;
;      MIN           n1  n2  ---  min                        L0
;               Leave the smaller of two numbers.

        +WORD "min"
W_MIN
        !word DO_COLON
        !word W_2DUP
        !word W_GREATER
        +ZBRANCH +
        !word W_SWAP
+       !word W_DROP
        !word W_PSEMI

; ****************************************************************************
; MOD 
; (n_1 n_2 -- n_3)
; ANSI 6.1.1890

; FIG:
;
;      MOD           n1  n2  ---  mod                        L0
;               Leave the remainder of n1/n2, with the same sign as n1.
;
;;
;;                                       MOD
;;                                       SCREEN 57 LINE 10
;;

!if 0 {
        +WORD "mod"
W_MOD
        !word DO_COLON
;          !word SLMOD
;          !word DROP
        !word W_PSEMI
}

; ****************************************************************************
; MOVE
; (addr_1 addr_2 u --)
; ANSI 6.1.1900


; FIG
;
;      MOVE          addr1  addr2  n  ---
;               Move the contents of n memory cells (16 bit contents) 
;               beginning at addr1 into n cells beginning at addr2.  The 
;               contents of addr1 is moved first.  This definition is 
;               appropriate on word addressing computers.
;
; TODO DMA?

!if 0 {
        +WORD "move"
        !word *+2


; !if ENABLE_RUNTIME_CHECKS {
;         ; TODO check alignment
;         lda 0,x
;         ror
;         bcc+
;         jmp fail_runtime_check
; +
; }

        rts
}

; ****************************************************************************
; NEGATE
; (n_1 -- n_2)
; ANSI 6.1.1910
; change sign (MINUS in FIG)
; from ANSI A.3.2.1:
;       : NEGATE INVERT 1+ ;

        +WORD "negate"
W_NEGATE
        !word *+2
        ; ldy #0 ; TODO
        ; see also DNEGATE (double)
        sec
        tya
        sbc 0,x
        sta 0,x
        tya
        sbc 1,x
        sta 1,x
        jmp NEXT

; ****************************************************************************
; OR
; (x_1 x_2 -- x_3)
; ANSI 6.1.1980

; FIG:
;
;      OR            n1  n2  ---  or                         L0
;               Leave the bit-wise logical or of two 16 bit values.

        +WORD "or"
W_OR
        !word *+2
        lda 0,x
        ora 2,x
        sta 2,x
        lda 1,x
        ora 3,x
        sta 3,x
        jmp POP

; ****************************************************************************
; OVER
; (x_1 x_2 -- x_1 x_2 x_1)
; ANSI 6.1.1990

; FIG:
;
;
;      OVER          n1  n2  ---  n1  n2  n1                 L0
;               Copy the second stack value, placing it as the new top.

        +WORD "over"
W_OVER
        !word *+2
        lda 2,x
        pha
        lda 3,x
        jmp PUSH

; ****************************************************************************
; POSTPONE
; ("text" --)
; ANSI 6.1.2033

        +WORD "postpone"
W_POSTPONE
        !word DO_COLON
        ; !word QCOMP
        !word W_RFROM
        !word W_DUP
        !word W_2PLUS
        !word W_TOR
        !word W_AT
        !word W_COMMA
        !word W_PSEMI

;FIG

; See discussion in ANSI A.6.1.2033 and POSTPONE

;      COMPILE                                               C2
;               When the word containing COMPILE executes, the execution 
;               address of the word following COMPILE is copied (compiled) 
;               into the dictionary.  This allows specific compilation 
;               situations to be handled in addition to simply compiling 
;               an execution address (which the interpreter already does).

;;
;;                                       COMPILE
;;                                       SCREEN 41 LINE 2
;;
!if 0 {
        +WORD "compile"
W_COMPILE
        !word DO_COLON
;          !word QCOMP
;          !word RFROM
;          !word DUP
;          !word TWOP
;          !word TOR
;          !word AT
;          !word COMMA
        !word W_PSEMI
}

; ****************************************************************************
; QUIT
; (???)
; ANSI 6.1.2050

; - Empties the return stack
; - Store 0 in SOURCE-ID
; - Make the user input device the input source
; - Enter interpretation state
; - Repeat the following:
;   - Accept a line from input source into input buffer, set >IN to 0 and interpret
;   - Display prompt if in an interpretation state, all processing completed and no ambiguous condition exists

; FIG:
;
;      QUIT                                                  L1
;               Clear the return stack, stop compilation, and return 
;               control to the operators terminal.  No message is given.

        +WORD "quit"
W_QUIT
        !word DO_COLON
QUIT
!if DEBUG {
        !word W_PDOTQ
        +STRING "<quit>"
        !word W_DOTS,W_CR
}

!if ENABLE_BLOCK {
        !word W_ZERO
        !word W_BLK
        !word W_STORE
}
        ; TODO set SOURCE-ID to 0
        !word W_LBRACKET

_quit_read_loop

        !word W_RPSTORE ; TODO HOW???

        +LITERAL UAREA+U_TIB ; TODO !!!!!!!!!!!!!!
        !word W_AT
        !word W_DUP
        +CLITERAL 80
!if DEBUG {
        !word W_PDOTQ
        +STRING "<quit-pre-accept>"
        !word W_DOTS,W_CR
}        
        !word W_ACCEPT
!if DEBUG {
        !word W_PDOTQ
        +STRING "<quit-post-accept>"
        !word W_DOTS,W_CR
}    
        !word W_EVALUATE

!if DEBUG {
        !word W_DOTS,W_CR
}

        !word W_STATE
        !word W_AT
        !word W_ZEQUALS
        +ZBRANCH +

!ifdef COLOUR_PROMPT {
        +CLITERAL COLOUR_PROMPT
        !word W_FOREGROUND
}

        !word W_PDOTQ
        +STRING " ok"
        !word W_CR
+
        +BRANCH _quit_read_loop

; ****************************************************************************
; R>
; (???)
; ANSI 6.1.2060

; FIG:
;
;      R>            ---  n                                  L0
;               Remove the top value from the return stack and leave it on 
;               the computation stack.  See >R and R.

        +WORD "r>"
W_RFROM
        !word *+2
        ; see also 2r> (core-ext), nr> (tools-ext)
        dex
        dex
        pla
        sta 0,x ; TODO can we use PUSH and avoid some dex's here?  would save a bit of code size
        pla
        sta 1,x
        jmp NEXT

; ****************************************************************************
; R@
; (???)
; ANSI 6.1.2070

; FIG
;      R             ---  n
;               Copy the top of the return stack to the computation stack.

        +WORD "r@"
W_RAT ; TODO rename to W_RFETCH?
        !word *+2
        stx <XSAVE
        tsx
        lda $101,x
        pha
        lda $102,x
        ldx <XSAVE
        jmp PUSH

; ****************************************************************************
; RECURSE
; (--)
; ANSI 6.1.2120

!if 0 {
        +WORD "recurse"
        !word *+2
        rts
}

; ****************************************************************************
; REPEAT
; (???)
; ANSI 6.1.2140

; From discussion in ANSI A.3.2.3.2:
;
;    : REPEAT POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE


; FIG
;
;      REPEAT        addr  n  ---        (compiling)         P,C2
;               Used within a colon-definition in the form:
;                         BEGIN  ...  WHILE  ...  REPEAT
;               At run-time, REPEAT forces an unconditional branch back to 
;               just after the corresponding BEGIN.
;
;               At compile-time, REPEAT compiles BRANCH and the offset 
;               from HERE to addr.  n is used for error testing.
;
;;
;;                                       REPEAT
;;                                       SCREEN 74 LINE 5

!if 0 {
        +WORD_IMM "repeat"
W_REPEAT
        !word DO_COLON
;          !word TOR
;          !word TOR
;          !word AGAIN
;          !word RFROM
;          !word RFROM
;          !word TWO
;          !word SUB
;          !word ENDIF
        !word W_PSEMI
}

; ****************************************************************************
; ROT
; (x_1 x_2 x_3 -- x_2 x_3 x_1)
; ANSI 6.1.2160

; FIG:
;
;      ROT           n1  n2  n3  ---  n2  n3  n1             L0
;               Rotate the top three values on the stack, bringing the 
;               third to the top.

        +WORD "rot"
W_ROT
        !word DO_COLON
        !word W_TOR
        !word W_SWAP
        !word W_RFROM
        !word W_SWAP
        !word W_PSEMI

; ****************************************************************************
; RSHIFT
; (x_1 u -- x_2)
; ANSI 6.1.2162

!if 0 {
        +WORD "rshift"
        !word *+2
        rts
}

; ****************************************************************************
; S"
; (???)
; ANSI 6.1.2165
; ANSI 11.6.1.2165

!if 0 {
        +WORD_IMM "s\""
W_SQUOTE
        !word DO_COLON
!if 0 {
        +CLITERAL '"'  ; for colourization ... "
        !word W_PARSE
        ; TODO ...
}
        !word W_PSEMI
}

; ****************************************************************************
; S>D
; (n -- d)
; ANSI 6.1.2170
;FIG:
;      S->D          n  ---  d
;               Sign extend a single number to form a double number.

        +WORD "s>d"
W_STOD
        !word DO_COLON
        !word W_DUP
        !word W_ZLESS
        !word W_NEGATE
        !word W_PSEMI

; ****************************************************************************
; SIGN
; (n --)
; ANSI 6.1.2210

; FIG:
;
;      SIGN          n  d  ---  d                            L0
;               Stores an ascii "-" sign just before a converted numeric 
;               output string in the text output buffer when n is 
;               negative.  n is discarded, but double number d is 
;               maintained.  Must be between <# and #>.

        +WORD "sign"
W_SIGN
        !word DO_COLON
;          !word ROT
;          !word ZLESS
;          !word ZBRANCH
;L3492:    !word $7       ; L3496-L3492
        +CLITERAL '-'
;          !word HOLD
        !word W_PSEMI

; ****************************************************************************
; SM/REM
; (d_1 n_1 -- n_2 n_3)
; ANSI 6.1.2214

!if 0 {
        +WORD "sm/rem"
        !word *+2
        rts
}

; ****************************************************************************
; SOURCE
; (-- c-addr u)
; ANSI 6.1.2216

        +WORD "source"
W_SOURCE
        !word DO_COLON
        +LITERAL &INPUT_BUFFER
        !word W_AT
        +LITERAL &INPUT_LEN
        !word W_AT
        ; TODO should we ripping of >IN leading chars?
        !word W_PSEMI

; ****************************************************************************
; SPACE
; (--)
; ANSI 6.1.2220

; FIG:
;
;      SPACE                                                 L0
;               Transmit an ascii blank to the output device.

        +WORD "space"
W_SPACE
        !word DO_COLON
        !word W_BL
        !word W_EMIT
        !word W_PSEMI

; ****************************************************************************
; SPACES
; (n --)
; ANSI 6.1.2230

; FIG:
;
;
;      SPACES        n  ---                                  L0
;               Transmit n ascii blanks to the output device.

        +WORD "spaces"
W_SPACES
        !word DO_COLON
        !word W_ZERO
        !word W_MAX     ; (n|0)
        !word W_QDUP
        +ZBRANCH _spaces_done

        !word W_ZERO    ; (n 0)
        !word W_PDO

_spaces_loop
        !word W_SPACE

        !word W_PLOOP
        !word _spaces_loop-*

_spaces_done
        !word W_PSEMI

; ****************************************************************************
; STATE
; (-- a-addr)
; ANSI 6.1.2250

; FIG:
;
;
;      STATE         ---  addr                               L0,U
;               A user variable containing the compilation state.  A non-
;               zero value indicates compilation.  The value itself may be 
;               implementation dependent.

; Only affectected by: :, ;, ABORT, QUIT, :NONAME, [, and ]
; [ (core) sets state to 0   (not compiling)
; ] (core) sets state to C0  (compiling?)

        +WORD "state"
W_STATE
        !word DO_USER
        !byte U_STATE

; ****************************************************************************
; SWAP
; (x_1 x_2 -- x_2 x_1)
; ANSI 6.1.2260

; FIG:
;
;      SWAP          n1  n2  ---  n2  n1                     L0
;               Exchange the top two values on the stack.

        +WORD "swap"
W_SWAP
        !word *+2
        lda 2,x
        pha
        lda 0,x
        sta 2,x
        lda 3,x
        ldy 1,x
        sty 3,x
        jmp PUT

; ****************************************************************************
; THEN
; (???)
; ANSI 6.1.2270
;
; Compilation: (C: orig --)
;       Append the runtime semantics below to the current definition.  Resolve the
;       forward reference orig using the location of the appended run-time semantics.
;
; Run-time: (--)
;
;
;

; FIG:
;
;      THEN                                                  P,C0,L0
;               An alias for ENDIF.
;
;;
;;                                       THEN
;;                                       SCREEN 73 LINE 7

!if 0 {
        +WORD_IMM "then"
W_THEN
        !word DO_COLON
;          !word ENDIF
        !word W_PSEMI
}

;      ENDIF         addr1  n  ---       (compile)           P,C0,L0
;               Occurs in a colon-definition in the form:
;                         IF  ...  ENDIF
;                         IF  ...  ELSE  ...  ENDIF
;               At run-time, ENDIF serves only as the destination of a 
;               forward branch from IF or ELSE.  It marks the conclusion 
;               of the conditional structure.  THEN is another name for 
;               ENDIF.  Both names are supported in fig-FORTH.  See also 
;               IF and ELSE.
;
;               At compile-time, ENDIF computes the forward branch offset 
;               from addr to HERE and stores it at addr.  n is used for 
;               error tests.

;;
;;                                       ENDIF
;;                                       SCREEN 73 LINE 5
;;

!if 0 {
;        +WORD "endif"
W_ENDIF
        !word DO_COLON
;          !word QCOMP
;          !word TWO
;          !word QPAIR
;          !word HERE
;          !word OVER
;          !word SUB
;          !word SWAP
;          !word STORE
        !word W_PSEMI
}

; ****************************************************************************
; TYPE
; (c-addr u --)
; ANSI 6.1.2310

; FIG:
;
;
;      TYPE          addr  count  ---                        L0
;               Transmit count characters from addr to the selected output 
;               device.
;
;;
;;                                       TYPE
;;                                       SCREEN 44 LINE 2
;;

        +WORD "type"
W_TYPE
        !word DO_COLON
        !word W_QDUP
        +ZBRANCH +
        !word W_OVER
        !word W_PLUS
        !word W_SWAP
        !word W_PDO
_type_loop
        !word W_I
        !word W_CAT
        !word W_EMIT
        !word W_PLOOP
        !word _type_loop-*
        +BRANCH ++
+       !word W_DROP
++      !word W_PSEMI

; ****************************************************************************
; U.
; (u --)
; ANSI 6.1.2320

!if 0 {
        +WORD "u."
        !word *+2
        rts
}

; ****************************************************************************
; U<
; (u_1 u_2 -- flag)
; ANSI 6.1.2340

; FIG:
;;
;;                                       U<
;;                                       Unsigned less than
;;

!if 0 {
        +WORD "u<"
W_ULESS
        !word DO_COLON
;          !word SUB      ; subtract two values
;          !word ZLESS    ; test sign
        !word W_PSEMI
}

; ****************************************************************************
; UM*
; (u_1 u_2 -- ud)
; ANSI 6.1.2360

; FIG
;      U*            u1  u2  ---  ud
;               Leave the unsigned double number product of two unsigned 
;               numbers.

        +WORD "um*"
W_UMSTAR
        !word *+2
        ; ldy #0 ; TODO
        lda 0,x
        sta MULTINA
        lda 2,x
        sta MULTINB
        lda 1,x
        sta MULTINA+1
        lda 3,x
        sta MULTINB+1
        sty MULTINA+2
        sty MULTINB+2
        sty MULTINB+3
        sty MULTINA+3
        ; No need to check MULBUSY (d70f bit 6)
        lda MULTOUT
        sta 2,x
        lda MULTOUT+1
        sta 3,x
        lda MULTOUT+2
        sta 0,x
        lda MULTOUT+3
        sta 1,x
        jmp NEXT

; ****************************************************************************
; UM/MOD
; (ud u_1 -- u_2 u_3)
; ANSI 6.1.2370

!if 0 {
        +WORD "um/mod"
        !word *+2
        rts
}

; FIG
;      U/            ud  u1  ---  u2  u3
;               Leave the unsigned remainder u2 and unsigned quotient u3 
;               from the unsigned double dividend ud and unsigned divisor 
;               u1.

;;
;;                                       U/
;;                                       SCREEN 24 LINE 1
;;

!if 0 {
        +WORD "u/"
W_USLASH
        !word *+2
;          LDA 4,X
;          LDY 2,X
;          STY 4,X
;          ASL A
;          STA 2,X
;          LDA 5,X
;          LDY 3,X
;          STY 5,X
;          ROL A
;          STA 3,X
;          LDA #16
;          STA N
;L433:     ROL 4,X
;          ROL 5,X
;          SEC
;          LDA 4,X
;          SBC 0,X
;          TAY
;          LDA 5,X
;          SBC 1,X
;          BCC L444
;          STY 4,X
;          STA 5,X
;L444:     ROL 2,X
;          ROL 3,X
;          DEC N
;          BNE L433
        jmp POP
}

; see discussion in ANSI A.3.2.2.1
; FIG
;      M/MOD         ud1  u2  ---  u3  u4
;               An unsigned mixed magnitude math operation which leaves a 
;               double quotient ud4 and remainder u3, from a double 
;               dividend ud1 and single divisor u2.

!if 0 {
        +WORD "m/mod"
W_MSMOD
        !word DO_COLON
;          !word TOR
;          !word ZERO
;          !word R
;          !word USLAS
;          !word RFROM
;          !word SWAP
;          !word TOR
;          !word USLAS
;          !word RFROM
        !word W_PSEMI
}

; ****************************************************************************
; UNLOOP
; (???)
; ANSI 6.1.2380

!if 0 {
        +WORD "unloop"
        !word *+2
        rts
}

; ****************************************************************************
; UNTIL
; (???)
; ANSI 6.1.2390

; FIG:
;
;
;      UNTIL               f  ---        (run-time)
;                    addr  n  ---        (compile)           P,C2,L0
;               Occurs within a colon-definition in the form:
;                         BEGIN  ...  UNTIL
;               At run-time, UNTIL controls the conditional branch back to 
;               the corresponding BEGIN.  If f is false, execution returns 
;               to just after begin; if true, execution continues ahead.
;
;               At compile-time, UNTIL compiles (0BRANCH) and an offset 
;               from HERE to addr.  n is used for error tests.
;
;;
;;                                       UNTIL
;;                                       SCREEN 73 LINE 15

!if 0 {
        +WORD_IMM "until"
W_UNTIL
        !word DO_COLON
;          !word ONE
;          !word QPAIR
;          !word COMPILE
;          !word W_ZBRANCH
;          !word BACK
        !word W_PSEMI
}

; ****************************************************************************
; VARIABLE
; (???)
; ANSI 6.1.2410

; FIG:
;
;      VARIABLE                                              E,L0
;               A defining word used in the form:
;                         n  VARIABLE  cccc
;               When VARIABLE is executed, it creates the definition cccc 
;               with its parameter field initialised to n.  When cccc is 
;               later executed, the address of its parameter field 
;               (containing n) is left on the stack, so that a fetch or 
;               store may access this location.
;
;
;;
;;                                       VARIABLE
;;                                       SCREEN 34 LINE 5
;;

!if 0 {
        +WORD "variable"
W_VARIABLE    
;       !word DO_COLON
        !word W_CONSTANT
;          !word PSCOD
}

; ****************************************************************************
; WHILE
; (???)
; ANSI 6.1.2430

; From discussion in ANSI A.3.2.3.2:
;
;    : WHILE POSTPONE IF 1 CS-ROLL ; IMMEDIATE
;
;


; FIG:
;
;
;      WHILE               f  ---        (run-time)
;                    ad1  n1  ---  ad1  n1  ad2  n2          P,C2
;               Occurs in a colon-definition in the form:
;                         BEGIN  ...  WHILE  (tp)  ...  REPEAT
;               At run-time, WHILE selects conditional execution based on 
;               boolean flag f.  If f is true (non-zero), WHILE continues 
;               execution of the true part through to REPEAT, which then 
;               branches back to BEGIN.  If f is false (zero), execution 
;               skips to just after REPEAT, exiting the structure.
;
;               At compile time, WHILE emplaces (0BRANCH) and leaves ad2 
;               of the reserved offset.  The stack values will be resolved 
;               by REPEAT.
;
;
;;
;;                                       WHILE
;;                                       SCREEN 74 LINE 13

!if 0 {
        +WORD_IMM "while"
W_WHILE
        !word DO_COLON
;          !word IF
;          !word TWOP
        !word W_PSEMI
}

; ****************************************************************************
; WORD
; (char "text" -- c-addr)
; ANSI 6.1.2450

; FIG:
;
;      WORD          c  ---                                  L0
;               Read the next text characters from the input stream being 
;               interpreted, until a delimiter c is found, storing the 
;               packed character string beginning at the dictionary buffer 
;               HERE.  WORD leaves the character count in the first byte, 
;               the characters, and ends with two or more blanks.  Leading 
;               occurances of c are ignored.  If BLK is zero, text is 
;               taken from the terminal input buffer, otherwise from the 
;               disc block stored in BLK.  See BLK, IN.

; TODO check BLK, then if it's zero, check SOURCE-ID

        +WORD "word"
W_WORD
        !word DO_COLON

;          !word BLK
;          !word AT
;          !word ZBRANCH
;L1908:    !word $C       ; L1914-L1908
;          !word BLK
;          !word AT
;          !word BLOCK
;          !word BRANCH
;L1913:    !word $6       ; L1916-L1913

;L1914:    !word TIB
;          !word AT

;L1916:    !word IN
;          !word AT
;          !word PLUS
;          !word SWAP
;          !word W_ENCLOSE
;          !word HERE
;          !word CLITERAL
;          !byte $22
;          !word BLANK
;          !word IN
;          !word PSTOR
;          !word OVER
;          !word SUB
;          !word TOR
;          !word R
;          !word HERE
;          !word CSTOR
;          !word PLUS
;          !word HERE
;          !word 1PLUS
;          !word RFROM
;          !word CMOVE
        !word W_PSEMI

;      ENCLOSE       addr1  c  ---  addr1  n1  n2  n3
;               The text scanning primitive used by WORD.  From the text 
;               address addr1 and an ascii delimiting character c, is 
;               determined the byte offset to the first non-delimiting 
;               character n1, the offset to the first delimiter after the 
;               text n2, and the offset to the first character not 
;               included.  This procedure will not process past an ascii 
;               'null', treating it as an unconditional delimiter.

;        +WORD "enclose"
W_ENCLOSE
        !word *+2

;          LDA #2               ; move addr and c to N
;          JSR SETUP 

;          TXA                  ; make space on data stack for 4 words
;          SEC
;          SBC #8    
;          TAX

;          STY 3,X              ; set MSB of n2 & n3 to 0 (assume MSB of n1 still 0, addr1 still on stack)
;          STY 1,X

;          DEY                  ; setup for loop
;-         INY                  ; skip leading delimiters
;          LDA (N+2),Y
;          CMP N
;          BEQ -
;          STY 4,X              ; and now set n1 (number of leading delimiters)

;-         LDA (N+2),Y
;          BNE ++
;          STY 2,X
;          STY 0,X
;          TYA
;          CMP 4,X
;          BNE +
;          INC 2,X
;+         JMP NEXT
;++        STY 2,X
;          INY
;          CMP N
;          BNE -
;          STY 0,X
        jmp NEXT

; ****************************************************************************
; XOR
; (x_1 x_2 -- x_3)
; ANSI 6.1.2490

; FIG:
;
;      XOR           n1  n2  ---  xor                        L1
;               Leave the bitwise logical exclusive-or of two values.

        +WORD "xor"
W_XOR
        !word *+2
        lda 0,x
        eor 2,x
        sta 2,x
        lda 1,x
        eor 3,x
        sta 3,x
        jmp POP

; ****************************************************************************
; [
; (--)
; ANSI 6.1.2500

; FIG:
;
;
;      [                                                     P,L1
;               Used in a colon-definition in the form:
;                         : xxx   [  words  ]   more  ;
;               Suspend compilation.  The words after [ are executed, not 
;               compiled.  This allows calculation or compilation 
;               exceptions before resuming compilation with ].  See 
;               LITERAL, ].

        +WORD_IMM "["
W_LBRACKET
        !word DO_COLON
        !word W_ZERO
        !word W_STATE
        !word W_STORE
        !word W_PSEMI

; ****************************************************************************
; [']
; (???)
; ANSI 6.1.2510

!if 0 {
        +WORD_IMM "[']"
        !word *+2
        rts
}

; ****************************************************************************
; [CHAR]
; ANSI 6.1.2520

; Compilation ("<spaces>name" --):
;       Skip leading space delimiters.  Parse name delimited by a space.
; Run-time (-- char):
;       Place char, the first character of name, on the stack
;

        +WORD_IMM "[char]"
W_BCHARB
        !word *+2
        jmp NEXT

; ****************************************************************************
; ]
; (--)
; ANSI 6.1.2540

; FIG
;      ]                                                     L1
;               Resume compilation, to the completion of a colon-
;               definition.  See [.

        +WORD "]"
W_RBRACKET
        !word DO_COLON
        +CLITERAL $C0 ; TODO ??????????????
        !word W_STATE
        !word W_STORE
        !word W_PSEMI
