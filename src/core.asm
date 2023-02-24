
; ****************************************************************************
; CORE

; ****************************************************************************
; ! 
; (x a-addr --)
; ANSI 6.1.0010

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

        inc 0,x
        bne +
        inc 1,x
+
        lda 3,x
        sta (0,x)
        jmp POP2

; ****************************************************************************
; # 
; (ud_1 -- ud_2)
; ANSI 6.1.0030

; See core.f

; ****************************************************************************
; #> 
; (xd -- c-addr u)
; ANSI 6.1.0040

; See core.f

; ****************************************************************************
; #S 
; (ud_1 -- ud_2)
; ANSI 6.1.0050

; See core.f

; ****************************************************************************
; ' 
; ("<spaces>name" -- xt)
; ANSI 6.1.0070

; See core.f

; ****************************************************************************
; ( 
; ("text" --)
; ANSI 6.1.0080
; ANSI 11.6.1.0080 (extensions in FILE)

; See core.f

; ****************************************************************************
; * 
; (n_1 n_2 -- n_3)
; ANSI 6.1.0090

!if 0 {
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
}

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

; See core.f

; ****************************************************************************
; + 
; (n_1 n_2 -- n_3)
; ANSI 6.1.0120

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

; See core.f

; TODO this seems to be exiting one iteration too early ...

;      (+LOOP)       n  ---                                  C2
;               The run-time procedure compiled by +LOOP, which increments 
;               the loop index by n and tests for loop completion.  See 
;               +LOOP.

        +WORD "(+loop)"
W_PPLOOP
        !word *+2
        ; see also (loop)
        ; TODO CLEAN THIS UP
        inx
        inx
        stx <XSAVE
        lda $ff,x
        pha
        pha
        lda $fe,x
        tsx
        inx
        inx
        clc
        adc $101,x
        sta $101,x
        pla
        adc $102,x
        sta $102,x
        pla
        ; bpl PL1
        bmi +
        jmp PL1
+
        clc
        lda $101,x
        sbc $103,x
        lda $102,x
        sbc $104,x
        jmp PL2

; ****************************************************************************
; , 
; (x --)
; ANSI 6.1.0150

        +WORD ","
W_COMMA
        !word DO_COLON
        !word W_HERE
        !word W_STORE   ; TODO check if aligned?
        !word W_TWO 
        !word W_ALLOT
        !word W_PSEMI

; ****************************************************************************
; - 
; (n_1 n_2 -- n_3)
; ANSI 6.1.0160

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

; See core.f

; TODO remove this! it's only used for hardcoded words

; (.")
;               The run-time procedure, compiled by ." which transmits the 
;               following in-line text to the selected output device.  See 
;               ."

        +WORD "(.\")"
W_PDOTQ
        ; see also (s") ; TODO just use (s") and postpone the extra type?
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

; TODO WRONG FOR SIGNED !!!!!!!!!

; See core.f for a temporary correct one

; FIG:
;      /             n1  n2  ---  quot                       L0
;               Leave the signed quotient of n1/n2.

!if 0 {
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
}

; ****************************************************************************
; /MOD 
; (n_1 n_2 -- n_3 n_4)
; ANSI 6.1.0240

; See core.f

; ****************************************************************************
; 0< 
; (n -- flag)
; ANSI 6.1.0250

        +WORD "0<"
W_ZLESS
        !word *+2
        ; ldy #0 ; TODO
        asl 1,x
        bcc +
        dey
+       sty 1,x
        sty 0,x 
        jmp NEXT

; ****************************************************************************
; 0= 
; (x -- flag)
; ANSI 6.1.0270

        +WORD "0="
W_ZEQUAL
        !word *+2
        ; see also 0<> (core-ext)
        ; ldy #0 ; TODO
        lda 1,x
        ora 0,x
        bne +
        dey
+       sty 0,x
        sty 1,x
        jmp NEXT

; ****************************************************************************
; 1+ 
; (n_1 -- n_2)
; ANSI 6.1.0290

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

; See core.f

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

; See core.f

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

        +WORD "2dup"
W_2DUP
        !word *+2
        dex
        dex
        lda 4,x
        sta 0,x
        lda 5,x
        sta 1,x
        lda 2,x
        pha
        lda 3,x
        jmp PUSH

; ****************************************************************************
; 2OVER 
; (x_1 x_2 x_3 x_4 -- x_1 x_2 x_3 x_4 x_1 x_2)
; ANSI 6.1.0400

        +WORD "2over"
W_2OVER
        !word *+2
        dex
        dex
        lda 8,x
        sta 0,x
        lda 9,x
        sta 1,x
        lda 6,x
        pha
        lda 7,x
        jmp PUSH

; ****************************************************************************
; 2SWAP 
; (x_1 x_2 x_3 x_4 -- x_3 x_4 x_1 x_2)
; ANSI 6.1.0430

        +WORD "2swap"
W_2SWAP
        !word *+2
        lda 4,x
        ldy 0,x
        sty 4,x
        sta 0,x
        lda 5,x
        ldy 1,x
        sty 5,x
        sta 1,x
        lda 6,x
        ldy 2,x
        sty 6,x
        sta 2,x
        lda 7,x
        ldy 3,x
        sty 7,x
        sta 3,x
        jmp NEXT

; ****************************************************************************
; : 
; (???)
; ANSI 6.1.0450

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
        !word W_PSCODE
DO_COLON
        ; See also DO_DEFER

        ; ldy #0 ; TODO
        
        ; Start executing the word with the code field pointed to by W
        ; (in a new stack frame)
!if PUSH_MSB_FIRST {
        lda <I+1 ; push I
        pha
        lda <I
        pha
} else {
        phw &I ; TODO why doesn't this work? looks like phw uses the opposite byte order
}

!if 0 { ; TODO debugging why phw isn't working ...
        phw &I ; 
        jsr RDUMP
        pla
        pla
        ldy #0
}

        clc ; ???
        lda <W ; I = W + 2 ; TODO faster to copy then inw?
        adc #2
        sta <I
        tya
        adc <W+1
        sta <I+1
        jmp NEXT

; ****************************************************************************
; ; 
; (???)
; ANSI 6.1.0460

        +WORD_IMM ";"
W_SEMI 
        !word DO_COLON
;          !word QCSP
        +LITERAL W_PSEMI
        !word W_COMMA ; COMPILEC?
        !word W_SMUDGE
        !word W_LBRACKET
        !word W_PSEMI

        +NONAME ; ";s" ?
W_PSEMI
        ; See also exit
        !word *+2
!if PUSH_MSB_FIRST {        
        pla
        sta <I
        pla
        sta <I+1
} else {
        ; TODO plw &I
        pla
        sta <I+1
        pla
        sta <I

}        
        jmp NEXT

; ****************************************************************************
; < 
; (n_1 n_2 -- flag)
; ANSI 6.1.0480

        +WORD "<"
W_LESS
        !word *+2
        ; ldy #0 ; TODO
        sec
        lda 2,x
        sbc 0,x
        lda 3,x
        sbc 1,x

        bvc +
        eor #$80
+       bpl +
        dey
+       sty 2,x
        sty 3,x
        jmp POP

; ****************************************************************************
; <# 
; (--)
; ANSI 6.1.0490

; See core.f

; ****************************************************************************
; = 
; (x_1 x_2 -- 0 | -1)
; ANSI 6.1.0530

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
        ora 2,x

        bne +
        dey
+       sty 2,x
        sty 3,x
        jmp POP

; ****************************************************************************
; > 
; (n_1 n_2 -- flag)
; ANSI 6.1.0540

; See core.f

; ****************************************************************************
; >BODY 
; (xt -- a-addr)
; ANSI 6.1.0550

; See core.f

; ****************************************************************************
; >IN 
; (-- a-addr)
; ANSI 6.1.0560

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

;      DIGIT         (c -- n -1)    if ok
;                    (c -- 0)      if bad
;               Converts the ascii character c (using BASE) to its 
;               binary equivalent n, accompanied by a true flag.  If the 
;               conversion is invalid, leaves only a false flag.


; TODO make this case insensitive

        ; +WORD "digit"
        +NONAME
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
        lda #$ff
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

        +WORD ">r"
W_TOR
        !word *+2
        ; see also 2>r (core-ext), n>r (tools-ext)
!if PUSH_MSB_FIRST {        
        lda 1,x ; TODO PUSH_MSB_FIRST
        pha
        lda 0,x
        pha
} else {
        lda 0,x ; TODO PUSH_MSB_FIRST
        pha
        lda 1,x
        pha
}
        jmp POP

; ****************************************************************************
; ?DUP 
; (x -- 0 | x x)
; ANSI 6.1.0630

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

        +WORD "abort"
W_ABORT
        !word DO_COLON
        !word W_SPSTORE
        !word W_DECIMAL
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

; See core.f

; ****************************************************************************
; ACCEPT 
; (c-addr +n_1 -- +n_2)
; ANSI 6.1.0695

; pretty much replaces the obsolescent EXPECT from core-ext

; input terminates on newline or if we reach the character limit
; characters are displayed as they are received (ie, we can assume keyboard input only)

        +WORD "accept"
W_ACCEPT
        !word DO_COLON

        ; Loop index is a pointer to the buffer entry
        !word W_OVER
        !word W_PLUS
        !word W_OVER
        +DO _accept_after_loop

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
        ; !word _accept_after_loop-*
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
        inw <HERE
+       jmp NEXT

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
        bne +
        inc 1,x
+       jmp NEXT

; ****************************************************************************
; ALLOT 
; (n --)
; ANSI 6.1.0710

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

        +WORD "base"
W_BASE
        !word DO_CONSTANT
        !word &BASE

; ****************************************************************************
; BEGIN 
; Compilation: (C: -- dest)
; Runtime: (--)
; ANSI 6.1.0760

; See core.f

; ****************************************************************************
; BL 
; (-- char)
; ANSI 6.1.0770

; TODO move top core.f

        +WORD "bl"
W_BL
        !word DO_CONSTANT
        !word ' '

; ****************************************************************************
; C! 
; (char c-addr --)
; ANSI 6.1.0850

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

; See core.f

; ****************************************************************************
; C@ 
; (c-addr -- char)
; ANSI 6.1.0870

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

; See core.f

; ****************************************************************************
; CELLS 
; (n_1 -- n_2)
; ANSI 6.1.0890

; See core.f

; ****************************************************************************
; CHAR 
; ("text" -- char)
; ANSI 6.1.0895

; See core.f

; ****************************************************************************
; CHAR+ 
; (c-addr_1 -- c-addr_2)
; ANSI 6.1.0897

; See core.f

; ****************************************************************************
; CHARS 
; (n_1 -- n_2)
; ANSI 6.1.0898

; See core.f

; ****************************************************************************
; CONSTANT 
; (x "<spaces>name" --)
; ANSI 6.1.0950

        +WORD "constant"
W_CONSTANT
        !word DO_COLON
        !word W_CREATE
        !word W_SMUDGE
        !word W_COMMA
        !word W_PSCODE
; Push the first cell in the data field
DO_CONSTANT
        ldy #2
        lda (<W),y
        pha
        iny
        lda (<W),y
        jmp PUSH

; ****************************************************************************
; COUNT 
; (c-addr_1 -- c-addr_2 u)
; ANSI 6.1.0980

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
; ("<spaces>name" --)
; ANSI 6.1.1000

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

        !word W_PARSE_NAME

!if 0 {
        +DOTQ "create["
        !word W_2DUP,W_TYPE
        +CLITERAL ']'
        !word W_EMIT,W_CR
}

!if CASE_INSENSITIVE {
        !word W_2DUP
        !word W_LOWER
}

        ; TODO look for an existing definition

;          +ZBRANCH L2163
;          !word DROP
;          !word NFA
;          !word IDDOT
;          +CLITERAL 4
;          !word MESSAGE
;          !word SPACE

        ; TODO if no existing defintion found ...

        !word W_ALIGN           

        !word W_HERE
        !word W_GET_CURRENT
        !word W_DUP
        !word W_AT
        !word W_COMMA
        !word W_STORE

        +CLITERAL 31            ; limit the length
        !word W_MIN

        ; (c-addr u)

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

        +LITERAL DO_VARIABLE    ; default code fields needs to push address of data field
        !word W_COMMA

        ; ()

;          !word LATEST
;          !word COMMA
;          !word CURRENT
;          !word AT
;          !word STORE
;          !word HERE
;          !word TWOP
;          !word COMMA

        !word W_PSEMI

; ****************************************************************************
; DECIMAL 
; (--)
; ANSI 6.1.1170

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

; See core.f

; TODO share code with 2>r

;      (DO)                                                   C
;               The run-time procedure compiled by DO which moves the loop 
;               control parameters to the return stack.  See DO.
        +WORD "(do)"
W_PDO
        !word *+2
        ; See also unloop
!if 1 {
        ; ldy #0 ; TODO
        lda (<I),y
        pha ; TODO PUSH_MSB_FIRST - looks like this one is backwards!!!
        inw <I
        lda (<I),y
        pha
        inw <I
}

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

!if 0 {
        +WORD "does>"
W_DOES
        !word DO_COLON
;          !word RFROM
;          !word LATES
;          !word PFA
;          !word STORE
        !word W_PSCODE
}
DO_DOES
;       LDA IP+1
;       PHA
;       LDA IP
;       PHA
;       LDY #2
;       LDA (W),Y
;       STA IP
;       INY
;       LDA (W),Y
;       STA IP+1
;       CLC
;       LDA W
;       ADC #4
;       PHA
;       LDA W+1
;       ADC #0
        jmp PUSH

; ****************************************************************************
; DROP 
; (x --)
; ANSI 6.1.1260

        +WORD "drop"
W_DROP
        !word *+2
        jmp POP

; ****************************************************************************
; DUP 
; (x -- x x)
; ANSI 6.1.1290

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

; See core.f

; ****************************************************************************
; EMIT 
; (x --)
; ANSI 6.1.1320

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

        !word W_SAVE_INPUT
        !word W_NTOR

        +LITERAL &INPUT_LEN
        !word W_STORE
        +LITERAL &INPUT_BUFFER
        !word W_STORE
        +LITERAL -1
        +LITERAL &SOURCE_ID
        !word W_STORE

        !word W_PEVALUATE

        !word W_NRFROM
        !word W_RESTORE_INPUT
        !word W_DROP            ; TODO check status from restore

        !word W_PSEMI

; This does the real work of EVALUATE but does not
; SAVE-INPUT/RESTORE-INPUT, set SOURCE-ID, etc.
; We need to split this off from EVALUATE so we can
; call it from INCLUDE-FILE - we don't want to
; change the SOURCE-ID in this case to allow
; REFILL to grab more input data (necessary for
; multiline '(' comments, [IF]/[ELSE]/[THEN], etc)

        +NONAME
W_PEVALUATE             ; ( -- )
        !word DO_COLON

        !word W_ZERO ; TODO should this be done in EVALUATE too?
        !word W_IN
        !word W_STORE

_pevaluate_loop
        !word W_PARSE_NAME

        ; (c-addr u)

        !word W_QDUP
        +ZBRANCH _pevaluate_done_loop

!if DEBUG {
        +DOTQ "pevaluate-name"
        +CLITERAL '['
        !word W_EMIT
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT
        ; !word W_DOTS,W_CR
}

!if CASE_INSENSITIVE { ; TODO move this to PARSE-NAME to get rid of duplication?
        !word W_2DUP
        !word W_LOWER
}

        !word W_2TOR    
        
        ; () (R: c-addr u)

        !word W_2RAT
        !word W_FORTH_WORDLIST ; TODO
        !word W_SEARCH_WORDLIST

        ; (0)     (R: c-addr u) - not found
        ; (xt 1)  (R: c-addr u) - immediate
        ; (xt -1) (R: c-addr u) - non-immediate

        ; TODO turn the 1/-1 into EXECUTE/COMPILE, then just execute
        ; (doing the same sort of thing as NAME>COMPILE)

        !word W_QDUP
        +ZBRANCH _pevaluate_word_not_found
        
        ; TODO clean up the immediate/non-immediate handling

        !word W_1MINUS
        +ZBRANCH _pevaluate_immediate

        ; non-immediate
        ; TODO execute if interpreting, move to definition if compiling
!if DEBUG {
        +DOTQ "<non-immediate>"
        !word W_DOTS,W_CR
}

        !word W_STATE
        !word W_AT
        +ZBRANCH _pevaluate_nonimmediate_interpreting

        !word W_COMMA
        +BRANCH _pevaluate_done_word

_pevaluate_nonimmediate_interpreting

        !word W_EXECUTE
        +BRANCH _pevaluate_done_word

_pevaluate_immediate
        ; TODO always execute
!if DEBUG {
        +DOTQ "<immediate>"
        !word W_DOTS,W_CR
}
        !word W_EXECUTE
        +BRANCH _pevaluate_done_word

_pevaluate_word_not_found

        ; (R: c-addr u)

        !word W_2RAT
        !word W_STONUMBER
        +ZBRANCH _pevaluate_stonumber_failed

        !word W_DROP ; drop MSW

        !word W_STATE
        !word W_AT
        +ZBRANCH _pevaluate_done_word

        +LITERAL W_PLITERAL
        !word W_COMMA ; COMPILEC?
        !word W_COMMA

        ; TODO if compiling postpone a pliteral, then the number
        +BRANCH _pevaluate_done_word

_pevaluate_stonumber_failed
        ; (ud) (R: c-addr u)
        !word W_NIP
        !word W_NIP
        ; +BRANCH _evaluate_error

_pevaluate_error
        ; (R: c-addr u)
        ; TODO error
        ; TODO change colour to red?
!if DEBUG {
        +DOTQ "<not found>"
        !word W_DOTS,W_CR
}

!ifdef COLOUR_ERROR {
        +CLITERAL COLOUR_ERROR
        !word W_FOREGROUND
}
        !word W_SPACE
        !word W_2RAT
        !word W_TYPE
        +DOTQ "? "
!ifdef COLOUR_ERROR {
        +CLITERAL COLOUR_OUTPUT
        !word W_FOREGROUND
}
        ; jmp _evaluate_done_word

_pevaluate_done_word
        !word W_2RFROM,W_2DROP
        +BRANCH _pevaluate_loop

_pevaluate_done_loop
        !word W_DROP ; (c-addr) was left on stack

        !word W_PSEMI

; ****************************************************************************
; EXECUTE 
; (i*x xt -- j*x)
; ANSI 6.1.1370

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

        +WORD "exit"
W_EXIT
        !word *+2
        ; See also ;
!if 0 {        
        jsr RDUMP
        jsr CR
        ldy #0
}
!if PUSH_MSB_FIRST {        
        pla
        sta <I
        pla
        sta <I+1
} else {
        ; TODO plw &I
        pla
        sta <I+1
        pla
        sta <I

}
        jmp NEXT

; ****************************************************************************
; FILL 
; (c-addr u char --)
; ANSI 6.1.1540

        +WORD "fill"
W_FILL
        !word *+2
!if 1 {
        ; TODO dma fills of length 0 look to be happily ignored on a MEGA65
        ; but cause bad things to happen with xemu - check this case
        ; explicitly for now
        lda 2,x
        ora 3,x
        bne +
        jmp POP3
+
}
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
        !byte $0b               ; F018B 12-byte format
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

; See core.f

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
;;  THIS IS SM/REM
!if 0 {
        +WORD "m/"
W_MSLASH
        !word DO_COLON
        ; !word W_OVER
        ; !word W_TOR
        ; !word W_TOR
        ; !word W_DABS
        ; !word W_R
        ; !word W_ABS
        ; !word W_USLASH
        ; !word W_RFROM
        ; !word W_R
        ; !word W_XOR
        ; !word W_PM
        ; !word W_SWAP
        ; !word W_RFROM
        ; !word W_PM
        ; !word W_SWAP
        !word W_PSEMI
}

; ****************************************************************************
; HERE 
; (-- addr)
; ANSI 6.1.1650

        +WORD "here"
W_HERE
        !word *+2
        lda <HERE
        pha
        lda <HERE+1
        jmp PUSH

; ****************************************************************************
; HOLD 
; (char --)
; ANSI 6.1.1670

; See core.f

; ****************************************************************************
; I 
; (???)
; ANSI 6.1.1680

        +WORD "i"
W_I
        !word W_RAT+2      ; share the code for R

; ****************************************************************************
; IF 
; (???)
; ANSI 6.1.1700

; See core.f

; ****************************************************************************
; IMMEDIATE 
; (--)
; ANSI 6.1.1710

        +WORD "immediate"
W_IMMEDIATE
        !word DO_COLON
        ; see also smudge (internals)
        !word W_LATEST
        !word W_2PLUS
        !word W_DUP
        !word W_CAT
        +CLITERAL F_IMMEDIATE
        !word W_OR
        !word W_SWAP
        !word W_CSTORE
        !word W_PSEMI

; ****************************************************************************
; INVERT 
; (x_1 -- x_2)
; ANSI 6.1.1720
; flip all bits

        +WORD "invert"
W_INVERT
        !word *+2
        lda 0,x
        eor #$ff
        sta 0,x
        lda 1,x
        eor #$ff
        sta 1,x
        jmp NEXT

; ****************************************************************************
; J 
; (???)
; ANSI 6.1.1730

        +WORD "j"
W_J
        !word *+2
        stx <XSAVE
        tsx
!if PUSH_MSB_FIRST {        
        lda $107,x
        pha
        lda $108,x
} else {
        lda $108,x
        pha
        lda $107,x
}        
        ldx <XSAVE
        jmp PUSH

; ****************************************************************************
; KEY 
; (-- char)
; ANSI 6.1.1750

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

        +WORD "leave"
W_LEAVE
        !word *+2
        pla ; TODO 
        pla
        pla
        pla
!if 1 { ; TODO PUSH_MSB_FIRST {        
        pla ; TODO new I
        sta <I+1 ; TODO BACKWARDS???
        pla
        sta <I
} else {
        pla ; TODO new I
        sta <I
        pla
        sta <I+1

}        
        jmp NEXT
        ;jmp BRANCH

; ****************************************************************************
; LITERAL 
; (???)
; ANSI 6.1.1780

; See core.f

; ****************************************************************************
; LOOP 
; (???)
; ANSI 6.1.1800

; See core.f

;      (LOOP)                                                 C2
;               The run-time procedure compiled by LOOP which increments 
;               the loop index and tests for loop completion.  See LOOP.

        +WORD "(loop)"
W_PLOOP
        !word *+2
        ; see also (+loop)
        ; TODO CLEAN THIS UP
        stx <XSAVE
        tsx
        inc $101,x
        bne +
        inc $102,x
+
        ; check for termination     TODO WHY DOES THIS WORK????
PL1 ; ??? used by (+loop)
        clc
        lda $103,x
        sbc $101,x
        lda $104,x
        sbc $102,x
PL2 ; ????   used by (+loop)
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
        ; TODO
        pla
        pla
        jmp BUMP

; ****************************************************************************
; LSHIFT 
; (x_1 u -- x_2)
; ANSI 6.1.1805

        +WORD "lshift"
W_LSHIFT
        !word *+2
        lda 0,x
beq +
-       clc
        rol 2,x
        rol 3,x
        dec
        bne -
+       jmp POP

; ****************************************************************************
; M* 
; (n_1 n_2 -- d)
; ANSI 6.1.1810

; See core.f

; ****************************************************************************
; MAX 
; (n_1 n_2 -- n_3)
; ANSI 6.1.1870

; TODO move to core.f

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

; TODO move to core.f?

        +WORD "min"
W_MIN
        !word DO_COLON
        !word W_2DUP
        !word W_SWAP
        !word W_LESS
        +ZBRANCH +
        !word W_SWAP
+       !word W_DROP
        !word W_PSEMI

; ****************************************************************************
; MOD 
; (n_1 n_2 -- n_3)
; ANSI 6.1.1890

; See core.f 

; ****************************************************************************
; MOVE
; (addr_1 addr_2 u --)
; ANSI 6.1.1900

; See core.f

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
        ; TODO use neg
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

        +WORD "over"
W_OVER
        !word *+2
        lda 2,x
        pha
        lda 3,x
        jmp PUSH

; ****************************************************************************
; POSTPONE
; ("<spaces>name" --)
; ANSI 6.1.2033
;
; Appends the *compilation* semantics of name to the current definition

; TODO move to core.f?

        +WORD_IMM "postpone"
W_POSTPONE
        !word DO_COLON
        !word W_PARSE_NAME
!if 0 {
        +DOTQ "<postpone>"
        +CLITERAL '['
        !word W_EMIT
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT
}        

        !word W_FORTH_WORDLIST ; TODO
        !word W_SEARCH_WORDLIST_NT ; (0 | nt)
        !word W_QDUP
        +ZBRANCH _postpone_done ; TODO error if not found

        !word W_DUP
        !word W_QIMMEDIATE
        +ZBRANCH _postpone_nonimmediate 

        ; immediate
        !word W_NAME_TO_INTERPRET
        !word W_COMMA
        +BRANCH _postpone_done

_postpone_nonimmediate
        !word W_NAME_TO_INTERPRET
        +LITERAL W_PLITERAL
        !word W_COMMA
        !word W_COMMA
        +LITERAL W_COMMA
        !word W_COMMA

_postpone_done
        !word W_PSEMI

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

        +WORD "quit"
W_QUIT
        !word DO_COLON
QUIT

!if ENABLE_BLOCK {
        !word W_ZERO
        !word W_BLK
        !word W_STORE
}

        !word W_LBRACKET

        !word W_ZERO
        +LITERAL &SOURCE_ID
        !word W_STORE

_quit_read_loop

        !word W_RPSTORE

        !word W_REFILL
        !word W_DROP
        !word W_PEVALUATE

        !word W_STATE
        !word W_AT
        !word W_ZEQUAL
        +ZBRANCH +

!ifdef COLOUR_PROMPT {
        +CLITERAL COLOUR_PROMPT
        !word W_FOREGROUND
}

        +DOTQ " ok"
        !word W_CR
+
        +BRANCH _quit_read_loop

; ****************************************************************************
; R>
; (???)
; ANSI 6.1.2060

        +WORD "r>"
W_RFROM
        !word *+2
        ; see also 2r> (core-ext), nr> (tools-ext)
        dex
        dex ; TODO PUSH_MSB_FIRST
!if PUSH_MSB_FIRST {        
        pla
        sta 0,x ; TODO can we use PUSH and avoid some dex's here?  would save a bit of code size
        pla
        sta 1,x
} else {
        pla
        sta 1,x ; TODO can we use PUSH and avoid some dex's here?  would save a bit of code size
        pla
        sta 0,x
}        
        jmp NEXT

; ****************************************************************************
; R@
; (???)
; ANSI 6.1.2070

        +WORD "r@"
W_RAT ; TODO rename to W_RFETCH?
        !word *+2
        stx <XSAVE
        tsx  ; TODO PUSH_MSB_FIRST
!if PUSH_MSB_FIRST {        
        lda $101,x
        pha
        lda $102,x
} else {
        lda $102,x
        pha
        lda $101,x
}        
        ldx <XSAVE
        jmp PUSH

; ****************************************************************************
; RECURSE
; (--)
; ANSI 6.1.2120

; See core.f

; ****************************************************************************
; REPEAT
; (???)
; ANSI 6.1.2140

; See core.f

; ****************************************************************************
; ROT
; (x_1 x_2 x_3 -- x_2 x_3 x_1)
; ANSI 6.1.2160

; TODO native implementation?

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

        +WORD "rshift"
W_RSHIFT
        !word *+2
        lda 0,x
beq +
-       clc
        ror 3,x
        ror 2,x
        dec
        bne -
+       jmp POP

; ****************************************************************************
; S"
; (???)
; ANSI 6.1.2165
; ANSI 11.6.1.2165

        +WORD_IMM "s\""
W_SQUOTE
        !word DO_COLON
        +CLITERAL '"'  ; for colourization ... "
        !word W_PARSE
        ; TODO ...
        !word W_PSEMI

        +WORD "(s\")"
W_PSQ
        ; see also (.")
        !word DO_COLON
        !word W_RAT
        !word W_COUNT
        !word W_DUP
        !word W_1PLUS ; account for length field
        !word W_RFROM
        !word W_PLUS
        !word W_TOR
        !word W_PSEMI

; ****************************************************************************
; S>D
; (n -- d)
; ANSI 6.1.2170

; See core.f

; ****************************************************************************
; SIGN
; (n --)
; ANSI 6.1.2210

; See core.f

; ****************************************************************************
; SM/REM
; (d_1 n_1 -- n_2 n_3)
; ANSI 6.1.2214

; See core.f

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

; TODO move to core.f

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

; See core.f

; ****************************************************************************
; STATE
; (-- a-addr)
; ANSI 6.1.2250
; ANSI 15.6.2.2250

        +WORD "state"
W_STATE
        !word DO_CONSTANT
        !word &STATE

; ****************************************************************************
; SWAP
; (x_1 x_2 -- x_2 x_1)
; ANSI 6.1.2260

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

; See core.f

; ****************************************************************************
; TYPE
; (c-addr u --)
; ANSI 6.1.2310

; TODO can we use primm? the problem is it uses a null terminated string

        +WORD "type"
W_TYPE
        !word DO_COLON
        !word W_QDUP
        +ZBRANCH +
        !word W_OVER
        !word W_PLUS
        !word W_SWAP
        +DO _type_after_loop
_type_loop
        !word W_I
        !word W_CAT
        !word W_EMIT
        !word W_PLOOP
        !word _type_loop-*
_type_after_loop
        +BRANCH ++
+       !word W_DROP
++      !word W_PSEMI

; ****************************************************************************
; U.
; (u --)
; ANSI 6.1.2320

; See core.f

; ****************************************************************************
; U<
; (u_1 u_2 -- flag)
; ANSI 6.1.2340

        +WORD "u<"
W_ULESS
        !word *+2
        lda 3,x
        cmp 1,x
        bne +
        lda 2,x
        cmp 0,x
+       bcs +
        dey
+       sty 2,x
        sty 3,x
        jmp POP

; ****************************************************************************
; UM*
; (u_1 u_2 -- ud)
; ANSI 6.1.2360

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

        +WORD "um/mod"
W_UMMOD
; W_USLASH                ; TODO rename - this is the old FIG name
        !word *+2

        ; TODO rewrite this using math unit
!if 1 {
; FIG
;      U/            ud  u1  ---  u2  u3
;               Leave the unsigned remainder u2 and unsigned quotient u3 
;               from the unsigned double dividend ud and unsigned divisor 
;               u1.
        lda 4,x
        ldy 2,x
        sty 4,x
        asl
        sta 2,x
        lda 5,x
        ldy 3,x
        sty 5,x
        rol
        sta 3,x
        lda #16
        sta <TEMP1
-       rol 4,x
        rol 5,x
        sec
        lda 4,x
        sbc 0,x
        tay
        lda 5,x
        sbc 1,x
        bcc +
        sty 4,x
        sta 5,x
+       rol 2,x
        rol 3,x
        dec <TEMP1
        bne -
}
        jmp POP

; ****************************************************************************
; UNLOOP
; (???)
; ANSI 6.1.2380

        +WORD "unloop"
W_UNLOOP
        !word *+2
        pla
        pla
        pla
        pla
        pla
        pla
        jmp NEXT

; ****************************************************************************
; UNTIL
; (???)
; ANSI 6.1.2390

; See core.f

; ****************************************************************************
; VARIABLE
; ("<spaces>name" --)
; ANSI 6.1.2410

        +WORD "variable"
W_VARIABLE    
        !word DO_COLON
        !word W_CREATE
        !word W_SMUDGE
        !word W_ZERO
        !word W_COMMA
        !word W_PSCODE
; Push the address of the first cell in the data field
DO_VARIABLE
        ; ldy #0 ; TODO
        clc
        lda <W
        adc #2
        pha
        tya
        adc <W+1
        jmp PUSH

; ****************************************************************************
; WHILE
; (???)
; ANSI 6.1.2430

; See core.f

; ****************************************************************************
; WORD
; (char "<chars>ccc<char>" -- c-addr)
; ANSI 6.1.2450

; TODO move to core.f

        +WORD "word"
W_WORD
        !word DO_COLON

        !word W_PPARSE_NAME     ; (c-addr u)
        !word W_DUP
        !word W_PAD
        !word W_CSTORE          ; (c-addr u)

        !word W_PAD
        !word W_1PLUS
        !word W_SWAP
        !word W_CMOVE           ; ()

        !word W_PAD

        !word W_PSEMI

; ****************************************************************************
; XOR
; (x_1 x_2 -- x_3)
; ANSI 6.1.2490

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

; See core.f

; ****************************************************************************
; [CHAR]
; ANSI 6.1.2520

; See core.f

; ****************************************************************************
; ]
; (--)
; ANSI 6.1.2540

        +WORD "]"
W_RBRACKET
        !word DO_COLON
        +CLITERAL $C0 ; TODO ??????????????
        !word W_STATE
        !word W_STORE
        !word W_PSEMI
