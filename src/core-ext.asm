
; ****************************************************************************
; CORE EXT

; ****************************************************************************
; .(
; ("text" --)
; ANSI 6.2.0200

; from ANSI A.6.2.2008 discussion:
;     : .( [CHAR] ) PARSE TYPE ; IMMEDIATE

!if ENABLE_CORE_EXT {
        +WORD_IMM ".("
W_DPAREN
        !word DO_COLON
        +CLITERAL ')'
        !word W_PARSE
        !word W_TYPE
        !word W_PSEMI
}

; ****************************************************************************
; .R
; (n_1 n_2 --)
; ANSI 6.2.0210

; FIG
;      .R            n1  n2  ---
;               Print the number n1 right aligned in a field of whose 
;               width is n2.  No following blank is printed.
;
;;
;;                                       .R
;;                                       SCREEN 76 LINE 7
;;

!if ENABLE_CORE_EXT {
!if 0 {
        +WORD ".r"
W_DOTR
        !word DO_COLON
;          !word TOR
;          !word STOD
;          !word RFROM
;          !word DDOTR
        !word W_PSEMI
}
}

; ****************************************************************************
; 0<>
; (x -- flag)
; ANSI 6.2.0260

!if ENABLE_CORE_EXT {
        +WORD "0<>"
W_ZNOTEQUALS
        !word *+2
        ; see also 0= (core)
        ; ldy #0 ; TODO
        lda 1,x
        sty 1,x
        ora 0,x
        beq +
        iny
+       sty 0,x
        jmp NEXT
}

; ****************************************************************************
; 0>
; (n -- flag)
; ANSI 6.2.0280

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; 2>R
; (x_1 x_2 --) (R: -- x_1 x_2)
; ANSI 6.2.0340

; The word itself is required by the implmentation (of FIND) but is only visible if CORE-EXT is enabled

!if ENABLE_CORE_EXT {
        +WORD "2>r"
} else {
        +NONAME
}
W_2TOR
        !word *+2
        ; see also >r (core)
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
; 2R>
; (???)
; ANSI 6.2.0410

; The word itself is required by the implmentation (of FIND) but is only visible if CORE-EXT is enabled

!if ENABLE_CORE_EXT {
        +WORD "2r>"
} else {
        +NONAME
}
W_2RFROM
        !word *+2
        ; see also r> (core)
        dex
        dex
        dex
        dex
        pla
        sta 0,x
        pla
        sta 1,x
        pla
        sta 2,x
        pla
        sta 3,x
        jmp NEXT     

; ****************************************************************************
; 2R@
; (-- x_1 x_2) (R: x_1 x_2 -- x_1 x_2)
; ANSI 6.2.0415

; The word itself is required by the implmentation (of FIND) but is only visible if CORE-EXT is enabled

!if ENABLE_CORE_EXT {
        +WORD "2r@"
} else {
        +NONAME
}
W_2RAT
        !word *+2
        ; see also r@ (core)
!if 1 {
        ; TODO KLUNKY !!!!!!!!!!!!!
        dex
        dex
        stx <XSAVE
        tsx
        lda $103,x
        ldx <XSAVE
        sta 0,x
        tsx
        lda $104,x
        ldx <XSAVE
        sta 1,x
        tsx
        lda $101,x
        pha
        lda $102,x
        ldx <XSAVE        
} else {
        dex
        dex
        stx <XSAVE ; TODO saving in Z might save a couple cycles
        tsx
        txy
        ldx <XSAVE
        lda $104,y
        lda #$ff
        sta 0,x
        lda $103,y
        lda #$ee
        sta 1,x
        lda $101,y
        pha
        lda $102,y
}        
        jmp PUSH

; ****************************************************************************
; :NONAME
; (???)
; ANSI 6.2.0455

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; <>
; (x_1 x_2 -- flag)
; ANSI 6.2.0500

!if ENABLE_CORE_EXT {
        +WORD "<>"
W_NOTEQUAL
        !word *+2
        ; see also = (core)
        ; ldy #0 ; TODO
        lda 0,x
        eor 2,x
        sta 2,x

        lda 1,x
        eor 3,x
        sty 3,x
        ora 2,x

        beq +
        iny
+       sty 2,x
        jmp POP
}

; ****************************************************************************
; ?DO
; (???)
; ANSI 6.2.0620

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; ACTION-OF
; (???)
; Forth 2012 6.2.0698

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; AGAIN
; (???)
; ANSI 6.2.0700

; FIG:
;
;
;      AGAIN         addr  n  ---        (compiling)         P,C2,L0
;               Used in a colon-definition in the form:
;                         BEGIN  ...  AGAIN
;               At run-time, AGAIN forces execution to return to 
;               corresponding BEGIN.  There is no effect on the stack.  
;               Execution cannot leave this loop (unless R> DROP is 
;               executed one level below).
;
;               At compile time, AGAIN compiles BRANCH with an offset from 
;               HERE to addr.  n is used for compile-time error checking.
;
;;
;;                                       AGAIN
;;                                       SCREEN 74 LINE 3

!if ENABLE_CORE_EXT {
!if 0 {
        +WORD_IMM "again"
W_AGAIN
        !word DO_COLON
;          !word ONE
;          !word QPAIR
;          !word COMP
;          !word BRANCH
;          !word BACK
        !word W_PSEMI
}
}

; ****************************************************************************
; BUFFER:
; (???)
; Forth 2012 6.2.0825

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; C"
; (???)
; ANSI 6.2.0855

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; CASE
; (???)
; ANSI 6.2.0873

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; COMPILE,
; (xt --)
; ANSI 6.2.0945

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; DEFER
; Forth 2012 6.2.1173

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; DEFER!
; Forth 2012 6.2.1175

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; DEFER@
; Forth 2012 6.2.1177

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; ENDCASE
; (???)
; ANSI 6.2.1342

; From discussion in ANSI A.3.2.3.2:
;
;    : ENDCASE POSTPONE DROP 0 ?DO POSTPONE THEN LOOP ; IMMEDIATE

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; ENDOF
; (???)
; ANSI 6.2.1343

; From discussion in ANSI A.3.2.3.2:
;
;   : ENDOF >R POSTPONE ELSE R> ; IMMEDIATE

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; ERASE
; (addr u --)
; ANSI 6.2.1350

; FIG:
;
;
;      ERASE         addr  n  ---
;               Clear a region of memory to zero from addr over n 
;               addresses.
;
;
;;
;;                                       ERASE
;;                                       SCREEN 46 LINE 4

!if ENABLE_CORE_EXT {
!if 0 {
        +WORD "erase"
W_ERASE
        !word DO_COLON
        !word W_ZERO
        !word W_FILL
        !word W_PSEMI
}
}

; ****************************************************************************
; FALSE
; (-- false)
; ANSI 6.2.1485

!if ENABLE_CORE_EXT {
        +WORD "false"
}
W_FALSE
        !word DO_CONSTANT
        !word 0

; ****************************************************************************
; HEX
; (--)
; ANSI 6.2.1660

; FIG:
;
;      HEX                                                   L0
;               Set the numeric conversion base to sixteen (hexadecimal).
!if ENABLE_CORE_EXT {
        +WORD "hex"
W_HEX
        !word DO_COLON
        +CLITERAL 16
        !word W_BASE
        !word W_STORE
        !word W_PSEMI
}

; ****************************************************************************
; HOLDS
; Forth 2012 6.2.1675

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; IS
; Forth 2012 6.2.1725

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; MARKER
; (???)
; ANSI 6.2.1850

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; NIP
; (x_1 x_2 -- x_2)
; ANSI 6.2.1930

; The word itself is required by the implmentation (of FIND) but is only visible if CORE-EXT is enabled

!if ENABLE_CORE_EXT {
        +WORD "nip"
} else {
        +NONAME
}
W_NIP
        !word *+2
        lda 0,x
        sta 2,x
        lda 1,x
        sta 3,x
        jmp POP

; ****************************************************************************
; OF
; (???)
; ANSI 6.2.1950

; From discussion in ANSI A.3.2.3.2:
;
;     : OF 1+ >R POSTPONE OVER POSTPONE = POSTPONE IF POSTPONE DROP R> ; IMMEDIATE

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; PAD
; (-- c-addr)
; ANSI 6.2.2000

; FIG:
;
;
;      PAD           ---  addr                               L0
;               Leave the address of the text output buffer, which is a 
;               fixed offset above HERE.
;
;;
;;                                       PAD
;;                                       SCREEN 46 LINE 13

; TODO always enable?

!if ENABLE_CORE_EXT {
        +WORD "pad"
W_PAD
        !word DO_COLON
        !word W_HERE
        !word W_CLITERAL
        !byte 68        ; PAD is 68 bytes above here. TODO ????????????
        !word W_PLUS
        !word W_PSEMI
}

; ****************************************************************************
; PARSE
; (char "ccc<char>" -- c-addr u)
; ANSI 6.2.2008
;
; Parse ccc delimited by char

; The word itself is required by the implementation but will only be visible if CORE-EXT is enabled

!if ENABLE_CORE_EXT {
        +WORD "parse"
} else {
        +NONAME
}
W_PARSE
        !word *+2

        ; ldy #0 ; TODO

        ; hold char in TEMP1
        lda 0,x
        sta <TEMP1

        ; initialize addr
        ; dex
        ; dex
        clc
        lda <INPUT_BUFFER
        adc <IN
        sta 0,x
        lda <INPUT_BUFFER+1
        adc <IN+1
        sta 1,x

        ; initialize len
        dex
        dex
        sty 1,x
        sty 0,x

        ; push a temporary of the end of the buffer
        dex
        dex
        clc
        lda <INPUT_BUFFER
        adc <INPUT_LEN
        sta 0,x
        lda <INPUT_BUFFER+1
        adc <INPUT_LEN+1
        sta 1,x

        ; push a temporary current position
        dex
        dex
        lda 6,x
        sta 0,x
        lda 7,x
        sta 1,x

_parse_loop
        ; check if we're at the end
        lda 0,x
        cmp 2,x
        bne +
        lda 1,x
        cmp 3,x
        beq _parse_done
+

        ; not at the end yet ...
        ; check for terminator
        lda (0,x)
        ; cmp 8,x
        cmp <TEMP1
        beq _parse_found_terminator

        ; advance the current position ...
        clc
        inc 0,x
        bne +
        inc 1,x
+

        ; ... increment the length ...
        clc
        inc 4,x
        bne +
        inc 5,x
+

        ; ... and increment IN
        clc   ; TODO inw
        inc <IN
        bne +
        inc <IN+1
+

        bra _parse_loop

_parse_found_terminator

        ; and increment IN
        clc ; TODO inw
        inc <IN
        bne +
        inc <IN+1
+
        ;bra _parse_done

_parse_done

        jmp POP2

; ****************************************************************************
; PARSE-NAME
; Forth 2012 6.2.2020
; ("<spaces>name<space>" -- c-addr u)


; like PARSE but is delimited by any whitespace

; see discussion in ANSI A.6.2.2008
;
; Skip leading spaces and parse name delimited by space.  c-addr is the address within the
; input buffer and u is the length of the selected string.  If the parse area is empty, the
; resulting string has a zero length.

; The word itself is required by the implementation but will only be visible if CORE-EXT is enabled

!if ENABLE_CORE_EXT {
        +WORD "parse-name"
} else {
        +NONAME
}
W_PARSE_NAME
        !word *+2

        ; ldy #0 ; TODO

        ; initialize addr
        dex
        dex
        clc
        lda <INPUT_BUFFER
        adc <IN
        sta 0,x
        lda <INPUT_BUFFER+1
        adc <IN+1
        sta 1,x

        ; initialize len
        dex
        dex
        sty 1,x
        sty 0,x

        ; push a temporary of the end of the buffer
        dex
        dex
        clc
        lda <INPUT_BUFFER
        adc <INPUT_LEN
        sta 0,x
        lda <INPUT_BUFFER+1
        adc <INPUT_LEN+1
        sta 1,x

        ; push a temporary current position
        dex
        dex
        lda 6,x
        sta 0,x
        lda 7,x
        sta 1,x

_parse_name_skip_whitespace_loop
        ; check if we're at the end
        lda 0,x
        cmp 2,x
        bne +
        lda 1,x
        cmp 3,x
        beq _parse_name_all_done
+

        ; not at the end yet ...
        ; check for whitespace
        lda (0,x)
        jsr isspace
        bne _parse_name_end_of_whitespace

        ; advance the current position ...
        clc
        inc 0,x
        bne +
        inc 1,x
+

        ; ... advance the start position ...
        clc
        inc 6,x
        bne +
        inc 7,x
+

        ; ... and increment IN
        clc   ; TODO inw
        inc <IN
        bne +
        inc <IN+1
+

        bra _parse_name_skip_whitespace_loop

_parse_name_end_of_whitespace

        ; now skip non-whitespace

_parse_name_skip_nonwhitespace_loop
        ; check if we're at the end
        lda 0,x
        cmp 2,x
        bne +
        lda 1,x
        cmp 3,x
        beq _parse_name_all_done
+

        ; not at the end yet ...
        ; check for whitespace
        lda (0,x)
        jsr isspace
        beq _parse_name_found_ending_whitespace

        ; advance the current position ...
        clc
        inc 0,x
        bne +
        inc 1,x
+

        ; ... increment the length ...
        clc
        inc 4,x
        bne +
        inc 5,x
+

        ; ... and increment IN
        clc ; TODO inw
        inc <IN
        bne +
        inc <IN+1
+

        bra _parse_name_skip_nonwhitespace_loop

_parse_name_found_ending_whitespace

        ; increment IN
        clc ; TODO inw
        inc <IN
        bne +
        inc <IN+1
+

_parse_name_all_done

        jmp POP2

; ****************************************************************************
; PICK
; (x_u...x_1 x_0 u -- x_u...x_1 x_0 x_u)
; ANSI 6.2.2030

!if ENABLE_CORE_EXT {
        +WORD "pick"
} else {
        +NONAME
}
W_PICK
        !word *+2
        stx <XSAVE
        clc
        lda 0,x
        asl
        adc <XSAVE
        tax
        lda 0,x
        pha
        lda 1,x
        ldx <XSAVE
        jmp PUT

; ****************************************************************************
; REFILL
; (-- flag)
; ANSI 6.2.2125

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; RESTORE-INPUT
; (x_n...x_1 n -- flag)
; ANSI 6.2.2148

; TODO always enable?

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; ROLL
; (x_u x_u-1...x_0 u -- x_u-1...x_0 x_u)
; ANSI 6.2.2150

!if ENABLE_CORE_EXT {
        +WORD "roll"
} else {
        +NONAME
}
W_ROLL
        !word *+2
        stx <XSAVE
        lda 0,x
        beq +           ; nothing to do if u = 0 except drop u

        tay             ; y = u
        asl
        clc
        adc <XSAVE
        tax             ; x += 2*u

-       lda 2,x
        pha
        lda 3,x
        pha

        lda 0,x
        sta 2,x
        lda 1,x
        sta 3,x

        dex
        dex
        dey
        bne -

        pla
        sta 3,x
        pla
        sta 2,x

+       jmp DROP

; ****************************************************************************
; S\"
; Forth 2012 6.2.2266
; Forth 2012 11.6.2.2266

; Needs to handle escape sequences:
; \a    BEL     7                               OK
; \b    BS      8                               OK use 20 (delete)
; \e    ESC     27                              OK
; \f    FF      12                              OK
; \l    LF      10                              OK
; \m    CR/LF   13 10                           OK just use 13
; \n    newline (implementation dependent)      OK just use 13
; \q            34 (quote)                      OK
; \r    CR      13                              OK
; \t    HT      9 (horizontal tab)              OK
; \v    VT      11 (vertical tab)               ??
; \z    NUL     0                               OK
; \"            34 (quote)                      OK
; \xNN          hex digit NN                    OK?
; \\            92 (backslash)                  OK

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; SAVE-INPUT
; (-- x_n...x_1 n)
; ANSI 6.2.2182

; TODO always enable?

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; SOURCE-ID
; (-- 0|-1)
; ANSI 6.2.2218
; (-- 0|-1|fileid)
; ANSI 11.6.1.2218

; TODO always enable?

; TODO extension to allow fileid if FILE enabled

!if ENABLE_CORE_EXT {
        +WORD "source-id"
W_SOURCE_ID
        !word *+2
        lda <SOURCE_ID
        pha
        lda <SOURCE_ID+1
        jmp PUSH
}

; ****************************************************************************
; TO
; (???)
; ANSI 6.2.2295

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; TRUE
; (-- true)
; ANSI 6.2.2298

!if ENABLE_CORE_EXT {
        +WORD "true"
} else {
        +NONAME
}
W_TRUE
        !word DO_CONSTANT
        !word $ffff

; ****************************************************************************
; TUCK
; (x_1 x_2 -- x_2 x_1 x_2)
; ANSI 6.2.2300

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; U.R
; (u n --)
; ANSI 6.2.2300

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; U>
; (u_1 u_2 flag)
; ANSI 6.2.2350

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; UNUSED
; (-- u)
; ANSI 6.2.2395

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; VALUE
; (???)
; ANSI 6.2.2405

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; WITHIN
; (n_1 n_2 n_3 -- flag)
; ANSI 6.2.2440

!if ENABLE_CORE_EXT {
}

; ****************************************************************************
; \
; ("text" --)
; ANSI 6.2.2535
; ANSI 7.6.2.2535

!if ENABLE_CORE_EXT {
        +WORD_IMM "\\"
W_BACKSLASH
        !word DO_COLON
        +CLITERAL '\r' ; TODO
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
}
