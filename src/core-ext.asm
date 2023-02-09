
; ****************************************************************************
; CORE EXT

; TODO always enable:
; PAD?
; PARSE
; PARSE-NAME
; SAVE-INPUT?
; RESTORE-INPUT?
; SOURCE-ID?

!if ENABLE_CORE_EXT {

; ****************************************************************************
; #TIB
; (-- a-addr)
; ANSI 6.2.0060 - marked as obsolescent (see SOURCE in core)
; (not in Forth 2012?)

; ****************************************************************************
; .(
; ("text" --)
; ANSI 6.2.0200

; from ANSI A.6.2.2008 discussion:
;     : .( [CHAR] ) PARSE TYPE ; IMMEDIATE

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
!if 0 {
        +WORD ".r"
W_DOTR
        !word DO_COLON
;          !word TOR
;          !word STOD
;          !word RFROM
;          !word DDOTR
        !word W_SEMI
}

; ****************************************************************************
; 0<>
; (x -- flag)
; ANSI 6.2.0260

; ****************************************************************************
; 0>
; (n -- flag)
; ANSI 6.2.0280

; ****************************************************************************
; 2>R
; (???)
; ANSI 6.2.0340

; ****************************************************************************
; 2R>
; (???)
; ANSI 6.2.0410

; ****************************************************************************
; 2R@
; (???)
; ANSI 6.2.0415

; ****************************************************************************
; :NONAME
; (???)
; ANSI 6.2.0455

; ****************************************************************************
; <>
; (x_1 x_2 -- flag)
; ANSI 6.2.0500

; ****************************************************************************
; ?DO
; (???)
; ANSI 6.2.0620

; ****************************************************************************
; ACTION-OF
; (???)
; Forth 2012 6.2.0698

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
!if 0 {
        +WORD_IMM "again"
W_AGAIN
        !word DO_COLON
;          !word ONE
;          !word QPAIR
;          !word COMP
;          !word BRANCH
;          !word BACK
        !word W_SEMI
}

; ****************************************************************************
; BUFFER:
; (???)
; Forth 2012 6.2.0825

; ****************************************************************************
; C"
; (???)
; ANSI 6.2.0855

; ****************************************************************************
; CASE
; (???)
; ANSI 6.2.0873

; ****************************************************************************
; COMPILE,
; (xt --)
; ANSI 6.2.0945

; ****************************************************************************
; CONVERT
; (ud_1 c-addr_1 -- ud_2 c-addr_2) - marked as obsolescent (see >NUMBER)
; ANSI 6.2.0970
; Not in Forth 2012?

; ****************************************************************************
; DEFER
; Forth 2012 6.2.1173

; ****************************************************************************
; DEFER!
; Forth 2012 6.2.1175

; ****************************************************************************
; DEFER@
; Forth 2012 6.2.1177

; ****************************************************************************
; ENDCASE
; (???)
; ANSI 6.2.1342

; From discussion in ANSI A.3.2.3.2:
;
;    : ENDCASE POSTPONE DROP 0 ?DO POSTPONE THEN LOOP ; IMMEDIATE

; ****************************************************************************
; ENDOF
; (???)
; ANSI 6.2.1343

; From discussion in ANSI A.3.2.3.2:
;
;   : ENDOF >R POSTPONE ELSE R> ; IMMEDIATE

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
!if 0 {
        +WORD "erase"
W_ERASE
        !word DO_COLON
        !word W_ZERO
        !word W_FILL
        !word W_SEMI
}

; ****************************************************************************
; EXPECT
; (c-addr +n --) - marked as obsolescent, superseded by ACCEPT in core
; ANSI 6.2.1390
; Not in Forth 2012?

; FIG:
;
;
;      EXPECT        addr  count  ---                        L0
;               Transfer characters from the terminal to address, until a 
;               "return" or the count of characters have been received.  
;               One or more nulls are added at the end of the text.
;
; TODO why doesn't it use WORD?

        +WORD "expect"
W_EXPECT
        !word DO_COLON
!if 1 {
        !word W_ACCEPT
        ; TODO store in SPAN?
        !word W_DROP
} else {        
;          !word OVER
;          !word PLUS
;          !word OVER
;          !word PDO


;L1736:    !word KEY


; Check for and handle backspace

;          !word DUP
;          !word CLITERAL
;          !byte $E
;          !word PORIGIN ; ORIG + $E -> backspace char ($08) ?
;          !word AT
;          !word EQUAL
;          !word ZBRANCH
;L1744:    !word $1F       ; L1760-L1744
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
;          !word BRANCH
;L1759:    !word $27       ; L1779-L1759


; Check for and handle linefeed

;L1760:    !word DUP
;          !word CLITERAL
;          !byte $0A	
;          !word EQUAL
;          !word ZBRANCH
;L1765:    !word $0E       ; L1772-L1765
;          !word LEAVE
;          !word DROP
;          !word BL
;          !word ZERO
;          !word BRANCH
;L1771:    !word 04        ; L1773-L1771


;L1772:    !word DUP
;L1773:    !word I
;          !word CSTOR
;          !word ZERO
;          !word I
;          !word 1PLUS
;          !word STORE


;L1779:    !word EMIT
;          !word PLOOP
;L1781:    !word $FFA9
;          !word DROP      ; L1736-L1781
}
        !word W_SEMI

; ****************************************************************************
; FALSE
; (-- false)
; ANSI 6.2.1485

; ****************************************************************************
; HEX
; (--)
; ANSI 6.2.1660

; FIG:
;
;      HEX                                                   L0
;               Set the numeric conversion base to sixteen (hexadecimal).
        +WORD "hex"
W_HEX
        !word DO_COLON
        !word W_CLITERAL
        !byte 16
        !word W_BASE
        !word W_STORE
        !word W_SEMI

; ****************************************************************************
; HOLDS
; Forth 2012 6.2.1675

; ****************************************************************************
; IS
; Forth 2012 6.2.1725

; ****************************************************************************
; MARKER
; (???)
; ANSI 6.2.1850

; ****************************************************************************
; NIP
; (x_1 x_2 -- x_2)
; ANSI 6.2.1930

        +WORD "nip"
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
!if 0 {
        +WORD "pad"
W_PAD
        !word DO_COLON
;          !word HERE
        !word W_CLITERAL
        !byte 68        ; PAD is 68 bytes above here.
        !word W_PLUS
        !word W_SEMI
}

; ****************************************************************************
; PARSE
; (char "ccc<char>" -- c-addr u)
; ANSI 6.2.2008
;
; Parse ccc delimited by char

        +WORD "parse"
W_PARSE
        !word *+2

        ; TODO !!!!!!!!!!!!!!!!!!!!

!if 0 {
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
        cmp #' '
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
        clc
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
        cmp #' '
        beq _parse_name_all_done

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
        clc
        inc <IN
        bne +
        inc <IN+1
+

        bra _parse_name_skip_nonwhitespace_loop

_parse_name_all_done

        bra POPTWO

}
        jmp NEXT


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

        +WORD "parse-name"
W_PARSE_NAME
        !word *+2

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
        cmp #' '
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
        clc
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
        cmp #' '
        beq _parse_name_all_done

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
        clc
        inc <IN
        bne +
        inc <IN+1
+

        bra _parse_name_skip_nonwhitespace_loop

_parse_name_all_done

        jmp POPTWO

; ****************************************************************************
; PICK
; (x_u...x_1 x_u u -- x_u...x_1 x_0 x_u)
; ANSI 6.2.2030

; ****************************************************************************
; QUERY
; (--)
; ANSI 6.2.2040 - marked as obsolescent (see ACCEPT in core, EVALUTATE in core)
; Not in Forth 2012?

; FIG:
;
;      QUERY
;               Input 80 characters of text (or until a "return") from the 
;               operators terminal.  Text is positioned at the address 
;               contained in TIB with IN set to zero.
        +WORD "query"
W_QUERY
        !word DO_COLON
        !word W_TIB
        !word W_AT
        !word W_CLITERAL
        !byte 80        ; 80 characters from terminal
        !word W_EXPECT
        !word W_ZERO
        !word W_IN 
        !word W_STORE
        !word W_SEMI

; ****************************************************************************
; REFILL
; (-- flag)
; ANSI 6.2.2125

; ****************************************************************************
; RESTORE-INPUT
; (x_n...x_1 n -- flag)
; ANSI 6.2.2148

; ****************************************************************************
; ROLL
; (x_u x_u-1...x_0 u -- x_u-1...x_0 x_u)
; ANSI 6.2.2150

; ****************************************************************************
; S\"
; Forth 2012 6.2.2266

; Needs to handle escape sequences:
; \a    BEL     7                               ?? use 20 (delete)
; \b    BS      8                               ??
; \e    ESC     27                              OK
; \f    FF      12                              OK
; \l    LF      10                              ??
; \m    CR/LF   13 10                           ??
; \n    newline (implementation dependent)      ??
; \q            34 (quote)                      OK
; \r    CR      13                              OK
; \t    HT      9 (horizontal tab)              ??
; \v    VT      11 (vertical tab)               ??
; \z    NUL     0                               OK
; \"            34 (quote)                      OK
; \xNN          hex digit NN                    OK?
; \\            92 (backslash)                  OK?


; ****************************************************************************
; SAVE-INPUT
; (-- x_n...x_1 n)
; ANSI 6.2.2182

; ****************************************************************************
; SOURCE-ID
; (-- 0|-1)
; ANSI 6.2.2218
        +WORD "source-id"
W_SOURCE_ID
        !word *+2
        lda <SOURCE_ID
        pha
        lda <SOURCE_ID+1
        jmp PUSH

; ****************************************************************************
; SPAN
; (-- a-addr)
; ANSI 6.2.2240 - marked as obsolescent (see ACCEPT)
; Not in Forth 2012?

; ****************************************************************************
; TIB
; (-- c-addr) 
; ANSI 6.2.2290 - marked as obsolescent (see SOURCE)
; Not in Forth 2012?

; FIG:
;
;      TIB           ---  addr                               U
;               A user variable containing the address of the terminal 
;               input buffer.

; TODO this returns the address of a pointer to the TIB !!!!!!!!!

        +WORD "tib"
W_TIB
        !word DO_USER
        !byte U_TIB

; ****************************************************************************
; TO
; (???)
; ANSI 6.2.2295

; ****************************************************************************
; TRUE
; (-- true)
; ANSI 6.2.2298

; ****************************************************************************
; TUCK
; (x_1 x_2 -- x_2 x_1 x_2)
; ANSI 6.2.2300

; ****************************************************************************
; U.R
; (u n --)
; ANSI 6.2.2300

; ****************************************************************************
; U>
; (u_1 u_2 flag)
; ANSI 6.2.2350

; ****************************************************************************
; UNUSED
; (-- u)
; ANSI 6.2.2395

; ****************************************************************************
; VALUE
; (???)
; ANSI 6.2.2405

; ****************************************************************************
; WITHIN
; (n_1 n_2 n_3 -- flag)
; ANSI 6.2.2440

; ****************************************************************************
; [COMPILE]
; ("text" --)
; ANSI 6.2.2530

; FIG:
;      [COMPILE]                                             P,C
;               Used in a colon-definition in the form:
;                         : xxx   [COMPILE]   FORTH  ;
;               [COMPILE] will force the compilation of an immediate 
;               definition, that would otherwise execute during 
;               compilation.  The above example will select the FORTH 
;               vocabulary when xxx executes, rather than at compile time.
;
;;
;;                                       [COMPILE]
;;                                       SCREEN 51 LINE 2
!if 0 {
        +WORD_IMM "[compile]"
W_BCOMPILE
        !word DO_COLON
;          !word DFIND
;          !word ZEQU
;          !word ZERO
;          !word QERR
;          !word DROP
;          !word CFA
;          !word COMMA
        !word W_SEMI
}

; ****************************************************************************
; \
; ("text" --)
; ANSI 6.2.2535

}
