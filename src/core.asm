
; ****************************************************************************
; CORE

; ****************************************************************************
; ! 
; (x a-addr --)
; ANSI 6.1.0010

        +WORD "!", 0
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
; * 
; (n_1 n_2 -- n_3)
; ANSI 6.1.0090

!if 0 {
        +WORD "*", 0
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
; + 
; (n_1 n_2 -- n_3)
; ANSI 6.1.0120

        +WORD "+", 0
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

        +WORD "+!", 0
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

        +WORD "(+loop)", 0
W_PPLOOP
        !word *+2
        ; see also (loop)
        lda 0,x
        sta <TEMP1
        lda 1,x
        sta <TEMP1+1

        stx <XSAVE
        tsx

        clc
        lda $103,x              ; compare old index against limit
        sbc $101,x
        lda $104,x
        sbc $102,x
        asl
        bcs +                   ; if old index >= limit
        iny
+

        clc                     ; increment index
        lda <TEMP1
        adc $101,x
        sta $101,x
        lda <TEMP1+1
        adc $102,x
        sta $102,x

        clc
        lda $103,x              ; compare new index against limit
        sbc $101,x
        lda $104,x
        sbc $102,x
        asl
        bcs +                   ; if new index >= limit
        iny
+

        ldx <XSAVE
        inx                     ; pop increment from data stack
        inx                     ; TODO stack check

        ; now if y & 1 is set, exactly one of the conditions was true
        ; meaning we crossed the boundary and so should exit the loop

        tya
        ror
        bcs +
        ldy #0                  ; BRANCH depends on y == 0
        jmp BRANCH
+       jmp LEAVE

; ****************************************************************************
; , 
; (x --)
; ANSI 6.1.0150

        +WORD ",", 0
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

        +WORD "-", 0
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

        +WORD ".", 0
W_DOT
        !word DO_DEFER
        !word W_SIMPLE_DOT

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
        +WORD "/", 0
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
; 0< 
; (n -- flag)
; ANSI 6.1.0250

        +WORD "0<", 0
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

        +WORD "0=", 0
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

        +WORD "1+", 0
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

        +WORD "1-", 0
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

        +WORD "2*", 0
W_2STAR
        !word *+2
        asl 0,x
        rol 1,x
        jmp NEXT

; ****************************************************************************
; 2/ 
; (x_1 -- x_2)
; ANSI 6.1.0330

        +WORD "2/", 0
W_2SLASH
        !word *+2
        asr 1,x
        ror 0,x
        jmp NEXT

; ****************************************************************************
; 2DROP 
; (x_1 x_2 --)
; ANSI 6.1.0370

        +WORD "2drop", 0
W_2DROP
        !word *+2
        jmp POP2

; ****************************************************************************
; 2DUP 
; (x_1 x_2 -- x_1 x_2 x_1 x_2)
; ANSI 6.1.0380

        +WORD "2dup", 0
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

        +WORD "2over", 0
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

        +WORD "2swap", 0
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

        +WORD ":", 0
W_COLON
        !word DO_COLON
;          !word QEXEC
;          !word SCSP  ; !csp
        !word W_PCREATE
        !word W_RBRACKET
        !word W_PSCODE
DO_COLON
        ; See also DO_DEFER

        ; ldy #0 ; TODO
        
        ; Start executing the word with the code field pointed to by W
        ; (in a new stack frame)
        lda <I+1 ; push I
        pha
        lda <I
        pha

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

        +WORD ";", F_IMMEDIATE
W_SEMI 
        !word DO_COLON
;          !word QCSP
        +LITERAL W_PSEMI
        !word W_COMMA ; COMPILEC?
        
        !word W_LATEST
        +ZBRANCH +
        !word W_LINK
+

        !word W_LBRACKET
        !word W_PSEMI

        +NONAME ; ";s" ?
W_PSEMI
        ; See also exit
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

        +WORD "<", 0
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
; = 
; (x_1 x_2 -- 0 | -1)
; ANSI 6.1.0530

        +WORD "=", 0
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
; >IN 
; (-- a-addr)
; ANSI 6.1.0560

        +WORD ">in", 0
W_IN
        !word DO_COLON
        +LITERAL &IN
        !word W_PSEMI

; ****************************************************************************
; >NUMBER 
; (ud_1 c-addr_1 u_1 -- ud_2 c-addr_2 u_2)
; ANSI 6.1.0570

; c-addr_2 u_2 is the unconverted portion of c-addr_1 u_1

        +WORD ">number", 0
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

        ; +WORD "digit"
        +NONAME
W_DIGIT
        !word *+2
        ; ldy #0 ; TODO
        lda 0,x

        cmp #$c1        ; Map $c1-$da to $41-$5a
        bcc +
        sec
        sbc #$80
+

        cmp #$61        ; Map $61-$7a to $41-$5a
        bcc +
        sec
        sbc #$20
+

        sec
        sbc #'0'
        bmi _digit_bad ; < '0'

        cmp #$A
        bmi +
        ; > '9'

        sec
        sbc #7 ; ('a'-'9'-1)
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

        +WORD ">r", 0
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

        +WORD "?dup", 0
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

        +WORD "@", 0
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
; ACCEPT 
; (c-addr +n_1 -- +n_2)
; ANSI 6.1.0695

; pretty much replaces the obsolescent EXPECT from core-ext

; input terminates on newline or if we reach the character limit
; characters are displayed as they are received (ie, we can assume keyboard input only)

        +WORD "accept", 0
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

        +LITERAL THEME_INPUT
        !word W_THEME

_accept_loop

        ; TODO get rid of index on stack and use OUT?
        ; (index)

        !word W_KEY

        ; (index key)

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
        +LITERAL K_RETURN
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

        +LITERAL THEME_OUTPUT
        !word W_THEME

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

        +WORD "align", 0
W_ALIGN
        !word *+2
        bbr0 <HERE, +
        inw <HERE
+       jmp NEXT

; ****************************************************************************
; ALIGNED 
; (addr -- a-addr)
; ANSI 6.1.0706

        +WORD "aligned", 0
W_ALIGNED
        !word *+2
        lda 0,x
        ror
        bcc +
        inc 0,x
        bne +
        inc 1,x
+       jmp NEXT

; ****************************************************************************
; ALLOT 
; (n --)
; ANSI 6.1.0710

        +WORD "allot", 0
W_ALLOT
        !word DO_COLON
        +LITERAL &HERE
        !word W_PSTORE
        !word W_PSEMI

; ****************************************************************************
; AND 
; (x_1 x_2 -- x_3)
; ANSI 6.1.0720

        +WORD "and", 0
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

        +WORD "base", 0
W_BASE
        !word DO_CONSTANT
        !word &BASE

; ****************************************************************************
; BL 
; (-- char)
; ANSI 6.1.0770

; TODO move to core.f

        +WORD "bl", 0
W_BL
        !word DO_CONSTANT
        !word ' '

; ****************************************************************************
; C! 
; (char c-addr --)
; ANSI 6.1.0850

        +WORD "c!", 0
W_CSTORE
        !word *+2
        lda 2,x
        sta (0,x)
        jmp POP2

; ****************************************************************************
; C@ 
; (c-addr -- char)
; ANSI 6.1.0870

        +WORD "c@", 0
W_CAT
        !word *+2
        ; ldy #0 ; TODO
        lda (0,x)
        sta 0,x
        sty 1,x
        jmp NEXT

; ****************************************************************************
; CONSTANT 
; (x "<spaces>name" --)
; ANSI 6.1.0950

; See also value (core-ext)

        +WORD "constant", 0
W_CONSTANT
        !word DO_COLON
        !word W_CREATE
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

        +WORD "count", 0
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

        +WORD "cr", 0
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

        +WORD "create", 0
W_CREATE
        !word DO_COLON
        !word W_PCREATE
        !word W_LINK
        !word W_PSEMI

        +NONAME ; link the last definition into the dictionary
W_LINK
        !word DO_COLON

        ; update the link in the word
        !word W_GET_CURRENT
        !word W_AT
        !word W_LATEST
        !word W_STORE

        ; add it to the front of the list
        !word W_LATEST
        !word W_GET_CURRENT
        !word W_STORE

        !word W_PSEMI

        +NONAME ; like create but doesn't link into the dictionary
W_PCREATE
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

;!if ENABLE_RUNTIME_CHECKS {
        !word W_DUP
        !word W_ZEQUAL
        +ZBRANCH +
        +LITERAL E_ZERO_LENGTH_NAME
        !word W_THROW
+
;}

;!if ENABLE_RUNTIME_CHECKS {
        +CLITERAL NAME_LEN_MASK ; limit the length
        !word W_OVER
        !word W_LESS
        +ZBRANCH +
        +LITERAL E_NAME_TOO_LONG
        !word W_THROW
+
;}

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
        +LITERAL &LATEST
        !word W_STORE

        !word W_ZERO
        !word W_COMMA           ; leave space for link field

        ; (c-addr u)

        !word W_DUP             ; store name len | flags
        !word W_HERE            ; TODO c,
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

        !word W_HERE
        +LITERAL &LATEST_XT
        !word W_STORE

        +LITERAL DO_VARIABLE    ; default code fields needs to push address of data field
        !word W_COMMA

        ; ()

        !word W_PSEMI

; ****************************************************************************
; DECIMAL 
; (--)
; ANSI 6.1.1170

        +WORD "decimal", 0
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

        +WORD "depth", 0
W_DEPTH
        !word *+2
        ; ldy #0 ; TODO
        lda #TOS-2
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

        +WORD "(do)", 0
W_PDO
        !word *+2
PDO     ; used by (?do)
        ; See also unloop
        ; ldy #0 ; TODO
        lda (<I),y
        pha ; TODO looks like this one is backwards!!!
        inw <I
        lda (<I),y
        pha
        inw <I

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

        +WORD "does>", F_IMMEDIATE
W_DOES
        !word DO_COLON

        +LITERAL W_PSCODE
        !word W_COMMA

        ; add 'jsr DO_DOES'
        +CLITERAL $20           ; jsr $nnnn
        !word W_HERE            ; TODO c,
        !word W_CSTORE
        !word W_ONE
        !word W_ALLOT
        +LITERAL DO_DOES
        !word W_COMMA

        !word W_PSEMI

DO_DOES
        ; TODO this could do with some cleanup
        
        pla
        sta <TEMP1
        pla
        sta <TEMP1+1
!if 0 {
        lda #'i'
        jsr EMIT
        lda <I+1
        jsr put_hex
        lda <I
        jsr put_hex
        lda #' '
        jsr EMIT
        lda #'w'
        jsr EMIT
        lda <W+1
        jsr put_hex
        lda <W
        jsr put_hex
        lda #' '
        jsr EMIT
        lda #'t'
        jsr EMIT
        lda <TEMP1+1
        jsr put_hex
        lda <TEMP1
        jsr put_hex
        lda #' '
        jsr EMIT
}
        
        lda <I+1
        pha
        lda <I
        pha

        lda <TEMP1
        sta <I
        lda <TEMP1+1
        sta <I+1
        inw <I

        clc                     ; push W+2 onto data stack before doing the stuff after DOES>
        lda <W
        adc #2                  
        pha
        lda <W+1
        adc #0

        jmp PUSH

; ****************************************************************************
; DROP 
; (x --)
; ANSI 6.1.1260

        +WORD "drop", 0
W_DROP
        !word *+2
        jmp POP

; ****************************************************************************
; DUP 
; (x -- x x)
; ANSI 6.1.1290

        +WORD "dup", 0
W_DUP
        !word *+2
        lda 0,x
        pha
        lda 1,x
        jmp PUSH

; ****************************************************************************
; EMIT 
; (x --)
; ANSI 6.1.1320

        +WORD "emit", 0
W_EMIT
        !word *+2
        lda 0,x
        jsr BASOUT
        jmp POP

; ****************************************************************************
; ENVIRONMENT? 
; (c-addr u -- false | i*x true)
; ANSI 6.1.1345

        +WORD "environment?", 0
        !word DO_COLON

        !word W_2DUP
        +LITERAL _environment_str_counted_string
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        +LITERAL 255
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_hold
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        +LITERAL HOLD_LEN
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_pad
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        +LITERAL PAD_LEN
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_address_unit_bits
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        +LITERAL 16
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_floored
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        !word W_FALSE ; we're symmetric, not floored
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_max_char
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        +LITERAL 255
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_max_d
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        !word W_TRUE
        +LITERAL $7fff
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_max_n
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        +LITERAL $7fff
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_max_u
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        !word W_TRUE
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_max_ud
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        !word W_TRUE
        !word W_TRUE
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_return_stack_cells
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        +LITERAL 128
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_2DUP
        +LITERAL _environment_str_stack_cells
        !word W_COUNT
        !word W_COMPARE
        !word W_ZEQUAL
        +ZBRANCH +
        +LITERAL (TOS - BOS) >> 1
        !word W_TRUE
        +BRANCH _environment_done
+
        !word W_FALSE

_environment_done
        !word W_PSEMI

_environment_str_counted_string
        +STRING "/counted-string"
_environment_str_hold
        +STRING "/hold"
_environment_str_pad
        +STRING "/pad"
_environment_str_address_unit_bits
        +STRING "address-unit-bits"
_environment_str_floored
        +STRING "floored"
_environment_str_max_char
        +STRING "max-char"
_environment_str_max_d
        +STRING "max-d"
_environment_str_max_n
        +STRING "max-n"
_environment_str_max_u
        +STRING "max-u"
_environment_str_max_ud
        +STRING "max-ud"
_environment_str_return_stack_cells
        +STRING "return-stack-cells"
_environment_str_stack_cells
        +STRING "stack-cells"

; ****************************************************************************
; EVALUTATE 
; (i*x c-addr u -- j*x)
; ANSI 6.1.1360

        +WORD "evaluate", 0
W_EVALUATE        
        !word DO_COLON

        !word W_SAVE_INPUT
        !word W_NTOR

        ; TODO we might want to move the setting of INPUT_LEN and INPUT_BUFFER to PEVALUATE?
        +LITERAL &INPUT_LEN
        !word W_STORE
        +LITERAL &INPUT_BUFFER
        !word W_STORE
        +LITERAL -1
        +LITERAL &SOURCE_ID
        !word W_STORE

        +LITERAL W_PEVALUATE

        !word W_CATCH

        !word W_NRFROM
        !word W_RESTORE_INPUT
        !word W_DROP            ; TODO check status from restore

        !word W_THROW           ; Propagate an exception if there was one

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
        !word W_QSTACK ; TODO where to put this? should we check for >= n available cells?
        !word W_PARSE_NAME

        ; (c-addr u)

        !word W_QDUP
        +ZBRANCH _pevaluate_done_loop

        !word W_2DUP
        !word W_FIND_NAME

        ; (c-addr u 0 | c-addr u nt)

        !word W_QDUP
        +ZBRANCH _pevaluate_word_not_found

        ; (c-addr u nt)

        !word W_NIP
        !word W_NIP

        ; (nt)
        
        !word W_STATE
        !word W_AT
        +ZBRANCH _pevaluate_interpret

        ; compiling ...        
        !word W_NAME_TO_COMPILE
        !word W_EXECUTE
        +BRANCH _pevaluate_loop

_pevaluate_interpret
        ; TODO check compile-only and throw E_INTERPRET_COMPILE_ONLY where appropriate
        !word W_NAME_TO_INTERPRET
        !word W_EXECUTE
        +BRANCH _pevaluate_loop

_pevaluate_word_not_found

        ; (c-addr u)

        !word W_2DUP
        !word W_STONUMBER
        +ZBRANCH _pevaluate_stonumber_failed

        ; (c-addr u d)

        !word W_DROP ; drop MSW

        !word W_NIP  ; drop c-addr u
        !word W_NIP

        ; (n)

        !word W_STATE
        !word W_AT
        +ZBRANCH _pevaluate_loop ; if interpreting, we're done

        ; compiling ...
        +LITERAL W_PLITERAL
        !word W_COMMA ; COMPILEC?
        !word W_COMMA
        +BRANCH _pevaluate_loop

_pevaluate_stonumber_failed
        ; (c-addr u d)
!if 0 {
        !word W_2DROP
        ; (c-addr u)

        ; TODO should this be printed in the handler?
        +LITERAL THEME_ERROR
        !word W_THEME

        !word W_BL,W_EMIT
        !word W_TYPE
        +DOTQ "? "

        +LITERAL THEME_OUTPUT
        !word W_THEME
} 

        +LITERAL E_UNDEFINED_WORD
        !word W_THROW

_pevaluate_done_loop
        ; (c-addr)
        !word W_DROP

        !word W_PSEMI

; ****************************************************************************
; EXECUTE 
; (i*x xt -- j*x)
; ANSI 6.1.1370

        +WORD "execute", 0
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
        inx             ; TODO stack check
        inx
        jmp &DO_JUMP_W

; ****************************************************************************
; EXIT 
; (???)
; ANSI 6.1.1380

        +WORD "exit", 0
W_EXIT
        !word *+2
        ; See also ;
!if 0 {        
        jsr RDUMP
        jsr CR
        ldy #0
}
        pla ; TODO backwards???
        sta <I
        pla
        sta <I+1
        jmp NEXT

; ****************************************************************************
; FILL 
; (c-addr u char --)
; ANSI 6.1.1540

        +WORD "fill", 0
W_FILL
        !word *+2
        lda 2,x                 ; Skip if len = 0
        ora 3,x                 ; TODO could move check below and save a byte or two
        bne +
        jmp POP3
+
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
        !byte $0b               ; F018B 12-byte format ; TODO use 11-byte?
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
; HERE 
; (-- addr)
; ANSI 6.1.1650

        +WORD "here", 0
W_HERE
        !word *+2
        lda <HERE
        pha
        lda <HERE+1
        jmp PUSH

; ****************************************************************************
; I 
; (???)
; ANSI 6.1.1680

        +WORD "i", 0
W_I
        !word W_RAT+2      ; share the code for R

; ****************************************************************************
; IMMEDIATE 
; (--)
; ANSI 6.1.1710

; TODO can this be moved to core.f?

        +WORD "immediate", 0
W_IMMEDIATE
        !word DO_COLON
        ; see also compile-only (internals)
        !word W_LATEST
        !word W_QDUP
        +ZBRANCH +
        !word W_2PLUS
        !word W_DUP
        !word W_CAT
        +CLITERAL F_IMMEDIATE
        !word W_OR
        !word W_SWAP
        !word W_CSTORE
+
        !word W_PSEMI

; ****************************************************************************
; INVERT 
; (x_1 -- x_2)
; ANSI 6.1.1720
; flip all bits

        +WORD "invert", 0
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

        +WORD "j", 0
W_J
        !word *+2
        stx <XSAVE
        tsx
        lda $107,x
        pha
        lda $108,x
        ldx <XSAVE
        jmp PUSH

; ****************************************************************************
; KEY 
; (-- char)
; ANSI 6.1.1750

        +WORD "key", 0
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

        +WORD "leave", 0
W_LEAVE
        !word *+2
LEAVE                   ; used by (loop) and (+loop)
        pla ; TODO 
        pla
        pla
        pla
        pla ; TODO new I
        sta <I+1 ; TODO BACKWARDS???
        pla
        sta <I
        jmp NEXT

; ****************************************************************************
; LOOP 
; (???)
; ANSI 6.1.1800

; See core.f

        +WORD "(loop)", 0
W_PLOOP
        !word *+2
        ; see also (+loop)
        stx <XSAVE
        tsx
        inc $101,x              ; increment loop index
        bne +
        inc $102,x
+
!if 0{
        clc
        lda $103,x              ; compare against loop limit
        sbc $101,x
        lda $104,x              ; TODO we should be able to get away with just testing for equality
        sbc $102,x
        ldx <XSAVE
        asl
        bcs +                   ; if index >= limit
} else {
        lda $103,x              ; this one is more correct
        eor $101,x             
        sta <TEMP1
        lda $104,x
        eor $102,x
        ldx <XSAVE
        ora <TEMP1
        beq +                   ; if index = limit
}
        jmp BRANCH
+       jmp LEAVE

; ****************************************************************************
; LSHIFT 
; (x_1 u -- x_2)
; ANSI 6.1.1805

        +WORD "lshift", 0
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
; NEGATE
; (n_1 -- n_2)
; ANSI 6.1.1910
; change sign (MINUS in FIG)
; from ANSI A.3.2.1:
;       : NEGATE INVERT 1+ ;

        +WORD "negate", 0
W_NEGATE
        !word *+2
        ; ldy #0 ; TODO
        ; see also DNEGATE (double)
        ; TODO use neg? neg doesn't set or use the carry flag
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

        +WORD "or", 0
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

        +WORD "over", 0
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

        +WORD "postpone", F_IMMEDIATE | F_COMPILE_ONLY
W_POSTPONE
        !word DO_COLON
        !word W_PARSE_NAME

        !word W_FIND_NAME
        !word W_QDUP
        +ZBRANCH _postpone_done ; TODO error if not found

        ; (nt)

        !word W_DUP
        !word W_NAME_TO_XT
        !word W_SWAP        
        !word W_QIMMEDIATE
        !word W_ZEQUAL
        +ZBRANCH +

        ; non-immediate
        ; TODO can we use name>compile?
        +LITERAL W_PLITERAL
        !word W_COMMA
        !word W_COMMA ; the xt
        +LITERAL W_COMMA
+
        !word W_COMMA ; the xt in the immediate case

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

        +WORD "quit", 0
W_QUIT
        !word DO_COLON
        +LITERAL E_QUIT
        !word W_THROW

        +NONAME
W_PQUIT
        !word DO_COLON

        ; The return stack will already have been cleared by the exception

!if ENABLE_BLOCK {
        !word W_ZERO
        !word W_BLK
        !word W_STORE
}

        !word W_LBRACKET

        !word W_ZERO
        +LITERAL &SOURCE_ID
        !word W_STORE

_pquit_read_loop

        !word W_REFILL
        !word W_DROP
        !word W_PEVALUATE               ; any exception will be caught in W_MAIN

        !word W_STATE
        !word W_AT
        !word W_ZEQUAL
        +ZBRANCH +

        +LITERAL THEME_PROMPT
        !word W_THEME

        +DOTQ " ok"
        !word W_CR
+
        +BRANCH _pquit_read_loop

; ****************************************************************************
; R>
; (???)
; ANSI 6.1.2060

        +WORD "r>", 0
W_RFROM
        !word *+2
        ; see also 2r> (core-ext), nr> (tools-ext)
        dex ; TODO data stack check
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

        +WORD "r@", 0
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
; ROT
; (x_1 x_2 x_3 -- x_2 x_3 x_1)
; ANSI 6.1.2160

; TODO native implementation?

        +WORD "rot", 0
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

        +WORD "rshift", 0
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
; SOURCE
; (-- c-addr u)
; ANSI 6.1.2216

        +WORD "source", 0
W_SOURCE
        !word DO_COLON
        +LITERAL &INPUT_BUFFER
        !word W_AT
        +LITERAL &INPUT_LEN
        !word W_AT
        ; TODO should we ripping of >IN leading chars?
        !word W_PSEMI

; ****************************************************************************
; STATE
; (-- a-addr)
; ANSI 6.1.2250
; ANSI 15.6.2.2250

        +WORD "state", 0
W_STATE
        !word DO_CONSTANT
        !word &STATE

; ****************************************************************************
; SWAP
; (x_1 x_2 -- x_2 x_1)
; ANSI 6.1.2260

        +WORD "swap", 0
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
; TYPE
; (c-addr u --)
; ANSI 6.1.2310

; TODO can we use primm? the problem is it uses a null terminated string

        +WORD "type", 0
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
; U<
; (u_1 u_2 -- flag)
; ANSI 6.1.2340

        +WORD "u<", 0
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

        +WORD "um*", 0
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

        +WORD "um/mod", 0
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

        +WORD "unloop", 0
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
; VARIABLE
; ("<spaces>name" --)
; ANSI 6.1.2410

        +WORD "variable", 0
W_VARIABLE    
        !word DO_COLON
        !word W_CREATE
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
; XOR
; (x_1 x_2 -- x_3)
; ANSI 6.1.2490

        +WORD "xor", 0
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

        +WORD "[", F_IMMEDIATE
W_LBRACKET
        !word DO_COLON
        !word W_ZERO
        !word W_STATE
        !word W_STORE
        !word W_PSEMI

; ****************************************************************************
; ]
; (--)
; ANSI 6.1.2540

        +WORD "]", 0
W_RBRACKET
        !word DO_COLON
        +CLITERAL $C0 ; TODO ??????????????
        !word W_STATE
        !word W_STORE
        !word W_PSEMI
