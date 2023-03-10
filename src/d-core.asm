
; ****************************************************************************
; CORE

; TODO move these to forth

        +CREATE_ENV "/hold"
        !word DO_CONSTANT
        !word HOLD_LEN

        +CREATE_ENV "/pad"
        !word DO_CONSTANT
        !word PAD_LEN

        +CREATE_ENV "max-d"
        !word DO_CONSTANT ; TODO 2CONSTANT
        !word $ffff
        !word $7fff

        +CREATE_ENV "max-ud"
        !word DO_CONSTANT ; TODO 2CONSTANT
        !word $ffff
        !word $ffff

        +CREATE_ENV "stack-cells"
        !word DO_CONSTANT
        !word (TOS - BOS) >> 1

; ****************************************************************************
; ! 
; (x a-addr --)

        +CREATE "!", 0
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

!if 0 {
        +CREATE "*", 0
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

        +CREATE "+", 0
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

        +CREATE "+!", 0
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

; See core.f

        +CREATE_INTERNAL "(+loop)", 0
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

        +CREATE ",", 0
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

        +CREATE "-", 0
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

        +CREATE ".", 0
W_DOT
        !word DO_DEFER
        !word W_SIMPLE_DOT

; ****************************************************************************
; / 
; (n_1 n_2 -- n_3)

; TODO WRONG FOR SIGNED !!!!!!!!!

; See core.f for a temporary correct one

!if 0 {
        +CREATE "/", 0
W_SLASH
        !word *+2
        ; TODO does this handle negative values?
        ; TODO check for division by zero
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

        +CREATE "0<", 0
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

        +CREATE "0=", 0
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

        +CREATE "1+", 0
W_1PLUS
        !word *+2
        inc 0,x
        bne +
        inc 1,x
+       jmp NEXT

; ****************************************************************************
; 1- 
; (n_1 -- n_2)

        +CREATE "1-", 0
W_1MINUS
        !word *+2
        lda 0,x
        bne +
        dec 1,x
+       dec 0,x
        jmp NEXT

; ****************************************************************************
; 2* 
; (x_1 -- x_2)

        +CREATE "2*", 0
W_2STAR
        !word *+2
        asl 0,x
        rol 1,x
        jmp NEXT

; ****************************************************************************
; 2/ 
; (x_1 -- x_2)

        +CREATE "2/", 0
W_2SLASH
        !word *+2
        asr 1,x
        ror 0,x
        jmp NEXT

; ****************************************************************************
; 2DROP 
; (x_1 x_2 --)

        +CREATE "2drop", 0
W_2DROP
        !word *+2
        jmp POP2

; ****************************************************************************
; 2DUP 
; (x_1 x_2 -- x_1 x_2 x_1 x_2)

        +CREATE "2dup", 0
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

        +CREATE "2over", 0
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

        +CREATE "2swap", 0
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

        +CREATE ":", 0
W_COLON
        !word DO_COLON
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

        +CREATE ";", F_IMMEDIATE
W_SEMI 
        !word DO_COLON
;          !word QCSP
        +LITERAL W_PSEMI
        !word W_COMMA
        
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

        +CREATE "<", 0
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

        +CREATE "=", 0
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

        +CREATE ">in", 0
W_IN
        !word DO_VARIABLE
IN        
        !word 0

; ****************************************************************************
; >NUMBER 
; (ud_1 c-addr_1 u_1 -- ud_2 c-addr_2 u_2)

; c-addr_2 u_2 is the unconverted portion of c-addr_1 u_1

        +CREATE ">number", 0
W_TONUMBER
        !word DO_COLON
        !word W_ZERO ; if dpl is >= 0, (>number) won't accept another '.'
        !word W_DPL
        !word W_STORE
        !word W_PTONUMBER
        !word W_PSEMI

        ; +CREATE "(>number)", 0
        +NONAME
W_PTONUMBER
        !word DO_COLON

        ; TODO cleanup!
 
_tonumber_loop
        ; (ud c-addr u) = (ud-low ud-high c-addr u)

        !word W_2DUP
        !word W_2TOR    ; (ud c-addr u) (R: c-addr u)

        !word W_QDUP
        +ZBRANCH _tonumber_done_1drop ; reached end of string

        !word W_DROP
        !word W_CAT
        !word W_DUP     ; (ud c c) (R: c-addr u)
        !word W_DIGIT
        +ZBRANCH _tonumber_digit_bad
        ; (ud c n) (R: c-addr u)
        !word W_NIP 
        ; (ud n) (R: c-addr u)
        !word W_DPL
        !word W_AT
        !word W_ZLESS
        +ZBRANCH _tonumber_inc_dpl
        +BRANCH _tonumber_handle_digit

_tonumber_digit_bad
        ; (ud c) (R: c-addr u)
        +CLITERAL '.'
        !word W_EQUAL
        +ZBRANCH _tonumber_done_0drop ; reached invalid char
        !word W_DPL
        !word W_AT
        !word W_ZLESS
        +ZBRANCH _tonumber_done_0drop ; invalid char (already seen a '.')
        ; (ud) (R: c-addr u)
        !word W_ONE
        !word W_DPL
        !word W_PSTORE
        +BRANCH _tonumber_next

_tonumber_inc_dpl
        ; (ud n) (R: c-addr u)
        !word W_ONE
        !word W_DPL
        !word W_PSTORE
        ; fall through ...

_tonumber_handle_digit
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

_tonumber_next
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

        ; +CREATE "digit"
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

        +CREATE ">r", 0
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

        +CREATE "?dup", 0
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

        +CREATE "@", 0
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

; input terminates on newline or if we reach the character limit
; characters are displayed as they are received (ie, we can assume keyboard input only)

; TODO move to bootstrap1.f

        +CREATE "accept", 0
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

        +CREATE "align", 0
W_ALIGN
        !word *+2
        lda HERE ; TODO can be simplified
        and #$01
        beq +
        inc HERE
        bne +
        inc HERE+1
+       jmp NEXT

; ****************************************************************************
; ALIGNED 
; (addr -- a-addr)

        +CREATE "aligned", 0
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

        +CREATE "allot", 0
W_ALLOT
        !word DO_COLON
        +LITERAL HERE
        ; TODO check for overflow
        !word W_PSTORE
        !word W_PSEMI

; ****************************************************************************
; AND 
; (x_1 x_2 -- x_3)

        +CREATE "and", 0
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

        +CREATE "base", 0
W_BASE
        !word DO_CONSTANT
        !word &BASE

; ****************************************************************************
; BL 
; (-- char)

; TODO move to core.f

        +CREATE "bl", 0
W_BL
        !word DO_CONSTANT
        !word ' '

; ****************************************************************************
; C! 
; (char c-addr --)

        +CREATE "c!", 0
W_CSTORE
        !word *+2
        lda 2,x
        sta (0,x)
        jmp POP2

; ****************************************************************************
; C@ 
; (c-addr -- char)

        +CREATE "c@", 0
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

; See also value (core-ext)

        +CREATE "constant", 0
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

        +CREATE "count", 0
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

        +CREATE "cr", 0
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

        +CREATE "create", 0
W_CREATE
        !word DO_COLON
        !word W_PCREATE
        !word W_LINK
        !word W_PSEMI

        +NONAME ; link the last definition into the dictionary
W_LINK
        !word DO_COLON

        ; TODO check for an existing definition

;          +ZBRANCH L2163
;          !word DROP
;          !word NFA
;          !word IDDOT
;          +CLITERAL 4
;          !word MESSAGE
;          !word SPACE

        ; update the link in the word
        !word W_CURRENT
        !word W_AT
        !word W_AT
        !word W_LATEST
        !word W_STORE

        ; add it to the front of the list
        !word W_LATEST
        !word W_CURRENT
        !word W_AT
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

        !word W_DUP
        !word W_ZEQUAL
        +LITERAL E_ZERO_LENGTH_NAME
        !word W_AND
        !word W_THROW

        +CLITERAL NAME_LEN_MASK ; limit the length
        !word W_OVER
        !word W_LESS
        +LITERAL E_NAME_TOO_LONG
        !word W_AND
        !word W_THROW

!if CASE_INSENSITIVE {
        !word W_2DUP
        !word W_LOWER
}

        ; TODO if no existing defintion found ...

        !word W_ALIGN           

        !word W_HERE
        +LITERAL LATEST
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
        +LITERAL LATESTXT
        !word W_STORE

        +LITERAL DO_VARIABLE    ; default code fields needs to push address of data field
        !word W_COMMA

        ; ()

        !word W_PSEMI

; ****************************************************************************
; DEPTH 
; (-- +n)

        +CREATE "depth", 0
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

; See core.f

; TODO share code with 2>r

        +CREATE_INTERNAL "(do)", 0
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

        +CREATE "does>", F_IMMEDIATE
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

        +CREATE "drop", 0
W_DROP
        !word *+2
        jmp POP

; ****************************************************************************
; DUP 
; (x -- x x)

        +CREATE "dup", 0
W_DUP
        !word *+2
        lda 0,x
        pha
        lda 1,x
        jmp PUSH

; ****************************************************************************
; EMIT 
; (x --)

        +CREATE "emit", 0
W_EMIT
        !word *+2
        lda 0,x
        jsr BASOUT
        jmp POP

; ****************************************************************************
; EVALUTATE 
; (i*x c-addr u -- j*x)

        +CREATE "evaluate", 0
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
        +LITERAL SOURCE_ID
        !word W_STORE
        !word W_ZERO
        !word W_SOURCE_LINE
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

        +CREATE_INTERNAL "(evaluate)", 0
W_PEVALUATE             ; ( -- )
        !word DO_COLON

        !word W_ZERO ; TODO should this be done in EVALUATE too?
        !word W_IN
        !word W_STORE

_pevaluate_loop
        !word W_QSTACK ; TODO where to put this? should we check for >= n available cells?
        !word W_PARSE_NAME

!if 0 {
        +CLITERAL '['
        !word W_EMIT
        !word W_2DUP,W_TYPE
        +CLITERAL ']'
        !word W_EMIT,W_CR
}

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
        !word W_NAME_TO_INTERPRET
        !word W_EXECUTE
        +BRANCH _pevaluate_loop

_pevaluate_word_not_found

        ; (c-addr u)

        !word W_2DUP
        !word W_STONUMBER
        +ZBRANCH _pevaluate_stonumber_failed

        ; (c-addr u d)

;        !word W_DPL
;        !word W_AT
;        !word W_ZLESS
;        +ZBRANCH +

        !word W_DROP ; drop MSW

+
        !word W_NIP  ; drop c-addr u
        !word W_NIP

        ; (n)

        !word W_STATE
        !word W_AT
        +ZBRANCH _pevaluate_loop ; if interpreting, we're done

        ; compiling ...
        ; TODO handle double literal case ...
        +LITERAL W_PLITERAL
        !word W_COMMA ; COMPILEC?
        !word W_COMMA
        +BRANCH _pevaluate_loop

_pevaluate_stonumber_failed
        ; (c-addr u d)

        +LITERAL E_UNDEFINED_WORD
        !word W_THROW

_pevaluate_done_loop
        ; (c-addr)
        !word W_DROP

        !word W_PSEMI

; ****************************************************************************
; EXECUTE 
; (i*x xt -- j*x)

        +CREATE "execute", 0
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

        +CREATE "exit", 0
W_EXIT
        !word *+2
        ; See also ;
        pla ; TODO backwards???
        sta <I
        pla
        sta <I+1
        jmp NEXT

; ****************************************************************************
; FILL 
; (c-addr u char --)

        +CREATE "fill", 0
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

        +CREATE "here", 0
W_HERE
        !word DO_CONSTANT
HERE
        !word 0
        
; ****************************************************************************
; I 
; (???)

        +CREATE "i", 0
W_I
        !word W_RAT+2      ; share the code for R

; ****************************************************************************
; INVERT 
; (x_1 -- x_2)
; flip all bits

        +CREATE "invert", 0
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

        +CREATE "j", 0
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

        +CREATE "key", 0
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

        +CREATE "leave", 0
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

; See core.f

        +CREATE_INTERNAL "(loop)", 0
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

        +CREATE "lshift", 0
W_LSHIFT
        !word *+2
        lda 0,x
        ; TODO check limits on u, throw if bad?
beq +
-       clc
        rol 2,x
        rol 3,x
        dec
        bne -
+       jmp POP

; ****************************************************************************
; OR
; (x_1 x_2 -- x_3)

        +CREATE "or", 0
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

        +CREATE "over", 0
W_OVER
        !word *+2
        lda 2,x
        pha
        lda 3,x
        jmp PUSH

; ****************************************************************************
; POSTPONE
; ("<spaces>name" --)
;
; Appends the *compilation* semantics of name to the current definition

        +CREATE "postpone", F_IMMEDIATE | F_COMPILE_ONLY
W_POSTPONE
        !word DO_COLON
        !word W_PARSE_NAME

        !word W_FIND_NAME
        !word W_DUP
        !word W_ZEQUAL
        +LITERAL E_INVALID_POSTPONE
        !word W_AND
        !word W_THROW

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
; R>
; (???)

        +CREATE "r>", 0
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

        +CREATE "r@", 0
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

; TODO native implementation?

        +CREATE "rot", 0
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

        +CREATE "rshift", 0
W_RSHIFT
        !word *+2
        lda 0,x
        ; TODO check limits on u, throw if bad?
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

        +CREATE "source", 0
W_SOURCE
        !word DO_COLON
        +LITERAL &INPUT_BUFFER
        !word W_AT
        +LITERAL &INPUT_LEN
        !word W_AT
        !word W_PSEMI

; ****************************************************************************
; STATE
; (-- a-addr)

        +CREATE "state", 0
W_STATE
        !word DO_CONSTANT
        !word &STATE

; ****************************************************************************
; SWAP
; (x_1 x_2 -- x_2 x_1)

        +CREATE "swap", 0
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

; TODO can we use primm? the problem is it uses a null terminated string

        +CREATE "type", 0
W_TYPE
        !word DO_COLON
!if 0 {
        +CLITERAL '>'
        !word W_EMIT
}
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
++      
!if 0 {
        +CLITERAL '<'
        !word W_EMIT
}
        !word W_PSEMI

; ****************************************************************************
; U<
; (u_1 u_2 -- flag)

        +CREATE "u<", 0
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

        +CREATE "um*", 0
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

        +CREATE "um/mod", 0
W_UMMOD
        !word *+2
        ; TODO check for division by zero
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

        +CREATE "unloop", 0
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

        +CREATE "variable", 0
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

        +CREATE "xor", 0
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

        +CREATE "[", F_IMMEDIATE
W_LBRACKET
        !word DO_COLON
        !word W_ZERO
        !word W_STATE
        !word W_STORE
        !word W_PSEMI

; ****************************************************************************
; ]
; (--)

        +CREATE "]", 0
W_RBRACKET
        !word DO_COLON
        +CLITERAL $C0 ; TODO ??????????????
        !word W_STATE
        !word W_STORE
        !word W_PSEMI
