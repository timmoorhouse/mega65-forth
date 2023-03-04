
; ****************************************************************************
; CORE EXT

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
        ora 0,x
        beq +
        dey
+       sty 0,x
        sty 1,x
        jmp NEXT
}

; ****************************************************************************
; 0>
; (n -- flag)
; ANSI 6.2.0280

!if ENABLE_CORE_EXT {
        +WORD "0>"
W_ZGREATER
        !word *+2
        ; ldy #0 ; TODO
        lda 1,x
        asl
        bcs +
        ora 0,x
        beq +
        dey
+       sty 1,x
        sty 0,x 
        jmp NEXT
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
!if PUSH_MSB_FIRST {        
        lda 3,x ; TODO PUSH_MSB_FIRST
        pha
        lda 2,x
        pha
        lda 1,x
        pha
        lda 0,x
        pha
} else {
        lda 2,x ; TODO PUSH_MSB_FIRST
        pha
        lda 3,x
        pha
        lda 0,x
        pha
        lda 1,x
        pha
}
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
!if PUSH_MSB_FIRST {        
        pla ; TODO PUSH_MSB_FIRST
        sta 0,x
        pla
        sta 1,x
        pla
        sta 2,x
        pla
        sta 3,x
} else {
        pla ; TODO PUSH_MSB_FIRST
        sta 1,x
        pla
        sta 0,x
        pla
        sta 3,x
        pla
        sta 2,x
}        
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
!if PUSH_MSB_FIRST {        
        lda $103,x ; TODO PUSH_MSB_FIRST
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
        lda $104,x ; TODO PUSH_MSB_FIRST
        ldx <XSAVE
        sta 0,x
        tsx
        lda $103,x
        ldx <XSAVE
        sta 1,x
        tsx
        lda $102,x
        pha
        lda $101,x
        ldx <XSAVE  

}       



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

; TODO could move this to core-ext.f if we have a way of setting latest, latextxt from forth

!if ENABLE_CORE_EXT {
        +WORD ":noname"
W_NONAME
        !word DO_COLON
        !word W_ALIGN
        !word W_HERE
        +LITERAL &LATEST_XT
        !word W_STORE
        !word W_RBRACKET
        !word W_ZERO
        +LITERAL &LATEST
        !word W_STORE
        !word W_HERE
        +LITERAL DO_COLON
        !word W_COMMA
        !word W_PSEMI
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
        ora 2,x 

        beq +
        dey
+       sty 2,x
        sty 3,x
        jmp POP
}

; ****************************************************************************
; ?DO
; (n1 n2 --)
; ANSI 6.2.0620

!if ENABLE_CORE_EXT {
        +WORD "(?do)"
W_PQDO
        !word *+2

        ; check if index = limit

        lda 0,x
        eor 2,x
        sta <TEMP1
        lda 1,x
        eor 3,x
        ora <TEMP1
        beq +
        jmp PDO
+       
        ; exit immediately
        lda (<I),y
        pha
        iny
        lda (<I),y
        sta <I+1
        pla
        sta <I
        jmp POP2
}

; ****************************************************************************
; DEFER
; Forth 2012 6.2.1173

; TODO move to core-ext.f once we have DOES> (see reference implementation)

!if ENABLE_CORE_EXT {
        +WORD "defer"
} else {
        +NONAME
}
W_DEFER
        !word DO_COLON
        !word W_CREATE
        !word W_ZERO
        !word W_COMMA
        !word W_PSCODE
DO_DEFER
        ; See also DO_COLON

        ; ldy #0 ; TODO

        ; TODO there's likely a faster way to do this ...

        clc
        lda <W
        adc #2
        sta <TEMP1
        lda <W+1
        adc #0
        sta <TEMP1+1

        ldy #0
        lda (<TEMP1),y
        sta <W
        iny
        lda (<TEMP1),y
        sta <W+1

        ldy #0
        jmp &DO_JUMP_W

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

        ; hold delimiter in TEMP1
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
        sta 1,x                 ; (addr)

        ; initialize len
        dex
        dex
        sty 1,x
        sty 0,x                 ; (addr len)

        ; push a temporary of the end of the buffer
        dex
        dex
        clc
        lda <INPUT_BUFFER
        adc <INPUT_LEN
        sta 0,x
        lda <INPUT_BUFFER+1
        adc <INPUT_LEN+1
        sta 1,x                 ; (addr len end)

        ; push a temporary current position
        dex
        dex
        lda 6,x
        sta 0,x
        lda 7,x
        sta 1,x                 ; (addr len end current)

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
        inw <IN

        bra _parse_loop

_parse_found_terminator

        ; and increment IN
        inw <IN
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
        !word DO_COLON
        !word W_BL
        !word W_PPARSE_NAME
        !word W_PSEMI

        +WORD "(parse-name)"
W_PPARSE_NAME ; (char "<chars>name<char>" -- c-addr u)
        !word *+2

        ; ldy #0 ; TODO

        ; copy delimiter into TEMP1
        lda 0,x
        sta <TEMP1

        ; initialize addr
        ;dex
        ;dex
        clc
        lda <INPUT_BUFFER
        adc <IN
        sta 0,x
        lda <INPUT_BUFFER+1
        adc <IN+1
        sta 1,x                 ; (addr)

        ; initialize len
        dex                     ; TODO stack check
        dex
        sty 1,x
        sty 0,x                 ; (addr len)

        ; push a temporary of the end of the buffer
        dex
        dex
        clc
        lda <INPUT_BUFFER
        adc <INPUT_LEN
        sta 0,x
        lda <INPUT_BUFFER+1
        adc <INPUT_LEN+1
        sta 1,x                 ; (addr len end)

        ; push a temporary current position
        dex
        dex
        lda 6,x
        sta 0,x
        lda 7,x
        sta 1,x                 ; (addr len end current)

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
        ; jsr isspace
        cmp <TEMP1
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
        inw <IN

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
        ; jsr isspace
        cmp <TEMP1
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
        inw <IN

        bra _parse_name_skip_nonwhitespace_loop

_parse_name_found_ending_whitespace

        ; increment IN
        inw <IN

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
        lda 2,x
        pha
        lda 3,x
        ldx <XSAVE
        jmp PUT

; ****************************************************************************
; REFILL
; (-- flag)
; ANSI 6.2.2125
; ANSI 7.6.2.2125
; ANSI 11.6.2.2125

!if ENABLE_CORE_EXT {
        +WORD "refill"
} else {
        +NONAME
}
W_REFILL
        !word DO_COLON

        !word W_SOURCE_ID

        ; TODO don't reset input buffer? (if we handle it in save/restore it shouldn't have changed)
        ; TODO we'd need to store the max len of the buffer

        !word W_QDUP
        +ZBRANCH _refill_tib

        !word W_ZLESS
        +ZBRANCH _refill_file

        ; SOURCE-ID < 0 ... its a string

        !word W_FALSE
        +BRANCH _refill_done

_refill_file

        !word W_SOURCE_ID
        !word W_BUFFER_OF_FILEID        ; (c-addr u)
        !word W_OVER
        !word W_SWAP                    ; (c-addr c-addr u)
        !word W_TWO
        !word W_SUB                     ; leave space for cr lf (spec requires this)
        !word W_SOURCE_ID
        !word W_READ_LINE
        ; (c-addr u flag ior)

        +ZBRANCH _refill_ior_ok
        ; ior bad ...
        !word W_DROP                    ; drop flag
_refill_flag_bad
        !word W_2DROP                   ; drop u2 and buffer address
        !word W_FALSE
        +BRANCH _refill_done

_refill_ior_ok
        +ZBRANCH _refill_flag_bad        
        ; everything ok ...
        ; (c-addr u)
        +BRANCH _refill_set_buffer

_refill_tib

        +LITERAL TIB
        !word W_DUP
        +LITERAL TIB_LEN
        !word W_ACCEPT

        ; (c-addr u)

_refill_set_buffer

        +LITERAL &INPUT_LEN
        !word W_STORE

        +LITERAL &INPUT_BUFFER
        !word W_STORE

        !word W_ZERO
        !word W_IN
        !word W_STORE

        !word W_TRUE

_refill_done

        ; (flag)

        !word W_PSEMI

; ****************************************************************************
; RESTORE-INPUT
; (x_n...x_1 n -- flag)
; ANSI 6.2.2148

; TODO always enable?

!if ENABLE_CORE_EXT {
        +WORD "restore-input"
} else {
        +NONAME
}
W_RESTORE_INPUT
        !word DO_COLON
        !word W_DROP            ; TODO check value
        +LITERAL &INPUT_LEN
        !word W_STORE
        +LITERAL &INPUT_BUFFER
        !word W_STORE
        !word W_IN
        !word W_STORE
        +LITERAL &SOURCE_ID
        !word W_STORE
        !word W_ZERO            ; input successfully restored
        !word W_PSEMI

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

        lda 2,x
        pha
        lda 3,x
        pha

-       lda 0,x
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

+       jmp POP

; ****************************************************************************
; S\"
; Forth 2012 6.2.2266
; Forth 2012 11.6.2.2266

; See http://www.forth200x.org/escaped-strings.html for a reference implementation

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
        +WORD "save-input"
} else {
        +NONAME
}
W_SAVE_INPUT
        !word DO_COLON
        ; TODO this could be made much smaller
        !word W_SOURCE_ID
        !word W_IN
        !word W_AT
        !word W_SOURCE
        +LITERAL 4             ; number of other cells pushed
        !word W_PSEMI

; ****************************************************************************
; SOURCE-ID
; (-- 0|-1)
; ANSI 6.2.2218
; (-- 0|-1|fileid)
; ANSI 11.6.1.2218

; TODO extension to allow fileid if FILE enabled

!if ENABLE_CORE_EXT {
        +WORD "source-id"
} else {
        +NONAME
}
W_SOURCE_ID
        !word *+2
        lda <SOURCE_ID
        pha
        lda <SOURCE_ID+1
        jmp PUSH

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
; UNUSED
; (-- u)
; ANSI 6.2.2395

!if ENABLE_CORE_EXT {
        +WORD "unused"
W_UNUSED
        !word DO_COLON
        +LITERAL DAREA
        !word W_HERE
        !word W_SUB
        !word W_PSEMI        
}

; ****************************************************************************
; VALUE
; (x "<spaces>name" --)
; ANSI 6.2.2405

; See also constant (core)

!if ENABLE_CORE_EXT {
        +WORD "value"
W_VALUE
        !word DO_COLON
        !word W_CREATE
        !word W_COMMA
        !word W_PSCODE
DO_VALUE
        ldy #2
        lda (<W),y
        pha
        iny
        lda (<W),y
        jmp PUSH
}
