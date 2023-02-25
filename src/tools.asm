
; ****************************************************************************
; TOOLS

; ****************************************************************************
; .S
; (--)
; ANSI 15.6.0220

; See also tools.f

        +WORD ".s"
W_DOTS
        !word DO_DEFER
        !word W_SIMPLE_DOTS

!if ENABLE_TOOLS {
        +NONAME
W_SIMPLE_DOTS
        !word *+2
        jsr SIMPLE_DOTS
        jmp NEXT

SIMPLE_DOTS
        stx <XSAVE
        lda #' '
        jsr EMIT
        lda #'<'
        jsr EMIT
        lda #TOS
        sec
        sbc XSAVE
        lsr
        jsr put_hex
        lda #'>'
        jsr EMIT

        lda #TOS
        sta <TEMP1

-       lda <TEMP1
        cmp <XSAVE
        beq +

        lda #' '
        jsr EMIT
        dec <TEMP1
        dec <TEMP1
        ldy <TEMP1
        lda base_page+1,y
        jsr put_hex
        ldy <TEMP1
        lda base_page,y
        jsr put_hex
        jmp -
+        
        rts
}

; ****************************************************************************
; ?
; (a-addr --)
; ANSI 15.6.1.0600

; FIG:
;      ?             addr  ---                               L0
;               Print the value contained at the address in free format 
;               according to the current base.
;
;;
;;                                       ?
;;                                       SCREEN 76 LINE 11

; TODO always enable?

!if ENABLE_TOOLS {
!if 0 {
        +WORD "?"
W_QUES
        !word DO_COLON
;          !word AT
;          !word DOT
        !word W_PSEMI
}
}

; ****************************************************************************
; DUMP
; (addr u --)
; ANSI 15.6.1.1280

!if ENABLE_TOOLS {
!if 0 {
        +WORD "dump"
W_DUMP
        !word *+2
        rts
}
}

; ****************************************************************************
; SEE
; ("text" --)
; ANSI 15.6.1.2194

; FIG
;      DUMP          addr  n  ---                            L0
;               Print the contents of n memory locations beginning at 
;               addr.  Both addresses and contents are shown in the 
;               current numeric base.

!if ENABLE_TOOLS {
!if 0 {
        +WORD "see"
W_SEE
        !word *+2
        rts
}
}

;
; TODO
; - lookup name
; - check code field
;   - DO_VARIABLE
;   - DO_CONSTANT
;   - DO_COLON
;     - print ': <name>'
;     - scan a word at a time, resolving word name until we hit (;)
;     - will need special handling for words like LITERAL, DO, BRANCH, 0BRANCH
;     - might not be able to transform control structures back to IF, etc (that should be ok?)
;     - will need to change builtins to only have a single (;)
;     - limit to maximum length in case we get out of sync
;     - show address of everything and hex dump (like acme listing)
;   - DO_DOES
;     - might not be able to do anything sensible for this case?
;
;
;

; ****************************************************************************
; WORDS
; (--)
; ANSI 15.6.1.2465

!if 1 {
!if ENABLE_TOOLS {
        +WORD "words"
W_WORDS
        !word DO_COLON
        !word W_CR
        +LITERAL W_PRINT_NAME
        !word W_FORTH_WORDLIST
        !word W_TRAVERSE_WORDLIST
        !word W_PSEMI

; TODO this is pretty much id.
W_PRINT_NAME    ; (nt -- u)
        !word DO_COLON

        !word W_OUT
        !word W_CAT
        !word W_DUP
        +ZBRANCH +
        !word W_SPACE
+       !word W_CSLL
        !word W_SWAP
        !word W_LESS
        +ZBRANCH +
        !word W_CR
+       !word W_NAME_TO_STRING
        !word W_TYPE
        !word W_TRUE

_print_name_end
        !word W_PSEMI

; FIG
;      ID.           addr ---
;               Print a definition's name from its name field address.

!if 0 {
        +WORD "id."
W_IDDOT
        !word DO_COLON
;          !word PAD
;          !word CLIT
;          !byte $20
;          !word CLIT
;          !byte $5F
;          !word FILL
;          !word DUP
;          !word PFA
;          !word LFA
;          !word OVER
;          !word SUB
;          !word PAD
;          !word SWAP
;          !word CMOVE
;          !word PAD
;          !word COUNT
;          !word CLIT
;          !byte $1F
;          !word ANDD
;          !word TYPE
;          !word SPACE
        !word W_PSEMI
}

}
}
