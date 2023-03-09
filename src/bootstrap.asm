
;
; Things only needed during bootstrapping
;
; Everything in this file must be unnecessary once boostrapping ends
; or bad things will happen
;

; TODO move rdump from debug here, add a nicer forth implementation

; TODO can we move put_string here?

W_SIMPLE_DOT
        !word *+2
        lda #'$'
        jsr EMIT
        lda 1,x
        jsr put_hex
        lda 0,x
        jsr put_hex
        jmp POP

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


        +NONAME
W_AUTOBOOT_BOOTSTRAP
        !word DO_COLON

        +DOTQ "Starting bootstrap stage 1..."
        !word W_CR

        !word W_BOOTSTRAP_MIN

        !word W_CR
        +DOTQ "Starting bootstrap stage 2..."
        !word W_CR
        +CSLITERAL "bootstrap2.f"
        !word W_COUNT
        +CSLITERAL "included"
        !word W_COUNT
        !word W_EVALUATE

        !word W_PSEMI

        +NONAME
W_BOOTSTRAP_MIN
        !word DO_COLON

        +LITERAL BOOTSTRAP_MIN_START
        ; ( c-addr )

        !word W_TOR

        ; (R: c-addr)

_bootstrap_min_loop

        ; (R: c-addr)

        !word W_RAT
        !word W_ZERO
        ; ( c-addr u ) (R: c-addr)

_bootstrap_min_extend_line

        ; TODO this won't evaluate the last line
        !word W_2DUP
        !word W_PLUS
        +LITERAL BOOTSTRAP_MIN_END
        !word W_LESS
        +ZBRANCH _bootstrap_min_done

        ; TODO extend to newline
        !word W_2DUP
        !word W_PLUS
        !word W_CAT
        +CLITERAL $0a   ; ASCII LF
        !word W_NOTEQUAL
        +ZBRANCH _bootstrap_min_found_linefeed

        !word W_1PLUS
        +BRANCH _bootstrap_min_extend_line

_bootstrap_min_found_linefeed

!if 0 {
        !word W_DOTS
        !word W_2DUP
        !word W_TYPE
        !word W_CR
}

        !word W_DUP
        !word W_TOR

        ; (c-addr u) (R: c-addr u)
        !word W_EVALUATE

!if 0 {
        +DOTQ "evaluate done"
        !word W_CR
}

        !word W_2RFROM
        !word W_PLUS
        !word W_1PLUS ; eat the linefeed

        !word W_TOR
!if 0 {
        !word W_DOTS,W_CR
}

        +BRANCH _bootstrap_min_loop

_bootstrap_min_done

        !word W_RFROM,W_DROP

        !word W_2DROP

        !word W_PSEMI

BOOTSTRAP_MIN_START
!binary "bootstrap1.f"
!byte $0a ; an extra linefeed since we don't handle the last line yet TODO remove
BOOTSTRAP_MIN_LEN = *-BOOTSTRAP_MIN_START
BOOTSTRAP_MIN_END
