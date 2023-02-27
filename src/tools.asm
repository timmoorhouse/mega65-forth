
; ****************************************************************************
; TOOLS

; ****************************************************************************
; .S
; (--)
; ANSI 15.6.0220

; See also tools.f

; TODO remove the defer once enough has been moved to forth that we don't need it for debugging?

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

; See tools.f

; ****************************************************************************
; DUMP
; (addr u --)
; ANSI 15.6.1.1280

; See tools.f

; ****************************************************************************
; SEE
; ("text" --)
; ANSI 15.6.1.2194

; See tools.f

; ****************************************************************************
; WORDS
; (--)
; ANSI 15.6.1.2465

; See tools.f
