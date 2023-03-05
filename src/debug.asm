
; TODO we could make . and .s deferred and move these simple implementations to high mem
; (then drop them after bootstrap)

; !if DEBUG {
        +NONAME
W_SIMPLE_DOT
        !word *+2
        ; stx <XSAVE
        lda 1,x
        jsr put_hex
        ; ldx <XSAVE
        lda 0,x
        jsr put_hex
        ; ldx <XSAVE
        jmp POP
; }

; !if DEBUG {
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
; }

; Dump return stack
; TODO can be done in forth using rp@
!if DEBUG {
        +NONAME
W_RDUMP
        !word *+2
        jsr RDUMP
        jmp NEXT

RDUMP
        stx <XSAVE
        lda #'r'
        jsr EMIT
        tsx
        lda #' '
        jsr EMIT
        lda $10a,x
        jsr put_hex
        lda $109,x
        jsr put_hex
        lda #' '
        jsr EMIT
        lda $108,x
        jsr put_hex
        lda $107,x
        jsr put_hex
        lda #' '
        jsr EMIT
        lda $106,x
        jsr put_hex
        lda $105,x
        jsr put_hex
        lda #' '
        jsr EMIT
        lda $104,x
        jsr put_hex
        lda $103,x
        jsr put_hex
        lda #' '
        jsr EMIT
        lda $102,x
        jsr put_hex
        lda $101,x
        jsr put_hex
        ; lda #' '
        ; jsr EMIT
        ; lda $100,x
        ; jsr put_hex
        ; lda $0FF,x
        ; jsr put_hex        
        jsr CR
        ldx <XSAVE
        rts
}
