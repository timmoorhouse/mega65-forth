
; Dump return stack
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
