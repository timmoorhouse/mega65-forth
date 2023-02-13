

; Dump return stack
W_RDUMP
        !word *+2
        stx <XSAVE
        lda #'r'
        jsr put_char
        tsx
        lda #' '
        jsr put_char
        lda $10a,x
        jsr put_hex
        lda $109,x
        jsr put_hex
        lda #' '
        jsr put_char
        lda $108,x
        jsr put_hex
        lda $107,x
        jsr put_hex
        lda #' '
        jsr put_char
        lda $106,x
        jsr put_hex
        lda $105,x
        jsr put_hex
        lda #' '
        jsr put_char
        lda $104,x
        jsr put_hex
        lda $103,x
        jsr put_hex
        lda #' '
        jsr put_char
        lda $102,x
        jsr put_hex
        lda $101,x
        jsr put_hex
        ; lda #' '
        ; jsr put_char
        ; lda $100,x
        ; jsr put_hex
        ; lda $0FF,x
        ; jsr put_hex        
        jsr CR
        ldx <XSAVE
        jmp NEXT
