
; TODO remove this stuff

; TODO looks like the kernel has an equivalent?

put_hex
        ; A: value to output
        ; TODO X,Y,Z: preserved
        pha
        lsr
        lsr
        lsr
        lsr
        jsr put_hex_digit
        pla
        ; jmp put_hex_digit

put_hex_digit
        ; A: digit to output
        ; TODO X,Y,Z: preserved
        and #$0f
        cmp #10
        bmi +
        adc #('a'-2-'9') ; cmp will have set C so -2 instead of -1
+       adc #'0'
        jmp EMIT

put_string
        ; TODO
        ; STRING points to string to print
        phx
        ldy #0
        lda (<STRING),y
+       taz

        beq +

-       iny
        lda (<STRING),y
        phy
        jsr EMIT
        ply
        dez
        bne -

+       plx
        rts                
