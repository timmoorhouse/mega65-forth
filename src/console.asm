
; TODO remove this stuff


petscii_to_screencode
        ; TODO this is a bit of a mess
        cmp #128
        bpl _128

        cmp #64
        bpl _64

        cmp #32
        bpl _32

        ; 0-31          +128
        clc
        adc #128
        rts

_32
        ; 32-63         +0
        rts

_64

        cmp #96
        bpl _96

        ; 64-96         -64
        sec
        sbc #64
        rts

_96
        ; 96-127        -32
        sec
        sbc #32
        rts

_128

        cmp #192
        bpl _192

        cmp #160
        bpl _160

        ; 128-159       +64
        clc
        adc #64
        rts

_160
        ; 160-191       -64
        sec
        sbc #64
        rts

_192        
        cmp #255
        beq _255
        ; 192-223       -128
        ; 224-254       -128
        sec
        sbc #128
        rts

_255
        ; 255           to 94?
        lda #94
        rts

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


ascii_to_petscii
        ; TODO this is a bit of a mess - should we just read scan codes and then a look up table?
        ; TODO
        cmp #$61
        bpl _61

        rts

_61
        cmp #$7b
        bpl _7b

        ; a..z
        sec
        sbc #$20
        rts

_7b
        rts        

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
