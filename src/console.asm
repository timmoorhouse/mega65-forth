
SCREEN_RAM = $0800  ; TODO get from register
COLOUR_RAM = $1f800 ; TODO get from register
COLOUR_RAM_32 = $0ff80000
; TODO has moved to $0FF80800 ???

COLOUR_USER  = $42; 42
COLOUR_DEBUG = $40

console_init
        lda #COLOUR_USER
        sta <COLOUR
        lda #0
        sta <SCREEN_X
        sta <SCREEN_Y
        jsr _recalc_screen_line
        jsr clear_screen
        jsr flush_keyboard
!ifdef DEBUG {
        ; lda <SCREEN_LINE+3
        ; jsr put_hex
        ; lda <SCREEN_LINE+2
        ; jsr put_hex
        ; lda <SCREEN_LINE+1
        ; jsr put_hex
        ; lda <SCREEN_LINE
        ; jsr put_hex
}
        rts


goto_x
        sta <SCREEN_X
        rts


goto_y
        sta <SCREEN_Y
        jsr _recalc_screen_line
        rts


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

        ; TODO EMIT
put_char
        cmp #$0d ; return
        bne +
        pha
        jmp CR
        pla
+        
        ; TODO delete
        ; TODO home

        jsr petscii_to_screencode
        jmp put_char_screencode

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
!convtab scr {
        adc #('a'-2-'9') ; cmp will have set C so -2 instead of -1
+       adc #'0'
}
        ; jmp put_char_screencode

put_char_screencode
        ; A: character to output
        ; Y: trashed
        ; TODO X,Z: preserved

        ; TODO special keycodes (CR, etc)?

        ldy <SCREEN_X
        cpy CHRCOUNT
        bmi +

        pha
        jsr CR
        pla
        
!if 1 {
+       ldy <SCREEN_X
        ; TODO assumes bank 0
        sta (<SCREEN_LINE),y
} else {
+       ldz &SCREEN_X
        ; sta (<SCREEN_LINE),y
        sta [<SCREEN_LINE],z
}
        ; TODO
!if 0 {
        ldz &SCREEN_X
        lda <COLOUR
        sta [<COLOUR_LINE],z
}

        ; TODO color ???
        inc <SCREEN_X
        rts


CR
        ldy #0
        sty <SCREEN_X
        ldy <SCREEN_Y
        cpy DISPROWS
        bmi +
        jmp _scroll_up
+       inc <SCREEN_Y
        ; pha
        clc
        lda <SCREEN_LINE
        adc LINESTEPLSB
        sta <SCREEN_LINE
        lda <SCREEN_LINE+1
        adc LINESTEPMSB
        sta <SCREEN_LINE+1
        clc
        lda <COLOUR_LINE
        adc LINESTEPLSB
        sta <COLOUR_LINE
        lda <COLOUR_LINE+1
        adc LINESTEPMSB
        sta <COLOUR_LINE+1
        ; pla
        rts

_move_down
        ; trashes A, preserves others
        inc <SCREEN_Y
        lda <SCREEN_Y
        cmp DISPROWS
        bmi +
        lda DISPROWS
        sta <SCREEN_Y
        jmp _scroll_up
+       rts


_scroll_up
        ; TODO this doesn't look to be working correctly
        +dma_run +
        ; TODO scroll colour ram
        +dma_run ++
        ; TODO clear colour ram of last row
        rts
        ; TODO use regs?
        ; Move lines [1,24] to [0,23]
+       +dma_options $00, $00
        +dma_options_end
        +dma_job_copy SCREEN_RAM+80, SCREEN_RAM, 2000-80, false, false
++      +dma_options $00, $00
        +dma_options_end
        +dma_job_fill $20, SCREEN_RAM+24*80, 80, false ; TODO use SCRNPTR
;+++      +dma_options $00, $00
;        +dma_options_end
;        +dma_job_fill $42, COLOUR_RAM, 2000, false ; TODO reg

_recalc_screen_line
        ; TODO this assumes bank 0
        ; TODO just use a lookup table?  definitely faster, may even be smaller

        ; recalculate SCREEN_LINE (pointer to start of screen mem for SCREEN_Y)
        ; TODO - can we arrange to only do this when needed?
        ; ignore the top 16 bits since they won't affect the lower 16-bits
        ; of the output
        ldy LINESTEPLSB
        sty MULTINB
        ldy LINESTEPMSB
        sty MULTINB+1
        ldy <SCREEN_Y
        sty MULTINA
        ldy #0
        sty MULTINA+1
        tay
        clc ; ????
        lda MULTOUT
        adc SCRNPTRLSB
        sta <SCREEN_LINE
        lda MULTOUT+1
        adc SCRNPTRMSB
        sta <SCREEN_LINE+1
        lda MULTOUT+2
        adc SCRNPTRBNK
        sta <SCREEN_LINE+2
        lda SCRNPTRMB
        and #$0f
        sta <SCREEN_LINE+3

!if 1 {       
        ; TODO do the same for colour ram
        clc
        lda MULTOUT
        adc <COLOUR_RAM_32 ; COLPTRLSB
        sta <COLOUR_LINE
        lda MULTOUT+1
        adc >COLOUR_RAM_32 ; COLPTRMSB
        sta <COLOUR_LINE+1
        lda MULTOUT+2
        adc ^COLOUR_RAM_32 ; #$01 ; bank ????
        sta <COLOUR_LINE+2
        lda ^(COLOUR_RAM_32 >> 8) ; mb ???
        ; and #$0f
        sta <COLOUR_LINE+3
}

        tya
        rts


clear_screen
        +dma_run +
        ; TODO clear colour ram
        +dma_run++
        rts
        ; TODO use registers
+       +dma_options $00, $00
        +dma_options_end
        +dma_job_fill $20, SCREEN_RAM, 2000, false ; TODO use SCRNPTR
++      +dma_options $00, $00
        +dma_options_end
        +dma_job_fill $42, COLOUR_RAM, 2000, false ; TODO reg

flush_keyboard
        ldy #0
-       lda ASCIIKEY
        sty ASCIIKEY
        bne -
        rts


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

get_char
        ; TO DO:
        ; - make sure MEGA65 I/O context is activated ??? (see F-12)
        ;   - think this is being done at the start of COLD
        ; - disable interrupts (sei) to prevent kernal from reading keys
        ; - read $d610
        ; - if 0, nothing has been pressed
        ; - if non-0, it's the most recent key press in ascii
        ;   - reading $d611 gives flags for different modifier keys
        ;   - need to write to $d610 to let it advance to the next key
        ;
-       lda ASCIIKEY
        ; brk
        beq -
        ; modifiers in $d611 (0=rshift,1=lshift,2=ctrl,3=mega,4=alt,5=noscroll,6=capslock,7=reserved)
        ; lda PETSCIIKEY
        ldy #0
        sty ASCIIKEY
        ; lda #40
        ; TODO this is a bit of a mess - should we just read scan codes and then a look up table?
        jsr ascii_to_petscii
        ; TODO if conversion fails, loop?
        beq -
        rts


put_char_xy
        ; TODO
        rts        

      
put_string
        ; TODO
        ; STRING points to string to print
        phx
        ldy #0
        lda (<STRING),y
        and #$1f ; TODO just for the case of printing names where we need to mask off control bits! - split into 7 and 8 bit versions?
        taz

        beq +

-       iny
        lda (<STRING),y
        phy
        jsr put_char
        ply
        dez
        bne -

+       plx
        rts                
