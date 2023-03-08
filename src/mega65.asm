
; TODO
; as a starting point:
; - take a pass over the BASIC keywords and make equivalents? (will want DIR, etc)
; - look at C64 forth implementations (eg superFORTH)

; TODO dir
; TODO type

!ifdef ENABLE_MEGA65 {
        +WORD "background", 0
} else {
        +NONAME
}
W_BACKGROUND
        !word *+2       ; (c --)
        lda 0,x
        sta $d021
        jmp POP

!ifdef ENABLE_MEGA65 {
        +WORD "border", 0
} else {
        +NONAME
}
W_BORDER                ; (c --)
        !word *+2
        lda 0,x
        sta $d020
        jmp POP

!ifdef ENABLE_MEGA65 {
        +WORD "foreground", 0
} else {
        +NONAME
}
W_FOREGROUND            ; (c --)
        !word *+2
        lda 0,x
        jsr FOREGROUND
        jmp POP

FOREGROUND
        sta $00f1
        rts

!ifdef ENABLE_MEGA65 {
        +WORD "theme", 0
} else {
        +NONAME
}
W_THEME                 ; (u --)
        !word DO_DEFER
        !word W_DROP

; ****************************************************************************
; MON

;      MON
;               Exit to the system monitor, leaving a re-entry to Forth, 
;               if possible.

!if ENABLE_MEGA65 {
        +WORD "mon", 0
W_MON
        !word *+2
        jsr MON
        jmp NEXT

MON
        ; TODO exiting the monitor will return to basic
        ; set monexit (ffa2) vector first?
        +KERNEL_CALL $ff56
        rts
}
