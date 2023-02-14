
; TODO
; as a starting point:
; - take a pass over the BASIC keywords and make equivalents? (will want DIR, etc)
; - look at C64 forth implementations (eg superFORTH)


; ****************************************************************************
; MON

;      MON
;               Exit to the system monitor, leaving a re-entry to Forth, 
;               if possible.

!if ENABLE_MEGA65 {
        +WORD "mon"
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
