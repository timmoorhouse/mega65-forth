
; ****************************************************************************
; FACILITY

; ****************************************************************************
; AT-XY
; (u_1 u_2 --)
; ANSI 10.6.1.0742

!if ENABLE_FACILITY {
        +WORD "at-xy"
W_AT_XY
        !word *+2
        lda 0,x
        jsr goto_x
        lda 2,x
        jsr goto_y
        jmp NEXT
}

; ****************************************************************************
; KEY?
; (-- flag)
; ANSI 10.6.1.1755

!if ENABLE_FACILITY {
}

; ****************************************************************************
; PAGE
; (--)
; ANSI 10.6.1.2005

!if ENABLE_FACILITY {
        +WORD "page"
W_PAGE
        !word *+2
        jsr clear_screen
        lda #0
        jsr goto_x
        lda #0
        jsr goto_y
        jmp NEXT
}
