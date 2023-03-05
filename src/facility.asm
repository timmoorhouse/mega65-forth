
; ****************************************************************************
; FACILITY

; ****************************************************************************
; AT-XY
; (u_1 u_2 --)
; ANSI 10.6.1.0742

; TODO use plot (kernel)

!if ENABLE_FACILITY {
        +WORD "at-xy", 0
W_AT_XY
        !word *+2
!if 0 {
        lda 0,x
        jsr goto_x
        lda 2,x
        jsr goto_y
}
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
        +WORD "page", 0
W_PAGE
        !word *+2
        jsr PAGE
        jmp NEXT
}

PAGE
        lda #147
        +KERNEL_CALL $ffd2 ; TODO use BASOUT (kernel)
        rts
