
;
; A collection of stuff for making kernel calls
;
;

; TODO zero page symbols
; TODO pntr = $00ec (used by OUT in fig)
; TODO interrupt vectors

; C64 jump table https://sta.c64.org/cbm64krnfunc.html
; C128 jump table
; MEGA65 jump table https://mega65.atlassian.net/wiki/spaces/MEGA65/pages/6619137/Kernel+Jump+Table

!macro KERNEL_PRE {
        stx <XSAVE
        pha
        lda #0
        tab
        pla
}
!macro KERNEL_POST {
        pha
        lda #>base_page
        tab
        pla
        ldx <XSAVE
}
!macro KERNEL_CALL .tgt {
        +KERNEL_PRE
        jsr .tgt
        +KERNEL_POST
}

; TODO naming convention for this stuff?

; TODO ff4d spin_spout
; TODO ff50 close_all
; TODO ff53 c64_mode
; TODO ff56 monitor_call
; TODO ff59 bootsys
; TODO ff5c phoenix
; TODO ff5f lkupla
; TODO ff62 lkupsa
; TODO ff65 swapper
; TODO ff68 pfkey

; ****************************************************************************
; $FF6B
; SETBANK
; A = bank for file data, X = bank for filename

W_SETBANK       ; (data filename --)
        !word *+2
        stx <TEMP1
        lda 2,x
        pha
        lda 0,x
        tax
        pla
        jsr SETBANK
        ldx <TEMP1
        jmp POPTWO

SETBANK
        +KERNEL_CALL $ff6b
        rts

; ****************************************************************************

; TODO ff6e jsr_far
; TODO ff71 jmp_far
; TODO ff74 lda_far
; TODO ff77 sta_far
; TODO ff7a cmp_far
; TODO ff7d primm
; ff80 - used for version on C128?
; TODO ff81 cint
; TODO ff84 ioinit
; TODO ff87 ramtas
; TODO ff8a restor
; TODO ff8d vector
; TODO ff90 setmsg
; TODO ff93 second
; TODO ff96 talksa
; TODO ff99 memtop
; TODO ff9c membot
; TODO ff9f key
; TODO ffa2 monexit
; TODO ffa5 acptr
; TODO ffa8 ciout
; TODO ffab untalk
; TODO ffae unlisten
; TODO ffb1 listen
; TODO ffb4 talk

; ****************************************************************************
; $FFB7	
; READST. Fetch status of current input/output device, value of ST variable. (For RS232, status is cleared.)
; Input: –
; Output: A = Device status.
; Used registers: A.
; Real address: $FE07.

; $01 timeout during write
; $02 timeout during read
; $04 short block
; $08 long block
; $10 mismatch during verify
; $20 checksum mismatch for block
; $40 end of file
; $80 device not present

W_READSS        ; (-- ior)
        !word *+2
        jsr READSS
        pha
        lda #0
        jmp PUSH

READSS
        +KERNEL_CALL $ffb7
        rts

; ****************************************************************************
; $FFBA	
; SETLFS. Set file parameters.
; Input: A = Logical number; X = Device number; Y = Secondary address.
; Output: –
; Used registers: –
; Real address: $FE00.

W_SETLFS        ; (logical device secondary --)
        !word *+2
        stx <TEMP1
        lda 4,x
        pha
        ldy 0,x
        lda 2,x
        tax
        pla
        jsr SETLFS
        ldx <TEMP1
        inx
        inx
        jmp POPTWO


SETLFS
        +KERNEL_CALL $ffba
        rts

; ****************************************************************************
; $FFBD	
; SETNAM. Set file name parameters.
; Input: A = File name length; X/Y = Pointer to file name.
; Output: –
; Used registers: –
; Real address: $FDF9.

W_SETNAM        ; (c-addr u --)
        !word *+2
        stx <TEMP1
        lda 0,x
        pha
        ldy 3,x ; MSB in y
        lda 2,x
        tax     ; LSB in x
        pla
        jsr SETNAM
        ldx <TEMP1
        jmp POPTWO

SETNAM
        +KERNEL_CALL $ffbd
        rts

; ****************************************************************************
; $FFC0	
; OPEN. Open file. (Must call SETLFS and SETNAM beforehands.)
; Input: –
; Output: –
; Used registers: A, X, Y.
; Real address: ($031A), $F34A.

W_OPEN          ; (--)
        !word *+2
        jsr OPEN
        jmp NEXT

OPEN
        +KERNEL_CALL $ffc0
        rts

; ****************************************************************************

; TODO ffc3 close

; ****************************************************************************
; $FFC6	
; CHKIN. Define file as default input. (Must call OPEN beforehands.)
; Input: X = Logical number.
; Output: –
; Used registers: A, X.
; Real address: ($031E), $F20E.

W_CHKIN         ; (u --)
        !word *+2
        stx <TEMP1
        lda 0,x
        tax
        jsr CHKIN
        ldx <TEMP1
        jmp POP

CHKIN
        +KERNEL_CALL $ffc6
        rts

; ****************************************************************************

; TODO ffc9 chkout
; TODO ffcc clrch

; ****************************************************************************
; $FFCF	
; CHRIN. Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
; Input: –
; Output: A = Byte read.
; Used registers: A, Y.
; Real address: ($0324), $F157.

; TODO duplication with W_KEY
W_BASIN         ; (-- c)
        !word *+2
        jsr BASIN
        pha
        lda #0
        jmp PUSH

BASIN
        +KERNEL_CALL $ffcf
        rts

; ****************************************************************************
; $FFD2	
; CHROUT. Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
; Input: A = Byte to write.
; Output: –
; Used registers: –
; Real address: ($0326), $F1CA.

!if 0 { ; skipping this ... it's the same as W_EMIT
W_BASOUT       ; (c --)
        !word *+2
        lda 0,x
        jsr CHROUT
        jmp POP
}

EMIT
BASOUT
        +KERNEL_CALL $ffd2
        rts

; ****************************************************************************

; TODO ffd5 load ; loadsp?

        ; $FFD5	
        ; LOAD. Load or verify file. (Must call SETLFS and SETNAM beforehands.)
        ; Input: A: 0 = Load, 1-255 = Verify; X/Y = Load address (if secondary address = 0).
        ; Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1); X/Y = Address of last byte loaded/verified (if Carry = 0).
        ; Used registers: A, X, Y.
        ; Real address: $F49E.

; TODO ffd8 save ; savesp?
; TODO ffdb set_time
; TODO ffde read_time
; TODO ffe1 stop
; TODO ffe4 getin
; TODO ffe7 clall
; TODO ffea scanstopkey
; TODO ffed screen_org
; TODO fff0 plot
; TODO fff3 iobase
