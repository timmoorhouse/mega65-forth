
; ****************************************************************************
; FILE

; TODO separate kernel stuff into separate file

FAM_BIN = 4
FAM_RO  = 1
FAM_WO  = 2
FAM_RW  = (FAM_RO|FAM_WO)

ldtnd = $0098
lat   = $035a
fat   = $0364
sat   = $036e

        +NONAME
W_UNUSED_LOGICAL
        !word *+2
        jsr _unused_logical
        pha
        lda #0
        jmp PUSH

; TODO use lkupla to check if la is in use

_unused_logical
!if 0 {
        lda #' '
        jsr EMIT
        lda ldtnd
        jsr put_hex
        ldy #0
-       cpy #10
        beq +
        lda #'-'
        jsr EMIT
        lda lat,y
        phy
        jsr put_hex
        ply
        iny
        bra -
+
        lda #' '
        jsr EMIT
}

        ; pick first unused >= 32
        lda #$1f
        ; check the next possibility ..
--      inc
        ; see if it's in use ...
        ldy #0
-       cpy ldtnd
        beq +
        cmp lat,y
        beq --
        iny
        bra -
        ; unused ... guaranteed to find one in the range [32,42]
+       rts

; TODO use lkupsa to check if sa is in use

        +NONAME
W_UNUSED_SECONDARY
        !word *+2
        jsr _unused_secondary
        pha
        lda #0
        jmp PUSH

_unused_secondary
!if 0 {
        lda #' '
        jsr EMIT
        lda ldtnd
        jsr put_hex
        ldy #0
-       cpy #10
        beq +
        lda #'-'
        jsr EMIT
        lda sat,y
        phy
        jsr put_hex
        ply
        iny
        bra -
+
        lda #' '
        jsr EMIT
}

        ; pick first unused >= 2
        lda #$61 ; these get stored as $60 | sa
        ; check the next possibility ..
--      inc
        ; see if it's in use ...
        ldy #0
-       cpy ldtnd
        beq +
        cmp sat,y
        beq --
        iny
        bra -
+       eor #$60
        ; unused ... guaranteed to find one in the range [2,12]
        rts

        +NONAME
W_BUFFER_OF_FILEID ; (fileid -- c-addr u)
        !word DO_COLON
        ; fileid will be in range [32,42)
        +LITERAL $0f
        !word W_AND             ; will be in range [0, 10)
        +LITERAL FILE_BUFFER_SIZE
        !word W_UMSTAR
        !word W_DROP
        +LITERAL DAREA
        !word W_PLUS
        +LITERAL FILE_BUFFER_SIZE
        !word W_PSEMI

; ****************************************************************************
; CLOSE-FILE
; (fileid -- ior)

!if ENABLE_FILE {
        +CREATE "close-file", 0
W_CLOSE_FILE
        !word DO_COLON

        !word W_CLOSE
        !word W_READSS

        !word W_PSEMI
}

; ****************************************************************************
; CREATE-FILE
; (c-addr u fam -- fileid ior)

!if 0 {
!if ENABLE_FILE {
        +CREATE "create-file", 0
W_CREATE_FILE
        !word DO_COLON

        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO

        !word W_PSEMI
}
}

; ****************************************************************************
; DELETE-FILE
; (c-addr u -- ior)

!if 0 {
!if ENABLE_FILE {
        +CREATE "delete-file", 0
W_DELETE_FILE
        !word DO_COLON

        ; TODO
        !word W_2DROP
        !word W_ZERO    

        !word W_PSEMI
}
}

; ****************************************************************************
; FILE-POSITION
; (fileid -- ud ior)

!if 0 {
!if ENABLE_FILE {
        +CREATE "file-position", 0
W_FILE_POSITION
        !word DO_COLON

        ; TODO
        !word W_DROP
        !word W_ZERO          
        !word W_ZERO          
        !word W_ZERO   

        !word W_PSEMI
}
}

; ****************************************************************************
; FILE-SIZE
; (fileid -- ud ior)

!if 0 {
!if ENABLE_FILE {
        +CREATE "file-size", 0
W_FILE_SIZE
        !word DO_COLON

        ; TODO
        !word W_DROP
        !word W_ZERO          
        !word W_ZERO          
        !word W_ZERO 

        !word W_PSEMI
}
}

; ****************************************************************************
; OPEN-FILE
; (c-addr u fam -- fileid ior)

!if ENABLE_FILE {
        +CREATE "open-file", 0
W_OPEN_FILE
        !word DO_COLON

        !word W_DROP    ; TODO use fam?

        !word W_ZERO
        !word W_DUP
        !word W_SETBANK

        !word W_SETNAM

        ; TODO in BASIC for DOPEN#, channel numbers in [1,127] use CR, [128,255] use CR LF
        ; TODO DOPEN# has a ,W flag for write access

        !word W_UNUSED_LOGICAL

        !word W_DUP
        +LITERAL 8 ; TODO how to select this?
        !word W_UNUSED_SECONDARY

        !word W_SETLFS

        !word W_OPEN 

        ; TODO if fam includes read, check that file exists

        ; max 10 channels open
        ; $98  ldtnd = # open channels
        ; $35a lat   = logical channel table
        ; $364 fat   = device number table
        ; $36e sat   = secondary table
        ; use lower nibble of logical channel as buffer index
        ; so save-input needs logical channel (0=keyboard) + position into buffer (>IN) - all fits in one word?

        !word W_READSS

        !word W_PSEMI
}

; ****************************************************************************
; READ-FILE
; (c-addr u_1 fileid -- u_2 ior)

!if ENABLE_FILE {
        +CREATE "read-file", 0
W_READ_FILE
        !word DO_COLON

        ; TODO       

        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO

        !word W_PSEMI
}

; ****************************************************************************
; READ-LINE
; (c-addr u_1 fileid -- u_2 flag ior)

; Stops at line ending
; Buffer should be at least u_1+2 characters long.
; A line ending is reached, u_2 does not include the line ending.
; If u_2 < u_1 the line ending has been reached.  If u_2 = u_1, the line ending has not been reached.

!if ENABLE_FILE {
        +CREATE "read-line", 0
W_READ_LINE
        !word DO_COLON

        ; TODO

        !word W_CHKIN

        ; Loop index is a pointer to the buffer entry
        !word W_OVER
        !word W_PLUS
        !word W_OVER
        +DO _read_line_after_loop

        !word W_DROP ; drop the original c-addr
        !word W_ZERO ; initial value for u2

_read_line_loop

        !word W_BASIN

        ; Check for return
        !word W_DUP
        +LITERAL 13 ; TODO should be CR???
        !word W_EQUAL
        +ZBRANCH _read_line_not_return

        !word W_ONE
        !word W_SOURCE_LINE
        !word W_PSTORE

        !word W_DROP ; drop the CR
        !word W_LEAVE
        !word _read_line_after_loop-*

_read_line_not_return
        ; A normal character, add it to the buffer        

        !word W_I
        !word W_CSTORE

        !word W_1PLUS
        !word W_PLOOP
        !word _read_line_loop-*
_read_line_after_loop

        !word W_TRUE

        ; TODO need to bump up file position counter?  or does the kernel track this?

        ; restore default input
        !word W_ZERO
        !word W_CHKIN

        !word W_READSS

        !word W_PSEMI
}

; ****************************************************************************
; REPOSITION-FILE
; (ud fileid -- ior)

!if 0 {
!if ENABLE_FILE {
        +CREATE "reposition-file", 0
W_REPOSITION_FILE
        !word DO_COLON

        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO

        !word W_PSEMI
}
}

; ****************************************************************************
; RESIZE-FILE
; (ud fileid -- ior)

!if 0 {
!if ENABLE_FILE {
        +CREATE "resize-file", 0
W_RESIZE_FILE
        !word DO_COLON

        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO

        !word W_PSEMI
}
}

; ****************************************************************************
; WRITE-FILE
; (c-addr u fileid -- ior)

!if ENABLE_FILE {
        +CREATE "write-file", 0
W_WRITE_FILE
        !word DO_COLON

        !word W_CHKOUT

        ; (c-addr u)

        ; Loop index is a pointer to the buffer entry
        !word W_OVER
        !word W_PLUS
        !word W_SWAP
        +DO _write_file_after_loop

_write_file_loop

        !word W_I
        !word W_CAT
        !word W_BASOUT

        !word W_PLOOP
        !word _write_file_loop-*
_write_file_after_loop

        ; restore default output
        !word W_ZERO
        !word W_CHKOUT

        !word W_READSS

        !word W_PSEMI
}

; ****************************************************************************
; WRITE-LINE
; (c-addr u fileid -- ior)

!if ENABLE_FILE {
        +CREATE "write-line", 0
W_WRITE_LINE
        !word DO_COLON

        ; TODO rewrite using WRITE-FILE

        !word W_CHKOUT

        ; (c-addr u)

        ; Loop index is a pointer to the buffer entry
        !word W_OVER
        !word W_PLUS
        !word W_SWAP
        +DO _write_line_after_loop

_write_line_loop

        !word W_I
        !word W_CAT
        !word W_BASOUT

        !word W_PLOOP
        !word _write_line_loop-*
_write_line_after_loop

        !word W_CR
        !word W_BASOUT

        ; restore default output
        !word W_ZERO
        !word W_CHKOUT

        !word W_READSS

        !word W_PSEMI
}
