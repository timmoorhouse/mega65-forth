
; ****************************************************************************
; FILE

FILE_BUFFER_SIZE = 128 ; should be long enough for a line
MAX_OPEN_FILES   = 10  ; kernel limit

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
; BIN
; (fam_1 -- fam_2)
; ANSI 11.6.1.0765

!if ENABLE_FILE {
        +WORD "bin", 0
W_BIN
        !word DO_COLON
        +LITERAL FAM_BIN
        !word W_OR
        !word W_PSEMI
}

; ****************************************************************************
; CLOSE-FILE
; (fileid -- ior)
; ANSI 11.6.1.0900

!if ENABLE_FILE {
        +WORD "close-file", 0
W_CLOSE_FILE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<close-file>"
        !word W_SIMPLE_DOTS
}
        !word W_CLOSE
        !word W_READSS
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; CREATE-FILE
; (c-addr u fam -- fileid ior)
; ANSI 11.6.1.1010

!if ENABLE_FILE {
        +WORD "create-file", 0
W_CREATE_FILE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<create-file-file>"
        !word W_SIMPLE_DOTS
}        
        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; DELETE-FILE
; (c-addr u -- ior)
; ANSI 11.6.1.1190

!if ENABLE_FILE {
        +WORD "delete-file", 0
W_DELETE_FILE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<create-file-file>"
        !word W_SIMPLE_DOTS
}        
        ; TODO
        !word W_2DROP
        !word W_ZERO    
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}    
        !word W_PSEMI
}

; ****************************************************************************
; FILE-POSITION
; (fileid -- ud ior)
; ANSI 11.6.1.1520

!if ENABLE_FILE {
        +WORD "file-position", 0
W_FILE_POSITION
        !word DO_COLON
!if DEBUG {
        +DOTQ "<create-file-file>"
        !word W_SIMPLE_DOTS
}
        ; TODO
        !word W_DROP
        !word W_ZERO          
        !word W_ZERO          
        !word W_ZERO   
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; FILE-SIZE
; (fileid -- ud ior)
; ANSI 11.6.1.1522

!if ENABLE_FILE {
        +WORD "file-size", 0
W_FILE_SIZE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<file-size>"
        !word W_SIMPLE_DOTS
}        
        ; TODO
        !word W_DROP
        !word W_ZERO          
        !word W_ZERO          
        !word W_ZERO 
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; INCLUDE-FILE
; (i*x fileid -- j*x)
; ANSI 11.6.1.1717

; TODO track line number for error reporting?

!if ENABLE_FILE {
        +WORD "include-file", 0
W_INCLUDE_FILE
        !word DO_COLON

        !word W_SAVE_INPUT
        !word W_NTOR

        +LITERAL &SOURCE_ID
        !word W_STORE

-       !word W_REFILL
        +ZBRANCH +

        +LITERAL W_PEVALUATE       ; TODO this should be wrapped in a catch
        !word W_CATCH

        !word W_QDUP
        +ZBRANCH -
        
        +BRANCH ++

+       !word W_ZERO

++

        !word W_NRFROM
        !word W_RESTORE_INPUT
        !word W_DROP            ; TODO check status from restore

        !word W_THROW           ; Propagate an exception if there was one

        !word W_PSEMI
}

; ****************************************************************************
; INCLUDED
; (i*x c-addr u -- j*x)
; ANSI 11.6.1718

!if ENABLE_FILE {
        +WORD "included", 0
W_INCLUDED
        !word DO_COLON
        !word W_RSLO
        !word W_OPEN_FILE
        +ZBRANCH +
        +LITERAL E_NONEXISTENT_FILE ; TODO what error? need to figure out what status byte looks like
        !word W_THROW
+
        !word W_TOR             ; need to move to return stack since include-file can do arbitrary things to the data stack
        !word W_RAT
        !word W_INCLUDE_FILE
        !word W_RFROM
        !word W_CLOSE_FILE
        !word W_DROP            ; TODO check status
        !word W_PSEMI
}

; ****************************************************************************
; OPEN-FILE
; (c-addr u fam -- fileid ior)
; ANSI 11.6.1.1970

!if ENABLE_FILE {
        +WORD "open-file", 0
W_OPEN_FILE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<open-file>"
        +CLITERAL '['
        !word W_EMIT
        !word W_TOR
        !word W_2DUP
        !word W_TYPE
        +CLITERAL ','
        !word W_EMIT
        !word W_RFROM
        !word W_DUP
        !word W_DOT
        +CLITERAL ']'
        !word W_EMIT
        !word W_SIMPLE_DOTS
}         
        !word W_DROP    ; TODO use fam?

        !word W_ZERO
        !word W_DUP
        !word W_SETBANK

        !word W_SETNAM

        ; TODO in BASIC for DOPEN#, channel numbers in [1,127] use CR, [128,255] use CR LF
        ; TODO DOPEN# has a ,W flag for write access

        !word W_UNUSED_LOGICAL
!if 0 {
        +DOTQ "logical="
        !word W_DUP
        !word W_DOT
}

        !word W_DUP
        +LITERAL 8 ; TODO how to select this?
        !word W_UNUSED_SECONDARY
!if 0 {
        +DOTQ "secondary="
        !word W_DUP
        !word W_DOT
}
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

!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}


; ****************************************************************************
; R/O
; (-- fam)
; ANSI 11.6.1.2054

!if ENABLE_FILE {
        +WORD "r/o", 0
W_RSLO
        !word DO_CONSTANT
        !word FAM_RO
}

; ****************************************************************************
; R/W
; (-- fam)
; ANSI 11.6.1.2056

!if ENABLE_FILE {
        +WORD "r/w", 0
W_RSLW
        !word DO_CONSTANT
        !word FAM_RW
}

; ****************************************************************************
; READ-FILE
; (c-addr u_1 fileid -- u_2 ior)
; ANSI 11.6.1.2080

!if ENABLE_FILE {
        +WORD "read-file", 0
W_READ_FILE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<read-file>"
        !word W_SIMPLE_DOTS
}           
        ; TODO       


        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; READ-LINE
; (c-addr u_1 fileid -- u_2 flag ior)
; ANSI 11.6.1.2090

; Stops at line ending
; Buffer should be at least u_1+2 characters long.
; A line ending is reached, u_2 does not include the line ending.
; If u_2 < u_1 the line ending has been reached.  If u_2 = u_1, the line ending has not been reached.

!if ENABLE_FILE {
        +WORD "read-line", 0
W_READ_LINE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<read-line>"
        !word W_SIMPLE_DOTS
}           
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

!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; REPOSITION-FILE
; (ud fileid -- ior)
; ANSI 11.6.1.2142

!if ENABLE_FILE {
        +WORD "reposition-file", 0
W_REPOSITION_FILE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<reposition-file>"
        !word W_SIMPLE_DOTS
}          
        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; RESIZE-FILE
; (ud fileid -- ior)
; ANSI 11.6.1.2147

!if ENABLE_FILE {
        +WORD "resize-file", 0
W_RESIZE_FILE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<resize-file>"
        !word W_SIMPLE_DOTS
}         
        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; W/O
; (-- fam)
; ANSI 11.6.1.2425

!if ENABLE_FILE {
        +WORD "w/o", 0
W_WSLO
        !word DO_CONSTANT
        !word FAM_WO
}

; ****************************************************************************
; WRITE-FILE
; (c-addr u fileid -- ior)
; ANSI 11.6.1.2480

!if ENABLE_FILE {
        +WORD "write-file", 0
W_WRITE_FILE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<write-file>"
        !word W_SIMPLE_DOTS
}       

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

!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}       
        !word W_PSEMI
}

; ****************************************************************************
; WRITE-LINE
; (c-addr u fileid -- ior)
; ANSI 11.6.1.2485

!if ENABLE_FILE {
        +WORD "write-line", 0
W_WRITE_LINE
        !word DO_COLON
!if DEBUG {
        +DOTQ "<write-line>"
        !word W_SIMPLE_DOTS
}       
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

!if DEBUG {
        !word W_SIMPLE_DOTS,W_CR
}
        !word W_PSEMI
}
