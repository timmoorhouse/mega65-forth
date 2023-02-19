
; ****************************************************************************
; FILE

FILE_BUFFER_SIZE = 128 ; should be long enough for a line
MAX_OPEN_FILES   = 10  ; kernel limit

; TODO separate kernel stuff into separate file

W_FILE_TEST
        !word DO_COLON
        !word W_PDOTQ
        +STRING "<file-test>"
        !word W_CR

        +LITERAL _test_filename
        !word W_COUNT
        !word W_RSLO
        !word W_OPEN_FILE

        !word W_DROP ; TODO check status?

        !word W_TOR     ; (R: fileid)

!if 1 {
        !word W_RAT
        !word W_INCLUDE_FILE
}

        ; TODO this isn't getting done if we include!
        !word W_RFROM
        !word W_CLOSE_FILE
        !word W_DROP ; TODO check status?

        !word W_DOTS,W_CR
        !word W_CR
        !word W_PSEMI

_test_filename
        +STRING "bootstrap.f"

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
; (
; ("text" --)
; ANSI 11.6.1.0080

; See core

; ****************************************************************************
; BIN
; (fam_1 -- fam_2)
; ANSI 11.6.1.0765

!if ENABLE_FILE {
        +WORD "bin"
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
        +WORD "close-file"
W_CLOSE_FILE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<close-file>"
        !word W_DOTS
}
        !word W_CLOSE
        !word W_READSS
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; CREATE-FILE
; (c-addr u fam -- fileid ior)
; ANSI 11.6.1.1010

!if ENABLE_FILE {
        +WORD "create-file"
W_CREATE_FILE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<create-file-file>"
        !word W_DOTS
}        
        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; DELETE-FILE
; (c-addr u -- ior)
; ANSI 11.6.1.1190

!if ENABLE_FILE {
        +WORD "delete-file"
W_DELETE_FILE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<create-file-file>"
        !word W_DOTS
}        
        ; TODO
        !word W_2DROP
        !word W_ZERO    
!if DEBUG {
        !word W_DOTS,W_CR
}    
        !word W_PSEMI
}

; ****************************************************************************
; FILE-POSITION
; (fileid -- ud ior)
; ANSI 11.6.1.1520

!if ENABLE_FILE {
        +WORD "file-position"
W_FILE_POSITION
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<create-file-file>"
        !word W_DOTS
}
        ; TODO
        !word W_DROP
        !word W_ZERO          
        !word W_ZERO          
        !word W_ZERO   
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; FILE-SIZE
; (fileid -- ud ior)
; ANSI 11.6.1.1522

!if ENABLE_FILE {
        +WORD "file-size"
W_FILE_SIZE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<file-size>"
        !word W_DOTS
}        
        ; TODO
        !word W_DROP
        !word W_ZERO          
        !word W_ZERO          
        !word W_ZERO 
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; INCLUDE-FILE
; (i*x fileid -- j*x)
; ANSI 11.6.1.1717

!if ENABLE_FILE {
        +WORD "include-file"
W_INCLUDE_FILE
        !word DO_COLON

        !word W_SAVE_INPUT
        !word W_NTOR

        +LITERAL &SOURCE_ID
        !word W_STORE

-       !word W_REFILL
        +ZBRANCH +
        !word W_PEVALUATE
        +BRANCH -
+

        !word W_NRFROM
        !word W_RESTORE_INPUT
        !word W_DROP            ; TODO check status from restore

        !word W_PSEMI
}

; ****************************************************************************
; INCLUDED
; (i*x c-addr u -- j*x)
; ANSI 11.6.1718

!if ENABLE_FILE {
        +WORD "included"
W_INCLUDED
        !word DO_COLON
        !word W_RSLO
        !word W_OPEN_FILE
        !word W_DROP            ; TODO check status
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
        +WORD "open-file"
W_OPEN_FILE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<open-file>"
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
        !word W_DOTS
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
        !word W_PDOTQ
        +STRING "logical="
        !word W_DUP
        !word W_DOT
}

        !word W_DUP
        +LITERAL 8 ; TODO how to select this?
        !word W_UNUSED_SECONDARY
!if 0 {
        !word W_PDOTQ
        +STRING "secondary="
        !word W_DUP
        !word W_DOT
}
        !word W_SETLFS

        !word W_OPEN 
        ; max 10 channels open
        ; $98  ldtnd = # open channels
        ; $35a lat   = logical channel table
        ; $364 fat   = device number table
        ; $36e sat   = secondary table
        ; use lower nibble of logical channel as buffer index
        ; so save-input needs logical channel (0=keyboard) + position into buffer (>IN) - all fits in one word?

        !word W_READSS

!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}


; ****************************************************************************
; R/O
; (-- fam)
; ANSI 11.6.1.2054

!if ENABLE_FILE {
        +WORD "r/o"
W_RSLO
        !word DO_CONSTANT
        !word FAM_RO
}

; ****************************************************************************
; R/W
; (-- fam)
; ANSI 11.6.1.2056

!if ENABLE_FILE {
        +WORD "r/w"
W_RSLW
        !word DO_CONSTANT
        !word FAM_RW
}

; ****************************************************************************
; READ-FILE
; (c-addr u_1 fileid -- u_2 ior)
; ANSI 11.6.1.2080

!if ENABLE_FILE {
        +WORD "read-file"
W_READ_FILE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<read-file>"
        !word W_DOTS
}           
        ; TODO       


        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_ZERO
!if DEBUG {
        !word W_DOTS,W_CR
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
        +WORD "read-line"
W_READ_LINE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<read-line>"
        !word W_DOTS
}           
        ; TODO

        !word W_CHKIN

        ; Loop index is a pointer to the buffer entry
        !word W_OVER
        !word W_PLUS
        !word W_OVER
        !word W_PDO

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
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; REPOSITION-FILE
; (ud fileid -- ior)
; ANSI 11.6.1.2142

!if ENABLE_FILE {
        +WORD "reposition-file"
W_REPOSITION_FILE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<reposition-file>"
        !word W_DOTS
}          
        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; RESIZE-FILE
; (ud fileid -- ior)
; ANSI 11.6.1.2147

!if ENABLE_FILE {
        +WORD "resize-file"
W_RESIZE_FILE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<resize-file>"
        !word W_DOTS
}         
        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

; ****************************************************************************
; S"
; (???)
; ANSI 6.1.2165
; ANSI 11.6.1.2165

; See core

; ****************************************************************************
; SOURCE-ID
; (-- 0|-1|fileid)
; ANSI 11.6.1.2218

; See core-ext

; ****************************************************************************
; W/O
; (-- fam)
; ANSI 11.6.1.2425

!if ENABLE_FILE {
        +WORD "w/o"
W_WSLO
        !word DO_CONSTANT
        !word FAM_WO
}

; ****************************************************************************
; WRITE-FILE
; (c-addr u fileid -- ior)
; ANSI 11.6.1.2480

!if ENABLE_FILE {
        +WORD "write-file"
W_WRITE_FILE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<write-file>"
        !word W_DOTS
}       

        !word W_CHKOUT

        ; (c-addr u)

        ; Loop index is a pointer to the buffer entry
        !word W_OVER
        !word W_PLUS
        !word W_SWAP
        !word W_PDO

_write_file_loop

        !word W_I
        !word W_CAT
        !word W_BASOUT

        !word W_PLOOP
        !word _write_file_loop-*

        ; restore default output
        !word W_ZERO
        !word W_CHKOUT

        !word W_READSS

!if DEBUG {
        !word W_DOTS,W_CR
}       
        !word W_PSEMI
}

; ****************************************************************************
; WRITE-LINE
; (c-addr u fileid -- ior)
; ANSI 11.6.1.2485

!if ENABLE_FILE {
        +WORD "write-line"
W_WRITE_LINE
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<write-line>"
        !word W_DOTS
}       
        ; TODO rewrite using WRITE-FILE

        !word W_CHKOUT

        ; (c-addr u)

        ; Loop index is a pointer to the buffer entry
        !word W_OVER
        !word W_PLUS
        !word W_SWAP
        !word W_PDO

_write_line_loop

        !word W_I
        !word W_CAT
        !word W_BASOUT

        !word W_PLOOP
        !word _write_line_loop-*

        !word W_CR
        !word W_BASOUT

        ; restore default output
        !word W_ZERO
        !word W_CHKOUT

        !word W_READSS

!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}
