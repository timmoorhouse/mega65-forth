
; ****************************************************************************
; FILE

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
!if DEBUG {
        !word W_PDOTQ
        +STRING "<bin>"
        !word W_DOTS
}
        +LITERAL FAM_BIN
        !word W_OR
!if DEBUG {
        !word W_DOTS,W_CR
}
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

; TODO lots of duplication with QUIT ...

!if ENABLE_FILE {
        +WORD "include-file"
W_INCLUDE_FILE
        !word DO_COLON
!if 1 {
        !word W_PDOTQ
        +STRING "<include-file>"
        !word W_DOTS
}

        ; TODO need to set SOURCE-ID

        !word W_TOR

_include_read_loop

        !word W_PAD ; TODO NEED TO USE A BUFFER NEAR END OF MEM SINCE THE EVALUATE CAN ALLOT !!!!!!!!!!!!

        !word W_DUP
        +CLITERAL 90
        !word W_RAT
        !word W_READ_LINE

        ; (c-addr u flag ior) (R: fileid)

        +ZBRANCH _include_ior_ok
        ; ior bad ...
        !word W_DROP ; drop flag
_include_flag_bad
        !word W_DROP ; drop u2
        !word W_DROP ; drop buffer address

!if 1 {
        !word W_PDOTQ
        +STRING "<include-file-error>"
        !word W_DOTS,W_CR
}
        !word W_LEAVE
        !word _include_after_loop-*

_include_ior_ok
        +ZBRANCH _include_flag_bad

        ; (c-addr u)

        !word W_EVALUATE ; TODO this sets SOURCE-ID to -1 !!!!!!!!!

        +BRANCH _include_read_loop
_include_after_loop
        !word W_RFROM,W_DROP ; drop fileid

!if 1 {
        !word W_PDOTQ
        +STRING "<include-file-end>"        
        !word W_DOTS,W_CR
}    
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
!if DEBUG {
        !word W_PDOTQ
        +STRING "<included>"
        !word W_DOTS
}      
        ; TODO
        !word W_2DROP
!if DEBUG {
        !word W_DOTS,W_CR
}
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

        +LITERAL 50      ; TODO find unused fileid

        !word W_DUP
        +LITERAL 8
        ;!word W_ZERO
        +LITERAL 5
        !word W_SETLFS

        !word W_OPEN

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
        ; TODO

        ; $FFD8	
        ; SAVE. Save file. (Must call SETLFS and SETNAM beforehands.)
        ; Input: A = Address of zero page register holding start address of memory area to save; X/Y = End address of memory area plus 1.
        ; Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1).
        ; Used registers: A, X, Y.
        ; Real address: $F5DD.

        !word W_DROP
        !word W_2DROP
        !word W_ZERO   
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
        ; TODO

        ; $FFC9	
        ; CHKOUT. Define file as default output. (Must call OPEN beforehands.)
        ; Input: X = Logical number.
        ; Output: –
        ; Used registers: A, X.
        ; Real address: ($0320), $F250.

        ; $FFD2	
        ; CHROUT. Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
        ; Input: A = Byte to write.
        ; Output: –
        ; Used registers: –
        ; Real address: ($0326), $F1CA.

        !word W_DROP
        !word W_2DROP
        !word W_ZERO  
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}
