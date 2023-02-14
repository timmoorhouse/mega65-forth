
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

        !word W_TOR

        !word W_PAD
        !word W_DUP
        +CLITERAL 80
        !word W_RAT

        !word W_READ_LINE

        !word W_DROP ; TODO check status?
        !word W_DROP ; TODO check flag

        +CLITERAL '['
        !word W_EMIT
        !word W_TYPE
        +CLITERAL ']'
        !word W_EMIT

        !word W_RFROM,W_DROP

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
        ; TODO
        !word W_DROP
        !word W_ZERO      
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
!if DEBUG {
        !word W_PDOTQ
        +STRING "<include-file>"
        !word W_DOTS
}
        ; TODO
        !word W_DROP     
!if DEBUG {
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

        !word W_SETNAM

        +LITERAL 1      ; TODO find unused fileid

        !word W_DUP
        +LITERAL 8
        !word W_ZERO
        !word W_SETLFS

        !word W_OPEN

        !word W_READST

!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

W_SETLFS        ; (logical device secondary --)
        !word *+2
        stx <TEMP1
        lda 4,x
        pha
        lda 2,x
        tax
        ldy 0,x
        pla
        jsr SETLFS
        ldx <TEMP1
        inx
        inx
        jmp POPTWO

; $FFBA	
; SETLFS. Set file parameters.
; Input: A = Logical number; X = Device number; Y = Secondary address.
; Output: –
; Used registers: –
; Real address: $FE00.
SETLFS
        +KERNEL_CALL $ffba
        rts

W_SETNAM        ; (c-addr u --)
        !word *+2
        stx <TEMP1
        lda 0,x
        pha
        ldy 3,x ; TODO check order of x & y
        lda 2,x
        tax
        pla
        jsr SETNAM
        ldx <TEMP1
        jmp POPTWO

; $FFBD	
; SETNAM. Set file name parameters.
; Input: A = File name length; X/Y = Pointer to file name.
; Output: –
; Used registers: –
; Real address: $FDF9.
SETNAM
        +KERNEL_CALL $ffbd
        rts

W_OPEN          ; (--)
        !word *+2
        jsr OPEN
        jmp NEXT

; $FFC0	
; OPEN. Open file. (Must call SETLFS and SETNAM beforehands.)
; Input: –
; Output: –
; Used registers: A, X, Y.
; Real address: ($031A), $F34A.

OPEN
        +KERNEL_CALL $ffc0
        rts

W_READST        ; (-- ior)
        !word *+2
        jsr READST
        pha
        lda #0
        jmp PUSH

; $FFB7	
; READST. Fetch status of current input/output device, value of ST variable. (For RS232, status is cleared.)
; Input: –
; Output: A = Device status.
; Used registers: A.
; Real address: $FE07.
READST 
        +KERNEL_CALL $ffb7
        rts


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

; FIG:
;      R/W           addr  blk  f  ---
;               The fig-FORTH standard disc read-write linkage.  addr 
;               specifies the source or destination block buffer, blk is 
;               the sequential number of the referenced block; and f is a 
;               flag for f=0 write and f=1 for read.  R/W determines the 
;               location on mass storage, performs the read-write and 
;               performs any error checking.
;
;;
;;                                       R/W
;;                              Read or write one sector
;;

!if ENABLE_FILE {
        +WORD "r/w"
W_RSLW
        !word DO_CONSTANT
        !word FAM_RW
}
!if 0 {
        +WORD "r/w"
W_RSLW
        !word DO_COLON
;          !word ZEQU,LIT,$C4DA,CSTOR
;          !word SWAP,ZERO,STORE
;          !word ZERO,OVER,GREAT,OVER
;          !word LIT,SECTL-1,GREAT,OR,CLIT
;          !byte 6
;          !word QERR
;          !word ZERO,LIT,SECTR,USLAS,1PLUS
;          !word SWAP,ZERO,CLIT
;          !byte $12
;          !word USLAS,DBCD,SWAP,1PLUS
;          !word DBCD,DDISC,CLIT
;          !byte 8
;          !word QERR
        !word W_PSEMI
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

        ; $FFD5	
        ; LOAD. Load or verify file. (Must call SETLFS and SETNAM beforehands.)
        ; Input: A: 0 = Load, 1-255 = Verify; X/Y = Load address (if secondary address = 0).
        ; Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1); X/Y = Address of last byte loaded/verified (if Carry = 0).
        ; Used registers: A, X, Y.
        ; Real address: $F49E.

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

        !word W_DROP
        !word W_CHRIN
        !word W_SWAP
        !word W_CSTORE
        !word W_ONE
        !word W_TRUE

        ; restore default input
        !word W_ZERO
        !word W_CHKIN

        !word W_READST

!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}

W_CHKIN         ; (u --)
        !word *+2
        stx <TEMP1
        lda 0,x
        tax
        jsr CHKIN
        ldx <TEMP1
        jmp POP

; $FFC6	
; CHKIN. Define file as default input. (Must call OPEN beforehands.)
; Input: X = Logical number.
; Output: –
; Used registers: A, X.
; Real address: ($031E), $F20E.

CHKIN
        +KERNEL_CALL $ffc6
        rts

; $FFCF	
; CHRIN. Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
; Input: –
; Output: A = Byte read.
; Used registers: A, Y.
; Real address: ($0324), $F157.

W_CHRIN         ; (-- c)
        !word *+2
        phy
        jsr CHRIN
        jmp PUSH

CHRIN
        +KERNEL_CALL $ffcf
        rts

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
