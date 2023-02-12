
; ****************************************************************************
; FILE

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
        ; TODO
        !word W_DROP
        !word W_2DROP
        !word W_ONE
        !word W_ZERO
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
        !word W_DROP
        !word W_2DROP
        !word W_ZERO
        !word W_TRUE
        !word W_ZERO
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
        !word W_DROP
        !word W_2DROP
        !word W_ZERO  
!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}
