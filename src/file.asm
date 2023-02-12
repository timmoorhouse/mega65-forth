
; ****************************************************************************
; FILE

; ****************************************************************************
; (
; ("text" --)
; ANSI 11.6.1.0080

!if ENABLE_FILE {
}

; ****************************************************************************
; BIN
; (fam_1 -- fam_2)
; ANSI 11.6.1.0765

!if ENABLE_FILE {
}

; ****************************************************************************
; CLOSE-FILE
; (fileid -- ior)
; ANSI 11.6.1.0900

!if ENABLE_FILE {
}

; ****************************************************************************
; CREATE-FILE
; (c-addr u fam -- fileid ior)
; ANSI 11.6.1.1010

!if ENABLE_FILE {
}

; ****************************************************************************
; DELETE-FILE
; (c-addr u -- ior)
; ANSI 11.6.1.1190

!if ENABLE_FILE {
}

; ****************************************************************************
; FILE-POSITION
; (fileid -- ud ior)
; ANSI 11.6.1.1520

!if ENABLE_FILE {
}

; ****************************************************************************
; FILE-SIZE
; (fileid -- ud ior)
; ANSI 11.6.1.1522

!if ENABLE_FILE {
}

; ****************************************************************************
; INCLUDE-FILE
; (i*x fileid -- j*x)
; ANSI 11.6.1.1717

!if ENABLE_FILE {
}

; ****************************************************************************
; INCLUDED
; (i*x c-addr u -- j*x)
; ANSI 11.6.1718

!if ENABLE_FILE {
}

; ****************************************************************************
; OPEN-FILE
; (c-addr u fam -- fileid ior)
; ANSI 11.6.1.1970

!if ENABLE_FILE {
}

; ****************************************************************************
; R/O
; (-- fam)
; ANSI 11.6.1.2054

!if ENABLE_FILE {
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
        !word W_SEMI
}

; ****************************************************************************
; READ-FILE
; (c-addr u_1 fileid -- u_2 ior)
; ANSI 11.6.1.2080

!if ENABLE_FILE {
}

; ****************************************************************************
; READ-LINE
; (c-addr u_1 fileid -- u_2 flag ior)
; ANSI 11.6.1.2090

!if ENABLE_FILE {
}

; ****************************************************************************
; REPOSITION-FILE
; (ud fileid -- ior)
; ANSI 11.6.1.2142

!if ENABLE_FILE {
}

; ****************************************************************************
; RESIZE-FILE
; (ud fileid -- ior)
; ANSI 11.6.1.2147

!if ENABLE_FILE {
}

; ****************************************************************************
; S"
; (???)
; ANSI 11.6.1.2165

!if ENABLE_FILE {
}

; ****************************************************************************
; SOURCE-ID
; (-- 0|-1|fileid)
; ANSI 11.6.1.2218

!if ENABLE_FILE {
}

; ****************************************************************************
; W/O
; (-- fam)
; ANSI 11.6.1.2425

; ****************************************************************************
; WRITE-FILE
; (c-addr u fileid -- ior)
; ANSI 11.6.1.2480

!if ENABLE_FILE {
}

; ****************************************************************************
; WRITE-LINE
; (c-addr u fileid -- ior)
; ANSI 11.6.1.2485

!if ENABLE_FILE {
}
