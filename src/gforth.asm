
; ****************************************************************************

; Selected words from gforth

; TODO form (-- urows ucols)


; TODO savesystem ("filename" --) - or just include this at the end of bootstrap sources?
; TODO return ior?

!if ENABLE_GFORTH {
        +WORD "savesystem"
W_SAVESYSTEM
        !word DO_COLON
!if DEBUG {
        !word W_PDOTQ
        +STRING "<savesystem>"
        !word W_DOTS
}
        !word W_PARSE_NAME
        !word W_WSLO
        !word W_OPEN_FILE
        !word W_DROP ; TODO check status

        +LITERAL $2001 ; start of BASIC
        !word W_DUP
        !word W_HERE
        !word W_SWAP
        !word W_SUB
        !word W_WRITE_FILE
        !word W_DROP ; TODO check status

!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}
