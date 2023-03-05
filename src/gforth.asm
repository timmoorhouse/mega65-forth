
; ****************************************************************************

; Selected words from gforth

; TODO form (-- urows ucols)

; ****************************************************************************
; TODO savesystem ("filename" --) - or just include this at the end of bootstrap sources?
; TODO return ior?

; SAVESYSTEM FOO,P,W

; TODO do this in forth

!if ENABLE_GFORTH {
        +WORD "savesystem", F_IMMEDIATE
W_SAVESYSTEM
        !word DO_COLON
!if DEBUG {
        +DOTQ "<savesystem>"
        !word W_DOTS
}
        !word W_PARSE_NAME
        !word W_WSLO
        !word W_OPEN_FILE
        !word W_DROP ; TODO check status

        !word W_TOR

        +LITERAL $2001 ; start of BASIC

        ; write load address
        !word W_SPAT
        !word W_TWO
        !word W_RAT
        !word W_WRITE_FILE
        !word W_DROP ; TODO check status

        ; TODO what to do about basepage?  does it make sense to move our base page to the top of memory and then
        ; initialize it in COLD?
        ; It would be very nice to have a savesystem on an unmodified dictionary give something identical to the
        ; original prg

        ; TODO inline dmalists will also have this issue

        !word W_DUP
        !word W_HERE
        !word W_SWAP
        !word W_SUB
        !word W_RAT
        !word W_WRITE_FILE
        !word W_DROP ; TODO check status

        !word W_RFROM
        !word W_CLOSE_FILE
        !word W_DROP ; TODO check status

!if DEBUG {
        !word W_DOTS,W_CR
}
        !word W_PSEMI
}
