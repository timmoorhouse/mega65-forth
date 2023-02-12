
; ****************************************************************************
; BLOCK EXT

!if ENABLE_BLOCK_EXT {

; ****************************************************************************
; EMPTY-BUFFERS
; (--)
; ANSI 7.6.2.1330

; FIG:
;      EMPTY-BUFFERS                                         L0
;               Mark all block-buffers as empty, not necessarily affecting 
;               the contents.  Updated blocks are not written to the disc.  
;               This is also an initialisation procedure before first use 
;               of the disc.
;
;;
;;                                       EMPTY-BUFFERS
;;                                       SCREEN 58 LINE 11
;;
        +WORD "empty-buffers"
W_EMPTY_BUFFERS
        !word DO_COLON
;          !word FIRST
;          !word LIMIT
;          !word OVER
;          !word SUB
;          !word ERASE
        !word W_PSEMI

; ****************************************************************************
; LIST
; (u --)
; ANSI 7.6.2.1770

; FIG:
;      LIST          n  ---                                  L0
;               Display the ascii text of screen n on the selected output 
;               device.  SCR contains the screen number during and after 
;               this process.
;
;;
;;                                       LIST
;;                                       SCREEN 77 LINE 2
;;
        +WORD "list"
W_LIST
        !word DO_COLON
;          !word DECIM
;          !word CR
;          !word DUP
;          !word SCR
;          !word STORE
;          !word PDOTQ
;          !byte 6,"SCR # "
;          !word DOT
;          !word CLIT
;          !byte 16
;          !word ZERO
;          !word PDO
;L3620:    !word CR
;          !word I
;          !word THREE
;          !word DOTR
;          !word SPACE
;          !word I
;          !word SCR
;          !word AT
;          !word DLINE
;          !word PLOOP
;L3630:    !word $FFEC
;          !word CR
        !word W_PSEMI

; ****************************************************************************
; REFILL
; (-- flag)
; ANSI 7.6.2.2125

; See file-ext

; ****************************************************************************
; SCR
; (-- a-addr)
; ANSI 7.6.2.2190

; FIG:
;
;      SCR           ---  addr                               U
;               A user variable containing the screen number most recently 
;               referenced by LIST.
;
;;
;;                                       SCR
;;                                       SCREEN 36 LINE 13
;;
        +WORD "scr"
W_SCR
;        !word DOUSE
;          !byte $1C

; ****************************************************************************
; THRU
; (i*x u_1 u_2 -- j*x)
; ANSI 7.6.2.2280

; ****************************************************************************
; \
; ANSI 7.6.2.2535

; See core-ext

}
