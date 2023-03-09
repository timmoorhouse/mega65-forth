
; FIG-specific stuff

!if ENABLE_FIG {

; ****************************************************************************
; ->
;      -->                                                   P,L0
;               Continue interpretation with the next disc screen.  
;               (pronounced next-screen).

!if 0 {
        +CREATE "-->"
W_DASHDASH
        !word DO_COLON
;          !word QLOAD
;          !word ZERO
;          !word IN
;          !word STORE
;          !word BSCR
;          !word BLK
;          !word AT
;          !word OVER
;          !word MOD
;          !word SUB
;          !word BLK
;          !word PSTOR
        !word W_PSEMI
}

; ****************************************************************************
; B/BUF

;      B/BUF         ---  n
;               This constant leaves the number of bytes per disc buffer, 
;               the byte count read from disc by BLOCK.

!if 0 {
        +CREATE "b/buf"
W_BBUF
        !word DO_CONSTANT
;          !word SSIZE    ; sector size
}

; ****************************************************************************
; B/SCR

;      B/SCR         ---  n
;               This constant leaves the number of blocks per editing 
;               screen.  By convention, an editing screen is 1024 bytes 
;               organised as 16 lines of 64 characters each.

!if 0 {
        +CREATE "b/scr"
W_BSCR
        !word DO_CONSTANT
;          !word 8        ; blocks to make one screen
;
}

; ****************************************************************************
; +BUF

;      +BUF          addr1  ---  addr2  f
;               Advance the disc buffer address addr1 to the address of 
;               the next buffer addr2.  Boolean f is false when addr2 is 
;               the buffer presently pointed to by variable PREV.

!if 0 {
        +CREATE "+buf"
W_PBUF
        !word DO_COLON
;          !word LIT
;          !word SSIZE+4  ; hold block #, one sector two num
;          !word PLUS
;          !word DUP
;          !word LIMIT
;          !word EQUAL
;          !word ZBRAN
;L2688:    !word 6        ; L2691-L2688
;          !word DROP
;          !word FIRST
;L2691:    !word DUP
;          !word PREV
;          !word AT
;          !word SUB
        !word W_PSEMI
}

; ****************************************************************************
; CONTEXT

;      CONTEXT       ---  addr                               U,L0
;               A user variable containing a pointer to the vocabulary 
;               within which dictionary searches will first begin.

!if 0 {
        +CREATE "context"
W_CONTEXT
        !word DO_USER
;          !byte $20
}

; ****************************************************************************
; DPL

;      DPL           ---  addr                               U,L0
;               A user variable containing the number of digits to the 
;               right of the decimal on double integer input.  It may also 
;               be used to hold output column location of a decimal point, 
;               in user generated formatting.  The default value on single 
;               number input is -1.

!if 0 {
        +CREATE "dpl"
W_DPL
        !word DO_USER
        !byte U_DPL
}

; ****************************************************************************
; FENCE

;      FENCE         ---  addr                               U
;               A user variable containing an address below which 
;               FORGETting is trapped.  To forget below this point the 
;               user must alter the contents of FENCE.

!if 0 {
        +CREATE "fence"
W_FENCE
        !word DO_USER
        !byte U_FENCE
}

; ****************************************************************************
; FIRST

;      FIRST         ---  n
;               A constant that leaves the address of the first (lowest) 
;               block buffer.

!if 0 {
        +CREATE "first"
W_FIRST
        !word DO_CONSTANT
;          !word DAREA    ; bottom of disk buffer area
}

; ****************************************************************************
; FLD

;      FLD           ---  addr                               U
;               A user variable for control of number output field width.  
;               Presently unused in fig-FORTH.

!if 0 {
        +CREATE "fld"
W_FLD
        !word DO_USER
;          !byte $2A
}

; ****************************************************************************
; LIMIT

;      LIMIT         ---  n
;               A constant leaving the address just above the highest 
;               memory available for a disc buffer.  Usually this is the 
;               highest system memory.

!if 0 {
        +CREATE "limit"
W_LIMIT
        !word DO_CONSTANT
;          !word UAREA    ; buffers end at user area
}

; ****************************************************************************
; (LINE)


;      (LINE)        n1  n2  ---  addr  count
;               Convert the line number n1 and the screen n2 to the disc 
;               buffer address containing the data.  A count of 64 
;               indicates the full line text length.

!if 0 {
        +CREATE "(line)"
W_PLINE
        !word DO_COLON
;          !word TOR
;          !word CSLL
;          !word BBUF
;          !word SSMOD
;          !word RFROM
;          !word BSCR
;          !word STAR
;          !word PLUS
;          !word BLOCK
;          !word PLUS
;          !word CSLL
        !word W_PSEMI
}

; ****************************************************************************
; .LINE

;      .LINE         line  scr  ---
;               Print on the terminal device, a line of text from the disc 
;               by its line and screen number.  Trailing blanks are 
;               suppressed.

!if 0 {
        +CREATE ".line"
W_DLINE
        !word DO_COLON
;          !word PLINE
;          !word DTRAI
;          !word TYPE
        !word W_PSEMI
}

; ****************************************************************************
; ?LOADING

;      ?LOADING
;               Issue an error message if not loading.

!if 0 {
        +CREATE "?loading"
W_QLOAD
        !word DO_COLON
;          !word BLK
;          !word AT
;          !word ZEQU
;          !word CLIT
;          !byte $16
;          !word QERR
        !word W_PSEMI
}

; ****************************************************************************
; MESSAGE

;      MESSAGE       n  ---
;               Print on the selected output device the text of line n 
;               relative to screen 4 of drive 0.  n may be positive or 
;               negative.  MESSAGE may be used to print incidental text 
;               such as report headers.  IF WARNING is zero, the message 
;               will simply be printed as a number (disc-unavailable).

!if 0 {
        +CREATE "message"
W_MESSAGE
        !word DO_COLON
;          !word WARN
;          !word AT
;          !word ZBRAN
;L2874:    !word $1B      ; L2888-L2874
;          !word DDUP
;          !word ZBRAN
;L2877:    !word $11      ; L2886-L2877
;          !word CLIT
;          !byte 4
;          !word W_OFFSET
;          !word AT
;          !word BSCR
;          !word SLASH
;          !word SUB
;          !word DLINE
;L2886:    !word BRAN
;L2887:    !word 13       ; L2891-L2887
;L2888:    !word PDOTQ
;          !byte 6,"MSG # "
;          !word DOT
        !word W_PSEMI
}

; ****************************************************************************
; PREV

;      PREV          ---  addr
;               A variable containing the address of the disc buffer most 
;               recently referenced.  The UPDATE command marks this buffer 
;               to be later written to disc.

!if 0 {
        +CREATE "prev"
W_PREV
        !word DO_VARIABLE
;          !word DAREA
}

; ****************************************************************************
; R#

;      R#            ---  addr                               U
;               A user variable which may contain the location of an 
;               editing cursor, or other file related function.

!if 0 {
        +CREATE "r#"
W_RNUM
        !word DO_USER
;          !byte $2E
}

; ****************************************************************************
; TRAVERSE

;      TRAVERSE      addr1  n  ---  addr2
;               Move across the name field of a fig-FORTH variable length 
;               name field.  addr1 is the address of either the length 
;               byte or the last letter.  If n=1, the motion is toward 
;               high memory; if n= -1, the motion is toward low memory.  
;               The addr2 resulting is address of the other end of the 
;               name.

!if 0 {
        +CREATE "traverse"
W_TRAVERSE
        !word DO_COLON
;          !word SWAP
;L1312:    !word OVER
;          !word PLUS
;          !word CLITERAL
;          !byte $7F
;          !word OVER
;          !word CAT
;          !word LESS
;          !word ZBRANCH
;L1320:    !word $FFF1    ; L1312-L1320
;          !word SWAP
;          !word DROP
        !word W_PSEMI
}

; ****************************************************************************
; USE

;      USE           ---  addr
;               A variable containing the address of the block buffer to 
;               use next, as the least recently written.

!if 0 {
        +CREATE "use"
W_USE
        !word DO_VARIABLE
;          !word DAREA
}

; ****************************************************************************
; VOC-LINK

;      VOC-LINK      ---  addr                               U
;               A user variable containing the address of a field in the 
;               definition of the most recently created vocabulary.  All 
;               vocabulary names are linked by these fields to allow 
;               control for FORGETting through multiple vocabularies.

!if 0 {
        +CREATE "voc-link"
W_VOCL
        !word DO_USER
;          !byte $14
}

; ****************************************************************************
; WARNING

;      WARNING       ---  addr                               U
;               A user variable containing a value controlling messages.  
;               If = 1, disc is present and screen 4 of drive 0 is the 
;               base location for messages.  If = 0, no disc is present 
;               and messages will be presented by number.  If = -1, 
;               execute (ABORT) for a user specified procedure.  See 
;               MESSAGE, ERROR.

!if 0 {
        +CREATE "warning"
W_WARNING
        !word DO_USER
;          !byte $E
}

; ****************************************************************************
; X

;      X
;               This is a psuedonym for the "null" or dictionary entry for 
;               a name of one character of ascii null.  It is the 
;               execution procedure to terminate interpretation of a line 
;               of text from the terminal or within a disc buffer, as both 
;               buffers always have a null at the end.

!if 0 {
        +CREATE "x"
W_X
        !word DO_COLON
;          !word BLK
;          !word AT
;          !word ZBRAN
;L1810:    !word $2A      ; L1830-l1810
;          !word ONE
;          !word BLK
;          !word PSTOR
;          !word ZERO
;          !word IN
;          !word STORE
;          !word BLK
;          !word AT
;          !word ZERO,BSCR
;          !word USLAS
;          !word DROP     ; fixed from model
;          !word ZEQU
;          !word ZBRAN
;L1824:    !word 8        ; L1828-L1824
;          !word QEXEC
;          !word RFROM
;          !word DROP
;L1828:    !word BRAN
;L1829:    !word 6        ; L1832-L1829
;L1830:    !word RFROM
;          !word DROP
        !word W_PSEMI
}

}
